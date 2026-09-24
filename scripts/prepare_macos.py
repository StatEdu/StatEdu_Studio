"""Prepare an isolated Apple Silicon developer build; never publish a release."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
TOOLS = ("prepare_macos.py", "macos_runtime.py", "relocate_macos_runtime.py", "validate_macos_runtime.R", "verify_macos_app.py", "doctor_macos.py", "archive_macos_preparation.py", "check_macos_packages.py", "bundled_validation_packages.expected.csv")


def verify_stage(stage):
    stage = assert_macos_destination(stage)
    manifest = json.loads((stage / "stage-integrity.json").read_text(encoding="utf-8"))
    for name, expected in manifest["sha256"].items():
        target = (stage / name).resolve()
        if stage not in target.parents:
            raise ValueError(f"Manifest path escapes stage: {name}")
        if not target.is_file() or hashlib.sha256(target.read_bytes()).hexdigest() != expected:
            raise ValueError(f"Prepared file missing or changed: {name}")
    package = json.loads((stage / "package.json").read_text(encoding="utf-8"))
    lock = json.loads((stage / "package-lock.json").read_text(encoding="utf-8"))
    if lock["packages"][""]["devDependencies"] != package["devDependencies"]:
        raise ValueError("Mac package dependency lock does not match package.json")
    return len(manifest["sha256"])


def assert_macos_destination(destination):
    destination = Path(destination).resolve()
    windows = (ROOT / "packaging/electron").resolve()
    windows_dist = (ROOT / "dist/electron").resolve()
    for protected in (windows, windows_dist):
        if destination == protected or protected in destination.parents:
            raise ValueError("macOS preparation must not write into Windows packaging or distribution")
    return destination


def prepare(destination):
    destination = assert_macos_destination(destination)
    if destination.exists():
        raise ValueError("Destination must not exist; existing files will not be overwritten")
    destination.mkdir(parents=True)
    electron = ROOT / "packaging/macos"
    package = json.loads((electron / "package.json").read_text(encoding="utf-8-sig"))
    version = (ROOT / "VERSION").read_text().strip().split("-")[0] + "-dev"
    package["version"] = version
    (destination / "package.json").write_text(json.dumps(package, indent=2) + "\n")
    lock = json.loads((electron / "package-lock.json").read_text(encoding="utf-8"))
    lock["version"] = version
    lock["packages"][""]["version"] = version
    (destination / "package-lock.json").write_text(json.dumps(lock, indent=2) + "\n", encoding="utf-8")
    for name in ("main.js", "preload.js"):
        shutil.copy2(electron / name, destination / name)
    (destination / "build").mkdir()
    shutil.copy2(electron / "build/icon.png", destination / "build/icon.png")
    # Only application source/assets: do not ship personal data, logs or evidence.
    files = subprocess.check_output(
        ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"], cwd=ROOT
    ).decode("utf-8").split("\0")
    tops = {"app.R", "run_app.R", "LICENSE", "SOURCE-OFFER.txt", "VERSION"}
    documentation = json.loads((ROOT / "docs/i18n/document_specs.json").read_text(encoding="utf-8-sig"))
    for language in documentation.values():
        for document in language.values():
            tops.add(document["path"])
    tops.update({"docs/i18n/document_specs.json", "docs/ANALYSIS_REFERENCE_COMPARISON_PUBLIC.md",
                 "docs/ANALYSIS_REFERENCE_COMPARISON_PUBLIC_KO.md"})
    prefixes = ("R/", "www/", "i18n/", "docs/assets/user-guide/en/", "docs/assets/user-guide/ko/")
    manifest = []
    for name in sorted(set(files) | tops):
        if not name or not (name in tops or name.startswith(prefixes) or
                            name.startswith(("README", "CHANGELOG"))):
            continue
        source = ROOT / name
        if source.is_symlink() or not source.is_file():
            raise ValueError(f"Unexpected source file: {name}")
        target = destination / "app" / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)
        manifest.append(name)
    (destination / "app/VERSION").write_text(version + "\n")
    (destination / "source-manifest.json").write_text(json.dumps(manifest, indent=2))
    (destination / "runtime").mkdir()
    (destination / "tools").mkdir()
    for name in TOOLS:
        shutil.copy2(ROOT / "scripts" / name, destination / "tools" / name)
    for name in ("build.command", "PREPARATION_README_KO.md"):
        shutil.copy2(electron / name, destination / name)
    (destination / "build.command").write_text((electron / "build.command").read_text(encoding="utf-8"),
                                               encoding="utf-8", newline="\n")
    (destination / "build.command").chmod(0o755)
    hashes = {str(p.relative_to(destination)).replace("\\", "/"): hashlib.sha256(p.read_bytes()).hexdigest()
              for p in sorted(destination.rglob("*")) if p.is_file()}
    (destination / "stage-integrity.json").write_text(json.dumps({"schema": 1, "sha256": hashes}, indent=2), encoding="utf-8")
    return destination


def check_runtime(stage):
    stage = assert_macos_destination(stage)
    package = json.loads((stage / "package.json").read_text(encoding="utf-8-sig"))
    if package.get("build", {}).get("appId") != "com.statedu.studio.mac.dev":
        raise ValueError("Not a StatEdu macOS developer stage")
    verify_stage(stage)
    if platform.system() != "Darwin" or platform.machine() != "arm64":
        raise RuntimeError("Runtime checks/build require native Apple Silicon macOS")
    from macos_runtime import audit_stage, runtime_environment
    audit_stage(stage)
    home = stage / "runtime/R.framework/Resources"
    rscript = home / "bin/Rscript"
    if not rscript.is_file():
        raise RuntimeError(f"Supply a relocatable R 4.5.3 arm64 framework at {home.parent}")
    env = runtime_environment(home)
    subprocess.run([str(rscript), "--vanilla", str(stage / "tools/validate_macos_runtime.R")],
                   cwd=stage / "app", env=env, check=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", help="New staging directory (default: a new temp directory)")
    parser.add_argument("--check-stage", help="Check an existing stage after adding its R framework")
    parser.add_argument("--verify-stage", help="Verify prepared source/tool hashes without R or macOS")
    parser.add_argument("--build", action="store_true", help="Build a local unsigned .app after runtime checks")
    args = parser.parse_args()
    if args.verify_stage:
        if args.build or args.check_stage or args.output:
            parser.error("--verify-stage cannot be combined with other actions")
        print(f"PASS: {verify_stage(args.verify_stage)} prepared files verified")
        return
    if args.build and not args.check_stage:
        parser.error("--build requires --check-stage; prepare and supply the runtime first")
    if args.check_stage:
        stage = Path(args.check_stage).resolve()
        check_runtime(stage)
        if args.build:
            env = dict(os.environ, CSC_IDENTITY_AUTO_DISCOVERY="false")
            subprocess.run(["npm", "ci", "--no-audit", "--no-fund"], cwd=stage, env=env, check=True)
            subprocess.run(["npm", "run", "dist"], cwd=stage, env=env, check=True)
            package = json.loads((stage / "package.json").read_text(encoding="utf-8"))
            bundle = stage / "dist/mac-arm64" / (package["build"]["productName"] + ".app")
            subprocess.run([sys.executable, str(stage / "tools/verify_macos_app.py"),
                            "--stage", str(stage), "--app", str(bundle)], check=True)
    else:
        stage = prepare(args.output or (Path(tempfile.mkdtemp(prefix="statedu-mac-")) / "stage"))
    print(stage)
    print("Developer preparation only; no signed/notarized release has been produced.")


if __name__ == "__main__":
    main()
