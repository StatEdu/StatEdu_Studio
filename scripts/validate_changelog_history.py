"""Every UI language must include the full release sequence and original dates."""
import re
from collections import Counter
from pathlib import Path

root = Path(__file__).resolve().parents[1]
pattern = re.compile(r"^## (v\S+)(?: - (.+))?$", re.M)
reference = pattern.findall((root / "CHANGELOG.md").read_text(encoding="utf-8-sig"))
assert reference and reference[-1][0] == "v0.1.0"
def sections(text):
    chunks = re.split(r"(?m)^## ", text)[1:]
    return {chunk.splitlines()[0].split()[0]: chunk for chunk in chunks}

source = sections((root / "CHANGELOG.md").read_text(encoding="utf-8-sig"))
failures = []
for language in ("KO", "JA", "ZH", "ES", "FR", "DE", "VI"):
    path = root / f"CHANGELOG_{language}.md"
    actual = pattern.findall(path.read_text(encoding="utf-8-sig"))
    if actual != reference:
        failures.append(f"{language}: {len(actual)} releases; expected {len(reference)} with identical dates/order")
    else:
        translated = sections(path.read_text(encoding="utf-8-sig"))
        for version, body in source.items():
            if version == "v1.3.0" or language == "KO":
                continue
            expected_bullets = len(re.findall(r"(?m)^- ", body))
            actual_bullets = len(re.findall(r"(?m)^- ", translated[version]))
            assert actual_bullets == expected_bullets, (language, version, expected_bullets, actual_bullets)
            assert len(re.findall(r"(?m)^### ", body)) == len(re.findall(r"(?m)^### ", translated[version])), (
                language, version, "subsection count changed")
            assert Counter(re.findall(r"`([^`]+)`", body)) == Counter(re.findall(r"`([^`]+)`", translated[version])), (
                language, version, "technical literals changed")
        print(f"PASS: {language}: {len(actual)} releases with identical dates/order and historical entry checks")
assert not failures, "\n".join(failures)
