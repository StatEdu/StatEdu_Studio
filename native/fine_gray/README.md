# Fine–Gray variance cache prototype

This directory retains the source needed to reproduce the optional Windows
native prototype. It is not automatically built, installed, or enabled by the
application or installer.

## Source and changes

`src/crr.f` is from Bob Gray's **cmprsk 2.2-12** source distribution:
https://cran.r-project.org/src/contrib/cmprsk_2.2-12.tar.gz

The source archive SHA-256 is
`773ECB93BE0EAC7BB5DFE9EA1480380DA89EA95497B7B2FEBB08FD7C5104ACDC`.
The upstream package declares **GPL (>= 2)**. Its metadata is retained in
`UPSTREAM_DESCRIPTION`; the GPL version 2 text is in `COPYING`.

StatEdu modifications, 2026-09-14: `generate_cached.py` preserves the original
source and appends `crrvvc` and `covt_static` in `src/crr_cached.f`. The static
covariate path calculates each row's exponential predictor once. Covariates
are copied into the original work vector on each use. Weight arithmetic and
accumulation order are retained. Time-varying covariates call the original
`crrvv`. Both DLLs are built separately; do not link both source files into one
DLL because they contain the same original routines.

## Reproduce

Use Python 3.10 or newer to regenerate `src/crr_cached.f`:

```powershell
python native/fine_gray/generate_cached.py
```

Use the Rtools45 base toolchain with GFortran 14.3.0. From the repository root:

```powershell
./scripts/build_fine_gray_native.ps1 -ToolchainRoot '<Rtools45 toolchain root>' -OutputDirectory 'output/fine-gray-build'
```

The output directory must not already contain the two DLLs or build manifest.
The script changes PATH only for its own process and restores it afterward.
It records compiler/source/binary hashes and compiler flags. The PE timestamp
is disabled and the preferred image base is fixed to permit repeated
byte-identical builds with the same toolchain in different output folders.
This is not a promise of identical binaries with a different toolchain.

Retain this source, generator, metadata, license, and build script together
when preparing the optional component for distribution. No installer wiring
or automatic activation is provided here. An unrecognized binary fingerprint
is intentionally rejected by `survival_fine_gray_engine()` until separately
validated and added to its accepted builds.

## Validate

```powershell
$env:STATEDU_FINE_GRAY_BUILD_DIR = 'output/fine-gray-build'
./packaging/electron/runtime/R-4.5.3/bin/Rscript.exe scripts/validate_fine_gray_native_build.R
$env:STATEDU_FINE_GRAY_TEST_DLL = 'output/fine-gray-build/candidate.dll'
./packaging/electron/runtime/R-4.5.3/bin/Rscript.exe scripts/validate_fine_gray_native_loader.R
```

Native tests cover 21 basic, 40 additional and 100 edge conditions, including
time-varying fallback, singular inputs and nonconvergence. Error cases are
compared as errors; these are not 161 successful fits. Loader tests verify the
accepted binary and fallback behavior in the supported R runtime. Evidence is
written into the selected build directory and the loader test output folder.
