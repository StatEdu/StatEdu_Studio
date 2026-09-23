# Public 1.3.0 source correction

This source snapshot restores the application source shipped with the public Windows 1.3.0 installer. The earlier v1.3.0 tag incorrectly pointed at the 1.2.0 source commit edae03ebe3edbf46d19249dacaab955a1d16482b. That commit is retained under v1.3.0-original-source-reference for historical reference.

The application source and Electron main/preload files were recovered from the distributed installer. Packaging configuration, lockfile, icons and the missing validation helper were supplied separately and used in the verified Windows rebuild. The source notice now links to this public repository. No developer 1.3.1 changes are included.

Original public installer SHA-256:
DEFE844B8A46CCD416553322818FE724441A81681C26FAA9C4EB02A2092C5E10

The isolated rebuild passed the existing regression gates and packaged source checks. The rebuilt installer hash differs from the original; this is not a claim of byte-for-byte reproducibility.

## Windows build prerequisites

Use R 4.5.3 and the package versions in bundled_validation_packages.lock.csv and license_report.csv. Prepare packaging/electron/runtime/R-4.5.3. In packaging/electron, run npm ci to install Electron 43.4.0 and electron-builder 26.15.3 from the committed lockfile. The Windows release entry point is scripts/build_electron_beta.ps1 without -Developer. Read its parameter definitions before execution; output paths are repository-relative.

The official release validation gates require private SmartPLS comparison evidence. That evidence is not distributed here, and the gates must not be bypassed. The public app can be inspected and run from source with the documented R dependencies; this snapshot is not a self-contained runtime or a promise that every third-party source archive is included. See THIRD-PARTY-NOTICES.txt, license_report.csv and LICENSES for component versions, source references and notices.

The existing public installer is unchanged and retains its historical source notice. This document corrects the public source destination; a future Store package must include the corrected notice separately.
