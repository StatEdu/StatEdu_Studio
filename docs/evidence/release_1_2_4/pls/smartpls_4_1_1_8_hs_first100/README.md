# SmartPLS 4.1.1.8 Student: Holzinger–Swineford first-100 PLS/PLSc evidence

## Scope

This is a deterministic Student-license supplement to the historical
301-row external handoff. It uses source data rows 1 through 100 (1-based data
rows after the header), x1 through x9 in source order, the same three reflective
common-factor blocks, and the same three structural paths. It does **not**
replace or complete the 301-row SmartPLS/ADANCO gate.

- Software: SmartPLS 4.1.1.8, Student license (free limited, non-Professional)
- Run date: 2026-08-23 (Asia/Seoul)
- PLS and PLSc: standardized results, path weighting, individual +1 initial
  outer weights, stop criterion 10^-7, SmartPLS fixed maximum 3,000 iterations
- Fit target: saturated model
- Convergence: PLS 26 iterations; PLSc initial PLS stage 26 iterations; both
  execution logs end with `All calculations done.`
- Output provenance: displayed values. The Student license locked Excel, HTML, and table-copy
  export, so this record claims agreement only at SmartPLS's displayed three
  decimal places.

## Results

| Estimator | Source | SRMR | d_G | d_ULS |
|:--|:--|--:|--:|--:|
| PLS | StatEdu, full precision | 0.112153043659770 | 0.175648969255271 | 0.566023734096761 |
| PLS | SmartPLS, displayed | 0.112 | 0.176 | 0.566 |
| PLSc | StatEdu, full precision | 0.122668303364477 | 0.281768771471191 | 0.677138069264375 |
| PLSc | SmartPLS, displayed | 0.123 | 0.282 | 0.677 |

All six comparisons pass the half-unit tolerance implied by three displayed
decimal places. `comparison.csv` contains the independently recomputed errors.

## Integrity and limitations

`external_run.json` binds the deterministic handoff data, canonical StatEdu
model contract, StatEdu results, transcribed external results, comparison
table, and fourteen HS private artifacts by basename, byte length, and SHA-256;
the TAM supplementary manifest binds two additional private screenshots.
Vendor UI/project/settings artifacts are not published: they are retained under
`STATEDU_SMARTPLS_EVIDENCE_ROOT` and are required by the release/installer gate.
The fit values and convergence count are manual attestations tied to those
private hashes; screenshots are not machine-parsed.

The historical 301-row handoff remains in `outputs/pls_external_handoff`. This
Student-license run was limited to 100 rows, and no completed 301-row execution
evidence was retained, so the 301-row profile remains pending. The existing
TAM-100 record remains a separate supplementary cross-check and is not silently
substituted for either Holzinger–Swineford profile.

Software citation (SmartPLS Terms §8.1 example): Ringle, C. M., Wende, S., and Becker, J.-M. (2024). SmartPLS 4. Bönningstedt: SmartPLS GmbH. https://www.smartpls.com .

Vendor screenshots and private UI/project/settings artifacts are excluded from
the public repository under [SmartPLS Terms §3.4](https://www.smartpls.com/terms/?locale=en-US).
