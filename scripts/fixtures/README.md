# Validation fixtures

`survival_validation.csv` is a deterministic synthetic fixture for the Kaplan-Meier and Cox regression validation scripts. It contains no patient or external-package data.

The fixture deliberately includes two grouping levels, three performance-status levels, continuous ages, censoring, events, tied analysis horizons, and follow-up beyond 400 time units. Its purpose is stable engine and rendering regression testing across the host and bundled R runtimes; it is not an example clinical data set.
