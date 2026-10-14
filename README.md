# Fixed speed cameras and injury crashes in France

This repository contains the code and archived numerical results for the paper
on fixed speed cameras and injury crashes in France. It is deliberately kept
separate from the reusable data package
[`jrenne/accidents-radars-france`](https://github.com/jrenne/accidents-radars-france).

The layout follows the author's other replication repositories: an R project,
one `main.R` entry point, numbered scripts, a short requirements section, and
separate data and result directories. No working-directory path needs editing.

## Reproduce the results

Open `accidents-radars-replication.Rproj` and use one of three modes.

```r
# Restore the archived paper results (default; no download or estimation).
source("main.R")
```

From a terminal, the explicit equivalents are:

```sh
Rscript main.R --mode=quick
Rscript main.R --mode=main
Rscript main.R --mode=full
```

| Mode | What it does | Intended use |
|---|---|---|
| `quick` | Restores the archived estimates, diagnostics, tables, and figures shipped with this repository | Reading, checking reported numbers, preparing the manuscript |
| `main` | Downloads verified inputs and re-estimates the preferred specifications and main diagnostics | Ordinary replication |
| `full` | Also runs every spatial, sample, outcome, date-quality, graph, placebo, and traffic robustness check | Archival reproduction |

The published calculations use 1,999 cluster-bootstrap draws. A smaller number
can be selected for a smoke test, for example:

```sh
Rscript main.R --mode=main --bootstrap=49
```

The package normally uses the frozen IGN road snapshot distributed with data
release `v1.1.0`. To query the current IGN service and rebuild the spatial
matches and monthly corridor counts instead, use:

```sh
Rscript main.R --mode=full --rebuild-spatial
```

That option is slower and may differ if the live road database has changed.

## Data

The scripts download the following files from release `v1.1.0` of the data
repository and verify every file with SHA-256:

- harmonized injury-crash records;
- harmonized road-user records, used to construct fatal outcomes;
- the linked 2018 and 2025 fixed-camera inventories;
- the longitudinal official 2024 and 2025 camera inventories;
- an approximately 66 MB auxiliary release asset containing the dated IGN road snapshot,
  administrative outlines, traffic files, and derived monthly spatial counts.

Downloaded files are stored under `data/` and ignored by Git. If a sibling
checkout named `accidents-radars-france` is present, the scripts use and verify
that local copy instead of downloading it.

The statistical code repository therefore remains small. The reusable data,
teaching extract, provenance, licences, and larger release assets remain in the
data repository.

## Requirements

The analysis was prepared with R 4.5. The required R packages are:

```r
install.packages(c(
  "curl", "data.table", "digest", "ggplot2", "glmnet",
  "igraph", "jsonlite", "patchwork"
))
```

Full mode additionally uses Python 3 and `pyshp==2.3.1` for the traffic-map
robustness check. Install it in the project-local directory with:

```sh
python3 -m pip install --target .python -r requirements.txt
```

`quick` mode needs neither the R packages nor Python.

## Repository structure

```text
.
├── main.R                         # single entry point and mode selection
├── accidents-radars-replication.Rproj
├── codes/                         # ordered construction and estimation scripts
├── R/                             # shared geometry helpers
├── config/data_manifest.csv       # pinned URLs and SHA-256 checksums
├── data/                          # downloaded inputs (ignored by Git)
├── results/cached/                # archived, lightweight paper outputs
├── results/current/               # outputs from the selected run (ignored)
└── tests/                         # deterministic helper tests
```

Each pipeline step writes a separate log to
`results/current/replication_logs/`. The driver stops at the first failed step.
Use `Rscript main.R --mode=main --check-only` to verify the inputs, packages,
and scripts without estimating the models.

## Provenance and licences

Original code in this repository is released under the MIT License. Data are
not relicensed here. BAAC, official camera inventories, IGN BD TOPO extracts,
and national-road traffic data retain the attribution and Open Licence 2.0
terms documented in the data repository.
