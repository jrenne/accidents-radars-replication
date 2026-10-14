# Fixed speed cameras and injury crashes in France

This repository contains the replication code and archived results for the
paper on fixed speed cameras and injury crashes in France. The reusable data
are kept separately in
[`jrenne/accidents-radars-france`](https://github.com/jrenne/accidents-radars-france).

## Reproduce the results

Open `accidents-radars-replication.Rproj`, choose the options at the top of
`main.R`, and run:

```r
source("main.R")
```

Three modes are available:

| Mode | Result |
|---|---|
| `quick` | Restore the archived paper results; no download or estimation |
| `main` | Download the inputs and reproduce the preferred results |
| `full` | Also reproduce the robustness checks |

The paper uses 1,999 cluster-bootstrap repetitions. A small value can be used
for a test run. The spatial inputs are frozen for reproducibility; set
`rebuild_spatial_inputs <- TRUE` only to reconstruct them from the source
files.

Long computations are therefore optional: the default `quick` mode uses the
lightweight cached outputs in `results/cached/`.

## Data and requirements

The code verifies and loads versioned inputs from release `v1.1.0` of the data
repository. Downloads are stored under `data/` and ignored by Git. A sibling
checkout of `accidents-radars-france` is used automatically when available.

The analysis was prepared with R 4.5. Install the required packages once:

```r
install.packages(c(
  "curl", "data.table", "digest", "ggplot2", "glmnet",
  "igraph", "jsonlite", "patchwork"
))
```

Full mode also requires Python 3 with `pyshp==2.3.1` for one traffic-data
robustness check. Quick mode requires neither the R packages nor Python.

## Structure

```text
main.R                    options and single entry point
codes/                    data construction, estimation, and figures
R/                        shared geometry functions
config/                   data and cached-result checksums
results/cached/           archived paper outputs
results/current/          outputs from the selected run (ignored by Git)
tests/                    deterministic checks
```

Original code is released under the MIT License. The source data retain the
attribution and licence terms documented in the data repository.
