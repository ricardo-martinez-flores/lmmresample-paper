# Code and results for the lmmresample article

Code and results that reproduce the simulation study, the analysis of the
pupillometry data set and the figures of the article

> Martínez-Flores, R., & Supèr, H. lmmresample: Valid inference for mixed
> models of trial-structured physiological data. A tutorial.

which presents the R package
[lmmresample](https://github.com/ricardo-martinez-flores/lmmresample):
permutation and bootstrap inference for linear mixed models of trial-structured,
densely sampled data.

## Contents

| Path | Purpose |
|---|---|
| `simulation_lmmresample.R` | Simulation study: false-positive rates, power, interval coverage, families of tests, pairwise comparisons, diagnostics and computing time |
| `analysis_real_data.R` | Analysis of the pupillometry data set with the package and the usual alternatives |
| `figures_lmmresample.R` | Figures of the article and of the supplement, drawn from the results |
| `lmmresample_simulations/` | Results of the simulation study (summaries as CSV, replicate-level results as gzipped CSV) |
| `lmmresample_realdata/` | Results of the analysis of the pupillometry data set (CSV) |
| `diagrams/` | Figures 1 and 8 (editable PowerPoint files and the scripts that build them) |
| `Dockerfile` | R environment used to run the simulations and analyses |
| `data/` | Location of the data file (not included; see below) |

## Data

The data are the preprocessed, baseline-corrected accepted trials of a visual
oddball task in 67 older adults with mild cognitive impairment, recorded at
33 Hz, published on Zenodo:

> Anonymized pupillometry data from older adults with mild cognitive
> impairment during a visual oddball task (33 Hz).
> https://doi.org/10.5281/zenodo.23160276

Download `preprocessed_accepted_trials.csv.gz` into `data/`. Another location
can be given with `LMM_DATA`.

The participants come from two cohorts (cross-sectional and the
pre-intervention recordings of a longitudinal study). To protect anonymity,
the published data do not identify the cohort. The analyses that involve the
cohort (cohort × stimulus, cohort × stimulus × time) run only when the data
file contains a `cohort` column; with the published file they are skipped.
The reported bootstrap intervals resample participants without stratifying by
cohort (`LMM_STRATA=none`), so all other results can be reproduced with the
published file up to Monte Carlo error.

## Requirements

R (≥ 4.1) with the packages lmmresample, lme4, lmerTest, mgcv, future and
future.apply. The figures also need ggplot2, ragg, systemfonts and patchwork.
Unless `LMM_SKIP_INSTALL=1` is set, the scripts install the packages they
need.

The simulations and analyses were run in the Docker image defined in
`Dockerfile`, which pins the version of lmmresample used
(see `run_info.txt` in each results folder):

```sh
docker build -t lmmresample-paper .
docker run --rm -v "$PWD":/work -w /work -e LMM_SKIP_INSTALL=1 \
  -e LMM_WORKERS=30 lmmresample-paper Rscript simulation_lmmresample.R
```

## Running

The figures can be drawn directly from the results in this repository:

```sh
Rscript figures_lmmresample.R      # -> figures/, figures/paper/, figures/supplementary/
```

The pupil curves of Figure 9 also need the data file in `data/`.

To recompute the results, run the two analysis scripts. Each runs from start
to end with `Rscript`, writes its results as CSV files, and can be interrupted
and run again: finished steps are kept.

```sh
Rscript simulation_lmmresample.R   # -> lmmresample_simulations/
Rscript analysis_real_data.R       # -> lmmresample_realdata/
```

| Variable | Script | Meaning |
|---|---|---|
| `LMM_WORKERS` | simulation, real data | parallel R sessions (default: available cores − 1) |
| `LMM_OUT` | simulation, real data | output folder |
| `LMM_DATA` | real data | data file |
| `LMM_STRATA` | real data | `none` to resample participants without stratifying by cohort |
| `LMM_RUN` | simulation | comma-separated subset of parts: `example`, `single`, `timecourse`, `triple`, `bootstrap`, `gca`, `families`, `posthoc`, `diagnostics`, `timing` |
| `LMM_TEST` | simulation | `1` for a quick check with very few replicates |
| `LMM_BASE` | figures | folder containing both results folders |
| `LMM_SKIP_INSTALL` | simulation, real data | `1` when the packages are already installed |

Both computations are long. On a 128-core server, the simulation study took
several days with 30 to 60 parallel sessions, and the analysis of the real
data about three days with 30 sessions, most of it in the calibration of the
growth curve models (`perm_calibrate()`). Seeds are fixed, so results do not
depend on the number of sessions.

## Citation

Please cite the article and the package. Citation metadata for this
repository are in `CITATION.cff`.

## License

The code and results are released under the MIT License (see `LICENSE`). The
data are distributed separately under their own license (CC BY 4.0).
