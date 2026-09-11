# cholera-alert-decisionmaking

Analyses to inform guidance on when and how OCV should be used in outbreak response settings

## About

This repository contains the analysis code for "A decision analytic framework for triggering cholera outbreak response based on early-case surveillance," which evaluates early cholera case-surveillance alert definitions as triggers for large-scale outbreak response using surveillance data from outbreak-prone locations across Africa.

## Reference

Alam C, Zheng Q, Perez-Saez J, Kim J-H, Azman AS, Lee EC. "A Decision Analytic Framework for Triggering Cholera Outbreak Response Based on Early-Case Surveillance." Unpublished manuscript.

## Data

Download the following files (which include only public data) and place them in the data directory to re-run analyses. We do not expect users will be able to exactly recreate the results in the original paper, which used a dataset including public and non-public data.

Repository: 

[Cholera surveillance time series in Africa from 2010 to 2023](https://osf.io/2ncf7/overview)

Files: 

- [Public_surveillance_dataset.parquet](https://osf.io/2ncf7/files/w8s7x)
- [Public_oubtreak_dataset.parquet](https://osf.io/2ncf7/files/tuhx5)
- [Public_surveillance_shapefiles.gkpg](https://osf.io/2ncf7/files/s6z8y)

## Dependencies

These pipelines require the [OutbreakExtractR](https://github.com/HopkinsIDD/OutbreakExtractR) package.

## Reproducing the analysis

Two pipeline scripts reproduce the full analysis:

- **`R/scripts/run_utility_pipeline.R`** — triggers alerts and runs the multidimensional utility analysis.
- **`R/scripts/run_bhm_pipeline.R`** — runs the Bayesian hierarchical model (BHM) analysis of implementation delay effects.

Run these scripts from the repository root, e.g.:

```r
source("R/scripts/run_utility_pipeline.R")
source("R/scripts/run_bhm_pipeline.R")
```

## Outputs

Generated HTML reports summarizing the results of each pipeline can be found in the `Notebooks` directory after running the scripts above.
