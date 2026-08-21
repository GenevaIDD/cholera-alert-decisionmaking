# cholera-alert-decisionmaking

Analyses to inform guidance on when and how OCV should be used in outbreak response settings

## About

This repository contains the analysis code for "A decision analytic framework for triggering cholera outbreak response based on early-case surveillance," which evaluates early cholera case-surveillance alert definitions as triggers for large-scale outbreak response using surveillance data from outbreak-prone locations across Africa.

## Reference

Alam C, Zheng Q, Perez-Saez J, Kim J-H, Azman AS, Lee EC. "A Decision Analytic Framework for Triggering Cholera Outbreak Response Based on Early-Case Surveillance." Unpublished manuscript.

[Preprint available here.](https://www.medrxiv.org/content/10.64898/2026.07.16.26358045v1)

## Data

We include a version of the original dataset with only publicly available data to re-run analyses. We do not expect users will be able to exactly recreate the results in the original paper, which used a dataset including public and non-public data.

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
