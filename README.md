# Introduction to methods in environmental epidemiology

Robbie M. Parks and Cascade Tuholske. With thanks to Marianthi-Anna Kioumourtzoglou and Garyfallos Konstantinoudis for previous work together informing this material.

## Introduction

GitHub repo through which we're developing and sharing materials for the Introduction to methods in environmental epidemiology Workshop, taking place in person during July 21st-23rd 2025.

## Notes for attendees

The workshop will be a series of lectures and interactive supervised lab sessions. We hope it's informative and fun!

We will be using `Posit (RStudio) Cloud`, which assumes knowledge of `R` and `RStudio`. We will also ask you to pull the final versions of the `GitHub` repo to your Cloud account. The basics of doing this are in a previously-created guide found via [another guide repo](https://github.com/rmp15/rstudio_cloud_tutorial/tree/main).

This Bayesian component of this workshop is largely written in [`NIMBLE`](https://r-nimble.org/).

Cascade's section of the workshop will be in `Python`.

Below is the set of labs to follow throughout the three days:

### Day 1 (Monday July 21st, 2025)

| Time | Activity |
|----|----|
| 9:00 - 10:00 | Introduction to Methods in Environmental Epidemiology 1 (Lecture) - Theories and assumptions, Data structuring |
| 10:00 - 11:00 | Introduction to Methods in Environmental Epidemiology 1 (Hands-on Lab) - Theories and assumptions, Data structuring |
| 11:00 - 11:15 | Break |
| 11:15 - 12:15 | Introduction to Methods in Environmental Epidemiology 2 (Lecture) - Model likelihood structures (Normal, Poisson, Bernoulli, Binomial) |
| 12:15 - 13:15 | Lunch |
| 13:15 - 14:15 | Introduction to Methods in Environmental Epidemiology 2 (Hands-on Lab) - Model likelihood structures (Normal, Poisson, Bernoulli, Binomial) |
| 14:15 - 15:15 | Introduction to Methods in Environmental Epidemiology 3 (Lecture) - Quantile regression |
| 15:15 - 15:30 | Break |
| 15:30 - 16:30 | Introduction to Methods in Environmental Epidemiology 3 (Hands-on Lab) - Quantile regression |
| 16:30 - 17:00 | Questions and Wrap-up |

### Day 2 (Tuesday July 22nd, 2025)

| Time | Activity |
|----|----|
| 9:00 - 10:00 | Modeling Non-linear and Delayed Time Series Effects 1 (Lecture) - Non-linear exposure-response curves |
| 10:00 - 11:00 | Modeling Non-linear and Delayed Time Series Effects 1 (Hands-on Lab) - Non-linear exposure-response curves |
| 11:00 - 11:15 | Break |
| 11:15 - 12:15 | Modeling Non-linear and Delayed Time Series Effects 2 (Lecture) - Case crossover design , Time series design |
| 12:15 - 13:15 | Lunch |
| 13:15 - 14:15 | Modeling Non-linear and Delayed Time Series Effects 2 (Hands-on Lab) - Case crossover design , Time series design |
| 14:15 - 15:15 | Modeling Non-linear and Delayed Time Series Effects 3 (Lecture) - Distributed lag non-linear models (DLNM) , Case crossover with hybrid DLNM |
| 15:15 - 15:30 | Break |
| 15:30 - 16:30 | Modeling Non-linear and Delayed Time Series Effects 3 (Hands-on Lab) - Distributed lag non-linear models (DLNM). Case crossover with hybrid DLNM |
| 16:30 - 17:00 | Questions and Wrap-up |

### Day 3 (Wednesday July 23rd, 2025)

| Time | Activity |
|----|----|
| 9:00 - 10:00 | Introduction to Bayesian Methods 1 (Lecture) - Brief history of Bayesian methods, Overview of Bayesian methods |
| 10:00 - 11:00 | Introduction to Bayesian Methods 1 (Hands-on Lab) - Brief history of Bayesian methods, Overview of Bayesian methods |
| 11:00 - 11:15 | Break |
| 11:15 - 12:15 | Introduction to Bayesian Methods 2 (Lecture) - Bayesian regression, Hierarchical modelling |
| 12:15 - 13:15 | Lunch |
| 13:15 - 14:15 | Introduction to Bayesian Methods 2 (Hands-on Lab) - Bayesian regression, Hierarchical modelling |
| 14:15 - 15:15 | Introduction to Heat Exposures 1 (Cascade) |
| 15:15 - 15:30 | Break |
| 15:30 - 16:30 | Introduction to Heat Exposures 2 (Cascade) |
| 16:30 - 17:00 | Questions and Wrap-up |

## Notes for those working on the repo

### Using `pre-commit`

Run `pre-commit install` to install the hooks. You now won't be able to commit until you pass the hooks. These (among other things) automatically format files and prevent us from committing ugly code. For more details, see the main [docs](https://pre-commit.com/) and the `R` [docs](https://lorenzwalthert.github.io/precommit/).

### Using `renv`

`renv` maintains consistency between users' `R` environments. Run `renv::restore()` and the environment will be downloaded into the repository based on the `renv.lock` file. If you want to add a packages to the lockfile, install the package and then run `renv::snapshot()`. For more details, see the [docs](https://rstudio.github.io/renv/articles/renv.html).

### Using `Quarto` for presentations

Quarto is pretty cool. I won't bore you, but have a look at the [docs](https://quarto.org/docs/guide/). Here, we're using it for [presentations](https://quarto.org/docs/presentations/revealjs/). It's designed by the folks at `RStudio`, so you `R` folk will be happy. Make a `.qmd` file and run `quarto render *.qmd` to generate the `html`, which you can open in browser. We can get fancy and import our own `css` to have a consistent theme for out presentations.
