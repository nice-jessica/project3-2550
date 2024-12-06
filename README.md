# Statistical Simulation Study

This repository contains code for running statistical simulations to determine the optimal study design under budget constraints. The simulations compare normal and Poisson distributions using a variety of cost parameters.

## Overview

The project code includes multiple simulations:
- Estimating the mean squared error (MSE) of beta estimates under normal and Poisson models.
- Optimizing the study design by adjusting the number of clusters and observations per cluster to stay within a set budget.
- Comparing the impact of cost ratios on MSE.

## Installation

You need R and several R packages installed to run the simulations. Use the following command to install these packages:

```R
install.packages(c("ggplot2", "plotly", "gt", "dplyr", "lme4", "tidyverse", "parallel", "gridExtra"))
