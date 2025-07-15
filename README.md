# crop_planting_wild_pigs
R code for fitting mixed effects linear models analyzing the effect of wild pig distribution and management on crop planting area in the United States.

## crop_anom_multi_chain_only_pigs_spatial.R
Fits mixed effects linear model to crop planting anomaly data using nimble R package. Runs three MCMC chains in parallel and loops through five crop types in a for loop.

## crop_anom_validation.R
Calculates Bayesian p-values and R2 values for linear models using posterior samples.

## crop_anom_combined_coeff_plots.R
Creates dot and whisker plot of all model fixed effects' posterior distributions in single plot. Also creates trace plots for model parameters.

## crop_functional_response_plots_spatial.R
Creates functional response plots for significant pig effects. 

## yield_comparison_table.R
Pulls planting, yield, and market data for manuscript reporting.

## Functions

### clean_crop_dat.R
Collates, cleans, and scales crop planting and covariate data from multiple sources. 

### fun_response.R
Simulates and backtransforms independent variables and calaculates functional response for plotting.
