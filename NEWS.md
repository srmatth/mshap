# mshap 0.1.0.9001

## Clean Up

- Add formatting to the X and Y axis of the summary plot, also change the dash to an actual negative sign.

# mshap 0.1.0.9000

# mshap 0.1.0

This is the first release of mSHAP on CRAN! It is an implementation of the mSHAP algorithm as described by Matthews and Hartman (2021) [(See Paper Here)](https://arxiv.org/abs/2106.08990), which allows for the computation of SHAP values on two-part models. This package also includes useful functions for visualizing mSHAP and SHAP values. See the README file on [the github page](https://github.com/srmatth/mshap) for more examples and use cases.

## Enhancement

- Some changes to paper code that is more accurate
- Limit the number of variables that are plotted in the plotting functions
- Add data (.buildignore'd) and code pertaining to mshap paper
- Added examples to all function documentation.
- Added capability of observation plot to have character values in the `variable_values` data frame.
- Changed `{ggplot2}`, `{ggbeeswarm}` and `{tidyr}` to imports, instead of suggests.
- Added tests for all functions (close to 100% codecov).
- Added documentation to all functions

## Feature

- Added a vignette called `mshap_plots` that documents different ways to customize and use the plots.
- Added parameters `colorscale`, `legend.position`, `font_family`, and `title` to the `summary_plot()` function, for greater ease of customization.
- Added parameters `fill_colors`, `connect_colors`, `expected_color`, `predicted_color`, `title`, and `font_family` to the `observation_plot()` function for greater ease of customization.
- Added Travis CI pipeline functionality.
- Added a helper function to make the `mshap` function more robust
- Finished the vignette

## Clean Up

- Made the warning message in `observation_plot()` go away (it was occurring because there was only one tick mark on each axis, so I added an invisible one)
- Adding dependencies and other items to ensure that there are no notes/warnings in `R CMD CHECK`

## Bug Fix

- Fixed bug in `mshap()` function where data frames with the same number of columns but different names were not getting flagged to add additional columns even when `shap_*_names` was specified.
- Fix bug that was causing the expected values to be off if the list-mshap was passed as the second argument to `mshap()`
- Fixed bug in the summary_plot function with where the names were assigned
