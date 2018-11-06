# CHANGES IN mosaicModel VERSION 0.3.0

- bootstrap capabilities added to mod_eval, mod_effect, and mod_plot

# CHANGES IN mosaicModel VERSION 0.3.1

- re-implementation of interface between mosaicModel and model-fitting packages
- vignette added
- bug fixes (of course)
- moved interval calculations to mosaicCore package, revising them to work with `mosaicCore::df_stats()`
- mod_eval() handles NULL models (e.g. response ~ 1). mod_effect() will throw an error for a null model.

# CHANGES IN mosaicModel VERSION 0.3.2

- added methods for nls models
- bug fixes
- added block-wise cross validation for time series

