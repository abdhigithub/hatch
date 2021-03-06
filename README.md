
[![Travis build status](https://travis-ci.org/abdhigithub/hatch.svg?branch=master)](https://travis-ci.org/abdhigithub/hatch)

[![Build status](https://ci.appveyor.com/api/projects/status/kd8ptwro18ak5imf/branch/master?svg=true)](https://ci.appveyor.com/project/abdhigithub/hatch/branch/master)

<!-- [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/hatch)](https://cran.r-project.org/package=hatch) -->
<!-- README.md is generated from README.Rmd. Please edit that file -->
hatch - Histogram matching After Tissue Classification to a Healthy control
---------------------------------------------------------------------------

Intensity Normalization in MRI brain images is am important pre-processing step for any rigirous statistical methodology, since the images are acquired in arbitrary units. Regular Histogram Matching (Nyul et. al, 1999) techniques tend to misclassify a significant amount of brain tissue to ensure that all the histograms look similar.

Hatch essentially sub-samples different tissues from the healthy portions of the brain and parse them through the piece-wise transformation which is applied to the entire brain. We hope that most of the inhomogeneity due to pathology will be maintained while trying to normalize healthy brain tissue, to render the intensities more compareable between different subjects. (This method was specifically designed for Patient MRI images with Glioblastomas)

The method is suitable for most modalities (T1, T2, FLAIR, T1(post contrast) etc.. ) as long as a suitable healthy template /control exists for that corresponding modality.

### Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("abdhigithub/hatch")
```

This is yet to be released on [CRAN](https://CRAN.R-project.org) repository.

### Example

I have included examples and a short tutorial in the [vignette here](https://github.com/abdhigithub/hatch/blob/master/vignettes/hatch_tutorial.Rmd).
