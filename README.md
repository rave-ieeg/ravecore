
# ravecore

<!-- badges: start -->
[![R-CMD-check](https://github.com/rave-ieeg/ravecore/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rave-ieeg/ravecore/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `ravecore` is to provide core infrastructure to read, process, and 
analyze intracranial electroencephalography and deep-brain stimulation in 
[`RAVE`](https://rave.wiki), a reproducible framework for analysis and 
visualization framework. This package supports [`BIDS`](https://bids.neuroimaging.io) 
or native file conventions to ingest signals in `HDF5`, `Matlab`, `EDF`, 
`BrainVision`, or `BlackRock` formats. For images, this package process images 
in `NIfTI` and `FreeSurfer` formats, providing brain imaging normalization to 
template brain, facilitate comprehensive electrode localization and 3D brain 
visualization.

See our official website for tutorials and documentations: 

* https://rave.wiki

To cite the package and 3D viewer in general

* Magnotti, J. F., Wang, Z., & Beauchamp, M. S. (2020). RAVE: Comprehensive open-source software for reproducible analysis and visualization of intracranial EEG data. NeuroImage, 223, 117341.

If you use `RAVE/YAEL` for localization or brain normalization, cite

* Wang, Z., Magnotti, J. F., Zhang, X., & Beauchamp, M. S. (2023). YAEL: Your Advanced Electrode Localizer. Eneuro, 10(10).


## Installation

Please check https://rave.wiki/posts/installation/installation.html for full installation guide.

For advanced developers, you can install the development version of `ravecore` with:

``` r
# install.packages("pak")
pak::pak("rave-ieeg/ravecore")
```


