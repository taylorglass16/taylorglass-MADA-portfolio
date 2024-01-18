---
author:
- |
  Wei Ma$^1$, Xiaoqing Ye$^1$, Fuyi Tu$^1$, Feifang Hu$^2$\
  1 Renmin University of China\
  2 George Washington University
title: "**Overview of Supplementary Codes**"
---

# Description

In the article ***carat**: An R package for Covariate-Adaptive
Randomization in Clinical Trials*, all the analysis was done in R or
Rcpp. The corresponding codes are provided to reproduce outputs of the
examples in Section 3, apply the package to CBASP data in Section 4, and
do benchmark as well as analysis in Section 5.

# Instructions

Only the scripts of *reproducibility-material.R* and *S3_carat-CLI.R* need to be performed in R for the audience. By running these scripts,
the audiences are able to reproduce the outputs of the examples in
Section 3, and of the exemplary application in Section 4, and generate
Figure 7, Figure 8, and Table 4 in Section 5.

Note that, on one hand, due to the different performances from computers
to computers, or from servers to servers, the run-time of the functions
can not be reproducible exactly, for example, Figure 7 and
Table 4. However, the tendency with different specific values and
differences of the run-time can be understood clearly.

On the other hand, *S3_carat-CLI.R* needs to be run in an interactive
session.

# Files

R version 4.1.3 was used, and the applications of shared files are as follows.


-   *AuxPkg_2.0.tar.gz*.

    It is an auxiliary package which includes the auxiliary functions for generating Figure 7, Figure 8, and Table 4 in *reproducibility-material.R*. Note that the functions coded by [Ma et
    al. (2020)](https://amstat.tandfonline.com/doi/abs/10.1080/01621459.2019.1635483?journalCode=uasa20) are included in this package.


-   *carat_2.2.0.tar.gz*.

    It is the the package carat which is used in the paper.


-   *caratOMP_2.2.0.tar.gz*.

    It is the OpenMP-supported version of the package carat.
    
-   *CBASP.csv*.

    It is the original data which is used in Section 4.

    
-   *reproducibility-material.R*.

    It contains the codes of examples presented in Section 3 (except those of the command-line interface), the codes for realizing an exemplary application with nefazodone CBASP data in Section 4, and the codes for generation of Figure 7-8 and Table 4 in Section 5.

-   *S3_carat-CLI.R*.

    It contains the codes of command-line interface in Section 3. Note
    that it must run in an interactive session. Details can be found in
    the script.

