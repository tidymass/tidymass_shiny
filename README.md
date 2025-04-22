# Shinyapp for tidymass

<-- logo -->

A user-friendly shinyapp designed for metabolomics data analysis within the **tidymass** framework.

# TODO list

- [ ] Single ion mode ;
- [ ] EIC for single peak;
- [ ] Implementation of Flexible Tool Functionality;
- [ ] Functional Analysis (e.g., Statistical Analysis, Metabolic Pathway Analysis);

# Quick start

## Install

```  r
##> for chinese users:
##> options("repos" = c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
##> options(BioC_mirror="https://mirrors.westlake.edu.cn/bioconductor")


if (!require('remotes')) install.packages('remotes');
if (!require('tidyverse')) install.packages('tidyverse');
if (!require('tidymass')) {
  source("https://www.tidymass.org/tidymass-packages/install_tidymass.txt");
  install_tidymass(from = "tidymass.org")
};
if (!require('shinyFiles')) remotes::install_github('thomasp85/shinyFiles');
if (!require('shinyWidgets')) remotes::install_github("dreamRs/shinyWidgets");
if (!require('shiny')) install.packages('shiny');
if (!require('bsicons')) install.packages('bsicons');
if (!require('bslib')) install.packages('bslib');
if (!require('plotly')) install.packages('plotly');
if (!require('colourpicker')) install.packages('colourpicker');
if (!require('tidymassshiny')) remotes::install_github('tidymass/tidymass_shiny')
```

# Start

```r
library(tidyverse)
library(tidymass)
library(tidymassshiny)
library(plantmdb) ## for test use
run_tidymass_shiny()
```

# Reference

# Citation
