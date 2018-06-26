Overview
========

Exploratory Data Analysis in R (`edar`) is a R package designed to
reduce the amount of code needed to do EDA in R. It provides a set of
wrap functions that incapsulate some
[tidyverse](https://www.tidyverse.org/) functionalities.

Instalation
===========

``` {.r .rundoc-block rundoc-language="R" rundoc-exports="code"}
devtools::install_github("DiogoFerrari/edar")
# If you don't want to update the dependencies, use: (you may need to install some dependencies manually)
devtools::install_github("DiogoFerrari/edar", dependencies=F)

```

Usage
=====

``` {.r .rundoc-block rundoc-language="R" rundoc-exports="both" rundoc-results="output" rundoc-hlines="yes" rundoc-colnames="yes"}
library(edar)
library(magrittr)

data(edar_survey)
data = edar_survey
# help(data)

## summarise all numerical variables
data %>% summarise_alln(., group=NULL, weight=NULL, spread=F)
data %>% summarise_alln(., group="gender", weight=NULL, spread=F)

## summarise all categorical variables
data %>% summarise_allc(., group=NULL)
data %>% summarise_allc(., group="gender")

## bundle all cateorical variables based on their categories and summarise them
tab = data %>% summarise_allcbundle(., group=NULL)
tab
tab$Table[[1]]  ## Table with counts
tab$Tablep[[1]] ## Table with percentages
tab$Tablel[[1]] ## Table with counts and percentages

## check balance of covariates between two groups (ex: treatment vs control, see Imbens, G. W., & Rubin, D. B., Causal inference in statistics, social, and biomedical sciences: an introduction (2015), : Cambridge University Press.) 
data %>% ebalance(., treatmentVar='treat')

```

See other functions in the package vignette.

``` {.r .rundoc-block rundoc-language="R" rundoc-exports="code"}
vignette(edar)
```

More information
================

See [this webpage](https://dioferrari.com/software/) and
[pdf](https://dioferrari.files.wordpress.com/2018/06/edar_vignette.pdf)
with examples.
