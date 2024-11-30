#' edar: A Package for Code-Efficient Exploratory Data Analysis in R
#'
#' The `edar` package provides functions that allow for efficient exploratory
#' data analysis with minimal lines of code. It includes various functions for
#' checking the balance of covariates between control and treatment groups, 
#' analyzing the outputs of model estimations, creating summary tables and 
#' plots, and conducting robustness checks of model results (e.g., multiple 
#' imputation, post-stratification). These tasks are frequently performed 
#' by quantitative researchers, and this package facilitates their completion 
#' quickly and with minimal coding effort.
#'
#' For examples of workflows, see the vignette: `vignette("edar")`
#' 
#' @docType _PACKAGE
#' @name edar
NULL



#' A National Survey from Brazil
#'
#' This dataset is a subset of a national survey conducted in Brazil in 2013.
#' The survey measures individual preferences regarding interpersonal and 
#' interregional redistribution of income, as well as preferences for the 
#' centralization of political authority.
#'
#' @format A data frame with 700 rows and 16 columns:
#'
#' \describe{
#'   \item{gender}{factor; "male" and "female"}
#'   \item{educ}{factor; "high" if the individual completed high school or more, and "low" otherwise}
#'   \item{age}{integer; age in years}
#'   \item{inc}{numeric; individual's household income per capita}
#'   \item{inc.iht}{numeric; inverse hyperbolic transformation of income}
#'   \item{state}{factor; the state in which the individual resides}
#'   \item{region}{factor; broader macroregion designation}
#'   \item{state.dev}{numeric; average household per capita income in the state, computed using the 2013 Brazilian National Household Survey (PNAD)}
#'   \item{state.gini}{numeric; Gini coefficient of the state calculated using the 2013 Brazilian National Household Survey (PNAD)}
#'   \item{trust}{numeric; level of trust in the federal government}
#'   \item{treat}{numeric; treatment group designation: 0 for control group, 1 for low-dose treatment, or 2 for high-dose treatment. This variable is randomly generated for illustrative purposes only}
#'   \item{racial.frag.ratio}{numeric; ratio of racial fractionalization at the state level compared to national levels}
#'   \item{reduce.income.gap}{numeric; degree of agreement with the statement "The government should reduce the income gap between the rich and the poor"}
#'   \item{transfer.state.tax}{numeric; degree of agreement with the statement "The government should redistribute resources from rich to poor states"}
#'   \item{minimum.wage}{numeric; response to the question "Who should decide about minimum wage policy?" Options are "Each city should decide", "Each state should decide", and "Should be the same across the country"}
#'   \item{unemployment.policy}{numeric; response to the question "Who should decide about unemployment policy?" Options are "Each city should decide", "Each state should decide", and "Should be the same across the country"}
#'   \item{red.to.poor}{numeric; response to the question "Who should decide about policies to redistribute income to the poor?" Options are "Each city should decide", "Each state should decide", and "Should be the same across the country"}
#' }
#'
#' @usage data(survey)
#' @docType data
#' @source \url{http://web.fflch.usp.br/centrodametropole/}
"survey"



#' Data set with information about movies.
#'
#' @usage data(movies)
#' @docType data
#' @source \url{http://www2.stat.duke.edu/~mc301/data/movies.html}
"movies"
