#' edar: A package for code-efficient Exploratory Data Analysis in R
#'
#' The package provides functions that allows efficient exploratory
#' data analyses with few lines of code. It also contains some
#' functions to check balance of covariates among control and treatment
#' groups, analyse output of model estimation, create summary tables and
#' plots, and conduct robustness checks of the model results (multiple
#' imputation, post-stratification, etc). Quantitative researchers conduct
#' those tasks repeatedly. The package provides functions to conduct them
#' fast and with with minimum code.
#'
#' See vignette(edar) for examples of workflow
#' 
#' @docType package
#' @name edar
NULL


#' A National Survey from Brazil
#' 
#' The data set is a subset of a national suvery conducted in Brazil in 2013.
#' The survey measures preferences of individuals for interpersonal and interregional
#' redistribution of income as well as preferences for centralization
#' of political authority.
#'
#' @format A data frame with 700 rows and 16 columns:
#'
#' \describe{
#'  \item{gender}{factor with "men" and "woman"}
#'  \item{educ}{factor with "high" if the individual completed high school or more, and "low" otherwise}
#'  \item{age}{integer with age in years}
#'  \item{yi}{numeric variable with household income per capita}
#'  \item{yi.iht}{inverse hyperbolic transformation of yi}
#'  \item{state}{factor with the state in which the individual lives}
#'  \item{region}{factor with macroregion}
#'  \item{ys.mean}{average household percapita income in the state, computed using the 2013 Brazilian National Household Survey (PNAD)}
#'  \item{trust}{factor, "high" or "low" trust in the federal government}
#'  \item{treat}{numeric, 0 for control group or 1 for treatment group. It is a randomly generated variable for used for ilustration of the examples and vignettes only}
#'  \item{ys.gini}{numeric, Gini coefficient of the state computed using the 2013 Brazilian National Household Survey (PNAD)}
#'  \item{racial.frag.ratio}{numeric, racial fractionalization at the state over racial fractionalization at the national level}
#'  \item{reduce.income.gap}{factor, "A"=Agree, "A+"=Strongly Agree, "D"=Disagree, "D+"=Strongly Disagree, "N"=Neither Agree or Disagree that "Government should reduce income gap between rich and poor"}
#'  \item{transfer.state.tax}{factor, "A"=Agree, "A+"=Strongly Agree, "D"=Disagree, "D+"=Strongly Disagree, "N"=Neither Agree or Disagree that the "Government should redistribute resources from rich to poor states"}
#'  \item{minimum.wage}{factor, captures the answer to "Who should decide about the minimum wage policy?". The levels are "Each city should decide", "Each state should decide", "Should be the same accros the country"}
#'  \item{unemployment.policy}{factor, captures the answer to "Who should decide about the unemployment policy?". The levels are "Each city should decide", "Each state should decide", "Should be the same accros the country"}
#'  \item{red.to.poor}{factor, captures the answer to "Who should decide about policies to redistribute income to poor?". The levels are "Each city should decide", "Each state should decide", "Should be the same accros the country"}
#' }
#'
#' @usage data(edar_survey)
#' @docType data
#' @source \url{http://web.fflch.usp.br/centrodametropole/}
"edar_survey"



#' Data set with information about movies.
#'
#' @usage data(edar_movies)
#' @docType data
#' @source \url{http://www2.stat.duke.edu/~mc301/data/movies.html}
"edar_movies"
