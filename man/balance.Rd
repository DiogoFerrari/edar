% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eda.R
\name{balance}
\alias{balance}
\title{Check balance of pre-treatment variables across treatment groups}
\usage{
balance(data, treat = NULL, reference = NULL, ttest = F, pscore = T)
}
\arguments{
\item{data}{A data frame that contains a dummy indicator
variable for the treatment and control
groups.}

\item{treat}{A string representing the name of the indicator
variable for the treatment.}

\item{ttest}{A boolean indicating whether a t-test of the
difference in means between treatment and control
groups should be computed.}

\item{pscore}{A boolean indicating whether the propensity score
should be computed.
**TBI**}
}
\value{
The function returns a list with named elements. The
        element \code{asmd} contains a table with the
        Absolute Standardized Mean Difference between groups.
        and control groups. The element \code{pscore} contains
        the propensity score of all observations. The element
        \code{lpscore} of the list is the logarithm of the propensity score.
}
\description{
Check balance of pre-treatment variables across treatment groups
}
\details{
The statistics computed here are recommended in
\itemize{
 \item Imbens, G. W., & Rubin, D. B. (2015). Causal inference in statistics, social, and biomedical sciences: An introduction. Cambridge University Press.
}
}
\examples{
data(survey)

balance(survey, treat = 'treat')

# or with a pipe
# survey \%>\% balance(., treat = 'treat')

}
