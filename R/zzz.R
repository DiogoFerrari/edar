# required (by devtools) to link the cpp code 
#' @importFrom magrittr "%>%"
#' @importFrom dplyr n
#' @importFrom mice mice
NULL

.onLoad <- function(libname, pkgname)
{
    ## set options
    op.default <- options()
    op.edar <- list(
        edar.names       = "edar: Exploratory Data Analysis in R",
        edar.desc.author = 'person("Diogo", "Ferrari", email="diogoferrari@gmail.com", role=c("aut", "cre"))',
        edar.dec.license = 'MIT'
    )
    toset <- !( names(op.edar) %in% names(op.default))
    if(any(toset)) options(op.edar[toset])

    invisible()
}

.onUnload <- function(libpath)
{
}
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage('
## =====================================================
## edar: Exploratory Data Analysis in R
## =====================================================
')
}

## global varibles for dplyr (used only so that the check ignores it, it does not actually creates global variables)
## -------------------------
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        "name",
                                                        ".rownames",
                                                        "est",
                                                        "ylabel",
                                                        "n",
                                                        "Percentage",
                                                        "label",
                                                        "..level..",
                                                        "var",
                                                        "value",
                                                        "KS.test",
                                                        "KS.text",
                                                        "d1",
                                                        "na",
                                                        "Variable",
                                                        "Category.Labels",
                                                        "Categories.Labels",
                                                        "Variable",
                                                        "Variables",
                                                        "Table",
                                                        "N.Variables",
                                                        "sd",
                                                        "N",
                                                        "Statistic",
                                                        "Stat",
                                                        "confint",
                                                        "y",
                                                        "term",
                                                        "estimate",
                                                        "std.error",
                                                        "term",
                                                        "deviance",
                                                        "p.value",
                                                        "sig",
                                                        "coef",
                                                        "conf.low.hc",
                                                        "conf.high.hc",
                                                        "d1",
                                                        "d2",
                                                        "estimation",
                                                        "tidye_func",
                                                        "statistic",
                                                        "conf.low",
                                                        "conf.high",
                                                        "CI",
                                                        "id",
                                                        "model.label",
                                                        "Model",
                                                        "Cats",
                                                        "digits",
                                                        "variable",
                                                        "time.value",
                                                        "var__to__interpolate__",
                                                        "time.var.__tmp",
                                                        "var__interpolated__.ili",
                                                        "var__interpolated__.isp",
                                                        "vars",
                                                        "cat.label",
                                                        "old.var"
                                                        ))
