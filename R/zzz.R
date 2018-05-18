# required (by devtools) to link the cpp code 
#' @importFrom magrittr "%>%"
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
    ## unload the Cpp files
    library.dynam.unload("edar", libpath)
}
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage('
## =====================================================
## edar: Exploratory Data Analysis in R
## =====================================================
')
}
