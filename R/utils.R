
.get_controls <- function(X)
{
    return(paste(X, collapse=' + '))
}

.signif_transformer <- function(digits = 3)
{
    force(digits)
    function(text, envir) {
        x <- identity_transformer(text, envir)
        if (is.numeric(x)) {
            signif(x, digits = digits)
        } else {
            x
        }
    }
}

.str_replace <- function(s, replace)
{
    res = vapply(s,
                 function(x) ifelse(x %in% names(replace),
                                    sub(x=x, pattern=names(replace[x]), replacement=replace[x]),
                                    x),
                 FUN.VALUE = '')
    return(res)
}

.pvalue_stars <- function(pvalue=NULL, stars=list('+'=0.1, '*'=0.05, '**'=0.01, '***'=0.001), keys=F)
{
    if (!keys) {
        stars = append(stars, list(' '= Inf))
        res = which.min(unlist(stars[pvalue<=stars]))
        res = names(res)
    }else{
        keys = sort(unlist(stars), T)
        res = paste0(names(keys),"p<", keys, collapse = '; ')
    }
    return(res)
}


