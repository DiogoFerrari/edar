## {{{ multiple inputation }}}

## {{{ docs }}}
#' Multiple imputation
#'
#' This is a wrap function for the Multiple Imputation package mice.
#'
#' @param data a data.frame
#' @param RHS.formula a string with the RHS of the regression model
#' @param dep.vars a string vector with the names of the dependent variables.
#' @param ind.vars a string vector with the names of the independent variables.
#' @param digits integer, number of significant digits to return in the table
#' @inheritParams mice::mice
#'
#' @return The function returns a list with the summary output of the models estimated after the multiple imputation is performed
#'
#' @details See help(mice)
#'
#' @examples
#' library(magrittr)
#'
#' data = tibble::data_frame(x1 = rnorm(200,3,1),
#'                           x2 = rexp(200),
#'                           cat.var  = sample(c(0,1), 200, replace=TRUE),
#'                           cat.var2 = sample(letters[1:4], 200, replace=TRUE),
#'                           y1 = 10*x1*cat.var+rnorm(200,0,10) +
#'                               3*x2*(6*(cat.var2=='a') -3*(cat.var2=='b') +
#'                                     1*(cat.var2=='c') +1*(cat.var2=='d')),
#'                           y2 = -10*x1*cat.var+rnorm(200,0,10) +
#'                               10*x2*(3*(cat.var2=='a') -3*(cat.var2=='b') +
#'                                      1*(cat.var2=='c') -1*(cat.var2=='d'))
#'                           )  %>%
#'     dplyr::mutate(cat.var=as.factor(cat.var)) 
#' data$x1[sample(1:nrow(data), 10)] = NA
#' 
#' 
#' formula = "x1*cat.var+x2*cat.var2"
#' imp = emultimputation(data, formula,  dep.vars = c("y1", "y2"),
#'                       ind.vars=c("x1", "x2", "cat.var", "cat.var2"))
#' imp
#' @export
## }}}
emultimputation <- function(data, RHS.formula, dep.vars, ind.vars, m=5, maxit=50, method='pmm', seed=500, digits=4)
{
    formula = paste0("y ~ ",  formula)

    models.imp.pooled.final <- list()
    i=1
    for (i in 1:length(dep.vars)){
        dat = data[,c(dep.vars[i], ind.vars)]
        names(dat)[1]='y'

        utils::capture.output(imputed_dat <- mice::mice(dat, m=m, maxit = maxit, method = method, seed = seed))
        if (length(unique(dat$y))==2 & all(unique(dat$y) %in% c(0,1))) {
            models.imp = with(imputed_dat, expr=stats::glm(formula=stats::formula(formula), family='binomial'))
        }else{
            models.imp = with(imputed_dat, expr=stats::lm(formula=stats::formula(formula)))
        }
        models.imp.pooled = mice::pool(models.imp)
        models.imp.pooled.final[[i]] = base::summary(models.imp.pooled) %>%
            broom::tidy(.) %>%
            dplyr::rename(term=.rownames, estimate=est, p.value="Pr...t..", low.95='lo.95', high.95='hi.95') %>%
            dplyr::mutate_if(is.numeric, round, digits=digits)
    }
    names(models.imp.pooled.final) = dep.vars
    return(models.imp.pooled.final)
}

## }}}

## {{{ post-stratification weights }}}

## {{{ docs }}}
#' Wrap function for post-stratification weights
#'
#' This is a wrap function to compute post-stratification weights based on simple sample design (probability sampling)
#'
#'
#' @param data a data frame with the data. The name of the (post) stratification variables and their categories must match the data provided in \code{pop.prop}.
#' @param pop.prop a data frame with the population frequencies. Each column must be a (post) stratification variable. It must include a column \code{Freq} with the percentage of the population in each strata. The name of the stratification variables and their categories must match those provided in the data
#' @param strata a formula with the name of the variables for stratification. See \code{\link[survey]{postStratify}} for details
#'
#' @return It returns a list with two elements. The first element is the weight, the second is the trimmed weights. See \code{\link[survey]{trimWeights}} for details
#'
#'
#' @export
## }}}
epoststrat <- function(data, pop.prop, strata)
{
    options(warn=-1)
    on.exit(options(warn=0))
    ## survey object
    data.svy  = survey::svydesign(ids= ~ 1, data=data)
    data.svy.rake = survey::postStratify(design     = data.svy,
                                         strata     = strata ,
                                         population = pop.prop, partial = T)
    data.svy.rake.trim <- survey::trimWeights(data.svy.rake, lower=0, upper=5, strict=TRUE) 
    return(
        list(weights = stats::weights(data.svy.rake), 
             weights.trimmed = stats::weights(data.svy.rake.trim))
        )
}


## }}}

## {{{ power and sample size }}}

## {{{ docs }}}

#' Compute power of a test
#'
#' This function compute power of a test or ideal sample size given desired power of a test
#'
#' @param mu1 a numeric vector with expected observed proportion of the first group. It must be between 0 and 1
#' @param mu2 a number between 0 and 1 with expected observed proportion of the second group.
#' @param power_ideal the desired power of the test of difference between proportions
#' @param n.current the current/planned sample size
#' @param n1.current the current/planned sample size for the first group
#' @param n2.current the current/planned sample size for the second group
#' @param lines boolean, if \code{TRUE} the plot generated by the function will display lines at current and ideal values of the power of the test
#' @param title string, title of the plot 
#'
#' @return It returns a table with the power and sample size as well as a plot with the information
#'
#' @examples
#'
#' epower(c(.2),.4, .8, 10)
#' epower(c(.2,.5),.4, .8, 10,30)
#' 
#' @export
## }}}
epower <- function(mu1=NULL,mu2=NULL, power_ideal=.8, n.current=NULL, n1.current=NULL, n2.current=NULL, title = NULL, lines=T){
    
    ESs  = pwr::ES.h(p1 = mu1, p2 = mu2) ## d: expected effect size over the pooled expected standard deviation
    power=list()
    n_ideal=c()
    power_current=c()

    for (i in 1:length(ESs)){
        power_current[i] = pwr::pwr.2p.test(h=ESs[i], n=n.current, alternative='two.sided')$power
        n_ideal[i]       = pwr::pwr.2p.test(h=ESs[i], power=power_ideal, alternative='two.sided')$n
        n_max            = ifelse(power_current[i] >1, n.current, pwr::pwr.2p.test(h=ESs[i], power=.99, alternative='two.sided')$n)
        N                = seq(1,n_max, length=20)
        power[[i]] = cbind(power=pwr::pwr.2p.test(h=ESs[i], n=N, alternative='two.sided')$power, n = N)
    }
    ES = tibble::data_frame(mu1 = mu1, mu2=mu2, ES = ESs, power_ideal=power_ideal, n_ideal=n_ideal, power_current=power_current, n_current=n.current, n_and_power=power)

    layout = .edar_get_layout(length(ES$ES))
    graphics::par(mfrow=c(layout))
    if (!is.null(title)){
        graphics::par(mar=c(4, 4, 6, 1))
    }else{
        graphics::par(mar=c(4, 4, 3, 1) )
    }
    graphics::par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mgp = c(2,.6,0))
    for (i in 1:length(ESs)){
        graphics::plot(x=ES$n_and_power[[i]][,'n'], ES$n_and_power[[i]][,'power'], type='l', col="#00000044", pch=20, cex=2, xlab='Sample Size', ylab='Power', ylim=c(0,1))
        graphics::points(x=n.current, y=power_current[i], col='red', pch=20, cex=3)
        graphics::points(x=ES$n_ideal[i], y=ES$power_ideal[i], col='blue', pch=20, cex=3)
        text_current = paste0('Current Design\nN=',n.current,'\nPower=',round(power_current[i],2))
        text_ideal   = paste0('Ideal Design\nN=',ceiling(ES$n_ideal[i]),'\nPower=',round(ES$power_ideal[i],2))
        ## text(x=ES$n_ideal[i], y=ES$power_ideal[i],col='blue', pch=1, cex=.8, labels=text_ideal, pos=4,offset=2)
        ## text(x=n.current, y=power_current[i], col='red', pch=1, cex=.8, labels=text_current, pos=4,offset=2)
        title(paste0('Effect size : ', round(ES$ES[i],2),  sep=''), cex.main=1.2)
        if (lines){
            ## abline(v=ES$n_ideal[i], lty=2, col='blue')
            ## abline(v=n.current, lty=2, col='red')
            graphics::segments(x0=ES$n_ideal[i], y0=0,y1=ES$power_ideal[i], lty=2, col="blue")
            graphics::segments(x0=0,x1=ES$n_ideal[i], y0=ES$power_ideal[i], lty=2, col="blue")
            graphics::segments(x0=n.current, y0=0,y1=power_current[i], lty=2, col="red")
            graphics::segments(x0=0,x1=n.current, y0=power_current[i], lty=2, col="red")
        }
        text_current = paste0('Current Design: N=',n.current,'  Power=',round(power_current[i],2))
        text_ideal   = paste0('Ideal Design    : N=',ceiling(ES$n_ideal[i]),'  Power=',round(ES$power_ideal[i],2))
        graphics::legend('bottomright', legend=c(text_ideal, text_current), pch=c(20,20), col=c('blue','red'), bty='n', cex=1.2)
    }
     if (!is.null(title)) graphics::mtext(title, line=-2, outer=T)
    return(ES)
}

## }}}
