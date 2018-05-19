
## {{{ anxilary }}}

edar_get_fitted <- function(newdata, model){
    stop("\n\nImplementation not completed vor model as tidy summary")
}
## {{{ docs }}}

#' Get new data set
#'
#' Create a new data set based on the provided data frame in which all covariates are kept fixed, except one.
#'
#' @param data a data frame with the data. Observations must be the rows, variables in the columns.
#' @param n integer, size of the new data set
#' @param x string with the name of the numeric variable that will vary within the range of its values in the original data set. All other numeric covariates will be set to their average value
#' @param group1.label either \code{NULL} or a string with the name of the categorical variable to set to a specific lavel. If \code{NULL}, the label used will the the first one in alphabetical order for strings, and the first category for factors
#' @param group1.value string with the category of the variable whose name is provided in group.label. 
#' @param group2.label same as group1.label, but for a second categorical variable
#' @param group2.value same as group1.value, but for a second categorical variable
#' @param group3.label same as group1.label, but for a third categorical variable
#' @param group3.value same as group1.value, but for a third categorical variable
#'
#' @export

## }}}
edar_get_new_data              <- function(data, n, x, group1.label=NULL, group1.value=NULL, group2.label=NULL, group2.value=NULL, group3.label=NULL, group3.value=NULL)
{
    data =tibble::as_data_frame(data)
    cat_vars = data %>%
        dplyr::select_if(function(col) {is.character(col) | is.factor(col) | is.ordered(col)})  %>%
        dplyr::summarize_all(function(x) sort(unique(x))[1])  %>%
        .[rep(1,n),] %>%
        tibble::as_data_frame()
    if(!is.null(group1.label)) cat_vars[,group1.label] = group1.value
    if(!is.null(group2.label)) cat_vars[,group2.label] = group2.value
    if(!is.null(group3.label)) cat_vars[,group3.label] = group3.value

    num_vars = data %>%
        dplyr::select_if(is.numeric)  %>%
        dplyr::mutate_all(mean, na.rm=T) %>%
        .[1:n,]
    num_vars[,x] = seq(min(data[,x], na.rm=T),max(data[,x], na.rm=T),length=n)

    newdata = cat_vars %>%
        dplyr::bind_cols(num_vars)
    return(newdata)
}

## }}}
## {{{ Plot with fitted values }}}

## {{{ docs }}}

#' Plot fitted values
#'
#' This function plot the fitted value or the predicted probability given the model estimated using \code{lm} or \code{glm} and the original data. 
#'
#' @param data the original data set used to fit the model
#' @param model the output of either \code{\link[stats]{lm} } or the \code{\link[stats]{glm}} functions
#' @param y a string with the name of the dependent variable
#' @param x a string with the name of the independent variable that will be used to plotted in the x-axis.
#' @param n an integer, the number of points of \code{x} generated to produce the predicted/fitted values
#' @param show.points boolean, indicating if the points from the original data set must be plotted or not
#' @param title a string, the title of the plot
#' @param title.position a string (or an number) with \code{center} (or .5), \code{left} (or 0), or \code{right} (or 1)
#' @param subtitle a string, the subtitle of the plot
#' @param footnote a string, the footnote of the plot
#' @param cat.var a string with the name of the categorical variable whose value will be set in the parameter \code{cat.value}. It allows one to set the category to use to get the fitted/predicted values
#' @param cat.value string with the category of the variable described in the parameter \code{car.var}
#' @param col.group a string with the name of the categorical variable to produce the color code of the plot
#' @param facet1 a string with the name of the categorical variable to generate the facets
#' @param facet1.ncol an integer, the number of columns of the facet. Used only when a single facet is used (\code{facet2=NULL})
#' @param facet2 (same as facet1)
#' @param facet2.ncol an integer, the number of columns of the facet. Used only when a single facet is used (\code{facet2=NULL})
#' @param col.pch a string or rgb code. It will be used as color for the points when col.group is NULL
#' @param legend.position a string (\code{top}, \code{bottom}, \code{left}, \code{right} (Default))
#' @param legend.omit boolean, if the legend should be omitted or not
#' @param scales used when facets are used. See \code{\link[ggplot2]{facet_wrap}}
#' @param grid boolean, if \code{TRUE} add grid in the plot
#' @param xlab string to display in the x-axis 
#' @param ylab string to display in the y-axis 
#' @param xlim two-dimensional numeric vector with the limits of the x-axis 
#' @param ylim two-dimensional numeric vector with the limits of the y-axis 
#'
#' @examples
#' library(magrittr)
#' set.seed(777)
#' data = tibble::data_frame(x1 = rnorm(200,3,1),
#'                           x2 = rexp(200),
#'                           cat.var  = sample(c(0,1), 200, replace=TRUE),
#'                           cat.var2 = sample(letters[1:4], 200, replace=TRUE),
#'                           y = -10*x1*cat.var+rnorm(200,0,10) +
#'                               10*x2*(3*(cat.var2=='a') -3*(cat.var2=='b') +
#'                                      1*(cat.var2=='c') -1*(cat.var2=='d')))  %>%
#'     dplyr::mutate(cat.var=as.factor(cat.var)) 
#' model = lm(y ~ x1*cat.var+x2*cat.var2, data)
#' 
#' model %>% gge_fit(., data, 'y', 'x1')
#' model %>% gge_fit(., data, 'y', 'x2')
#' model %>% gge_fit(., data, 'y', 'x1', cat.var='cat.var', cat.value="0")
#' model %>% gge_fit(., data, 'y', 'x1', cat.var='cat.var2', cat.value="b")
#' model %>% gge_fit(., data, 'y', 'x1', cat.var='cat.var',
#'                  cat.value="1", col.group='cat.var2')
#' model %>% gge_fit(., data, 'y', 'x2', cat.var='cat.var',
#'                  cat.value="1", col.group="cat.var")
#' model %>% gge_fit(., data, 'y', 'x2', cat.var='cat.var',
#'                  cat.value=c("0","1"), col.group="cat.var")
#' model %>% gge_fit(., data, 'y', 'x2', cat.var='cat.var2',
#'                  cat.value=c("a","b"), col.group="cat.var2")
#' model %>% gge_fit(., data, 'y', 'x2', cat.var='cat.var2',
#'                  cat.value=c("a","b", 'c','d'), col.group="cat.var2")
#' model %>% gge_fit(., data, 'y', 'x2', facet1="cat.var")
#' model %>% gge_fit(., data, 'y', 'x2', facet1="cat.var2")
#' model %>% gge_fit(., data, 'y', 'x2', facet1="cat.var2", facet1.ncol=1)
#' model %>% gge_fit(., data, 'y', 'x2', facet1="cat.var2",
#'                  facet1.ncol=1, , cat.var='cat.var2',
#'                  cat.value=c("a","b", 'c','d'), col.group="cat.var2")
#' model %>% gge_fit(., data, 'y', 'x2', facet1="cat.var2", facet2='cat.var')
#' model %>% gge_fit(., data, 'y', 'x2', facet1="cat.var2", facet2='cat.var')
#' ## variable var.cat2 fixed at level 'a'
#' model %>% gge_fit(., data, 'y', 'x2', cat.var='cat.var',
#'                  cat.value=c("0","1"), facet1="cat.var2", facet2='cat.var')
#' ## variable var.cat fixed at level '0'
#' model %>% gge_fit(., data, 'y', 'x2', cat.var='cat.var2',
#'                  cat.value=c("a","b", 'c','d'), col.group="cat.var2",
#'                  facet1="cat.var2", facet2='cat.var')
#'
#' ## logistic regression
#' ## -------------------
#' data = tibble::data_frame(x1 = rnorm(200,0,1),
#'                           x2 = rexp(200),
#'                           cat.var  = sample(c(0,1), 200, replace=TRUE),
#'                           cat.var2 = sample(letters[1:4], 200, replace=TRUE),
#'                           mu = 7*x1 + 2*x2*(3*(cat.var2=='a') -3*(cat.var2=='b') +
#'                                           1*(cat.var2=='c') -1*(cat.var2=='d'))+rnorm(200,0,1) ,
#'                           p  = 1/(1+exp(- mu)),
#'                           y  = sapply(p, function(eta) sample(c(0, 1), size=1, prob=c(eta, 1-eta))),
#'                           )  %>%
#'     dplyr::mutate(cat.var=as.factor(cat.var),
#'                   cat.var2=as.factor(cat.var2),
#'                   ) 
#' 
#' model = glm(y ~ x1+x2*cat.var2, data=data, family='binomial')
#' 
#' 
#' model %>% edar::gge_fit(., data, 'y', 'x1')
#' model %>% edar::gge_fit(., data, 'y', 'x1', facet1="cat.var2")
#' model %>% edar::gge_fit(., data, 'y', 'x1', cat.var='cat.var', cat.val='d')
#' 
#' model %>% edar::gge_fit(., data, 'y', 'x2')
#' model %>% edar::gge_fit(., data, 'y', 'x2', col.group='cat.var2')
#' model %>% edar::gge_fit(., data, 'y', 'x2', facet1="cat.var2")
#' 
#' ## note: gge_fit produces the same result as geom_smooth under the same model
#' ## -----
#' model = glm(y ~ x1, data=data, family='binomial')
#' 
#' model %>% edar::gge_fit(., data, 'y', 'x1')
#' data %>%
#'     ggplot2::ggplot(.,ggplot2::aes(x=x1, y=y) ) +
#'     ggplot2::geom_point(size=2) +
#'     ggplot2::geom_smooth(size=.5,  method="glm", method.args = list(family = "binomial"))
#' 
#' 
#' @export

## }}}
gge_fit <- function(model, data, y, x, n=200, show.points=T, title=NULL, cat.var=NULL, cat.value=NULL, col.group=NULL, facet1=NULL, facet1.ncol=NULL, facet2=NULL, facet2.ncol=NULL,  col.pch=NULL, subtitle=NULL, footnote=NULL, legend.position=NULL, legend.omit=F, title.position="left", grid=T, ylim=NULL, xlim=NULL, xlab=NULL, ylab=NULL, scales='fixed')
{
    warnings("FALSE")
    on.exit(warnings("TRUE"))
    par.default <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(par.default), add=TRUE)
    op.default <- options()
    on.exit(options(op.default), add=TRUE)
    dir.default <- getwd()
    on.exit(setwd(dir.default), add=TRUE)

    ## some aesthetic elements
    ## -----------------------
    if(is.null(col.pch)) col.pch="#00000044"
    if(title.position=="left")   title.position = 0
    if(title.position=="center") title.position = .5
    if(title.position=="right")  title.position = 1

    ## get the new data
    ## ----------------
    newdata = edar_get_new_data(data=data, n=n, x=x, group1.label=cat.var, group1.value=cat.value) 

    if(!is.null(facet1))
    {
        labels=names(table(data[,facet1]))
        for (label in labels)
        {
            newdata_tmp = edar_get_new_data(data=data, n=n, x=x, group1.label=facet1, group1.value=label)
            newdata=newdata %>% dplyr::bind_rows(newdata_tmp) 
        }
    }

    if (any(class(model) %in% 'data.frame')) {
        pred = edar_get_fitted(newdata, model)
    }else{
        pred  = broom::augment(model, newdata = newdata, type.predict='response')#, type.predict='response' for glm
        pred$ylb = pred$.fitted-1.96*pred$.se.fit
        pred$yub = pred$.fitted+1.96*pred$.se.fit
    }
    
    
    ## main plot
    ## ---------
    g = ggplot2::ggplot(data)+
        ggplot2::theme_classic() 

    ## ylim and xlim
    ## -------------
    if(!is.null(xlim)) g = g + ggplot2::xlim(xlim)
    if(!is.null(ylim)) g = g + ggplot2::ylim(ylim)

    ## add grid
    ## --------
    if(grid) g = g + ggplot2::theme(panel.grid.major=ggplot2::element_line(linetype='dotted', color='grey80'))
                                    #panel.grid.minor=ggplot2::element_line(linetype='dotted', color='grey80'))
    
    if(show.points) g = g + ggplot2::geom_point(data=data,ggplot2::aes_string(x= x, y= y), colour=col.pch, size=2) 
    
    if(!is.null(col.group) & show.points) g = g + ggplot2::geom_point(data=data,ggplot2::aes_string(x= x, y= y, colour=col.group)) +
                                              viridis::scale_color_viridis(option="viridis", discrete=TRUE, alpha=1) 
    if(!is.null(facet1) & is.null(facet2)) g = g + ggplot2::facet_wrap(stats::as.formula(paste("~", facet1)), scales=scales) 
    if(!is.null(facet1.ncol))              g = g + ggplot2::facet_wrap(stats::as.formula(paste("~", facet1)), scales=scales, ncol=facet1.ncol) 
    if(is.null(facet1) & !is.null(facet2)) g = g + ggplot2::facet_wrap( ~ facet2,  scales=scales) 
    if(!is.null(facet1) & !is.null(facet2)) g = g + ggplot2::facet_grid(stats::as.formula(paste(facet1, "~", facet2)),  scales=scales) + ggplot2::theme_bw()
    if(!is.null(facet1) | !is.null(facet2)) g = g + ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"),
                                                                   strip.text.x = ggplot2::element_text(size=12, face='bold'),
                                                                   strip.text.y = ggplot2::element_text(size=12, face="bold")) 

    if (!is.null(cat.value) & length(cat.value)>1) {
        g = g +  ggplot2::geom_line(data=pred, ggplot2::aes_string(x= x, ".fitted", group=cat.var, colour=cat.var)) +
            ggplot2::geom_ribbon(data=pred, ggplot2::aes_string(x = x, ymin="ylb", ymax="yub", group=cat.var, fill=cat.var), alpha=0.4) +
            viridis::scale_fill_viridis(option="viridis", discrete=TRUE, alpha=1) 
            
    }else{
        g = g +  ggplot2::geom_line(data=pred, ggplot2::aes_string(x= x, ".fitted"), colour='grey60') +
            ggplot2::geom_ribbon(data=pred, ggplot2::aes_string(x = x, ymin="ylb", ymax="yub"), alpha=0.2) 
    }
    
    if(!is.null(title)) g = g +  ggplot2::labs(title = title) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = title.position))
    if(!is.null(subtitle)) g  =g +  ggplot2::labs(subtitle = subtitle)
    if(!is.null(footnote)) g  =g +  ggplot2::labs(caption = footnote)
    if(!is.null(legend.position)) g = g + ggplot2::theme(legend.position = legend.position) 
    if(legend.omit) g = g + ggplot2::theme(legend.position = 'none') 

    ## xlab and ylab
    ## -------------
    if(!is.null(xlab)) g = g + ggplot2::xlab(xlab)
    if(!is.null(ylab)) g = g + ggplot2::ylab(ylab)
    
    if(!is.null(xlim)) g = g + ggplot2::xlim(xlim)
    if(!is.null(ylim)) g = g + ggplot2::ylim(ylim)

    return(g)

    
}

## }}}
## {{{ Plot Coefficients }}}

## {{{ docs }}}
#' Dotwhisker plot
#'
#' This is a wrap function for the function \code{dwplot} from the package \code{dotwhisker}
#'
#' @param model a tidy data.frame with the model summary. It must contain the columns term (the name of the covariates), estimate (the point estimate), and either std.error or conf.low or conf.high. If there are more than one model in the data.frame, it must contain a column with labels identifying each model.
#' @param var.order A vector of variable names that specifies the order in which the variables are to appear along the y-axis of the plot. 
#' @param model.id a string with the name of the column that contain the id of each model
#' @inheritParams gge_fit 
#' 
#' @export
## }}}
gge_coef <- function(model, xlab="Coefficient Estimate", ylab="", var.order=NULL, model.id=NULL, title=NULL, subtitle=NULL, footnote=NULL, legend.position=NULL,title.position="left")
{
    if (is.null(model.id)) {
        g = model  %>% 
            dotwhisker::dwplot(., order_vars = var.order)+
            ggplot2::geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
            ggplot2::xlab(xlab) + 
            ggplot2::ylab(ylab) +
            ggplot2::theme_bw()+
            ggplot2::theme(legend.position = "none") 
    }else{
        g = model  %>% 
            dotwhisker::dwplot(., order_vars = var.order, model_name=model.id)+
            ggplot2::geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
            ggplot2::xlab(xlab) + 
            ggplot2::ylab(ylab) +
            ggplot2::theme_bw()+
            ggplot2::theme(legend.position = legend.position) 
    }
    if (!is.null(title))    g = g + ggplot2::ggtitle(title)
    if (!is.null(subtitle)) g = g + ggplot2::ggtitle(label=title, subtitle=subtitle)
    if(!is.null(footnote))  g = g + ggplot2::labs(caption = footnote)
    return(g)

}


## }}}
## {{{ Tidy summaries (with robust std errors) }}}

## {{{ docs }}}

#' Tidy linear model summary
#'
#' This function uses \code{\link[broom]{tidy}} to create a data.frame with model results from \code{lm} or \code{\link[stats]{glm}} functions
#'
#'
#' @param model a \code{lm} of \code{glm} model
#' @param digits integer, the number of significant digitis to use
#' @param hc boolean, if \code{TRUE}  robust standard errors (homoskedasticity corrected) are returned
#' @param hc.type a string with the method to compute the standard errors (see \code{\link[car]{hccm}}) 
#'
#' @export

## }}}
tidye  <- function(model, digits=4, hc=FALSE, hc.type=c("hc3", "hc0", "hc1", "hc2", "hc4"))
{
    options(warn=-1)
    on.exit(options(warn=0))

    if(class(model)[1]!="list") {model = list(Model=model); list.provided=FALSE}else{list.provided=TRUE}
    if(is.null(names(model))) names(model) = paste0("Model ", 1:length(model)) 
    tab = tibble::data_frame(estimation = model, 
                             model      = names(model),
                             family     = purrr::map_chr(estimation, ~ paste0(class(.x), collapse='-') ),
                             tidye_func = dplyr::case_when(family == "glm-lm"        & !hc ~ list(tidye_glm),
                                                           family == "glm-lm"        &  hc ~ list(tidyehc_glm),
                                                           family == "multinom-nnet" & !hc ~ list(tidye_multin),
                                                           family == "multinom-nnet" &  hc ~ list(tidyehc_multin),
                                                           )) %>%
        dplyr::mutate(summary = purrr::pmap(list(est=estimation, f=tidye_func, model.label=model), function(est, f, model.label) f(model=est, digits=digits, hc=hc, hc.type=hc.type) %>% dplyr::mutate(model=model.label)  ))  %>%
        dplyr::select(summary)  %>% 
        dplyr::pull(.) %>%
        dplyr::bind_rows(.) %>%
        dplyr::select(dplyr::contains("multin"), model, dplyr::everything())

    if(!list.provided) tab = tab %>% dplyr::select(-model) 
    tab = tab %>% tibble::as_data_frame(.) 
    return(tab)
}

## anxillary for multinomial models (from nnet package)
## --------------------------------
tidyehc_glm <- function(model, digits=4, hc=FALSE, hc.type=c("hc3", "hc0", "hc1", "hc2", "hc4") )
{
 
    ## "hc0"    White SE
    ## "hc1"    Stata's Default
    ## "hc2"    Unbiased under homoskedasticity
    ## "hc3"    Default (conservative)

    type = hc.type[1]

    V <- car::hccm(model, type=type)
    sumry <- summary(model)
    class(sumry) = 'summary.lm'
    table <- coef(sumry)
    table[,2] <- sqrt(diag(V))
    table[,3] <- table[,1]/table[,2]
    table[,4] <- 2*stats::pt(abs(table[,3]), stats::df.residual(model), lower.tail=FALSE)
    
    sumry$coefficients <- table
    p <- nrow(table)
    hyp <- cbind(0, diag(p - 1))
    sumry$fstatistic[1] <- car::linearHypothesis(model, hyp,white.adjust=type)[2,"F"]

    
    sumry = broom::tidy(sumry, conf.int=T)
    sumry = sumry %>% 
        dplyr::mutate(conf.low  = estimate - 1.96*std.error,
                      conf.high = estimate + 1.96*std.error,)  %>%
        dplyr::full_join(., broom::tidy(model, conf.int=T) , by=c("estimate", "term"), suffix=c("", ".nonhc"))  %>%
        dplyr::mutate_if(is.numeric, round, digits=digits)  %>%
        dplyr::select(term, estimate, std.error, conf.low, conf.high, statistic, p.value, dplyr::everything()) 

    return(sumry)
}
tidye_glm <- function(model, digits=4, hc=FALSE, hc.type=c("hc3", "hc0", "hc1", "hc2", "hc4"))
{
    tab       = broom::tidy( model , conf.int=TRUE)
    tab[,-1]  = round(tab[,-1],digits)
    tab = tab  %>% dplyr::select(term, estimate, std.error, dplyr::contains("conf"), statistic, p.value) 
    return(tab)
}
tidye_multin <- function(model, digits=4, hc=FALSE, hc.type=c("hc3", "hc0", "hc1", "hc2", "hc4"))
{
    covars <- summary(model)$coefficients %>% colnames
    Beta   <- summary(model)$coefficients
    seBeta <- summary(model)$standard.errors
    z      <- summary(model)$coefficients/summary(model)$standard.errors
    ci     <- confint(model)
    p      <- 2*(1 - stats::pnorm(abs(z), 0, 1))
    ncaty  <- nrow(Beta)
    yj     <- rownames(Beta)
    results <- list()
    for (i in 1:length(yj)){
        ci.l <- ci[,1,i]
        ci.u <- ci[,2,i]
        results[[i]] <-data.frame(y.multin.cat = yj[i],
                                  term         = covars,
                                  estimate     = Beta[i,],
                                  std.error    = seBeta[i,],
                                  conf.low     = ci.l,
                                  conf.high    = ci.u,
                                  statistic    = z[i,],
                                  p.value      = p[i,]) %>% 
            dplyr::mutate_if(is.numeric, round, digits=digits)
    }
    tab = base::do.call(base::rbind, results)
    return(tab)
}
tidyehc_multin <- function(model, digits=4, hc=FALSE, hc.type=c("hc3", "hc0", "hc1", "hc2", "hc4") )
{
    stop("\n\nFunction is not implemented to handle models from class multinom from package nnet\n\n")
}
## }}}
## {{{ Diagnostics (lm) }}}

plot_leverage <- function(model){
    par.default <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(par.default))

    graphics::par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,4,3,1), mgp = c(2,.6,0), col="#00000044")
    hi       <- stats::lm.influence(model)$hat
    ei       <- model$residuals
    sigmahat <- summary(model)$sigma
    ri       <- ei / sigmahat * sqrt(1-hi)
    
    
    mat = matrix(c(1,2), byrow=T, nrow=1)
    graphics::layout(mat)

    faraway::halfnorm(hi, main='Leverage', ylab=bquote('Leverage ('~H[ii]~') of observations (sorted)'))
    graphics::grid()
    graphics::par(mar=c(4, 4.6, 3, 1))
    graphics::plot(y=ri,x=ei, ylab=bquote(r[i] == frac(hat(epsilon)[i],
                                             hat(sigma)*sqrt(1-h[i]))),
         xlab='raw residual', main='(Internally) Studentized residual',col="#00000044")
    graphics::grid()
} 
find_outlier <- function(model, alpha=0.05)
{
    n <- length(model$residuals)
    k <- length(model$coef)
    
    ti <- abs(stats::rstudent(model))
    pvalue <- 1- stats::pt(ti,df=n-k-1)
    outliers <- ti > abs(stats::qt(alpha/(2*n), df=n-k-1))

    if(sum(outliers)==0) {
        results <- list(Outliers="No outlier",
                        min_pvalue=min(pvalue),
                        max_t=max(ti))
        return(results)
    }else{
        
        results <- list(n_outliers=length(outliers[outliers]),
                        Outliers=outliers[outliers],
                        pvalue=pvalue[outliers],
                        t=ti[outliers])
        return(results)
    }
}
plot_residualsDiagnostic <- function(model)
{
    par.default <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(par.default))

    graphics::par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,4,3,1), mgp = c(2,.6,0))
    graphics::par(mfrow=c(2,2))
    graphics::par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,5,3,2))
    graphics::plot(model, col="#00000044")
}
plot_partialRegression <- function(model)
{
    par.default <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(par.default))
    graphics::par(las=1,cex.axis=.7,bty='l',  cex.main=.9, mar=c(4,4,1,1), mgp = c(2,.6,0))
    k = ncol(stats::model.matrix(model)) -1
    m = .edar_get_layout(k)

    car::avPlots(model, main=bquote('Partial Regression Plot: '~hat(epsilon)[(y~'~'~  x[(j)])]~'vs'~hat(epsilon)[(x[j]~'~'~ x[(j)])]~' and partial coefficients'~hat(beta)[j]), grid=T, 
                 layout=m, col="#00000044",  pch=20)

}
plot_partialResidual <- function(model)
{
    par.default <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(par.default))

    k = length(stats::terms(model))
    m = .edar_get_layout(k)
    graphics::par(mfrow=c(m))

    coef <- names(model$coeff)
    graphics::par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,4,3,1), mgp = c(2,.6,0), pch=19, col="#00000044")
    stats::termplot(model, partial.resid=T, terms=NULL, ylab=paste('e + ',coef,'*b'), main='Partial Residual Plot',
             se=T, ask=F)
}
find_collinearity <- function(model)
{
    ## VIF > 2?
    VIF <- cbind(VIF=faraway::vif(model), car::vif(model)[,2:3], sqrt_VIF_BiggerThan2 = sqrt(faraway::vif(model)) > 2)
    X <- as.matrix(stats::model.matrix(model))
    XtX <- t(X) %*% X
    lambda <- eigen(XtX)$values
    Eigen <- cbind(kappa=sqrt(lambda/min(lambda)),Eigen=eigen(XtX)$values)
    return(list(VIF=VIF, Eigen=Eigen))
}
## {{{ docs }}}

#' Generate diagnostics for lm models
#'
#'
#' @param model a \code{lm} object 
#'
#' @export

## }}}
plot_diagnostic <- function(model)
{
    par.default <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(par.default))
    graphics::par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,5,3,2))

    readline('Colinearity ... Press [Enter]...')             ; print(find_collinearity(model))
    readline('Ouliers ... Press [Enter]...')                 ; print(find_outlier(model))
    readline('Cook Distance ... Press [Enter]...')           ; plot_residualsDiagnostic(model)
    readline('Leverage ... Press [Enter]...')                ; plot_leverage(model)
    readline('Partial Regression Plot ... Press [Enter]...') ; plot_partialRegression(model)
    readline('Partial Residuals Plot ... Press [Enter]...')  ; plot_partialResidual(model)
}

## }}}



