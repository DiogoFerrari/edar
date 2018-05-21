
## {{{ anxilary }}}

edar_get_fitted <- function(newdata, model)
{

}
## {{{ docs }}}

#' Get new data set
#'
#' Create a new data set based on the provided data frame in which all covariates are kept fixed, except those specified by the user.
#'
#' @param data a data frame with the data. Observations must be the rows, variables in the columns.
#' @param n integer, size of the new data set
#' @param x string with the name of the numeric variable that will vary within the range of its values in the original data set. All other numeric covariates will be set to their average value
#' @param cat.values named list of string vectors. The name of each element of the list (the string vectors) must match variable names in the data. The element of the string vectors must be strings with the name of the categories to use in the new data set. To generate the new data set, the numeric columns will be set to their mean value, except the column specified in the parameter \code{x}. The categorical values are set to their first category or the first category in alphabetic order. One can set the categorical variables to different values or use more than one category by setting this parameter \code{cat.values} as desired. For instance, suppose there is a categorical variable in the data set named education, taking the values of \code{High} or \code{Low}. If \code{cat.values=NULL}, the new data set returned will be fixed at \code{education=High}. One can use \code{education="Low"} by setting \code{cat.value=list(eductation="low")}. One can have both levels of education returned by setting \code{cat.value=list(eductation=c("low", "high"))}. See more examples in the documentation below.
#'
#' @export

## }}}
edar_get_new_data          <- function(data, n, x, cat.values=NULL)
{
    if(!is.null(cat.values)){
        cat_vars1 = cat.values %>%
            do.call(expand.grid,.) %>%
            base::replicate(n, ., simplify = FALSE) %>%
            dplyr::bind_rows(.) %>%
            dplyr::arrange_(.dots=names(cat.values)) 
        cat_vars2 = data %>%
            tibble::as_data_frame(.) %>%
            dplyr::select_if(function(col) !is.numeric(col)) %>%
            dplyr::select(-dplyr::one_of(names(cat.values)))  %>%
            dplyr::summarize_all(function(x) sort(unique(x))[1])  %>%
            dplyr::mutate_if(is.factor, as.character) %>%
            base::replicate(nrow(cat_vars1), ., simplify=FALSE)  %>%
            dplyr::bind_rows(.)
        cat_vars = dplyr::bind_cols(cat_vars1, cat_vars2)  %>%
            tibble::as_data_frame(.) 

        num_vars = data %>%
            dplyr::select_if(is.numeric)  %>%
            dplyr::mutate_all(mean, na.rm=T) %>%
            dplyr::slice(., 1) %>% 
            .[rep(1,nrow(cat_vars)),] %>%
            dplyr::bind_rows(.)

        newdata = dplyr::bind_cols(num_vars, cat_vars)

        newx = newdata %>%
            dplyr::group_by_(.dots=names(cat.values)) %>%
            dplyr::mutate( x = seq(min(data[,x], na.rm=T), max(data[,x], na.rm=T),length=n()))  %>%
            dplyr::ungroup(.)  %>% 
            dplyr::select(x) 

        newdata[,x] = newx
    }else{
        cat_vars = data %>%
            tibble::as_data_frame(.) %>%
            dplyr::select_if(function(col) !is.numeric(col)) %>%
            dplyr::summarize_all(function(x) sort(unique(x))[1])  %>%
            dplyr::mutate_if(is.factor, as.character) %>%
            .[rep(1,n),]
        num_vars = data %>%
            dplyr::select_if(is.numeric)  %>%
            dplyr::mutate_all(mean, na.rm=T) %>%
            dplyr::slice(., 1) %>% 
            .[rep(1,n),]
        num_vars[,x] = seq(min(data[,x], na.rm=T),max(data[,x], na.rm=T),length=n)
        newdata = cat_vars %>% dplyr::bind_cols(., num_vars)
    }

    newdata = newdata %>% dplyr::mutate_if(is.factor, as.character)
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
#' @param cat.values named list of string vectors. The name of each element of the list (the string vectors) must match variable names in the data. The element of the string vectors must be strings with the name of the categories to use in the plot with the fitted values. To generate the fitted values, the numeric columns will be set to their mean value, except the column specified in the parameter \code{x}. The categorical values are set to their first category or the first category in alphabetic order. One can set the categorical variables to different values or use more than one category by setting this parameter \code{cat.value} as desired. For instance, suppose there is a categorical variable in the data set named education, taking the values of \code{High} or \code{Low}, which was used in the model. If \code{cat.value=NULL}, the plot with the predicted values will be fixed at \code{education=High}. One can use \code{education="Low"} by setting \code{cat.value=list(eductation="low")}. One can generate predicted values for both levels of education by setting \code{cat.value=list(eductation=c("low", "high"))}. See more examples in the documentation below.
#' @param colour.groups a string with the name of the categorical variable to produce the color code for the points of the plot
#' @param facets a string vector with the name of the categorical variable to generate the facets. The fitted values will be produced for each facet.
#' @param facets.ncol an integer, the number of columns of grid when using facets. 
#' @param col.pch a string or rgb code. 
#' @param legend.position a string (\code{top}, \code{bottom}, \code{left} (Default)), \code{right} 
#' @param legend.title a string with the title of the legend 
#' @param legend.omit boolean, if \code{TRUE} the legend is omitted
#' @param scales used when facets are used. See \code{\link[ggplot2]{facet_wrap}}
#' @param xlab string with text to display in the x-axis 
#' @param ylab string with text to display in the y-axis 
#' @param xlim two-dimensional numeric vector with the limits of the x-axis 
#' @param ylim two-dimensional numeric vector with the limits of the y-axis
#' @param facet.title.position a string with either \code{left} (Default), \code{right}, or \code{center}. It indicates the position of the facet tiltle
#'
#' @examples
#' library(magrittr)
#' set.seed(77)
#' 
#' data = tibble::data_frame(n = 300,
#'                           x1   = rnorm(n,3,1),
#'                           x2   = rexp(n),
#'                           cat1 = sample(c(0,1), n, replace=TRUE),
#'                           cat2 = sample(letters[1:4], n, replace=TRUE),
#'                           y    = -10*x1*cat1 + 10*x2*(3*(cat2=='a')
#'                                  -3*(cat2=='b') +1*(cat2=='c') -1*(cat2=='d')) + 
#'                               rnorm(n,0,10), 
#'                           y.bin = ifelse(y < mean(y), 0, 1),
#'                           y.mul = 1+ifelse( - x1 - x2 + rnorm(n,sd=10) < 0, 0,
#'                                     ifelse( - 2 * x2 + rnorm(n,sd=10) < 0, 1, 2)),
#'                           ) %>%
#'     dplyr::mutate(cat1 = as.factor(cat1), cat2=as.factor(cat2)) 
#' 
#' model.g1 = lm(y ~ x1, data)
#' model.g2 = lm(y ~ x1 + x2, data)
#' model.g  = lm(y ~ x1*cat1 + x2*cat2, data)
#' model.bin = glm(y.bin ~ x1+x2*cat2, data=data, family='binomial')
#' model.mul <- nnet::multinom(y.mul ~ x1 + x2, data)
#' 
#' model.g %>% edar::gge_fit(., data, 'y', 'x1')
#' model.g %>% edar::gge_fit(., data, 'y', 'x2')
#' 
#' model.g %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat1'="0"))
#' model.g %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat1'="1"))
#' model.g %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat1'=c("0","1")))
#' 
#' 
#' model.g %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat2'="a"))
#' model.g %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat2'="b"))
#' 
#' model.g %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat1'="0"), colour.group='cat1')
#' model.g %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat1'="1"), colour.group='cat1')
#' model.g %>% edar::gge_fit(., data, 'y', 'x1',
#'                           cat.values=list('cat1'=c("0","1")), colour.group='cat1')
#' 
#' model.bin %>% edar::gge_fit(., data, 'y.bin', 'x2', facets="cat2")
#' model.bin %>% edar::gge_fit(., data, 'y.bin', 'x2', facets="cat2", facets.ncol=1)
#' 
#' model.bin %>% edar::gge_fit(., data, 'y.bin', 'x2', facets="cat2", colour.groups='cat1')
#' model.bin %>% edar::gge_fit(., data, 'y.bin', 'x2', facets="cat2", colour.groups='cat2')
#' 
#' model.bin %>% edar::gge_fit(., data, 'y.bin', 'x2', facets=c("cat1", "cat2"))
#' 
#' 
#' 
#' ## note: gge_fit produces the same result as geom_smooth under the same model
#' ## -----
#' model = glm(y.bin ~ x1, data=data, family='binomial')
#' 
#' model %>% edar::gge_fit(., data, 'y.bin', 'x1')
#' 
#' data %>%
#'     ggplot2::ggplot(.,ggplot2::aes(x=x1, y=y.bin) ) +
#'     ggplot2::geom_point(size=2) +
#'     ggplot2::geom_smooth(size=.5,  method="glm", method.args = list(family = "binomial"))+
#'     ggplot2::theme_bw()
#'  
#' @export
## }}}
gge_fit <- function(model, data, y, x, n=200, show.points=T,  cat.values=NULL, colour.groups=NULL, facets=NULL, facets.ncol=NULL, col.pch=NULL, subtitle=NULL, footnote=NULL, legend.position='top', legend.title=NULL, legend.omit=F, title=NULL, title.position="left", ylim=NULL, xlim=NULL, xlab=NULL, ylab=NULL, scales='fixed', facet.title.position = "left")
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

    if (facet.title.position == 'right' ) facet.title.position =  1
    if (facet.title.position == 'center') facet.title.position = .5
    if (facet.title.position == 'left'  ) facet.title.position =  0

    ## get the new data
    ## ----------------
    newdata = edar_get_new_data(data=data, n=n, x=x, cat.values = cat.values)
    if(!is.null(facets)){
        if (length(facets)>1) {
            list.cat.values = data %>%
                dplyr::select(facets)  %>%
                dplyr::summarise_all( function(x) list(unique(as.character(x))) )  %>%
                base::apply(., 2, function(x) x  %>% unlist)
        }else{
            list.cat.values = data %>%
                dplyr::select(facets)  %>%
                dplyr::summarise_all( function(x) list(unique(as.character(x))) )  %>%
                dplyr::pull(.) %>%
                list %>%
                .[[1]] %>%
                stats::setNames(., facets)
        }
        newdata = newdata %>%
            dplyr::mutate_if(is.factor, as.character) %>%
            dplyr::bind_rows(., edar_get_new_data(data=data, n=n, x=x, cat.values = list.cat.values ) )
    }


    ## get predicted values
    ## --------------------
    if (any(class(model)[1] %in% c('data.frame',"tbl_df","tbl" ))) {
        pred = edar_get_fitted(newdata, model)
    }else{
        pred  = broom::augment(model, newdata = newdata, type.predict='response')#, type.predict='response' for glm
        pred$ylb = pred$.fitted-1.96*pred$.se.fit
        pred$yub = pred$.fitted+1.96*pred$.se.fit
    }
    
    ## create colour groups in the data frame
    ## --------------------------------------
    if (!is.null(colour.groups)) {
        col.group=paste0(colour.groups, collapse=' - ') 
        data %>%
            tidyr::unite(col.group, colour.groups, sep=' - ', remove=FALSE)
    }else{
        col.group=NULL
    }

    ## create variable to groups the data given categorical variables set by the user
    ## ------------------------------------------------------------------------------
    if (!is.null(cat.values)) {
        cat.group = names(cat.values)
        data %>%
            tidyr::unite(col.group, cat.group, sep=' - ', remove=FALSE)
    }else{
        cat.group=NULL
    }
    
    ## main plot
    ## ---------
    g = ggplot2::ggplot(data)+
        ggplot2::geom_line(data=pred, ggplot2::aes_string(x= x, ".fitted", group=cat.group, colour=col.group)) +
        ggplot2::geom_ribbon(data=pred, ggplot2::aes_string(x = x, ymin="ylb", ymax="yub", group=cat.group, fill=cat.group), alpha=0.4) +
        ggplot2::theme_bw() 

    ## show points
    ## -----------
    if(show.points) 
        g = g + ggplot2::geom_point(data=data,ggplot2::aes_string(x= x, y= y), colour=col.pch, size=2) 
    ## colour groups
    ## -------------
    if(!is.null(col.group) & show.points)
        g = g + ggplot2::geom_point(data=data,ggplot2::aes_string(x= x, y= y, colour=col.group)) +
            viridis::scale_color_viridis(option="viridis", discrete=TRUE, alpha=1) 

    ## facets
    ## ------
    if(!is.null(facets)) g = g + ggplot2::facet_wrap(stats::as.formula(paste("~", paste0(facets, collapse="+") )), scales=scales, ncol=facets.ncol) 


    
    if(!is.null(title)) g = g +  ggplot2::labs(title = title) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = title.position))
    if(!is.null(subtitle)) g  =g +  ggplot2::labs(subtitle = subtitle)
    if(!is.null(footnote)) g  =g +  ggplot2::labs(caption = footnote)
    if(!is.null(legend.position)) g = g + ggplot2::theme(legend.position = legend.position) 


    ## xlab and ylab
    ## -------------
    if(!is.null(xlab)) g = g + ggplot2::xlab(xlab)
    if(!is.null(ylab)) g = g + ggplot2::ylab(ylab)
    
    if(!is.null(xlim)) g = g + ggplot2::xlim(xlim)
    if(!is.null(ylim)) g = g + ggplot2::ylim(ylim)

    if (!is.null(legend.title)) g = g + ggplot2::guides(colour=ggplot2::guide_legend(legend.title)) 

    ## legend
    ## ------
    if (any(sapply(cat.values, length)>1)) {
            g = g + viridis::scale_fill_viridis(option="viridis", discrete=TRUE, alpha=1) 
    }else{
        g = g + ggplot2::scale_fill_manual(values = rep(col.pch, length(cat.values)), guide=FALSE)
    }
    
    if(legend.omit) g = g + ggplot2::theme(legend.position = 'none') 
    g = g + ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"),
                           strip.text.x = ggplot2::element_text(size=12, face='bold', hjust=facet.title.position),
                           strip.text.y = ggplot2::element_text(size=12, face="bold")) 
        
    
    return(g)

    
}


## }}}
## {{{ Plot Coefficients }}}

## {{{ docs }}}

#' Dotwhisker plot
#'
#' This is a wrap function for the function \code{dwplot} from the package \code{dotwhisker}
#'
#' @param model either an object from \code{lm}, \code{glm}, or a \code{nnet::multinom} or a tidy data.frame with the model summary. If the data frame is provided, it must contain a column named term (the name of the covariates), estimate (the point estimate), and either std.error (standard errors of the estimates) or conf.low or conf.high (low and high intervals of the point estimate). If there are more than one model in the data.frame, it must contain a column with labels identifying each model.
#' @param var.order A vector of variable names that specifies the order in which the variables are to appear along the y-axis of the plot. If a data.frame is provided, the order of the y-axis will follow the order of the rows in the data.frame
#' @param model.id a string with the name of the column that contain the id of each model
#' @inheritParams gge_fit
#' @inheritParams tidye
#'
#' @details Robust standard errors are computed when \code{hc=T} only when the parameter \code{model} is an object from \code{lm}, \code{glm}. 
#'
#' @examples
#' library(magrittr)
#' set.seed(77)
#' 
#' data = tibble::data_frame(n = 300,
#'                           x1   = rnorm(n,3,1),
#'                           x2   = rexp(n),
#'                           cat1 = sample(c(0,1), n, replace=TRUE),
#'                           cat2 = sample(letters[1:4], n, replace=TRUE),
#'                           y    = -10*x1*cat1 + 10*x2*(3*(cat2=='a')
#'                                  -3*(cat2=='b') +1*(cat2=='c') -1*(cat2=='d')) + 
#'                               rnorm(n,0,10), 
#'                           y.bin = ifelse(y < mean(y), 0, 1),
#'                           y.mul = 1+ifelse( - x1 - x2 + rnorm(n,sd=10) < 0, 0,
#'                                     ifelse( - 2*x2 + rnorm(n,sd=10) < 0, 1, 2)),
#'                           )
#' 
#' model.g1 = lm(y ~ x1, data)
#' model.g2 = lm(y ~ x1 + x2, data)
#' model.g  = lm(y ~ x1*cat1 + x2*cat2, data)
#' model.bin = glm(y.bin ~ x1+x2*cat2, data=data, family='binomial')
#' model.mul <- nnet::multinom(y.mul ~ x1 + x2, data)
#' 
#' 
#' gge_coef(model.g)
#' gge_coef(model.g, hc=TRUE)
#' gge_coef(tidye(model.g) %>% dplyr::arrange(estimate), hc=TRUE)
#' 
#' gge_coef(model.bin)
#' gge_coef(model.bin, hc=TRUE)
#' gge_coef(tidye(model.g))
#' 
#' ## many models at once
#' models=tidye(list('Standard Model'=model.bin)) %>%
#'     dplyr::bind_rows(tidye(list('Robust std. error'=model.bin), hc=TRUE) )
#' gge_coef(models)
#' gge_coef(models, model.id='model')
#' 
#' ## list of models
#' gge_coef(list(model.g, model.g1, model.g2), model.id='model')
#' gge_coef(list('Complete model'=model.g, "One Covar"=model.g1,
#'               "Two covars"=model.g2), model.id='model' )
#' 
#' ## pipe can be used
#' ## modelg %>% gge_coef
#' 
#' @export

## }}}
gge_coef <- function(model, xlab="Coefficient Estimate", ylab="", var.order=NULL, model.id=NULL, title=NULL, subtitle=NULL, footnote=NULL, legend.position='top', legend.title='', title.position="left", hc=FALSE, hc.type=c("hc3", "hc0", "hc1", "hc2", "hc4"))
{

    if(! class(model)[1] %in% c('data.frame', "tbl_df", "tbl")) model = tidye(model, hc=hc, hc.type=hc.type)
    if (is.null(model.id)) {
        g = model  %>% 
            dotwhisker::dwplot(., order_vars = var.order)+
            ggplot2::geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
            ggplot2::xlab(xlab) + 
            ggplot2::ylab(ylab) +
            ggplot2::theme_bw() +
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
    if (!is.null(footnote))  g = g + ggplot2::labs(caption = footnote)
    if (!is.null(legend.title)) g = g + ggplot2::guides(colour=ggplot2::guide_legend(legend.title)) 
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
#' @param hc boolean, if \code{TRUE}  robust standard errors (heteroskedasticity corrected) are returned
#' @param hc.type a string with the method to compute the standard errors (see \code{\link[car]{hccm}})
#' @param tibble boolean, if \code{TRUE}, it returns a tibble data.frame. Default \code{TRUE}
#' @param keep.nohc boolean, if \code{TRUE} and \code{hc=TRUE} the std.errors originally estimated will be kept and returned alongside the robust estimates. The original estimates will have the suffix .nohc (no heteroskedasticity corrected)
#'
#' @examples
#'
#' set.seed(77)
#' 
#' data = tibble::data_frame(n = 300,
#'                           x1   = rnorm(n,3,1),
#'                           x2   = rexp(n),
#'                           cat1 = sample(c(0,1), n, replace=TRUE),
#'                           cat2 = sample(letters[1:4], n, replace=TRUE),
#'                           y    = -10*x1*cat1 + 10*x2*(3*(cat2=='a')
#'                                  -3*(cat2=='b') +1*(cat2=='c') -1*(cat2=='d')) + 
#'                               rnorm(n,0,10), 
#'                           y.bin = ifelse(y < mean(y), 0, 1),
#'                           y.mul = 1+ifelse( - x1 - x2 + rnorm(n,sd=10) < 0, 0,
#'                                     ifelse( - 2*x2 + rnorm(n,sd=10) < 0, 1, 2)),
#'                           )
#' 
#' model.g1 = lm(y ~ x1, data)
#' model.g2 = lm(y ~ x1 + x2, data)
#' model.g  = lm(y ~ x1*cat1 + x2*cat2, data)
#' model.bin = glm(y.bin ~ x1+x2*cat2, data=data, family='binomial')
#' model.mul <- nnet::multinom(y.mul ~ x1 + x2, data)
#' 
#' ## digits and output format
#' tidye(model.g)
#' tidye(model.g, digits=12)
#' tidye(model.g, tibble=FALSE)
#' tidye(model.g, digits=12, tibble=FALSE)
#' 
#' ## other model types
#' tidye(model.bin)
#' tidye(model.mul)
#' 
#' ## with robust std.errors
#' tidye(model.g, hc=TRUE)
#' tidye(model.g, hc=TRUE, keep.nohc=TRUE) # keep no heterocedastic corrected std.errors
#' tidye(model.bin, hc=TRUE)
#' 
#' ## list of models
#' tidye(list(model.g), hc=TRUE)
#' tidye(list(model.g, model.bin, model.mul))
#' tidye(list(Gaussian=model.g, Binomial=model.bin, Multinomial=model.mul))
#' tidye(list(model.g, model.g1, model.g2), hc=TRUE)
#' tidye(list('Only model'=model.g))
#' 
#' @export

## }}}
tidye  <- function(model, digits=4, hc=FALSE, hc.type=c("hc3", "hc0", "hc1", "hc2", "hc4"), tibble=TRUE, keep.nohc=FALSE)
{
    options(warn=-1)
    on.exit(options(warn=0))

    if(class(model)[1]!="list") {model = list(Model=model); list.provided=FALSE}else{list.provided=TRUE}
    if(is.null(names(model))) names(model) = paste0("Model ", 1:length(model)) 
    tab = tibble::data_frame(estimation = model, 
                             model      = names(model),
                             family     = purrr::map_chr(estimation, ~ paste0(class(.x), collapse='-') ),
                             tidye_func = dplyr::case_when(family %in% c("glm-lm", "lm")  & !hc ~ list(tidye_glm),
                                                           family %in% c("glm-lm", "lm")  &  hc ~ list(tidyehc_glm),
                                                           family == "multinom-nnet"      & !hc ~ list(tidye_multin),
                                                           family == "multinom-nnet"      &  hc ~ list(tidyehc_multin),
                                                           )) %>%
        dplyr::mutate(summary = purrr::pmap(list(est=estimation, f=tidye_func, model.label=model), function(est, f, model.label) f(model=est, digits=digits, hc=hc, hc.type=hc.type) %>% dplyr::mutate(model=model.label)  ))  %>%
        dplyr::select(summary)  %>% 
        dplyr::pull(.) %>%
        dplyr::bind_rows(.) %>%
        dplyr::select(dplyr::contains("multin"), model, dplyr::everything())

    if(!list.provided) tab = tab %>% dplyr::select(-model) 
    if (tibble) {
        tab = tab %>% tibble::as_data_frame(.)  %>% dplyr::mutate_if(is.numeric, round, digits=digits)
    }else{
        tab = tab %>% as.data.frame(.) %>% dplyr::mutate_if(is.numeric, round, digits=digits)
    }
    if (!keep.nohc) {
        tab = tab  %>% dplyr::select(-dplyr::contains(".nohc")) 
    }
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
        dplyr::full_join(., broom::tidy(model, conf.int=T) , by=c("estimate", "term"), suffix=c("", ".nohc"))  %>%
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
    stop("\n\nOption is not implemented to handle models from class \'multinom\' from package \'nnet\'\n\n")
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
find_collinearity <- function(model)
{
    ## VIF > 2?
    VIF <- cbind(VIF=faraway::vif(model), sqrt_VIF_BiggerThan2 = sqrt(faraway::vif(model)) > 2)
    X <- as.matrix(stats::model.matrix(model))
    XtX <- t(X) %*% X
    lambda <- eigen(XtX)$values
    Eigen <- cbind(kappa=sqrt(lambda/min(lambda)),Eigen=eigen(XtX)$values)
    return(list(VIF=VIF, Eigen=Eigen))
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
