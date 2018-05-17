
## {{{ docs }}}
#' short title
#'
#' description
#'
#'
#' @param parameter 
#'
#' @return what is returnts...
#'
#' @examples
#'
#' \dontrun{
#' }
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

## {{{ docs }}}
#' Plot fitted values
#'
#' This function plot the fitted value or the predicted probability given the model estimated using \code{\link{lm}} or \code{\link{glm}} and the original data. 
#'
#' @param data the original data set used to fit the model
#' @param model the output of either \code{\link{lm}} or the \code{\link{glm}} functions
#' @param y a string with the name of the dependent variable
#' @param x a string with the name of the independent variable that will be used to plotted in the x-axis.
#' @param n an integer, the number of points of \code{x} generated to produce the predicted/fitted values
#' @param show.points boolean, indicating if the points from the original data set must be plotted or not
#' @param tittle a string, the title of the plot
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
#' @param scales used when facets are used. See \link{ggplot2::facet_wrap}
#' @inheritParams graphics::plot
#'
#' @examples
#'
#' data = tibble::data_frame(x1 = rnorm(200,3,1),
#'                           x2 = rexp(200),
#'                           cat.var  = sample(c(0,1), 200, replace=T),
#'                           cat.var2 = sample(letters[1:4], 200, replace=T),
#'                           y = -10*x1*cat.var+rnorm(200,0,10) +
#'                               10*x2*(3*(cat.var2=='a') -3*(cat.var2=='b') +
#'                                      1*(cat.var2=='c') -1*(cat.var2=='d')))  %>%
#'     dplyr::mutate(cat.var=as.factor(cat.var)) 
#' model = lm(y ~ x1*cat.var+x2*cat.var2, data)
#' 
#' data %>% gge_fit(., model, 'y', 'x1')
#' data %>% gge_fit(., model, 'y', 'x2')
#' data %>% gge_fit(., model, 'y', 'x1', cat.var='cat.var', cat.value="0")
#' data %>% gge_fit(., model, 'y', 'x1', cat.var='cat.var2', cat.value="b")
#' data %>% gge_fit(., model, 'y', 'x1', cat.var='cat.var', cat.value="1", col.group='cat.var2')
#' data %>% gge_fit(., model, 'y', 'x2', cat.var='cat.var', cat.value="1", col.group="cat.var")
#' data %>% gge_fit(., model, 'y', 'x2', cat.var='cat.var', cat.value=c("0","1"), col.group="cat.var")
#' data %>% gge_fit(., model, 'y', 'x2', cat.var='cat.var2', cat.value=c("a","b"), col.group="cat.var2")
#' data %>% gge_fit(., model, 'y', 'x2', cat.var='cat.var2', cat.value=c("a","b", 'c','d'), col.group="cat.var2")
#' data %>% gge_fit(., model, 'y', 'x2', facet1="cat.var")
#' data %>% gge_fit(., model, 'y', 'x2', facet1="cat.var2")
#' data %>% gge_fit(., model, 'y', 'x2', facet1="cat.var2", facet1.ncol=1)
#' data %>% gge_fit(., model, 'y', 'x2', facet1="cat.var2", facet1.ncol=1, , cat.var='cat.var2', cat.value=c("a","b", 'c','d'), col.group="cat.var2")
#' data %>% gge_fit(., model, 'y', 'x2', facet1="cat.var2", facet2='cat.var')
#' data %>% gge_fit(., model, 'y', 'x2', facet1="cat.var2", facet2='cat.var')
#' ## variable var.cat2 fixed at level 'a'
#' data %>% gge_fit(., model, 'y', 'x2', cat.var='cat.var', cat.value=c("0","1"), facet1="cat.var2", facet2='cat.var')
#' ## variable var.cat fixed at level '0'
#' data %>% gge_fit(., model, 'y', 'x2', cat.var='cat.var2', cat.value=c("a","b", 'c','d'), col.group="cat.var2", facet1="cat.var2", facet2='cat.var')
#' @export
## }}}
gge_fit <- function(data, model, y, x, n=200, show.points=T, title=NULL, cat.var=NULL, cat.value=NULL, col.group=NULL, facet1=NULL, facet1.ncol=NULL, facet2=NULL, facet2.ncol=NULL,  col.pch=NULL, subtitle=NULL, footnote=NULL, legend.position=NULL, legend.omit=F, title.position="center", grid=T, ylim=NULL, xlim=NULL, xlab=NULL, ylab=NULL, scales='fixed', ...)
{
    warnings("FALSE")
    on.exit(warnings("TRUE"))
    par.default <- par(no.readonly = TRUE)
    on.exit(par(par.default), add=TRUE)
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
    pred  = broom::augment(model, newdata = newdata, type.predict='response')#, type.predict='response') #(for glm)
    pred$ylb = pred$.fitted-1.96*pred$.se.fit
    pred$yub = pred$.fitted+1.96*pred$.se.fit
    
    
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
    if(!is.null(facet1) & is.null(facet2)) g = g + ggplot2::facet_wrap(as.formula(paste("~", facet1)), scales=scales) 
    if(!is.null(facet1.ncol))              g = g + ggplot2::facet_wrap(as.formula(paste("~", facet1)), scales=scales, ncol=facet1.ncol) 
    if(is.null(facet1) & !is.null(facet2)) g = g + ggplot2::facet_wrap( ~ facet2,  scales=scales) 
    if(!is.null(facet1) & !is.null(facet2)) g = g + ggplot2::facet_grid(as.formula(paste(facet1, "~", facet2)),  scales=scales) + theme_bw()
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


## {{{ LM }}}

## plots
## -----
ds_RegLine_lm <- function(x,y, w=NULL, col='grey87',lty=1,p.value.legend=T,points=F,leg.pos='topright', add=F, grid=T, scale=F, ...){
    oldpar <- par(no.readonly = T)
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,4,3,1), mgp = c(2,.6,0))
    if(scale){
        y = c(scale(y))
        x = c(scale(x))
    }
    model=lm(y ~ x, weights=w)
    ## confidence interval
    x.new = seq(min(x, na.rm=T), max(x,na.rm=T), length=100)
    y.new = seq(min(y, na.rm=T), max(y,na.rm=T), length=100)
    pred = predict(model, newdata=data.frame(x=x.new,y=y.new), interval="confidence", se.fit=T)$fit
    ub=pred[,'upr']
    lb=pred[,'lwr']
    if(!add) plot(x,y,col='white')
    polygon(x=c(x.new,rev(x.new)), y=c(lb, rev(ub)), col = col, border = NA)
    abline(a = model$coefficients[1] , b = model$coefficients[2] , col='black', lwd=1, lty=lty, ...)
    if(points) points(x,y,..., pch=20, cex=.5)
    if (grid) grid()
    ## if(p.value.legend) legend('topleft', legend= paste('t stat=',t.value,'\n=',p.value), bty='n')
    if(p.value.legend) {
        p.value=round(summary(model)$coefficients[2,'Pr(>|t|)'], 2)
        beta=round(summary(model)$coefficients[2,'Estimate'], 4)
        legend(leg.pos, legend= paste('Corr=',beta,' p-value=',p.value), bty='n')
    }
    par(oldpar)
}
ds_ggRegLine_lm <- function(x,y, colour.group=NULL, h.panel.group=NULL, v.panel.group=NULL, size=NULL, data, layout=c('horizontal','vertical','squared'), show.grid=T, method=c('lm','loess')){

    if (!is.null(colour.group)){
        ncol = length(levels(data[,colour.group]))
        col = c('firebrick1', 'black', 'steelblue4', 'yellow3', 'grey', 'green3','purple2', 'cyan', 'lightpink2','orangered', 'blue3')
        cols=col[1:ncol]
    }
    
    g = ggplot(data,aes_string(x=x,y=y,col=colour.group,fill=colour.group))+geom_point(aes_string( size=size))+
        geom_smooth(method=method[1],alpha=.2)+theme_bw()+xlab(x)+ylab(y)+
        scale_colour_manual(values = cols)+
        scale_fill_manual(values = cols) +
        theme(plot.margin = unit(c(1.5,1.5,0.2,0.2), "cm"))
    if (!is.null(h.panel.group) & !is.null(v.panel.group)){g = g +facet_grid(paste0(h.panel.group ,'~ ', v.panel.group))}
    if (!is.null(h.panel.group) &  is.null(v.panel.group)){g = g +facet_grid(paste0(h.panel.group ,'~. '))}
    if ( is.null(h.panel.group) & !is.null(v.panel.group)){g = g +facet_grid(paste0('.~',v.panel.group ))}
    plot(g)
    grid::grid.text(unit(0.5,"npc") ,unit(.98,'npc'),label = v.panel.group, rot = 0)   # top
    grid::grid.text(unit(0.98,"npc"),unit(0.5,'npc'),label = h.panel.group, rot = 270) # right
    invisible(g)

}

## tables
## ------
ds_lm2tbl  <- function(model, digits=4){
    model       = broom::tidy( model , conf.int=T)
    model[,-1]  = round(model[,-1],digits)
    return(model)
}
ds_lm2tbl_hc <- function(model, type=c("hc3", "hc0", "hc1", "hc2", "hc4"), ...){
 
    ## "hc0"    White SE
    ## "hc1"    Stata's Default
    ## "hc2"    Unbiased under homoskedasticity
    ## "hc3"    Default (conservative)

    type <- match.arg(type)
    V <- car::hccm(model, type=type)
    sumry <- summary(model)
    table <- coef(sumry)
    table[,2] <- sqrt(diag(V))
    table[,3] <- table[,1]/table[,2]
    table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)
    
    sumry$coefficients <- table
    p <- nrow(table)
    hyp <- cbind(0, diag(p - 1))
    sumry$fstatistic[1] <- car::linearHypothesis(model, hyp,white.adjust=type)[2,"F"]
    
    sumry = broom::tidy(sumry, conf.int=T)
    print(sumry)
    cat("Note: Heteroscedasticity-consistent standard errors using adjustment", type, "\n")
    invisible(sumry)
 
}

## diagnostic
## ----------
ds_leverage <- function(model){
    par(mfrow=c(1,2), mar=c(4,6,3,2))
    hi <- lm.influence(model)$hat
    ei <- model$residuals
    
    halfnorm(hi, main='Leverage', ylab=bquote('Leverage ('~H[ii]~') of observations (sorted)'))

    sigmahat <- summary(model)$sigma
    ri <- ei / sigmahat * sqrt(1-hi)
    plot(y=ri,x=ei, ylab=bquote(r[i] == frac(hat(epsilon)[i],
                                             hat(sigma)*sqrt(1-h[i]))),
         xlab='raw residual', main='(Internally) Studentized residual')

    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,5,3,2))
    #readline('Leverage Plots ... Press [Enter]...');leveragePlots(model)
} 
ds_outlier <- function(model, alpha=0.05){
    n <- length(model$residuals)
    k <- length(model$coef)
    
    ti <- abs(rstudent(model))
    pvalue <- 1- pt(ti,df=n-k-1)
    outliers <- ti > abs(qt(alpha/(2*n), df=n-k-1))

    if(sum(outliers)==0) {
        results <- list(Outliers="No outlier",
                        min_pvalue=min(pvalue),
                        max_t=max(ti))
        print(results)
        return(results)
    }else{
        
        results <- list(n_outliers=length(outliers[outliers]),
                        Outliers=outliers[outliers],
                        pvalue=pvalue[outliers],
                        t=ti[outliers])
        print(results)
        return(results)
    }
}
ds_residualsDiagnostic <- function(model){
    par(mfrow=c(2,2))
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,5,3,2))
    plot(model)
}
ds_partialRegression <- function(model){
    avPlots(model,
            main=bquote('Partial Regression Plot: '~hat(epsilon)[(y~'~'~  x[(j)])]~'vs'~hat(epsilon)[(x[j]~'~'~ x[(j)])]~' and partial coefficients'~hat(beta)[j]), grid=F, pch=20)
}
ds_partialResidual <- function(model){
    k <- length(names(model$model))

    m <- ceiling(sqrt(ncol(data)))
    if(m*(m-1)>=ncol(data))
        par(mfrow=c(m,m-1))
    else
        par(mfrow=c(m,m))
    
    coef <- names(model$coeff)[-1]
    termplot(model, partial.resid=T, terms=1:(k-1),
             ylab=paste('e + ',coef,'*b'), main='Partial Residual Plot')
}
ds_collinearity <- function(model){
    ## VIF
    corrMatrix <- cor(model$model)
    ## VIF > 2?
    VIF <- cbind(VIF=vif(model),sqrtVIFbigger2=sqrt(vif(model)) > 2)
    X <- as.matrix(model$model)
    XtX <- t(X) %*% X
    lambda <- eigen(XtX)$values
    Eigen <- cbind(kappa=sqrt(lambda/min(lambda)),Eigen=eigen(XtX)$values)
    return(list(corrMatrix=corrMatrix, VIF=VIF, Eigen=Eigen))
}
ds_diagnostic_lm <- function(model){
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,5,3,2))
    readline('Cook Distance ... Press [Enter]...');residualsDiagnostic(model)
    readline('Leverage ... Press [Enter]...');leverage(model)
    readline('Ouliers ... Press [Enter]...');outlier(model)
    readline('Partial Regression Plot ... Press [Enter]...');partialRegression(model)
    readline('Partial Residuals Plot ... Press [Enter]...');partialResidual(model)
    readline('Colinearity ... Press [Enter]...');collinearity(model)
}
ds_diagnosticFunc <- function(){
    print(rbind(
        c('diagnostic','run all func'),
        c('leverage',''),
        c('outlier',''),
        c('residualsDiagnostic',''),
        c('partialRegression',''),
        c('partialResidual',''),
        c('colinearity','')))
    }

## }}}
## {{{ GLM }}}

## https://strengejacke.wordpress.com/2013/03/22/plotting-lm-and-glm-models-with-ggplot-rstats/

## tables
## ------
ds_glm2tbl    <- function(model, digits=4){
    model       = broom::tidy( model , conf.int=T)
    model[,-1]  = round(model[,-1],digits)
    return(model)
}
summaryMultin <- function(model, show.ci=F){
    Beta   <- summary(model)$coefficients
    seBeta <- summary(model)$standard.errors
    z      <- summary(model)$coefficients/summary(model)$standard.errors
    ci     <- confint(model)
    p      <- 2*(1 - pnorm(abs(z), 0, 1))
    ncaty <- nrow(Beta)
    yj <- rownames(Beta)
    results <- list()
    if (!show.ci){    
        for (i in 1:length(yj)){
            results[[i]] <- 
                data.frame(Estimate=Beta[i,],
                           Std.Error=seBeta[i,],
                           z=z[i,],
                           'pvalue'=p[i,],
                           '...'= sig(p[i,]))
        }
    }else{   
        for (i in 1:length(yj)){
            ci.l <- ci[,1,i]
            ci.u <- ci[,2,i]
            results[[i]] <- 
                data.frame(Estimate=Beta[i,],
                           Std.Error=seBeta[i,],
                           z=z[i,],
                           'pvalue'=p[i,],
                           '...'= sig(p[i,]),
                           ci.l=ci.l,
                           ci.u=ci.u)
        }
    }
    names(results) <- yj
    results$stars <- c('Signif. codes:  <0.001 \'***\';  <0.01 \'**\'; <0.05 \'*\'; <0.1 \'.\'')
    results$deviance <- deviance(model)
    return(results)
}

## plots
## -----
ds_RegLine_glm   <- function(y,x, w=NULL, family='bernoulli', shaded.area=T, se.lines=F,
                           shaded.area.col='grey87', lty=1, p.value.legend=T, points=F, leg.pos='topright', grid=T, add=F, ...){
    oldpar <- par(no.readonly = T)
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,4,3,1), mgp = c(2,.6,0))
    model = glm(y~x, family='binomial')
    x.new = seq(min(x, na.rm=T), max(x,na.rm=T), length=100)
    pred = predict(model, newdata=data.frame(x=x.new), interval="confidence",  se.fit=T, type='response')
    pred.prob = pred$fit
    se = pred$se.fit
    ub=pred.prob + 1.96*se
    lb=pred.prob - 1.96*se
    if(!add) plot(x,y, col='white')
    if (grid) grid()
    if (points) points(x,y)
    if (shaded.area) polygon(x=c(x.new,rev(x.new)), y=c(lb, rev(ub)), col = shaded.area.col, border = NA)
    if (se.lines) {
        lines(x.new, pred.prob+1.96*se, lty=2, col="black")
        lines(x.new, pred.prob-1.96*se, lty=2, col="black")
    }
    lines(x.new, pred.prob, col="black")
    if(p.value.legend){
        p.value=round(summary(model)$coefficients[2,'Pr(>|z|)'], 2)
        beta=round(summary(model)$coefficients[2,'Estimate'], 2)
        legend(leg.pos, legend= paste('beta=',beta,'  p-value=',p.value), bty='n')
    }
    par(oldpar)
}
ds_ppplot_glm    <- function(model, new_data=NULL, x='', clse=F, cluster=NULL,
                          col='black', points=F, xobs,yobs, grid=T, shaded.area=T,shaded.area.col='grey87', lines=F){
    oldpar <- par(no.readonly = T)

    if (is.null(new_data)){
        n = 200
        new_data = .edar_get_new_data(model,n,x)
    }
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,5,3,2))
    if(clse){
        phat <- predict(model, newdata=new_data, se.fit=T, type='response',
                        clcov=plm::vcovHC(model, cluster=cluster, type='HC3'))$fit
        se   <- predict(model, newdata=new_data, se.fit=T, type='response',
                        clcov=plm::vcovHC(model, cluster=cluster, type='HC3'))$se.fit
    }else{
        phat <- predict(model, newdata=new_data, se=T, type='response')$fit
        se   <- predict(model, newdata=new_data, se=T, type='response')$se.fit
    }
    ci.lower <- phat-1.96*se
    ci.upper <- phat+1.96*se
    plot(phat ~ new_data[,x], ylim=c(0,1), xlim=c(min(new_data[,x]),max(new_data[,x])),
         ylab='Predicted Probability',pch='', xlab=x)
    if (shaded.area) polygon(x=c(new_data[,x],rev(new_data[,x])), y=c(ci.lower, rev(ci.upper)), col = shaded.area.col, border = NA)
    if (grid) grid()
    if (points) points(x=xobs,y=yobs)
    ## lines
    lines(x=new_data[,x],y=phat, col=col)
    if (lines){
        lines(x=new_data[,x],y=ci.lower, lty=2, col=col)
        lines(x=new_data[,x],y=ci.upper, lty=2, col=col)
    }
    par(oldpar)
}
ds_ggRegLine_glm <- function(x,y, colour.group=NULL, h.panel.group=NULL, v.panel.group=NULL, size=NULL, data, layout=c('horizontal','vertical','squared'), show.grid=T){

    if (!is.null(colour.group)){
        ncol = length(levels(data[,colour.group]))
        col = c('firebrick1', 'black', 'steelblue4', 'yellow3', 'grey', 'green3','purple2', 'cyan', 'lightpink2','orangered', 'blue3')
        cols=col[1:ncol]
    }
    
    g = ggplot(data,aes_string(x=x,y=y,col=colour.group,fill=colour.group))+geom_point(aes_string( size=size))+
        geom_smooth(method='glm',alpha=.2,  method.args = list(family = "binomial"))+theme_bw()+xlab(x)+ylab(y)+
        scale_colour_manual(values = cols)+
        scale_fill_manual(values = cols) +
        theme(plot.margin = unit(c(1.5,1.5,0.2,0.2), "cm"))
    if (!is.null(h.panel.group) & !is.null(v.panel.group)){g = g +facet_grid(paste0(h.panel.group ,'~ ', v.panel.group))}
    if (!is.null(h.panel.group) &  is.null(v.panel.group)){g = g +facet_grid(paste0(h.panel.group ,'~. '))}
    if ( is.null(h.panel.group) & !is.null(v.panel.group)){g = g +facet_grid(paste0('.~',v.panel.group ))}
    plot(g)
    grid::grid.text(unit(0.5,"npc") ,unit(.98,'npc'),label = v.panel.group, rot = 0)   # top
    grid::grid.text(unit(0.98,"npc"),unit(0.5,'npc'),label = h.panel.group, rot = 270) # right
    invisible(g)
}

## }}}

