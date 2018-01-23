##
## Author: Diogo A. Ferrari 
## 

## {{{ LM or GLM }}}

ds_plot_fit <- function(x, fitted, se=NULL, yobs=NULL, xobs=NULL, col.lty=NULL, col.pch=NULL, col.lty.se=NULL, shaded.area=T, col.shaded.area=NULL, grid=T, points=F, ...){
    if(is.null(col.lty)) col.lty="lightblue"
    if(is.null(col.pch)) col.pch="#00000044"
    if(is.null(col.lty.se))  col.lty.se="#00000044"
    if (is.null(col.shaded.area)) col.shaded.area ="#00000044"

    lb = fitted - 1.96*se
    ub = fitted + 1.96*se
    
    par.default <- par(no.readonly = TRUE)
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,4,3,1), mgp = c(2,.6,0))
    plot(x, fitted, type='l', col=col.lty)
    lines(x, lb, lty=2, col=col.lty.se)
    lines(x, ub, lty=2, col=col.lty.se)
    if (shaded.area) polygon(x=c(x,rev(x)), y=c(lb, rev(ub)), col = col.shaded.area, border = NA)
    if (grid) grid()
    if (points) points(x=xobs,y=yobs)

    par(par.default) 
}

## }}}
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

