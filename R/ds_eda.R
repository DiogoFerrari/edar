
## {{{ Describe the data }}}

.edar_describe2 <- function(data, col.size=120, digits=4){
    default.width  = options()$width
    options(width=col.size)
    print( mosaic::inspect(data, digits=digits) )
    options(width=default.width)
    invisible(mosaic::inspect(data))
}
## {{{ doc }}}
#' Descriptive Statistics in the Treatment and Control Groups
#'
#' This function generates a table with descriptive statistics of
#' numerical variables by group (Treatment versus Control groups)
#'
#'
#' @param data A data frame that contains a dummy indicator
#'             variable for the treatment and control
#'             groups
#' @param treatmentVar a string with the name of the indicator
#'                     variable of the treatment
#' @param alpha number between 0 and 1. It is used to ...
#' @param ttest boolean, indicates if a t-test of difference in
#'              means between treatment and control groups is
#'              computed
#' @param showMahalanobisDist boolean, indicates if the Mahalanobis
#'                            distance is computed
#' @param showpscore boolean, indicates if the pscore is computed
#' @param showLinPscore ...
#' @param col.size integer, size of the columns in the result
#'                 printed in the screen
#' @return The function returns a list with named elements. The
#'         element \code{balance} constains a table with the
#'         statistics of all numerical variables by treatment
#'         and control groups. The element \code{pscore} contains
#'         the pscore of all observations. The element \code{lpscore}
#'         of the list is the logarithm of pscore.
#'
#' @details The statistics computed here are recomended in
#' \itemize{
#'  \item Imbens, G. W., & Rubin, D. B. (2015). Causal inference in statistics, social, and biomedical sciences: an introduction. : Cambridge University Press.
#' }
#'
#' @examples
#' data(edar_survey)
#' edar_survey  %>% edar_balance(., treatmentVar='treat')
#' 
#' @export
## }}}
edar_balance   <- function(data, treatmentVar, alpha=0.05, ttest=F, showMahalanobisDist=T,showpscore=T,showLinPscore=T, col.size=120){
    op.default <- options()
    on.exit(options(op.default), add=TRUE)

    options(width=col.size)

    data[,treatmentVar] = as.numeric(data[,treatmentVar]  %>% dplyr::pull())
    data = data %>%
        dplyr::select_if(is.numeric) %>%
        as.data.frame
    idx <- data[,treatmentVar]==1
    idxCov <- which(colnames(data) != treatmentVar)
    treated <- data[ idx,idxCov]
    control <- data[!idx,idxCov]
    Tr <- data[,treatmentVar]
    ## Std diff in mean
    ## ----------------
    muc = apply(control,2, mean,na.rm=T)
    mut = apply(treated,2,mean,na.rm=T)
    s2t = apply(treated,2,var,na.rm=T)
    s2c = apply(control,2,var,na.rm=T)
    delta = (mut - muc)/sqrt((s2t+s2c)/2)

    ## t-test difference in means
    ## --------------------------
    nt     <- nrow(treated)
    nc     <- nrow(control)
    t      <- (mut-muc) / sqrt( s2t/nt + s2c/nc)
    df     <- ( s2t/nt + s2c/nc)^2 / ( (s2t/nt)^2/(nt-1) +  (s2c/nc)^2/(nc-1) )
    pttest <- 2*(1-pt(abs(t), df=df))

    ## log ratio of std dev
    ## --------------------
    gamma <- log(sqrt(s2t)/sqrt(s2c))

    ## proportion of tail of the other group
    ## -------------------------------------
    ltt <- apply(treated,2,quantile,prob=c(alpha/2), na.rm=T)
    utt <- apply(treated,2,quantile,prob=c(1-alpha/2), na.rm=T)

    ltc <- apply(control,2,quantile,prob=c(alpha/2), na.rm=T)
    utc <- apply(control,2,quantile,prob=c(1-alpha/2), na.rm=T)

    pit <- vector()
    pic <- vector()
    for (i in 1:ncol(treated)){
        x <- treated[,i]
        pit[i] <- (sum(x<ltc[i],na.rm=T) + sum(x>utc[i],na.rm=T))/
          length(x[!is.na(x)])
        x <- control[,i]
        pic[i] <- (sum(x<ltt[i],na.rm=T) + sum(x>utt[i],na.rm=T))/
          length(x[!is.na(x)])
    }

    ## prop score
    ## ----------
    X            <- as.matrix(data[,idxCov])
    pscore       <- glm(Tr ~ X,family=binomial(link="logit")) 
    phat         <- pscore$fitted.values
    phatlin      <- pscore$linear.predictors
    ## statistics for the raw pscore
    pscore_meant <- mean(phat[Tr==1], na.rm=T)
    pscore_vart  <- var(phat[Tr==1], na.rm=T)
    pscore_meanc <- mean(phat[Tr==0], na.rm=T)
    pscore_varc  <- var(phat[Tr==0], na.rm=T)
    pscore_dif   <- (pscore_meant - pscore_meanc)/sqrt((pscore_vart+pscore_varc)/2)
    pscore_lnSTD <- log(sqrt(pscore_vart)/sqrt(pscore_varc))
    pscore_pit <- quantile(phat[Tr==1],prob=c(alpha/2,1-alpha/2), na.rm=T)
    pscore_pic <- quantile(phat[Tr==0],prob=c(alpha/2,1-alpha/2), na.rm=T)
    pscore_pialphat <- (sum(phat[Tr==1]<pscore_pic[1],na.rm=T) +
                     sum(phat[Tr==1]>pscore_pic[2],na.rm=T))/
                     length(phat[Tr==1][!is.na(phat[Tr==1])])
    pscore_pialphac    <- (sum(phat[Tr==0]<pscore_pit[1],na.rm=T) +
                     sum(phat[Tr==0]>pscore_pit[2],na.rm=T))/
                     length(phat[Tr==1][!is.na(phat[Tr==1])])
    ## statistics for the linearized pscore
    lpscore_meant <- mean(phatlin[Tr==1], na.rm=T)
    lpscore_vart  <- var(phatlin[Tr==1], na.rm=T)
    lpscore_meanc <- mean(phatlin[Tr==0], na.rm=T)
    lpscore_varc  <- var(phatlin[Tr==0], na.rm=T)
    lpscore_dif   <- (lpscore_meant - lpscore_meanc)/
      sqrt((lpscore_vart+lpscore_varc)/2)
    lpscore_lnSTD <- log(sqrt(lpscore_vart)/sqrt(lpscore_varc))
    lpscore_pit <- quantile(phatlin[Tr==1],prob=c(alpha/2,1-alpha/2), na.rm=T)
    lpscore_pic <- quantile(phatlin[Tr==0],prob=c(alpha/2,1-alpha/2), na.rm=T)
    lpscore_pialphat <- (sum(phatlin[Tr==1]<lpscore_pic[1],na.rm=T) +
                     sum(phatlin[Tr==1]>lpscore_pic[2],na.rm=T))/
                     length(phatlin[Tr==1][!is.na(phatlin[Tr==1])])
    lpscore_pialphac    <- (sum(phatlin[Tr==0]<lpscore_pit[1],na.rm=T) +
                     sum(phatlin[Tr==0]>lpscore_pit[2],na.rm=T))/
                     length(phatlin[Tr==1][!is.na(phatlin[Tr==1])])
    
    pscore <- round(c(pscore_meant,sqrt(pscore_vart),pscore_meanc,
                      sqrt(pscore_varc), pscore_dif,pscore_lnSTD,pscore_pialphat,
                      pscore_pialphac),3)
    lpscore <- round(c(lpscore_meant,sqrt(lpscore_vart),lpscore_meanc,
                       sqrt(lpscore_varc),
                       lpscore_dif,lpscore_lnSTD,lpscore_pialphat,
                      lpscore_pialphac),3)
    ## Mahalanobis distance
    ## --------------------
    sigmat <- var(treated, na.rm=T)
    sigmac <- var(control, na.rm=T)
    avCovInv <- solve( (sigmat + sigmac)/2 )
    mahalanobis <- sqrt( t(mut - muc) %*% avCovInv %*% (mut - muc) )

    ## final results
    ## -------------
    if (ttest){
        pscore <- round(c(pscore_meant,sqrt(pscore_vart),pscore_meanc,
                          sqrt(pscore_varc),
                          pscore_dif,NA,pscore_lnSTD,pscore_pialphat,
                          pscore_pialphac),3)
        lpscore <- round(c(lpscore_meant,sqrt(lpscore_vart),lpscore_meanc,
                           sqrt(lpscore_varc),
                           lpscore_dif,NA,lpscore_lnSTD,lpscore_pialphat,
                           lpscore_pialphac),3)        
        results <- cbind(mut=mut,st=sqrt(s2t), muc=muc, sc=sqrt(s2c),
                         NorDiff=delta, "p(T>|t|)"=pttest,lnRatioSdtDev=gamma,
                         pit=pit,pic=pic)
        mahalan <- as.numeric(c(rep('',4),round(mahalanobis,3),rep('',4)))
        N <- as.numeric(c(round(sum(Tr==1),0),'',round(sum(Tr==0),0),c(rep('',6))))
    }else{
        pscore <- round(c(pscore_meant,sqrt(pscore_vart),pscore_meanc,
                      sqrt(pscore_varc), pscore_dif,pscore_lnSTD,pscore_pialphat,
                      pscore_pialphac),3)
    lpscore <- round(c(lpscore_meant,sqrt(lpscore_vart),lpscore_meanc,
                       sqrt(lpscore_varc),
                       lpscore_dif,lpscore_lnSTD,lpscore_pialphat,
                      lpscore_pialphac),3)
        results <- cbind(mut=mut,st=sqrt(s2t), muc=muc, sc=sqrt(s2c),
                         NorDiff=delta, lnRatioSdtDev=gamma,
                         pit=pit,pic=pic)
        mahalan <- as.numeric(c(rep('',4),round(mahalanobis,3),rep('',3)))
        N <- as.numeric(c(round(sum(Tr==1),0),'',round(sum(Tr==0),0),
                          c(rep('',5))))        
    }
    results <- rbind(results,MahalanobisDist=mahalan)
    results <- rbind(results,pscore=pscore, LinPscore=lpscore, N=N)
    
    if(!showMahalanobisDist){
        results <- results[!rownames(results) == 'MahalanobisDist',]}
    if(!showpscore){
        results <- results[!rownames(results) == 'pscore',]}
    if(!showLinPscore){
        results <- results[!rownames(results) == 'LinPscore',]}

    cat('\n')
    print(results, na.print = " ")
    invisible(list(balance=results, pscore=phat, lpscore=phatlin))
}
## {{{ doc }}}

#' Describe the variables of a data frame
#'
#' This function generate two tables. One summarizes the non-numerical
#' variables, the other the numerical ones.
#'
#' @param data a data frame
#' @param weight a vector with the weights
#' @param minMax boolean, display the minimum and maximum in the summary
#'               when minMax=T
#' @param digits the number of digits to display
#' @param maxVars integer, the maximum number of variables in the data to summarize.
#'        It accepts any number, by it may result in slow computaiton of the summary
#'        depending on the size of the data set
#' @param col.size integer, the size of the column to display the summary
#'
#' @return It returns a list of tibbles. The first contains the summary of the
#'         categorical variables, the second the summary of the numerical ones.
#'
#' @examples
#' data(edar_survey)
#' edar_survey %>% edar_describe()
#' 
#' tb = edar_survey %>% edar_describe()
#' tb$Cat
#' tb$Num
#' @export
## }}}
edar_describe  <- function(data, weight=NULL, minMax=T, digits=2, maxVars=50, col.size=120){
    if(ncol(data)>maxVars) return(cat("\n\n Max variables permited by default:",50,"\n \n You can overwrite using \"maxVars\" parameter. It may result in inefficiency\n\n"))

    op.default <- options()
    on.exit(options(op.default), add=TRUE)
    options(width=col.size)
    
    idxChar <- sapply(sapply(data, class), function(x) x[[1]]) == 'factor' |
        sapply(sapply(data, class), function(x) x[[1]]) == 'character'  |
        sapply(sapply(data, class), function(x) x[[1]]) == 'ordered'

    ## Numerical Variables
    ## -------------------
    if (sum(!idxChar)>0)
    {
        NumVars = sum(!idxChar)
        CharVars =  sapply(data, class) == 'character'
        Factors = sapply(sapply(data, class), function(x) x[[1]]) == 'factor'
        Ordered = sapply(sapply(data, class), function(x) x[[1]]) == 'ordered'
        
        dataVars <- data.frame(N=dim(data)[1], nVars=dim(data)[2],
                               Char=sum(CharVars), Factor=sum(Factors),
                               Numerical=NumVars)
        dataVars <- list(VarsType=dataVars,Strings=names(data)[CharVars],
                         Factors=names(data)[Factors],
                         Ordered=names(data)[Ordered],
                         Numerical=names(data)[!idxChar])

        ## summary statistics
        N      = apply(data[,!idxChar],2,length)
        NAs    = apply(data[,!idxChar],2,function (x) sum(is.na(x)))
        if(is.null(weight)){
            mean   = apply(data[,!idxChar],2,mean,na.rm=TRUE)
            median = apply(data[,!idxChar],2,median,na.rm=T)
            sd     = apply(data[,!idxChar],2,sd,na.rm=T)
            se     = sd / sqrt(N)
            q      = apply(data[,!idxChar],2,quantile,probs=c(.05,.95),na.rm=T) 
        }else{
            mean   = apply(data[,!idxChar],2,function(x) questionr::wtd.mean(x,na.rm=TRUE, weight=weight))
            median = apply(data[,!idxChar],2,function(x) isotone::weighted.median(x,w=weight))
            sd     = apply(data[,!idxChar],2,function(x) sum(weight*(x - questionr::wtd.mean(x,na.rm=TRUE, weight=weight))^2))
            se     = sd/sqrt(N)
            q      = apply(data[,!idxChar],2,function(x) Hmisc::wtd.quantile(x, probs=c(.05,.95), weights=weight, na.rm=T)) 
        }
        numericSummary <- data.frame(name = names(data[,!idxChar]),
                                     N=N, NAs=NAs, mean=mean
                                    ,sd=sd,se=se,q.05= q[1,],
                                     median=median,q.95=q[2,])
        ## minimum and maximum
        if(minMax==T) {
            minimum <- apply(data[,!idxChar],2,min, na.rm=T)
            maximum <- apply(data[,!idxChar],2,max, na.rm=T)        
            numericSummary <- cbind(numericSummary, minimum, maximum)
            numericSummary <- numericSummary[c("name","N", "NAs", "mean","sd","se","median",
                                               "minimum","maximum","q.05","q.95")]
        }
        row.names(numericSummary) = NULL
        numericSummary = tibble::as_data_frame(numericSummary)
    }else
    {
        numericSummary = NULL
    }

    ## categorical variables
    ## ---------------------
    if (sum(idxChar)>0)
    {
        tables <- list()
        if (sum(idxChar)>0){
            for (i in 1:sum(idxChar)){        
                if (is.null(weight)){
                    tables[[i]] <- table(as.data.frame(data[,idxChar])[,i],useNA = "always")
                }else{
                    tables[[i]] <- round(questionr::wtd.table(x = as.data.frame(data[,idxChar])[,i], weights = weight, na.show=T, na.rm=F), digits=digits)
                }
            }
            names(tables) <- names(idxChar)[idxChar==T]
        }else{
            tables <- 'no categorical or factor variables'
        }
        tables.summary =  do.call(rbind,lapply(tables,data.frame))
        tables.summary = cbind(tables.summary, round(100*tables.summary[,2]/nrow(data), 2))
        names(tables.summary) = c('Category','N',"Freq")
        tables.summary$Variable = row.names(tables.summary)
        tables.summary = tables.summary[,c(4,1:3)]
        tables.summary$Variable= gsub(tables.summary$Variable, pattern=".1$",replacement='')
        tables.summary$Variable[grepl(tables.summary$Variable, pattern="[[0-9]]*$")]=''
        row.names(tables.summary) = 1:nrow(tables.summary)
        levels(tables.summary$Category) = c(levels(tables.summary$Category), "NAs")
        tables.summary$Category[is.na(tables.summary$Category)] = "NAs"

        ## rows with NA
        idxNA <- which(apply(data,1,function (x) sum(is.na(x)))!=0)
        rowNA <- apply(data,1,function (x) sum(is.na(x)))[idxNA]
        rowNA <- data.frame(line=idxNA,total.NAs=rowNA)

        n = nrow(data)
        tables_summary = lapply(tables, function(x) paste0(names(x),' (',100*x/n,"%)",  collapse='; ') )
        tables_summary = tibble::data_frame(name = names(tables),
                                            N    = nrow(data),
                                            levels = unlist(lapply(tables, length)),
                                            missing = unlist(lapply(tables, function(x) x[is.na(names(x))])),
                                            class =unlist(lapply(subset(data, select=names(tables)), class)),
                                            freq_tables = tables,
                                            distribution = c(do.call(rbind, tables_summary))
                                            )
    }else
    {
        tables_summary = NULL
    }
    
    results <- list(Num = numericSummary,
                    Cat = tables_summary
                    ##Vars=dataVars,
                    ##CatVarsSummaryTable=tables.summary
                    ## NAsRow=rowNA
                    )
    cat("\nCategorical Variables:\n")
    print(results$Cat)
    cat('\nNumerical Variables:\n')
    print(results$Num)
    invisible(results)

}
## {{{ docs }}}

#' Smart univariate summary of the categorical variables
#'
#' This function summarizes the distribution of the non-numerical
#' variables. It bundles together all the variables that have the same
#' categories and displays their summary in a single table.
#'
#'
#' @param data a data frame
#' @param vars a vector with the name of the variables to display.
#'             If none is provided (vars=NULL), it summarizes all
#'             the categorical variables
#' @param print a string with the type of summary to print in the
#'              screen (see \code{value} below for the complete
#'              description of the return). Values can be one of
#'              'prop' (display the proportions), 'count' (display
#'              the counts), or 'prop_count' (display both the proportion
#'              and counts).
#' @param weight vector with the weights
#' @param digits integer, the number of digits to print
#'
#' @return It retuns a tibble in which the columns are lists. The columns
#'         of the tibble are \code{var.idx}, which is a list with the idx
#'         of the variables, \code{vars}, which is a list with the
#'         variable names bundled together. Finally \code{table.p},
#'         \code{table.n}, and \code{table.pn} are the tables with,
#'         respectively, the proportions, counts, and both proportions
#'         and counts for the bundled variables
#'
#' @examples
#' data(edar_survey)
#' edar_survey %>% edar_bundle_cat()
#' tb = edar_survey %>% edar_bundle_cat()
#' tb
#' tb$var.idx
#' tb$table.n
#' tb$table.p
#' tb$table.pn
#' 
#'@export
## }}}
edar_bundle_cat <- function(data, vars=NULL, print.summary=NULL, weight=NULL, digits=4, width.size=150){
    if(!is.null(vars)){
        data_cat = data.frame(data[,vars])
    }else{
        data_cat = .edar_select_categorical(data) %>% dplyr::mutate_if(is.character, as.factor)
    }

    op.default <- options()
    options(width=width.size)
    on.exit(options(op.default), add=TRUE)

    list.of.levels = lapply(data_cat,levels) %>% purrr::map(sort) %>% purrr::map(paste, collapse='') %>% unlist
    duplic = lapply(1:length(list.of.levels), function(i) which(list.of.levels[i] == list.of.levels ))
    bundles = duplic[!duplicated(duplic)]    
    summaries = tibble::data_frame()
    for (bundle in bundles){
        tab.p=t(apply(data.frame(data_cat[,names(bundle)]),2, function(x)
            round(prop.table(questionr::wtd.table(x = x, weights = weight, na.show=T, na.rm=F)),2)))
        tab.n=t(apply(data.frame(data_cat[,names(bundle)]),2, function(x)
            questionr::wtd.table(x = x, weights = weight, na.show=T, na.rm=F)))
        tab.pn=t(apply(data.frame(data_cat[,names(bundle)]),2, function(x)
            paste0(paste(100*round(prop.table(questionr::wtd.table(x = x, weights = weight, na.show=T, na.rm=F)),digits), '%'),
                   paste0(' (N=',questionr::wtd.table(x = x, weights = weight, na.show=T, na.rm=F),')'))))
        row.names(tab.p)=names(bundle)
        row.names(tab.n)=names(bundle)
        row.names(tab.pn)=names(bundle)
        colnames(tab.p) = colnames(tab.n)
        colnames(tab.pn) = colnames(tab.n)
        summaries = summaries %>%
            dplyr::bind_rows(
                tibble::data_frame(var.idx = list(bundle),
                                   vars = list(names(bundle)),
                                   table.p = list(tab.p),
                                   table.n = list(tab.n),
                                   table.pn = list(tab.pn))
                )
    }
    if (! is.null(print.summary ))
    {
        if (! print.summary %in% c('prop', 'count','prop_count'))
        {
            stop("\n\n The parameter print.summary must be NULL or one of: 'prop', 'count','prop_count'\n\n")
        }else{
            if(print.summary[1]=='prop') print(summaries$table.p)
            if(print.summary[1]=='count') print(summaries$table.n)
            if(print.summary[1]=='prop_count') print(summaries$table.pn)
        }
    }
    invisible(summaries)
}

## }}}
## {{{ Univariate   Analysis }}}

## continuous variables
## --------------------
edar_plotdensity  <- function(x, ub=quantile(x,.975), lb=quantile(x,.025), shaded.area=F, shaded.area.col='grey90', lty=1, bty='n', add=F, grid=T, ...){
    data.rug = sample(x, size=min(1000, length(x)))
    if (add) {
        lines(density(x, adjust=1), lty=lty,  cex.axis=.9, ...)
    }else{
        plot(density(x, adjust=1), lty=lty,  cex.axis=.9, ...)
    }
    rug(x=data.rug)
    if(shaded.area){
    ## shadded area
        ## x.new <- density(x, adjust=1)$x
        x.new<- density(x, adjust=1)$x
        y <- density(x, adjust=1)$y
        x.coord <- c(lb, x.new[lb <= x.new & x.new <= ub], ub)
        y.coord <- c(0,  y[lb <= x.new & x.new <= ub], 0)
        polygon(x.coord,y.coord,col=shaded.area.col, lty=lty, border= NA)
        length(x.coord)
        length(y.coord)
    }
    if(grid) grid()
}
edar_densities <- function(data, qline=T, quantiles=c(.05,.995), ncat=T){
    op=par(no.readonly=TRUE)


    data <- data.frame(data)
    idxChar <- sapply(sapply(data, class), function(x) x[[1]]) == 'factor' |
        sapply(sapply(data, class), function(x) x[[1]]) == 'character'  |
        sapply(sapply(data, class), function(x) x[[1]]) == 'ordered'
    
    idxNum <- !idxChar
    idxFactor <- sapply(sapply(data, class), function(x) x[[1]]) == 'factor' |
        sapply(sapply(data, class), function(x) x[[1]]) == 'ordered'
    
    ## check if there is any numerical variable for the summary
    maxVars <- 16
    if(ncol(data)>16)
        stop(cat("\n\n Max variables permited by default:",maxVars))

    m <- ceiling(sqrt(ncol(data)))
    if(m*(m-1)>=ncol(data))
        par(mfrow=c(m-1,m))
    else
        par(mfrow=c(m,m))

    if (sum(idxNum)>0){
        X <- as.data.frame(data[,idxNum])
        for (i in 1:ncol(X)){
            x <- X[,i]
            edar_plotdensity(x,main=names(data[,idxNum])[i], qline)
            q <- quantile(x,probs=quantiles)
            abline(v=q, col='red', lty=2)

        }
    }
    if (sum(idxFactor)>0 ){
        X <- as.data.frame(data[,idxFactor])
        for (i in 1:ncol(X)){
            x <- X[,i]
            barPlot(as.factor(x), title=names(data[,idxFactor])[i])
        }
    }
    par(op)
}

## categorical variables
## ---------------------
edar_barPlot       <- function(y, title='', show.group.name=T, show.group.count=T, col='lightgrey'){
    if(class(y)=='table') {
        n <- sum(table)
        NAs <- 0
        table <- y
        tableProp <- round(y/sum(y),3)
    }else{
        y <- factor(y)
        n <- length(y)
        NAs <- sum(is.na(y))
        table <- table(y)
        tableProp <- round(table(y)/length(y),3)
        
    }
    if (show.group.count){
        N <- paste('\n(N=',table,')', sep='')
        names(tableProp) <-  paste(names(table),N)
    }        
    ypos <- barplot(tableProp, horiz=T, xlim=c(0,1.1), main=title,plot=F)
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,5,3,0))
    barplot(tableProp, horiz=T, xlim=c(0,1.3), col=col, border=NA, xaxt='n')
    title(main=title, cex.main=1.1)
    axis(1, at=seq(0,1,by=.2), labels=seq(0,1,by=.2))
    mtext(text=paste('N=',n,', NA=',NAs,sep=''), cex=.7)
    if(show.group.name) text(x=tableProp,y=ypos, labels=tableProp,cex=.9,pos=4)
}
edar_utable     <- function(x, na='always'){
    addmargins(table(x, useNA=na))
}

## }}}
## {{{ Bivariate    Analysis }}}

## continuous variables
## --------------------
edar_heatPlot <- function(x='', y='', xlab='x', ylab='y', title='', text='', replicationPlot=F, clean=F, ...){
    colors_palette <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11,'Spectral')))
    r <- colors_palette(32)
    h1 <- hist(x, breaks=30, plot=F)
    h2 <- hist(y, breaks=30, plot=F)
    top <- max(h1$counts, h2$counts)
    k <- MASS::kde2d(x, y, n=100)
    ## margins
    if(clean){
        image(k, col=r, xlab='', ylab='', bg='black', xaxt='n', yaxt='n', ...)
    }else{
        image(k, col=r, xlab=xlab, ylab=ylab, bg='black', ...)     
    }
    title(title)
}
edar_heatPlotHist <- function( x='', y='', title='', subtitle='', xlab='x', ylab='y', oldstyle=F, breaks=30, 
                         xlim=c(min(x), max(x)), ylim=c(min(y), max(y)), ...){
    color_palette<- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11,'Spectral')))
    r <- color_palette(32)
    h1 <- hist(x, breaks=seq(xlim[1],xlim[2],length=breaks), plot=F)
    h2 <- hist(y, breaks=seq(ylim[1],ylim[2],length=breaks), plot=F)
    top <- max(h1$counts, h2$counts)
    k <- MASS::kde2d(x, y, n=100)
                                        # margins
    oldpar <- par(no.readonly = T)
    graphics::layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(5,5,.1,.1))
    image(k, col=r, xlab=xlab, ylab=ylab, oldstyle=oldstyle, useRaster=T, xlim=xlim, ylim=ylim) #plot the image
    ##points(df$x,df$y, cex=.3)
    par(mar=c(0,4.5,3,0))
    barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='lightgrey')
    title(title)
    mtext(text=subtitle, cex=.8)
    par(mar=c(5,0,0.1,4))
    barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='lightgrey', horiz=T)
    par(oldpar)
}
edar_contourPlot <- function(x='', y='', title='', nlevels=10, show.points=FALSE, color='RdBu', rev.color=T, subtitle='', ...){
    max.col <- min(nlevels, RColorBrewer::brewer.pal.info[color,]$maxcolors)
    if (rev.color){color_palette  <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(max.col,name=color)))
    }else{color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(max.col,name=color))}
    r <- color_palette (nlevels+1)
    k <- MASS::kde2d(x, y, n=100)
    ## margins
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,5,3,2))    
    if(show.points) {
        plot(x, y, cex=.3, ...)
        contour(k, col=r, nlevels=nlevels, add=T)        
    }else{
        contour(k, col=r, nlevels=nlevels, ...)    
    }
    title(main=title)
    mtext(text=subtitle, cex=.8)
}
edar_contourPlotHist <- function(x='', y='', title='', subtitle='', nlevels=10, color='RdBu',
                               show.points=F, rev.color=T, adjust=50,
                            breaks=30, v='', h='', a='', b='',
                            v.col='black', v.lty=1, v.lwd=1, h.col='black', h.lty=1, h.lwd=1, ab.col='black', ab.lty=1, ab.lwd=1,
                            xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),leg.position=NULL, leg.text='', leg.lty=1, leg.bty='n', leg.cex=.8, 
                            ...){
    
    max.col <- min(nlevels, brewer.pal.info[color,]$maxcolors)
    if (rev.color){rf <- colorRampPalette(rev(brewer.pal(max.col,name=color)))
    }else{rf <- colorRampPalette(brewer.pal(max.col,name=color))}
    r <- rf(nlevels+1)    
    k <- kde2d(x, y, n=adjust)
    
    h1 <- hist(x, breaks=seq(xlim[1],xlim[2],length=breaks), plot=F)
    h2 <- hist(y, breaks=seq(ylim[1],ylim[2],length=breaks), plot=F)
    top <- max(h1$counts, h2$counts)
    ## margins
    oldpar <- par(no.readonly = T)
    graphics::layout(matrix(c(1,0,3,2),2,2,byrow=T),c(3,1), c(1,3))
    
    par(mar=c(0.1,4.5,3,0))
    barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='lightgrey')
    title(title)
    mtext(text=subtitle, cex=.8)
    par(mar=c(5,0.1,0.1,4))
    barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='lightgrey', horiz=T)

    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(5,5,.1,.1))
    if(show.points) {
        plot(x, y, cex=.3, ...)
        contour(k, col=r, nlevels=nlevels, xlim=xlim, ylim=ylim, add=T, ...)        
    }else{
        contour(k, col=r, nlevels=nlevels, xlim=xlim, ylim=ylim, ...)    
    }
    if (v[1]!='')         abline(v=v,   col=v.col,  lty=v.lty,  lwd=v.lwd, ...) 
    if (h[1]!='')         abline(h=h,   col=h.col,  lty=h.lty,  lwd=h.lwd, ...)
    if (a[1]!='' & b[1]!='') abline(a=a,b=b, col=ab.col, lty=ab.lty, lwd=ab.lwd, ...)
    if (!is.null(leg.position)){
        legend(leg.position, legend=leg.text, lty=leg.lty, bty=leg.bty, cex=leg.cex)    
    }
    
    par(oldpar)
}
edar_ggscatter <- function(x,y,text=NULL,points=T, data, col='black', slope=NULL, intercept=NULL,
                         h=NULL, v=NULL, h.lty='solid', v.lty='solid', si.lty='solid',...){
    g = ggplot2::ggplot(data, ggplot2::aes_string(x, y, label = text)) +
        ggplot2::theme_bw()+
        ggplot2::theme(legend.position='bottom',
              ## panel.grid.major=element_blank(),
              ## panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              axis.line=element_line(size=.2)) 
    if (!is.null(text)){g = g+ ggrepel::geom_text_repel() }
    if (points) g=g +  geom_point(color = col)  
    if (!is.null(slope) & !is.null(intercept)) g = g + geom_abline(slope=1,intercept=0, linetype=si.lty)
    if (!is.null(h) ) g = g + geom_hline(yintercept=h, linetype=h.lty)
    if (!is.null(v) ) g = g + geom_vline(xintercept=v, linetype=v.lty)

    return(g)
}
edar_summaryBy <- function(data, group='', vars=NULL,summaries=NULL){
    if(is.null(vars))
        data_num = edar_select_numerical(data)
    else
        data_num = data.frame(data[,vars])
    data_num  = data_num %>%
        dplyr::bind_cols(setNames(data.frame(data[,group]), nm=group)) %>%
        dplyr::group_by_(group) %>%
        dplyr::summarize_all(dplyr::funs(mean(.,na.rm=T),
                                  sd(.,na.rm=T),
                                  min(.,na.rm=T),
                                  median(.,na.rm=T),
                                  max(.,na.rm=T),
                                  sum(is.na(.))
                                  )) %>%
        dplyr::select(group,contains("mean"), contains("sd"), contains("median"), contains('min'), contains('max'),contains('sum')) %>%
        dplyr::rename_all(funs(stringr::str_replace_all(., 'sum', 'NA')))
    if(!is.null(summaries)){
        if(length(summaries)>1) summaries <- paste(summaries, collapse = "|")
        data_num = data_num %>%
            dplyr::select(group, matches(summaries))
    }
    print(data_num)
    invisible(data_num)
}

## categorical variables
## ---------------------
edar_covMatrix <- function(data){
    oldpar <- par(no.readonly = T)
    par(mfrow=c(1,1))
    data <-edar_select_numerical(data)
    ## idxChar <- sapply(data, class) == 'factor' | sapply(data, class) == 'character'
    ## idxNum <- !idxChar

    table <- cor(data)
    corrplot::corrplot(table, order='AOE', addrect=3)
    par(oldpar)
}
edar_biPlotProp <- function(x,y,var1.label='',var2.label='', xncat=5, yncat=5, N=T, perc=T){
    
    oldpar <- par(no.readonly = T)
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,5,3,2))

    if (class(x)[1] == 'character' | class(y)[1] == 'character')
        stop("Function only accepts factors and numerical variables")

    if (class(x)[1] == 'numeric' | class(x)[1] == 'integer')
        x <- cut(x,xncat)
    if (class(y)[1] == 'numeric')
        y <- cut(y,yncat)
        
    tab <- prop.table(table(x,y, useNA='no'),1)
    tab.n <- table(x,y, useNA='no')
    col=1:ncol(tab)
    par(new=F)
    graphics::matplot(tab,type="l",xlab=var1.label,ylab="Proportion",lty=col, col=col, xaxt = "n", ylim=c(0,1))
    axis(1, at = 1:nrow(tab), labels = rownames(tab), cex.axis = 0.7)
    for (i in 1:nrow(tab)){
        for (j in 1:ncol(tab)){
            text=text.p=text.n = ''
            if(perc) text.p = paste0(100*round(tab[i,j],4)," %" ,  sep='')
            if(N)    text.n = paste0('(N = ', tab.n[i,j],4,")" ,  sep='')
            text=paste(text,'\n', text.p, '\n',text.n,  collapse='')
            text(i, tab[i,j], labels= text, cex=.8)
        }
    }


    legend('topright', col=col, legend=colnames(tab),lty=col, bty='n', title=var2.label)    
    par(oldpar)
}

## }}}
## {{{ Multivariate Analysis }}}

## categorical variables
## ---------------------
edar_ggboxplot <- function(x,y, data, colour.group=NULL, h.panel.group=NULL, v.panel.group=NULL, pos=.2, width=.2, layout=c('horizontal','vertical','squared'), show.grid=T){
    pd = position_dodge(width = pos)
    width = width
    g = ggplot2::ggplot(data, aes_string(x=x, y=y, col=colour.group, fill=colour.group))+
        geom_boxplot(position=pd, width=.2, alpha=.4)+
        theme_bw()+
        theme(legend.position='bottom',
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              axis.line=element_line(size=.2)) +
        xlab(x) +
        ylab(y)+
        theme(plot.margin = unit(c(1.5,1.5,0.2,0.2), "cm"))
    if (!is.null(h.panel.group) & !is.null(v.panel.group)){g = g +facet_grid(paste0(h.panel.group ,'~ ', v.panel.group))}
    if (!is.null(h.panel.group) &  is.null(v.panel.group)){g = g +facet_grid(paste0(h.panel.group ,'~. '))}
    if ( is.null(h.panel.group) & !is.null(v.panel.group)){g = g +facet_grid(paste0('.~',v.panel.group ))}
    if(show.grid) g = g + theme(panel.grid.major=element_line(colour="lightgrey", size=0.2))
    plot(g)
    grid::grid.text(unit(0.5,"npc") ,unit(.98,'npc'),label = v.panel.group, rot = 0)   # top
    grid::grid.text(unit(0.98,"npc"),unit(0.5,'npc'),label = h.panel.group, rot = 270) # right
}

## }}}




