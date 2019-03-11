
## {{{ Describe data (dplyr extension) }}}

## {{{ docs }}}

#' Summarise numerical variables of the data set
#'
#' This function summarises all the numerical variables in the data set.
#'
#' @param df data frame
#' @param group string vector with the names of the grouping variables (can be more than one). The summary will be computed by groups. Default \code{NULL}.
#' @param weight string vector with the name of the column in the data set containing the weights. Default \code{NULL}.
#' @param spread boolean, if \code{TRUE} the summaries for each group are spread along the columns. Default \code{FALSE}
#' @param digits integer, number of digiits to return in the summaries (defualt 4)
#'
#' @return It returns a tibble data frame with summaries for all numerical variables
#'
#' @examples
#' data(starwars, package='dplyr')
#' 
#' summarise_alln(starwars, group=NULL, weight=NULL, spread=FALSE)
#' summarise_alln(starwars, group="gender", weight=NULL, spread=FALSE)
#' summarise_alln(starwars, group="gender", weight=NULL, spread=TRUE)
#'
#' # or use with pipe
#' # starwars  %>% summarise_alln(., group="gender", weight=NULL, spread=T)
#' @export

## }}}
summarise_alln <- function(df, group=NULL, weight=NULL, spread=F, digits=4)
{
    options(warn=-1)
    on.exit(options(warn=0))
    
    flag=FALSE
    ifelse(is.null(weight), df$weight<-1, df$weight <- df %>% dplyr::select_(weight) %>% dplyr::pull(.)) 
    if (is.null(group)) {
        flag=TRUE
        df$grouping_null__ = 1
        group="grouping_null__"
    }
    tab = df %>%
        dplyr::select(-dplyr::one_of(group))  %>% 
        dplyr::select_if(is.numeric) %>%
        dplyr::bind_cols(., df[,group])  %>%
        tidyr::gather(var, value,  -group, -weight) %>%
        dplyr::mutate(na = 1 - is.na(value)) %>%
        dplyr::group_by_("var", .dots=group) %>%
        dplyr::summarise(N      = sum(na),
                         NAs    = sum(is.na(value)),
                         Mean   = ifelse(N!=0, stats::weighted.mean(value, w=weight, na.rm=TRUE), NA_real_),
                         sd     = ifelse(N!=0, sqrt(Hmisc::wtd.var(value, weights=weight, na.rm=TRUE)), NA_real_),
                         se     = ifelse(N!=0, sd/sqrt(N), NA_real_),
                         Median = ifelse(N!=0, isotone::weighted.median(value, w=weight), NA_real_),
                         Min    = ifelse(N!=0, min(value, na.rm = TRUE), NA_real_),
                         Max    = ifelse(N!=0, max(value, na.rm = TRUE), NA_real_),
                         q.025  = ifelse(N!=0, reldist::wtd.quantile (value, q=0.025, na.rm = TRUE, weight=weight), NA_real_),
                         q.25   = ifelse(N!=0, reldist::wtd.quantile (value, q=0.25, na.rm = TRUE, weight=weight), NA_real_),
                         q.75   = ifelse(N!=0, reldist::wtd.quantile (value, q=0.75, na.rm = TRUE, weight=weight), NA_real_),
                         q.975  = ifelse(N!=0, reldist::wtd.quantile (value, q=0.975, na.rm = TRUE, weight=weight), NA_real_),
                         )  %>%
        dplyr::mutate(sd = dplyr::case_when(is.nan(sd) ~ 0, TRUE ~ sd)) %>%   
        dplyr::arrange(var)  %>%
        dplyr::ungroup(.)
    if (flag) {
        tab = tab %>% dplyr::select(-dplyr::one_of(group)) 
    }
    if(spread){
        tab = tab %>%
            tidyr::gather(key = Statistic, value=value, -var,-group)  %>%
            tidyr::unite(., Stat, Statistic, group) %>%
            dplyr::select(var, Stat, value)  %>%
            tidyr::spread(., key=Stat, value=value)
    }
    tab = tab %>%
        dplyr::mutate_if(is.numeric, round, digits=digits)
    return(tab)
}
## {{{ docs }}}
#' Summarise the categorical variables of the data set
#'
#' This function summarises all the categorical variables in the data set.
#'
#' @inheritParams summarise_alln 
#'
#' @return It returns a tibble data frame with summaries for all categorical variables. 
#'
#' @examples
#' data(starwars, package='dplyr')
#'
#' summarise_allc(starwars, group=NULL)
#' summarise_allc(starwars, group="gender")
#' summarise_allc(starwars, group=c("gender", "eye_color"))
#'
#' # or with pipe
#' # starwars %>% summarise_allc(., group=c("gender", "eye_color"))
#' 
#' @export
## }}}
summarise_allc <- function(df, group=NULL)
{
    options(warn=-1)
    on.exit(options(warn=0))
    
    flag=FALSE
    if (is.null(group)) {
        flag=TRUE
        df$grouping_null__ = 1
        group="grouping_null__"
    }
    tab = df %>%
        dplyr::select_if(function(col) class(col)[1] != 'list') %>% 
        dplyr::select(-dplyr::one_of(group))  %>% 
        dplyr::select_if(function(col) !is.numeric(col)) %>%
        dplyr::bind_cols(., df[,group]) %>%
        tidyr::gather(var, value,  -group) %>%
        dplyr::mutate(na = 1 - is.na(value)) %>%
        dplyr::group_by_("var", .dots=group) %>%
        dplyr::summarise(N      = sum(na),
                         NAs    = sum(is.na(value)),
                         Categories = length(unique(value[!is.na(value)])),
                         Frequency = paste0(stringr::str_pad(stringr::str_sub(names(table(value)),1,5), width=5, side='right'), " (", formatC(100*table(value)/sum(table(value)),format="f",digits=2), " %)",collapse=", ") ,
                         ## Table     = list(table(value, useNA="always") %>% stats::setNames(., nm=c(names(.)[1:(length(.)-1)], "NAs")) %>% rbind %>% tibble::as_data_frame(.)  %>% cbind(Variable=unique(var)) %>% dplyr::select(Variable, dplyr::everything())  ),
                         Table     = list(table(value, useNA="always") %>% stats::setNames(., nm=(names(.) %>% sapply(., toString)) ) %>% rbind %>% tibble::as_data_frame(.)  %>% cbind(Variable=unique(var)) %>% dplyr::select(Variable, dplyr::everything())  ),
                         Categories.Labels = paste0(sort(unique(value)), collapse=", ") ,
                         )   %>% 
        dplyr::ungroup(.) %>% 
        dplyr::arrange(var) 
    if (flag) {
        tab = tab %>% dplyr::select(-dplyr::one_of(group)) 
    }
    return(tab)
}
## {{{ docs }}}

#' Summarise categorical variables in bundles
#'
#' This function summarises all the categorical variables in the data set and create bundles based on the category labels
#'
#' @inheritParams summarise_allc 
#'
#' @return It returns a tibble data frame with summaries for all categorical variables. The summaries are aggregated into single tables based on the category labels. 
#'
#' @examples
#'
#' data(edar_survey)
#' ## help(edar_survey)
#' tab =  summarise_allcbundle(edar_survey)
#' tab
#' tab$Table[[4]]
#' tab$Tablep[[4]]
#' tab$Tablel[[4]]
#' tab = summarise_allcbundle( edar_survey, group="gender")
#' tab
#' tab$Table[[5]]
#' tab$Tablep[[5]]
#' tab$Tablel[[5]]
#'
#' # or with pipe
#' # edar_survey  %>% summarise_allcbundle(., group="gender")
#' 
#' @export

## }}}
summarise_allcbundle <- function(df, group=NULL)
{
    if( is.null(group) )    return( summarise_allcbundle_g0(df, group) )
    if( length(group) == 1) return( summarise_allcbundle_g1(df, group) )
    if( length(group) > 1 ) return( cat("\n\nCurrently, this function only supports aggragation by 1 group\n\n"))
}
summarise_allcbundle_g0 <- function(df, group=NULL)
{
    options(warn=-1)
    on.exit(options(warn=0))
    
    vars = df %>%  summarise_allc(., group=group) %>%
        dplyr::group_by(Categories.Labels) %>%
        dplyr::summarise(Variables = list(var),
                         N.Variables = purrr::map_int(.x=Variables, ~length(.x)))
    tab = df %>% summarise_allc(., group=group) %>%
        dplyr::group_by(Categories.Labels) %>%
        dplyr::select(Categories.Labels, Table)  %>%
        dplyr::summarise(Table   = list(do.call(rbind,Table))) %>%
        dplyr::full_join(., vars , by=c("Categories.Labels"))  %>%
        dplyr::select(N.Variables, Variables, dplyr::contains("Labels"), Table)  %>%
        dplyr::ungroup(.) %>%
        dplyr::mutate(Tablep = purrr::map(.x=Table, ~ .x %>% 
                                                 tidyr::gather(key = cat, value=N, -Variable)  %>%
                                                 dplyr::group_by(Variable) %>% 
                                                 dplyr::mutate(Frequency = round(100*N/sum(N),2)) %>%
                                                 dplyr::arrange(Variable)  %>%
                                                 dplyr::select(-N)  %>% 
                                                 tidyr::spread(., key=cat, value=Frequency) %>%
                                                 dplyr::ungroup(.)  %>%
                                                 as.data.frame %>%
                                                 dplyr::select(Variable,   'NA', dplyr::everything() ) %>% 
                                                 dplyr::select(c(1, 3:ncol(.)), 2 )
                                   ),
                      Tablel = purrr::map(.x=Table, ~ .x %>% 
                                                 tidyr::gather(key = cat, value=N, -Variable)  %>%
                                                 dplyr::group_by(Variable) %>% 
                                                 dplyr::mutate(Frequency = paste0(round(100*N/sum(N),2), " % (N=",  N,")") ) %>%
                                                 dplyr::arrange(Variable)  %>%
                                                 dplyr::select(-N)  %>% 
                                                 tidyr::spread(., key=cat, value=Frequency) %>%
                                                 dplyr::ungroup(.)  %>%
                                                 as.data.frame %>%
                                                 dplyr::select(Variable,   'NA', dplyr::everything() ) %>% 
                                                 dplyr::select(c(1, 3:ncol(.)), 2 )
                                     ),
                      ) 

    return(tab)
}
summarise_allcbundle_g1 <- function(df, group=NULL)
{
    options(warn=-1)
    on.exit(options(warn=0))
    
    vars = df %>%  summarise_allc(., group=group) %>%
        dplyr::rename(group=!!group) %>% 
        dplyr::group_by(Categories.Labels, group) %>%
        dplyr::summarise(Variables = list(var),
                         N.Variables = purrr::map_int(.x=Variables, ~length(.x)))
    tab = df %>% summarise_allc(., group=group) %>%
        dplyr::rename(group=!!group) %>% 
        dplyr::group_by(Categories.Labels, group) %>%
        dplyr::select(Categories.Labels, group, Table)  %>%
        dplyr::summarise(Table   = list(do.call(rbind,Table))) %>%
        dplyr::full_join(., vars , by=c("Categories.Labels", "group"))  %>%
        dplyr::select(group, N.Variables, Variables, dplyr::contains("Labels"), Table)  %>%
        dplyr::group_by(Categories.Labels) %>%
        dplyr::summarise(Table   = list(purrr::map2(.x=group,.y = Table, .f=function(.x,.y) cbind(group=.x,.y)) %>% do.call(rbind,. )%>% dplyr::arrange(Variable, group))  ) %>% 
        dplyr::ungroup(.) %>%
        dplyr::mutate(Tablep = purrr::map(.x=Table, ~ .x %>% 
                                                 tidyr::gather(key = cat, value=N, -Variable, -group)  %>%
                                                 dplyr::group_by(Variable, group) %>% 
                                                 dplyr::mutate(Frequency = round(100*N/sum(N),2)) %>%
                                                 dplyr::arrange(Variable)  %>%
                                                 dplyr::select(-N)  %>% 
                                                 tidyr::spread(., key=cat, value=Frequency) %>%
                                                 dplyr::ungroup(.)  %>%
                                                 as.data.frame %>%
                                                 dplyr::select(group, Variable,   'NA', dplyr::everything() ) %>% 
                                                 dplyr::select(c(1, 2, 4:ncol(.)), 3 ) %>%
                                                 dplyr::arrange(Variable, group) 
                                   ),
                      Tablel = purrr::map(.x=Table, ~ .x %>% 
                                                 tidyr::gather(key = cat, value=N, -Variable, -group)  %>%
                                                 dplyr::group_by(Variable, group) %>% 
                                                 dplyr::mutate(Frequency = paste0(N, " (", round(100*N/sum(N),2) ," %)") ) %>%
                                                 dplyr::arrange(Variable)  %>%
                                                 dplyr::select(-N)  %>% 
                                                 tidyr::spread(., key=cat, value=Frequency) %>%
                                                 dplyr::ungroup(.)  %>%
                                                 as.data.frame %>%
                                                 dplyr::select(group, Variable,   'NA', dplyr::everything() ) %>% 
                                                 dplyr::select(c(1, 2, 4:ncol(.)), 3 ) %>%
                                                 dplyr::arrange(Variable, group) 
                                   ),
                      )

    return(tab)
}

## }}}
## {{{ Describe data (tables) }}}

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
#'
#' ebalance(edar_survey , treatmentVar='treat')
#' 
#' # or with paper
#' # edar_survey  %>% ebalance(., treatmentVar='treat')
#' 
#' @export

## }}}
ebalance   <- function(data, treatmentVar, alpha=0.05, ttest=F, showMahalanobisDist=T,showpscore=T,showLinPscore=T, col.size=120)
{
    op.default <- options()
    on.exit(options(op.default), add=TRUE)

    options(width=col.size)
    data    = data %>% dplyr::rename(treatmentVar=!!treatmentVar) %>% dplyr::mutate(treatmentVar=as.numeric(treatmentVar)) %>% dplyr::select_if(is.numeric)
    treated = data %>% dplyr::filter(treatmentVar==1) %>%  dplyr::select(-treatmentVar)
    control = data %>% dplyr::filter(treatmentVar==0) %>%  dplyr::select(-treatmentVar)
    Tr      = data  %>% dplyr::select(treatmentVar)  %>% dplyr::pull(.)
    ## Std diff in mean
    ## ----------------
    muc = apply(control,2, mean,na.rm=T)
    mut = apply(treated,2,mean,na.rm=T)
    s2t = apply(treated,2,function(x) stats::var(x,na.rm=T))
    s2c = apply(control,2,function(x) stats::var(x,na.rm=T))
    delta = (mut - muc)/sqrt((s2t+s2c)/2)

    ## t-test difference in means
    ## --------------------------
    nt     <- nrow(treated)
    nc     <- nrow(control)
    t      <- (mut-muc) / sqrt( s2t/nt + s2c/nc)
    df     <- ( s2t/nt + s2c/nc)^2 / ( (s2t/nt)^2/(nt-1) +  (s2c/nc)^2/(nc-1) )
    pttest <- 2*(1-stats::pt(abs(t), df=df))

    ## log ratio of std dev
    ## --------------------
    gamma <- log(sqrt(s2t)/sqrt(s2c))

    ## proportion of tail of the other group
    ## -------------------------------------
    ltt <- apply(treated,2,stats::quantile,prob=c(alpha/2), na.rm=T)
    utt <- apply(treated,2,stats::quantile,prob=c(1-alpha/2), na.rm=T)

    ltc <- apply(control,2,stats::quantile,prob=c(alpha/2), na.rm=T)
    utc <- apply(control,2,stats::quantile,prob=c(1-alpha/2), na.rm=T)

    pit <- vector()
    pic <- vector()
    for (i in 1:ncol(treated)){
        x <- treated[,i] %>% dplyr::pull(.)
        pit[i] <- (sum(x<ltc[i],na.rm=T) + sum(x>utc[i],na.rm=T))/ length(x[!is.na(x)])
        x <- control[,i] %>% dplyr::pull(.)
        pic[i] <- (sum(x<ltt[i],na.rm=T) + sum(x>utt[i],na.rm=T))/ length(x[!is.na(x)])
    }

    ## prop score
    ## ----------
    dat          <- data %>% cbind(., Tr) 
    pscore       <- stats::glm(Tr ~ ., data=dat, family=stats::binomial(link="logit")) 
    phat         <- pscore$fitted.values
    phatlin      <- pscore$linear.predictors
    ## statistics for the raw pscore
    pscore_meant <- base::mean(phat[Tr==1], na.rm=T)
    pscore_vart  <- stats::var(phat[Tr==1], na.rm=T)
    pscore_meanc <- base::mean(phat[Tr==0], na.rm=T)
    pscore_varc  <- stats::var(phat[Tr==0], na.rm=T)
    pscore_dif   <- (pscore_meant - pscore_meanc)/sqrt((pscore_vart+pscore_varc)/2)
    pscore_lnSTD <- log(sqrt(pscore_vart)/sqrt(pscore_varc))
    pscore_pit <- stats::quantile(phat[Tr==1],prob=c(alpha/2,1-alpha/2), na.rm=T)
    pscore_pic <- stats::quantile(phat[Tr==0],prob=c(alpha/2,1-alpha/2), na.rm=T)
    pscore_pialphat <- (sum(phat[Tr==1]<pscore_pic[1],na.rm=T) +
                     sum(phat[Tr==1]>pscore_pic[2],na.rm=T))/
                     length(phat[Tr==1][!is.na(phat[Tr==1])])
    pscore_pialphac    <- (sum(phat[Tr==0]<pscore_pit[1],na.rm=T) +
                     sum(phat[Tr==0]>pscore_pit[2],na.rm=T))/
                     length(phat[Tr==1][!is.na(phat[Tr==1])])
    ## statistics for the linearized pscore
    lpscore_meant <- base::mean(phatlin[Tr==1], na.rm=T)
    lpscore_vart  <- stats::var(phatlin[Tr==1], na.rm=T)
    lpscore_meanc <- base::mean(phatlin[Tr==0], na.rm=T)
    lpscore_varc  <- stats::var(phatlin[Tr==0], na.rm=T)
    lpscore_dif   <- (lpscore_meant - lpscore_meanc)/
      sqrt((lpscore_vart+lpscore_varc)/2)
    lpscore_lnSTD <- log(sqrt(lpscore_vart)/sqrt(lpscore_varc))
    lpscore_pit <- stats::quantile(phatlin[Tr==1],prob=c(alpha/2,1-alpha/2), na.rm=T)
    lpscore_pic <- stats::quantile(phatlin[Tr==0],prob=c(alpha/2,1-alpha/2), na.rm=T)
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
    sigmat <- stats::var(treated, na.rm=T)
    sigmac <- stats::var(control, na.rm=T)
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

    results = results %>% data.frame(Variable=rownames(results))  %>%
    tibble::as_data_frame(.)  %>%
        dplyr::select(Variable, dplyr::everything())
    results = list(balance=results, pscore=phat, lpscore=phatlin)
    class(results) = "edar_balance"
    return(results)
}

## {{{ docs }}}

#' Print
#'
#' Generic method to print the output of a \code{ebalance}
#'
#' @param x an output of the function \code{ebalance}
#' @param digits integer, number of digits to display 
#' @param ... ingored
#'
#'
#' @export
## }}}
print.edar_balance <- function(x, digits=4, ...)
{
    x$balance = x$balance %>% dplyr::mutate_if(is.numeric, round, digits=digits)
    return(x$balance)
}

## Note: funcions summarise_alln and summarise_allcbundle in this package are better option to used instead of edescribe edar_bundle_cat 
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
#' edescribe(edar_survey)
#' 
#' tb = edescribe(edar_survey)
#' tb$Cat
#' tb$Num
#'
#' # or with pipe
#' # edar_survey %>% edescribe()
#' @export

## }}}
edescribe  <- function(data, weight=NULL, minMax=T, digits=2, maxVars=NULL, col.size=120)
{

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
            q      = apply(data[,!idxChar],2,stats::quantile,probs=c(.05,.95),na.rm=T) 
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
        numericSummary = tibble::as_data_frame(numericSummary) %>%
            dplyr::arrange(name)
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
        tables_summary = lapply(tables, function(x) paste0(names(x),' (',round(100*x/n,2),"%)",  collapse='; ') )
        tables_summary = tibble::data_frame(name = names(tables),
                                            N    = nrow(data),
                                            levels = unlist(lapply(tables, length)),
                                            missing = unlist(lapply(tables, function(x) x[is.na(names(x))])),
                                            class =unlist(lapply(subset(data, select=names(tables)), class)),
                                            freq_tables = tables,
                                            distribution = c(do.call(rbind, tables_summary))
                                            ) %>%
            dplyr::arrange(name)
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
    op.default <- options()
    on.exit(options(op.default), add=TRUE)
    options(tibble.print_max = max(nrow(results$Num), nrow(results$Cat)))

    return(results)

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
#' @param print.summary boolean, if \code{TRUE} the function will print a summary
#' @param width.size numeric, size of the columns in the screen to print the results
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
#' edar_bundle_cat(edar_survey)
#' tb = edar_bundle_cat(edar_survey )
#' tb
#' tb$var.idx
#' tb$table.n
#' tb$table.p
#' tb$table.pn
#'
#' # with pipe:
#' # edar_survey %>% edar_bundle_cat()
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
## {{{ Describe data (plots) }}}

## {{{ docs }}}

#' Descriptive Plots
#'
#' This function generate plots with all the variables in the data frame individually
#'
#' @param df a data frame
#' @param group a string with the name of the varible(s) to group the plots
#' @param weight a string with the name of the varible containing the weights. They are used to compute the summary stattistics
#' @param mean boolean, if \code{TRUE}, displays a line with the averave value
#' @param median boolean, if \code{TRUE}, displays a line with the median value
#' @param conf.int boolean, if \code{TRUE}, displays lines with the 95\% confidence interval (it is meaningful for variables that normally distributed)
#' @param quantile boolean, if \code{TRUE}, displays lines with the 25\% and 75\% quantiles
#' @param n.plots integer, the number of plots to display in the grid
#' @param common.legend boolean, if \code{TRUE} the plots uses a single legend
#' @param hist boolean, if \code{TRUE}, histograms are displayed instead of densities for the numerical variables
#' @param legend.position a string (\code{top}, \code{bottom}, \code{left} (Default)), \code{right} 
#'
#' @export

## }}}
gge_describe <- function(df, group=NULL, weight=NULL, mean=TRUE, median=FALSE, conf.int=TRUE, quantile=FALSE, n.plots=16, common.legend=TRUE, hist=FALSE, legend.position='top')
{
    if (length(group)>1) {
        stop("\n\nCurrently, this function supports only 1 group")
    }
    ## Debug/Monitoring message --------------------------
    msg <- paste0('\n','Generating the plots ...',  '\n'); cat(msg)
    ## ---------------------------------------------------
    options(warn=-1)
    on.exit(options(warn=0))

    g=list()
    j = 1
    idx.cat=NA
    idx.num=NA
    vars = names(df)
    for (i in 1:ncol(df))
    {
        var = vars[i]
        if (!var %in% c(group, weight)) {
            numeric = df[,var] %>% dplyr::pull(.)  %>% is.numeric
            if (numeric) {
                if (hist) {
                    g[[j]] = gge_histogram(df[,c(var, group, weight)], group, weight)
                    idx.num = c(idx.num, j)
                }else{
                    g[[j]] = gge_density(df[,c(var, group, weight)], group, weight)
                    idx.num = c(idx.num, j)
                }
            }else{
                g[[j]] = gge_barplot_one(df[,c(var, group)], variable=var, group=group)
                idx.cat = c(idx.cat, j)
            }
        }
        j=j+1
    }
    idx.num=idx.num[-1]
    idx.cat=idx.cat[-1]
    remaining = length(g)
    ## display continuous covariates first
    chunks = list()
    i=1
    idx.plots = 1:length(g)
    idx.plots =idx.plots[c(idx.num,idx.cat)]
    while(length(idx.plots)>0){
        n = min(n.plots, length(idx.plots))
        chunks[[i]] = idx.plots[1:n]
        idx.plots = idx.plots[-c(1:n)]
        i=i+1
    }
    for (chunk in chunks)
    {
        print(ggpubr::ggarrange(plotlist=g[chunk],  common.legend = common.legend, legend="top"))

        remaining = remaining - length(chunk)
        if(remaining>0){
            cat(paste0("\n",remaining, " plots remaining to display.") )
            readline(prompt="\nPress any key to continue ...")
        }
    }
    invisible()
}
## {{{ docs }}}

#' Plot densities by group and facet, with Kolmogorov-Smirnov statistics
#'
#' @param df a data frame
#' @param value a string with the name of the numerical value to plot the density
#' @param group a string with the name of the variable to group the densities
#' @param facet a string with the name of the variable to create the grid of plots
#' @param scale see \code{\link[ggplot2]{facet_wrap}}
#'
#' @details All the variables in the data frame will be plotted
#' @export

## }}}
gge_fdensity <- function(df, value, group, facet, scale='fixed')
{
    if (length(group)>1) {
        stop("\n\nCurrently, this function supports only 1 group")
    }
    ## Debug/Monitoring message --------------------------
    ## msg <- paste0('\n','Generating the plots ...',  '\n'); cat(msg)
    ## ---------------------------------------------------
    options(warn=-1)
    on.exit(options(warn=0))

    tab = df %>%
        dplyr::rename_(group=group, value=value, facet=facet) %>% 
        dplyr::mutate(group=factor(group), facet=factor(facet))
    tab = tab %>% 
        dplyr::group_by(facet) %>%
        dplyr::summarise(d1 = list(base::split(value, group)[[1]]),
                         d2 = list(base::split(value, group)[[2]])) %>%
        dplyr::mutate(KS.test = purrr::map2_dbl(d1,d2, ~ ifelse( all(is.na(.x)) | all(is.na(.y)), 1,  ks.test(.x,.y)$p.value)),
                      KS.text = paste0("K-S test : ", round(KS.test,4)) ) %>%
        dplyr::select(facet, dplyr::contains("KS")) %>%
        dplyr::full_join(., tab  , by=c("facet"))
    g = tab  %>% 
        ggplot2::ggplot(.) +
        ggplot2::geom_density(ggplot2::aes(x=value, fill=group), alpha=.6) +
        ggplot2::facet_wrap( ~ facet,  scales=scale,labeller=ggplot2::label_parsed) +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        viridis::scale_fill_viridis(option="A", discrete=TRUE, alpha=1, name="", begin=.066, end=.77) +
        viridis::scale_colour_viridis(option="A", discrete=TRUE, alpha=1, name="", begin=.066, end=.77) +
        ggplot2::geom_text(data=tab %>% dplyr::select(facet, KS.text)  %>% dplyr::filter(!duplicated(.)), ggplot2::aes(x=-Inf, y=Inf, label=KS.text), vjust=2, hjust=-.5)
    g = g +
        ggplot2::theme_bw() +
        ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"),
                           strip.text.x = ggplot2::element_text(size=12, face='bold'),
                           strip.text.y = ggplot2::element_text(size=12, face="bold")) 
    g = g + ggplot2::theme(legend.position = "top") 

    return(g)
}
## {{{ docs }}}
#' Plot densities
#'
#' Plot densities of all numerical variables in the data frame
#'
#' @inheritParams gge_describe
#' 
#' @export
## }}}
gge_density <- function(df, group=NULL, weight=NULL, mean=TRUE, median=FALSE, conf.int=TRUE, quantile=FALSE)
{
    if (length(group)>1) {
        stop("\n\nCurrently, this function supports only 1 group")
    }
    ## Debug/Monitoring message --------------------------
    ## msg <- paste0('\n','Generating the plots ...',  '\n'); cat(msg)
    ## ---------------------------------------------------
    options(warn=-1)
    on.exit(options(warn=0))
    
    flag=FALSE
    ifelse(is.null(weight), df$weight<-1, df$weight <- df %>% dplyr::select_(weight) %>% dplyr::pull(.)) 
    if (is.null(group)) {
        flag=TRUE
        df$grouping_null__ = 1
        group="grouping_null__"
    }

    ## preparing the data to plot
    df[,group] = df[,group] %>% 
        dplyr::mutate_all(as.factor)
    tab = df %>% 
        summarise_alln(., group=group, weight="weight", spread=F)
    aes.statistics = c("Mean","Median",  "Quantile (2.5%, 97.5%)",  "Quantile (25%, 75%)")
    aes.var.names = c("Average", "Med", "CI.l", "CI.u", "q25", "q75")
    df$Average = aes.statistics [1]
    df$Med     = aes.statistics [2]
    df$CI.l    = aes.statistics [3]
    df$CI.u    =aes.statistics [3]
    df$q25     = aes.statistics [4]
    df$q75     = aes.statistics [4]

    tab = df %>%
        dplyr::select(-dplyr::one_of(group))  %>% 
        dplyr::select_if(is.numeric) %>%
        dplyr::bind_cols(., df[,c(group, aes.var.names)])  %>%
        tidyr::gather(var, value,  -group, -weight,  -aes.var.names)  %>%
        dplyr::full_join(., tab , by=c("var", group))  %>%
        dplyr::mutate(value=value*weight) 

    
    if(!is.null(group)){
        g = tab %>% 
            ggplot2::ggplot(.) + 
            ggplot2::geom_density(ggplot2::aes_string(x="value",  fill=group), adjust=1, alpha=.5) +
            ggplot2::facet_wrap( ~ var , scales='free',labeller=ggplot2::label_parsed)  +
            ggplot2::scale_x_continuous(expand = c(0, 0)) +
            ggplot2::scale_y_continuous(expand = c(0, 0)) +
            viridis::scale_fill_viridis(option="A", discrete=TRUE, alpha=1, name="", begin=.066, end=.77) +
            viridis::scale_colour_viridis(option="A", discrete=TRUE, alpha=1, name="", begin=.066, end=.77) +
            ggplot2::guides(linetype=ggplot2::guide_legend("")) 
    }else{
        g = tab %>% 
            ggplot2::ggplot(.) + 
            ggplot2::geom_density(ggplot2::aes(value), adjust=1, alpha=.5) +
            ggplot2::facet_wrap( ~ var , scales='free',labeller=ggplot2::label_parsed)  +
            ggplot2::scale_x_continuous(expand = c(0, 0)) +
            ggplot2::scale_y_continuous(expand = c(0, 0)) +
            ggplot2::guides(linetype=ggplot2::guide_legend("")) 
    }

    if(mean)
        g = g + ggplot2::geom_vline(ggplot2::aes_string(xintercept="Mean", col=group, linetype="Average")) 

    if(median) 
        g = g + ggplot2::geom_vline(ggplot2::aes_string(xintercept="Median", col=group, linetype="Med")) 

    if(conf.int) 
        g = g + ggplot2::geom_vline(ggplot2::aes_string(xintercept="q.025", col=group, linetype="CI.l")) +
            ggplot2::geom_vline(ggplot2::aes_string(xintercept="q.975", col=group, linetype="CI.u")) 

    if(quantile) 
        g = g + ggplot2::geom_vline(ggplot2::aes_string(xintercept="q.25", col=group, linetype="q25")) +
            ggplot2::geom_vline(ggplot2::aes_string(xintercept="q.75", col=group, linetype="q75")) 

    g = g + ggplot2::theme_bw() + ggplot2::theme(legend.position = "top") 

    ## KS test if two groups
    if (length(unique(df[,group] %>% dplyr::pull(.)))==2) {
        gr = df[,group]
        ks = tab %>%
            dplyr::group_by(var) %>%
            dplyr::summarise(d1 = list(base::split(value, gr)[[1]]),
                             d2 = list(base::split(value, gr)[[2]])) %>%
            dplyr::mutate(KS.test = purrr::map2_dbl(d1,d2, ~ks.test(.x,.y)$p.value),
                          KS.text = paste0("K-S test : ", round(KS.test,4)) ) %>%
            dplyr::select(var, dplyr::contains("KS")) 
        tab = tab %>%
            dplyr::full_join(., ks , by=c("var")) 
        g = g + ggplot2::geom_text(data=tab %>% dplyr::select(var, KS.text)  %>% dplyr::filter(!duplicated(.)),
                          ggplot2::aes(x=-Inf, y=Inf, label=KS.text), vjust=2, hjust=-.5)
    }
    g= g+ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"),
               strip.text.x = ggplot2::element_text(size=12, face='bold'),
               strip.text.y = ggplot2::element_text(size=12, face="bold")) 
    if (group[1] == "grouping_null__") {
        g = g + ggplot2::guides(fill=FALSE, colour=F)
    }

    return(g)
}
## {{{ docs }}}

#' Plot histograms
#'
#' @inheritParams gge_describe
#'
#' @details All the variables in the data frame will be plotted
#' @export

## }}}
gge_histogram <- function(df, group=NULL, weight=NULL, mean=T, median=F, conf.int=T, quantile=F, legend.position='top')
{
    if (length(group)>1) {
        stop("\n\nCurrently, this function supports only 1 group")
    }
    ## Debug/Monitoring message --------------------------
    ## msg <- paste0('\n','Generating the plots ...',  '\n'); cat(msg)
    ## ---------------------------------------------------
    options(warn=-1)
    on.exit(options(warn=0))
    
    flag=FALSE
    ifelse(is.null(weight), df$weight<-1, df$weight <- df %>% dplyr::select_(weight) %>% dplyr::pull(.)) 
    if (is.null(group)) {
        flag=TRUE
        df$grouping_null__ = 1
        group="grouping_null__"
    }

    ## preparing the data to plot
    df[,group] = df[,group] %>% dplyr::mutate_all(as.factor)
    tab = df %>% summarise_alln(., group=group, weight="weight", spread=F)
    aes.statistics = c("Mean","Median",  "Quantile (2.5%, 97.5%)",  "Quantile (25%, 75%)")
    aes.var.names = c("Average", "Med", "CI.l", "CI.u", "q25", "q75")
    df$Average = aes.statistics [1]
    df$Med     = aes.statistics [2]
    df$CI.l    = aes.statistics [3]
    df$CI.u    =aes.statistics [3]
    df$q25     = aes.statistics [4]
    df$q75     = aes.statistics [4]

    tab = df %>%
        dplyr::select(-dplyr::one_of(group))  %>% 
        dplyr::select_if(is.numeric) %>%
        dplyr::bind_cols(., df[,c(group, aes.var.names)])  %>%
        tidyr::gather(var, value,  -group, -weight,  -aes.var.names)  %>%
        dplyr::full_join(., tab , by=c("var", group))  %>%
        dplyr::mutate(value=value*weight) 

    
    if(!is.null(group)){
        g = tab %>% 
            ggplot2::ggplot(.) + 
            ggplot2::geom_histogram(ggplot2::aes_string(x="value",  fill=group), alpha=.5) +
            ggplot2::facet_wrap( ~ var , scales='free',labeller=ggplot2::label_parsed)  +
            ggplot2::scale_x_continuous(expand = c(0, 0)) +
            ggplot2::scale_y_continuous(expand = c(0, 0)) +
            viridis::scale_fill_viridis(option="A", discrete=TRUE, alpha=1, name="", begin=.066, end=.77) +
            viridis::scale_colour_viridis(option="A", discrete=TRUE, alpha=1, name="", begin=.066, end=.77) +
            ggplot2::guides(linetype=ggplot2::guide_legend("")) +
            ggplot2::theme(legend.position = legend.position) 
    }else{
        g = tab %>% 
            ggplot2::ggplot(.) + 
            ggplot2::geom_histogram(ggplot2::aes(value), alpha=.5) +
            ggplot2::facet_wrap( ~ var , scales='free',labeller=ggplot2::label_parsed)  +
            ggplot2::scale_x_continuous(expand = c(0, 0)) +
            ggplot2::scale_y_continuous(expand = c(0, 0)) +
            ggplot2::guides(linetype=ggplot2::guide_legend("")) 
    }

    if(mean)
        g = g + ggplot2::geom_vline(ggplot2::aes_string(xintercept="Mean", col=group, linetype="Average")) 

    if(median) 
        g = g + ggplot2::geom_vline(ggplot2::aes_string(xintercept="Median", col=group, linetype="Med")) 

    if(conf.int) 
        g = g + ggplot2::geom_vline(ggplot2::aes_string(xintercept="q.025", col=group, linetype="CI.l")) +
            ggplot2::geom_vline(ggplot2::aes_string(xintercept="q.975", col=group, linetype="CI.u")) 

    if(quantile) 
        g = g + ggplot2::geom_vline(ggplot2::aes_string(xintercept="q.25", col=group, linetype="q25")) +
            ggplot2::geom_vline(ggplot2::aes_string(xintercept="q.75", col=group, linetype="q75")) 

    g = g + ggplot2::theme(legend.position = "top") +
        ggplot2::theme_bw() +
        ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"),
              strip.text.x = ggplot2::element_text(size=12, face='bold'),
              strip.text.y = ggplot2::element_text(size=12, face="bold")) 
    if (group[1] == "grouping_null__") {
        g = g + ggplot2::guides(fill=FALSE, colour=F)
    }
    return(g)
}
## {{{ docs }}}

#' Plot categorical Variables using barplot
#'
#' @inheritParams gge_describe
#' @param ncol integer with the number of columns in the grid to plot. If \code{NULL} (default), plots will be places in a squared grid
#' @param nrow inteter with the number of rows in the grid to plot. If \code{NULL} (default), plots will be places in a squared grid
#'
#' @export

## }}}
gge_barplot <- function(df, group=NULL, ncol=NULL, nrow=NULL)
{
    options(warn=-1)
    on.exit(options(warn=0))

    if (length(group)>1) {
        stop("\n\nCurrently, this function supports only 1 group")
    }
    ## Debug/Monitoring message --------------------------
    ## msg <- paste0('\n','Generating the plots ...',  '\n'); cat(msg)
    ## ---------------------------------------------------
    vars = names(df) 
    g = list()
    j = 1
    for (i in 1:length(vars))
    {
        var = vars[i]
        if (!is.numeric(df[,var] %>% dplyr::pull(.))) {
            if (!is.null(group)) {
                if (var != group) {
                    g[[j]] = gge_barplot_one(df, var, group) 
                    j = j+1
                }
            }else{
                g[[j]] = gge_barplot_one(df, var, group) 
                j = j+1
            }
        }
    }
    print(ggpubr::ggarrange(plotlist=g, common.legend=TRUE, ncol=ncol, nrow=nrow))
    invisible()
}
## {{{ docs }}}

#' Descriptive Plots
#'
#' This function generate plots with all the variables in the data frame individually. It is a faster version of gge_describe
#'
#' @param data a data frame
#' @param qline boolean, if \code{TRUE} the plots will display lines in the quantiles
#' @param quantiles vector with values between 0 and 1 indicating the quantiles to display
#' 
#' @details All the variables in the data frame will be plotted
#' @export

## }}}
plot_edescribe <- function(data, qline=T, quantiles=c(.025,.975))
{
    op=graphics::par(no.readonly=TRUE)

    ## Debug/Monitoring message --------------------------
    msg <- paste0('\n','Generating the plots ...',  '\n'); cat(msg)
    ## ---------------------------------------------------

    dfc = data %>% 
        dplyr::select_if(function(col) !is.numeric(col)) 
    dfn = data %>%
        dplyr::select_if(is.numeric)
    ## check if there is any numerical variable for the summary
    maxVars <- 16
    if(ncol(data)>16)
        stop(cat("\n\n Max variables permited by default:",maxVars))

    m <- ceiling(sqrt(ncol(data)))
    if(m*(m-1)>=ncol(data))
        graphics::par(mfrow=c(m-1,m))
    else
        graphics::par(mfrow=c(m,m))

    if (nrow(dfn)>0){
        X <- dfn
        for (i in 1:ncol(X)){
            x <- X[,i] %>% dplyr::pull(.)
            x = x[!is.na(x)]
            edar_plotdensity(x,main=names(dfn)[i], qline)
            q <- stats::quantile(x,probs=quantiles)
            graphics::abline(v=q, col='red', lty=2)
            graphics::abline(v=mean(x, na.rm=T), col='red', lty=1)
            graphics::legend('topleft', lty=c(1,2), legend=c("Mean", paste0("Quantiles (", paste0(paste0(100*quantiles, "%"), collapse=','),  ")") ), col=c('red', 'red'), bty='n')
        }
    }
    if (ncol(dfc)>0 ){
        for (i in 1:ncol(dfc)){
            x <- dfc[,i] %>% dplyr::pull(.)
            edar_barPlot(as.factor(x), title=names(dfc)[i])
        }
    }
    graphics::par(op)
}


## {{{ docs }}}
#' Barplot
#'
#' This function generate a barplot
#'
#' @param data a data frame
#' @param variable string with the name of the categorical variable that is going to be used to create the plot
#' @param title either \code{NULL} or a string with the title of the plot
#' @param subtitle either \code{NULL} or a string with the subtitle of the plot
#' @inheritParams gge_describe
#' 
#' @export
## }}}
gge_barplot2 <- function(data, variable, group=NULL, title = NULL, subtitle=NULL, legend.position='top')
{
    options(warn=-1)
    on.exit(options(warn=0))
    
    cat = variable
    if (!is.null(group)) {
        g =  data %>% 
            dplyr::select(cat, group) %>%
            dplyr::rename(cat = !!cat, group=!!group) %>% 
            dplyr::mutate(group=factor(group))  %>%
            dplyr::group_by(cat) %>%
            dplyr::mutate(cat.label = paste0(cat, "\n(n=",n(),")" ))  %>% 
            dplyr::ungroup(.)  %>% 
            dplyr::mutate(cat = cat.label)  %>% 
            dplyr::group_by(cat, group) %>%
            dplyr::summarise(N=n())  %>% 
            dplyr::ungroup(.)  %>% 
            dplyr::mutate(Percentage= round(100*N/sum(N, na.rm=T),2) )  %>%
            dplyr::mutate(label = paste0(Percentage, " % (n=",N,")") )  %>%
            dplyr::ungroup(.)  %>%
            ggplot2::ggplot(.) +
            ggplot2::geom_col(ggplot2::aes(y= Percentage, x = cat, fill=group, group=group), stat='identity', position = 'dodge', alpha=.6) + 
            ggplot2::geom_text(ggplot2::aes(y= Percentage, x = cat, label=label, group=group), hjust=-.1, size=3, position = ggplot2::position_dodge(1))+
            ggplot2::coord_flip() +
            ggplot2::xlab("")+
            ggplot2::ggtitle(cat)+
            viridis::scale_fill_viridis(option="A", discrete=TRUE, alpha=1, name="", begin=.066, end=.77) +
            ggplot2::theme(legend.position = "bottom") 
    }else{
        g = data %>% 
            dplyr::select(cat) %>%
            dplyr::rename(cat = !!cat) %>% 
            dplyr::group_by(cat) %>%
            dplyr::mutate(ylabel = paste0(unique(cat), "\n(N=",n(),")") )  %>% 
            dplyr::group_by(ylabel) %>% 
            dplyr::summarise(n=n())  %>% dplyr::mutate("Percentage"=round(100*n/sum(n),2)) %>% 
            dplyr::mutate(label = paste0(Percentage, "%") ,
                          ylabel=factor(ylabel, levels=as.character(ylabel[order(Percentage)]))) %>% 
            ggplot2::ggplot(.) +
            ggplot2::geom_bar(ggplot2::aes(y= Percentage, x = ylabel), stat='identity',position = 'dodge', alpha=.5) + 
            ggplot2::geom_text(ggplot2::aes(y= Percentage, x = ylabel, label=label), hjust=-.1, size=4)+
            ggplot2::coord_flip() +
            ggplot2::xlab("")+
            ggplot2::ggtitle(cat)
    }
    if (!is.null(title) | !is.null(subtitle)) {
        if (is.null(title)) title=""
        g = g + ggplot2::ggtitle(title, subtitle=subtitle)
    }
    g = g + ggplot2::theme_bw()
    if (!is.null(group)) {
        g = g + ggplot2::theme(legend.position = legend.position) 
    }
    g = g + ggplot2::scale_y_continuous(expand = c(0, 0), limits=c(0,105), breaks=c(0,25,50,75,100), labels= )
    return(g)
}

## ---------
## anxillary
## ---------
gge_barplot_one <- function(df, variable, group=NULL)
{
    options(warn=-1)
    on.exit(options(warn=0))

    cat = variable
    if (!is.null(group)) {
        g =  df %>% 
            dplyr::select(cat, group) %>%
            dplyr::rename(cat = !!cat, group=!!group) %>% 
            dplyr::mutate(group=factor(group))  %>%
            dplyr::group_by(cat, group) %>%
            dplyr::mutate(ylabel = paste0(unique(cat), "\n(",group,"; N=",n(),")") )  %>%
            dplyr::group_by(ylabel, group) %>% 
            dplyr::summarise(n=n())  %>% 
            dplyr::ungroup(.) %>%
            dplyr::group_by(group) %>% 
            dplyr::mutate("Percentage"=round(100*n/sum(n),2))  %>%
            dplyr::mutate(label = paste0(Percentage, "%") ,
                          ylabel=factor(ylabel, levels=as.character(ylabel[order(Percentage)]))) %>% 
            ggplot2::ggplot(.) +
            ggplot2::geom_bar(ggplot2::aes(y= Percentage, x = ylabel, fill=group), stat='identity',position = 'dodge', alpha=.5) + 
            ggplot2::geom_text(ggplot2::aes(y= Percentage, x = ylabel, label=label), hjust=-.1, size=4)+
            ggplot2::coord_flip() +
            ggplot2::ylim(0,100) +
            ggplot2::xlab("")+
            ggplot2::ggtitle(cat)+
            viridis::scale_fill_viridis(option="A", discrete=TRUE, alpha=1, name="", begin=.066, end=.77) +
            ggplot2::theme(legend.position = "bottom") 
    }else{
        g = df %>% 
            dplyr::select(cat) %>%
            dplyr::rename(cat = !!cat) %>% 
            dplyr::group_by(cat) %>%
            dplyr::mutate(ylabel = paste0(unique(cat), "\n(N=",n(),")") )  %>% 
            dplyr::group_by(ylabel) %>% 
            dplyr::summarise(n=n())  %>% dplyr::mutate("Percentage"=round(100*n/sum(n),2)) %>% 
            dplyr::mutate(label = paste0(Percentage, "%") ,
                          ylabel=factor(ylabel, levels=as.character(ylabel[order(Percentage)]))) %>% 
            ggplot2::ggplot(.) +
            ggplot2::geom_bar(ggplot2::aes(y= Percentage, x = ylabel), stat='identity',position = 'dodge', alpha=.5) + 
            ggplot2::geom_text(ggplot2::aes(y= Percentage, x = ylabel, label=label), hjust=-.1, size=4)+
            ggplot2::coord_flip() +
            ggplot2::ylim(0,100) +
            ggplot2::xlab("")+
            ggplot2::ggtitle(cat)
    }
    g = g + ggplot2::theme_bw()
    return(g)
}
edar_plotdensity  <- function(x, ub=stats::quantile(x,.975), lb=stats::quantile(x,.025), shaded.area=F, shaded.area.col='grey90', lty=1, bty='n', add=F, grid=T, main="")
{
    data.rug = sample(x, size=min(1000, length(x)))
    if (add) {
        graphics::lines(stats::density(x, adjust=1), lty=lty,  cex.axis=.9)
    }else{
        graphics::plot(stats::density(x, adjust=1), lty=lty,  cex.axis=.9, main=main)
    }
    graphics::rug(x=data.rug)
    if(shaded.area){
    ## shadded area
        ## x.new <- density(x, adjust=1)$x
        x.new<- stats::density(x, adjust=1)$x
        y <- stats::density(x, adjust=1)$y
        x.coord <- c(lb, x.new[lb <= x.new & x.new <= ub], ub)
        y.coord <- c(0,  y[lb <= x.new & x.new <= ub], 0)
        graphics::polygon(x.coord,y.coord,col=shaded.area.col, lty=lty, border= NA)
        length(x.coord)
        length(y.coord)
    }
    if(grid) graphics::grid()
}
edar_barPlot       <- function(y, title='', show.group.name=T, show.group.count=T, col='lightgrey')
{
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
    ypos <- graphics::barplot(tableProp, horiz=T, xlim=c(0,1.1), main=title,plot=F)
    graphics::par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,5,3,0))
    graphics::barplot(tableProp, horiz=T, xlim=c(0,1.3), col=col, border=NA, xaxt='n', cex.axis=1.2, cex.names=1.2)
    title(main=title, cex.main=1.1)
    graphics::axis(1, at=seq(0,1,by=.2), labels=seq(0,1,by=.2))
    graphics::mtext(text=paste('N=',n-NAs,', NA=',NAs,sep=''), cex=.7)
    if(show.group.name) graphics::text(x=tableProp,y=ypos, labels=tableProp,cex=.9,pos=4)
}

## }}}
## {{{ Bivariate    Analysis (plots) }}}

## continuous variables
## --------------------
## {{{ docs }}}

#' Heat plot
#'
#'
#' @param x,y numeric vector with values to plot
#' @param xlab string to be displayed in the x-axis
#' @param ylab string to be displayed in the y-axis
#' @param title string with the title of the plot
#' @param axis boolean, if \code{TRUE} the axis are displayed
#' @param text text annotation 
#'
#'
#' @export

## }}}
plot_heat <- function(x='', y='', xlab='x', ylab='y', title='', text='', axis=TRUE){
    colors_palette <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11,'Spectral')))
    r <- colors_palette(32)
    h1 <- graphics::hist(x, breaks=30, plot=F)
    h2 <- graphics::hist(y, breaks=30, plot=F)
    top <- max(h1$counts, h2$counts)
    k <- MASS::kde2d(x, y, n=200)
    ## margins
    if(axis){
        raster::image(k, col=r, xlab=xlab, ylab=ylab, bg='black')     
    }else{
        raster::image(k, col=r, xlab='', ylab='', bg='black', xaxt='n', yaxt='n')
    }
    graphics::title(title)
}
## {{{ docs }}}

#' Heat plot with histogram
#'
#' @inheritParams plot_heat
#' @param ylim two dimensional numeric vector with limits of the y-axis 
#' @param xlim two dimensional numeric vector with limits of the x-axis
#' @param breaks integer, for the marginal histograms
#' @param subtitle a string with subtitles of the plot 
#'
#' @export

## }}}
plot_heathist <- function( x='', y='', title='', subtitle='', xlab='x', ylab='y',  breaks=50, 
                         xlim=c(min(x), max(x)), ylim=c(min(y), max(y)))
{
    color_palette<- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(11,'Spectral')))
    r <- color_palette(32)
    h1 <- graphics::hist(x, breaks=seq(xlim[1],xlim[2],length=breaks), plot=F)
    h2 <- graphics::hist(y, breaks=seq(ylim[1],ylim[2],length=breaks), plot=F)
    top <- max(h1$counts, h2$counts)
    k <- MASS::kde2d(x, y, n=200)
                                        # margins
    oldpar <- graphics::par(no.readonly = T)
    graphics::layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
    graphics::par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(5,5,.1,.1))
    graphics::image(k, col=r, xlab=xlab, ylab=ylab,  useRaster=T, xlim=xlim, ylim=ylim) #plot the image
    ##points(df$x,df$y, cex=.3)
    graphics::par(mar=c(0,4.5,3,0))
    graphics::barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='lightgrey', border='white')
    title(title)
    graphics::mtext(text=subtitle, cex=.8)
    graphics::par(mar=c(5,0,0.1,4))
    graphics::barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='lightgrey', horiz=T, border='white')
    graphics::par(oldpar)
}
## {{{ docs }}}

#' Contour plot
#'
#' @param df a data frame
#' @param x string with the name of the variable to plot in the x-axis
#' @param y string with the name of the variable to plot in the y-axis
#' @param points boolean, if \code{TRUE} displays the points
#' @param contour.breaks integer, number of breaks to use in the contour plot
#' @param palette a string with the name of a color brewer continuous palette
#' @param hist.fill a string with the colour to fill the histogram bars
#' @param hist.col a string with the color of the border of the histogram bars
#' @param group a string with the categories to group the points
#' @param xlim two dimensional numeric vectors with the limits of the x-axis 
#' @param xlab a string to be displayed in the x-axis
#' @param ylab a string to be displayed in the y-axis
#' @param direction either 1 or -1 indicating the direction of the color sequence.
#'
#' @export

## }}}
gge_contour <- function(df, x, y, points=FALSE, group=NULL, palette='Blues', hist.fill="lightsteelblue2", hist.col='white', xlim=NULL, xlab=NULL, ylab=NULL, contour.breaks=200, direction=1)
{
    if (is.null(group))  g= gge_contour_g0(df, x, y, points, group, palette, hist.fill, hist.col, xlim, xlab, ylab,
                                           contour.breaks=contour.breaks, direction=direction) 
    if (!is.null(group)) g= gge_contour_g1(df, x, y, points, group, palette, hist.fill, hist.col, xlim, xlab, ylab,
                                           contour.breaks=contour.breaks, direction=direction) 
    if (!is.null(xlim)) {
        g = g + xlim(xlim)
    }
    if (!is.null(xlab)) {
        g = g + xlab(xlab)
    }
    if (!is.null(ylab)) {
        g = g + ylab(ylab)
    }
    g = ggExtra::ggMarginal(g, type = "histogram",  colour=hist.col, fill = hist.fill)
    return(g)
}
gge_contour_g0 <- function(df, x, y, points, group=NULL, palette='Blues', hist.fill="lightsteelblue2", hist.col='white', xlim=NULL, xlab=NULL, ylab=NULL, contour.breaks = 200, direction=1)
{
    g = df %>%
        dplyr::rename(x=!!x, y=!!y)  %>%
        ggplot2::ggplot(., ggplot2::aes(x = x , y = y)) +
        ggplot2::stat_density_2d(ggplot2::aes(fill = ..level..), n=contour.breaks, size=0, contour=T, geom='polygon', alpha=.4) +
        ggplot2::scale_fill_distiller(palette=palette, direction=direction) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom")+
        ggplot2::guides(fill=FALSE)
    if (points) g = g + ggplot2::geom_point( colour="#00000024", size=2) 
    return(g)
}
gge_contour_g1 <- function(df, x, y, points, group=NULL, palette='A', hist.fill="lightsteelblue2", hist.col='white', xlim=NULL, xlab=NULL, ylab=NULL, contour.breaks = 200, direction=1)
{
    g = df %>%
        dplyr::rename(group=!!group, x=!!x, y=!!y)  %>%
        dplyr::mutate(group=factor(group))  %>% 
        ggplot2::ggplot(., ggplot2::aes(x = x , y = y, colour=group)) +
        ggplot2::stat_density_2d(ggplot2::aes(fill = ..level..), n=contour.breaks, colour='white', size=0,contour=T, geom='polygon',alpha=.4) +
        ggplot2::scale_fill_distiller(palette=palette, direction=direction) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom")+
        ggplot2::guides(fill=FALSE)
    if (points) g = g +  ggplot2::geom_point(size=1.2) +  ggplot2::scale_colour_brewer(palette='Dark2', name="")
    return(g)

}

## }}}


