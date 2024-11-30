
## * DONE balance
## ** main

#' Check balance of pre-treatment variables across treatment groups
#'
#' @param data A data frame that contains a dummy indicator
#'             variable for the treatment and control
#'             groups.
#' @param treat A string representing the name of the indicator
#'              variable for the treatment.
#' @param ttest A boolean indicating whether a t-test of the
#'               difference in means between treatment and control
#'               groups should be computed.
#' @param pscore A boolean indicating whether the propensity score
#'               should be computed.
#'               **TBI**
#' @return The function returns a list with named elements. The
#'         element \code{asmd} contains a table with the
#'         Absolute Standardized Mean Difference between groups.
#'         and control groups. The element \code{pscore} contains
#'         the propensity score of all observations. The element
#'         \code{lpscore} of the list is the logarithm of the propensity score.
#'
#' @details The statistics computed here are recommended in
#' \itemize{
#'  \item Imbens, G. W., & Rubin, D. B. (2015). Causal inference in statistics, social, and biomedical sciences: An introduction. Cambridge University Press.
#' }
#'
#' @examples
#' data(survey)
#'
#' balance(survey, treat = 'treat')
#' 
#' # or with a pipe
#' # survey %>% balance(., treat = 'treat')
#' 
#' @export
balance   <- function(data,
                      treat=NULL,
                      reference=NULL,
                      ttest=F,
                      pscore=T)
{
    if(is.null(treat))(stop("'treat' must be provided"))
    if ("haven_labelled" %in% class(dplyr::pull(data[,treat])))
        stop("'treat' cannot be a labelled or nested column")

    data = tibble::tibble(data) 
    treat.unique = dplyr::pull(data[, treat] %>% tidyr::drop_na(.) %>% dplyr::distinct(.))
    treat.unique = sort(treat.unique)
    ntreat       = base::length(treat.unique)
    treat.pairs  = lapply(treat.unique[2:ntreat], function(x) c(treat.unique[1], x))

    res.asmd = tibble::tibble() 
    for (treat.pair in treat.pairs)
    {
        ## ASMD
        ## ----
        idx = dplyr::pull(data[,treat]) %in% treat.pair
        asmd = balance.asmd(data=data[idx,],
                              treat=treat,
                              reference=reference,
                              treat.pair=treat.pair,
                              ttest=ttest
                              )
        res.asmd = res.asmd %>% dplyr::bind_rows(asmd)
    }
 
    res = list(asmd=res.asmd)
    class(res) = "edar_balance"
    return(res)
}

## ** ancillary
balance.asmd   <- function(data,
                           treat,
                           reference,
                           treat.pair,
                           alpha=0.05,
                           ttest=F)
{
    treated.value = treat.pair[2]
    control.value = treat.pair[1]

    control.idx = dplyr::pull(data[,treat]) == treat.pair[1]
    treated.idx = dplyr::pull(data[,treat]) == treat.pair[2]
    treated = data[treated.idx,] %>% dplyr::select(-!!treat)
    control = data[control.idx,] %>% dplyr::select(-!!treat)
    Tr      = dplyr::pull(data[,treat])
    ## Std diff in mean
    ## ----------------
    muc = apply(control,2, mean,na.rm=T)
    mut = apply(treated,2,mean,na.rm=T)
    s2t = apply(treated,2,function(x) stats::var(x,na.rm=T))
    s2c = apply(control,2,function(x) stats::var(x,na.rm=T))
    delta = abs((mut - muc)/sqrt((s2t+s2c)/2))

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

    ## final results
    ## -------------
    results <- cbind(mut=mut,
                     st=sqrt(s2t),
                     muc=muc,
                     sc=sqrt(s2c),
                     NorDiff=delta,
                     lnRatioSdtDev=gamma,
                     pit=pit,
                     pic=pic)
    if (ttest){
        results[,"p(T>|t|)"]=pttest
    }
    ## collecing
    results = (
        results 
        %>% base::data.frame(Variable=rownames(results))  
        %>% tibble::as_tibble(.) 
        %>% dplyr::mutate(
                       T1 = control.value,
                       T2 = treated.value,
                       )
        %>% dplyr::select(
                       Variable, T1, T2,
                       !!glue::glue("Mean (T1)"):=muc,
                       !!glue::glue("Mean (T2)"):=mut,
                       !!glue::glue("Abs SMD (T2-T1)"):=NorDiff,
                       !!glue::glue("SD (T1)"):=sc,
                       !!glue::glue("SD (T2)"):=st,
                       ) 
        %>% tidyr::drop_na(.)
    )

    return(results)
}

balance.pscore <- function(data,
                           treat,
                           reference,
                           treat.pair,
                           alpha=0.05,
                           ttest=F,
                           showMahalanobisDist=F,
                           showpscore=T,
                           showLinPscore=T, col.size=120)
{
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
    
    pscore <- round(c(pscore_meant,sqrt(pscore_vart),pscore_meanc,
                      sqrt(pscore_varc),
                      pscore_dif,NA,pscore_lnSTD,pscore_pialphat,
                      pscore_pialphac),3)
    lpscore <- round(c(lpscore_meant,sqrt(lpscore_vart),lpscore_meanc,
                       sqrt(lpscore_varc),
                       lpscore_dif,NA,lpscore_lnSTD,lpscore_pialphat,
                       lpscore_pialphac),3)        


    
    pscore <- round(c(pscore_meant,sqrt(pscore_vart),pscore_meanc,
                      sqrt(pscore_varc), pscore_dif,pscore_lnSTD,pscore_pialphat,
                      pscore_pialphac),3)
    lpscore <- round(c(lpscore_meant,sqrt(lpscore_vart),lpscore_meanc,
                       sqrt(lpscore_varc),
                       lpscore_dif,lpscore_lnSTD,lpscore_pialphat,
                       lpscore_pialphac),3)

}

## ** print
#' Print
#'
#' Generic method to print the output of a \code{balance}
#'
#' @param x an output of the function \code{balance}
#' @param digits integer, number of digits to display 
#' @param ... ignored
#'
#' @export
print.edar_balance <- function(x, digits=2, ...)
{
    res = x$asmd %>%
        huxtable::huxtable(.) %>% 
        huxtable::set_number_format(value=digits) %>% 
        huxtable::set_number_format(value=0, row=1) %>% 
        ## huxtable::set_col_width(c(.2, .1, .1, .1,.1,.1, .1, .1)) %>% 
        huxtable::set_all_padding(4) %>% 
        huxtable::set_outer_padding(0) %>% 
        ## huxtable::set_bold(row = 1, col = everywhere) %>% 
        huxtable::set_right_border(huxtable::everywhere, 1, huxtable::brdr(3, "double", "grey"))%>% 
        huxtable::set_top_border(row = 1, col = huxtable::everywhere) %>% 
        huxtable::set_bottom_border(row = 1, col = huxtable::everywhere) %>% 
        huxtable::set_bottom_border(row = nrow(.), col = huxtable::everywhere) %>% 
        huxtable::set_na_string("")%>% 
        huxtable::set_caption("\n")%>% 
        huxtable::set_position("left")
    print(res)
}

## ** print
#' Plot
#'
#' Generic method to plot the output of a \code{balance}
#'
#' @param x an output of the function \code{balance}
#' @param ... ignored
#'
#' @export
plot.edar_balance  <- function(x, ...)
{
    tab = x$asmd
    ref.group = tab$T1[1]
    x = "`Abs SMD (T2-T1)`"
    y = "Variable"
    fill     = 'T2'
    leg      = glue::glue("Treatment group (Reference Group: {ref.group })")
    dodge    = .1
    xlab     = 'Absolute Standardized Mean Difference'
    ylab     = 'Variable'
    g = (
        tab
        %>% ggplot2::ggplot(.) 
        + ggplot2::geom_point(ggplot2::aes_string(x=x, y=y, fill=fill),
                              position=ggplot2::position_dodge(dodge))
        + ggplot2::geom_vline(ggplot2::aes(xintercept=0.1), linetype="dashed", col="red")
        + ggplot2::labs(
                       x        = xlab,
                       y        = ylab,
                       ## color    = leg, 
                       fill     = leg,
                       ## linetype = leg,
                       ## shape    = leg,
                       ## alpha    = leg,
                       ## title    = title,
                       ## subtitle = subtitle,
                       ## caption  = caption
                   )
        ## + ggguides()
        + ggplot2::theme_light()
        + ggplot2::theme(
                       legend.position = 'top',
                       legend.justification = 'left'
                   )
        + ggplot2::guides(fill=ggplot2::guide_legend(title.position="top"))
    )
    return(g)
}

## * DONE descriptive statistics

#' Descriptive Statistics
#'
#' This function computes descriptive statistics for specified variables in a dataset,
#' optionally grouped by certain categorical variables. The function returns a tidy data frame
#' containing the summaries of the selected variables, including relevant statistics such as 
#' mean, standard deviation, and median.
#'
#' @param data A data frame containing the variables for which descriptive statistics 
#' should be calculated.
#' @param vars A character vector or symbol specifying the variables for which 
#' descriptive statistics will be computed. If NULL, all variables in the data frame will be used.
#' @param groups A character vector or symbol specifying one or more grouping variables. 
#' If NULL, no grouping will be performed.
#' @param use.labels A logical value indicating whether to use variable labels. Default is TRUE.
#' @param digits An integer specifying the number of decimal places to round the numeric results. 
#' Default is 4.
#'
#' @return A data frame containing the descriptive statistics for the specified variables.
#'
#' @examples
#' # Load example dataset
#' data(mtcars)
#'
#' # Get descriptive statistics for all variables
#' descriptive.statistics(mtcars)
#'
#' # Get descriptive statistics for specific variables grouped by a factor
#' descriptive.statistics(mtcars, vars = c("mpg", "hp"), groups = "cyl")
#'
#' @export
descriptive.statistics <- function(data, vars=NULL, groups=NULL, use.labels=T,
                                   digits=4)
{
    ## handling vars NULL
    vars <- rlang::enquo(vars)
    if (rlang::quo_is_null(vars)) {
        vars=names(data)
    } else if (rlang::quo_is_symbol(vars)) {
        vars <- rlang::get_expr(vars)
    } 
    ## ## 
    ## handling groups NULL
    groups <- rlang::enquo(groups)
    if (rlang::quo_is_null(groups)) {
        groups <- NULL
    } else if (rlang::quo_is_symbol(groups)) {
        groups <- rlang::get_expr(groups)
    }
    ## 
    data = data %>% select(!!vars, !!groups)
    res = (
        data
        %>% tidyr::nest(-!!groups) 
        %>% dplyr::mutate(
                nobs = purrr::map_int(.x=data, function(.x) nrow(.x)),
                summary = purrr::map2(.x=data, .y=nobs, function(.x, .y)
                    .x
                    %>% rstatix::get_summary_stats(type='common')  
                    %>% dplyr::select(-ci) 
                    %>% dplyr::mutate(`Missing (%)`=100*(.y-n)/.y) 
                    %>% dplyr::select(#!!vars.labels[groups],
                            Variable=variable,
                            N=n,
                            dplyr::contains("Miss"),
                            Mean=mean,
                            Std.Dev=sd,
                            Std.Err=se,
                            Min=min,
                            Median=median,
                            Max=max,
                            IQR=iqr) 
                    ))  
        %>% dplyr::select(-data, -nobs)
        %>% tidyr::unnest(summary)
    )
    if (use.labels) {
        var.names  = data %>% names()
        var.labels = data %>% sjlabelled::label_to_colnames() %>% names()
        res = (
            res 
            %>% dplyr::mutate(Variable = base::factor(Variable,
                                                      var.names,
                                                      var.labels)) 
            %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round, digits))
        )
        ## needed to rename labels of grouping variables, when groups are not NULL
        groups <- rlang::enquo(groups)
        if (!rlang::quo_is_null(groups)){
            res = (
                res 
                %>% labelled::set_variable_labels(
                                  .,
                                  !!!stats::setNames(var.names, var.labels), .strict=F) 
                %>% sjlabelled::label_to_colnames()
            )
        }
    }
    res %>% print(n=Inf)
    invisible(res)
}

## * DONE frequency table

## ** doc

#' freq Function Documentation
#'
#' This function computes frequency distributions for a specified variable and optionally groups within the data set. It can provide outputs in data frame or LaTeX format, including confidence intervals.
#'
#' @param data A data frame containing the variables of interest.
#' @param v The variable name for which the frequency distribution is calculated. This does not need to be quoted.
#' @param groups Optional; a variable name or vectors of variable names to group the results by. This does not need to be quoted.
#' @param show.na Logical; if TRUE, NA values are shown. Default is TRUE. Computation always consider NAs to calculate the percentages.
#' @param print.labels Logical; if TRUE, labels are printed in the results. Default is FALSE.
#' @param output Character; specifies output format, either 'data.frame' or 'latex'. Default is 'data.frame'.
#' @param latex.path Character; path to save the LaTeX files. Default is NULL.
#' @param latex.fn Character; file name for the LaTeX output. Default is NULL.
#' @param latex.align Character; alignment for LaTeX columns. Default is NULL.
#' @param latex.longtable Logical; if TRUE, long tables are used for LaTeX output. Default is FALSE.
#' @param latex.caption Character; caption for the LaTeX table. Default is NULL.
#' @param latex.label Character; label for referencing the LaTeX table. Default is ''.
#' @param latex.na.rep Character; representation of NA in LaTeX output. Default is 'NA'.
#' @param latex.escape Logical; if TRUE, special LaTeX characters are escaped. Default is TRUE.
#' @param latex.digits Integer; number of digits after the decimal point in LaTeX output. Default is 2.
#'
#' @return A data frame or LaTeX table displaying frequency distributions and related statistics.
#'
#' @examples
#' data(survey)
#' freq(survey, treat, c(educ, gender))
#'
#' @export
## ** func
freq <- function(data, v, groups=NULL,
                 show.na=T,
                 print.labels=F,
                 output          = 'data.frame',
                 latex.path      = NULL,
                 latex.fn        = NULL,
                 latex.align     = NULL,
                 latex.longtable = F,
                 latex.caption   = NULL,
                 latex.label     = '',
                 latex.na.rep    = 'NA',
                 latex.escape    = T,
                 latex.digits    = 2)
{
    
    ## Define the function to be executed
    original_options <- options()
    on.exit(options(original_options), add = TRUE)
    

    ## handling groups NULL
    groups <- rlang::enquo(groups)
    if (rlang::quo_is_null(groups)) {
        groups <- NULL
    } else if (rlang::quo_is_symbol(groups)) {
        groups <- rlang::get_expr(groups)
    } 
    z = stats::qnorm((1 + .95) / 2) 

    ## results
    res = (
        data 
        %>% tidyr::nest(-!!groups)
        %>% dplyr::mutate(
                data = purrr::map(.x=data,
                                  function(.x)
                                  (
                                      .x
                                      %>% sjmisc::frq(!!rlang::ensym(v), show.na=show.na)
                                      %>% data.frame()
                                      %>% tibble::as_tibble() 
                                  ))
            )
        %>% tidyr::unnest(data) 
        %>% dplyr::select(!!groups, variable, everything())  
        ## %>% arrange(!!rlang::enquo(groups))
        %>% dplyr::rename(
                n = frq,
                perc = raw.prc,
                perc.cum = cum.prc
            )
        %>% dplyr::rowwise(.)
        %>% dplyr::mutate(
                se = 100*sqrt((perc/100)*(1-(perc/100))/n),
                lo = perc - z * se,
                hi = perc + z * se
            ) 
        %>% dplyr::ungroup(.)  
        )

    ## print
    print(res, n=Inf, width=Inf) 

    ## latex table
    if (output=='latex') {
        options(knitr.kable.NA = latex.na.rep)

        if (is.null(latex.align)) {
            ncol      = ncol(res)
            ncolfix   = 5
            align     = c(rep("l", times=ncol-ncolfix), rep("c", times=ncolfix))
        }
        if (is.null(latex.caption)) {
            latex.caption=glue::glue("\\label{{{latex.label}}}Variable: {unique(res$variable)}")
        }
        tab = (
            res 
            %>% dplyr::mutate('95% CI'=glue::glue("({round(lo, digits=latex.digits)}, ",
                                          "{round(hi, digits=latex.digits)})"))
            %>% dplyr::select(-variable, -lo, -hi, -perc.cum)
            %>% dplyr::rename("Freq. (%)"=perc,
                              'Label'=label,
                              'Value'=val,
                              'N'=n,
                              'SE'=se,
                              ## "Cum. %"=perc.cum
                      )
        )
        tabl = (
            tab
            %>% kableExtra::kbl(., "latex", booktabs = T, caption=latex.caption,
                                escape=latex.escape, align=latex.align,
                                position="htb", digits=latex.digits, table.envir = "table",
                                longtable = latex.longtable,
                                linesep = NULL
                                )
            %>% kableExtra::kable_styling(latex_options = c("repeat_header"),
                                          repeat_header_continued = T,
                                          position = "center", font_size=NULL) 
        )
        print(tabl, n=Inf, width=Inf) 
        if (!is.null(latex.fn))
        {
            latex.path = ifelse(!is.null(latex.path), latex.path, "./")
            save.table(tab, tabl, latex.path, latex.fn)
        }
    }

    invisible(res)
}

## ** ancillary

## #' @export
## print.edar_freq <- function(x, digits=2, ...)
## {
##     tab = x
##     caption = glue::glue("Variable: {unique(tab$variable)}\n\n")
##     ht = tab %>%  
##         dplyr::mutate('95% CI'=glue::glue("({round(lo, digits=digits)}, ",
##                                           "{round(hi, digits=digits)})"))%>% 
##         dplyr::select(-variable, -lo, -hi, -perc.cum)%>%  
##         dplyr::rename("Perc."=perc,
##                       'Label'=label,
##                       'Value'=val,
##                       'N'=n,
##                       'SE'=se,
##                       ## "Cum. %"=perc.cum
##                       )%>% 
##         ## 
##         huxtable::huxtable(.) %>% 
##         huxtable::set_number_format(value=digits) %>% 
##         huxtable::set_number_format(value=0, row=1) %>% 
##         ## 
##         huxtable::set_col_width(c(rep(.1, times=ncol(.)-1), .2)) %>% 
##         huxtable::set_width(300) %>% 
##         ## 
##         huxtable::set_all_padding(4) %>% 
##         huxtable::set_outer_padding(0) %>% 
##         huxtable::set_bold(row = 1, col = everywhere) %>% 
##         huxtable::set_top_border(row = 1, col = everywhere) %>% 
##         huxtable::set_bottom_border(row = 1, col = everywhere) %>% 
##         huxtable::set_bottom_border(row = nrow(.), col = everywhere) %>% 
##         huxtable::set_na_string("NA")%>% 
##         huxtable::set_caption(glue::glue("\n\n{caption}"))%>% 
##         ## 
##         huxtable::set_position("left")
##     huxtable::print_screen(ht)
## }

## * DONE 2x2 table

#** doc
#' Table Function
#'
#' This function creates a contingency table from two specified variables in a dataset, with optional grouping, statistical calculations, and formatting.
#'
#' @param data A data frame containing the data to be analyzed. 
#' @param v1 The first variable for the contingency table. No need to be quoted.
#' @param v2 The second variable for the contingency table. No need to be quoted.
#' @param groups  Optional grouping variable of vector of variables. No need to be quoted.
#' @param margin Specifies the margin for proportions ('all', 'row', or 'col').
#' @param stat The type of statistic to calculate: "n" for counts, "perc" for percentages, or "both" for both.
#' @param total Specifies whether to include totals for rows or columns ("row", "col").
#' @param use.labels Logical; should labels from the data be used?
#' @param digits The number of digits to display for percentages.
#' @param na.label The label to use for NA values.
#' @return A printed table of counts and/or proportions as specified, with totals where applicable.
#' 
#' @export
## ** func
tbl <- function(data, v1, v2, groups=NULL,
                   margin='all',
                   stat="both",
                   total=c("row", "col"),
                   use.labels=T,
                   digits=1,
                   na.label='NA'
                   )
{
    ## handling groups NULL
    groups <- rlang::enquo(groups)
    if (rlang::quo_is_null(groups)) {
        groups <- NULL
    } else if (rlang::quo_is_symbol(groups)) {
        groups <- rlang::get_expr(groups)
    } 

    data = data %>% dplyr::select(!!rlang::ensym(v1), !!rlang::ensym(v2), !!groups)
    if (use.labels) {
        data = data%>% sjlabelled::as_label(.) 
    }
    res = (
        data  
        %>% tidyr::nest(-!!groups)
        %>% dplyr::mutate(
                data = purrr::map(.x=data,
                                  function(.x)
                                  (
                                      if (stat=='n') {
                                          (.x
                                              %>% janitor::tabyl(!!rlang::ensym(v1),
                                                                 !!rlang::ensym(v2))   
                                              %>% janitor::adorn_totals(total)
                                          )
                                      }else if (stat=='perc'){
                                          (.x
                                              %>% janitor::tabyl(!!rlang::ensym(v1),
                                                                 !!rlang::ensym(v2))   
                                              %>% janitor::adorn_totals(total)
                                              %>% janitor::adorn_percentages(margin)
                                          )
                                      }else{
                                          (.x
                                              %>% janitor::tabyl(!!rlang::ensym(v1),
                                                                !!rlang::ensym(v2))   
                                              %>% janitor::adorn_totals(total)
                                              %>% janitor::adorn_percentages(margin) 
                                              %>% janitor::adorn_pct_formatting(digits = digits) 
                                              %>% janitor::adorn_ns() 
                                          )
                                      }
                                  ))
            )
        %>% tidyr::unnest(data)
    )    
    res = res%>% dplyr::mutate(dplyr::across(!dplyr::where(is.numeric),
                                             ~tidyr::replace_na(.x, na.label)))
    ## ----- printing -----
    row.label = labelled::var_label(data %>% dplyr::pull(!!rlang::ensym(v1)))
    row.label = ifelse(is.null(row.label), '--', row.label)
    col.label = labelled::var_label(data %>% dplyr::pull(!!rlang::ensym(v2)))
    col.label = ifelse(is.null(col.label), '--', col.label)
    cat(glue::glue("\n\n",
             "Rows: {row.label} ({rlang::ensym(v1)})",
             "\n",
             "Cols: {col.label} ({rlang::ensym(v2)})",
             "\n\n"))
    res%>% print(n=Inf)
    ## ----- -------- -----
    invisible(res)
}
