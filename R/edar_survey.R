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

## =====================================================
## Recodings
## =====================================================


## {{{ docs }}}
#'
#' Recode variable that uses likert scale
#'
#' This function can be used to quickly recode variables that use 5 points likert scale
#'
#' @param df a data.frame
#' @param vars.to.recode a string vector with the names of the variables to recode
#' @param invert boolean, indicates if the values of the new variable should be inverted
#' @param new.levels a string vector with the name of the new categories
#'
#' @return The function returns the data frame with 6 new variables named <vars.to.recode>5 (numerical variable with 5 levels from 1 to 5),<vars.to.recode>5c (factor with 5 levels), <vars.to.recode>3 (numerical variable with 3 levels, -1, 0, and 1), <vars.to.recode>3c (3 levels categorical variable), <vars.to.recode>2 (2 levels numerical variable), <vars.to.recode>2c (two levels categorical variable).
#'
#' @export
## }}}
likert5.recode <- function(df, vars.to.recode, invert=FALSE, new.levels=NULL)
{
    new.vars.names = names(vars.to.recode) %>% paste0(., c("5", "5c", "3", "3c", "2", "2c"))
    if (any(new.vars.names %in% names(df))) {
        df[, which(names(df) %in% new.vars.names)]  %>% print()
        stop("\n\n Some var names already exists \n\n")
    }
    if (is.null(new.levels)) {new.levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")}
    for (i in 1:length(vars.to.recode))
    {
        var          = vars.to.recode[i]
        names(var)   = NULL
        new.var.name = names(vars.to.recode)[i]
        df[,"old.var"] = df %>% dplyr::select(var) 
        if (invert) {
            df = df %>%
                dplyr::mutate(new.var5 = dplyr::case_when(old.var == 1 ~ 5,
                                                          old.var == 2 ~ 4,
                                                          old.var == 3 ~ 3,
                                                          old.var == 4 ~ 2,
                                                          old.var == 5 ~ 1)) %>%
                dplyr::mutate(new.var5c = dplyr::case_when(new.var5 == 1 ~ new.levels[1], #"Strongly disagree",
                                                           new.var5 == 2 ~ new.levels[2], #"Disagree",
                                                           new.var5 == 3 ~ new.levels[3], #"Neither agree nor disagree",
                                                           new.var5 == 4 ~ new.levels[4], #"Agree",
                                                           new.var5 == 5 ~ new.levels[5], #"Strongly agree"
                                                           ),
                              new.var3 = dplyr::case_when(new.var5 %in% 1:2 ~ -1,
                                                          new.var5 %in% 3   ~  0,
                                                          new.var5 %in% 4:5 ~  1), 
                              new.var3c = dplyr::case_when(new.var3 == -1 ~ new.levels[2], #"Disagree",
                                                           new.var3 == 0  ~ new.levels[3], #"Neutral", 
                                                           new.var3 == 1  ~ new.levels[4], #"Agree"
                                                           ),
                              new.var2 = dplyr::case_when(new.var5 %in% 1:3 ~ 0,
                                                          new.var5 %in% 4:5 ~ 1),
                              new.var2c = dplyr::case_when(new.var5 %in% 1:3 ~ new.levels[2], #"Disagree",
                                                           new.var5 %in% 4:5 ~ new.levels[4], #"Agree"
                                                           ),
                              ) 
        }else{
            df = df %>% 
                dplyr::mutate(new.var5 = dplyr::case_when(old.var %in%  1:5 ~ old.var)) %>%
                dplyr::mutate(new.var5c = dplyr::case_when(new.var5 == 5 ~ new.levels[1], #"Strongly disagree",
                                                           new.var5 == 4 ~ new.levels[2], #"Disagree",
                                                           new.var5 == 3 ~ new.levels[3], #"Neutral",
                                                           new.var5 == 2 ~ new.levels[4], #"Agree",
                                                           new.var5 == 1 ~ new.levels[5], #"Strongly agree"
                                                           ),
                              new.var3 = dplyr::case_when(new.var5 %in% 1:2 ~  -1,
                                                          new.var5 %in% 3   ~  0,
                                                          new.var5 %in% 4:5 ~ 1), 
                              new.var3c = dplyr::case_when(new.var3 == 1  ~ new.levels[2], #"Agree",
                                                           new.var3 == 0  ~ new.levels[3], #"Neutral",
                                                           new.var3 == -1 ~ new.levels[4], #"Disagree"
                                                           ),
                              new.var2 = dplyr::case_when(new.var5 %in% 1:2 ~ 1,
                                                          new.var5 %in% 3:5 ~ 0),
                              new.var2c = dplyr::case_when(new.var5 %in% 3:5 ~ new.levels[2], #"Agree",
                                                           new.var5 %in% 1:2 ~ new.levels[4], #"Disagree"
                                                           ),
                              ) 
        }
        df = df %>% dplyr::rename_at(vars(dplyr::contains("new.var")), dplyr::funs(stringr::str_replace(string=., pattern="new.var", replacement=new.var.name)) )
    }
    df = df %>% dplyr::select(-old.var) 
    return(df)
}



## {{{ docs }}}

#' Recode occupation to DGP class scheme
#'
#' The function creates a factor with the EGP class scheme
#'
#'
#' @param isco88 numeric vector with the isco88 4 digit code.
#' @inheritParams isco88toESeC
#' @param collapse boolean, if TRUE, returns a 7 categories class scheme. Otherwise (default), it returns the 11 default categories.
#'
#'
#' @export

## }}}
isco88toEGP <- function(isco88, n.employees=NULL, self.employed=NULL, collapse=FALSE)
{
    ## Recode ISCO88 to EGP using Ganzebooms' code
    egp = rep(NA, length(isco88))

    ## Code the simple version, based on 4 digit ISCO88 values
    ## CLASS 1
    egp[isco88 %in% c(1000, 2000, 1100, 2100, 1110, 2110, 1120, 2111, 2112, 
                      2113, 2114, 2120, 2121, 1200, 2122, 1210, 2130, 1220, 2131, 1222, 1223, 
                      2140, 1224, 2141, 1225, 2142, 1226, 2143, 1227, 2144, 1228, 2145, 1229, 
                      2146, 1230, 2147, 1231, 1232, 2149, 1233, 2200, 3143, 1234, 2210, 3144, 
                      1235, 2211, 1236, 2212, 1237, 2213, 1239, 2220, 2221, 1250, 2222, 1251, 
                      2223, 2224, 2229, 2310, 2350, 2351, 2352, 2400, 2411, 2420, 2421, 2422, 
                      2429, 2440, 2441, 2442, 2443, 2445)] <- 1 ## CLASS 1
    ## CLASS 2
    egp[isco88 %in% c(3000, 3100, 3110, 3111, 1130, 3112, 1140, 3113, 1141, 
                      3114, 1142, 3115, 5121, 1143, 3116, 3117, 3118, 3119, 2132, 3120, 2139, 
                      3121, 3122, 3123, 3130, 3131, 3132, 3133, 3139, 5150, 3140, 5151, 2148, 
                      3141, 5152, 3142, 3145, 3150, 3151, 3152, 1240, 3200, 3210, 3211, 1252, 
                      3212, 1300, 3213, 1310, 2230, 3220, 2300, 3221, 1312, 3222, 1313, 2320, 
                      3223, 1314, 2321, 3224, 1315, 2322, 3225, 1316, 2323, 3226, 1317, 2330, 
                      3227, 1318, 2331, 3228, 1319, 2332, 3229, 2340, 3240, 2359, 3241, 3242, 
                      2410, 2412, 2419, 3400, 3410, 3411, 2430, 3412, 2431, 3413, 2432, 3414, 
                      3415, 3416, 3417, 3419, 2444, 3420, 3421, 2446, 3422, 2450, 3423, 2451, 
                      3429, 2452, 2453, 3431, 2454, 3432, 2455, 2460, 3434, 3440, 3441, 3442, 
                      3443, 3444, 3449, 3450, 3451, 3470, 3471, 3472, 3473, 3474, 
                      3475)] <- 2 ## CLASS 2
    
    ## EGP CLASS 3
    egp[isco88 %in% c(4000, 4100, 4122, 9100, 4110, 9110, 4111, 9111, 4112, 9112, 
                      4113, 9113, 4114, 4115, 4120, 4121, 3230, 3231, 3232, 3300, 3310, 3320, 3330, 
                      3340, 3430, 3433, 3439, 3460, 3480)] <- 3 
    
    ## EGP CLASS 4
    egp[isco88 %in% c(5000, 5100, 5110, 5111, 5112, 5113, 5120, 4130, 5131, 
                      4131, 4132, 5133, 4133, 4140, 4141, 4143, 4144, 4190, 4200, 4210, 4211, 
                      4212, 4213, 4214, 4215, 4220, 4221, 5200, 4222, 5210, 4223, 5220, 5230)] <- 4  
    ## CLASS 7
    egp[isco88 %in% c(3452, 7510)] <- 7 ## CLASS 7
    ## CLASS 8
    egp[isco88 %in% c(7000, 7120, 5122, 7124, 7129, 7130, 7132, 5140, 7133, 5141, 7134, 5143, 7136, 8150, 7137, 8151, 7140, 8152, 7141, 8153, 8154, 8155, 5161, 7200, 8159, 
                      5162, 7210, 8160, 7211, 8161, 5164, 7212, 8162, 7213, 8163, 7214, 8170, 7215, 8171, 7216, 8172, 7220, 7221, 7222, 7223, 7224, 7230, 7231, 7232, 7233, 7240, 7241, 7242, 
                      7243, 7244, 7245, 7300, 7310, 7311, 7312, 7313, 7323, 7324, 7340, 7341, 7342, 7343, 7344, 7345, 7346, 7400, 7410, 7411, 7412, 7413, 7414, 7415, 7416, 7420, 7422, 8311, 
                      7423, 7430, 7433, 7434, 7435, 7436, 8332, 7437, 8333, 7440, 7441, 7442, 7500, 7520)] <- 8  ## CLASS 8
    ## CLASS 9
    egp[isco88 %in% c(8000, 9000, 7100, 8100, 7110, 8110, 7111, 8111, 7112, 8112, 7113, 8113, 8120, 9120, 7121, 8121, 9130, 7122, 8122, 9131, 5123, 7123, 8123, 9132, 5130, 
                      8124, 9133, 8130, 9140, 5132, 8131, 9141, 7131, 8139, 9142, 5139, 8140, 9150, 8141, 9151, 8142, 9152, 4142, 5142, 7135, 8143, 9153, 9160, 5149, 9161, 9162, 9200, 7142, 
                      5160, 7143, 5163, 9300, 9310, 5169, 9311, 9312, 9313, 9320, 8200, 9321, 8210, 9322, 8211, 9330, 8212, 9331, 8220, 9332, 8221, 9333, 8222, 8223, 8224, 7234, 8229, 8230, 
                      8231, 8232, 8240, 8250, 8251, 8252, 8253, 8260, 8261, 8262, 7320, 8263, 7321, 8264, 7322, 8265, 8266, 8269, 7330, 8270, 7331, 8271, 7332, 8272, 8273, 8274, 8275, 8276, 
                      8277, 8278, 8279, 8280, 8281, 8282, 8283, 8284, 8285, 8286, 8290, 8300, 7421, 8310, 8312, 7424, 8320, 8321, 7431, 8322, 7432, 8323, 8324, 8330, 8334, 8340, 8400, 
                      7530)] <- 9  

    ## CLASS 10
    egp[isco88 %in% c(6000, 6100, 6110, 6111, 6112, 6113, 6114, 6120, 6121, 6122, 6123, 6124, 6129, 6130, 6134, 6140, 6141, 6142, 6150, 6151, 9210, 6152, 9211, 6153, 9212, 
                      6154, 9213, 8331)] <- 10  
    ## CLASS 11
    egp[isco88 %in% c(1221, 6131, 6132, 6133, 6200, 6210, 1311)] <- 11  

    ## CLASS 5 and 6
    if (!is.null(self.employed) & !is.null(n.employees)) {
        n.employees[is.na(n.employees)]         = 0
        self.employed[is.na(self.employed)] = 0

        ## CLASS 5
        egp[self.employed == 1 & n.employees > 0 ]  = 5
        ## CLASS 6
        egp[self.employed == 1 & n.employees == 0 ] = 6
    }
    if (collapse) {
        egp[egp %in%  c(1)]   = 1
        egp[egp %in%  c(2)]   = 2
        egp[egp %in%  c(3:4)] = 3
        egp[egp %in%  c(5:7)] = 4
        egp[egp %in%  c(8)]   = 5
        egp[egp %in%  c(9)]   = 6
        egp[egp %in%  c(10:11)] = 7
        egp = factor(egp, levels=1:7, labels=c("I   Service class I",
                                                "II  Service class II",
                                                "III Routine non-manual",
                                                ## "III.a Routine non-manual, higher grade",
                                                ## "III.b Routine non-manual, lower grade",
                                                "IV  Self-employed",
                                                ## "IV.a  Self-employed with employees",
                                                ## "IV.b  Self-employed with no empoyees",
                                                ## "IV.c  Self-employed Farmers etc",
                                                "V   Manual supervisors/Lower grade technicians",
                                                "VI  Skilled workers",
                                                "VII Unskilled workers/Farm labours"
                                                ## "VII.a Unskilled workers",
                                                ## "VII.b Farm labours"
                                                ))
    }else{
        egp = factor(egp, levels=1:11, labels=c("I     Service class I",
                                                "II    Service class II",
                                                "III.a Routine non-manual, higher grade",
                                                "III.b Routine non-manual, lower grade",
                                                "IV.a  Self-employed with employees",
                                                "IV.b  Self-employed with no empoyees",
                                                "IV.c  Self-employed Farmers etc",
                                                "V     Manual supervisors/Lower grade technicians",
                                                "VI    Skilled workers",
                                                "VII.a Unskilled workers",
                                                "VII.b Farm labours"))
    }
    return(egp)
    
}

## {{{ docs }}}
#' Compute ESeC 
#'
#' This function returns the ESeC (European Socio-economic classification) scheme
#'
#'
#' @param isco88 a vector with the isco88 codes
#' @param employed a vector containing the value 1 whenever the person is employed
#' @param unemployed a vector containing the value 1 whenever the person is unemployed
#' @param self.employed a vector containing the value 1 whenever the person is self-employed
#' @param supervisor a vector containing the value 1 whenever the person is a supervisor
#' @param n.employees a vector containing the number of employees.
#' @param n.cat either one of the following integers: 10 (default), 6 ,5, or 3 indicating the number of categories to be used inthe ESEeC scheme
#' @param isco88.armed.forces numeric vector with the code for armed forces (usually isco88 major code 0)
#'
#' @return It returns a list with two elements, the ESec code and its label
#'
#' @export
## }}}
isco88toESeC <- function(isco88, employed=NULL, unemployed=NULL, self.employed=NULL, supervisor=NULL, n.employees=NULL, n.cat=10, isco88.armed.forces=NULL)
{
    ## check https://www.iser.essex.ac.uk/archives/esec/user-guide
    ## recode armed forces
    ## -------------------
    if (!is.null(isco88.armed.forces)) {
        isco88[isco88 %in% isco88.armed.forces] = 10
    }else{
        on.exit(cat("\n\nNo code provided for armed forces in the parameter isco88.armed.forces"))
    }
    ## error control
    ## -------------
    if (any(isco88[(!is.na(isco88)) & isco88 != 10] %>% nchar < 4)) {
        stop("\n\nPlease, provide isco88 with 4 digits for all cases. Check isco88 for armed foreces and provide it in the parameter isco88.armed.forces! \n\n")
    }

    if (is.null(employed) | is.null(unemployed) | is.null(self.employed) | is.null(supervisor) | is.null(n.employees)) {
        stop("\n\nThe parameters employed, unemployed, self.employed, supervisor, and n.employees must be non-NULL.\n\n")
    }

    ## reduces isco88 to 3 digits
    ## --------------------------
    isco88 = isco88 %>% stringr::str_replace(string=., pattern=".$", replacement="")

    ## set indicator variables
    ## -----------------------
    self.employed[self.employed != 1 | is.na(self.employed)] = 0
    unemployed   [unemployed    != 1 | is.na(unemployed)   ] = 0
    employed     [employed      != 1 | is.na(employed)     ] = 0
    supervisor   [supervisor    != 1 | is.na(supervisor)   ] = 0
    
    ## Compute employment status categories
    ## ------------------------------------
    status = rep(NA, length(isco88))
    status[self.employed == 1 & n.employees > 9]                       <- 1  #### 1: self-employed 10+ employees
    status[self.employed == 1 & n.employees %in% 1:9]                  <- 2  #### 2: small employers <10
    status[self.employed == 1 & n.employees == 0]                      <- 3  #### 3: self-employed, no employees 
    status[self.employed != 1 & employed    == 1 & supervisor == 1]    <- 4  #### 4: supervisors
    status[self.employed != 1 & employed    == 1 & supervisor != 1]    <- 5  #### 5: employee
    status[unemployed    == 1]                                         <- 6  #### 5: unemployed

    ## Compute ESeC based on status categories
    ## ---------------------------------------
    ## ESeC status categories:
    ## 1  Large employers, higher grade professional, administrative and managerial occupations                                        /Higher salariat
    ## 2  Lower grade professionals, administrative and managerial occupations and higher grade technician and supervisory occupations /Lower salariat
    ## 3  Intermediate occupations                                                                                                     /Higher grade white collar workers
    ## 4  Small employers and self-employed occupations(non-agriculture)                                                               /Petit bourgeoisie or independents
    ## 5  Small employers and self-employed occupations (agriculture)                                                                  /Petit bourgeoisie or independents (agriculture)
    ## 6  Lower supervisors and technicians                                                                                            /Higher grade blue collar workers
    ## 7  Lower services, sales and clerical occupations                                                                               /Lower grade white collar workers
    ## 8  Lower technical occupations                                                                                                  /Skilled workers
    ## 9  Routine occupations                                                                                                          /semi- and non-skilled workers
    ## 10 Never worked and long-term unemployed                                                                                        /Unemployed

    esec = rep(NA, length(isco88))
    ## ------------------------------------------
    ## 1) Self-employed 10+ employees. Defaults to 1 
    ## ------------------------------------------
    esec[status == 1] <- 1
    esec[status == 1 & isco88 %in% c(344, 345)] <- 2
    esec[status == 1 & isco88 %in% c(011, 516)] <- 3
    esec[status == 1 & isco88 == 621] <- 5
    ## ------------------------------------------
    ## 2) Small employers <10. Defaults to 4
    ## ------------------------------------------
    esec[status == 2] <- 4
    esec[status == 2 & isco88 %in% c(010, 110, 111, 114, 200, 210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
    esec[status == 2 & isco88 %in% c(223, 230, 232, 233, 234, 243, 244, 245, 246, 247, 310, 311, 312, 314, 320, 321, 322, 323, 334, 342, 344, 345, 348)] <- 2
    esec[status == 2 & isco88 %in% c(011, 516)] <- 3
    esec[status == 2 & isco88 %in% c(600, 610, 611, 612, 613, 614, 615, 621, 920, 921)] <- 5
    ## ------------------------------------------
    ## 3) Self-employed with no employees. Defaults to 4
    ## ------------------------------------------
    esec[status == 3] <- 4
    esec[status == 3 & isco88 %in% c(010, 110, 111, 114, 200, 210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
    esec[status == 3 & isco88 %in% c(223, 230, 232, 233, 234, 243, 244, 245, 246, 247, 310, 311, 312, 314, 320, 321, 322, 323, 334, 342, 344, 345, 348)] <- 2
    esec[status == 3 & isco88 %in% c(011,516)] <- 3
    esec[status == 3 & isco88 %in% c(600, 610, 611, 612, 613, 614, 615, 621, 920, 921)] <- 5
    ## ------------------------------------------
    ## 4) Supervisors. Defaults to 6
    ## ------------------------------------------
    esec[status == 4] <- 6
    esec[status == 4 & isco88 %in% c(010, 100, 110, 111, 114, 120, 121, 123, 200, 210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
    esec[status == 4 & isco88 %in% c(011, 122, 130,131, 223, 230, 232, 233, 234, 243, 244, 245, 246, 247, 300, 310, 311, 312, 313, 314, 320, 321, 322, 323, 330, 331, 332, 333, 334, 340,341, 342, 343, 
                                                    344, 345, 346, 347, 348, 400, 410, 411, 412, 419, 420, 521)] <- 2
    esec[status == 4 & isco88 == 621] <- 5
    ## ------------------------------------------
    ## 5) Employees
    ## ------------------------------------------
    esec[status == 5 & isco88 %in% c(010, 100,110,111,114,120, 121, 123, 200, 210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
    esec[status == 5 & isco88 %in% c(122,130,131, 223, 230, 232, 233, 234, 243, 244, 245, 246, 247, 310, 311, 312, 314, 320, 321, 322, 323, 334, 342, 344, 345, 348, 521)] <- 2
    esec[status == 5 & isco88 %in% c(011, 300, 330, 331, 332, 333, 340, 341, 343, 346, 347, 400, 410, 411, 412, 419, 420)] <- 3
    esec[status == 5 & isco88 == 621] <- 5
    esec[status == 5 & isco88 %in% c(313, 315, 730,731)] <- 6
    esec[status == 5 & isco88 %in% c(413, 421, 422, 500, 510, 511, 513, 514, 516, 520, 522, 911)] <- 7
    esec[status == 5 & isco88 %in% c(600, 610, 611, 612, 613, 614, 615, 700, 710, 711, 712, 713, 714, 720, 721, 722, 723, 724, 732, 733, 734, 740, 741, 742, 743, 744, 825, 831, 834)] <- 8
    esec[status == 5 & isco88 %in% c(414, 512, 800, 810, 811,  812, 813, 814, 815, 816, 817, 820, 821, 822, 823, 824, 826, 827, 828, 829, 830, 832, 833,900, 910, 912, 913, 914, 915, 916, 920, 921, 930, 931, 932, 933)] <- 9
    ## ------------------------------------------
    ## 6) Unemployed
    ## ------------------------------------------
    esec[status == 6] <- 10

    ## Harrison & Rose, 2006, "(ESeC) User Guide", pg. 9-10
    ## ----------------------------------------------------
    ## The 10 class model may be collapsed to 6, 5 or 3 classes. 
    ##
    ## - 6-class: 
    ##  - classes 1 and 2 = class 1 "the salariat"
    ##  - classes 3 and 6 = "intermediate employee" class 2
    ##  - classes 4 and 5 =  class 3 "small employers and self-employed"
    ##  - class 7 becomes class 4;
    ##  - class 8 becomes class 5;
    ##  - class 9 becomes class 6.
    ##
    ## - 5-class model:
    ##  - classes 5 and 6 in the sidata class model: 
    ##      "lower technical and routine occupations".
    ##
    ## - the three class model:
    ##  - classes 1 and 2=salariat;
    ##  - 3, 4, 5 and 6=intermediate;
    ##  - 7, 8 and 9=working class.
    ##
    ## - Class 10 may be added as an additional in any of the models, 
    ##      if desired.;
    ## Collapse 7 class
    if (n.cat == 7) {
        esec[esec %in% 1:2] <- 1
        esec[esec %in% c(3,6)] <- 2
        esec[esec %in% c(4,5)] <- 3
        esec[esec == 7] <- 4
        esec[esec == 8] <- 5
        esec[esec == 9] <- 6
        esec[esec == 10] <- 7
    }  
    ## Collapse 6 class
    if (n.cat == 6) {
        esec[esec %in% 1:2] <- 1
        esec[esec %in% c(3,6)] <- 2
        esec[esec %in% c(4,5)] <- 3
        esec[esec == 7] <- 4
        esec[esec %in% 8:9] <- 5
        esec[esec %in% 10] <- 6
    }  
    ## Collapse 4 class
    if (n.cat == 4) {
        esec[esec %in% 1:2] <- 1
        esec[esec %in% 3:6] <- 2
        esec[esec %in% 7:9] <- 3
        esec[esec == 10] <- 4
    }

    esec = set.esec.labels(esec, n.cat=n.cat)
    return(esec)
}
set.esec.labels <- function(esec, n.cat=NULL) 
{
    if (n.cat == 10) {
        labels = factor(esec, levels=1:10, labels = c(
                                               'Higher salariat',                   # 1 
                                               'Lower salariat',                    # 2 
                                               'Higher grade white collar workers', # 3
                                               'Petit bourgeoisie',                 # 4
                                               'Petit bourgeoisie (agriculture)',   # 5
                                               'Higher grade blue collar workers',  # 6 
                                               'Lower grade white collar workers',  # 7
                                               'Skilled workers',                   # 8
                                               'Semi- and non-skilled workers',           # 9
                                               'Unemployed'                         # 10
                                           ))
    }
    if (n.cat == 7) {
        labels = factor(esec, levels=1:7, labels = c(
                                              'Salariat',                         # 1
                                              'Intermediate employee',            # 2 
                                              'Small/self employers',             # 3 
                                              'Lower grade white collar workers', # 4
                                              'Skilled workers',                  # 5
                                              'Semi- and non-skilled workers'        ,  # 6
                                               'Unemployed'                       # 10
                                         ))
    }
    if (n.cat == 6) {
        labels = factor(esec, levels=1:6, labels = c(
                                              'Salariat',                           # 1
                                              'Intermediate employee',              # 2 
                                              'Small/self employers',               # 3 
                                              'Lower grade white collar workers',   # 4
                                              'Skilled and semi- and non-skilled workers',# 5
                                               'Unemployed'                         # 6
                                         ))
    }
    if (n.cat == 4) {
        labels = factor(esec, levels=1:4, labels = c('Salariat', 'Intermediate employee/small employers', 'Working class', 'Unemployed'))
    }
    return(labels)
}


