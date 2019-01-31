
## {{{ anxilary }}}

## old (first) function used by gge_fit
edar_get_fitted <- function(data, smry, formula, newdata=NULL, x=NULL, n=NULL, cat.values)
{
    ## input checks
    ## ------------
    if (is.null(n)) {stop("\n\nParameter 'n' must be provided\n\n")}
    if (is.null(x)) {stop("\n\nParameter 'x' must be provided\n\n")}
    if (is.null(formula)) {stop("\n\nParameter 'formula' must be provided\n\n")}

    ## creating aux values to get fitted value for each model
    ## ------------------------------------------------------
    if (!is.null(model.id)) {
        if (!model.id %in% names(smry)) {
            stop(paste0("\n\nThe column ", model.id," used in the parameter 'model.id' is not in the summary table used for the parameter smry\n\n") )
        }
        models   = smry %>% dplyr::select(model.id)  %>% dplyr::pull(.) %>% unique
        smrys    = smry %>% dplyr::rename(model.id = !!model.id) 
        formulas = formula
    }else{
        smrys          = smry
        smrys$model.id = "Model 1"
        models         = "Model 1"
        formulas      = list("Model 1" = formula)
    }


    terms   = smry$term
    n       = nrow(data)

    cat.vars = data  %>%  dplyr::select_if(function(col) !is.numeric(col)) %>% names
    if (any(cat.vars %in% all.vars(formula))) {
        ## if there are categorical variables, create a data.frame containing all combinations of factors, 
        ## one combination per row, with no repetitions
        ## Numerical variables are set to their mean
        factors = data %>%
            dplyr::group_by(.dots=cat.vars) %>%
            dplyr::summarise_all(mean, na.rm=T) %>%
            dplyr::ungroup(.)
        
        nrow.newdata = newdata %>% nrow
        nrow.factors = factors %>% nrow
        
        Xp = newdata %>% 
            dplyr::bind_rows(., factors) %>%
            stats::model.matrix(formula, .)  %>% 
            tibble::as_data_frame(.) %>%
            dplyr::select(terms)  %>%
            dplyr::slice(., 1:nrow.newdata) %>%
            as.matrix
    }else{
        Xp = newdata %>%
            stats::model.matrix(formula, .) %>%
            tibble::as_data_frame(.) %>%
            dplyr::select(terms)  %>%
            as.matrix
    }


    y.label  = all.vars(formula)[1]
    n       = nrow(data)
    np      = nrow(newdata)
    k       = ncol(Xp) - 1
    y        = data[,y.label] %>% as.matrix(., ncol=1)
    X        = stats::model.matrix(formula, data)[,terms]
    XtX.inv  = solve(t(X) %*% X)
    beta.hat = smry$estimate
    y.hat    = X %*% beta.hat
    .fitted  = c(Xp %*% beta.hat)

    ylb=rep(NA,np)
    yub=rep(NA,np)

    ## binomial/logistic regression
    if (all(y %in% c(0,1))) {
        pi.hat = c(1/(1+exp(-X %*% beta.hat)))
        V = diag( pi.hat * (1-pi.hat) ) 
        XtVX.inv = solve( t(X) %*% V %*% X )
        for (i in 1:np)
        {
            H          = c(Xp[i,] %*% XtVX.inv %*% Xp[i,])
            ylb[i]     = 1/(1 + exp(- .fitted[i] - stats::qt(.975, df = n-(k+1)) * sqrt(H) ))
            yub[i]     = 1/(1 + exp(- .fitted[i] + stats::qt(.975, df = n-(k+1)) * sqrt(H) ))
            .fitted[i] = 1/(1 + exp(-.fitted[i]))
        }
    }else{
        ## continuous outcome
        se.beta.hat = smry$std.error
        epsilon  = y - y.hat
        sigma2.hat = (1/n) * ( t(epsilon) %*% epsilon ) %>% c
        for (i in 1:np)
        {
            H          = c(Xp[i,] %*% XtX.inv %*% Xp[i,])
            ylb[i]     = .fitted[i] - stats::qt(.975, df = n-(k+1)) * sqrt(sigma2.hat * ( H))
            yub[i]     = .fitted[i] + stats::qt(.975, df = n-(k+1)) * sqrt(sigma2.hat * ( H))
        }

    }
    results = newdata %>%
        dplyr::bind_cols(., tibble::data_frame(.fitted = .fitted,
                                               ylb     = ylb,
                                               yub     = yub) )
    return(results)
}
## {{{ docs }}}

#' Get fitted values
#'
#' Get fitted values from summary table
#' @param data the dataset used to estimate the model
#' @param smry the summary table organized in data frame (see function \code{broom::tidy()}). It must contain at least three columns: one named \code{term} that follows the pattern of the table return by \code{lm} function; a column named \code{estimate} with the point estimate of the linear coefficient; \code{std.error} or a similar measure used to capture the uncertainty of the \code{esitmate}. The table may contain results of multiple models, in which case it must contain an additional column whose name must be provided to the parameter \code{model.id}.
#' @param formula an R formula used to estimate the model, as used in the function \link{lm}, for instance. If the \code{smry} contains multiple models, formula must be a named list. Each element of the list must contain the formula of the respective model, and the names must match the names used in the column to identity the models in the summary table passed in the parameter \code{smry}.
#' @param newdata either \code{NULL} or a data set that will be used to compute the fitted values.
#' @param x a string with the name of the variable that will be used as varying dependent variable to compute the fitted values.
#' @param n an integer with the number of data points that will be used to compute the fitted values. Default: 200.
#' @inheritParams gge_fit
#'
#'
#' @export

## }}}
get_fitted <- function(data, smry, formula=NULL, newdata=NULL, x=NULL, n=200, cat.values=NULL, model.id=NULL)
{

    ## input checks
    ## ------------
    if (is.null(n)) {stop("\n\nParameter 'n' must be provided\n\n")}
    if (is.null(x)) {stop("\n\nParameter 'x' must be provided\n\n")}
    if (is.null(formula)) {stop("\n\nParameter 'formula' must be provided\n\n")}
    if (is.list(formula) & is.null(model.id)) {stop("\n\nYou are providing a list of formulas. The variable 'model.id' must be provided as well.\n\n")}
    if (!is.list(formula) & !is.null(model.id)) {stop("\n\nYou are providing a value for model.id. The variable 'formula' must be a named list of formulas. See help(get_fitted).\n\n")}

    
    if (!is.null(model.id)) {
        if (!model.id %in% names(smry)) {
            stop(paste0("\n\nThe column ", model.id," used in the parameter 'model.id' is not in the summary table used for the parameter smry\n\n") )
        }
        models   = smry %>% dplyr::select(model.id)  %>% dplyr::pull(.) %>% unique
        smrys    = smry %>% dplyr::rename(model.id = !!model.id) 
        formulas = formula
    }else{
        smrys          = smry
        smrys$model.id = "Model 1"
        models         = "Model 1"
        formulas      = list("Model 1" = formula)
    }
    ## get new data if it is null
    if (is.null(newdata)) {
        ## newdata = edar_gnd(data, n=n, x=x, cat.values=cat.values)
        newdata = gge_fit_get_new_data(data, n=n, x=x, cat.values=cat.values)
    }

    n.size.newdata = n
    results.final = tibble::data_frame() 
    for (i in 1:length(models))
    {
        n = n.size.newdata

        model    = models[i]
        smry     = smrys %>% dplyr::filter(model.id == !!model) 
        terms    = smry$term
        formula  = formulas[[model]]

        cat.vars = data  %>%  dplyr::select_if(function(col) !is.numeric(col)) %>% names %>% intersect(., all.vars(formula))
        if (length(cat.vars ) > 0)
        {
            ## create a data frame with unique case of each categorical variable (even those not used in the regression
            ## to keep consistency with other function, such as those that create new data. Below we bind rows with new data)
            ## The goal is to create a data frame with unique combinations of each categorical variable.
            ## It is needed for the function that creates the design matrix below.
            factors = data %>%
                dplyr::select(all.vars(formula))  %>% 
                dplyr::group_by(.dots=cat.vars) %>%
                dplyr::summarise_all(mean, na.rm=T) %>%
                dplyr::ungroup(.)  %>% 
                dplyr::mutate_if(is.factor, as.character)

            nrow.newdata = newdata %>% nrow
            nrow.factors = factors %>% nrow
            
            Xp     = newdata %>% dplyr::select(all.vars(formula))  %>% dplyr::bind_rows(., factors)
            Xp     = set_ref_categories(Xp, data, cat.vars)
            ## create a prelimilaty model matrix to check if all categorical variables are in fact using the correct reference category. If not, ask user to set it correctly in the data.
            Xp.tmp = Xp %>% stats::model.matrix(formula, .)
            if (any(!terms %in% colnames(Xp.tmp))) {
                stop(
                    paste0("\n\nThe data used to estimate the model and the data provided are using different categories of reference for the categorical variables.\n",
                           "Please, set the correct referecne category (using relevel()) and try again.\n\n",
                           "The categories shown in the estimation that must be set as reference are/is: ",
                           colnames(Xp.tmp)[!terms %in% colnames(Xp.tmp)],
                           "\n\n"
                           ) 
                )
            }else{
                Xp = Xp.tmp %>% 
                    tibble::as_data_frame(.) %>%
                    dplyr::select(terms)  %>%
                    dplyr::slice(., 1:nrow.newdata) %>%
                    as.matrix
            }
        }else{
            Xp = newdata %>%
                stats::model.matrix(formula, .) %>%
                tibble::as_data_frame(.) %>%
                dplyr::select(terms)  %>%
                as.matrix
        }

        y.label  = all.vars(formula)[1]
        n       = nrow(data)
        np      = nrow(newdata)
        k       = ncol(Xp) - 1
        y        = data[,y.label] %>% as.matrix(., ncol=1)
        X        = stats::model.matrix(formula, data)
        XtX.inv  = solve(t(X) %*% X)
        beta.hat = smry$estimate
        y.hat    = X %*% beta.hat
        .fitted  = c(Xp %*% beta.hat)

        ylb=rep(NA,np)
        yub=rep(NA,np)

        ## binomial/logistic regression
        if (all(y %in% c(0,1))) {
            pi.hat = c(1/(1+exp(-X %*% beta.hat)))
            V = diag( pi.hat * (1-pi.hat) ) 
            XtVX.inv = solve( t(X) %*% V %*% X )
            for (i in 1:np)
            {
                H          = c(Xp[i,] %*% XtVX.inv %*% Xp[i,])
                ylb[i]     = 1/(1 + exp(- .fitted[i] - stats::qt(.975, df = n-(k+1)) * sqrt(H) ))
                yub[i]     = 1/(1 + exp(- .fitted[i] + stats::qt(.975, df = n-(k+1)) * sqrt(H) ))
                .fitted[i] = 1/(1 + exp(-.fitted[i]))
            }
        }else{
            ## continuous outcome
            se.beta.hat = smry$std.error
            epsilon  = y - y.hat
            sigma2.hat = (1/n) * ( t(epsilon) %*% epsilon ) %>% c
            for (i in 1:np)
            {
                H          = c(Xp[i,] %*% XtX.inv %*% Xp[i,])
                ylb[i]     = .fitted[i] - stats::qt(.975, df = n-(k+1)) * sqrt(sigma2.hat * ( H))
                yub[i]     = .fitted[i] + stats::qt(.975, df = n-(k+1)) * sqrt(sigma2.hat * ( H))
            }

        }
        results = newdata %>%
            dplyr::bind_cols(., tibble::data_frame(.fitted = .fitted,
                                                   .ylb     = ylb,
                                                   .yub     = yub) ) %>%
            tibble::as_data_frame() 

        if (!is.null(model.id)) {
            results$model.id = model
            results = results %>% dplyr::rename_at(vars(model.id), dplyr::funs(paste0(model.id) ) )
        }
        results.final = results.final %>% dplyr::bind_rows(results)
    }
    
    return(results.final)
}
set_ref_categories <- function(X, data, cat.vars)
{
    for (cat.var in cat.vars)
    {
        ## cat.var = cat.vars[1] 
        ref.cat = data %>% dplyr::pull(cat.var) %>% magrittr::extract2(1) %>% levels(.) %>% magrittr::extract(1) 
        if (is.null(ref.cat)) {
            stop(paste0("\n\nThe variable '", cat.var ,"' is coded as character. Please set it to factor and use as reference level (see relevel()) the same level used in the regression.\n\n") )
        }else{
            X = X %>% dplyr::mutate_at(dplyr::vars(cat.var), dplyr::funs(as.factor(.))) %>% dplyr::mutate_at(dplyr::vars(cat.var), dplyr::funs(relevel(., ref.cat)))
        }
    }
    return(X)
}
edar_get_matrix_with_factors <- function(data, cat.values, n, cat.vars=NULL, formula)
{
    factors = data %>%
        dplyr::select(intersect(cat.vars, all.vars(formula)))  %>% 
        dplyr::filter(!duplicated(.)) %>% 
        dplyr::mutate_all(dplyr::funs(as.character)) %>%
        tidyr::gather(key = variable, value=value)  %>% 
        dplyr::filter(!duplicated(.)) %>% 
        tidyr::unite(., col=cat, sep="", remove=TRUE)  %>%
        dplyr::mutate(value = 0)  %>% 
        tidyr::spread(., key=cat, value=value)
    if (!is.null(cat.values)) {
        for (i in 1:length(cat.values))
        {
            variable = names(cat.values)[i]
            values   = cat.values[[i]]
            for (j in 1:length(values))
            {
                ## crate a size n temporaty data.frame
                factors.tmp = factors %>%  .[rep(1, n),]
                value = values[j]
                mat.design.varname = paste0(variable, value) 
                ## store that data.frame with 1's for the specific factors
                factors.tmp = factors.tmp %>% dplyr::rename(variable___tmp___ = !!mat.design.varname)  %>%  dplyr::mutate(variable___tmp___ = 1 %>% as.integer) %>% dplyr::rename_at(vars(variable___tmp___), dplyr::funs(paste0(mat.design.varname) ) ) 
                factors = factors %>% dplyr::bind_rows(., factors.tmp)
            }
        }
        factors = factors[-1,]
    }else{
        factors = factors %>% .[rep(1, n),]
    }
    return(factors)
}
edar_gnd <- function(data, n, x, cat.values=NULL)
{
    variables = names(data)
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


    newdata = newdata %>% dplyr::mutate_if(is.factor, as.character) %>% dplyr::select(variables) 
    return(newdata)
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
## {{{ Plot fitted values    }}}

## {{{ docs }}}

#' Plot fitted values
#'
#' This function plot the fitted value or the predicted probability given the model estimated using \code{lm} or \code{glm} and the original data. 
#'
#' @param model a model object, a list of them, or a tidy data frame with summary(ies) of the estimation(s). A model object can be one produced as output of \code{\link[stats]{lm} } or \code{\link[stats]{glm}}.  If the tidy summary is provided instead of the model object it must contain the columns \code{term}, \code{estimate} (the point estimate), and std.error (standard error of the estimates). The content of the column \code{term} must match the name of the covariate associated the estimates as produced by \code{attr(terms(formula), 'term.labels')}, and \code{formula} must also be provided as a parameter. If there are more than one model in the summary table, it must have a column identifying the models.
#' @param data the original data set used to fit the model
#' @param y a string with the name of the dependent variable
#' @param x a string with the name of the independent variable that will be used to plotted in the x-axis.
#' @param n an integer, the number of points of \code{x} generated to produce the predicted/fitted values
#' @param formula a expression as used in \code{lm} function. It is used when the parameter \code{model} receives a tidy summary. If more than one model is provided in the table, \code{formula} must be a named list. The names must match the values in the column in the table that identify each model. The formula must also match the model. See examples.
#' @param cat.values named list of string vectors. The name of each element of the list (the string vectors) must match variable names in the data. The element of the string vectors must be strings with the name of the categories to use in the plot with the fitted values. To generate the fitted values, the numeric columns will be set to their mean value, except the column specified in the parameter \code{x}. The categorical values are set to their first category or the first category in alphabetic order. One can set the categorical variables to different values or use more than one category by setting this parameter \code{cat.value} as desired. For instance, suppose there is a categorical variable in the data set named education, taking the values of \code{High} or \code{Low}, which was used in the model. If \code{cat.value=NULL}, the plot with the predicted values will be fixed at \code{education=High}. One can use \code{education="Low"} by setting \code{cat.value=list(eductation="low")}. One can generate predicted values for both levels of education by setting \code{cat.value=list(eductation=c("low", "high"))}. See more examples in the documentation below.
#' @param model.id a string with the name of the column that identifies the model in the table smry.
#' @param show.points boolean, indicating if the points from the original data set must be plotted or not
#' @param pch.col a string or rgb code.
#' @param pch.pos a string with position of the points (e.g., jitter, identity). Default is identity. 
#' @param pch.col.cat a string vector with the name of the categorical variable in the data to use as color key for the points
#' @param pch.col.palette a named string vector with one element. The name must be the color palette (brewer, viridis and the value a string with the pallete available (ex: "Blues" for brewer palette, "A" for viridis, etc)
#' @param fill.palette as as for pch.col.palette 
#' @param xlab string with text to display in the x-axis 
#' @param ylab string with text to display in the y-axis 
#' @param xlim two-dimensional numeric vector with the limits of the x-axis 
#' @param ylim two-dimensional numeric vector with the limits of the y-axis
#' @param facets a string vector with the name of the categorical variable to generate the facets. The fitted values will be produced for each facet.
#' @param facets.title.position a string with \code{left}, \code{right}, or \code{center}. Indicates the position of the title of the facets
#' @param facets.ncol an integer, the number of columns of grid when using facets. 
#' @param facets.scales used when facets are used. See \code{\link[ggplot2]{facet_wrap}}
#' @param legend.show.formulas boolean, if \code{TRUE} and more than one model is provided, the formula associated with the model is displayed in the legend 
#' @param legend.show.cat boolean, if \code{TRUE} and both cat.values and more than one model are provided, it shows the categories used to fit the values in each model
#' @param title a string, the title of the plot
#' @param title.position a string (or an number) with \code{center} (or .5), \code{left} (or 0), or \code{right} (or 1)
#' @param subtitle a string, the subtitle of the plot
#' @param footnote a string, the footnote of the plot
#' @param legend.position a string (\code{top}, \code{bottom}, \code{left} (Default)), \code{right} 
#' @param legend.omit boolean, if \code{TRUE} the legend is omitted
#' @param legend.title a string with the title of the legend
#' @param legend.direction see \code{\link[ggplot2]{guide_legend}}
#' @param legend.box see \code{\link[ggplot2]{guide_legend}}
#' @param legend.ncol.colour an integer indicating the number of columns to use in the legend with color code
#' @param legend.ncol.fill an integer indicating the number of columns to use in the legend with fill code (fitted line estimation interval)
#' @param legend.col.title string, the title of the legend with color code of the points. Default: \code{Categories}
#' @param legend.fill.title string, the title of the legend with color code of the fitted values estimated interval 
#' @inheritParams tidye
#'
#' 
#' set.seed(77)
#' data = tibble::data_frame(n = 300,
#'                           x1   = rnorm(n,3,1),
#'                           x2   = rexp(n),
#'                           cat1 = sample(c(0,1), n, replace=T),
#'                           cat2 = sample(letters[1:4], n, replace=T),
#'                           y    = -10*x1*cat1 + 10*x2*(3*(cat2=='a') -3*(cat2=='b') +1*(cat2=='c') -1*(cat2=='d')) + 
#'                               rnorm(n,0,10), 
#'                           y.bin = ifelse(y < mean(y), 0, 1),
#'                           y.mul = 1+ifelse( - x1 - x2 + rnorm(n,sd=10) < 0, 0,
#'                                     ifelse( - 2*x2 + rnorm(n,sd=10) < 0, 1, 2)),
#'                           ) %>%
#'     dplyr::mutate(cat1 = as.factor(cat1), cat2 = as.factor(cat2)) 
#' 
#' ## Simple examples
#' formula = y ~ x1*cat1 + x2*cat2
#' model   = lm(formula, data)
#' 
#' ## provide either the model object of the summary and the formula
#' model %>%  gge_fit(., data, "y", "x1")
#' model %>% tidye %>%  gge_fit(., data, "y", "x1", formula) 
#' 
#' 
#' ## other parameters can be used:
#' ## Set the categories for the fitted values
#' g1 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat1'="0"), title='cat1 at 0')
#' g2 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat1'="1"), title='cat1 at 1')
#' g3 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat1'=c("0","1")), title='Cat 1 at 0 and 1', legend.position="right")
#' g4 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat1'=c("0","1")), title='Cat 1 at 0 and 1', legend.position="right", pch.col.cat='cat1')
#' ggpubr::ggarrange(g1,g2,g3, g4)
#' 
#' g1 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat2'="a"), title='cat2 at a')
#' g2 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat2'="b"), title='cat2 at b')
#' g3 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat2'="c"), title='cat2 at c')
#' g4 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat2'=c("a","b","c")), title='cat2 at a, b, and c', legend.position="right")
#' ggpubr::ggarrange(g1,g2,g3, g4)
#' 
#' g1 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat2'="a"), title='cat2 at a', pch.col.cat="cat2")
#' g2 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat2'="b"), title='cat2 at b', pch.col.cat="cat2")
#' g3 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat2'="c"), title='cat2 at c', pch.col.cat="cat2")
#' g4 = model %>% edar::gge_fit(., data, 'y', 'x1', cat.values=list('cat2'=c("a","b","c")), title='cat2 at a, b, and c',  pch.col.cat="cat2")
#' ggpubr::ggarrange(g1,g2,g3, g4, common.legend=TRUE)
#' ggpubr::ggarrange(g1,g2,g3, g4) 
#'     
#' ## Set the colour groups by category
#' model %>% edar::gge_fit(., data, 'y', 'x1', pch.col.cat='cat1')
#' model %>% edar::gge_fit(., data, 'y', 'x1', pch.col.cat='cat2')
#' 
#' ## set facets
#' model %>% edar::gge_fit(., data, 'y', 'x1', facets='cat1')
#' model %>% edar::gge_fit(., data, 'y', 'x1', facets='cat2')
#' model %>% edar::gge_fit(., data, 'y', 'x1', facets=c('cat1','cat2'))
#' model %>% edar::gge_fit(., data, 'y', 'x1', facets=c('cat1','cat2'), pch.col.cat='cat1', pch.col.palette=c(brewer="Set2"))
#' model %>% edar::gge_fit(., data, 'y', 'x1', facets='cat2', pch.col.cat='cat1', pch.col.palette=c(brewer="Set2"))
#' model %>% edar::gge_fit(., data, 'y', 'x1', facets='cat1', pch.col.cat='cat2')
#' 
#' 
#' ## list of models
#' ## --------------
#' formula.gau1 = y ~ x1 
#' formula.gau2 = y ~ x1 + x2*cat1
#' formula.gau3 = y ~ x1*cat1 + x2*cat2
#' model.gau1   = lm(formula.gau1, data)
#' model.gau2   = lm(formula.gau2, data)
#' model.gau3   = lm(formula.gau3, data)
#' 
#' 
#' formulas = list("Model 1" = formula.gau1, "Model 2" = formula.gau2, "Model 3" = formula.gau3)
#' models   = list("Model 1" = model.gau1, "Model 2" = model.gau2, "Model 3" = model.gau3)
#' 
#' models %>%  gge_fit(., data, "y", "x1", formulas)
#' models %>%  gge_fit(., data, "y", "x1", formulas, legend.show.formulas=F)
#' 
#' ## setting the values of the categories used to get the fitted values
#' ## ------------------------------------------------------------------
#' models %>%  gge_fit(., data, "y", "x1", formulas, cat.values=list(cat1=c('0')))
#' models %>%  gge_fit(., data, "y", "x1", formulas, cat.values=list(cat1=c('1')))
#' models %>%  gge_fit(., data, "y", "x1", formulas, cat.values=list(cat2=c('a', 'b', "c")), legend.ncol.fill=3)
#' 
#' ## setting the facets
#' ## ------------------
#' models %>%  gge_fit(., data, "y", "x1", formulas, facets='cat1', legend.ncol.fill=3)
#' models %>%  gge_fit(., data, "y", "x1", formulas, facets='cat2', legend.ncol.fill=3)
#' models %>%  gge_fit(., data, "y", "x1", formulas, facets='cat1' , cat.values=list(cat2='d'))
#' 
#' ## setting the facets and colours
#' ## ------------------------------
#' models %>%  gge_fit(., data, "y", "x1", formulas, cat.values=list(cat2=c('a', 'b')), facets='cat1', legend.ncol.fill=3)
#' models %>%  gge_fit(., data, "y", "x1", formulas, cat.values=list(cat1=c('0', '1')), facets='cat2')
#' 
#' ## using color codes for the points
#' ## --------------------------------
#' models %>%  gge_fit(., data, "y", "x1", formulas, cat.values=list(cat2='d'), pch.col.cat='cat1')
#' models %>%  gge_fit(., data, "y", "x1", formulas, cat.values=list(cat2='d'), pch.col.cat='cat1', 
#'                     pch.col.palette = c(brewer='Set2'))
#' 
#' models %>%  gge_fit(., data, "y", "x1", formulas, cat.values=list(cat1=c('0', '1')), facets='cat1', 
#'                     pch.col.cat='cat1',pch.col.palette = c(brewer='Set2'))
#' models %>%  gge_fit(., data, "y", "x1", formulas, cat.values=list(cat1=c('0', '1')), facets='cat1',
#'                     pch.col.cat='cat2')
#' 
#' 
#' ## ... the same applies for logistic regressions
#' ## ---------------------------------------------
#' formula.bin1 = y.bin ~ x1
#' formula.bin2 = y.bin ~ x1+x2*cat1
#' model.bin1   = glm(formula.bin1, data=data, family='binomial')
#' model.bin2   = glm(formula.bin2, data=data, family='binomial')
#' 
#' formulas = list("Model 1" = formula.bin1, "Model 2" = formula.bin2)
#' models   = list("Model 1" = model.bin1, "Model 2" = model.bin2)
#' 
#' models %>%  gge_fit(., data, "y.bin", "x1", formulas)
#' models %>%  gge_fit(., data, "y.bin", "x1", formulas, legend.show.formulas=F)
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
gge_fit <- function(model, data, y, x, formula=NULL, n=200, cat.values=NULL, model.id=NULL,
                    ## points
                    ## ------
                    show.points=T,
                    pch.col="#00000044", 
                    pch.pos="identity",
                    pch.col.cat=NULL,
                    ## x,y lims and labs
                    ## -----------------
                    ylim=NULL, xlim=NULL, xlab=NULL, ylab=NULL,
                    ## facets
                    ## ------
                    facets=NULL,
                    facets.title.position = "left",
                    facets.ncol=NULL,
                    facets.scales='fixed',
                    ## title/footnote
                    ## ------
                    title=NULL,
                    title.position="left",
                    subtitle=NULL,
                    footnote=NULL,
                    ## legend
                    ## ------
                    legend.show.formulas=TRUE,
                    legend.show.cat=FALSE,
                    legend.position='top',
                    legend.omit=F,
                    legend.title=NULL,
                    legend.direction = "horizontal",
                    legend.box = "horizontal",
                    legend.ncol.colour = NULL,
                    legend.ncol.fill = NULL,
                    legend.col.title=NULL,
                    legend.fill.title = NULL,
                    ## color palettes/colors
                    ## ------
                    pch.col.palette = c(brewer="BrBG"),
                    fill.palette    = c(brewer="Paired")
                    )
{
    warnings("FALSE")
    on.exit(warnings("TRUE"))
    par.default <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(par.default), add=TRUE)
    op.default <- options()
    on.exit(options(op.default), add=TRUE)
    dir.default <- getwd()
    on.exit(setwd(dir.default), add=TRUE)

    if ( any(lapply(model, function(x) class(x) %in% 'multinom')  %>% unlist) ) 
        stop("\n\nFunction not implemented to display 'nnet::multinomial' models\n\n")
    if ("y.multin.cat" %in% names(model))
        stop("\n\nFunction not implemented to display 'nnet::multinomial' models\n\n")
    

    ## some aesthetic elements
    ## -----------------------
    if(title.position=="left")   title.position = 0
    if(title.position=="center") title.position = .5
    if(title.position=="right")  title.position = 1

    if (facets.title.position == 'right' ) facets.title.position =  1
    if (facets.title.position == 'center') facets.title.position = .5
    if (facets.title.position == 'left'  ) facets.title.position =  0

    ## ## get the new data
    ## ## ----------------
    newdata = gge_fit_get_new_data(data=data, facets=facets, cat.values=cat.values, n=n, x=x)

    ## get predicted values
    ## --------------------
    ## check if parameter 'model' is a tibble (model summary) or a list of models
    if (any(class(model)[1] %in% c('data.frame',"tbl_df","tbl" )) | class(model)[1] == 'list') {
        ## if it is a list, convert to tibble summary
        if (class(model)[1] == 'list') model = model %>% tidye(.)
        
        ## check if there are non numeric columns other than the columns term (covar names), which identify the different models in the table
        ids = model %>%  dplyr::select_if(function(col) !is.numeric(col)) %>% dplyr::select(-term)  %>% names
        if (!is.null(model.id)) {
            model$model = model[,model.id] %>% dplyr::pull(.)
        }else{
            if (length(ids)>0) {
                model = model %>%
                    tidyr::unite(model, sort(ids), sep=" ") %>%
                    dplyr::mutate(model = stringr::str_replace(model, " NA", "")) 
                model.id='model'
            }
        }

        ## if tidy summary is provided as model, formula must be provided as well
        if (is.null(formula)) {stop("\n\n'formula' must be provided\n\n")}
        if (is.null(formula)) {stop("\n\n'formula' must be provided\n\n")}
        ## if (is.list(formula) & is.null(model.id)) {stop("\n\nYou are providing a list of formulas. The variable 'model.id' must be provided as well.\n\n")}
        ## if (!is.list(formula) & !is.null(model.id)) {stop("\n\nYou are providing a value for model.id. The variable 'formula' must be a named list of formulas. See help(gge_fit).\n\n")}

        ## if there are multiple models in the table, find predicted value for each one of them (in parallel)
        if(!is.null(model.id)){
            if (is.null(names(formula))) {stop("\n\n'formula' must be a named list and the names must match the names of the models\n\n")}

            models = split(model, model$model) 

            models   = models[names(models)]
            formulas = formula[names(models)]
        
            doParallel::registerDoParallel(parallel::detectCores() - 1) ## -- Parallel (start) -----------------------------------------
            "%dopar%" <- foreach::"%dopar%"
            pred = foreach::foreach(model = models,
                                    model.label = names(models),
                                    formula     = formulas,
                                    .maxcombine = length(models),
                                    .combine = list, .multicombine=T) %dopar% ( get_fitted(data=data, smry=model, formula=formula, newdata=newdata, x=x, n=n, cat.values=cat.values) %>% cbind(., Model=model.label) %>% dplyr::mutate_if(is.factor, as.character) )
            doParallel::stopImplicitCluster()  ## --------------------------- Parallel (stop) -------------------------------------------
            pred = pred %>% dplyr::bind_rows(.)
            multiple.models.flag=TRUE
            if(legend.show.formulas)
            {
                pred = pred %>%
                    dplyr::full_join(., tibble::data_frame(Model = names(models), formula=formulas %>% as.character) , by=c("Model"))  %>%
                    dplyr::mutate(Model = paste0(Model, " (",formula,")") ) 
            }
        }else{
            pred = get_fitted(data=data, smry=model, formula=formula, newdata=newdata, x=x, n=n, model.id=model.id)
            multiple.models.flag=FALSE
        }
    }else{
        multiple.models.flag=FALSE
        if (class(model) == 'lm') {
            pred         = broom::augment(model, newdata = newdata, type.predict='response')
            pred$.ylb     = pred$.fitted-1.96*pred$.se.fit
            pred$.yub     = pred$.fitted+1.96*pred$.se.fit
        }else{
            pred         = broom::augment(model, newdata = newdata)#, type.predict='response' for glm
            pred$.ylb     = 1/(1 + exp(-pred$.fitted-1.96*pred$.se.fit))
            pred$.yub     = 1/(1 + exp(-pred$.fitted+1.96*pred$.se.fit))
            pred$.fitted = 1/(1 + exp(- pred$.fitted))
        }
    }

    ## create colour groups in the data frame
    ## --------------------------------------
    if (!is.null(pch.col.cat)) {
        cat.colors = pch.col.cat
        pch.col.cat=paste0(pch.col.cat, collapse=' ; ') 
        data = data %>% dplyr::mutate_if(is.factor, as.character) %>% tidyr::unite(pch.col.cat, cat.colors, sep=' ; ', remove=FALSE)
    }

    ## create variable to groups the data given categorical variables set by the user
    ## ------------------------------------------------------------------------------
    if (!is.null(cat.values)) {
        cat.groups = names(cat.values)
        pred = pred %>% tidyr::unite(cat.groups, cat.groups, sep=' - ', remove=FALSE)
        if(multiple.models.flag){
            pred = pred %>%
                tidyr::unite(Cats, cat.groups, remove=FALSE, sep="; ") %>% 
                dplyr::mutate(Model = paste0(Model, " (Fixed at: ",Cats, ")") ) 
            cat.groups = 'Model' 
        }else{
            cat.groups = 'cat.groups'
        } 
    }else{
        cat.groups=NULL
        cat.group=NULL
        if(multiple.models.flag) cat.groups ='Model'
    }
    
    ## main plot
    ## ---------
    g = ggplot2::ggplot(data) +
        ggplot2::theme_bw() 

    ## show points
    ## -----------
    if(show.points){
        if(!is.null(pch.col.cat)){
            if (is.null(legend.col.title)) legend.col.title = "Categories"
            pch.col.cat   = 'pch.col.cat'
            g = g + ggplot2::geom_point(data=data,ggplot2::aes_string(x= x, y= y, colour=pch.col.cat), size=2, alpha=.7, position=pch.pos) 
            if (names(pch.col.palette) == 'brewer')  {g = g + ggplot2::scale_colour_brewer(palette=pch.col.palette, name=legend.col.title)}
            if (names(pch.col.palette) == 'viridis') {g = g + viridis::scale_color_viridis(option=pch.col.palette, discrete=TRUE, alpha=.6, name=legend.col.title) }
                
        }else{
            g = g + ggplot2::geom_point(data=data,ggplot2::aes_string(x= x, y= y), colour=pch.col, size=2, position=pch.pos) 
        }
    }

    ## fitted values
    ## -------------
    g = g +
        ggplot2::geom_line(data=pred, ggplot2::aes_string(x= x, ".fitted", group=cat.groups)) +
        ggplot2::geom_ribbon(data=pred, ggplot2::aes_string(x = x, ymin=".ylb", ymax=".yub", group=cat.groups, fill=cat.groups), alpha=0.4)


    ## facets, legend, title
    ## ---------------------
    if(!is.null(facets))           g = g + ggplot2::facet_wrap(stats::as.formula(paste("~", paste0(facets, collapse="+") )), scales=facets.scales, ncol=facets.ncol) 
    if(!is.null(title))            g = g + ggplot2::labs(title = title) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = title.position))
    if(!is.null(subtitle))         g = g + ggplot2::labs(subtitle = subtitle)
    if(!is.null(footnote))         g = g + ggplot2::labs(caption = footnote)
    if(!is.null(legend.position))  g = g + ggplot2::theme(legend.position = legend.position) 
    if (!is.null(legend.title))    g = g + ggplot2::guides(colour=ggplot2::guide_legend(legend.title)) 
    if(legend.omit)                g = g + ggplot2::theme(legend.position = 'none') 

    ## facets appearance (strips)
    ## -----------------
    g = g + ggplot2::theme(strip.background = ggplot2::element_rect(colour="white", fill="white"),
                           strip.text.x = ggplot2::element_text(size=12, face='bold', hjust=facets.title.position),
                           strip.text.y = ggplot2::element_text(size=12, face="bold")) 

    ## x and y axis
    ## ------------
    if(!is.null(xlab)) g = g + ggplot2::xlab(xlab)
    if(!is.null(ylab)) g = g + ggplot2::ylab(ylab)
    if(!is.null(xlim)) g = g + ggplot2::xlim(xlim)
    if(!is.null(ylim)) g = g + ggplot2::ylim(ylim)

    if(legend.position %in% c("top", "bottom"))
        g = g + ggplot2::theme(legend.title = ggplot2::element_text(face = "bold"),
                               legend.direction = legend.direction, 
                               legend.box = legend.box,  ## arrangement of multiple legends ("horizontal" or "vertical")
                               legend.title.align = 1,
                               legend.justification = c(0, 0)) +
            ggplot2::guides(colour = ggplot2::guide_legend(title.position="top", title.hjust = 0, ncol = legend.ncol.colour),
                            fill   = ggplot2::guide_legend(title.position="top", title.hjust = 0, ncol = legend.ncol.fill  ))

    ## fill
    ## ----
    if(is.null(legend.fill.title)) legend.fill.title = 'Groups'
    if(is.null(legend.fill.title) & !is.null(cat.groups)) 
        if(cat.groups != 'Model') legend.fill.title = 'Groups'
    if (any(sapply(cat.values, length)>1) | "Model" %in% cat.groups ) {
        ## if ("Model" %in% cat.groups) {
        if (names(fill.palette) == 'brewer')  g = g + ggplot2::scale_fill_brewer(palette=fill.palette, name=legend.fill.title )
        ## if (names(fill.palette) == 'dutchmasters')  g = g + scale_fill_dutchmasters(palette=fill.palette, alpha=1)
        if (names(fill.palette) == 'viridis') g = g + viridis::scale_fill_viridis(option=fill.palette, discrete=TRUE, alpha=1, name=legend.fill.title )
        ## }else{    
        ##     g = g + viridis::scale_fill_viridis(option="viridis", discrete=TRUE, alpha=1)
        ## }
    }else{
        g = g + ggplot2::scale_fill_manual(values = rep(pch.col, length(cat.values))) +
            ggplot2::guides(fill  = FALSE)
            
    }
    return(g)

    
}

gge_fit_get_new_data <- function(data, facets=NULL, cat.values, n, x)
{
    if (!is.null(facets)) {
        if (length(facets)>1) {
            list.facets = data %>%
                dplyr::select(facets)  %>%
                dplyr::summarise_all( function(x) list(unique(as.character(x))) )  %>%
                base::apply(., 2, function(x) x  %>% unlist)
        }else{
            list.facets = data %>%
                dplyr::select(facets)  %>%
                dplyr::summarise_all( function(x) list(unique(as.character(x))) )  %>%
                dplyr::pull(.) %>%
                list %>%
                .[[1]] %>%
                stats::setNames(., facets)
        }
        if (is.null(cat.values)) {
            cat.values.newdata = list.facets
        }else{
            cat.values.newdata = append(cat.values, list.facets)
            groups = cat.values.newdata %>% 
                tibble::data_frame(cat=., var=names(.)) %>%
                dplyr::group_by(var) %>%
                dplyr::summarise(cat = list(cat %>% unlist  %>% unique) ) %>%
                dplyr::ungroup(.) 
            names = groups$var
            values = groups$cat
            cat.values.newdata = values
            names(cat.values.newdata) = names
        }
    }else{
        cat.values.newdata = cat.values
    }
    newdata = edar_get_new_data(data=data, n=n, x=x, cat.values = cat.values.newdata)
    return(newdata)
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
#' @param model an output of \code{lm}, \code{glm}, or \code{\link[nnet]{multinom}} functions. It can also be a list (named or not) with combinations of those objects
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
        results[[i]] <-data.frame(y.multin.cat = paste0('Category ',yj[i]) ,
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
## {{{ Tidy summary to formatted table }}}

## {{{ docs }}}

#' Scientific-like table for model summary
#'
#' This function produces a data frame with model summaries. The organization of the table resembles final format used in publications. 
#'
#' @param model an output of \code{lm}, \code{glm}, \code{\link[nnet]{multinom}} functions, or a tidy summary of those objects. It can also be a list (named or not) with combinations of outputs of \code{lm}, \code{glm}, \code{\link[nnet]{multinom}}, or a tidy summary of such list produced by tidye
#' @inheritParams tidye
#'
#' @examples
#' library(magrittr)
#' 
#' set.seed(77)
#' data = tibble::data_frame(
#'       n = 300,
#'       x1   = rnorm(n,3,1),
#'       x2   = rexp(n),
#'       cat1 = sample(c(0,1), n, replace=TRUE),
#'       cat2 = sample(letters[1:4], n, replace=TRUE),
#'       y    = -10*x1*cat1 + 10*x2*(3*(cat2=='a')
#'              -3*(cat2=='b') +1*(cat2=='c') -1*(cat2=='d')) + 
#'               rnorm(n,0,10), 
#'       y.bin = ifelse(y < mean(y), 0, 1),
#'       y.mul = 1+ifelse( - x1 - x2 + rnorm(n,sd=10) < 0, 0,
#'                  ifelse( - 2*x2 + rnorm(n,sd=10) < 0, 1, 2)),
#'        )
#' formula1    = y ~ x1
#' formula2    = y ~ x1 + x2
#' formula3    = y ~ x1*cat1 + x2*cat2
#' formula4bin = y.bin ~ x1+x2*cat2
#' formula5mul = y.mul ~ x1 + x2
#' model.g1    = lm(formula1, data)
#' model.g2    = lm(formula2, data)
#' model.g3    = lm(formula3, data)
#' model.bin   = glm(formula4bin, data=data, family='binomial')
#' model.mul   = nnet::multinom(formula5mul, data)
#' 
#' model.mul %>% tidye %>%                         etab
#' list(model.g1,model.g3) %>% tidye %>%           etab
#' list(model.g1,model.g2, model.g3) %>% tidye %>% etab
#' list(model.mul, model.g3) %>% tidye %>%         etab
#' model.g3%>% tidye%>%                            etab
#' 
#' model.mul %>%                          etab
#' list(model.g1,model.g3) %>%            etab
#' list(model.g1,model.g2, model.g3) %>%  etab
#' list(model.mul, model.g3) %>%          etab
#' model.g3%>%                            etab
#'
#' @export

## }}}
etab <- function(model, digits=4, hc=FALSE, hc.type=c("hc3", "hc0", "hc1", "hc2", "hc4"))
{
    if (!any(class(model)[1] %in% c('data.frame',"tbl_df","tbl" ))) model = model %>% tidye(., hc=hc, hc.type=hc.type[1])
        
    ids = model %>%  dplyr::select_if(function(col) !is.numeric(col)) %>% dplyr::select(-term)  %>% names
    if(length(ids) > 0)
    {
        model = model %>%
            tidyr::unite(model, sort(ids), sep=" ") %>%
            dplyr::mutate(model = stringr::str_replace(model, " NA", "")) 
        tab = model %>% 
            dplyr::select(model, term, estimate,  dplyr::contains("conf")) %>%
            dplyr::mutate_if(is.numeric, round, digits=digits) %>% 
            tidyr::unite(CI, conf.low, conf.high, sep=', ') %>%
            dplyr::mutate(CI = paste0("(",CI,")") ) %>%
            tidyr::gather(key = id, value=estimate, -term, -model)  %>%
            tidyr::spread(., key=model, value=estimate) %>%
            dplyr::arrange(term, dplyr::desc(id)) %>%  
            dplyr::mutate_if(is.factor, as.character)  %>% 
            dplyr::mutate(term = dplyr::case_when(id == "CI" ~"",
                                                  TRUE ~ term))  %>% 
            dplyr::rename(Covariate=term)  %>%
            dplyr::select(-id) 
        tabNA    = tab %>% dplyr::filter(!stats::complete.cases(.))
        tabnotNA = tab %>% dplyr::filter(stats::complete.cases(.))
        tab      = tabnotNA %>% dplyr::bind_rows(., tabNA)
    }else{
        tab = model %>% 
            dplyr::select(term, estimate,  dplyr::contains("conf")) %>%
            dplyr::mutate_if(is.numeric, round, digits=digits) %>% 
            tidyr::unite(CI, conf.low, conf.high, sep=', ') %>%
            dplyr::mutate(CI = paste0("(",CI,")") )  %>%
            tidyr::gather(key = id, value=estimate, -term)  %>%
            dplyr::arrange(term) %>%  
            dplyr::mutate_if(is.factor, as.character)  %>% 
            dplyr::mutate(term = dplyr::case_when(id == "CI" ~"",
                                                  TRUE ~ term))  %>% 
            dplyr::select(term, estimate)   %>%
            dplyr::rename(Covariate=term) 
    }
    tab[is.na(tab)] = ""
    return(tab)
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
## {{{ Interpolation }}}


## auxiliar to compute interpolation
compute_if_linear  <- function(v)
{
    if(sum(!is.na(v))>1){
        return(stats::approxfun(base::seq_along(v), v)(base::seq_along(v)))
    }else{
        return(v)
    }
}
compute_if_splines <- function(v, yr)
{
    if(sum(!is.na(v))>1){
        return( stats::spline(x = yr, y = v,  xout=yr)$y )
    }else{
        return(v)
    }
}
## {{{ docs }}}
#' Interpolation
#'
#' This function produces linear and spline interpolated values
#'
#' @param data a data frame to interpolate. It must contain at least the variables we want to interpolate and the \code{time.var}, which marks the intervals we want to fill with interpolated value.
#' @param vars.to.interpolate a string vector with the names of the variables to interpolate
#' @param group a string vector with the names of the grouping variables. The interpolation will be conducted within each group. Note: if you are using time.values and extending the current data set, only the variables specified in \code{group}, \code{vars.to.interpolate}, and \code{time.var} will be extended to fill the values in \code{time.values}. The other variables will receive \code{NA} for the extended cases. If the data set is extended using \code{time.values} parameter and there are two categorical variables that represent the same group used to interpolated in different ways, it is possible to pass that variable in the parameter \code{group} as well to avoid NA values in the extended data set that is returned (see examples) 
#' @param time.var a string with the name of the variable indicating the interval in which the measurements of the \code{vars.to.interpolate} were collected or are missing. Usually it represents time (years, months) for which there are some NA values in the variables described in \code{vars.to.interpolate}, which we want to substitute for interpolated values.
#' @param time.values either \code{NULL} (default) or a vector with values in the same unit of time.var. This range will be used to expand the data set and interpolate the values. If \code{NULL}, only \code{time.var} values are used to interpolate
#' @param extrapolate.spline boolean, if \code{TRUE} the values intepolated using the spline function will also return extrapolted values
#'
#' @details The function linear interpolated values in a variable sufixed with .ili (interplation, linear) and interpolated values using the default method of the \code{spline} function. Those values are stored in a value suffixed with .isp (interpolated, spline)
#'
#' @examples
#'
#' library(magrittr)
#' library(ggplot2)
#' dat = tibble::data_frame(cat2 = c("a1", "a1", "b1", "a1", "b1", "a1", "b1"),
#'                          cat = c("a", "a", "b", "a", "b", "a", "b"),
#'                          yr = c(1980, 1990, 1987, 1993, 1990, 1999, 1999),
#'                          value1 = c(NA, 1, 10, NA, NA, 50, NA),
#'                          value2 = c(2, NA, 1, 10, 5, NA, 100)) 
#' 
#' einterpolate(dat, vars.to.interpolate=c('value1'), group='cat', time.var='yr')
#' ## using pipe and interpolating multiple variables at once
#' dat %>% einterpolate(., vars.to.interpolate=c('value1', "value1"), group='cat',
#'                      time.var='yr')
#' 
#' ## extending the data set
#' dat  %>%  einterpolate(., vars.to.interpolate=c('value1', "value2"),
#'                        group=c('cat'), time.var='yr', time.values = 1980:1999)
#' 
#' ##  cat and cat2 are two ways to desctibe the same group. but in the previous code
#' ## cat2 returns NA for the extended values. To avoid this:
#' dat  %>%  einterpolate(., vars.to.interpolate=c('value1', "value2"),
#'                        group=c('cat', 'cat2'), time.var='yr', time.values = 1980:1999)
#' 
#' ## to extrapolate (only using spline)
#' dat  %>%  einterpolate(., vars.to.interpolate=c('value1', "value2"),
#'                        group=c('cat', 'cat2'), time.var='yr', time.values = 1980:1999,
#'                        extrapolate.spline=TRUE)
#' 
#' ## see it visually:
#' 
#' v = dat %>%
#'     einterpolate(., vars.to.interpolate=c('value1', "value2"),
#'                  group=c('cat', 'cat2'), time.var='yr',
#'                  time.values = 1980:1999, extrapolate=TRUE)
#' 
#' ## plot interpolated variable value2
#' v %>%
#'     dplyr::select(dplyr::contains("value2"), cat, yr)  %>%
#'     tidyr::gather(key = Method, value=value, -cat,  -yr) %>% 
#'     dplyr::mutate(labels =
#'             dplyr::case_when(
#'                    Method == "value2" ~ "Observed",
#'                    Method == "value2.ili" ~ "Interpolated (linear)",
#'                    Method == 'value2.isp' ~ 'Interpolated + Extrapolation (spline)') ) %>% 
#'     ggplot2::ggplot(.) +
#'     ggplot2::geom_point(aes(x=yr, y=value , colour=labels, size=labels), alpha=.3) +
#'     ggplot2::geom_line(aes(x=yr, y=value, group=labels, colour=labels)) +
#'     ggplot2::facet_wrap( ~ cat, ncol = , scales='free',labeller=label_parsed)  +
#'     ggplot2::scale_size_discrete(range=c(2,10), name='')+
#'     ggplot2::scale_colour_manual(
#'                 values = c("Observed"= "red",
#'                 "Interpolated (linear)" = "black",
#'                 "Interpolated + Extrapolation (spline)"="lightblue"), name='') +
#'     ggplot2::theme_bw()+
#'     ggplot2::theme(legend.position = "bottom") +
#'     ggplot2::ggtitle("Variable: Value 2")
#' @export
## }}}
einterpolate <- function(data, vars.to.interpolate, group=NULL, time.var=NULL, time.values=NULL, extrapolate.spline=FALSE)
{
    if (!is.null(time.values)) {
        ## create a in long format
        tab.long = data %>%
            dplyr::select(group, time.var, vars.to.interpolate)  %>%
            dplyr::filter(!duplicated(.)) %>%
            tidyr::gather(key = variable, value=value, -group, -time.var) 
        ## complete that tab with the years in the range time.values and convert to wide format
        tab = data %>%
            dplyr::select(group)  %>%
            dplyr::filter(!duplicated(.)) %>%
            tidyr::crossing(time.value = time.values) %>%
            dplyr::rename_at(dplyr::vars(time.value), ~ paste0(time.var) )  %>%
            tidyr::crossing( 
                data %>%
                dplyr::select(group, vars.to.interpolate,time.var)  %>% 
                tidyr::gather(key = variable, value=value, -group, -time.var ) %>% 
                dplyr::mutate_at(dplyr::vars(time.var), dplyr::funs(as.character)) %>% 
                dplyr::select_if(function(col) !is.numeric(col)) %>%
                dplyr::select(-group, -time.var)  %>% 
                dplyr::filter(!duplicated(.))
            ) %>%
            dplyr::left_join(., tab.long) %>%
            tidyr::spread(., key=variable, value=value) %>%
            dplyr::left_join(., data) 
    }else{
        tab = data
    }

    for (var in vars.to.interpolate)
    {
        tab = tab %>% 
            ## rename variable to facilitate manipulation in dplyr
            dplyr::rename(var__to__interpolate__ = !!var, time.var.__tmp = !!time.var) %>% 
            dplyr::group_by_at(dplyr::vars(dplyr::one_of(group))) %>% 
            ## dplyr::arrange_(group, "time.var.__tmp") %>% 
            dplyr::mutate(var__interpolated__.ili = compute_if_linear (var__to__interpolate__),
                          var__interpolated__.isp = compute_if_splines(var__to__interpolate__, time.var.__tmp),
                   ) %>%
            dplyr::ungroup(.) 
        if (!extrapolate.spline) tab = tab %>% dplyr::mutate(var__interpolated__.isp = dplyr::case_when(is.na(var__interpolated__.ili) ~ NA_real_, TRUE ~ var__interpolated__.isp))
        ## rename variable back to its original name
        tab = tab %>%
            dplyr::rename_at(dplyr::vars(var__to__interpolate__, var__interpolated__.ili, var__interpolated__.isp, time.var.__tmp),
                             dplyr::funs(c(var, paste0(var,".ili"), paste0(var, '.isp'), time.var   )) ) 
    }
    return(tab)
}





## }}}
