
## general
## -------
.edar_select_numerical   <- function(data){
    idxChar <- sapply(sapply(data, class), function(x) x[[1]]) == 'factor' |
        sapply(sapply(data, class), function(x) x[[1]]) == 'character'  |
        sapply(sapply(data, class), function(x) x[[1]]) == 'ordered'
    return(data.frame(data[,!idxChar]))
}
.edar_select_categorical <- function(data){
    idxChar <- sapply(sapply(data, class), function(x) x[[1]]) == 'factor' |
        sapply(sapply(data, class), function(x) x[[1]]) == 'character'  |
        sapply(sapply(data, class), function(x) x[[1]]) == 'ordered'
    return(data.frame(data[,idxChar]))
}
.edar_multi_ggplot       <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

    ## Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)
    
    ## If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        ## Make the panel
        ## ncol: Number of columns of plots
        ## nrow: Number of rows needed, calculated from ## of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        ## Set up the page
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
        
        ## Make each plot, in the correct location
        for (i in 1:numPlots) {
            ## Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
.edar_get_layout         <-function(n, by.col=T){
    if (by.col)
    {
        nlines  = floor(sqrt(n))
        ncols   = ceiling(sqrt(n))
        m=ifelse(n - nlines*ncols>0, 1,0)
        return(c(nlines=nlines+m, ncols=ncols))
    }else
    {
        ncols  = floor(sqrt(n))
        nlines = ceiling(sqrt(n))
        m      = ifelse(n - nlines*ncols>0, 1,0)
        return(c(nlines=nlines, ncols=ncols+m))
    }
}


## regression
## ----------
edar_sig                       <- function(pvalues){
    significance                <- rep(' ', times=length(pvalues))
    significance[pvalues<0.1]   <- '.'
    significance[pvalues<0.05]  <- '*'
    significance[pvalues<0.01]  <- '**'
    significance[pvalues<0.001] <- '***'
    return(significance)
    
}
edar_get_new_data_cat_vars     <- function(model, n){
    dat = model$model
    cat.vars = names( which(attr(attr(model$model, 'terms'), 'dataClasses') =='factor') ) 
    cat.vars.tbl = data.frame(id=NA)
    for (i in 1:length(cat.vars)){
        cat.vars.tbl = cbind(cat.vars.tbl, levels(dat[,cat.vars[i]])[1])
    }
    cat.vars.tbl = cat.vars.tbl[,-1]
    print(cat.vars.tbl)
    names(cat.vars.tbl ) =  cat.vars
    cat.vars.tbl = cat.vars.tbl[rep(1, n), ] 
    return(cat.vars.tbl)
}
edar_get_new_data_num_vars     <- function(model, n, variable){
    dat = model$model
    num.vars = names( which(attr(attr(model$model, 'terms'), 'dataClasses') =='numeric') ) 
    num.vars.tbl= model$model %>%
        dplyr::select( num.vars ) %>%
        dplyr::select_( paste("-", variable) ) %>%
        dplyr::mutate_all(dplyr::funs(mean)) %>%
        dplyr::filter(dplyr::row_number()==1) %>%
        .[rep(1, n), ] %>%
        tibble::as_data_frame() %>%
        stats::setNames(num.vars[num.vars != variable])
    return(num.vars.tbl)
}
edar_get_predicted_lm          <- function(model, variable=NULL, new_data=NULL){
    if(is.null(variable)){
        if(is.null(new_data)){
            return(broom::augment( model ))
        }
        else{
            return( broom::augment( model, newdata=new_data ) )
        }
    }else{
        ## new data set
        n = 200
        new_data = edar_get_new_data(n,x=variable)
        return(broom::augment( model, newdata=new_data))
    }
}
.edar_get_new_data              <- function(data, x, n=200){
    x.min = min(data[,x])
    x.max = max(data[,x])
     new_data = data %>%
        dplyr::summarize_if(is.numeric, base::mean) %>%
        dplyr::bind_cols(data %>% dplyr::summarize_if(is.factor, function(x) levels(x)[1] ) ) %>%
        dplyr::bind_rows(rep(list(.),n-1))
     new_data[,x] = seq(x.min, x.max,length=n)
    return(new_data)
}


## Colors for Plot
## ---------------
addalpha <- function(colors, alpha=1.0) {
    r <- grDevices::col2rgb(colors, alpha=T)
                                        # Apply alpha
    r[4,] <- alpha*255
    r <- r/255.0
    return(grDevices::rgb(r[1,], r[2,], r[3,], r[4,]))
}
colorRampPaletteAlpha <- function(colors, n=32, interpolate='linear') {
                                        # Create the color ramp normally
    cr <- grDevices::colorRampPalette(colors, interpolate=interpolate)(n)
                                        # Find the alpha channel
    a <- grDevices::col2rgb(colors, alpha=T)[4,]
                                        # Interpolate
    if (interpolate=='linear') {
        l <- stats::approx(a, n=n)
    } else {
        l <- stats::spline(a, n=n)
    }
    l$y[l$y > 255] <- 255 # Clamp if spline is > 255
    cr <- addalpha(cr, l$y/255.0)
    return(cr)
}

## Pltos
## -----
## {{{ docs }}}

#' Reorder labels 
#'
#' This function reorder labels in a ggplot2 barplot figure with facets (see examples)
#'
#'
#' @param x the name of the variable that need to be ordered
#' @param by the name of the variable that will be used to order \code{x}
#' @param within the name of the variable used to create the facets
#' @param fun (not used)
#' @param sep default variable to manipulate the labels. Default "__"
#' @param ... additional parameter...
#'
#'
#' @export

## }}}
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

## {{{ docs }}}
#'
#' Reorder labels in ggplot
#'
#' Function used in conjunction with \link{reorder_within} to reorder labels based on a numerical variable in plots with facets
#'
#' @inheritParams reorder_within
#'
#' @export
## }}}
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}
## {{{ docs }}}
#'
#' Reorder labels in ggplot
#'
#' Function used in conjunction with \link{reorder_within} to reorder labels based on a numerical variable in plots with facets
#'
#' @inheritParams reorder_within
#'
#' @export
## }}}
scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}



## get fitted values from summary tables
## -------------------------------------
get_matrix_with_factors <- function(data, cat.values, n)
{
    factors = data %>%
        dplyr::select(intersect(cat.vars, all.vars(formula)))  %>% 
        dplyr::filter(!duplicated(.)) %>% 
        dplyr::mutate_all(dplyr::funs(as.character)) %>%
        tidyr::gather(key = variable, value=value)  %>% 
        dplyr::filter(!duplicated(.)) %>% 
        tidyr::unite(., col=cat, sep="", remove=TRUE)  %>%
        dplyr::mutate(value = 0)  %>% 
        tidyr::spread(., key=cat, value=value) %>%
        .[rep(1, n),]
    if (!is.null(cat.values)) {
        for (i in 1:length(cat.values))
        {
            variable  = paste0(names(cat.values)[i], cat.values[[i]]) 
            factors = factors %>% dplyr::rename(variable = !!variable)  %>%  dplyr::mutate(variable = 1 %>% as.integer) %>% dplyr::rename_at(vars(variable), dplyr::funs(paste0(variable) ) ) 
        }
    }
    return(factors)
}
gnd <- function(data, n, x, cat.values=NULL)
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

## =====================================================
## For mediation package
## =====================================================
med2df <- function (x)
{ 
    clp <- 100 * x$conf.level
    isLinear.y <- ((class(x$model.y)[1] %in% c("lm", "rq")) || 
                   (inherits(x$model.y, "glm") && x$model.y$family$family == 
                    "gaussian" && x$model.y$family$link == "identity") || 
                   (inherits(x$model.y, "survreg") && x$model.y$dist == 
                    "gaussian"))

    printone <- !x$INT && isLinear.y

    if (printone) {

        smat <- c(x$d1, x$d1.ci, x$d1.p)
        smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
        smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
        smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))

        rownames(smat) <- c("ACME", "ADE", "Total Effect", "Prop. Mediated")

    } else {
        smat <- c(x$d0, x$d0.ci, x$d0.p)
        smat <- rbind(smat, c(x$d1, x$d1.ci, x$d1.p))
        smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
        smat <- rbind(smat, c(x$z1, x$z1.ci, x$z1.p))
        smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
        smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
        smat <- rbind(smat, c(x$n1, x$n1.ci, x$n1.p))
        smat <- rbind(smat, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
        smat <- rbind(smat, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
        smat <- rbind(smat, c(x$n.avg, x$n.avg.ci, x$n.avg.p))

        rownames(smat) <- c("ACME (control)", "ACME (treated)", 
                            "ADE (control)", "ADE (treated)", "Total Effect", 
                            "Prop. Mediated (control)", "Prop. Mediated (treated)", 
                            "ACME (average)", "ADE (average)", "Prop. Mediated (average)")

    }

    colnames(smat) <- c("Estimate", paste(clp, "% CI Lower", sep = ""), 
                        paste(clp, "% CI Upper", sep = ""), "p-value")
    smat
}

