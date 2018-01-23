# required (by devtools) to link the cpp code 
#' @useDynLib edar
#' @importFrom Rcpp sourceCpp
NULL


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
    if (!require("grid")) install.packages('grid', repos='http://cran.us.r-project.org'); library(grid)


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
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        ## Make each plot, in the correct location
        for (i in 1:numPlots) {
            ## Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
.edar_get_layout         <-function(n, bycol=T){
    if(bycol){
        ncols = ceiling(sqrt(n))
        nlines  = floor(sqrt(n))
        m=ifelse(n - nlines*ncols>0, 1,0)
        grid=c(nlines=nlines+m, ncols=ncolsm)
    }else{
        nlines = ceiling(sqrt(n))
        ncols  = floor(sqrt(n))
        m=ifelse(n - nlines*ncols>0, 1,0)
        grid=c(nlines=nlines, ncols=ncols+m)
    }
    return(grid)
}


## regression
## ----------
.edar_sig                       <- function(pvalues){
    significance                <- rep(' ', times=length(pvalues))
    significance[pvalues<0.1]   <- '.'
    significance[pvalues<0.05]  <- '*'
    significance[pvalues<0.01]  <- '**'
    significance[pvalues<0.001] <- '***'
    return(significance)
    
}
.edar_get_new_data_cat_vars     <- function(model, n){
    dat = model$model
    cat.vars = names( which(attr(attr(model$model, 'terms'), 'dataClasses') =='factor') ) 
    cat.vars.tbl = data.frame(id=NA)
    for (i in 1:length(cat.vars)){
        cat.vars.tbl = cbind(cat.vars.tbl, levels(dat[,cat.vars[i]])[1])
    }
    cat.vars.tbl = cat.vars.tbl[,-1]
    names(cat.vars.tbl ) =  cat.vars
    cat.vars.tbl = cat.vars.tbl[rep(1, n), ] 
    return(cat.vars.tbl)
}
.edar_get_new_data_num_vars     <- function(model, n, variable){
    dat = model$model
    num.vars = names( which(attr(attr(model$model, 'terms'), 'dataClasses') =='numeric') ) 
    num.vars.tbl= model$model %>%
        dplyr::select( num.vars ) %>%
        dplyr::select_( paste("-", variable) ) %>%
        dplyr::mutate_all(dplyr::funs(mean)) %>%
        dplyr::filter(row_number()==1) %>%
        .[rep(1, n), ] %>%
        tibble::as_data_frame() %>%
        setNames(num.vars[num.vars != variable])
    return(num.vars.tbl)
}
.edar_get_new_data              <- function(model, n, x){
    cat_vars = edar_get_new_data_cat_vars(model,n)
    num_vars = edar_get_new_data_num_vars(model, n, x)
    new_var = dplyr::data_frame( seq(min(model.matrix(model)[,x]),
                                     max(model.matrix(model)[,x]), length=n )) %>% setNames(x)
    response = setdiff(attr(attr(attr(model$model, "terms"),"dataClasses"), 'names'),
                       attr(attr(model$model, "terms"),"term.labels") )
    new_data = cbind(num_vars, cat_vars, new_var)
    new_data = new_data[,names(new_data)!=response]
    row.names(new_data) = NULL
    return(new_data)
}
.edar_get_predicted_lm          <- function(model, variable=NULL, new_data=NULL){
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
.edar_get_new_data_new          <- function(data, x, n=200){
    x.min = min(data[,x])
    x.max = max(data[,x])
     new_data = data %>%
        dplyr::summarize_if(is.numeric, mean) %>%
        dplyr::bind_cols(data %>% dplyr::summarize_if(is.factor, function(x) levels(x)[1] ) ) %>%
        dplyr::bind_rows(rep(list(.),n-1))
     new_data[,x] = seq(x.min, x.max,length=n)
    return(new_data)
}
