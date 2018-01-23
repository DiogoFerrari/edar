
## {{{ Simulate data sets }}}

edar_simulate_data <- function(sample_size, n_data_sets=1, n_replications=1, dgp){
    ## This function requites as input the dgp
    data_sets = tibble::data_frame()
    for (i in 1:n_data_sets){
        data_sets = data_sets %>%
            bind_rows(bind_cols(id_data_set=rep(i, n_replications), dgp(sample_size, n_replications)))
    }
    return(data_sets)
}


## }}}
