
## {{{ multiple inputation }}}



## }}}

## {{{ post-stratification weights }}}




## }}}

## {{{ power and sample size }}}

ds_power <- function(mu1=NULL,mu2=NULL, power_ideal=.8, N_current=NULL, N1_current=NULL, N2_current=NULL, title = NULL, lines=T){
    
    ESs  = ES.h(p1 = mu1, p2 = mu2) ## d: expected effect size over the pooled expected standard deviation
    power=list()
    n_ideal=c()
    power_current=c()

    for (i in 1:length(ESs)){
        power_current[i] = pwr.2p.test(h=ESs[i], n=N_current, alternative='two.sided')$power
        n_ideal[i]       = pwr.2p.test(h=ESs[i], power=power_ideal, alternative='two.sided')$n
        n_max            = ifelse(power_current[i] >1, N_current, pwr.2p.test(h=ESs[i], power=.99, alternative='two.sided')$n)
        N                = seq(1,n_max, length=20)
        power[[i]] = cbind(power=pwr.2p.test(h=ESs[i], n=N, alternative='two.sided')$power, n = N)
    }
    ES = tibble::data_frame(mu1 = mu1, mu2=mu2, ES = ESs, power_ideal=power_ideal, n_ideal=n_ideal, power_current=power_current, n_current=N_current, n_and_power=power)

    layout = .get_layout(length(ES$ES))
    par(mfrow=c(layout))
    if (!is.null(title)){
        par(mar=c(4, 4, 6, 1))
    }else{
        par(mar=c(4, 4, 3, 1) )
    }
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mgp = c(2,.6,0))
    for (i in 1:length(ESs)){
        plot(x=ES$n_and_power[[i]][,'n'], ES$n_and_power[[i]][,'power'], type='l', col="#00000044", pch=20, cex=2, xlab='Sample Size', ylab='Power', ylim=c(0,1))
        points(x=N_current, y=power_current[i], col='red', pch=20, cex=3)
        points(x=ES$n_ideal[i], y=ES$power_ideal[i], col='blue', pch=20, cex=3)
        text_current = paste0('Current Design\nN=',N_current,'\nPower=',round(power_current[i],2))
        text_ideal   = paste0('Ideal Design\nN=',ceiling(ES$n_ideal[i]),'\nPower=',round(ES$power_ideal[i],2))
        ## text(x=ES$n_ideal[i], y=ES$power_ideal[i],col='blue', pch=1, cex=.8, labels=text_ideal, pos=4,offset=2)
        ## text(x=N_current, y=power_current[i], col='red', pch=1, cex=.8, labels=text_current, pos=4,offset=2)
        title(paste0('Effect size : ', round(ES$ES[i],2),  sep=''), cex.main=1.2)
        if (lines){
            ## abline(v=ES$n_ideal[i], lty=2, col='blue')
            ## abline(v=N_current, lty=2, col='red')
            segments(x0=ES$n_ideal[i], y0=0,y1=ES$power_ideal[i], lty=2, col="blue")
            segments(x0=0,x1=ES$n_ideal[i], y0=ES$power_ideal[i], lty=2, col="blue")
            segments(x0=N_current, y0=0,y1=power_current[i], lty=2, col="red")
            segments(x0=0,x1=N_current, y0=power_current[i], lty=2, col="red")
        }
        text_current = paste0('Current Design: N=',N_current,'  Power=',round(power_current[i],2))
        text_ideal   = paste0('Ideal Design    : N=',ceiling(ES$n_ideal[i]),'  Power=',round(ES$power_ideal[i],2))
        legend('bottomright', legend=c(text_ideal, text_current), pch=c(20,20), col=c('blue','red'), bty='n', cex=1.2)
    }
     if (!is.null(title)) mtext(title, line=-2, outer=T)
    return(ES)
}


## }}}
