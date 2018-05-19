
## {{{ docs }}}
#' Plot bivariate gaussian distribution
#'
#' This functions generates a contour plot of a bivariate gaussian distribution 
#'
#'
#' @param mu a two-dimensional numerical vector with the mean parameters of the bivariate gaussian distribution
#' @param Sigma a 2x2 covariance matrix 
#'
#'
#' @export
## }}}
plot_mvgaussian   <- function(mu=c(0,0), Sigma=diag(2)){
    x1 = seq(mu[2]-4,mu[1]+4,length=100)
    x2 = seq(mu[2]-4,mu[1]+4,length=100)
    dNorm = base::outer(x1,x2, FUN=function(x,y) apply(cbind(x,y), 1, function(x) 
        mvtnorm::dmvnorm(x, mean=mu, sigma=Sigma)) )
    graphics::par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,4,3,1), mgp = c(2,.6,0))
    plot3D::contour2D(dNorm, x1,x2,colkey = FALSE, ylab="x1", xlab="x2")
}
## {{{ docs }}}
#' 3D gaussian distribution
#'
#' @inheritParams plot_mvgaussian
#'
#' @export
## }}}
plot_mvgaussian3D <- function(mu=c(0,0), Sigma=diag(2)){
    x1 = seq(-4,4,length=100)
    x2 = seq(-4,4,length=100)
    dNorm = outer(x1,x2, FUN=function(x,y) apply(cbind(x,y), 1, function(x) mvtnorm::dmvnorm(x, mean=mu, sigma=Sigma)) )
    graphics::par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,4,3,1), mgp = c(2,.6,0))
    plot3D::persp3D(x1, x2, dNorm, theta=10, phi=30, expand=0.8 ,shade = 0.03, colkey = FALSE, xlab="x1", ylab='x2', zlab='density')
}
