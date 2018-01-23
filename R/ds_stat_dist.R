
plotMVGaussianContour <- function(mu=c(0,0), Sigma=diag(2)){
    x1 = seq(mu[2]-4,mu[1]+4,length=100)
    x2 = seq(mu[2]-4,mu[1]+4,length=100)
    dNorm = outer(x1,x2, FUN=function(x,y) apply(cbind(x,y), 1, function(x) dmvnorm(x, mean=mu, sigma=Sigma)) )
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,4,3,1), mgp = c(2,.6,0))
    contour2D(dNorm, x1,x2,colkey = FALSE, ylab="x1", xlab="x2")
}
plotMVGaussian3D <- function(mu=c(0,0), Sigma=diag(2)){
    x1 = seq(-4,4,length=100)
    x2 = seq(-4,4,length=100)
    dNorm = outer(x1,x2, FUN=function(x,y) apply(cbind(x,y), 1, function(x) dmvnorm(x, mean=mu, sigma=Sigma)) )
    par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,4,3,1), mgp = c(2,.6,0))
    persp3D(x1, x2, dNorm, theta=10, phi=30, expand=0.8 ,shade = 0.03, colkey = FALSE, xlab="x1", ylab='x2', zlab='density')
}
