
#' dshade
#' @description
#' shade function are used with curve package for shading a specific area of t distribution.
#' This will be beneficial when users want to show rejection region (critical area) or accepting area in density curve with given degree of freedom.
#' @param from the starting point for shading (t statistics ex specific number or value calculated by using qnorm)
#' @param to the ending point for shading
#' @param col color of shaded areas
#' @export
#' @examples
#' curve(dt(x,49),xlim=c(-5,5),ylim=c(0,0.4),ylab="Density")
#' dshade(from=-4, to=qnorm(0.025))
#' dshade(from= qnorm(0.975), to= 4)
#' dshade(from= qnorm(0.025), to= qnorm(0.975), col = "whitesmoke")
#' abline(v = 0.567, col="red", lwd=1, lty=2)
#' text(0.567, 0.3 , "t =  0.567 ", pos = 2, srt = 90,cex =0.7)
#'
#'
#' curve(dt(x,50),xlim=c(-5,5),ylim=c(0,0.4),ylab="Density", xlab = "t")
#' dshade(from = qnorm(0.01), to= 4)
#' text(4, 0.3 , "Accept H0: \n t > -2.403 ", pos = 2 ,cex =0.7)

#'
#'
#' x_axis_labels <- c(-2.009,-0.57,0,0.57,2.009)
#' curve(dt(x,49),xlim=c(-5,5),ylim=c(0,0.4),ylab="Density")
#' dshade(from=-4, to=qnorm(0.2866513))
#' dshade(from= qnorm(1-0.2866513), to= 4)
#' dshade(from= qnorm(0.2866513), to= qnorm(1-0.2866513) , col = "whitesmoke")
#' dshade(from=-4, to=qnorm(0.025), col = "#FF000033")
#' dshade(from= qnorm(0.975), to = 4,  col ="#FF000033")
#' text(3, 0.3 , "p value \n= 0.286 ", pos = 2, ,cex =0.7)
#' text(-1, 0.3 , "p value \n= 0.286 ", pos = 2, ,cex =0.7)
#'
#'
#'
#'
dshade <- function(from, to, ..., col="steelblue"){
  y_seq <- seq(from, to, length.out=500)
  d <- c(0, dnorm(y_seq, ...), 0)
  polygon(c(from, y_seq, to), d, col=col, density=NULL)
}





