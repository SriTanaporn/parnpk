#' plots
#'
#' @description This function allows user to plot the fitted value or the forecast from diffrent model compared to the observed data.
#' The graphs apply ggplot2, so the user can add other option which applied to ggplot to the result from this function
#' such as ggtitle.
#'
#' @param date serie
#' @param serie observed data
#' @param dffit datafrome containing the fitted valued from different models
#' @export
#' @examples Using economics dataset from ggplot
#' #prepare data
#' pop = ts(economics$pop, start = c(1967,7),freq = 12)
#' date = seq(1967.50,2015.25,length=length(pop))
#' dffit = data.frame(
#' fitm1  = tslm(pop~trend)$fitted,
#' fitm2  = exp(tslm(log(pop) ~ trend)$fitted))
#'
#' #apply function
#' re = plots(date = date,serie = pop, dffit = dffit)
#' re +  ggtitle("Time series Trend graph using differnet Models")

plots = function(date, serie, dffit) {
#package(reshape2)
  aa = data.frame(time = date,
                  observed_data = pop)
  aa = cbind(aa,dffit)
  dfg = reshape2::melt(aa,"time")
  result = dfg %>%
    ggplot(aes(x = time , y = value, colour = variable)) + geom_line()
  return(result)
}

