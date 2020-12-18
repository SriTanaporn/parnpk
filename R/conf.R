
#' Conf
#'
#' @description calculate confidence interval
#' @param x = value(mean)
#' @param se =  standard error
#' @param alpha = confidence level
#' @param df = degree of freedom
#' @param two = TRUE if it is a two-tailed test and FALSE it is a one-tailed test
#'
#' @examples
#' conf(20, 1.5, alpha = 0.025, df = 500, two = FALSE)
#' @export
#'
conf =  function(x,se ,alpha = 0.05,df, two = TRUE) {
  result = matrix( nrow=3, ncol=1)
  if (two ==1) {
    result[1,] = x
    result[2,] = (x - (qt((1-alpha/2),df =df)*se)/sqrt(df))
    result[3,] = (x + (qt((1-alpha/2),df =df)*se)/sqrt(df))
  } else {
    result[1,] =  x
    result[2,] = (x - (qt(1-alpha,df =df)*se)/sqrt(df))
    result[3,] = (x + (qt((1-alpha),df =df)*se)/sqrt(df))
  }

  colnames(result) = c('value')
  rownames(result) = c("mean", "lower bound", "upper bound")
  return(result)
}


