
#' sigt
#'
#' @description calculate t statistics given variable and show criticals t-statistic.
#' @param x observed value
#' @param a hyptosis value
#' @param se standard error
#' @param df degree of freedom
#' @param alpha confidence level
#' @param two = TRUE if it is a two-tailed test and FALSE it is a one-tailed test
#'
#' @export
#' @examples sigt(100,99,1.5,600)
#'
#'
sigt = function(x, a, se,df,alpha = 0.05, two = TRUE) {
  result = matrix( nrow=3, ncol=1)
  if (two ==1) {
    result[1,] = x
    result[2,] = (x-a)/(se)
    result[3,] = qt((1-alpha/2),df =df)
  } else {
    result[1,] = x
    result[2,] = (x-a)/(se)
    result[3,] = qt((1-alpha/2),df =df)
  }
  colnames(result) = c('value')
  rownames(result) = c("value", "tstatistics", "critical value")
  return(result)
}


