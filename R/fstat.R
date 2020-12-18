
#' fstat
#'
#' @param sser SSE of restricted value
#' @param sseu SSE of unrestricted value
#' @param j hypothesis test
#' @param n total observation
#' @param k number of coefficent
#' @examples fvalue(sser = 200,sseu = 150,j = 2,n = 320,k =5)
#' @export

fstat= function(sser,sseu,j,n,k) {
  f = ((sser-sseu)/(j))/(sseu/(n-k))
  return(f)
}
