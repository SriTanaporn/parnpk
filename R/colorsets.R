
#' colorsets
#'
#' @description
#' Function shows display and hex codes of my color theme. My theme contains 7 sub-theme including
#' formal, main, cool, hot, mixed and grey.
#'
#' @param col = color sub-theme in my theme that users want to look their display and hex codes .
#' The defaults shows the total colors in my theme.
#'
#' @examples
#' colorsets()
#' colorsets("formal")
#' @export

colorsets = function(col = "total") {
  set1<- c(
    `red`        = "#bb1414",
    `green`      = "#146b3a",
    `blue`       = "#06aed5",
    `darkblue`   = "#086788",
    `orange`     = "#f46f00",
    `yellow`     = "#f0c808",
    `light grey` = "#e5e5e5",
    `dark grey`  = "#8c8c8c")

  colorset <- function(...) {
    cols <- c(...)
    if (is.null(cols))
      return (set1)
    set1[cols]
  }

  parn_palettes <- list(
    `formal` = colorset("darkblue", "blue","light grey","red"),
    `main`  = colorset("blue", "green", "yellow"),
    `cool`  = colorset("blue", "green"),
    `hot`   = colorset("yellow", "orange", "red"),
    `mixed` = colorset("blue", "green", "yellow", "orange", "red"),
    `grey`  = colorset("light grey", "dark grey")
  )

  a=0
  if (col == "formal" ){
    a =1
  } else if (col == "main"){
    a =2
  } else if (col == "cool"){
    a=3
  } else if (col == "hot"){
    a=4
  } else if (col == "mixed"){
    a = 5
  } else if (col == "grey"){
    a= 6
  } else if (col == "total"){
    a= 0
  }
  if (a != 0) {
    print(parn_palettes[[a]])
    show_col(parn_palettes[[a]]) #package(scales)
  } else {
    print(set1)
    show_col(set1)
  }
}

