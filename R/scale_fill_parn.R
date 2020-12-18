#' scale_fill_parn
#'
#' Color theme function is used with graph created by ggplots. This function is apply for ggplot2 with color scale function.
#' (ggplot with color function, use another one called scale_color_parn). My color theme has contains 7 sub-theme including formal, main, cool, hot, mixed and grey.
#'
#' @param palette my color sub-theme in my theme (use colorsets function to explore more details)
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' (i.e. if the variable is not discrete, we should apply color gradient scale so discrete = FALSE)
#' @param reverse  Boolean indicating whether the palette should be reversed
#' @param col color sub-theme in my theme that users want to look their display and hex codes .
#' The defaults shows the total colors in my theme.
#' @return
#' @export
#'
#'
#' @examples
#' data(iris)
#'
#' ggplot(mpg, aes(y = class)) + geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE), alpha = 0.8) + scale_fill_parn("hot")
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar() + scale_fill_parn("formal")

scale_fill_parn <- function(palette = "formal", discrete = TRUE, reverse = FALSE, ...) {
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

  parn_pal <- function(palette = "formal", reverse = FALSE, ...) {
    pal <- parn_palettes[[palette]]
    if (reverse) pal <- rev(pal)
    colorRampPalette(pal, ...)
  }
  pal <- parn_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("parn_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
