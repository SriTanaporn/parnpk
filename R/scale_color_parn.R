
#' scale_color_parn
#'
#' Color theme function is used with graph created by ggplots. This function is apply for ggplot2 with color scale function.
#' (ggplot with fill function, use another one called scale_fill_parn). My color theme has contains 7 sub-theme including formal, main, cool, hot, mixed and grey.
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
#' @examples
#' data(iris)
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point(size = 4) +
#' scale_color_parn(palette = "formal")
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#' geom_point(size = 4, alpha = .6) +
#' scale_color_parn(discrete = FALSE, palette = "mixed")

scale_color_parn <- function(palette = "formal", discrete = TRUE, reverse = FALSE, ...) {
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
    discrete_scale("colour", paste0("parn_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}
