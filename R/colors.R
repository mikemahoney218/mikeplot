#' Nice-enough color palettes
#'
#' @param n The number of colours to generate for the palette
#' @param alpha The opacity of the generated colours. If specified rgba values
#' will be generated. The default (`NULL`) will generate rgb values which
#' corresponds to `alpha = 1`
#' @param direction Either `1` or `-1`. If `-1` the palette will be reversed
#' @param palette The name of the palette to sample from. See
#' [scico_palette_names()] for a list of possible names
mikeplot_discrete <- function(n, alpha = NULL, direction = 1, palette = "mikeplot1d") {

    if (abs(direction) != 1) {
      stop("direction must be 1 or -1")
    }

    cols <- palettes[[match.arg(palette, names(palettes))]]
    if (direction == -1) cols <- rev(cols)
    n_cols <- length(cols)
    if (n > n_cols) {
      rlang::abort(
        glue::glue("Palette {palette} only has {n_cols} colors, but {n} were requested.")
      )
    }


    if (is.null(alpha)) {
      cols
    } else {
      cols <- grDevices::col2rgb(cols)
      rgb(cols[1, ], cols[2, ], cols[3, ], alpha = alpha * 255, maxColorValue = 255)
    }
  }


palettes <- list(
  mikeplot1d = c("#7b4485", "#5e80ae", "#efdb9e", "#82b065", "#dda665", "#54a1a9")
)
