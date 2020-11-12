# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

colRcvd <- function(hexCol, cvd = NULL) {

  colRcvd_internal <- function(hexCol, cvd) {

    if(!sum(str_detect(str_to_lower(cvd), c("deu", "pro", "tri"))) >= 1) {

      stop("cvd not recognised")

    }

    require(tidyverse); require(colorspace); require(farver)

    if(str_detect(str_to_lower(cvd), "deu")) { cvdCS <- "deutanomaly" } # "Deuteranopia"
    if(str_detect(str_to_lower(cvd), "pro")) { cvdCS <- "protanomaly" } # "Protanopia"
    if(str_detect(str_to_lower(cvd), "tri")) { cvdCS <- "tritanomaly" } # "Tritanopia"

    tMatrix <- getFromNamespace(paste0(cvdCS, "_cvd"), "colorspace")[[11]]

    rgb <- farver::decode_colour(hexCol) %>% t()
    rgb <- tMatrix %*% rgb
    rgb[rgb < 0] <- 0
    rgb[rgb > 255] <- 255
    hexColNew <- farver::encode_colour(rgb %>% t)

    return(hexColNew)

  }

  if(is.null(cvd)) {
    output <- sapply(c("deu", "pro", "tri"), function(x) { matrix(colRcvd_internal(hexCol, x)) })
    if(length(hexCol) == 1) { output <- c(original = hexCol, output) }
    if(length(hexCol) > 1) { output <- cbind(original = hexCol, output) }
    }
  if(!is.null(cvd)) { output <- colRcvd_internal(hexCol, cvd) }

  return(output)

}

colRplot <- function(input) {

  require(tidyverse)

  # Tibble - Type, hex and i
  if(is_tibble(input)) {

    if(sum(names(input) %in% c("type", "hex", "i")) != 3) {

      stop("Tibble column names do not match; set names as type, hex and i")

    }

    input <-
      input %>%
      mutate(type = fct_inorder(type), hex = fct_inorder(hex))

    p <-
      input %>%
      ggplot(aes(x = type, y = i, fill = hex, label = hex)) +
      facet_grid(cols = vars(type),
                 rows = vars(i),
                 scales = "free", space = "free", switch = "y")
    hexCols <- input$hex %>% as.vector
  }

  # Vector
  if(is.vector(input)) {

    # Unnamed
    if(is.null(names(input))) {

      dt <-
        tibble(hex = input) %>%
        mutate(hex = hex %>% fct_inorder,
               i = 1:length(input))
      p <-
        dt %>%
        ggplot(aes(x = 1, y = 1:length(input),
                   fill = hex, label = hex)) +
        facet_grid(rows = vars(i), scales = "free", switch = "y")

    }

    # Named by type
    if(!is.null(names(input))) {

      dt <-
        tibble(hex = input) %>%
        mutate(type = names(input) %>% fct_inorder,
               hex = hex %>% fct_inorder)

      p <-
        dt %>%
        ggplot(aes(x = 1, y = 1, fill = hex, label = hex)) +
        facet_grid(cols = vars(type), scales = "free", space = "free", switch = "y")
    }

    hexCols <- dt$hex %>% as.vector
  }

  # Create plot & format
  p +

    geom_tile(show.legend = FALSE, colour = "white") +
    geom_text(colour = "black") +

    scale_fill_manual(values = hexCols) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +

    theme_minimal() +
    theme(panel.spacing = unit(0, "cm"),
          strip.background = element_rect(fill = "white", colour = "white"),
          strip.text = element_text(colour = "black", face = "bold"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())

}

colRtidy <- function(colRcvd) {

  colRcvd %>%
    as_tibble() %>%
    mutate(i = 1:n()) %>%
    gather("type", "hex", -i)

}
