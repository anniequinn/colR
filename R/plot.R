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
    hexCols <- input$hex %>% levels
  }

  # Vector
  if(is.vector(input)) {

    # Unnamed
    if(is.null(names(input))) {

      dt <-
        tibble(hex = factor(input)) %>%
        mutate(i = 1:length(input))
      p <-
        dt %>%
        ggplot(aes(x = 1, y = 1:length(input),
                   fill = hex, label = hex)) +
        facet_grid(rows = vars(i), scales = "free", switch = "y")

    }

    # Named by type
    if(!is.null(names(input))) {

      dt <-
        tibble(hex = factor(input)) %>%
        mutate(type = names(input) %>% fct_inorder)

      p <-
        dt %>%
        ggplot(aes(x = 1, y = 1, fill = hex, label = hex)) +
        facet_grid(cols = vars(type), scales = "free", space = "free", switch = "y")
    }

    hexCols <- dt$hex %>% levels
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
