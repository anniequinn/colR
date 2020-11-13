colRtidy <- function(colR) {

  colR %>%
    as_tibble() %>%
    mutate(i = 1:n()) %>%
    gather("type", "hex", -i)

}
