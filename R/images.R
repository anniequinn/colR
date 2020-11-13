colRpickr <- function(image, top_n = 10, web = FALSE) {

  if(str_ends(image, ".jpg|.jpeg")) { filetype <- ".jpg"}
  if(str_ends(image, ".png")) { filetype <- ".png"}

  if(web == TRUE) {

    dir <- file.path(tempdir(), paste0("colR", filetype))
    download.file(image, dir, mode = "wb")
    image <- dir

  }

  if(filetype == ".jpg") { t <- jpeg::readJPEG(image) }
  if(filetype == ".png") { t <- png::readPNG(image) }

  t <-
    t %>%
    as.raster %>%
    as.vector %>%
    table %>%
    as_tibble() %>%
    rename("col" = ".") %>%
    mutate(col = str_sub(col,1,7)) %>%
    group_by(col) %>%
    summarise(n = sum(n), .groups = "drop_last") %>%
    top_n(top_n, n) %>%
    arrange(desc(n))

  return(t)

}
