colRcvd <- function(hexCol, cvd = NULL) {

  if(is.null(cvd)) {
    output <- sapply(c("deu", "pro", "tri"), function(x) { matrix(colRcvd_internal(hexCol, x)) })
    if(length(hexCol) == 1) { output <- c(original = hexCol, output) }
    if(length(hexCol) > 1) { output <- cbind(original = hexCol, output) }
  }
  if(!is.null(cvd)) { output <- .colRcvd(hexCol, cvd) }

  return(output)

}

.colRcvd <- function(hexCol, cvd) {

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
