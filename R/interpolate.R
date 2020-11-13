.colRpalBezier <- function(t, p) {

  lt <- length(t)
  pd <- dim(p)
  deg <- pd[1] - 1
  seg <- matrix(c(1, deg + 1), nrow = 1, ncol = 2)
  rdeg <- 0:deg
  p_sub <- matrix(p[seg[1, 1]:seg[1, 2], ], nrow = pd[1], ncol = pd[2])
  ch <- choose(deg, rdeg)

  output <-
    sapply(1:lt, function(j) {

      colSums(ch *
                ((1 - t[j])^(deg - rdeg)) *
                (t[j]^rdeg) *
                p_sub)

    }) %>% matrix(ncol = 3, nrow = lt, byrow = TRUE)

  return(output)

}

colRpalBezier <- function(hex, nSteps, colourSpace = "lab") {

  b <- hex %>% farver::decode_colour(to = colourSpace)
  t <- seq(0, 1, length = nSteps)
  b <- .colRpalBezier(t = t, p = b)
  b %>% farver::encode_colour(from = colourSpace)

}

.colRseq <- function(mat, col, nSteps) { n <- dim(mat)[1]; seq(mat[,col][1], mat[,col][n], length.out = nSteps) }

colRpalLinear <- function(hex, nSteps, colourSpace = "lab") {

  l <- hex %>% farver::decode_colour(to = colourSpace)

  n <- dim(l)[1]; N <- lapply(1:(n-1), function(x) { c(x, x+1) })

  nChunks <- ((nSteps-n)/(n-1))+2

  if(as.integer(nChunks) != nChunks) { stop("Length of hex does not divide into nSteps-1 equal-sized chunks for interpolation")}

  l <-
    lapply(N, function(y) {

      L <- l[y,];
      sapply(1:ncol(L), function(x) { .colRseq(L, x, nChunks) }) %>%
        farver::encode_colour(from = colourSpace)

    }) %>% unlist %>% unique
  #unique

  return(l)

}
