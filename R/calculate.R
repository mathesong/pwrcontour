pwr.t.test_contourcalc <- function(nmin, nmax, dmin, dmax,
                               sig.level=0.05,
                               type = c("two.sample", "one.sample", "paired"),
                               alternative = c("two.sided",
                                               "less", "greater")) {


  type <- match.arg(type)
  alternative <- match.arg(alternative)

  contour_length <- 100

  # N contour

  if( (nmax - nmin) > contour_length) {
    nseq <- seq(nmin, nmax, length.out = contour_length)
  } else {
    nseq <- seq(nmin, nmax, by = 1)
  }

  # D contour
  dseq <- seq(dmin, dmax, length.out=contour_length)

  # Creating the data
  pwr_contours <- expand.grid(n = nseq, ES=dseq)

  pwr_contours$power <- purrr::map2_dbl(pwr_contours$n, pwr_contours$ES,
                  ~pwr::pwr.t.test(n = .x, d = .y,
                                   sig.level = sig.level,
                                   type = type,
                                   alternative = alternative)$power )

  return(pwr_contours)

}

pwr.r.test_contourcalc <- function(nmin, nmax, rmin, rmax,
                               sig.level=0.05,
                               alternative = c("two.sided",
                                               "less", "greater")) {


  alternative <- match.arg(alternative)

  contour_length <- 100

  # N contour

  if( (nmax - nmin) > contour_length) {
    nseq <- seq(nmin, nmax, length.out = contour_length)
  } else {
    nseq <- seq(nmin, nmax, by = 1)
  }

  # r contour
  rseq <- seq(rmin, rmax, length.out=contour_length)

  # Creating the data
  pwr_contours <- expand.grid(n = nseq, ES=rseq)

  pwr_contours$power <- purrr::map2_dbl(pwr_contours$n, pwr_contours$ES,
                           ~pwr::pwr.r.test(n = .x, r = .y,
                                            sig.level = sig.level,
                                            alternative = alternative)$power )

  return(pwr_contours)

}



