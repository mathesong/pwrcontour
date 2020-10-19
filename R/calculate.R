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

pwr.prop.test_contourcalc <- function(nmin, nmax, propmin, propmax, propcomp=0.5,
                                   sig.level=0.05,
                                   type = c("two.sample", "one.sample"),
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
  propseq <- seq(propmin, propmax, length.out=contour_length)
  hseq    <- purrr::map_dbl(propseq, ~pwr::ES.h(p1=.x, p2=propcomp))

  # Creating the data
  pwr_contours <- expand.grid(n = nseq, ES=hseq)
  pwr_contours$Proportions <- purrr::map_dbl(pwr_contours$ES, ~h2p1(.x, propcomp))

  if( type == "two.sample" ) {
    pwr_contours$power <- purrr::map2_dbl(pwr_contours$n, pwr_contours$ES,
                                  ~pwr::pwr.2p.test(n = .x, h = .y,
                                            sig.level = sig.level,
                                            alternative = alternative)$power )
  } else if( type == "one.sample" ) {
    pwr_contours$power <- purrr::map2_dbl(pwr_contours$n, pwr_contours$ES,
                                  ~pwr::pwr.p.test(n = .x, h = .y,
                                            sig.level = sig.level,
                                            alternative = alternative)$power )
  }


  return(pwr_contours)

}

h2p1 <- function(h, p2) {
  sin(0.5*(h + 2 * asin(sqrt(p2))))^2
}

h2p2 <- function(h, p1) {
  sin(0.5*(2*asin(sqrt(p1)) - h))^2
}

# p1 = 0.7
# p2 = 0.2
# h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
# h <- pwr::ES.h(p1, p2)
#
# # solve for p1
# sin(0.5*(h + 2 * asin(sqrt(p2))))^2
#
# # solve for p2
# sin(0.5*(2*asin(sqrt(p1)) - h))^2



pwr.eq.t.test_contourcalc <- function(nmin, nmax, dmin, dmax,
                                   sig.level=0.05,
                                   type = c("two.sample", "one.sample", "paired")) {


  type <- match.arg(type)

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

  if(type == "two.sample") {
    pwr_contours$power <- purrr::map2_dbl(pwr_contours$n, pwr_contours$ES,
                                ~quiet(TOSTER::powerTOSTtwo(N = .x,
                                                          low_eqbound_d = .y*-1,
                                                          high_eqbound_d = .y,
                                                          alpha = sig.level) ))
  } else if(type == "paired") {
    pwr_contours$power <- purrr::map2_dbl(pwr_contours$n, pwr_contours$ES,
                             ~quiet(TOSTER::powerTOSTpaired(N = .x,
                                                         low_eqbound_dz = .y*-1,
                                                         high_eqbound_dz = .y,
                                                         alpha = sig.level) ))
  } else if(type == "one.sample") {
    pwr_contours$power <- purrr::map2_dbl(pwr_contours$n, pwr_contours$ES,
                                          ~quiet(TOSTER::powerTOSTone(N = .x,
                                                                         low_eqbound_d = .y*-1,
                                                                         high_eqbound_d = .y,
                                                                         alpha = sig.level) ))
  }

  return(pwr_contours)

}


pwr.eq.r.test_contourcalc <- function(nmin, nmax, rmin, rmax,
                                   sig.level=0.05) {

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
                                        ~quiet(TOSTER::powerTOSTr(N = .x,
                                                      low_eqbound_r = .y*-1,
                                                      high_eqbound_r = .y,
                                                      alpha = sig.level) ))

  return(pwr_contours)

}



