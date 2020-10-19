#' Plot a Power Contour for a t-test comparison
#'
#' Plot a contour of sample size, effect size and power for t-test comparisons
#' in the style of Richard Morey's jpower JAMOVI module.
#'
#' @param nmin Minimum sample size for the plot.
#' @param nmax Maximum sample size for the plot.
#' @param esmin Minimum effect size for the plot. Defaults to Cohen's d, but can be percentages or units of measurement if desired using the `d_unitconversion` parameter.
#' @param esmax Maximum effect size for the plot. Defaults to Cohen's d, but can be percentages or units of measurement if desired using the `d_unitconversion` parameter.
#' @param sig.level Significance level, or alpha. Defaults to 0.05.
#' @param type The t-test type. The default is "two.sample", but other options are "paired" or "one.sample".
#' @param alternative The alternative hypothesis. The default is "two.sided", but other options are "less" or "greater".
#' @param pwrbands Additional black power bands to display on the figure to emphasise certain power levels. Defaults to c(0.5, 0.8, 0.95).
#' @param d_unitconversion Unit conversion of Cohen's d. If specified as 5, for example, 5 units of change in the units of the figure are considered equivalent to Cohen's d = 1. This conversion should be calculated in advance. Defaults to 1 for Cohen's d.
#' @param d_units The units of the difference. Defaults to "Cohen's d".
#' @param alpha The transparency of the contours, in order to show grid lines below.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @author Granville J Matheson, \email{mathesong@@gmail.com}
#'
#' @examples
#' # A straightforward example
#' pwr.t.test.contour(3, 100, 0.1, 1, sig.level=0.05)
#'
#' # Custom units
#' pwr.t.test.contour(3, 100, 0.5, 5, sig.level=0.05,
#'                    d_unitconversion=5, d_units="mmHg")
#'
#' # Adding things
#' pwr.t.test.contour(3, 100, 0.5, 5, sig.level=0.05,
#'                    d_unitconversion=5, d_units="mmHg") +
#'                    ggplot2::annotate("text", x=25, y=2,
#'                    label="We don't want to be here")
pwr.t.test.contour <- function(nmin, nmax, esmin, esmax,
                               sig.level=0.05,
                               type = c("two.sample", "one.sample", "paired"),
                               alternative = c("two.sided",
                                               "less", "greater"),
                               pwrbands = c(0.5, 0.8, 0.95),
                               d_unitconversion=1,
                               d_units="Cohen\'s d",
                               alpha=0.6) {


  type <- match.arg(type)
  alternative <- match.arg(alternative)

  dmin <- esmin / d_unitconversion
  dmax <- esmax / d_unitconversion

  values <- pwr.t.test_contourcalc(nmin, nmax, dmin, dmax,
                                   sig.level, type,
                                   alternative)

  values$ES = values$ES * d_unitconversion

  ylab <- paste0("Hypothetical effect size (", d_units, ")")
  xlab <- ifelse(type == "one.sample",
                 yes="Sample size",
                 no="Sample size of each group")

  create_contour(x = values$n,
                            y = values$ES,
                            pwr = values$power,
                            pwrcolours = seq(0,1, by=0.1),
                            xlab=xlab,
                            ylab=ylab,
                            pwrbands=pwrbands,
                            alpha=alpha)


}

#' Create a Power Table for a t-test comparison
#'
#' Plot a power analysis table of effect size and power for t-test comparisons
#' in the style of Richard Morey's jpower JAMOVI module.
#'
#' @param n The proposed sample size.
#' @param sig.level Significance level, or alpha. Defaults to 0.05.
#' @param type The t-test type. The default is "two.sample", but other options are "paired" or "one.sample".
#' @param alternative The alternative hypothesis. The default is "two.sided", but other options are "less" or "greater".
#' @param d_unitconversion Unit conversion of Cohen's d. If specified as 5, for example, 5 units of change in the units of the table are considered equivalent to Cohen's d = 1. This conversion should be calculated in advance. Defaults to 1 for Cohen's d.
#' @param d_units The units of the difference. Defaults to "Cohen's d".
#' @param kable Should the table be presented as a `knitr::kable()` figure? Defaults to `TRUE`. Otherwise a `tibble` is returned.
#' @param digits The number of digits.
#' @param ... Extra input arguments for the `knitr::kable()` function
#'
#' @return A power table.
#' @export
#'
#' @author Granville J Matheson, \email{mathesong@@gmail.com}
#'
#' @examples
#' # Straightforward example
#' pwr.t.test.table(n=25)
#'
#' # More customised example
#' pwr.t.test.table(n=25, d_unitconversion=20, d_units="%", digits=1)
pwr.t.test.table <- function(n,
                             sig.level=0.05,
                             type = c("two.sample", "one.sample", "paired"),
                             alternative = c("two.sided",
                                             "less", "greater"),
                             d_unitconversion=1,
                             d_units="Cohen's d",
                             kable = TRUE,
                             digits=3, ...) {


  type <- match.arg(type)
  alternative <- match.arg(alternative)

  powerlevels = c(0.5, 0.8, 0.95)

  powertable <- data.frame(
    power = powerlevels
  )

  powertable$ES = as.numeric(lapply(powertable$power,
                                    function(x) pwr::pwr.t.test(n=n, power=x,
                                                                sig.level = sig.level,
                                                                type=type,
                                                                alternative=alternative)$d))

  powertable$ES <- powertable$ES * d_unitconversion



  descriptions_text <- c("Likely miss",
                         "Good chance of missing",
                         "Probably detect",
                         "Almost surely detect")

  power_text <- c("< 50%", "50% - 80%", "80% - 95%", "> 95%")

  #### Text Effect Sizes

  eslevel_extra <- round(c(0, powertable$ES, Inf), digits)
  ele_n <- length(eslevel_extra)-1
  eslevels_text <- paste(eslevel_extra[1:ele_n], "<", "delta", "<", eslevel_extra[2:(ele_n+1)],sep = " ")

  eslevels_text[1] <- paste( "delta", "<", eslevel_extra[2],sep = " ")
  eslevels_text[ele_n] <- paste( "delta", ">", eslevel_extra[(ele_n)],sep = " ")


  ### Pretty table

  prettytable <- tibble::tibble(
    "True Effect Size" = eslevels_text,
    "Power to detect" = power_text,
    Description = descriptions_text
  )

  colnames(prettytable)[1] <- paste0("True Effect Size (", d_units, ")")

  if( kable ) {
    prettytable <-
      knitr::kable(prettytable, align = "ccl",
                   caption = "Power by effect size", ...)
  }

  return(prettytable)

}

#' Plot a Power Contour for a correlation analysis
#'
#' Plot a contour of sample size, effect size and power for correlation comparisons
#' in the style of Richard Morey's jpower JAMOVI module.
#'
#' @param nmin Minimum sample size for the plot.
#' @param nmax Maximum sample size for the plot.
#' @param rmin Minimum Pearson's r for the plot.
#' @param rmax Maximum Pearson's r for the plot.
#' @param sig.level Significance level, or alpha. Defaults to 0.05.
#' @param alternative The alternative hypothesis. The default is "two.sided", but other options are "less" or "greater".
#' @param pwrbands Additional black power bands to display on the figure to emphasise certain power levels. Defaults to c(0.5, 0.8, 0.95).
#' @param alpha The transparency of the contours, in order to show grid lines below.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @author Granville J Matheson, \email{mathesong@@gmail.com}
#'
#' @examples
#' pwr.r.test.contour(5, 100, 0.1, 0.7, sig.level=0.05)
pwr.r.test.contour <- function(nmin, nmax, rmin, rmax,
                               sig.level=0.05,
                               alternative = c("two.sided",
                                               "less", "greater"),
                               pwrbands = c(0.5, 0.8, 0.95),
                               alpha=0.6) {


  alternative <- match.arg(alternative)

  values <- pwr.r.test_contourcalc(nmin, nmax, rmin, rmax,
                                   sig.level,
                                   alternative)

  ylab <- paste0("Hypothetical effect size (Pearson\'s r)")
  xlab <- paste0("Sample size")

  create_contour(x = values$n,
                 y = values$ES,
                 pwr = values$power,
                 pwrcolours = seq(0,1, by=0.1),
                 xlab=xlab,
                 ylab=ylab,
                 pwrbands=pwrbands,
                 alpha=alpha)


}

#' Create a Power Table for a correlation analysis
#'
#' Plot a power analysis table of effect size and power for Pearson's r tests
#' in the style of Richard Morey's jpower JAMOVI module.
#'
#' @param n The proposed sample size.
#' @param sig.level Significance level, or alpha. Defaults to 0.05.
#' @param alternative The alternative hypothesis. The default is "two.sided", but other options are "less" or "greater".
#' @param kable Should the table be presented as a `knitr::kable()` figure? Defaults to `TRUE`. Otherwise a `tibble` is returned.
#' @param digits The number of digits.
#' @param ... Extra input arguments for the `knitr::kable()` function
#'
#' @return A power table.
#' @export
#'
#' @examples
#' pwr.r.test.table(n=25)
pwr.r.test.table <- function(n,
                             sig.level=0.05,
                             alternative = c("two.sided",
                                             "less", "greater"),
                             kable = TRUE, digits=3, ...) {


  alternative <- match.arg(alternative)

  powerlevels = c(0.5, 0.8, 0.95)

  powertable <- data.frame(
    power = powerlevels
  )

  powertable$ES = as.numeric(lapply(powertable$power,
                                    function(x) pwr::pwr.r.test(n=n, power=x,
                                                                sig.level = sig.level,
                                                                alternative=alternative)$r))


  descriptions_text <- c("Likely miss",
                         "Good chance of missing",
                         "Probably detect",
                         "Almost surely detect")

  power_text <- c("< 50%", "50% - 80%", "80% - 95%", "> 95%")

  #### Text Effect Sizes

  eslevel_extra <- round(c(0, powertable$ES, 1), digits)
  ele_n <- length(eslevel_extra)-1
  eslevels_text <- paste(eslevel_extra[1:ele_n], "<", "r", "<", eslevel_extra[2:(ele_n+1)],sep = " ")

  eslevels_text[1] <- paste( "r", "<", eslevel_extra[2],sep = " ")
  eslevels_text[ele_n] <- paste( "r", ">", eslevel_extra[(ele_n)],sep = " ")


  ### Pretty table

  prettytable <- tibble::tibble(
    "True Effect Size" = eslevels_text,
    "Power to detect" = power_text,
    Description = descriptions_text
  )

  colnames(prettytable)[1] <- "True Effect Size (Pearson's r)"

  if( kable ) {
    prettytable <-
      knitr::kable(prettytable, align = "ccl",
                   caption = "Power by effect size", ...)
  }

  return(prettytable)

}


#' Plot a Power Contour for a proportion test
#'
#' Plot a contour of sample size, effect size and power for proportion test
#' comparisons in the style of Richard Morey's jpower JAMOVI module.
#'
#' @param nmin Minimum sample size for the plot.
#' @param nmax Maximum sample size for the plot.
#' @param propmin Minimum proportion for the plot.
#' @param propmax Maximum proportion for the plot.
#' @param propcomp Comparison proportion value against which the measured proportion is compared.
#' @param sig.level Significance level, or alpha. Defaults to 0.05.
#' @param type The proportion test type. The default is "two.sample", but another option is "one.sample".
#' @param alternative The alternative hypothesis. The default is "two.sided", but other options are "less" or "greater".
#' @param pwrbands Additional black power bands to display on the figure to emphasise certain power levels. Defaults to c(0.5, 0.8, 0.95).
#' @param alpha The transparency of the contours, in order to show grid lines below.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @author Granville J Matheson, \email{mathesong@@gmail.com}
#'
#' @examples
#' pwr.prop.test.contour(5, 100, 0.1, 0.9)
pwr.prop.test.contour <- function(nmin, nmax, propmin, propmax, propcomp=0.5,
                               sig.level=0.05,
                               type = c("two.sample", "one.sample"),
                               alternative = c("two.sided",
                                               "less", "greater"),
                               pwrbands = c(0.5, 0.8, 0.95),
                               alpha=0.6) {


  alternative <- match.arg(alternative)

  values <- pwr.prop.test_contourcalc(nmin, nmax, propmin, propmax,
                                      propcomp,
                                      sig.level, type,
                                      alternative)

  ylab <- paste0("Hypothetical observed proportion")
  xlab <- paste0("Sample size")

  create_contour(x = values$n,
                 y = values$Proportions,
                 pwr = values$power,
                 pwrcolours = seq(0,1, by=0.1),
                 xlab=xlab,
                 ylab=ylab,
                 pwrbands=pwrbands,
                 alpha=alpha) +
    ggplot2::geom_abline(slope = 0, intercept=propcomp, linetype="dotted")


}







#' Create a Power Table for a proportion test
#'
#' Plot a power analysis table of effect size and power for a proportion test
#' in the style of Richard Morey's jpower JAMOVI module.
#'
#' @param n The proposed sample size.
#' @param propcomp Comparison proportion value against which the measured proportion is compared.
#' @param sig.level Significance level, or alpha. Defaults to 0.05.
#' @param type The t-test type. The default is "two.sample", but other options are "paired" or "one.sample".
#' @param alternative The alternative hypothesis. The default is "two.sided", but other options are "less" or "greater".
#' @param d_unitconversion Unit conversion of Cohen's d. If specified as 5, for example, 5 units of change in the units of the table are considered equivalent to Cohen's d = 1. This conversion should be calculated in advance. Defaults to 1 for Cohen's d.
#' @param d_units The units of the difference. Defaults to "Cohen's d".
#' @param kable Should the table be presented as a `knitr::kable()` figure? Defaults to `TRUE`. Otherwise a `tibble` is returned.
#' @param digits The number of digits.
#' @param ... Extra input arguments for the `knitr::kable()` function
#'
#' @return A power table.
#' @export
#'
#' @author Granville J Matheson, \email{mathesong@@gmail.com}
#'
#' @examples
#' # Straightforward example
#' pwr.t.test.table(n=25)
#'
#' # More customised example
#' pwr.t.test.table(n=25, d_unitconversion=20, d_units="%", digits=1)
pwr.prop.test.table <- function(n, propcomp=0.5,
                             sig.level=0.05,
                             type = c("two.sample", "one.sample"),
                             alternative = c("two.sided",
                                             "less", "greater"),
                             kable = TRUE,
                             digits=3, ...) {


  type <- match.arg(type)
  alternative <- match.arg(alternative)

  powerlevels = c(0.5, 0.8, 0.95)

  powertable <- data.frame(
    power = powerlevels
  )

  if( type == "two.sample" ) {
    powertable$ES <- purrr::map_dbl(powertable$power,
                                 ~pwr::pwr.2p.test(n = n, power = .x,
                                                  sig.level = sig.level,
                                                  alternative = alternative)$h )
  } else if( type == "one.sample" ) {
    powertable$ES <- purrr::map_dbl(pwr_contours$ES,
                                ~pwr::pwr.p.test(n = n, power = .x,
                                                 sig.level = sig.level,
                                                 alternative = alternative)$h )
  }

  powertable_g <- powertable
  powertable_l <- powertable

  powertable_g$Proportion <- purrr::map_dbl( powertable$ES, ~h2p1(.x, propcomp))
  powertable_l$Proportion <- purrr::map_dbl(-powertable$ES, ~h2p1(.x, propcomp))



  descriptions_text <- c("Likely miss",
                         "Good chance of missing",
                         "Probably detect",
                         "Almost surely detect")

  power_text <- c("< 50%", "50% - 80%", "80% - 95%", "> 95%")

  #### Text Effect Sizes

  # Greater

  eslevel_extra_g <- round(c(0, powertable_g$Proportion, Inf), digits)
  ele_n <- length(eslevel_extra_g)-1

  eslevels_text_g <- paste(eslevel_extra_g[1:ele_n], "<", "prop", "<", eslevel_extra_g[2:(ele_n+1)],sep = " ")
  eslevels_text_g[1] <- paste( "prop", "<", eslevel_extra_g[2],sep = " ")
  eslevels_text_g[ele_n] <- paste( "prop", ">", eslevel_extra_g[(ele_n)],sep = " ")

  # Less

  eslevel_extra_l <- round(c(0, powertable_l$Proportion, Inf), digits)
  ele_n <- length(eslevel_extra_l)-1

  eslevels_text_l <- paste(eslevel_extra_l[1:ele_n], ">", "prop", ">", eslevel_extra_l[2:(ele_n+1)],sep = " ")
  eslevels_text_l[1] <- paste( "prop", ">", eslevel_extra_l[2],sep = " ")
  eslevels_text_l[ele_n] <- paste( "prop", "<", eslevel_extra_l[(ele_n)],sep = " ")


  ### Pretty table

  prettytable_g <- tibble::tibble(
    "True Proportion" = eslevels_text_g,
    "Power to detect" = power_text,
    Description = descriptions_text
  )

  prettytable_l <- tibble::tibble(
    "True Proportion" = eslevels_text_l,
    "Power to detect" = power_text,
    Description = descriptions_text
  )

  ## Combining

  if( alternative == "two.sided" ) {
    prettytable <- tibble::tibble(
      "True Increased Proportion" = eslevels_text_g,
      "True Decreased Proportion" = eslevels_text_l,
      "Power to detect" = power_text,
      Description = descriptions_text)
  } else if(alternative == "greater") {
    prettytable = prettytable_g
  } else if(alternative == "less") {
    prettytable = prettytable_l
  }

  if( kable ) {
    prettytable <-
      knitr::kable(prettytable, align = "ccl",
                   caption = paste("Power by proportion, relative to", propcomp), ...)
  }

  return(prettytable)

}


#' Plot a Power Contour for a test of equivalence for a t-test
#'
#' Plot a contour of sample size, effect size and power for t-test
#' of equivalence in the style of Richard Morey's jpower JAMOVI module,
#' assuming the true effect is equal to zero.
#'
#' @param nmin Minimum sample size for the plot.
#' @param nmax Maximum sample size for the plot.
#' @param esmin Minimum effect size for the plot. Defaults to Cohen's d, but can be percentages or units of measurement if desired using the `d_unitconversion` parameter.
#' @param esmax Maximum effect size for the plot. Defaults to Cohen's d, but can be percentages or units of measurement if desired using the `d_unitconversion` parameter.
#' @param sig.level Significance level, or alpha. Defaults to 0.05.
#' @param type The t-test type. The default is "two.sample", but other options are "paired" or "one.sample".
#' @param pwrbands Additional black power bands to display on the figure to emphasise certain power levels. Defaults to c(0.5, 0.8, 0.95).
#' @param d_unitconversion Unit conversion of Cohen's d. If specified as 5, for example, 5 units of change in the units of the figure are considered equivalent to Cohen's d = 1. This conversion should be calculated in advance. Defaults to 1 for Cohen's d.
#' @param d_units The units of the difference. Defaults to "Cohen's d".
#' @param alpha The transparency of the contours, in order to show grid lines below.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @author Granville J Matheson, \email{mathesong@@gmail.com}
#'
#' @examples
#' # A straightforward example
#' pwr.eq.t.test.contour(3, 100, 0.1, 1, sig.level=0.05)
#'
#' # Custom units
#' pwr.eq.t.test.contour(3, 100, 0.5, 5, sig.level=0.05,
#'                    d_unitconversion=5, d_units="mmHg")
#'
#' # Adding things
#' pwr.eq.t.test.contour(3, 100, 0.5, 5, sig.level=0.05,
#'                    d_unitconversion=5, d_units="mmHg") +
#'                    ggplot2::annotate("text", x=25, y=2,
#'                    label="We don't want to be here")
pwr.eq.t.test.contour <- function(nmin, nmax, esmin, esmax,
                               sig.level=0.05,
                               type = c("two.sample", "one.sample", "paired"),
                               pwrbands = c(0.5, 0.8, 0.95),
                               d_unitconversion=1,
                               d_units="Cohen\'s d",
                               alpha=0.6) {


  type <- match.arg(type)

  dmin <- esmin / d_unitconversion
  dmax <- esmax / d_unitconversion

  values <- pwr.eq.t.test_contourcalc(nmin, nmax, dmin, dmax,
                                   sig.level, type)

  values$ES = values$ES * d_unitconversion

  ylab <- paste0("Hypothetical equivalence bounds (", d_units, ")")
  xlab <- ifelse(type == "one.sample",
                 yes="Sample size",
                 no="Sample size of each group")

  create_contour(x = values$n,
                 y = values$ES,
                 pwr = values$power,
                 pwrcolours = seq(0,1, by=0.1),
                 xlab=xlab,
                 ylab=ylab,
                 pwrbands=pwrbands,
                 alpha=alpha)


}

#' Create a Power Table for a test of equivalence for a t-test
#'
#' Plot a power analysis table of effect size and power for t-test comparisons
#' in the style of Richard Morey's jpower JAMOVI module,
#' assuming the true effect is equal to zero.
#'
#' @param n The proposed sample size.
#' @param sig.level Significance level, or alpha. Defaults to 0.05.
#' @param type The t-test type. The default is "two.sample", but other options are "paired" or "one.sample".
#' @param d_unitconversion Unit conversion of Cohen's d. If specified as 5, for example, 5 units of change in the units of the table are considered equivalent to Cohen's d = 1. This conversion should be calculated in advance. Defaults to 1 for Cohen's d.
#' @param d_units The units of the difference. Defaults to "Cohen's d".
#' @param kable Should the table be presented as a `knitr::kable()` figure? Defaults to `TRUE`. Otherwise a `tibble` is returned.
#' @param digits The number of digits.
#' @param ... Extra input arguments for the `knitr::kable()` function
#'
#' @return A power table.
#' @export
#'
#' @author Granville J Matheson, \email{mathesong@@gmail.com}
#'
#' @examples
#' # Straightforward example
#' pwr.t.test.table(n=25)
#'
#' # More customised example
#' pwr.t.test.table(n=25, d_unitconversion=20, d_units="%", digits=1)
pwr.eq.t.test.table <- function(n,
                                sig.level=0.05,
                                type = c("two.sample", "one.sample", "paired"),
                                d_unitconversion=1,
                                d_units="Cohen's d",
                                kable = TRUE,
                                digits=3, ...) {


  type <- match.arg(type)

  powerlevels = c(0.5, 0.8, 0.95)

  powertable <- data.frame(
    power = powerlevels
  )

  if(type == "two.sample") {

    powertable$ES <- purrr::map_dbl(powertable$power,
                                    ~quiet(TOSTER::powerTOSTtwo(N = n,
                                                                statistical_power = .x,
                                                                alpha = sig.level)[1] ))
  } else if(type == "paired") {

    powertable$ES <- purrr::map_dbl(powertable$power,
                                    ~quiet(TOSTER::powerTOSTpaired(N = n,
                                                                   statistical_power = .x,
                                                                   alpha = sig.level)[1] ))
  } else if(type == "one.sample") {

    powertable$ES <- purrr::map_dbl(powertable$power,
                                    ~quiet(TOSTER::powerTOSTone(N = n,
                                                                statistical_power = .x,
                                                                alpha = sig.level)[1] ))
  }

  powertable$ES <- powertable$ES * d_unitconversion



  descriptions_text <- c("Likely miss",
                         "Good chance of missing",
                         "Probably detect",
                         "Almost surely detect")

  power_text <- c("< 50%", "50% - 80%", "80% - 95%", "> 95%")

  #### Text Effect Sizes

  eslevel_extra <- round(c(0, powertable$ES, Inf), digits)
  ele_n <- length(eslevel_extra)-1
  eslevels_text <- paste(eslevel_extra[1:ele_n], "<", "delta", "<", eslevel_extra[2:(ele_n+1)],sep = " ")

  eslevels_text[1] <- paste( "delta", "<", eslevel_extra[2],sep = " ")
  eslevels_text[ele_n] <- paste( "delta", ">", eslevel_extra[(ele_n)],sep = " ")


  ### Pretty table

  prettytable <- tibble::tibble(
    "True Effect Size" = eslevels_text,
    "Power to detect" = power_text,
    Description = descriptions_text
  )

  colnames(prettytable)[1] <- paste0("True Effect Size (", d_units, ")")

  if( kable ) {
    prettytable <-
      knitr::kable(prettytable, align = "ccl",
                   caption = "Power by effect size", ...)
  }

  return(prettytable)

}

#' Plot a Power Contour for a test of equivalence for a correlation
#'
#' Plot a contour of sample size, effect size and power for a test of
#' equivalence for a correlation comparisons in the style of Richard Morey's
#' jpower JAMOVI module, assuming the true effect is equal to zero.
#'
#' @param nmin Minimum sample size for the plot.
#' @param nmax Maximum sample size for the plot.
#' @param rmin Minimum Pearson's r for the plot.
#' @param rmax Maximum Pearson's r for the plot.
#' @param sig.level Significance level, or alpha. Defaults to 0.05.
#' @param pwrbands Additional black power bands to display on the figure to
#'   emphasise certain power levels. Defaults to c(0.5, 0.8, 0.95).
#' @param alpha The transparency of the contours, in order to show grid lines
#'   below.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @author Granville J Matheson, \email{mathesong@@gmail.com}
#'
#' @examples
#' pwr.r.test.contour(5, 100, 0.1, 0.7, sig.level=0.05)
pwr.eq.r.test.contour <- function(nmin, nmax, rmin, rmax,
                               sig.level=0.05,
                               pwrbands = c(0.5, 0.8, 0.95),
                               alpha=0.6) {


  values <- pwr.eq.r.test_contourcalc(nmin, nmax, rmin, rmax,
                                   sig.level)

  ylab <- paste0("Hypothetical equivalence bounds (Pearson\'s r)")
  xlab <- paste0("Sample size")

  create_contour(x = values$n,
                 y = values$ES,
                 pwr = values$power,
                 pwrcolours = seq(0,1, by=0.1),
                 xlab=xlab,
                 ylab=ylab,
                 pwrbands=pwrbands,
                 alpha=alpha)


}




#' Create a Power Table for a for a test of equivalence for a correlation
#'
#' Plot a power analysis table of effect size and power for Pearson's r tests
#' in the style of Richard Morey's jpower JAMOVI module,
#' assuming the true effect is equal to zero.
#'
#' @param n The proposed sample size.
#' @param sig.level Significance level, or alpha. Defaults to 0.05.
#' @param kable Should the table be presented as a `knitr::kable()` figure? Defaults to `TRUE`. Otherwise a `tibble` is returned.
#' @param digits The number of digits.
#' @param ... Extra input arguments for the `knitr::kable()` function
#'
#' @return A power table.
#' @export
#'
#' @examples
#' pwr.r.test.table(n=25)
pwr.eq.r.test.table <- function(n,
                             sig.level=0.05,
                             kable = TRUE, digits=3, ...) {



  powerlevels = c(0.5, 0.8, 0.95)

  powertable <- data.frame(
    power = powerlevels
  )

  powertable$ES <- purrr::map_dbl(powertable$power,
                         ~quiet(TOSTER::powerTOSTr(N = n,
                                                       statistical_power = .x,
                                                       alpha = sig.level)[1] ))


  descriptions_text <- c("Likely miss",
                         "Good chance of missing",
                         "Probably detect",
                         "Almost surely detect")

  power_text <- c("< 50%", "50% - 80%", "80% - 95%", "> 95%")

  #### Text Effect Sizes

  eslevel_extra <- round(c(0, powertable$ES, 1), digits)
  ele_n <- length(eslevel_extra)-1
  eslevels_text <- paste(eslevel_extra[1:ele_n], "<", "r", "<", eslevel_extra[2:(ele_n+1)],sep = " ")

  eslevels_text[1] <- paste( "r", "<", eslevel_extra[2],sep = " ")
  eslevels_text[ele_n] <- paste( "r", ">", eslevel_extra[(ele_n)],sep = " ")


  ### Pretty table

  prettytable <- tibble::tibble(
    "True Effect Size" = eslevels_text,
    "Power to detect" = power_text,
    Description = descriptions_text
  )

  colnames(prettytable)[1] <- "True Effect Size (Pearson's r)"

  if( kable ) {
    prettytable <-
      knitr::kable(prettytable, align = "ccl",
                   caption = "Power by effect size", ...)
  }

  return(prettytable)

}
