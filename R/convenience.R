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

#' Plot a Power Contour for a correlation test comparison
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


#' Create a Power Table for a t-test comparison
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
