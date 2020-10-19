#' Plot a power contour
#'
#' This is the function which creates the contours from a set of values. These
#' can either be generated with the contourcalc functions, or with any custom
#' values derived from, for example, simulations.
#'
#' @param x The x values.
#' @param y The y values.
#' @param pwr The power values.
#' @param pwrcolours The breaks for the power bands. Defaults to `seq(0,1,0.1)`.
#' @param xlab The label of the x axis.
#' @param ylab The label of the y axis.
#' @param pwrbands The additional black power bands. Defaults to
#'   `c(0.5, 0.8, 0.95)`.
#' @param alpha The transparency of the contour for seeing the grid lines
#'   underneath.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' vals <- pwrcontour:::pwr.t.test_contourcalc(5, 50, 0.3, 0.7)
#' create_contour(vals$n, vals$ES, vals$power, xlab="x", ylab="y")
create_contour <- function(x, y, pwr, pwrcolours = seq(0,1, by=0.1),
                           xlab = "Sample size",
                           ylab = "Hypothetical effect size",
                           pwrbands = c(0.5, 0.8, 0.95), alpha=0.6) {

  pwrcolours <- pwrcolours[order(pwrcolours)]

  if( utils::tail(pwrcolours, 2)[1] > 0.99 ) {
    stop("The highest colour cannot distinguish above 0.99")
  }

  pwr <- ifelse( pwr == 1, 0.99, pwr )

  pwr_data <- data.frame(x=x, y=y, pwr=pwr)

  ggplot2::ggplot(pwr_data, ggplot2::aes(x=.data$x, y=.data$y, z=.data$pwr)) +
    ggplot2::geom_contour_filled(alpha=alpha, breaks=pwrcolours) +
    ggplot2::labs(x = xlab,
         y = ylab) +
    viridis::scale_fill_viridis("Power", discrete = T) +
    ggplot2::geom_contour(breaks=pwrbands, colour="black", linetype="dashed") +
    ggplot2::theme_minimal()

}
