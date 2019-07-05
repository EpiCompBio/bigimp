#' @title
#'
#' @description imp_plot_bw()
#'
#' @param
#'
#' @param
#'
#' @return
#'
#' @note
#'
#' @author Antonio J Berlanga-Taylor, George Adams, Deborah Schneider-Luftman <\url{https://github.com/EpiCompBio/bigimp}>
#'
#' @seealso \code{\link{functioname}},
#' \code{\link[packagename]{functioname}}.
#'
#' @examples
#'
#' \dontrun{
#'
#'
#'
#' }
#'
#' @export
#'
# @importFrom pack func1
#'

imp_plot_bw <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
  }
  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 940

  # Further exploratory plots
  # # TO DO: save legends
  # Diagnostics for plausible values. compare imputed vs observed values
  # Assuming data are missing completely at random (MCAR)
  # imputations should have the same distribution as the observed data.
  # Distributions may differ because missing data are
  # missing at random (MAR) or non-random (MNAR)
  # Very large discrepancies should not exist though, check with:
  svg(sprintf('bwplots_imputation_%s.svg', output_name))
  bwplot(imp_merged,
         subset = (.imp == 0 | # get the original data
                     .imp == 1 | .imp == 2 | .imp == 3 | .imp == 4 | .imp == 5 |
                     .imp == 6 | .imp == 7 | .imp == 8 | .imp == 9 | .imp == 10),
         # col = mdc(1:2), #col = mdc(1:2), pch=20, cex=1.5,
         pch = 1, cex = 0.7,
         strip = strip.custom(par.strip.text = list(cex = 0.7))
  )
  dev.off()
  return(something_I_need)
  }
