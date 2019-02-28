#' @title
#'
#' @description imp_plot_strip()
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
#' @importFrom pack func1
#'

imp_plot_strip <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
}
  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 959

  # Stripplots might look better, check the first 10 imputed datasets:
  svg(sprintf('stripplots_imputation_%s.svg', output_name))
  stripplot(imp_merged,
            subset = (.imp == 0 | # get the original data
                        .imp == 1 | .imp == 2 | .imp == 3 | .imp == 4 | .imp == 5 |
                        .imp == 6 | .imp == 7 | .imp == 8 | .imp == 9 | .imp == 10),
            # col = mdc(1:2), #col = mdc(1:2), pch=20, cex=1.5,
            pch = 1, cex = 0.7,
            strip = strip.custom(par.strip.text = list(cex = 0.7))
  )
  # Magenta are imputed, blue observed
  dev.off()

  # Can also run for variables of interest only:
  # TO DO: needs unquoted vars
  # stripplot(imp_merged, vars_interest~.imp, pch = 20, cex = 2)

  # TO DO: save legend
  # Legend: Strip plot of observed (blue) and imputed (red) values for each variable imputed.
  # The figure shows whether distributions are similar.
  # If using the PMM method, imputed values have the same gaps as the observed data,
  # and are always within the range of the observed data.

  # Under MCAR, univariate distributions of the observed and imputed data
  # are expected to be identical.
  # Under MAR, they can be different, both in location and spread,
  # but their multivariate distribution is assumed to be identical.

  # The only way to test the amount of missing data that is to test
  # imputed vs complete cases and see if the results change
  return(something_I_need)
  }
