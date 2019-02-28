#' @title
#'
#' @description imp_plot_missingness()
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

imp_plot_missingness <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('mice', quietly = TRUE)) {
  stop('Package mice needed for this function to work. Please install it.',
  call. = FALSE)
}
  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 625

  # summary(input_data)
  # dim(input_data)

  # Inspect the missing data pattern:
  # TO DO: print out legend
  svg(sprintf('missingness_pattern_%s.svg', output_name))
  missingness <- mice::md.pattern(input_data,
                                  plot = TRUE)
  dev.off()
  return(missigness)
  # TO DO:
  # Does the missing data of var x depend on var y?
  # Plot histograms conditional on missingness for vars_interest eg:
  # https://gerkovink.github.io/miceVignettes/Missingness_inspection/Missingness_inspection.html
  # (in step 8)
  # R <- is.na(boys$gen)
  # histogram(~age|R, data=boys)
  }
