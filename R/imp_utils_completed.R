#' @title
#'
#' @description imp_utils_completed()
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

imp_utils_completed <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
  }
  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 1003

  # Extract the completed data:
  imp_merged_comp <- complete(imp_merged, 1) # first imputed data set
  # complete() provides more options for exploring with include and action
  # for outputting both observed and imputed and long, broad, etc. formatted datasets
  # Sanity check the number of missing values, will error if complete though:
  # md.pattern(imp_merged_comp)

  # Save the long format if needed for extensions, rownames are taken as .id:
  imp_merged_long <- complete(imp_merged, action = 'long', include = TRUE)
  # inlcude = T, saves also the original data, as is suggested
  # This format can then be passed back to increase the number of imputations with:
  # as.mids(imp_merged_long)
  return(something_I_need)
  }
