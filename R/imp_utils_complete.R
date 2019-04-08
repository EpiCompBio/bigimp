#' @title Extract one complete (imputed) dataset
#'
#' @description imp_utils_complete() extracts one imputed dataset. This is a
#' covenience function to wrap  mice::complete()
#'
#' @param data An object of class mids, see mice::complete()
#' @param action Numeric vector or keyword, usually the imputed dataset number.
#'               Use eg 'long' to get imputed data sets stacked vertically.
#'               Default is 1.
#' @param ... pass any other mice::complete() parameters
#' @return
#'
#' @note Pass include = 'TRUE' to get the original dataset as well.
#'
#' @author Antonio J Berlanga-Taylor, George Adams, Deborah Schneider-Luftman
#' <\url{https://github.com/EpiCompBio/bigimp}>
#'
#' @seealso \code{\link{imp_imp_dry_run}}, \code{\link{imp_imp_mice}}
#' \code{\link[mice]{mids}},
#' \code{\link[mice]{complete}}.
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

imp_utils_complete <- function(data = NULL,
                               action = 1,
                               ...
                               ) {

# Use this instead or library or require inside functions:
if (!requireNamespace('mice', quietly = TRUE)) {
  stop('Package mice needed for this function to work. Please install it.',
  call. = FALSE)
  }
  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 1003

  # Extract the completed data:
  imp_merged_comp <- mice::complete(data = data,
                                    action = action,
                                    ...)
  # first imputed data set if action = 1
  # complete() provides more options for exploring with include and action
  # for outputting both observed and imputed and long, broad, etc.
  # formatted datasets
  # Sanity check the number of missing values, will error if complete though:
  # md.pattern(imp_merged_comp)

  # Save the long format if needed for extensions, rownames are taken as .id:
  # imp_merged_long <- complete(imp_merged, action = 'long', include = TRUE)
  # inlcude = T, saves also the original data, as is suggested
  # This format can then be passed back to increase the number of imputations
  # with as.mids(imp_merged_long)
  return(imp_merged_comp)
  }
