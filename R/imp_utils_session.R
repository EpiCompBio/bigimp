#' @title Save an R session
#'
#' @description imp_utils_session() is a convenience function to save an R
#' session, wraps base save() function
#'
#' @param output_name File name as string, '.RData' is appended. Default is
#' 'session_date.RData' eg 'session_2019-03-01.RData'
#'
#' @param objects_to_save String with list of objects to save. Assumes you only
#' want a subset from the R session. Use saveRDS() for one object or save.image()
#' for the whole session instead.
#'
#' @param ... pass any other parameters from save() such as compress = 'gzip'
#'
#' @return Saves an external representation of th eR objects to file
#'
#' @author Antonio J Berlanga-Taylor, George Adams, Deborah Schneider-Luftman <\url{https://github.com/EpiCompBio/bigimp}>
#'
#' @seealso \code{\link{functioname}},
#' \code{\link[base]{save.image}}, \code{\link[base]{saveRDS}},
#' \code{\link[base]{load}}, \code{\link[base]{readRDS}}.
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- stats::runif(20)
#' y <- list(a = 1, b = TRUE, c = "oops")
#' imp_utils_session(output_name = "xy.RData",
#'                   objects_to_save = 'x, y,')
#' }
#'
#' @export
#'

imp_utils_session <- function(output_name = NULL,
                              objects_to_save = NULL,
                              ...
                              ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
  }

  objects_to_save <- c(objects_to_save) # needs to be a character vector

  # Filename to save current R session, data and objects at the end:
  if (is.character(output_name)) {
  output_name <- output_name
  } else {
    output_name <- sprintf('session_%s.RData', Sys.Date())
  }
  save(list = objects_to_save,
       file = output_name,
       ...
       )
  print('Saved: ')
  print(output_name)
  print('Session information: ')
  print(sessionInfo())
  }
