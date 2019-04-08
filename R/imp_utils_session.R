#' @title Save an R session
#'
#' @description imp_utils_session() is a convenience function to save an R
#' session and a log of the session information.
#'
#' @param output_prefix File name as string, '.RData' or '_log.txt' are appended
#' to saved objects and log file respectively. Default is
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
#' @author Antonio J Berlanga-Taylor, George Adams, Deborah Schneider-Luftman
#' <\url{https://github.com/EpiCompBio/bigimp}>
#'
#' @seealso \code{\link[base]{save.image}},
#' \code{\link[base]{saveRDS}},
#' \code{\link[base]{load}},
#' \code{\link[base]{readRDS}}.
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- stats::runif(20)
#' y <- list(a = 1, b = TRUE, c = "oops")
#' # See objects to save:
#' ls()
#' objects_to_save <- c('x', 'y')
#' imp_utils_session(output_prefix = 'xy',
#'                   objects_to_save = objects_to_save
#'                  )
#' dir()
#' rm(list = objects_to_save)
#' ls()
#' load('xy.RData')
#' ls()
#' }
#'
#' @export
#'

# TO DO:
# move this to episcout

imp_utils_session <- function(output_prefix = NULL,
                              objects_to_save = NULL,
                              ...
                              ) {

  objects_to_save <- c(objects_to_save) # needs to be a character vector

  # Filename to save current R session, data and objects at the end:
  if (is.character(output_prefix)) {
    output_name1 <- sprintf('%s.RData', output_prefix)
    output_name2 <- sprintf('%s_log.txt', output_prefix)
    } else {
      output_name1 <- sprintf('session_%s.RData', Sys.Date())
      output_name2 <- sprintf('session_%s_log.txt', Sys.Date())
    }
  save(list = objects_to_save,
       file = output_name1,
       ...
       )
  print(sprintf('Saved objects as: %s', output_name1))
  print(sprintf('Session information saved in: %s', output_name2))
  writeLines(capture.output(sessionInfo()), output_name2)
  }
