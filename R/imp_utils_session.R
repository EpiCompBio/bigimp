#' @title
#'
#' @description imp_utils_session()
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

imp_utils_session <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
  }

  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 1042

  # The end:
  # Remove objects that are not necessary to save:
  # ls()
  # object_sizes <- sapply(ls(), function(x) object.size(get(x)))
  # as.matrix(rev(sort(object_sizes))[1:10])
  #rm(list=ls(xxx))
  objects_to_save <- c('imp_merged') # needs to be a character vector for save()

  # Filename to save current R session, data and objects at the end:
  if (!is.null(args[['--session']])) { # arg is NULL
    save_session <- sprintf('%s_%s.RData', output_name, suffix)
    print(sprintf('Saving an R session image as: %s', save_session))
    save(list = objects_to_save, file = save_session, compress = 'gzip')
  } else {
    print('Not saving an R session image, this is the default. Specify
          the --session option otherwise')
  }
  sessionInfo()
  return(something_I_need)
  }
