#' @title
#'
#' @description imp_imp_dry_run()
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

imp_imp_dry_run <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
  }

  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 497

  print('Running a dry imputation to get methods and predictor matrix.')
  dry_mice <- mice(input_data, maxit = 0, print = F)
  # Save predictor matrix:
  # pred[ ,"hyp"] <- 0
  fwrite(as.data.frame(dry_mice$pred),
         sprintf('predictor_matrix_%s', output_file_name),
         sep = '\t',
         na = 'NA',
         col.names = TRUE,
         row.names = TRUE,
         quote = FALSE
  )
  # Save methods:
  # overview of the methods in mice can be found by
  # methods(mice)
  # dry_mice$meth
  # Change as eg:
  # meth["bmi"] <- "norm"
  fwrite(as.list(dry_mice$meth),
         sprintf('methods_%s', output_file_name),
         sep = '\t',
         na = 'NA',
         col.names = TRUE,
         row.names = FALSE
  )
  # And quit after this:
  print('Dry run finished, exiting.')

  return(something_I_need)
  }
