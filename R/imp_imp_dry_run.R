#' @title Wrapper function for a dry imputation run using mice
#'
#' @description imp_imp_dry_run() runs a dry imputation to get methods and
#' predictor matrix
#'
#' @param data a data frame or matrix to impute, passed to 'data' in mice::mice()
#'
# @param maxit Number of iterations required, must be 0
#'
#' @param print If TRUE, mice will print history on console. Default is FALSE
#'
#' @param output_suffix Suffix string for outfiles, default is 'dry_run.tsv'
#'
#' @param ... pass any other mice::mice() parameters
#'
#' @return Saves the predictor matrix and default methods that mice will use
#'
#' @note Run this first, modify and pass 'method' and 'predictorMatrix' parameters in mice()
#'
#' @author Antonio J Berlanga-Taylor, George Adams, Deborah Schneider-Luftman
#'         <\url{https://github.com/EpiCompBio/bigimp}>
#'
#' @seealso \code{\link[mice]{mice}}, <\url{https://stefvanbuuren.name/fimd/}>,
#' <\url{https://stefvanbuuren.name/mice/}>,
#' \code{\link[data.table]{fwrite}}.
#'
#' @examples
#'
#' \dontrun{
#'  my_data <- read.csv('my_file_with_missing_data.tsv', sep = '\\t')
#'  imp_imp_dry_run(my_data)
#'  dry_mice$pred # inspect prediction matrix that will be used, saved to disk
#'  dry_mice$meth # inspect methods that will be used, saved to disk
#'  # Modify methods or predictor matrix and overwrite if needed:
#'  pred[ ,"hyp"] <- 0
#'  meth["bmi"] <- "norm"
#'  # Save files or pass to imputation function
#'  # Examples from https://stefvanbuuren.name/mice/
#' }
#'
#' @export
#'

imp_imp_dry_run <- function(data = NULL,
                            # maxit = 0,
                            print = FALSE,
                            output_suffix = 'dry_run.tsv',
                            ...
                            ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('mice', quietly = TRUE)) {
  stop('Package mice needed for this function to work. Please install it.',
  call. = FALSE)
  }
if (!requireNamespace('data.table', quietly = TRUE)) {
  stop('Package data.table needed for this function to work. Please install it.',
  call. = FALSE)
  }
  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 497

  print('Running a dry imputation to get methods and predictor matrix.')
  dry_mice <- mice::mice(data = data,
                         maxit = 0,
                         print = print,
                         ...
                        )
  # TO DO: move separately
  # Save predictor matrix:
  # pred[ ,"hyp"] <- 0
  data.table::fwrite(as.data.frame(dry_mice$pred),
                     sprintf('predictor_matrix_%s', output_suffix),
                     sep = '\t',
                     na = 'NA',
                     col.names = TRUE,
                     row.names = TRUE,
                     quote = FALSE
                     )
  # TO DO: move separately
  # Save methods:
  # overview of the methods in mice can be found by
  # methods(mice)
  # dry_mice$meth
  # Change as eg:
  # meth["bmi"] <- "norm"
  data.table::fwrite(as.list(dry_mice$meth),
                     sprintf('methods_%s', output_suffix),
                     sep = '\t',
                     na = 'NA',
                     col.names = TRUE,
                     row.names = FALSE
                    )
  # And quit after this:
  print('Dry run finished, exiting.')
  return(dry_mice)
  }
