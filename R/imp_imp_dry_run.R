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
# @param output_suffix Suffix string for outfiles, default is 'dry_run.tsv'
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
#' \code{\link[data.table]{fwrite}},
#' #' \code{\link[bigimp]{imp_imp_mice}}.
#'
#' @examples
#'
#' \dontrun{
#' library(mice)
#' library(data.table)
#' # my_data <- read.csv('my_file_with_missing_data.tsv', sep = '\t')
#' # Or use a pre-loaded dataset from mice:
#' my_data <- nhanes
#' dry_mice <- imp_imp_dry_run(my_data)
#' dry_mice$pred # inspect prediction matrix that will be used
#' dry_mice$meth # inspect methods that will be used
#' # Modify methods or predictor matrix and overwrite if needed:
#' dry_mice$pred[, 'hyp'] <- 0
#' dry_mice$method['bmi'] <- 'norm'
#' # An overview of the methods in mice can be found with:
#' methods(mice)
#' # Save files or pass to imputation function:
#' output_suffix <- 'dry_run.tsv'
#' data.table::fwrite(as.data.frame(dry_mice$pred),
#'                    sprintf('predictor_matrix_%s', output_suffix),
#'                    sep = '\t',
#'                    na = 'NA',
#'                    col.names = TRUE,
#'                    row.names = TRUE,
#'                    quote = FALSE
#'                    )
#' # Save methods:
#' data.table::fwrite(as.list(dry_mice$methods),
#'                    sprintf('methods_%s', output_suffix),
#'                    sep = '\t',
#'                    na = 'NA',
#'                    col.names = TRUE,
#'                    row.names = FALSE
#'                   )
#' # Examples from https://stefvanbuuren.name/mice/
#' }
#'
#' @export
#'

imp_imp_dry_run <- function(data = NULL,
                            # maxit = 0,
                            print = FALSE,
                            # output_suffix = 'dry_run.tsv',
                            ...
                            ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('mice', quietly = TRUE)) {
  stop('Package mice needed for this function to work. Please install it.',
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
  # And quit after this:
  print('Dry run finished.')
  return(dry_mice)
  }
