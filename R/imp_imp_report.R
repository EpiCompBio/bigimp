#' @title Get details from an imputation run
#'
#' @description imp_imp_report() prints or saves to file information from a mids object
#' (see mice::mice()). Get imputation methods, predictor matrix, call, etc.
#'
#' @param mids mids object to extract information from
#' @param output_name string for file names. Strings are composed as e.g.
#' 'imputation_report%' with txt (log) or tsv (methods and predictor matrix) suffixes.
#' Default is empty ''.
#'
#' @return Saves information from an imputation run
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/EpiCompBio/bigimp}>
#'
#' @seealso \code{\link[mice]{mice}},
#' \code{\link[data.table]{fwrite}},
#' \code{\link[bigimp]{imp_imp_mice}},
#' \code{\link[bigimp]{imp_imp_dry_run}},
#' \code{\link[episcout]{epi_write}}.
#'
#' @examples
#'
#' \dontrun{
#' library(mice)
#' library(data.table)
#' library(episcout)
#' # my_data <- read.csv('my_file_with_missing_data.tsv', sep = '\t')
#' # Or use a pre-loaded dataset from mice:
#' my_data <- nhanes
#' imp <- imp_imp_mice(my_data)
#' imp$predictorMatrix # inspect prediction matrix that will be used
#' imp$method # inspect methods that will be used
#' # Save imputation details:
#' imp_imp_report(mids = imp)
#' }
#'
#' @export
#'

imp_imp_report <- function(mids = NULL, output_name = '') {
  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 805-872
  # Save predictor matrix:
  output_name1 <- sprintf('predictor_matrix%s.tsv', output_name)
  episcout::epi_write(as.data.frame(mids$predictorMatrix),
                      row.names = TRUE,
                      file_name = output_name1
                      )
  print(sprintf('Saved file: %s', output_name1))
  # Save methods:
  output_name2 <- sprintf('methods%s.tsv', output_name)
  episcout::epi_write(as.list(mids$method),
                      row.names = TRUE,
                      file_name = output_name2
                      )
  print(sprintf('Saved file: %s', output_name2))

  output_name3 <- sprintf('imputation_report_log%s.txt', output_name)
  sink(output_name3,
       append = FALSE,
       split = TRUE,
       type = c("output", "message")
       )

  # Save call:
  print('Saving mice call details:')
  print(mids$call)

  # Save number of multiple imputations performed:
  print('Number of multiple imputations performed')
  print(mids$m)

  # Save formulas:
  print('Saving formulas:')
  print(mids$formulas)

  # Save seed:
  print('Saving seed specified:')
  print(mids$seed)

  # Save iterations:
  print('Saving number of iterations:')
  print(mids$iteration)

  # Save logged events:
  print('Saving logged events:')
  print(mids$loggedEvents)

  # Save version of mice:
  print('Saving version of mice:')
  print(mids$version)

  # Save date of analysis run:
  print('Saving date and time of analysis:')
  print(Sys.time())

  # End:
  sink(file = NULL) # stop sinking
  print(sprintf('Saved file: %s', output_name3))
  }
