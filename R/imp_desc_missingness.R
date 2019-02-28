#' @title
#'
#' @description imp_desc_missingness()
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

imp_desc_missingness <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
  }
  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 638

  # Save missingness pattern:
  fwrite(as.data.frame(missingness),
         sprintf('missingness_pattern_%s.tsv', output_name),
         sep = '\t',
         na = 'NA',
         col.names = TRUE,
         row.names = TRUE, # keep as these are the ID column
         quote = FALSE
  )
  # TO DO: save as table with caption
  # each row corresponds to a missing data pattern (1=observed, 0=missing).
  # Rows and columns are sorted in increasing amounts of missing information.
  # number of rows is equal to the number of patterns identified
  # first column (no header) x observations with y vars missing
  # The last column and row contain row and column counts, respectively.
  # lower right corner = total missing data points
  # last row shows total missing values for each variable
  # last column (no header) shows number of missing variables
  return(something_I_need)
  }


imp_desc_prop_missing <- function(param1 = some_default,
                                 ...
                                 ) {
  # Use this instead or library or require inside functions:
  if (!requireNamespace('some_pkg', quietly = TRUE)) {
    stop('Package some_pkg needed for this function to work. Please install it.',
         call. = FALSE)
  }
  # Check proportion of missing data:
  prop_NA <- function(x) {sum(is.na(x)) / length(x) * 100}
  # Individuals with more than X% of missing variables:
  rows_missing <- apply(input_data, 1, prop_NA) # by rows
  rows_above_cut <- nrow(input_data[which(rows_missing > missingness_cut), ])
  print(sprintf('Number of rows with >%s%% missing data: %s',
                missingness_cut, rows_above_cut))
  # By columns:
  cols_missing <- apply(input_data, 2, prop_NA)
  cols_above_cut <- ncol(input_data[, which(cols_missing > missingness_cut)])
  print(sprintf('Number of columns with >%s%% missing data: %s',
                missingness_cut, cols_above_cut))
  }
