#' @title Get missingness pattern
#'
#' @description imp_plot_missingness() generates pattern and plot, saves both to file.
#'
#' @param data A datafame with missing data.
#'
#' @param get_plot Determines whether an svg plot should be created and saved.
#' Default is TRUE
#'
#' @param save_table Save the missingness pattern to file. Default is TRUE
#'
#' @param missingness_cut Proportion to use to report number of rows and columns
#' missing. Default is 30.
#'
#' @param output_name string for plot and table. Strings are composed as
#' 'missingness_pattern%s.tsv' and 'missingness_pattern%s.svg'. Default is empty ''.
#'
#' @return
#'
#' @note missingness_cut is only used to print to screen the number of rows and
#' columns with more than x missing data
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/EpiCompBio/bigimp}>
#'
#' @seealso \code{\link[mice]{md.pattern}},
#' \code{\link[episcout]{epi_stats_na_perc}}.
#'
#' @examples
#'
#' \dontrun{
#' library(mice)
#' imp_plot_missingness(nhanes)
#' dir() # a tsv and svg files should be saved with the missingness pattern and plot
#' }
#'
#' @export
#'

imp_plot_missingness <- function(data = NULL,
                                 get_plot = TRUE,
                                 output_name = '',
                                 save_table = TRUE,
                                 missingness_cut = 30
                                 ) {
# Use this instead or library or require inside functions:
  if (!requireNamespace('mice', quietly = TRUE)) {
    stop('Package mice needed for this function to work. Please install it.',
    call. = FALSE)
  }
  if (!requireNamespace('mice', quietly = TRUE)) {
    stop('Package mice needed for this function to work. Please install it.',
         call. = FALSE)
  }
  # Inspect the missing data pattern:
  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 625

  if (get_plot == TRUE) {
  # TO DO: print out legend
  svg(sprintf('missingness_pattern%s.svg', output_name))
  missingness <- mice::md.pattern(x = data, plot = get_plot)
  dev.off()
  } else {
    missingness <- mice::md.pattern(x = data, plot = get_plot)
  }
  if (save_table == TRUE) {
  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 638
  # Save missingness pattern:
  data.table::fwrite(as.data.frame(missingness),
                     sprintf('missingness_pattern%s.tsv', output_name),
                     sep = '\t',
                     na = 'NA',
                     col.names = TRUE,
                     row.names = TRUE, # keep as these are the ID column
                     quote = FALSE
                     )
  }

  # Check proportion of missing data:
  prop_NA <- function(x) {sum(is.na(x)) / length(x) * 100}
  # Individuals with more than X% of missing variables:
  rows_missing <- apply(data, 1, prop_NA) # by rows
  rows_above_cut <- nrow(data[which(rows_missing > missingness_cut), ])
  print(sprintf('Number of rows with >%s%% missing data: %s',
                missingness_cut, rows_above_cut))
  # By columns:
  cols_missing <- apply(data, 2, prop_NA)
  cols_above_cut <- ncol(data[, which(cols_missing > missingness_cut)])
  print(sprintf('Number of columns with >%s%% missing data: %s',
                missingness_cut, cols_above_cut))

  return(missingness)
}

  # TO DO:
  # Does the missing data of var x depend on var y?
  # Plot histograms conditional on missingness for vars_interest eg:
  # https://gerkovink.github.io/miceVignettes/Missingness_inspection/Missingness_inspection.html
  # (in step 8)
  # R <- is.na(boys$gen)
  # histogram(~age|R, data=boys)

  # Get some basic stats to print to screen
  # Use episcout::epi_stats_na_perc() for NA counts and percentage, returns a dataframe


  # TO DO: save as table with caption
  # each row corresponds to a missing data pattern (1=observed, 0=missing).
  # Rows and columns are sorted in increasing amounts of missing information.
  # number of rows is equal to the number of patterns identified
  # first column (no header) x observations with y vars missing
  # The last column and row contain row and column counts, respectively.
  # lower right corner = total missing data points
  # last row shows total missing values for each variable
  # last column (no header) shows number of missing variables
