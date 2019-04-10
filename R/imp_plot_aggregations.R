#' @title Get missing and imputed values per variable
#'
#' @description imp_plot_aggregations() wraps VIM::aggr() function. Convenienty
#' plots and saves results to file.
#'
#' @param data A datafame with missing or imputed data.
#' @param vars_interest = NULL
#' @param output_name string for plot and table. Strings are composed as
#' 'missingness_vars_interest%s.txt' and 'missingness_vars_interest%s.svg'.
#' Default is empty ''.
#' @param get_plot Determines whether an svg plot should be created and saved.
#' Default is TRUE
#' @param save_table Save the missing/imputed aggregations to file.
#' Default is TRUE
#'
#' @return Returns the aggr() object. Also log info to screen,
#' plot and missing aggregations pattern to file.
#'
#' @note
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/EpiCompBio/bigimp}>
#'
#' @seealso \code{\link{imp_plot_missingness}},
#' \code{\link[VIM]{aggr}}.
#'
#' @note Many values for VIM::aggr() are hard-coded for convenience, this is intended
#' for faster exploring and processing. Use the original function for tuning.
#'
#' @examples
#'
#' \dontrun{
#' library(mice)
#' library(VIM)
#' nhanes
#' vim_aggr_plot <- imp_plot_aggregations(nhanes)
#' # Explore object:
#' vim_aggr_plot$missings
#' summary(vim_aggr_plot) # If save_table is TRUE the output is saved
#' plot(vim_aggr_plot) # If get_plot is TRUE this is saved to a file
#' # Plot and look at only a subset of variables:
#' data <- nhanes
#' vars_interest <- c('age', 'bmi')
#' vim_aggr_plot <- imp_plot_aggregations(data = data,
#'                                        vars_interest = vars_interest,
#'                                        output_name = '_bmi_age'
#'                                        )
#' # The above will only look at BMI and age and will save
#' # the files 'missingness_vars_interest_bmi_age' svg plot and log info (txt)
#' }
#'
#' @export
#'

# TO DO: values are hard-coded for convenience, prob not worth changing though
# @param ... pass any other

imp_plot_aggregations <- function(data = NULL,
                                  vars_interest = NULL,
                                  output_name = '',
                                  get_plot = TRUE,
                                  save_table = TRUE,
                                  ...
                                  ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('VIM', quietly = TRUE)) {
  stop('Package VIM needed for this function to work. Please install it.',
  call. = FALSE)
  }

  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines ~691

  # See pattern using VIM and mice libraries
  # Plot aggregations for missing/imputed values:
  # TO DO: could move this to post imputation to make full use

  if (!is.null(vars_interest)) {
      data <- data[, vars_interest]
   } #else {
  #     data <- data
  # }
  if (get_plot == TRUE) {
    # TO DO: print out legend
    svg(sprintf('missingness_vars_interest%s.svg', output_name))
    # TO DO: save legend
    aggr_plot <- VIM::aggr(x = data,
                           combined = FALSE, # plot bar and pattern separately
                           only.miss = FALSE, # Plot combinations only for missing variables
                           numbers = TRUE,
                           sortVars = TRUE,
                           labels = names(data),#[, vars_interest]),
                           cex.axis = 0.4,
                           gap = 2,
                           ylab = c('Proportion of missing data', 'Pattern'),
                           plot = get_plot,
                           ...
                           )
    dev.off()
    } else {
      aggr_plot <- VIM::aggr(x = data,
                             combined = FALSE, # plot bar and pattern separately
                             only.miss = FALSE, # Plot combinations only for missing variables
                             numbers = TRUE,
                             sortVars = TRUE,
                             labels = names(data),#[, vars_interest]),
                             cex.axis = 0.4,
                             gap = 2,
                             ylab = c('Proportion of missing data', 'Pattern'),
                             plot = get_plot,
                             ...
                             )
        }
  if (save_table == TRUE) {
    # this is from stats_utils/stats_utils/run_mice_impute.R
    # lines 638
    # Save aggregation plot summary as log for information only
    # Provides counts and proportion of NAs

    sink(sprintf('missingness_vars_interest%s.txt', output_name),
         append = FALSE,
         split = TRUE,
         type = c("output", "message")
         )
    print(summary(aggr_plot))
    sink(file = NULL) # stop sinking
  } else {
      # just print to screen for info:
      print(summary(aggr_plot))
    }

  return(aggr_plot)
  }
