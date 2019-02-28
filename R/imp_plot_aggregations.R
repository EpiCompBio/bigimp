#' @title
#'
#' @description imp_plot_aggregations()
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

imp_plot_aggregations <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
  }

  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines ~691

  # See pattern using VIM and mice libraries
  # Plot aggregations for missing/imputed values:
  # TO DO: could move this to post imputation to make full use
  # input_data gets rewritten below though, and is used after input from both
  # long format for --extension and imputation
  svg(sprintf('missingness_vars_interest_VIM_%s.svg', output_name))
  # TO DO: save legend
  aggr_plot <- aggr(input_data,#[, vars_interest],
                    combined = FALSE, # plot bar and pattern separately
                    only.miss = FALSE, # Plot combinations only for missing variables
                    numbers = TRUE,
                    sortVars = TRUE,
                    labels = names(input_data),#[, vars_interest]),
                    cex.axis = 0.4,
                    gap = 2,
                    ylab = c('Proportion of missing data', 'Pattern')
  )
  dev.off()
  # TO DO:
  # save summary of aggregations for missing/imputed values as table in separate function:
  summary(aggr_plot)
  # Barplot (left) shows the proportion of missing or imputed values in each variable.
  # Aggregation plot (middle) shows all existing combinations of of  missing  (red),
  # imputed (orange) and observed (blue) values.
  # Barplot (right) shows the frequencies of different variable combinations
  return(something_I_need)
  }
