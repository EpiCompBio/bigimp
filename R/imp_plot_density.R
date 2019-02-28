#' @title
#'
#' @description imp_plot_density()
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

imp_plot_density <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
  }

  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 887

  # Explore by visualising the main variables of interest
  # Plot vars of interest original data:
  # TO DO: save legend
  out <- vector(mode = 'list', length = length(vars_interest))
  names(out) <- vars_interest
  for (i in vars_interest) {
    xlab <- sprintf('%s %s, observed values', input_name, i)
    out[[i]] <- densityplot(input_data[[i]],
                            xlab = xlab)
  }

  # Save to disk, one plot per file:
  for (i in names(out)) {
    plot_name <- sprintf('densityplots_%s_%s.svg', output_name, i)
    svg(plot_name)
    # cols_plot <- max(1, 2 %% length(vars_interest))
    # par(mfrow = c(length(vars_interest), cols_plot))
    print(out[[i]])
    dev.off()
  }

  # Plot all numerical variables with 2 or more missing values:
  # densityplot(imp_merged, ~ bmi) # will give only one var, has to be unquoted
  # densityplot(imp_merged, ~ bmi | .imp) # will plot each imputed dataset separately
  # TO DO: save legend
  # TO DO: this errors if there are only 2 missing rows:
  # if () {
  #   svg(sprintf('densityplots_imputation_%s.svg', output_name))
  #   lattice::densityplot(imp_merged)
  #   dev.off()
  # }

  # blue is observed, magenta imputed

  return(something_I_need)
  }
