#' @title Parallel imputation using mice
#'
#' @description imp_imp_mice() imputes in parallel missing data using the
#' mice package
#'
#' @param data a data frame or matrix to impute, passed to 'data' in mice::mice()
#' @param mincor Minimum correlation for quickpred. Default: 0.3
#' @param minpuc Minimum proportion of usable cases for predictors to be used for imputation
#'               Default is 0.3
#' @param m Number of imputed datasets to return. Default is 5
#' @param maxit Number of iterations per dataset to impute. Default is 30
#' @param print If TRUE, mice will print history on console. Default is FALSE
# @param diagnostics: TO CHECK: this was in stats_utils, unsure what it provides, not a method for mice::mice()
#' @param seed A seed number to pass for parallel work. Default is 12345
#' @param num_cores Number of cores to use. Default is 4
#' @param cl_type Type of cluster to use. Default is "FORK"
#' @param ... pass any other mice::mice() parameters
#'
#' @return
#'
#' @note Parallelizes imputation, see makeCluster from parallel. The total number
#' of imputed datasets will be num_cores * m.
#'
#' @author Antonio J Berlanga-Taylor, George Adams, Deborah Schneider-Luftman
#'         <\url{https://github.com/EpiCompBio/bigimp}>
#'
#' @seealso \code{\link[mice]{mice}},
#' <\url{https://stefvanbuuren.name/fimd/}>,
#' <\url{https://stefvanbuuren.name/mice/}>,
#' \code{\link[parallel]{parLapply}},
#' \code{\link[parallel]{makeCluster}},
#' \code{\link[data.table]{fwrite}},
#' \code{\link[bigimp]{imp_imp_dry_run}}.
#'
#' @examples
#'
#' \dontrun{
#' # See example in imp_imp_dry_run()
#' library(mice)
#' library(parallel)
#' # my_data <- read.csv('my_file_with_missing_data.tsv', sep = '\\t')
#' my_data <- nhanes
#' imp_imp_dry_run(my_data)
#' imp <- imp_imp_mice(data = my_data, num_cores = 3)
#' # Explore the imputed object:
#' imp$data
#' imp$imp
#' imp$call
#'
#' }
#'
#' @export
#'

imp_imp_mice <- function(data = NULL,
                         mincor = 0.3, # set the minimum correlation for variable
                         minpuc = 0.3,
                         m = 5, # Number of imputed datasets
                         maxit = 30, # max iterations per imputation
                         print = FALSE, # omit printing of the iteration cycle
                         # diagnostics = TRUE,
                         # methods and predictor matrix:
	                     pred = NULL,
	                     method = NULL,
                         seed = 12345,
                         num_cores = 4,
                         cl_type = "FORK",
                         ...
                         ) {
# Use this instead or library or require inside functions:
  if (!requireNamespace('mice', quietly = TRUE)) {
    stop('Package mice needed for this function to work. Please install it.',
         call. = FALSE)
    }

  # Set-up options:
  if (!is.null(pred)) {
    print('Using predictor matrix provided')
  }
  	else {
  	  pred <- mice::quickpred(data = data,
                              mincor = mincor,
                              minpuc = minpuc
                             )
      print('Predictor matrix not provided, ')
      print('using defaults with quickpred and ')
      print(sprintf('%s for minimum correlation between variables.', mincor))
      print(sprintf('%s for minimum proportion of usable cases.', minpuc))
    }


  # Start and stop cluster functions:
  # See also:
  # https://github.com/AntonioJBT/episcout/blob/master/R/epi_utils_multicore.R

  # Setup the cluster
  # FORK runs only in Unix like, PSOCK is default but needs env vars passed to each core
  cl <- parallel::makeCluster(num_cores,
                              type = cl_type
                              )
  # Pass a seed:
  parallel::clusterSetRNGStream(cl, iseed = seed)
  # Use the following if PSOCK is needed:
  # Export variables and libraries to so that they are available to all cores:
  # clusterExport(cl, input_data) # export all objects needed for function
  # clusterEvalQ(cl, library(mice)) # export all libraries needed
  # At the end run stopCluster(cl)
  # run gc() and rm() if needed # only gc() for garbage collection

  # Run imputation:
  # The following will yield num_cores * m imputed datasets
  # which will be contained in imp_pars as a list object
  # Each list within, eg imp_pars[[1]] will correspond to the structure of
  # a mids object, where imp_pars[[1]][1] is data,
  # imp_pars[[1]][2] contains the imputed data for each variable, etc.
  # mice::ibind merges and attributes it as class mids below

  print('Starting imputations.')
  print(sprintf('Total number of imputed datasets to complete: %s', num_cores * m))
  imp_pars <- parallel::parLapply(cl = cl,
                                  X = 1:num_cores,
                                  fun = function(no) {
                                            mice::mice(data = data,
                                                       # Number of imputed datasets:
                                                       m = m,
                                                       # max iterations per imputation:
                                                       maxit = maxit,
                                                       # omit printing of the iteration cycle:
                                                       print = print,
                                                       # diagnostics = diagnostics,
                                                       seed = seed,
                                                       method = method,
                                                       # pass predictor matrix:
                                                       predictorMatrix = pred,
                                                       ...
                                                       )
                   }
                       )
  # Merge the datasets and create a mids object:
  imp_merged <- imp_pars[[1]]
  for (n in 2:length(imp_pars)) {
    imp_merged <- mice::ibind(imp_merged,
                              imp_pars[[n]])
  }

  # Stop cluster and free up the cores taken:
  parallel::stopCluster(cl)
  gc(verbose = TRUE) # Prob not necessary but ensure R returns memory to the OS
  print('Finished running imputations.')
  return(imp_merged)
  }
