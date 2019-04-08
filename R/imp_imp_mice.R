#' @title Parallel imputation using mice
#'
#' @description imp_imp_mice() imputes in parallel missing data using the
#' mice package
#'
#' @param data a data frame or matrix to impute, passed to 'data' in mice::mice()
#' @param m Number of imputed datasets to return. Default is 5
#' @param maxit Number of iterations per dataset to impute. Default is 30
#' @param quickpred Minimum correlation for quickpred. Default: 0.3
#' @param print If TRUE, mice will print history on console. Default is FALSE
#' @param diagnostics
#' @param pred = pred
#' @param meth = meth
#' @param seed A seed number to pass for parallel work. Default is 12345
#' @param output_suffix Suffix string for outfiles, default is 'imputed.tsv'
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

imp_imp_mice <- function(data = NULL,
                         m = 5, # Number of imputed datasets
                         maxit = 30, # max iterations per imputation
                         quickpred = 0.3, # set the minimum correlation for variable
                         # selection in the predictor matrix:
                         print = FALSE, # omit printing of the iteration cycle
                         diagnostics = TRUE,
                         pred = pred,
                         meth = meth,
                         seed = 12345,
                         print = FALSE,
                         output_suffix = 'imputed.tsv',
                         num_cores = 4,
                         cl_type = "FORK",
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
  # lines 747
  # parallel lines from 608, 795

  # Start and stop cluster functions:
  # See also:
  # https://github.com/AntonioJBT/episcout/blob/master/R/epi_utils_multicore.R

  # Setup the cluster
  # FORK runs only in Unix like, PSOCK is default but needs env vars passed to each core
  cl <- parallel::makeCluster(num_cores = num_cores,
                              type = cl_type)
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
  imp_pars <- parLapply(cl = cl,
                        X = 1:num_cores,
                        fun = function(no) {
                                  mice::mice(data = data,
                                  m = m, # Number of imputed datasets, 5 is default
                                  maxit = maxit, # max iterations per imputation
                                  quickpred = quickpred,
                                  print = print, # omit printing of the iteration cycle
                                  diagnostics = diagnostics,
                                  # selection in the predictor matrix:
                                  pred = pred,
                                  meth = meth,
                                  seed = seed,
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
  parallel::gc(verbose = TRUE) # Prob not necessary but ensure R returns memory to the OS
  # TO DO: write to disk?
  return(imp_merged)
  }
