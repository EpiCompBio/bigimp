#' @title
#'
#' @description imp_imp_mice()
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

imp_imp_mice <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
  }
if(param1 == TRUE) {
  print('something')
  } else {
      print('something else')
  }
  # this is from stats_utils/stats_utils/run_mice_impute.R
  # lines 747
  # parallel lines from 608, 795

  # TO DO:
  # Start and stop cluster functions:
  # See:
  # https://github.com/AntonioJBT/episcout/blob/master/R/epi_utils_multicore.R
  # decide where to move them to, skip or import (library('episcout'))

  # Setup the cluster
  # FORK runs only in Unix like, PSOCK is default but needs env vars passed to each core
  cl <- makeCluster(num_cores, type = "FORK")
  # Pass a seed:
  clusterSetRNGStream(cl, iseed = seed)
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

  # TO DO: check adding extension works OK, when parallelising and with ibind()
  # Only run if -I given but without --extend
  if (!is.null(args[['-I']]) &  # arg is NULL
      args[['--extend']] == FALSE) {  # arg is boolean
    print('Starting imputations.')
    print(sprintf('Total number of imputed datasets to complete: %s', num_cores * m))
    imp_pars <-
      parLapply(cl = cl,
                X = 1:num_cores,
                fun = function(no) {
                  mice(input_data,
                       m = m, # Number of imputed datasets, 5 is default
                       maxit = maxit, # max iterations per imputation
                       # quickpred = set the minimum correlation for variable
                       # selection in the predictor matrix:
                       pred = pred,
                       print = F, # omit printing of the iteration cycle
                       diagnostics = TRUE,
                       meth = meth,
                       seed = seed
                  )
                }
      )
  } else if (!is.null(args[['-I']]) & # if both arguments are given run
             args[['--extend']] == TRUE) {
    # imp_pars <- mice.mids(input_data, maxit = 35, print = F)
    print('Extending iterations.')
    imp_pars <-
      parLapply(cl = cl,
                X = 1:num_cores,
                fun = function(no) {
                  mice.mids(input_data,
                            maxit = maxit, # max iterations per imputation
                            print = F # omit printing of the iteration cycle
                  )
                }
      )
    # plot(imp_pars)
  }

  # Merge the datasets and create a mids object:
  imp_merged <- imp_pars[[1]]
  for (n in 2:length(imp_pars)) {
    imp_merged <- mice::ibind(imp_merged,
                              imp_pars[[n]])
  }
  ##########

  ##########
  # TO DO:
  # Move this/ use function, see:
  # https://github.com/AntonioJBT/episcout/blob/master/R/epi_utils_multicore.R
  # Stop cluster and free up the cores taken:
  stopCluster(cl)
  gc(verbose = TRUE) # Prob not necessary but ensure R returns memory to the OS
  return(something_I_need)
  }
