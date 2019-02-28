#' @title
#'
#' @description imp_utils_chunk()
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

imp_utils_chunk <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
  }

  ########
  # This from George
  #### CHUNKUKB - function

  #### This function divides the dataset into a 'N' number of "chunks:

  ################## input variables;
  ### N = number of chunks to make
  ### seed = the set.seed(XXX) value to use
  ### data = data to input (ukb)


  #setwd("~/Dropbox/010 EPIDEMIOLOGY Msc/026 Research Module/016_github_msc/UPDATED_CLEANING")

  ### FUNCTION

  chunkukb <- function(data, n, seed){

    set.seed(seed) ## set.seed

    sample_size <- floor((1/n)* nrow(data)) # fraction of the data


    data <- ukb ## setup for the loop

    for (j in 1:n){
      new_data <- data
      train_ind <- sample(seq_len(nrow(new_data)), size = sample_size)
      setq <- new_data[train_ind, ]
      data <- new_data[-train_ind, ]
      assign(paste0("set",j),setq)
    }


    ## set up the equation
    lhs <- paste0("set", 1:n)
    rhs <- sprintf("'ukb_q%.f'", 1:n)
    equationtext <- paste(paste(rhs, lhs, sep = "="), collapse = ", ")
    equationtext2 <- sprintf("ls = list( %s, 'ukb_q%.f' = data )",equationtext, n+1)

    eval(parse(text = equationtext2)) ## parse the equation to create 'ls'
    return(ls)
  }

  ############## RUN THE FUNCTION #######################
  n <- 10 ## the number of chunks to use
  ls <-chunkukb(data = ukb, seed = 123, n = n)
  ## ls is a list of the chunks of the ukb dataframe


  ############## WRITE TO .csv FILES ####################

  for (k in 1:n+1) {
    write_csv(as.data.frame(ls[k]), sprintf("ukb_q%.f.csv", k))  ## write the files
  }
  ########



  return(something_I_need)
  }
