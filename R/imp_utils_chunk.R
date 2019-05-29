#' @title Divide a datasets into n random chunks
#'
#' @description imp_utils_chunk() takes a dataframe randomly assigns
#' rows into separate chunks and writes these to file.
#'
#' @param data A dataframe to be randomly divided (chunks).
#'
#' @param n number of chunks to make. Default is 10.
#'
#' @param seed A seed number to pass for parallel work. Default is 12345
#'
#' @param output_name string for naming dataframe chunks. Strings are composed
#' as '%schunk#.tsv' where # is the sequence number. Default is empty ''.
#'
#' @return Returns n dataframes which are subsets of the input data
#'
#' @note The dataframe is expected to have rows as observations/samples and
#' columns as features/variables. Rows are taken randomly without replacement.
#'
#' @author George Adams, Antonio J Berlanga-Taylor <\url{https://github.com/EpiCompBio/bigimp}>
#'
#' @seealso \code{\link{functioname}},
#' \code{\link[packagename]{functioname}}.
#'
#' @examples
#'
#' \dontrun{
#' library(mice)
#' library(episcout)
#' my_data <- nhanes
#' my_data <- rbind(nhanes, nhanes, nhanes, nhanes)
#' dim(my_data)
#' # chunk_list is a list of the chunks of the ukb dataframe:
#' chunk_list <- imp_utils_chunk(data = my_data)
#' str(chunk_list)
#' # Write the files:
#' for (k in 1:(length(chunk_list) - 1)) # TO DO: check why this was +1, last element is empty data.frame
#'   epi_write(as.data.frame(chunk_list[k]),
#'             sprintf("my_data%.f.tsv", k)
#'            )
#' }
#'
#' @export
#'

imp_utils_chunk <- function(data = NULL,
                            n = 10,
                            seed = 12345,
                            output_name = '',
                            ...
                            ) {
  #### This function divides the dataset into a 'N' number of "chunks:
  ################## input variables
  ### N = number of chunks to make
  ### seed = the set.seed(XXX) value to use
  ### data = data to input (ukb)
  set.seed(seed = seed) ## set.seed
  sample_size <- floor((1 / n) * nrow(data)) # fraction of the data
  for (j in 1:n) {
    new_data <- data
    train_ind <- sample(seq_len(nrow(new_data)), size = sample_size)
    setq <- new_data[train_ind, ]
    data <- new_data[-train_ind, ]
    assign(paste0('set', j), setq)
  }

  ## set up the equation
  lhs <- paste0("set", 1:n)
  rhs <- sprintf("'%schunk%.f'", output_name, 1:n)
  equationtext <- paste(paste(rhs, lhs, sep = "="), collapse = ", ")
  equationtext2 <- sprintf("chunk_list = list( %s, '%schunk%.f' = data )",
                           equationtext,
                           output_name,
                           n + 1
                           )

    eval(parse(text = equationtext2)) ## parse the equation to create 'chunk_list'
    return(chunk_list)
  }

