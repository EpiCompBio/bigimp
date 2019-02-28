#' @title
#'
#' @description imp_stats_ks()
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

imp_stats_ks <- function(param1 = some_default,
               ...
               ) {
# Use this instead or library or require inside functions:
if (!requireNamespace('some_pkg', quietly = TRUE)) {
  stop('Package some_pkg needed for this function to work. Please install it.',
  call. = FALSE)
  }

  ########
  # This from George:
  # TO DO
  # divide into separate functions
  # most are plot or stats, place as eg imp_stats_skew, etc. or as appropriate
  # missing plot functions?

  ### Assessment of the quality of replacement

  # setwd("~/Dropbox/010 EPIDEMIOLOGY Msc/026 Research Module/017_testdata/POST_MSC_DATA")



  #ls <- readRDS("VERSION2_TESTTRAIN.rds")
  ls <- readRDS("PRACTISE2_test_train.rds")
  train <- ls$train
  test <- ls$test
  train <- rbind(train, test)

  #train <- rbind(train, test)


  eid <- train$eid


  # # # train %<>% select(-eid)
  # add_random_nas_to_frame <- function(frame, num_features) {
  #   col_order <- names(frame)
  #   rand_cols <- sample(ncol(frame), num_features)
  #   left_overs <- which(!names(frame) %in% names(frame[,rand_cols]))
  #   other_frame <- frame[,left_overs]
  #   nas_added <- data.frame(lapply(frame[,rand_cols], function(x) x[sample(c(TRUE, NA), prob = c(sample(100, 1)/100, 0.15), size = length(x), replace = TRUE)]))
  #   final_frame <- cbind(other_frame, nas_added)
  #   final_frame <- final_frame[,col_order]
  #   return(final_frame)
  # }
  #
  # #
  # train_na <- add_random_nas_to_frame(train, 10)

  # anyNA(train_na)




  library(magrittr)
  library(dplyr)
  library(tidyr)

  ##################################################################
  ### ASSESSING IMPUTATION #########################################
  ##################################################################

  ks_comparison <- function(train, train_na){

    train %<>% select(-eid)
    train_na %<>% select(-eid)

    train %<>% as_tibble()
    train_na %<>% as_tibble()
    ## KS-test comparison
    res = rbind("Statistic", "P.value")
    colnames(res)[1] <- "value"
    for (i in 1:ncol(train)) {
      ks_out <- ks.test(as.matrix(train[,i]), as.matrix(train_na[,i]))
      res = cbind(res,c(ks_out$statistic, ks_out$p.value))
      colnames(res)[i+1] <- colnames(train[,i])
    }


    res %<>% as_tibble()  ## outcome1

    return(res)
  }

  res <- ks_comparison(train, train_na)  ## works

  #################################################################
  #### CALCULATE MOMENTS ##########################################
  #################################################################


  MomentsAssessment <- function(train, train_na){

    library(magrittr)
    library(dplyr)
    library(tidyr)
    #######################################
    ########### SKEW ##################
    #######################################

    moment3 <- function(train, train_na){

      train %<>% select(-eid)
      train_na %<>% select(-eid)


      output_skew_head = matrix(NA, ncol = 3)
      colnames(output_skew_head) <- c("PRE_skew", "POST_skew", "RATIO_skew")
      output_skew_head %<>% as_tibble()
      output_skew_head[,]
      #install.packages("moments")
      library(moments)
      for (i in 1:ncol(train)){
        skew_pre = skewness(train_na[,i], na.rm = TRUE)
        skew_post = skewness(train[,i], na.rm = TRUE)
        RESULT <- skew_pre/skew_post
        output_skew=c(skew_pre, skew_post, RESULT)
        output_skew_head <- rbind(output_skew_head, output_skew)
      }
      output_skew_head <- output_skew_head[-1,]
      output_skew_head$names_skew <- colnames(train)

      #output_skew_head$RESULT[is.na(output_skew_head$RESULT)] <- 0

      return(output_skew_head)
    }

    output_skew_head <- moment3(train, train_na)  # works

    #######################################
    ########### KURTOSIS ##################
    #######################################

    moment4 <- function(train, train_na){

      train %<>% select(-eid)
      train_na %<>% select(-eid)


      output_kurtosis_head = matrix(NA, ncol = 3)
      colnames(output_kurtosis_head) <- c("PRE_kurtosis", "POST_kurtosis", "RATIO_kurtosis")
      output_kurtosis_head %<>% as_tibble()

      #install.packages("moments")
      #library(moments)
      for (i in 1:ncol(train)){
        kurtosis_pre = kurtosis(train_na[,i], na.rm = TRUE)
        kurtosis_post = kurtosis(train[,i], na.rm = TRUE)
        RESULT <- kurtosis_pre/kurtosis_post
        output_kurtosis=c(kurtosis_pre, kurtosis_post, RESULT)
        output_kurtosis_head <- rbind(output_kurtosis_head, output_kurtosis)
      }

      output_kurtosis_head <- output_kurtosis_head[-1,]

      output_kurtosis_head$names_kurt <- colnames(train)

      #output_kurtosis_head$RESULT[is.na(output_kurtosis_head$RESULT)] <- 0

      return(output_kurtosis_head)
    }

    output_kurtosis_head <- moment4(train, train_na)  # this works

    #######################################
    ########### VARIANCE (f test) #########
    #######################################

    moment2 <- function(train, train_na){

      train %<>% select(-eid)
      train_na %<>% select(-eid)

      output_var_head = matrix(NA, ncol = 3)
      colnames(output_var_head) <- c("PRE_var", "POST_var", "RATIO_var")
      output_var_head %<>% as_tibble()

      library(moments)
      #install.packages("moments")
      #library(moments)
      for (i in 1:ncol(train)){
        var_pre = var(train_na[,i],na.rm = TRUE)
        var_post = kurtosis(train[,i], na.rm= TRUE)
        RESULT <-var_pre/var_post
        output_var=c(var_pre, var_post, RESULT)
        output_var_head <- rbind(output_var_head, output_var)
      }

      output_var_head <- output_var_head[-1,]



      output_var_head$names_var <- colnames(train)


      #output_var_head$RESULT[is.na(output_var_head$RESULT)] <- 0

      return(output_var_head)
    }

    output_var_head <- moment2(train, train_na)   ## this works


    #######################################
    ########### MEAN #########
    #######################################
    moment1 <- function(train, train_na){
      train %<>% select(-eid)
      train_na %<>% select(-eid)


      output_mean_head = matrix(NA, ncol = 3)
      colnames(output_mean_head) <- c("PRE_mean", "POST_mean", "RATIO_mean")
      output_mean_head %<>% as_tibble()
      tr_m <-as.matrix(train)
      tr_na_m <- as.matrix(train_na)

      for (i in 1:ncol(train)){
        mean_post = mean(tr_m[,i], na.rm = TRUE)
        mean_pre = mean(tr_na_m[,i],na.rm = TRUE)
        RESULT <-mean_pre/mean_post
        output_mean=c(mean_pre, mean_post, RESULT)
        output_mean_head <- rbind(output_mean_head, output_mean)
      }
      output_mean_head <- output_mean_head[-1,]

      output_mean_head$names_mean <- colnames(train)

      #output_mean_head$RESULT[is.na(output_mean_head$RESULT)] <- 0

      return(output_mean_head)
    }

    output_mean_head <- moment1(train, train_na)  ## works

    #### bring the moment datatogether
    df_moments <- cbind(output_mean_head, output_var_head, output_skew_head, output_kurtosis_head)



    return(df_moments)
  }

  #########################################

  output1<- MomentsAssessment(train, train_na)
  output1  ## this works
  ####### END FUNCTION


  ##### IMPUTED RESULTS

  # dim(train_na) == dim(train)
  #
  # train[train != train_na,]
  # train[is.na(train_na),]
  #
  # for (i in 1:ncol(train)){
  #   for (j in 1:nrow(train)){
  #
  #     if (is.na(train_na[j,i])){
  #       output[i,j] <- train[j,i]
  #     }else{output[i,j] <- NA}
  #   }
  # }
  #
  # output
  #
  #
  # output = matrix(NA, ncol = ncol(train), nrow = nrow(train))
  # colnames(output) <- colnames(train)
  #
  # dim(output); dim(train); dim(train_na)

  ################################################################
  # FUNCTION TO IDENTIFY THE IMPUTED VALUES                      #
  ################################################################

  presentNA <- function(train, train_na){

    output = matrix(NA, ncol = ncol(train), nrow = nrow(train))
    colnames(output) <- colnames(train)
    output %<>% as.data.frame()

    for (i in 1:ncol(train)){
      for (j in 1:nrow(train)){
        if (is.na(train_na[j,i])){
          x <- train[j,i]
          output[j,i] <- x
        }else{output[j,i] <- NA}
      }
    }

    return(output)
  }


  output <- presentNA(train, train_na)

  ## This works

  #### UNFORTUNATELY THESE DON"T WORK WITH THE KS_COMPARISON TEST


  ### ASSESSMENT OF MOMENTS of the imputated dataset!
  output2 <- MomentsAssessment(train, output)

  ###################################################


  output_final = list("moments_pre_post" = output1,
                      "moments_imputed_post" = output2,
                      "ks_pre_post" = res)


  saveRDS(output_final, "output_imputation_assessment.rds")

  #####
  return(something_I_need)
  }
