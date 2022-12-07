#' @title Impute missing data
#'
#' @description
#' This \code{impute} function is used impute the missing values of a data frame using a knn, missForest or mice approach.
#'
#' @details
#' The impute function is a wrapper for the following functions:
#' \href{https://www.rdocumentation.org/packages/class/versions/7.3-20/topics/knn}{knn} function,
#' \href{https://www.rdocumentation.org/packages/missForest/versions/1.5}{missForest} function,
#' \href{https://www.rdocumentation.org/packages/mice/versions/3.15.0/topics/mice}{mice} function,
#' and \href{https://rdrr.io/cran/sjmisc/man/merge_imputations.html}{merge_imputation} function.
#'
#' @param data data frame with missing values
#' @param method method to be applied to impute the missing values
#' @importFrom missForest missForest
#' @importFrom sjmisc merge_imputations
#' @importFrom VIM kNN
#'
#' @return a new data frame where the missing value are estimated based on the specified method
#'
#' @examples
#' # airquality is a data frame from base R
#'
#' new_df <- impute(airquality, "knn")
#' new_df <- impute(airquality, "missForest")
#' new_df <- impute(airquality, "mice")
#'
#' @export
#'

impute <- function(data, method = c("knn","missForest","mice")){
  # check if input is a data frame
  if (!is.data.frame(data)) stop("Your input should be a data frame")

  # check if method input is among listed ones
  method <- match.arg(method)

  # check if there's any missing data
  if (!any(is.na(data))) return(cat("No missing data"))

  if(method == "knn"){
    data_imp <- VIM::kNN(data, imp_var = FALSE)
  } else if (method == "missForest"){
    data_imp <- missForest::missForest(data)$ximp
  } else if (method == "mice"){
    imp <- mice::mice(data, printFlag = FALSE)
    x <- sjmisc::merge_imputations(data, imp)
    data_imp <- cbind(data[colSums(sapply(data, is.na))==0], x)
  }
  return(data_imp)
}
