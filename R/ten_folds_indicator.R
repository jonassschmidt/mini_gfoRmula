#' Adds 10 fold indicators while respecting id's. 
#'
#' @param D data frame
#'
#' @return D with an addition column for the 10 fold indicator
#' @export
#' 
#' @import dplyr
#'
#' @examples
#' ten_folds_indicator(gfoRmula::basicdata)
ten_folds_indicator <- function(D){
  D <- as_tibble(D)
  N <- max(D$id)
  folds <- sample(1:10, N, replace=T)
  D <- left_join(D, tibble(id=1:N, fold=folds))
  proportions <- round(table(D$fold)/length(D$fold),3)
  return(list(D, proportions))
}


