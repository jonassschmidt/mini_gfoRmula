#' Add Specified Histories of Specified Columns to Data Set
#' 
#' The j'th entry in histories is the desired history to be added from the j'th entry in variables
#'
#' @param D data set
#' @param variables a vector containing variable names in the data set
#' @param histories a vector containing elements of the type: "lag_k", "cumsum", "cummean", "sum_prev_k"
#'
#' @return data frame
#' @export
#'
#' @import RcppRoll
#' @importFrom dplyr mutate group_by
#' @importFrom magrittr %>%
#'
#' @examples
#' add_histories(gfoRmula::basicdata[1:20,], c("L1","L2"), c("lag_1", "cumsum"))
#' add_histories(gfoRmula::basicdata[1:20,], c("L1","L2","L3","A"), c("lag_3", "cumsum", "cummean", "sum_prev_2"))
add_histories <- function(D, variables, histories){
  if (length(variables) != length(histories)){
    return("Length of variables must be the same as length of histories")
  }
  n <- length(variables)
  for (i in 1:n){
    var_name <- variables[i]
    type <- histories[i]
    
    if (substring(type,1,3)=="lag"){
      k <- as.integer(substring(type,5))
      D <- D %>%
        group_by(id) %>%
        mutate(!!paste(var_name, type,sep="_") := lag(get(var_name),k,default=0))
    }
    
    else if (type=="cumsum"){
      D <- D %>%
        group_by(id) %>%
        mutate(!!paste(var_name, type,sep="_") := cumsum(get(var_name)))
    }
    
    else if (type=="cummean"){
      D <- D %>%
        group_by(id) %>%
        mutate(!!paste(var_name, type,sep="_") := cummean(get(var_name)))
    }
    
    #Cannot handle if id only has one observation
    else if (substring("sum_prev_k",1,8)=="sum_prev"){
      k <- as.integer(substring(type,10))
      D <- D %>%
        group_by(id) %>%
        mutate(!!paste(var_name, type,sep="_") := roll_sum(lag(get(var_name),1,default=0), k, fill=0, align="right"))
    }
  }
  return(D)
}