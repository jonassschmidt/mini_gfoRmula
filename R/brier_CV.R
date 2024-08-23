#' Brier-score based cross-validation of supplied p and q models
#' 
#' The i'th element in formulae is the model formulation for the i'th
#' model type (algorithm) in types. 
#'
#' @param data data frame 
#' @param formulae a vector of model formulations
#' @param types a vector of type of algorithm (glm or gam)
#'
#' @return
#' @export
#' 
#' @import gam
#'
#' @examples
#' brier_CV(add_histories(gfoRmula::basicdata, c("A","L1","L2"), c("lag_1","lag_1","lag_1")), 
#'         c("Y ~ A + L1 + L2 + L3 + A_lag_1 + L1_lag_1 + L2_lag_1 + t0", "Y ~ A + L1 + s(L2,df=4) + L3 + A_lag_1 + L1_lag_1 + L2_lag_1 + t0"),
#'         c("glm","gam"))
brier_CV <- function(data, formulae, types) {
  
  if (length(formulae) != length(types)){
    return("formulae not the same length as type")
  }
  
  n_models <- length(formulae)
  data_w_folds <- ten_folds_indicator(data)[[1]]
  brier_scores <- matrix(0, nrow=10, ncol=n_models)
  for (f in 1:10){
    data_train <- filter(data_w_folds, fold != f)
    data_eval <- filter(data_w_folds, fold == f)
    for (formula in 1:n_models){
      if (types[formula] == "glm"){
        model <- glm(as.formula(formulae[formula]), data_train, family=binomial(link="logit"))
      }
      else if (types[formula] == "gam"){
        model <- gam(as.formula(formulae[formula]), data_train, family=binomial(link="logit"))
      }
      
      brier <- sum((plogis(predict(model, data_eval))-data_eval$Y)^2, na.rm=T)
      brier_scores[f, formula] <- brier
    }
  }
  
  return(colMeans(brier_scores))
}





