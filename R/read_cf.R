#' Read causal forest object
#'
#' tSNE-Clustering visualization start by a causal forest object.
#' This function read a causal forest object that has been created and
#' process it for next t-SNE clustering analysis.
#' @param object causal forest object that has been created.
#' @param X Vector of covariate names or number of clumns to includes in later t-SNE analysis.Defaults to all covariates in X.orig of cf object.
#' @return A dataframe with selected covariates that is ready for t-SNE clustering analysis
#' @references Athey, Susan, Julie Tibshirani, and Stefan Wager. Generalized Random Forests. Annals of Statistics (forthcoming), 2018
#' @references Wager, Stefan, and Susan Athey. Estimation and Inference of Heterogeneous Treatment Effects using Random Forests. Journal of the American Statistical Association (forthcoming), 2018.
#' @export
#' @examples
#' # Train a causal forest.
#'n = 50; p = 10
#'X = matrix(rnorm(n*p), n, p)
#'W = rbinom(n, 1, 0.5)
#'Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)
#' c.forest <- causal_forest(X, Y, W)
#' my_cf <- read.cf(c.forest, X = c(2:7))

read.cf <- function(object, X = "All"){
  library(dplyr)
  #check causal forest object
  if(class(object)[1]!="causal_forest") stop("Fail to read causal forest object. Please check object type.")
  #create dataframe for t-SNE clusterin
  df <- list()
  df$prd <- object$predictions
  if(sum(X == "All")) {df$X <- object$X.orig}
  else if(is.numeric(X)) {df$X <- object$X.orig[,X]}
  else if(is.character(X)) {df$X <- object$X.orig %>% dplyr::select(X)}
  else stop("Invalid covariates input")
  df
}
