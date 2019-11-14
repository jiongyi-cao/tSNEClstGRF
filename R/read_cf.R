#' Read causal forest object
#'
#' tSNE-Clustering visualization starts by a causal forest object.
#' This function read a causal forest object that has been created and
#' process data for t-SNE clustering analysis on next stage.
#' @param object causal forest object that has been created.
#' @param X Vector of covariate names or number of clumns to includes in later t-SNE analysis.Defaults to all covariates in X.orig of cf object.
#' @references Athey Susan, Julie Tibshirani, and Stefan Wager. Generalized Random Forests. Annals of Statistics (forthcoming), 2018
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

  #check variable type
    col_num <- sapply(df$X,function(i) length(unique(i)) >= 10) #numeric column
    df$X[,col_num] <- as.data.frame(sapply(df$X[,col_num],as.numeric))
   # col_log <- sapply(dt,function(i)length(unique(i)) == 2)  #binary column
   # dt[,col_log] <- as.data.frame(sapply(dt[,col_log],if_else(x==1, TRUE, FALSE)))
    col_fac <- sapply(df$X,function(i) length(unique(i)) < 10) #factor column
    df$X[,col_fac] <- as.data.frame(sapply(df$X[,col_fac],as.factor))

    #helper func for changing variable type
    change_df <- function(){
     change_num <- unlist(strsplit(readline("type variables that are numeric (seperate by space)")," "))
     df$X[,change_num] <- as.data.frame(sapply(df$X[,change_num],as.numeric))
     change_fac <- unlist(strsplit(readline("type variables that are factor (seperate by space)")," "))
     df$X[,change_fac] <- as.data.frame(sapply(df$X[,change_num],as.numeric))
     return(df)
    }

    #prompt user to specify variable type
    switch(menu(c("Yes","No"),title = cat("Are the variable types correct?","\n",
                                            "numeric:",colnames(df$X)[col_num],"\n",
                                            "factor:",colnames(df$X)[col_fac])),
           return(df),change_df())
    }
