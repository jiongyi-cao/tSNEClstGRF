#' Run t-SNE clustering algorithm
#'
#' This function read previous proceeded causal forest object and get 2D projection of covariates space based on t-SNE algorithm
#' @param object Causal forest object that has been created.
#' @param n Size of subsample from whole dataset. Suggest no larger than few thousands.
#' @param distance Distance metrix of covariate space either provided by user or default: gower distance
#' @param perplexity Perplexity parameter (should no larger than 3 * perplexity < nrow(X) - 1, see details for interpretation).
#' @return A t-SNE clustering object with the follwing elements:
#' \item{index} sub sample index
#' \item{distance} distance metrix of covariate space.
#' \item{result} final dataframe proceed for visualization
#' @references Maaten, L. Van Der, 2014. Accelerating t-SNE using Tree-Based Algorithms. Journal of Machine Learning Research, 15, p.3221-3245
#' @references van der Maaten, L.J.P. & Hinton, G.E., 2008. Visualizing High-Dimensional Data Using t-SNE. Journal of Machine Learning Research, 9, pp.2579-2605.
#' @export
#' @examples
#' tsne_obj <- run.analsis(my_cf)



run.analsis<- function(object,n,distance = NULL, perp= NULL){
    library(dplyr)
    #note about covariate type for shiny application/as well as gower dist
    tClst_obj <- list()

    #set perpelexity
    if(is.null(perp)) {
      if (n >= 5000) perp = 500
      else perp = floor(n/20)
    }

    #generate subsampling index
    size <- length(object$prd)
    if(n > size) stop("sample size larger than total data size")
    set.seed(1234)
    index <- sample(size, n)
    tClst_obj$index <- index

    #calculate distance metric based on distance algorithm
    if(is.null(distance)) d <- StatMatch::gower.dist(object$X[index,])
    else d<- dist(object$X[index,],method = distance)
    tClst_obj$dist <- d

    #get tsne projection
    set.seed(1234)
    tsne_obj <- Rtsne::Rtsne(d, is_distance = TRUE, perplexity = perp)

    #create final dataframe
    q <- round(quantile(object$prd,c(0,0.2,0.4,0.6,0.8,1)),4)
    lab <- c(paste("1st quantile:",q[[1]],"~",q[[2]]),
             paste("2nd quantile:",q[[2]],"~",q[[3]]),
             paste("3rd quantile:",q[[3]],"~",q[[4]]),
             paste("4th quantile:",q[[4]],"~",q[[5]]),
             paste("5th quantile:",q[[5]],"~",q[[6]]))
    tClst_obj$result <- tsne_obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y"))%>%
      mutate(tau = object$prd[index]) %>%
      mutate(Level = case_when(
        tau < q[[2]] ~ lab[1],
        tau >= q[[2]] & tau < q[[3]] ~ lab[2],
        tau >= q[[3]] & tau < q[[4]] ~ lab[3],
        tau >= q[[4]] & tau < q[[5]] ~ lab[4],
        tau >= q[[5]] ~ lab[5]),
        Level = factor(Level, levels = lab)
        ) %>% cbind.data.frame(object$X[index,])

    tClst_obj


}
