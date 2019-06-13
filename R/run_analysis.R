#' Run t-SNE clustering algorithm
#'
#' This function read previous proceeded causal forest object and get 2D projection of covariates space based on t-SNE algorithm
#' @param object causal forest object that has been created.
#' @param n size of subsample from whole dataset. Suggest no larger than 10,000.
#' @param distance distance metrix of covariate space either provided by user or use default: gower distance
#' @param perplexity Perplexity parameter (should no larger than 3 * perplexity < nrow(X) - 1, see details for interpretation).
#' @return A t-SNE clustering object with the follwing elements:
#' \item{distance} distance metrix of covariate space.
#' \item{result} final datafram proceed for visualization
#' @references Maaten, L. Van Der, 2014. Accelerating t-SNE using Tree-Based Algorithms. Journal of Machine Learning Research, 15, p.3221-3245
#' @references van der Maaten, L.J.P. & Hinton, G.E., 2008. Visualizing High-Dimensional Data Using t-SNE. Journal of Machine Learning Research, 9, pp.2579-2605.
#' @export
#' @examples
#' tsne_obj <- run.analsis(my_cf)



run.analsis<- function(object,n,distance = NULL, perp= NULL){
    #note about covariate type for shiny application/as well as gower dist
    tClst_obj <- list()

    #set perpelexity
    if(is.null(perp)) {
      if (n >= 5000) perp = 500
      else perp = floor(n/10)
    }

    #generate subsampling index
    size <- length(object$prd)
    if(n > size) stop("sample size larger than total data size")
    set.seed(1234)
    index <- sample(size, n)

    #calculated distance metric based on distance algorithm
    if(is.null(distance)) distance <- StatMatch::gower.dist(object$X[index,])
    tClst_obj$dist <- distance

    #get tsne projection
    set.seed(1234)
    tsne_obj <- Rtsne::Rtsne(distance, is_distance = TRUE, perplexity = perp)

    #create final dataframe
    tClst_obj$result <- tsne_obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y"))%>%
      mutate(tau = object$prd[index], z = (object$prd/sd(object$prd))[index]) %>%
      mutate(Level = case_when(
        z >= -1.28 & z < 1.28 ~ "p > 0.1",
        z >= 1.28 & z <  1.96 ~ "positive 0.05 < p < 0.1",
        z >= 1.96 ~ "positive p < 0.05",
        z <= -1.28 & z >  -1.96 ~ "negative 0.05 < p < 0.1",
        z <= -1.96 ~ "negative p < 0.05"),
        Level = factor(Level, levels=c("p > 0.1","positive 0.05 < p < 0.1","positive p < 0.05","negative 0.05 < p < 0.1","negative p < 0.05"))) %>%
      cbind.data.frame(object$X[index,])

    #define class for object
    #class() <- c("")

    tClst_obj


}
