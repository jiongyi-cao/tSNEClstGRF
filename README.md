tSneClstGRF is an R package that generates an interactive Shiny interface
for easily visualizing and exploring heterogeneous causal effect estimated
by causal forest proposed by Athey et al. (2018).

Installation
------------

To install this package from the github repository, use:

    if(!require(devtools)) install.packages("devtools") # If not already installed
    devtools::install_github("jiongyi-cao/tSneClstGRF")

Usage
-----

A basic workflow of tSneClstGRF contains major three steps:

-   read causal forest object
-   run t-SNE analysis
-   generate shiny interface

The following is an illustration using the replication from Athey, S., &
Wager, S. (2019). Estimating treatment effects with causal forests: An
application.

    library(grf)
    library(tSneClstGRF)
    #load dataset
    data(nslm)
    X <- nslm[,-c(29,30,31)]
    attach(nslm)
    ## Replication from Athey & Stefan's GRF application
    Y.forest = regression_forest(X, Y, clusters = school.id)
    Y.hat = predict(Y.forest)$predictions
    W.forest = regression_forest(X, W, clusters = school.id)
    W.hat = predict(W.forest)$predictions

    cf.raw = causal_forest(X, Y, W,
                           Y.hat = Y.hat, W.hat = W.hat,
                           clusters = school.id)
    varimp = variable_importance(cf.raw)
    selected.idx = which(varimp > mean(varimp))

    cf = causal_forest(X[,selected.idx], Y, W,
                       Y.hat = Y.hat, W.hat = W.hat,
                       clusters = school.id,
                       #samples.per.cluster = 50,
                       tune.parameters = "all")

    #read causal forest object
    my_cf <- read.cf(cf)
    #run t-SNE Clustering algorithm
    tsne_obj <- run.analsis(my_cf,3000)
    #generate shiny application
    create.shiny(tsne_obj$result)

References
----------

Athey Susan, Julie Tibshirani, and Stefan Wager.
<a href="https://arxiv.org/abs/1610.01271">Generalized Random
Forests.</a> <i>Annals of Statistics (forthcoming)</i>, 2018

Athey, S., & Wager, S.
<a href="https://arxiv.org/pdf/1902.07409.pdf">Estimating treatment
effects with causal forests: An application.</a>,2019
