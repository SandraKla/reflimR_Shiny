###################################################################################################
######################### Script to use mclust ####################################################
###################################################################################################

#' Gaussian mixture modelling for the verification of reference intervals
#'
#' @param x Numeric vector of positive laboratory values without missing values.
#'   The length of \code{x} should be at least 200 observations.
#' @param lognormal Logical; if \code{TRUE}, a lognormal transformation is applied
#'   before clustering. Default is \code{FALSE}.
#' @param targets Optional numeric vector specifying target values. Default is \code{NULL}.
#' @param plot.it Logical; if \code{TRUE}, diagnostic plots are generated.
#'   Default is \code{TRUE}.
#' @param add.boxplot Logical; if \code{TRUE}, a boxplot is added to the histogram.
#'   Default is \code{TRUE}.
#' @param plot.legend Logical; if \code{TRUE}, a legend is added to the plot.
#'   Default is \code{TRUE}.
#' @param pos.legend Character string specifying the legend position, passed to
#'   \code{legend()}. Default is \code{"topright"}.
#' @param plot.bic Logical; if \code{TRUE}, the Bayesian Information Criterion (BIC)
#'   is plotted for model comparison. Default is \code{FALSE}.
#' @param main Character string specifying the main title of the plot.
#' @param xlab Character string specifying the x-axis label.
#' @param hist.bins Integer; number of bins used for the histogram.
#'   Default is 50.
#' @param model Character string specifying the variance model used for clustering.
#'   If \code{NULL}, the model is selected automatically. Possible values include
#'   \code{"E"} (equal variance) and \code{"V"} (variable variance).
#' @param n.cluster Integer or integer vector specifying the number of clusters.
#'   If \code{NULL}, the optimal number of clusters is selected automatically.
#' @param apply.rounding Logical; if \code{TRUE}, results are rounded.
#'   Default is \code{TRUE}.
#' @param digits Integer specifying the number of decimal places used for rounding.
lab_mclust <- function(x, lognormal = FALSE, targets = NULL,
                       plot.it = TRUE, add.boxplot = TRUE, 
                       plot.legend = TRUE, pos.legend = "topright",
                       plot.bic = FALSE,
                       main = "", xlab = "",
                       hist.bins = 50, model = NULL, n.cluster = NULL,
                       apply.rounding = TRUE, digits = NULL){

  if(lognormal){
    xx <- log(x)
  }else{
    xx <- x
  }
  if(is.null(model)){
    mc <- Mclust(xx, G = n.cluster)
  }else{
    mc <- Mclust(xx, modelNames = model, G = n.cluster)
  }
  for (i in 1 : mc$G) {
    if(is.na(mc$parameters$variance$sigmasq[i])){
      mc$parameters$variance$sigmasq[i] <- mc$parameters$variance$sigmasq[1]
    }
  }
  res.tab <- data.frame(matrix(NA, mc$G, 5))
  for(i in 1 : mc$G){
    res.tab[i, 3] <- round(mc$parameters$pro[i] * 100, 1)
    res.tab[i, 4] <- round(mc$parameters$mean[i], 3)
    res.tab[i, 5] <- round(sqrt(mc$parameters$variance$sigmasq[i]), 3)
  }
  if(lognormal){
    colnames(res.tab) <- c("ll", "ul", "percent", "meanlog", "sdlog")
    for (i in 1 : mc$G) {
      res.tab[i, 1] <- qlnorm(0.025, mc$parameters$mean[i], sqrt(mc$parameters$variance$sigmasq[i]))
      res.tab[i, 2] <- qlnorm(0.975, mc$parameters$mean[i], sqrt(mc$parameters$variance$sigmasq[i]))
    }
  }else{
    colnames(res.tab) <- c("ll", "ul", "percent", "mean", "sd")
    for (i in 1 : mc$G) {
      res.tab[i, 1] <- qnorm(0.025, mc$parameters$mean[i], sqrt(mc$parameters$variance$sigmasq[i]))
      res.tab[i, 2] <- qnorm(0.975, mc$parameters$mean[i], sqrt(mc$parameters$variance$sigmasq[i]))
    }
  }
  if(is.null(digits)){
    digits <- 2 - floor(log10(median(xx)))
    if(digits < 0){digits <- 0}
  }
  if(apply.rounding){
    res.tab[, 1] <- round(res.tab[, 1], digits)
    res.tab[, 2] <- round(res.tab[, 2], digits)
  }
  
  if(plot.it){
    col = rainbow(9)
    d <- density(x)
    y.max <- max(d$y) * 1.1
    if(add.boxplot){y.max <- y.max  * 1.4}
    if(length(x) > 200){
      breaks <- seq(0.9 * min(x), 1.1 * max(x), length.out = hist.bins)
    }else{
      breaks <- "Sturges"
    }
    hist(x, freq = FALSE, 
         breaks = breaks,
         col = "white", border = "grey", 
         ylim = c(0, y.max),yaxt = "n",
         main = main, xlab = xlab, ylab = "")
    box()
    lines(d, lty = 2)
    if(add.boxplot){
      boxplot(x, horizontal = TRUE, at = y.max * 0.9, boxwex = y.max * 0.1, add = TRUE)
    }
    if(lognormal){
      for (i in 1 : nrow(res.tab)) {
        curve(dlnorm(x, meanlog = mc$parameters$mean[i], sdlog = sqrt(mc$parameters$variance$sigmasq[i])) * mc$parameters$pro[i],
              from = min(x), to = max(x), lwd = 2, col = col[i], add = TRUE)
      }
    }else{
      for (i in 1 : nrow(res.tab)) {
        curve(dnorm(x, mean = mc$parameters$mean[i], sd = sqrt(mc$parameters$variance$sigmasq[i])) * mc$parameters$pro[i],
              from = min(x), to = max(x), lwd = 2, col = col[i], add = TRUE)
      }
    }
    if(!is.null(targets)){
      lines(rep(targets[1], 2), c(0, y.max * 0.8), lty = 2)
      lines(rep(targets[2], 2), c(0, y.max * 0.8), lty = 2)
      text(targets, rep(y.max * 0.85, 2), targets)
    }
    if(plot.legend){
      legend(pos.legend, 
             paste0(round(res.tab$ll, digits), "-", round(res.tab$ul, digits),
                   " (", res.tab$percent, "%)"),
             lwd = 2, col = col[1 : nrow(res.tab)], cex = 0.8, bty = "n")
    }
    if(plot.bic){
      plot(mc$BIC)
    }
  }
  if(is.null(n.cluster)){n.c <- mc$G} else {
    n.c <- paste(mc$G, "from", deparse(n.cluster)) }
  return(list(n.cluster = noquote(n.c), 
              stats = res.tab, BIC = mc$BIC))
}