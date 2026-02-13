###################################################################################################
######################### Script for the VeRUS Interval ###########################################
###################################################################################################

#' Computes the standard Box-Cox transformation.
#'
#' @param x Data to be transformed.
#' @param lambda The parameter of the Box-Cox transformation.
#'
#' @return The Box-Cox transformed data.
box.cox.trans <- function(x,lambda=1){
  if (lambda==0){
    return(log(x))
  }else{
    return((x^lambda - 1)/lambda)
  }
}

#' Computes the standard inverse Box-Cox transformation.
#'
#' @param x Data to be transformed.
#' @param lambda The parameter of the (inverse) Box-Cox transformation.
#'
#' @return The inverse Box-Cox transformed data.
box.cox.inv.trans <- function(x,lambda=1){
  if (lambda==0){
    return(exp(x))
  }else{
    return((x*lambda + 1)^(1/lambda))
  }
}

#' Computes a normal-approximation confidence interval for a quantile.
#'
#' @param p The target quantile (probability), e.g. 0.975.
#' @param alpha Significance level for the confidence interval.
#' @param n Sample size.
#'
#' @return A numeric vector of length 2 containing the lower and upper
#'   bounds of the confidence interval for the quantile.
i.norm <- function(p=0.975,alpha=0.1,n=120){
  half.width <- abs(qnorm(alpha/2)*sqrt(p*(1-p))/(dnorm(qnorm(p))*sqrt(n)))
  centre.point <- qnorm(p)
  return(c(centre.point-half.width,centre.point+half.width))
}

#' Transforms lower and upper limits using the Box-Cox transformation.
#'
#' @param ll Lower limit.
#' @param ul Upper limit.
#' @param lambda Parameter of the Box-Cox transformation.
#'
#' @return A numeric vector containing the transformed lower and upper
#'   limits.
compute.mu.sigma <- function(ll,ul,lambda=0){
  ll.t <- box.cox.trans(ll,lambda=lambda)
  ul.t <- box.cox.trans(ul,lambda=lambda)
  
  return(list(lower = ll.t, upper = ul.t))
}

#' Computes VeRUS-type confidence limits based on a Box-Cox transformed scale.
#'
#' The function transforms the provided lower and upper limits to a Box-Cox
#' scale, estimates the corresponding normal mean and standard deviation,
#' constructs normal-approximation confidence intervals for selected
#' quantiles, and transforms the results back to the original scale.
#'
#' @param ll Lower limit.
#' @param ul Upper limit.
#' @param lambda Parameter of the Box-Cox transformation.
#' @param delta Optional shift added after back-transformation.
#' @param p Numeric vector of probabilities for the lower and upper quantiles.
#' @param alpha Significance level for the confidence intervals.
#' @param n Sample size.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{lower.lim.low}{Lower bound of the CI for the lower quantile.}
#'   \item{lower.lim.upp}{Upper bound of the CI for the lower quantile.}
#'   \item{upper.lim.low}{Lower bound of the CI for the upper quantile.}
#'   \item{upper.lim.upp}{Upper bound of the CI for the upper quantile.}
#' }
verus.limits <- function(ll, ul, lambda, delta=0, p=c(0.025,0.975), alpha=0.1, n=120){
  ll.t <- box.cox.trans(ll,lambda=lambda)
  ul.t <- box.cox.trans(ul,lambda=lambda)
  
  mu <- (ll.t + ul.t)/2
  sigma <- (ul.t - ll.t)/(qnorm(0.975)-qnorm(0.025)) #factor 3.92
  
  ll <- box.cox.inv.trans(i.norm(p=p[1],alpha=alpha,n=n)*sigma + mu,lambda=lambda) + delta 
  ul <- box.cox.inv.trans(i.norm(p=p[2],alpha=alpha,n=n)*sigma + mu,lambda=lambda) + delta 
  
  return(list(lower.lim.low=ll[1],lower.lim.upp=ll[2],upper.lim.low=ul[1],upper.lim.upp=ul[2]))
}

#' Computes the mid-range coefficient of variation.
#'
#' @param ll Lower limit or lower value.
#' @param ul Upper limit or upper value.
#'
#' @return A numeric value representing the relative spread between the upper and lower values.
mocov <- function(ll, ul){
  return((ul - ll) / (ul + ll))
}