###################################################################################################
######################### Script to compute the zlog value ########################################
###################################################################################################

#' Computes the zlog value of x given the lower und upper reference limits L and U
#'
#' @param x value
#' @param L lower reference limit
#' @param U upper reference limit
zlog <- function(x,L=0,U=0){
  if (is.na(x) | is.na(L) | is.na(U) | L<=0 | U<=0 | U<=L){
    return(NA)
  }
  
  logl <- log(L)
  logu <- log(U)
  mu.log <- (logl+logu)/2
  sigma.log <- (logu - logl)/(3.919928)
  
  return((log(x)-mu.log)/sigma.log)
}

#' Round numeric values from a dataframe
#' 
#' @param x Expects dataframe
#' @param digits Digits to round
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  return(x)
}

#' Get the colors to the zlog values
#' 
#' Returns a color between blue via white to red (HEX or RGB)
#' 
#' @param x Expects a (zlog) value x
zlogcolor <- function(x, hex = TRUE,
                      a = c(0, 20), w = c(255, 235), t = c(4, 4),
                      s = c(1.5, 1.5), m = c(-4, -6)){
  
  R = round(a[1] + w[1] / ((1 + t[1] * exp(-s[1] * ( x - m[1]))) ^ (1 / t[1])))
  B = round(a[1] + w[1] / ((1 + t[1] * exp(-s[1] * (-x - m[1]))) ^ (1 / t[1])))
  
  G = sapply(x, function(x) ifelse(x < 0,
    round(a[2] + w[2] / ((1 + t[2] * exp(-s[2] * ( x - m[2]))) ^ (1 / t[2]))),
    round(a[2] + w[2] / ((1 + t[2] * exp(-s[2] * (-x - m[2]))) ^ (1 / t[2])))))

  # if(x < 0) {
  #   G = round(a[2] + w[2] / ((1 + t[2] * exp(-s[2] * ( x - m[2]))) ^ (1 / t[2])))
  # } else {
  #   G = round(a[2] + w[2] / ((1 + t[2] * exp(-s[2] * (-x - m[2]))) ^ (1 / t[2])))
  # }
  
  R[is.na(R)] <- 255
  B[is.na(B)] <- 255
  G[is.na(G)] <- 255
  
  ifelse (hex,
          return(rgb(R, G, B, max = 255)),
          return(c(R, G, B)))
  
}

#' Get the zlog value and check if the background is to dark and change the textcolor to white
#' 
#' Returns a color between blue via white to red (HEX or RGB)
#' 
#' @param x Expects a (zlog) value x
#' @param threshold Given threshold for the zlog value
#' @param background Variable to decide id the color affects the background or the text
highzlogvalues <- function(x, hex = TRUE, threshold = 8, background = FALSE){

  if(!background){
    G = sapply(x, function(x) ifelse(x < -threshold, 255, 0))
    R = sapply(x, function(x) ifelse(x < -threshold, 255, 0))
    B = sapply(x, function(x) ifelse(x < -threshold, 255, 0))
  } else{
    G = sapply(x, function(x) ifelse(x > threshold, 192, 255))
    R = sapply(x, function(x) ifelse(x > threshold, 192, 255))
    B = sapply(x, function(x) ifelse(x > threshold, 192, 255))
  }
  
  R[is.na(R)] <- 0
  B[is.na(B)] <- 0
  G[is.na(G)] <- 0
  
  ifelse (hex,
          return(rgb(R, G, B, max = 255)),
          return(c(R, G, B)))
}