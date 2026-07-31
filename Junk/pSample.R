#' Generating posterior distribution of data completeness estimates
#' 
#' @description 
#' This function generates builds on the function \code{pSample}, but if the data completeness estimates are obtained for more than one data set.
#'     The inputs are arrays for the number of successes (x) and totals (n). Each entry in x should correspond to an entry in n and the rows typically
#'     represent a given temporal distance to the "present" (time of the nowcast). The samples correspondig
#' 
#' @param NC A list whose first element is an integer array with observed numbers (incomplete due to reporting delay); 
#'     the columns represent decreasing temporal distance from "now", the rows represent the complete numbers.
#'     different times in the nowcast period. 
#' @param nsamples The number of samples; the default is 10'000.
#' @returns A numeric vector.
#' @examples
#' pSample(c(10,20),c(12,40))
#' pSample(23,100)
#' @import dplyr
#' 
#' @export

pSampleMult <- function(NC, nsamples = 10000){
  x <- NC[[1]]
  nColx <- ncol(x)
  nRowx <- nrow(x)
  n <- NC[[2]]
  
  for(i in 1:nColx) {
    alpha <- x[,i] + 1;
    beta = n[,i] - x[,i] + 1;
    pVecTmp <- sapply(1:nRowx,function(k) rbeta(nsamples,alpha[k],beta[k])) |> unlist() |> c()
    if(!exists("pArr")) {
      pArr <- pVecTmp
    } else {
      pArr <- bind_cols(pArr,pVecTmp,.name_repair = "unique_quiet",)
    }
  }
  
  pArr |> as.matrix()
}
