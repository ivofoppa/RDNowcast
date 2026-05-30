#' Draws from the posterior distribution of \code{\eqn{\lambda}}
#' 
#' @description
#' This function draws samples from distribution of the Poisson parameter \code{\eqn{\lambda}}, given \code{x} events. 
#'     It makes use of the fact, that the distribution is a Gamma distribution with parameters \code{\eqn{\alpha = x +1}} and
#'     \code{\eqn{\beta = 1}}.
#' 
#' @import dplyr
#' @param x An integer vector.
#' @param nsamples The number of samples; the default is 10'000.
#' @returns A numeric vector of length \code{nsamples * length(x)}.
#' @examples
#' lambdavec <- rSample(c(100,120))
#' lambdavec |> quantile(probs = c(.5,.025,.975))
#' @export
rSample <- function(x,nsamples = 10000) {
  alpha <- x + 1
  beta <- 1
  
  sapply(seq_along(x), function(k) rgamma(nsamples,alpha[k], beta)) |> 
    unlist() |> as.vector()
}
