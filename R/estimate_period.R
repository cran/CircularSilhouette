#' @title Estimating the Period of Noisy Periodical Data
#'
#' @importFrom OptCirClust CirClust
#'
#' @description
#' By performing circular clustering and calculating circular
#'  silhouette, the function estimates the period of periodical data.
#'
#' @param x a numeric vector of data points that are one-dimensional,
#'  noisy, periodical
#' @param possible.periods a numeric vector representing a set of
#'  period values to evaluate
#' @param ks a numeric vector of numbers of clusters within one period
#'
#' @details The user can estimate a period by providing the number of
#' clusters within one period and a set of periods for examination.
#' An optimal circular clustering algorithm
#' \code{\link[OptCirClust]{CirClust}} in R package \pkg{OptCirClust}
#' is used to cluster the periodical data. The algorithm converts the
#' periodical data to circular data of a circumference equal to twice
#' the tested period. Then circular silhouette information for
#' each circumference and number of clusters are computed to find the
#' maximum silhouette information. The half of circumference giving
#' maximum silhouette information is selected to be the estimated period.
#'
#' The possible periods provided by the function should be close to the
#' true period. This is not ideal and we are improving the design to be
#' more robust.
#'
#' @return The function returns a numeric value representing the
#'  estimated period.
#'
#' @examples
#' library(OptCirClust)
#' x=c(40,41,42,50,51,52,60,61,62,70,71,72,80,81,82,90,91,92)
#' x <- x + rnorm(length(x))
#' clusterrange=c(2:5)
#' periodrange=c(80:120)/10
#' period<-estimate.period(x, periodrange, clusterrange)
#' cat("The estimated period is", period, "\n")
#' plot(x, rep(1, length(x)), type="h", col="purple",
#'      ylab="", xlab="Noisy periodic data",
#'      main="Period estimation",
#'      sub=paste("Estimated period =", period))
#' k <- (max(x) - min(x)) %/% period
#' abline(v=min(x)+period/2 + period * (0:k), lty="dashed", col="green3")
#'
#' @export
estimate.period <- function(
  x, possible.periods = diff(range(x))/2^(1:5),
  ks=2:10)
{
  i=1
  high=0
  result=NULL
  for(j in ks){
    cir = possible.periods * 2
    for(L in cir){
      result_FOCC <- OptCirClust::CirClust(x%%L, j, L, method = "FOCC")
      Sil = circular.sil(x%%L, result_FOCC$cluster, L, "linear")
      result[i]=Sil#/L
      if(high<result[i]){
        hj=j; hl=L; h=i; high=result[i];
      }
      i=i+1
    }
  }
  period <- hl/2
  return(period)
}
