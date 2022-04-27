#' @useDynLib CircularSilhouette, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#'
#' @title Calculating Silhouette on Circular Data Clusters
#'
#' @description
#' A fast linear-time algorithm to calculate silhouette information on
#' circular data with cluster labels.
#'
#' @param O a numeric vector of circular data points
#' @param cluster an integer vector of cluster labels for each point
#' @param Circumference a numeric value giving the circumference of the
#'  circle
#' @param method a character value to specify the algorithm to calculate
#'  the silhouette information. The default value is \code{"linear"},
#'  indicating a fast linear time algorithm for calculating circular
#'  silhouette. The option of \code{"quadratic"} is provided for
#'  testing and comparison, not meant for production use.
#'
#' @details If \code{method} takes the value of \code{"linear"}
#'  (default), the
#'  silhouette information on circular data is calculated by a fast
#'  linear-time algorithm; if \code{method} is \code{"quadratic"},
#'  a quadratic-time algorithm is used instead to calculate silhouette
#'  by definition. There is an overhead of sorting \eqn{O(n \log n)}{O(n log n)} if the
#'  input data are not sorted.
#'
#' One important assumption is that a cluster cannot be contained in
#' another cluster in the input cluster labels.
#'
#'
#' @return The function returns a numeric value of the average
#'  silhouette information calculated on the input circular data
#'  clusters.
#'
#' @examples
#' O <- c(-1.2, -2, -3, -2.5, 1, 0.8, 1.5, 1.2)
#' cluster <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' circular.sil(O, cluster, 3)
#'
#' @export
circular.sil <- function(O, cluster, Circumference,
                         method=c("linear", "quadratic"))
{
  if(length(O)!=length(cluster)) {
    stop("The length of O and cluster are not equal.")
  }

  method=match.arg(method)
  size=table(cluster)

  if(method=="quadratic"){
    Silnow = slowsil(cluster, O, size, Circumference)
  }
  else if(method=="linear"){
    Silnow = FastCirSil(cluster, O, size, Circumference)
  }

  return(Silnow)
}

