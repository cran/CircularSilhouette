#' @title Finding an Optimal Number of Circular Data Clusters
#'
#' @importFrom OptCirClust CirClust
#' @importFrom Rdpack reprompt
#' @description
#' An optimal number of clusters is selected on circular data such that
#' the number maximizes the circular silhouette information.
#'
#' @param O a numeric vector of coordinates of data points along a circle.
#' @param Circumference a numeric value giving the circumference of the
#'  circle
#' @param ks an integer vector representing possible choices for the
#'  number of clusters
#'
#' @details Using the circular clustering algorithm in the R package
#' \pkg{OptCirClust} \insertCite{Debnath21}{OptCirClust}, we will examine every value of \eqn{k} in the given
#'  choices of number of clusters. We select a \eqn{k} that maximizes the
#'  circular silhouette information.
#'
#' @return The function returns an integer number that is optimal in
#' maximizing circular silhouette.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' library(OptCirClust)
#' Circumference=100
#' O=c(99,0,1,2,3,15,16,17,20,50,55,53,70,72,73,69)
#' K_range=c(2:8)
#' k <- find.num.of.clusters(O, Circumference, K_range)
#' result_FOCC <- CirClust(O, k, Circumference, method = "FOCC")
#' opar <- par(mar=c(0,0,2,0))
#' plot(result_FOCC, cex=0.5, main="Optimal number of clusters",
#'      sub=paste("Optimal k =", k))
#' par(opar)
#' @export
find.num.of.clusters <- function(O, Circumference, ks=2:10)
{
  highestSil = -Inf
  chosenK = 0
  chosenClustering = 0
  i=1
  result=NULL
  for(n in ks){
    result_FOCC <- OptCirClust::CirClust(O, n, Circumference, method = "FOCC")
    currentSil = circular.sil(O%%Circumference, result_FOCC$cluster, Circumference,"linear")
    result[i]=currentSil
    i=i+1
    if(currentSil > highestSil){
      highestSil = currentSil
      chosenK = n
      chosenClustering = result_FOCC
    }
  }
  return(chosenK)
}
