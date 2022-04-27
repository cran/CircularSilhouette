## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 200
)

## -----------------------------------------------------------------------------
library(CircularSilhouette)
o=c(19,0,4,6,10,12,15)
c=c(1,1,2,2,2,2,2)
circumference=20
silhouette=circular.sil(o, c, circumference, method="linear")
# print(silhouette)
knitr::kable(silhouette, col.names="Circular silhouette")

## ----out.width="70%", fig.show='hold'-----------------------------------------
library(OptCirClust)

Circumference=100
O=c(99,0,1,2,3,15,16,17,20,50,55,53,70,72,73,69)
K_range=c(2:8)
k <- find.num.of.clusters(O, Circumference, K_range)
result_FOCC <- CirClust(O, k, Circumference, method = "FOCC")
opar <- par(mar=c(0,0,2,0))
plot(result_FOCC, main="Optimal number of clusters",
     sub=paste("Optimal k =", k))
par(opar)

## ----out.width="70%"----------------------------------------------------------
x=c(40,41,41,42,44,45,45,46,46,46,47,50,51,51,52,54,55,55,56,56,56,57,
    60,61,61,62,64,65,65,66,66,66,67,70,71,71,72,74,75,75,76,76,76,77,
    80,81,81,82,84,85,85,86,86,86,87,90,91,91,92,94,95,95,96,96,96,97)
set.seed(111)
x <- x + rnorm(length(x))

periodrange=c(80:120)/10
period<-estimate.period(x, periodrange) 

cat("The estimated period is", period, "\n")

plot(x, rep(1, length(x)), type="h", col="purple", 
     ylab="", xlab="Noisy periodic data",  
     main="Period estimation", 
     sub=paste("Estimated period =", period))
k <- (max(x) - min(x)) %/% period
abline(v=min(x)+ period * (0:k), lty="dashed", col="green3")

