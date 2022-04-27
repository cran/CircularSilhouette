#linear data
test_that("Test fast linear silhouette", {
  o=c(0,1,2,3,4,5,6,7,8,9)
  c=c(1,1,1,4,4,4,2,2,3,3)
  fast=fast.sil(o, c)
  manual=(4/3+2.825)/10
  expect_equal(fast, manual)
})

#linearsil
test_that("Test linearsil function", {
  points=c(0,1,2,3,4,5,6,7,8,9)
  cluster=c(1,1,1,4,4,4,2,2,3,3)
  sil=linearsil(points, cluster)
  # expect_equal(sil, 0.41583333)

  sil.def <- cluster::silhouette(cluster, dist(points))
  expect_equal(sil, summary(sil.def)$avg.width)

  points=c(0,10,20,30,40,50,60,70,80,90)
  cluster=c(1,1,1,2,2,2,3,3,4,4)
  sil=linearsil(points, cluster)
  # expect_equal(sil, 0.41583333)
  sil.def <- cluster::silhouette(cluster, dist(points))
  expect_equal(sil, summary(sil.def)$avg.width)

  points=c(0,10,20,300,400,5000,5100,5200,10000,10001)
  cluster=c(1,1,1,2,2,3,3,3,4,4)
  sil=linearsil(points, cluster)
  # expect_equal(sil, 0.91960731)

  sil.def <- cluster::silhouette(cluster, dist(points))
  expect_equal(sil, summary(sil.def)$avg.width)

  points=c(0,1,10,20,300,400,5000,5100,5200,10000,10001)
  cluster=c(1,1,2,2,3,3,4,4,4,5,5)
  sil=linearsil(points, cluster)
  # expect_equal(sil, 0.78213648)

  sil.def <- cluster::silhouette(cluster, dist(points))
  expect_equal(sil, summary(sil.def)$avg.width)

  points=c(0,1,1000002,10,20,300,400,5000,5100,5200,10000,10001, 1000001)
  cluster=c(1,1,6,2,2,3,3,4,4,4,5,5,6)
  sil=linearsil(points, cluster)
  # expect_equal(sil, 0.81565379)

  sil.def <- cluster::silhouette(cluster, dist(points))
  expect_equal(sil, summary(sil.def)$avg.width)
})
