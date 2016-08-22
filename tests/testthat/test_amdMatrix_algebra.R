library(gpuRclblas)
context("amdMatrix algebra")

# set seed
set.seed(123)

ORDER <- 4

# Base R objects
Aint <- matrix(sample(seq(10), ORDER^2, replace=TRUE), nrow=ORDER, ncol=ORDER)
Bint <- matrix(sample(seq(10), ORDER^2, replace=TRUE), nrow=ORDER, ncol=ORDER)
A <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)
B <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)


test_that("amdMatrix Single Precision Matrix multiplication successful", {

  has_cpu_skip()

  C <- A %*% B

  fgpuA <- amdMatrix(A, type="float")
  fgpuB <- amdMatrix(B, type="float")

  fgpuC <- fgpuA %*% fgpuB

  expect_is(fgpuC, "famdMatrix")
  expect_equal(fgpuC[,], C, tolerance=1e-07,
               info="float matrix elements not equivalent")
})

test_that("amdMatrix Single Precision Matrix Subtraction successful", {

  has_cpu_skip()

  C <- A - B

  fgpuA <- amdMatrix(A, type="float")
  fgpuB <- amdMatrix(B, type="float")

  fgpuC <- fgpuA - fgpuB

  expect_is(fgpuC, "famdMatrix")
  expect_equal(fgpuC[,], C, tolerance=1e-07,
               info="float matrix elements not equivalent")
})

test_that("amdMatrix Single Precision Matrix Addition successful", {

  has_gpu_skip()

  C <- A + B

  fgpuA <- amdMatrix(A, type="float")
  fgpuB <- amdMatrix(B, type="float")

  fgpuC <- fgpuA + fgpuB

  expect_is(fgpuC, "famdMatrix")
  expect_equal(fgpuC[,], C, tolerance=1e-07,
               info="float matrix elements not equivalent")
})

test_that("amdMatrix Double Precision Matrix multiplication successful", {

  has_gpu_skip()
  has_double_skip()

  C <- A %*% B

  dgpuA <- amdMatrix(A, type="double")
  dgpuB <- amdMatrix(B, type="double")

  dgpuC <- dgpuA %*% dgpuB

  expect_is(dgpuC, "damdMatrix")
  expect_equal(dgpuC[,], C, tolerance=.Machine$double.eps ^ 0.5,
               info="double matrix elements not equivalent")
})

test_that("amdMatrix Double Precision Matrix Subtraction successful", {

  has_gpu_skip()
  has_double_skip()

  C <- A - B

  dgpuA <- amdMatrix(A, type="double")
  dgpuB <- amdMatrix(B, type="double")

  dgpuC <- dgpuA - dgpuB

  expect_is(dgpuC, "damdMatrix")
  expect_equal(dgpuC[,], C, tolerance=.Machine$double.eps ^ 0.5,
               info="double matrix elements not equivalent")
})

test_that("amdMatrix Double Precision Matrix Addition successful", {

  has_gpu_skip()
  has_double_skip()

  C <- A + B

  dgpuA <- amdMatrix(A, type="double")
  dgpuB <- amdMatrix(B, type="double")

  dgpuC <- dgpuA + dgpuB

  expect_is(dgpuC, "damdMatrix")
  expect_equal(dgpuC[,], C, tolerance=.Machine$double.eps ^ 0.5,
               info="double matrix elements not equivalent")
})
