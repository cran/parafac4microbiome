test_that("f can be computed with a fac object", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2, output="Fac")
  expect_no_error(parafac_fun(init, X))
})

test_that("f can be computed with a vect object", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2, output="vect")
  expect_no_error(parafac_fun(init, X))
})

test_that("f can be computed with lambdas", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  init = initializePARAFAC(X, 2, output="vect")
  expect_no_error(parafac_fun(init, X, lambdas=c(1,1)))
})

test_that("f is nonzero if any solution is found", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  Fac = initializePARAFAC(X, 2)
  f = parafac_fun(Fac, X)
  expect_gt(f, 0)
})

test_that("f is zero if the perfect solution is found", {
  A = array(rnorm(108,2), c(108,2))
  B = array(rnorm(100,2), c(100,2))
  C = array(rnorm(10,2), c(10,2))
  X = reinflateTensor(A, B, C, returnAsTensor=TRUE)
  Fac = list(A, B, C)
  f = parafac_fun(Fac, X)
  expect_equal(f, 0)
})
