## HW5_testscript.R


test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("sparse add generic", {
  expect_true(isGeneric("sparse_add"))
})

test_that("sparse mult generic", {
  expect_true(isGeneric("sparse_mult"))
})

test_that("sparse sub generic", {
  expect_true(isGeneric("sparse_sub"))
})

test_that("sparse crossprod generic", {
  expect_true(isGeneric("sparse_crossprod"))
})
test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

test_that("mean() works correctly for sparse_numeric", {
  x_dense <- c(1, 0, 2, 0)
  x_sparse <- as(x_dense, "sparse_numeric")

  expect_equal(mean(x_sparse), mean(x_dense))
})

test_that("mean() handles all-zero sparse vectors", {
  x_sparse <- as(c(0, 0, 0, 0), "sparse_numeric")
  expect_equal(mean(x_sparse), 0)
})

test_that("norm() computes Euclidean norm correctly", {
  x_dense <- c(3, 4, 0)
  x_sparse <- as(x_dense, "sparse_numeric")

  expect_equal(norm(x_sparse), 5)
})

test_that("norm() is zero for all-zero sparse vectors", {
  x_sparse <- as(c(0, 0, 0), "sparse_numeric")
  expect_equal(norm(x_sparse), 0)
})

test_that("standardize() produces mean 0 and sd 1 for non-constant vectors", {
  x_dense <- c(1, 2, 3, 4)
  x_sparse <- as(x_dense, "sparse_numeric")

  x_std <- standardize(x_sparse)
  x_std_dense <- as(x_std, "numeric")

  expect_equal(mean(x_std_dense), 0, tolerance = 1e-8)
  expect_equal(sd(x_std_dense), 1, tolerance = 1e-8)
})

test_that("standardize() returns all zeros for all-zero vector", {
  x_sparse <- as(c(0, 0, 0, 0), "sparse_numeric")
  x_std <- standardize(x_sparse)

  expect_equal(as(x_std, "numeric"), rep(0, 4))
})

test_that("standardize() returns all zeros for constant vectors", {
  x_dense <- c(5, 5, 5)
  x_sparse <- as(x_dense, "sparse_numeric")

  x_std <- standardize(x_sparse)
  expect_equal(as(x_std, "numeric"), c(0, 0, 0))
})

test_that("mean, norm, and standardize work on length-1 vector", {
  x_sparse <- as(c(7), "sparse_numeric")

  expect_equal(mean(x_sparse), 7)
  expect_equal(norm(x_sparse), 7)

  x_std <- standardize(x_sparse)
  expect_equal(as(x_std, "numeric"), 0)
})

test_that("operator + works for sparse_numeric", {
  x <- as(c(0, 2, 0, 4), "sparse_numeric")
  y <- as(c(1, 0, 3, 0), "sparse_numeric")

  result <- x + y
  expect_equal(as(result, "numeric"), c(1, 2, 3, 4))
})

test_that("operator - works for sparse_numeric", {
  x <- as(c(3, 2, 1), "sparse_numeric")
  y <- as(c(1, 1, 1), "sparse_numeric")

  result <- x - y
  expect_equal(as(result, "numeric"), c(2, 1, 0))
})

test_that("operator * works for sparse_numeric", {
  x <- as(c(2, 0, 3), "sparse_numeric")
  y <- as(c(4, 0, 5), "sparse_numeric")

  result <- x * y
  expect_equal(as(result, "numeric"), c(8, 0, 15))
})

test_that("sparse_crossprod works with no overlapping non-zero positions", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 3), "sparse_numeric")

  expect_equal(sparse_crossprod(x, y), 0)
})

test_that("sparse_crossprod works with full overlap", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(4, 5, 6), "sparse_numeric")

  expect_equal(sparse_crossprod(x, y), sum(c(1*4, 2*5, 3*6)))
})

test_that("show() prints without error", {
  x <- as(c(0, 5, 0, 6), "sparse_numeric")
  expect_output(show(x))
})

test_that("plot() works for sparse_numeric objects", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(0, 3, 4), "sparse_numeric")

  # Expect no graphical errors
  expect_no_error(plot(x, y))
})

test_that("sparse_mult returns empty sparse vector when no positions match", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 0), "sparse_numeric")

  result <- sparse_mult(x, y)
  expect_equal(length(result@value), 0)
  expect_equal(length(result@pos), 0)
})

test_that("sparse_sub fails with mismatched lengths", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(1, 2), "sparse_numeric")

  expect_error(sparse_sub(x, y))
})
