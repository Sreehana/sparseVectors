## HW5 Class/Methods

#' Sparse numeric vector class
#'
#' An S4 class that stores a numeric vector in sparse format,
#' using non-zero values and their positions.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of positions of non-zero values.
#' @slot length Integer scalar giving the full length of the vector.
#'
#' @exportClass sparse_numeric

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)


setValidity("sparse_numeric", function(object) {
  if (length (object@value) != length(object@pos))
    return("values and pos must have the same length" )
  if (any(object@pos < 1L | object@pos > object@length))
    return ("All of the positions in pos must be between 1 and 'length'")
  if (any(duplicated(object@pos)))
    return("There are duplicate positions in pos")
  if (length(object@length) != 1L || object@length <= 0)
    return("'length' must be a single positive integer.")
  TRUE

})

#' @importFrom methods new as coerce show
#' @importFrom stats sd
#' @importFrom graphics points legend
NULL

#' Add two sparse numeric vectors
#'
#' @param x A \code{sparse_numeric} object.
#' @param y A \code{sparse_numeric} object of the same length as \code{x}.
#' @param e1,e2 \code{sparse_numeric} objects (used by the \code{+} operator).
#' @param ... Additional arguments (not used).
#' @return A \code{sparse_numeric} object representing \code{x + y}.
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' @rdname sparse_add
#' @exportMethod sparse_add
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length) {
              stop("Both vectors must have the same length.")
            }

            all_pos <- sort(unique(c(x@pos, y@pos)))
            x_vals <- numeric(length(all_pos))
            y_vals <- numeric(length(all_pos))

            x_match <- match(x@pos, all_pos)
            y_match <- match(y@pos, all_pos)
            x_vals[x_match] <- x@value
            y_vals[y_match] <- y@value

            result_vals <- x_vals + y_vals

            keep <- result_vals != 0
            result_vals <- result_vals[keep]
            result_pos  <- all_pos[keep]

            new("sparse_numeric",
                value  = result_vals,
                pos    = as.integer(result_pos),
                length = x@length)
          })

#' Subtract two sparse numeric vectors
#'
#' @param x A \code{sparse_numeric} object.
#' @param y A \code{sparse_numeric} object of the same length as \code{x}.
#' @param e1,e2 \code{sparse_numeric} objects (used by the \code{-} operator).
#' @param ... Additional arguments (not used).
#' @return A \code{sparse_numeric} object representing \code{x - y}.
#' @export
setGeneric("sparse_sub", function(x,y, ...) standardGeneric("sparse_sub"))

#' @rdname sparse_sub
#' @exportMethod sparse_sub
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x,y) {
            if (x@length != y@length) {
              stop("Both vectors must have the same length")
            }

            all_pos <- sort(unique(c(x@pos, y@pos)))
            x_vals <- numeric(length(all_pos))
            y_vals <- numeric(length(all_pos))

            x_match <- match(x@pos, all_pos)
            y_match <- match(y@pos, all_pos)
            x_vals[x_match] <- x@value
            y_vals[y_match] <- y@value

            result_vals <- x_vals - y_vals

            keep <- result_vals != 0
            result_vals <- result_vals[keep]
            result_pos  <- all_pos[keep]

            new("sparse_numeric",
                value  = result_vals,
                pos    = as.integer(result_pos),
                length = x@length)

          })
#' Multiply two sparse numeric vectors elementwise
#'
#' @param x A \code{sparse_numeric} object.
#' @param y A \code{sparse_numeric} object of the same length as \code{x}.
#' @param e1,e2 \code{sparse_numeric} objects (used by the \code{*} operator).
#' @param ... Additional arguments (not used).
#' @return A \code{sparse_numeric} object representing elementwise multiplication.
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' @rdname sparse_mult
#' @exportMethod sparse_mult
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length) {
              stop("Both vectors must have the same length.")
            }

            common_pos <- intersect(x@pos, y@pos)
            if (length(common_pos) == 0) {
              return(new("sparse_numeric",
                         value = numeric(0),
                         pos = integer(0),
                         length = x@length))
            }

            x_vals <- x@value[match(common_pos, x@pos)]
            y_vals <- y@value[match(common_pos, y@pos)]
            result_vals <- x_vals * y_vals

            new("sparse_numeric",
                value  = result_vals,
                pos    = as.integer(common_pos),
                length = x@length)
          })
#' @rdname sparse_add
#' @exportMethod +
setMethod("+", c("sparse_numeric","sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_sub
#' @exportMethod -
setMethod("-", c("sparse_numeric","sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' @rdname sparse_mult
#' @exportMethod *
setMethod("*", c("sparse_numeric","sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

#' Sparse cross product
#'
#' @param x A \code{sparse_numeric} object.
#' @param y A \code{sparse_numeric} object of the same length as \code{x}.
#' @param ... Additional arguments (not used).
#' @return A numeric scalar equal to \code{sum(x * y)}.
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' @rdname sparse_crossprod
#' @exportMethod sparse_crossprod

setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y) {
            if (x@length != y@length) {
              stop("Both vectors must have the same length.")
            }

            common_pos <- intersect(x@pos, y@pos)
            if (length(common_pos) == 0) return(0)

            x_vals <- x@value[match(common_pos, x@pos)]
            y_vals <- y@value[match(common_pos, y@pos)]
            sum(x_vals * y_vals)
          })

#' Compute the Euclidean norm of a sparse numeric vector
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Additional arguments (not used).
#' @return A numeric scalar equal to \code{sqrt(sum(x^2))}.
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @rdname norm
#' @exportMethod norm
setMethod("norm", "sparse_numeric", function(x, ...) {
  sqrt(sum(x@value^2))
})

#' Standardize a sparse numeric vector
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Additional arguments (not used).
#' @return A \code{sparse_numeric} object representing the standardized vector.
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @rdname standardize
#' @exportMethod standardize
setMethod("standardize", "sparse_numeric", function(x, ...) {
  full <- as(x, "numeric")
  mu <- mean(full)
  sd_full <- sd(full)

  if (is.na(sd_full) || sd_full == 0) {
    return(new("sparse_numeric",
               value  = numeric(0),
               pos    = integer(0),
               length = x@length))
  }

  full_std <- (full - mu) / sd_full

  as(full_std, "sparse_numeric")
})


#' Coercion between numeric and sparse_numeric
#'
#' These methods convert between dense numeric vectors and sparse_numeric
#' vectors.
#'
#' @name coerce_sparse
#' @aliases coerce,numeric,sparse_numeric-method
#'   coerce,sparse_numeric,numeric-method
NULL

#' Coerce numeric to sparse_numeric
#'
#' Converts a dense numeric vector into a \code{sparse_numeric} object by
#' storing only non-zero entries and their positions.
#'
#' @param from A numeric vector.
#' @return A \code{sparse_numeric} object.
#' @exportMethod coerce
#' @rdname coerce_sparse
#' @name coerce_sparse
setAs("numeric", "sparse_numeric", function(from) {
  new("sparse_numeric",
      value  = from[from != 0],
      pos    = which(from != 0),
      length = as.integer(length(from)))
})

#' Coerce sparse_numeric to numeric
#'
#' Converts a \code{sparse_numeric} object into a full dense numeric vector.
#'
#' @param from A \code{sparse_numeric} object.
#' @return A numeric vector.
#' @exportMethod coerce
#' @rdname coerce_sparse
#' @name coerce_sparse
setAs("sparse_numeric", "numeric", function(from) {
  x <- numeric(from@length)
  x[from@pos] <- from@value
  x
})

#' Show method for sparse_numeric
#'
#' Prints a human-readable summary of a sparse_numeric vector.
#'
#' @param object A sparse_numeric object.
#' @return Invisibly returns \code{object}.
#' @exportMethod show
#' @rdname show_sparse
setMethod("show", "sparse_numeric", function(object) {
  cat("Sparse numeric vector of length", object@length, "\n")
  if (length(object@pos) == 0) {
    cat("(all zeros)\n")
  } else {
    cat("Non-zero values:\n")
    df <- data.frame(pos = object@pos, value = object@value)
    print(df, row.names = FALSE)
  }
})

#' Plot method for sparse_numeric objects
#'
#' Plots the non-zero entries of two sparse_numeric vectors for comparison.
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Additional arguments passed to \code{plot()} (not used).
#' @return Invisibly returns \code{NULL}.
#' @exportMethod plot
#' @rdname plot_sparse
setMethod("plot", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            plot(x@pos, x@value,
                 col = "pink", pch = 16,
                 xlab = "Position", ylab = "Value",
                 main = "Sparse Numeric Comparison",
                 xlim = c(1, max(x@length, y@length)))
            points(y@pos, y@value, col = "purple", pch = 17)
            legend("topright", legend = c("x", "y"),
                   col = c("pink", "purple"), pch = c(16, 17))
          })

#' Mean for sparse_numeric
#' Computes the mean of all entries in a sparse_numeric vector,
#' including the implicit zeros.
#'
#' @param x A sparse_numeric object.
#' @param ... Additional arguments (not used).
#' @return Numeric scalar giving the mean.
#' @exportMethod mean
#' @rdname mean
setMethod("mean", "sparse_numeric", function(x, ...) {
  total <- sum(x@value)
  n <- x@length
  total / n
})
