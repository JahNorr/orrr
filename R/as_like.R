


#' Can Vector be Coerced to Numeric
#'
#' @param x numeric or character vector
#'
#' @return logical - True if is.numeric or can be coerced to a numeric
#' @export
#'
#' @examples
is.numeric_like <- function(x) {

  ret <- FALSE
  if(is.numeric(x)) {
    return(TRUE)
  } else if(is.character(x)){
    tryCatch(
      {
        x1 <- as.numeric(x)
        return(!is.na(x))
      },
      warning = function(cond) {
        return(FALSE)
      }

    )


  } else {
    return(FALSE)
  }
  ret

}





#' Can Vector be Coerced to Integer
#'
#' @param x integer, numeric, or character vector
#'
#' @return logical - True if is.integer or can be coerced to an integer
#' @export
#'
#' @examples
is.integer_like <- function(x) {

  ret <- FALSE
  if(is.numeric(x)) {
    return(TRUE)
  } else if(is.character(x)){
    tryCatch(
      {
        x1 <- as.integer(x)
        return(TRUE)
      },
      warning = function(cond) {
        return(FALSE)
      }

    )


  } else {
    return(FALSE)
  }
  ret

}


