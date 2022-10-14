
#' Matching Parentheses
#'
#' Finds matching parentheses in a string. Returns a data frame with each row representing a matching pair
#' of parentheses, `op` being the opening position and `cl` being the closing position. If processed in row
#' order, you should never see an outer set before its inner set(s)
#'
#' @param text
#'
#' @return data frame with matching pair positions
#' @export
#'
#' @examples
#' txt <- "((x + 5) * (y - 7)) / (z^2 + 4)"
#' matching_parens(txt)
#'
matching_parens <- function(text) {
  require(dplyr)
  opens <- gregexpr("(",text, fixed  = T)[[1]]
  closes <- gregexpr(")",text, fixed  = T)[[1]]


  pairs <- sapply(sort(opens, decreasing = T),function(op) {
    x <- min(which(closes>op))
    cl <- closes[x]
    closes <<- closes[-x]

    c(op = op, cl = cl)
  })

  as.data.frame(pairs) %>% t() %>% {rownames(.) <- NULL;.} %>% as.data.frame()
}
