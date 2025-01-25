
#' Keep rows that match any condition
#'
#' @param df A data frame, data frame extension (e.g. a tibble),
#' @param ... <data-masking> A variable in .data
#' @param patterns A vector of patterns with which the variable will be compared with grepl
#' @param invert  logical. If TRUE return rows for elements that do not match.
#' @param fixed  logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments
#' @param ignore.case  logical. if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
#' @return The filter_any() function is used to subset the rows of .data, applying the patterns to the variable (...) to determine which rows should be retained.
#' @export
#'
#' @examples
#'
#' require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
#'
#' my_cars <- c("mazda", "Merc 2.*")
#' mtcars %>%
#'   mutate(car = row.names(.)) %>%
#'   filter_any(car, patterns = my_cars, ignore.case = T) %>%
#'   select(-car)
#'
#' starwars %>% data.frame() %>%
#'   filter_any(species,patterns = c("human","droid"), ignore.case = T, invert = T) %>%
#'   select(name, species) %>%
#'   arrange(species, name)


filter_any <- function(df, ..., patterns, invert = FALSE, fixed = FALSE, ignore.case = FALSE) {

  quosures <- quos(..., .ignore_empty = "all")
  quo <- quosures[[1]]

  expr <- rlang::quo_get_expr(quo)
  from <- as.character(expr)

  found <- rep(FALSE, nrow(df))

  sapply(patterns,function(pattern) {
    x <- grepl(pattern, df[[from]],ignore.case = ignore.case, fixed = fixed)
    found <<- found | x
  })

  if(invert) found <- !found

  df %>% filter(found)
}



#' Convert column of integers to excel dates
#'
#' @param df - data.frame
#' @param ... - column to convert
#'
#' @return
#' @export
#'
#' @examples
as_excel_dates<-function(df, ...) {
  quosures <- quos(..., .ignore_empty = "all")
  quo <- quosures[[1]]
  nm <- names(quosures[1])
  expr <- rlang::quo_get_expr(quo)
  if(nm == "") nm <- expr

  dts <- df %>% pull({{expr}})

  df %>% mutate({{nm}} := get(expr)) %>%
    mutate({{nm}} := as.Date(as.integer(get(nm)), origin ="1899-12-30"))
}
