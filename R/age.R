


#' Add Age Intervals
#'
#'Add a column (factor) with plain language description of the age range
#'
#' @param df - data.frame
#' @param breaks - integer - vector of age intervals (passed to cut)
#' @param age_col - character - name of column with numeric ages
#' @param group_col  - character - name of factor column with text age ranges
#' @param under - character - text to use for less/younger than
#' @param bridge - character - text to use between the min and max
#' @param over - character - text to use for greater/older than
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
add_age_intervals <- function(df, breaks=NULL, age_col = "Age", group_col = "AgeGroup",
                              under = "Younger than ",
                              bridge = " to ",
                              over = " or Older",  keep_attrs = FALSE) {

  if(!is.null(breaks)) {
    ages <- df %>% pull(!!age_col)
    group  <-   ages %>% cut(breaks = breaks, right = FALSE)

    lvls <- levels(group)
    lvls <- gsub(","," - ", lvls) %>%
      gsub("\\(|\\[|\\]|\\)","", .)

    age0  <-  as.integer(gsub("(.*)-.*","\\1",lvls))
    age1  <-  as.integer(gsub("(.*)-(.*)","\\2",lvls))-1

    age <-  ifelse(age0 == 0, paste0(under,  age1+1),paste0(age0, bridge, age1))
    age <-  ifelse(age1 > 120, paste0( age0, over),age)
    age <-  ifelse(age0 == age1 & age0 != 0, as.character(age0) , age)

    levels(group) <- age

    if(keep_attrs) {
      attributes(group) <- c(attributes(group),attributes(ages))
    }

    df <- df %>% mutate({{group_col}} := group)
    #attributes(df[[group_col]]) <- attribs
  }

  df
}
