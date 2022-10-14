
#
# build_names <- function() {
#   df_last_names <- read.csv("./data_raw/LastNames.csv")
#   usethis::use_data(df_last_names, overwrite = TRUE, internal = TRUE)
#   df_first_names <- read.csv("./data_raw/FirstNames.csv")
#   usethis::use_data(df_first_names, overwrite = TRUE, internal = TRUE)
#
# }

#' Load Data Frame with Random First Names
#'
#' @param df
#' @param name_col - column name with first name data
#' @param gender_col- column name with gender data
#' @param genders- gender text for female,male (default=c("F","M"))
#'
#' @return
#' @export
#'
#' @examples
random_first_name <- function(df, name_col, gender_col = NULL, genders = c("F","M")) {

  require(dplyr)

  fn <- df_first_names %>% filter(Frequency>600)

  fn_f <- fn %>% filter(Gender=="F")
  fn_m <- fn %>% filter(Gender=="M")

  #browser()

  nrows <- nrow(df)

  rnd <- sample(nrow(fn),size = nrows,replace = TRUE)
  df_new <- data.frame(A = toupper(fn$Name[rnd]))

  rnd <- sample(nrow(fn_f),size = nrows,replace = TRUE)
  #fn_new_f <- toupper(fn_f$Name[rnd])
  df_new <- df_new %>% mutate(F = toupper(fn_f$Name[rnd]))

  rnd <- sample(nrow(fn_m),size = nrows,replace = TRUE)
  #fn_new_m <- toupper(fn_m$Name[rnd])
  df_new <- df_new %>% mutate(M = toupper(fn_m$Name[rnd]))

  if(!is.null(gender_col)) {
    df_new <- df_new %>%
      mutate(G = (df %>% pull({{gender_col}}))) %>%
      mutate(A = ifelse(G==genders[1],F,M))

  }


  names <- df_new %>% pull(A)

  df <- df  %>%
    mutate({{name_col}} := names)

  df

}

#' Load Data Frame with Random Last Names
#'
#' @param df data.frame - data.frame to store the last name column
#' @param name_col character - column name with last name data
#' @param n integer number of names if df is NULL
#'
#' @return
#' @export
#'
#' @examples
random_last_name <- function(df = NULL, name_col = NULL, n=1) {

  require(dplyr)

  if(is.null(name_col)) name_col <- "last_name"

  if(is.null(df) ) {

    df <- data.frame(last_name = character(n)) %>%
      rename(last_name = {{name_col}})

  } else {
    n <- nrow(df)
  }

  ln <- df_last_names

  nrows <- nrow(df)

  rnd <- sample(nrow(ln),size = nrows,replace = TRUE)
  ln_new <- ln$name[rnd]

  df <- df  %>%
    mutate({{name_col}} :=  ln_new)

  df

}

