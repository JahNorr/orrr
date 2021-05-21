

#############################################################
##
##  this creates a vertical list of column names
##    that are quoted and separated by commas
##    useful for getting a usable list for copying and pasting
##

#' Pretty Column Names
#' this prints a vertical list of column names
#'   that are quoted and separated by commas
#'    useful for getting a usable list for copying and pasting
#'
#' @param df - a data.frame
#' @param vert - logical - separate names by a newline?
#'
#' @return - nothing
#' @export
#'
#' @examples
#' colnames.pretty(mtcars)
#' colnames.pretty(mtcars,vert=TRUE)
#'
colnames.pretty<-function(df,vert=F) {

  cnames<-colnames(df)
  if(vert) vchar<-"\n" else vchar<-""
  x<-paste0(cnames,collapse = paste0("\",",vchar,"\""))
  x<-paste0("\"",x,"\"")

  cat(x)
}
