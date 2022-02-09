

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
#' @param quotes - logical - names are quoted?
#' @param vert - logical - separate names by a newline?
#' @param sep - character - character to separate names by, default = ","
#'
#' @return - nothing
#' @export
#'
#' @examples
#' colnames.pretty(mtcars)
#' colnames.pretty(mtcars,vert=TRUE)
#'
colnames.pretty<-function(df,quotes=TRUE,vert=F, sep=",") {

  cnames<-colnames(df)
  if(vert) vchar<-"\n" else vchar<-""
  if(quotes) qchar<-"\"" else qchar<-""

  x<-paste0(cnames,collapse = paste0(qchar,sep,vchar,qchar))
  x<-paste0(qchar,x,qchar)

  cat(x)
}
