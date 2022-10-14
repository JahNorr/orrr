

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
#' @param sort - logical - sort names?
#' @param quotes - logical - names are quoted?
#' @param vert - logical - separate names by a newline?
#' @param sep - character - character to separate names by, default = ","
#' @param colwid - integer - width of the string holding each column name (uses padding)
#'
#' @return - nothing
#' @export
#'
#' @examples
#' colnames.pretty(mtcars)
#' colnames.pretty(mtcars,vert=TRUE)
#'
colnames.pretty<-function(df,sort = TRUE, quotes = TRUE, vert = FALSE, sep=",", colwid = NULL) {

  cnames<-colnames(df)
  if(!is.null(colwid)) cnames <- stringr::str_pad(cnames,colwid)
  if(sort) cnames <- sort(cnames)
  if(vert) vchar<-"\n" else vchar<-""
  if(quotes) qchar<-"\"" else qchar<-""

  x<-paste0(cnames,collapse = paste0(qchar,sep,vchar,qchar))
  x<-paste0(qchar,x,qchar)

  cat(x)
}
