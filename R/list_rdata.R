
#' Data file items
#'
#'  Returns the names of items in a data file
#'
#'@param file - character name of file
#' @return character - vector of names of items in a .RData file
#' @export
#'
#' @examples
#' dir.project()
#'

list.rdata<-function(file){
  e<-new.env()

  load(file = file,envir = e)
  ls(e)
}
