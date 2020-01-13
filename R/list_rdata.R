
#' Data file items
#'
#'  Returns the names of items in a data file
#'
#' @param file - character name of file
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


#' Get Data file item
#'
#'  Returns the nth item in a data file
#'
#' @param file - character name of file
#' @param item - integer item to retrieve (default=1)
#' @return nth item in a .RData file
#' @export
#'
#' @examples
#' dir.project()
#'

get.rdata<-function(file,item=1){
  e<-new.env()

  load(file = file,envir = e)
  get(ls(e)[item],envir = e)
}
