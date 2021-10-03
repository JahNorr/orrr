
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
#' @param item - integer item to retrieve (default=1) or character matching one item (using grep)
#' @return requested item in a .RData file
#' @export
#'
#' @examples
#' dir.project()
#'


get.rdata<-function(file,item=1){
  e<-new.env()

  load(file = file,envir = e)
  if(is.character(item)) {
    match<-grep(item,ls(e))
    nmatches<-length(match)
    if(nmatches>0) {
      item<-match[1]
      if(nmatches>1) warning("There was more than 1 match, using the first one")
    } else {
      match<-0
      warning("There were no matches.")
      return(NULL)
    }
  }

  get(ls(e)[item],envir = e)
}



