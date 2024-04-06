

#' Project Directory
#'
#'  Returns the path of the project directory and adds and other folders passed
#'  as the names argument
#'
#' @param names character vector of folder names in a branch. If missing, this function
#' will return project root folder
#' @param slash logical append slash at end of path. Default is TRUE.
#
#' @return character - path of the project directory/folder
#' @export
#'
#' @examples
#' \dontrun{
#' dir.project()
#'
#' dir.project("data")
#'
#' # following are equivalent
#' dir.project("data/excel/csv")
#' dir.project(c("data","excel","csv"))
#'}
#'
dir.project<-function(names,useapi = TRUE, slash=T) {

  #############################################################
  ##
  ##   never found this function when looking years ago
  ##    ... the wd in shiny and markdown files was not the
  ##      base project folder so I found it my own way
  ##      but this rstudioapi function does it ...
  ##      problem though ... You can't use this package
  ##      unless you are in an interactive RStudio session.
  ##      So you get the error ... Error: RStudio not running ... when using Quarto

  if (useapi) {
    wd <- rstudioapi::getActiveProject()

  } else {
    #############################################################
    ##
    ##    i guess the below was unnecessary for so many years

    wd<-getwd()
    #
    files<-list.files(wd)
    ynp<-grep("\\.Rproj$",files)
    while(length(ynp)==0 && (!any(grep("^[A-Z]:/$",wd)))) {
      wd<-dirname(wd)
      #
      files<-list.files(wd)
      ynp<-grep("\\.Rproj$",files)
    }

  }

  ####################################################
  ##
  ##    add rest of path indicated in arguments

  if(!missing(names)) {
    file_list<-paste(names,collapse="/")
    wd<-paste0(wd,"/",file_list)
  }
  if(slash) wd<-paste(wd,"/",sep="")
  wd
}
#' Project Directory Exists
#'
#'  Returns whether or not the folder exists, and optionally creates the path (recursively) if missing
#'
#' @param names character vector of folder names in a branch
#' @param create logical create folders if missing?
#'
#' @return logical - does the folder exist
#' @export
#'
#' @examples
#' \dontrun{
#' dir.project.exists("data")
#'}

dir.project.exists<-function(names, create=F) {
  path<-orrr::dir.project(names)
  exists<-dir.exists(path)

  if(!exists && create) {
    exists<-dir.create(path,recursive = T)

  }

  exists
}

#' Create Project Directory
#'
#'  Returns whether or not the folder was created
#'
#' @param names character vector of folder names in a branch
#'
#' @return character - path, or empty string if not successful
#' @export
#'
#' @examples
#' \dontrun{
#' dir.project.create("data")
#'}

dir.project.create<-function(names) {
  path<-orrr::dir.project(names)
  exists<-dir.exists(path)

  if(!exists) {
    success<-dir.create(path,recursive = T)
  } else {
    success<-T
  }

  if (success) return (path) else return ("")
}


#' Normalize path
#'
#'  Returns a vector substituting the project folder for .
#'
#' @param names character vector of path name
#'
#' @return character - path, or empty string if not successful
#' @export
#'
#' @examples
#' \dontrun{
#' convert.dot("./data")
#' }
#'
convert.dot<-function(path) {
  dir<- orrr::dir.project(slash = F)

  gsub("^[.]",dir,path)

}
