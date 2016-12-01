

#' Project Directory
#'
#'  Returns the name of the project directory by stepping out from the working directory until a *.Rproj file is found
#'
#' @param names character vector of folder names in a branch. if missing will return project root folder
#' @param slash logical append slash at end of path. Default is TRUE.
#
#' @return character - path of the project directory/folder
#' @export
#'
#' @examples
#' dir.project()
#'
#' dir.project("data")
#'
#' # following are equivalent
#' dir.project("data/excel/csv")
#' dir.project(c("data","excel","csv"))
#'
#'
dir.project<-function(names,slash=T) {
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
#' dir.project.exists("data")
#'

dir.project.exists<-function(names, create=F) {
  path<-orrr::dir.project(names)
  exists<-dir.exists(path)

  if(!exists && create) {
    exists<-dir.create(path,recursive = T)

  }

  exists
}

