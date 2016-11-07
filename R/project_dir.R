

#' Project Directory
#'
#'  Returns the name of the project directory by stepping out from the working directory until a *.Rproj file is found
#'
#' @return character - name of trhe project directory/folder
#' @export
#'
#' @examples
#' dir.project()
#'
dir.project<-function(slash=F) {
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
  if(slash) wd<-paste(wd,"/",sep="")
  wd
}
