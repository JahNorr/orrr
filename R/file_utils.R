
#' returns the lines of a file as a character vector
#'
#' @param filename - character - name of file to readLoines from
#'
#' @return character - the lines of the file minus blank lines
#' @export
#'


getLines<-function(filename) {
  conn <- file(filename,open="r")
  opt<-getOption("warn")
  options(warn=-1)
  lines <-readLines(conn,ok = T)
  options(warn=opt)
  close(conn)

  lines<-as.character(sapply(lines,function(line) {
    as.character(gsub(pattern = "\\t","",line))
  }))

  #
  #   get rid of lines that are blank or just spaces
  #
  as.vector(lines[grep("^ {0,}$",lines,invert=T)])

}


#' sources the file (usually a script file of r functions)
#'
#' @param libname - character - name of file to readLoines from
#' @param proj_path - character - path to the project,  passed to orrr::dir.project; default value= "orrr::dir.project()"
#' @param lib_path - character - path to the project,  passed to orrr::dir.project; default value= "code/libs/"
#' @param prefix - character - prefix added to lib to get filename; default value = "lib_"
#' @param verbose - logical - cat the filename; default value = FALSE
#'
#' @return the value of the sourcing of the file
#' @export
#'

source_lib<-function(libname, proj_path = orrr::dir.project(), lib_path = "code/libs/",prefix="lib_", verbose=FALSE) {

  file<-paste0(proj_path, libpath,prefix,libname,".R")

  if(! file.exists(file)){
    warning(paste0("The file you are trying to source (",filename,") does not exist"))
  } else {
    if(verbose) cat(paste0("[",file,"]","\n"))
    source(file)
  }
}



# source_lib<-function(lib,path=c("code","libs"),prefix="lib_") {
#
#   #
#   #   set the filename
#   #
#   filename<-paste0(orrr::dir.project(path),prefix,lib,".R")
#
#   if(!file.exists(filename)) {
#     warning(paste0("The file you are trying to source (",filename,") does not exist"))
#   } else {
#     source(filename)
#   }
# }

