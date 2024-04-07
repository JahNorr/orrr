
require(dplyr)
require(cli)

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

  if(!grepl("/$",proj_path)) proj_path <- paste0(proj_path,"/")

  file<-paste0(proj_path, lib_path,prefix,libname,".R")

  if(! file.exists(file)){
    warning(paste0("The file you are trying to source (",file,") does not exist"))
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


#' R File Search
#'
#' @param find - character: text to search for
#'
#' @return vector of results
#' @export
#'
#' @examples
#' search_r_files("foobar")
#'
search_r_files <- function(find = NULL, ext = "R") {


  r_root <- "~/../r_workspace"

  wd <- getwd()
  setwd(r_root)

  files <- list.files(recursive = T) %>%
    grep(paste0("[.]", ext, "$"), ., value = T)

  found <- NULL

  x <- sapply(files, function(file) {

    f <- gsub(".*/","",file)

    lines <- readLines(file, warn = FALSE)

    fok <- grep(find, lines, value = TRUE)

    # if(length(fok) > 0) {
    #   cat("===================================================\n",file, "\n")
    #   cat(fok, sep = "\n")
    # }

    comm <- stringr::str_trim(fok) %>% substring(1,1) %>% {. == "#"}
    #browser()
    fok <- fok[!comm]

    if(length(fok) > 0) {
      return(c("==============================================================\n",
               file, "\n",
               style_italic(
                 col_red(
                   paste0(fok, sep = "\n")))))
    } else
      return(NULL)
  })

  setwd(wd)

  unname(x) %>% unlist() %>% cat()
}


