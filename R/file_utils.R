
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
#'
x <-  7



#' R File Search
#'
#'  Sends formatted text to command line showing results for the search
#'
#' @param find - character: text to search for
#' @param ext - character - extension (default: .R)
#' @param r_root - character - folder root for searching for .R files
#' @param file_col - character - filename text color (use #hhhhhh format )
#' @param results_col - character - results text color (use #hhhhhh format )
#' @param results_bg - character - results bg color (use #hhhhhh format )
#' @param ignore_comments - logical - ignore all lines that are comments (start with #)
#' @param ... - unused
#'
#' @return
#' @export
#'
#' @examples
#' search_r_files("foobar")


search_r_files <- function(find = NULL, ext = "R", r_root =  "~/../r_workspace",
                           file_col = "#4444ff", results_col = "red", results_bg = "#f7f7f7",
                           ignore_comments = TRUE, ...) {

  require(cli)

  if(is.null(r_root)) r_root <-  rstudioapi::readRStudioPreference("default_open_project_location", NULL)
  if(is.null(r_root)) r_root <- paste0("../", here::here())

  wd <- getwd()
  setwd(r_root)

  files <- list.files(recursive = T) %>%
    grep(paste0("[.]", ext, "$"), ., value = T)

  if(grepl("^#",results_col)) {
    results_col <- make_ansi_style(results_col)
  } else {
    results_col <- paste0("col_", results_col)
  }

  if(grepl("^#",file_col)) {
    file_col <- make_ansi_style(file_col)
  } else {
    file_col <- paste0("col_", file_col)
  }

  if(grepl("^#",results_bg)) {
    results_bg <- make_ansi_style(results_bg, bg = TRUE)
  } else {
    results_bg <- paste0("bg_", results_bg)
  }

  found <- NULL

  x <- sapply(files, function(file) {

    f <- gsub(".*/","",file)

    lines <- readLines(file, warn = FALSE)

    fok <- grep(find, lines, value = TRUE)

    if(ignore_comments) {
      comm <- stringr::str_trim(fok) %>% substring(1,1) %>% {. == "#"}
      #browser()
      fok <- fok[!comm]
    }

    if(length(fok) > 0) {
      file_txt <- do.call(file_col, list(file) )
      file_txt <- do.call(paste0("style_", "bold"), list(file_txt) )

      #browser()
      res_txt <- paste0(fok, sep = "\n")
      res_txt <- do.call(results_col, list(res_txt) )
      res_txt <- do.call(paste0("style_", "italic"), list(res_txt) )

      rep_txt <- do.call(results_bg, list(find) )
      #rep_txt <- do.call(paste0("col_", "white"), list(rep_txt) )

      #rep_txt <- find
      res_txt <- gsub(find, rep_txt,res_txt)
      txt <-  c("==============================================================\n",
                file_txt, "\n",
                res_txt)

      return(txt)
    } else
      return(NULL)
  })

  setwd(wd)

  unname(x) %>% unlist() %>% cat()
}

