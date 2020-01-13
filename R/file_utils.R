
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

