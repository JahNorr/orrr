
##########################################################################################################
##
##
##
##
#########################################################################################################

#' Center a string in a string with a given length by padding both ends with spaces
#'
#' @param x - input string
#' @param len_out - total length of output string
#' @param ctr_char - char - character to center on
#'
#' @return character - a padded string
#' @export
#'
center_string<-function(x,len_out,ctr_char) {
  if(missing(len_out)) len_out<-nchar(x)
  x<-trimws(x)

  len_in<-nchar(x)

  if(len_in>len_out) {
    ret<-substr(x,1,len_out)
  } else {
    if(missing(ctr_char)){
      spc1<-as.integer((len_out-len_in)/2)
      spc2<-len_out-len_in-spc1
    } else {
      browser()
      pos = regexpr(ctr_char,x)
      if(pos>0) {
        spc1<-as.integer(len_out/2) - pos
        spc2<-len_out-len_in-spc1
      }
    }
    ret<-paste(spaces(spc1),x,spaces(spc2),sep="")
  }

  ret
}


#' Repeat newlines
#'
#' @param n - integer: number of times to repeat
#'
#' @return
#' @export
#'
newlines<-function(n) {
  strrep(x = "\n",times = n)
}


#' Repeat spaces
#'
#' @param n - integer: number of times to repeat
#'
#' @return
#' @export
#'
spaces<-function(n) {
  strrep(x = " ",times = n)
}


