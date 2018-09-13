
##########################################################################################################
##
##
##
##
#########################################################################################################

#' Center a string in a string with a given length by padding the ends
#'
#' @param x
#' @param len_out
#' @param ctr_char
#'
#' @return
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

#' chars
#'
#' @param chr
#' @param n
#'
#' @return
#' @export
#'
chars<-function(chr,n) {
  paste(rep(x=chr,times=n),sep="",collapse="")
}

#' newlines
#'
#' @param n
#'
#' @return
#' @export
#'
newlines<-function(n) {
  chars(chr="\n",n=n)
}


#' Title
#'
#' @param n
#'
#' @return
#' @export
#'
spaces<-function(n) {
  chars(chr=" ",n=n)
}


