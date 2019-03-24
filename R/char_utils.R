
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


#' String ruler
#' Fuction to assist with visualizing positions of a string, usually used with long fixed width
#' character vectors
#' @param x - character: string to measure
#' @param split - integer: split lines every [split] characters
#' @param under - logical: underline the ruler digits
#' @param uchar - character: character to use for the underline (default="-")
#' @param nl - integer: number of newlines after each ruled line
#' @param nlchar - character: character to use for the new lines (default=" ")
#'
#' @return character: string with "ruler" above
#' @export
#'
string_ruler<-function(x,split=0,under=T,uchar="-",nl=0,nlchar=" ") {

  slen<-nchar(x[1])
  #strret<-""
  lines<-character(0)
  sp9<-strrep(" ",9)

  if(slen>999) {
    s<-strrep(" ",999)
    s_thou<-""
    sapply(1:(slen%/%1000), function(i) {
      s_thou<<-paste0(s_thou,s,i)
    })
    lines<-c(lines,s_thou)
  }

  if(slen>99) {

    s_hun<-""
    sapply(seq(10,slen,10), function(i) {
      if(i<100) c<-" " else c<-as.character(i%/%100)
      s_hun<<-paste0(s_hun,sp9,c)
    })
    lines<-c(lines,s_hun)
  }

  if(slen>9) {
    s_dec<-""
    sapply(1:(slen%/%10), function(i) {
      s_dec<<-paste0(s_dec,sp9,i%%10)
    })
    lines<-c(lines,s_dec)
  }

  s<-strrep(" ",9)
  s_dig<-""
  sapply(1:slen, function(i) {
    s_dig<<-paste0(s_dig,i%%10)
  })

  lines<-c(lines,s_dig)

  if(under) lines<-c(lines,strrep(uchar,slen) )

  lines<-c(lines,x)
  if(nl>0) lines<-c(lines,rep(strrep(nlchar,slen),nl))

  if(split>0) {
    newlines<-character(0)
    nsplits<-((slen-1)%/%split)+1

    df<-data.frame(tmp=1:length(lines))
    sapply(1:nsplits, function(col) {
      start<-(col-1)*split+1
      end<-start+split-1
      addlines<-substr(lines,start,end)
      newlines<<-c(newlines,addlines)
      df[,col]<<-addlines
      invisible()
    })
    cat(paste0(newlines,collapse = "\n"))

  } else {
    cat(paste0(lines,collapse = "\n"))

  }


  invisible()

}

