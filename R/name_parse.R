

#' Retieve additional name data from the character vector containing the full name
#'
#' @param name - character: the full name of the person in Meditech name format (LASTNAME [SUFFIX],FIRSTNAME MIDDLENAME [SUFFIX])
#' @param hyphen - logical: if TRUE, substitute hyphens for spaces in last name. Some applications prefer no spaces in last names.
#'
#' @return list : elements are: name, last_name, first_name, middle_name, Suffix
#' @export
#'
#' @examples
#' parse_name("JOHNSON SR, JAMES R.")
#'
parse_name<-function(name,hyphen=F) {

  dxl<-list()

  dxl$name<-name
  dxl$last_name<-""
  dxl$first_name<-""
  dxl$middle_name<-""
  dxl$suffix<-""


  ##############
  # get rid of periods

  nm<-gsub("\\."," ",name)

  ##############
  # get space after MC e.g. MC INTOSH

  nm<-gsub("(MC|Mc|mc) ","\\1",nm)

  ##########################
  # handle comma
  if (!grepl(pattern = ",",x = nm)) {
    nms<-strsplit(x=nm, split = " ")[[1]]
    if(stringr::str_to_lower(nms[length(nms)])%in%c("jr","sr","ii","iii","iv","v","vi","vii","viii","ix","x")) {
      dxl$suffix<-nms[length(nms)]
      nms<-nms[1:length(nms)-1]
    }
    nm<-paste0(nms[length(nms)],", ", paste(nms[1:length(nms)-1], collapse=" "))
  }
  ##############
  # strip last name (everything before comma)

  last_name<-gsub("(.*),(.*)","\\1",nm)
  nms<-strsplit(x=last_name, split = " ")[[1]]
  dxl$last_name<-nms[1]

  if (length(nms)>1){
    sapply(nms[2:length(nms)],function(nm) {
      sep<-ifelse(hyphen,"-"," ")

      if(stringr::str_to_lower(nm)%in%c("jr","sr","ii","iii","iv","v","vi","vii","viii","ix","x")) {

        dxl$suffix<<-nm
      } else {
        dxl$last_name<<-paste(dxl$last_name,nm,sep=sep)
      }
    })
  }
  ##############################################
  # continue with everything after the comma

  nm<-gsub("(.*),(.*)","\\2",nm)

  nms<-strsplit(x=nm, split = " ")[[1]]
  nms<-nms[nchar(nms)>0]
  #########################################
  #
  #
  dxl$first_name<-nms[1]

  if (length(nms)>1){
    sapply(nms[2:length(nms)],function(nm) {
      sep<-" "

      if (grepl("(JR|SR|II|II|IV)",nm)) {
        dxl$suffix<<-nm
      } else {
        dxl$middle_name<<-paste(dxl$middle_name,nm,sep=sep)
      }
    })
  }
  dxl$middle_name<-gsub("^ {0,}(.*) {0,}$","\\1",dxl$middle_name)

  dxl
}
