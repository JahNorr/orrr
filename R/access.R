#' Read Access Table
#'
#'  Returns a data.frame with the contents of a MS Access table
#'
#' @param file character path/file name of MS Access database file (.accbd, .mdb)
#' @param tbl character Name of the table to fetch - if missing, returns the name of all tables
#' @param pwd character Password, default is ""
#
#' @return character - path of the project directory/folder
#' @export
#'
#' @examples
#'\dontrun{
#' file<-"C:/Data/xyz.accdb"
#' tbl<-"tblSomeData"
#' df<-read.access.table(file,tbl,pwd="uJ80s$R")
#'
#'
#'}
read.access.table<-function(file,tbl,pwd = "") {

  if(missing(file)) {
    file<-choose.files(multi = F)

    if(length(file)==0) return(invisible())
  }

  channel <- RODBC::odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file,";pwd=",pwd))

  if(missing(tbl)) {

    df <- RODBC::sqlTables( channel)

  } else {
    df <- RODBC::sqlQuery( channel , paste0 ("SELECT * FROM ",tbl))
  }
  RODBC::odbcClose(channel)
  df
}

