#' Read Access Table
#'
#'  Returns a data.frame with the contents of a MS Access table
#'
#' @param file character path/file name of MS Access database file (.accbd, .mdb)
#' @param tbl character Name of the table to fetch
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

  channel <- RODBC::odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file,";pwd=",pwd))
  df <- RODBC::sqlQuery( channel , paste ("select * from ",tbl))
  RODBC::odbcClose(channel)
  df
}

