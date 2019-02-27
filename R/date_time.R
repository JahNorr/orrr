#' Number of integral years between two Dates
#'
#'  Returns the number of years (integer) between two dates. The fraction is truncated. (like age)
#'
#' @param date1 date earlier date
#' @param date2 date later date
#
#' @return integer - number of years between date1 and date2
#' @export
#'
#' @examples
#'
#' dt1<-as.Date("2000-01-01")
#' years_between(dt1,Sys.Date())
#

years_between <- function(date1, date2)
{
  year1 <- as.numeric(format(date1,format="%Y"))
  month1 <-as.numeric(format(date1,format="%m"))
  day1 <- as.numeric(format(date1,format="%d"))
  year2 <- as.numeric(format(date2,format="%Y"))
  month2 <- as.numeric(format(date2,format="%m"))
  day2 <- as.numeric(format(date2,format="%d"))

  years<-year2-year1

  #browser()
  if(month2<month1 || (month2==month1 && day2<day1)) {
    years<-years-1
  }

  years
}

#' Current age in integral years between two Dates
#'
#'  Returns the current age (integer) based on a date of birth. The fraction is truncated.
#'
#' @param dob date of birth
#
#' @return integer - age in years
#' @export
#'
#' @examples
#'
#' dob<-as.Date("2000-01-01")
#' current_age(dob)
#
current_age<-function(dob) {
  years_between(dob,Sys.Date())
}

#' Add date information
#'
#' Add columns to a data frame with information extracted from the indicated date column
#'
#' @param df data frame that has the date column
#' @param dt_col name of the column with the date of interest
#' @param cols columns to add, currently "year","yr","month","week","day", "weekday","month_name","month_abb","weekday_name","weekday_abb"
#'
#' @return same data frame with added columns
#' @export
#'
#' @examples
#'
#' df<-data.frame(date=sample(seq(as.Date('2010/01/01'), as.Date('2019/12/31'), by="day"), 30))
#' add_date_cols(df,"date")
#'
add_date_cols<-function(df,dt_col,cols) {

  cols_out<-c("year","yr","month","week","day", "weekday","month_name","month_abb","weekday_name","weekday_abb")
  fmt<-c("%Y","%y","%m","%W","%e","%w","%B","%b","%A","%a")
  as_int<-c(T,T,T,T,F,T,F,F,F,F)

  if(missing(cols)) {
    cols<-cols_out
    nms<-colnames(cols)
  } else {
    if(is.null(names(cols))) {
      nms<-cols
    } else {

      nms<-names(cols)
      fix<-nms==""
      nms[fix]<-cols[fix]
    }
  }

  icols<-which(cols_out%in%cols)
  sapply(icols,function(index){

    col_nm<-nms[which(cols_out[index]==cols)]
    if(as_int[index]) {
      df[,col_nm]<<-as.integer(format(df[,dt_col],fmt[index]))
    } else {
      df[,col_nm]<<-format(df[,dt_col],fmt[index])
    }

  })

  df
}
