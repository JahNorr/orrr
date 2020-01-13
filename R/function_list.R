



#' List of Functions
#'
#' Gets a list of the functions in a file, a folder, a vector of files or folders, or all
#' files in ./code or ./R (if files parameter is missing)
#'
#' @param files character - files or folders or a mix; if blank all files and folders in ./code and ./R
#' @param saveto character - name of file to save (sink) the text output to
#' @param verbose boolean - if verbose is true, print the structure ... folder/file/function
#'
#' @return invisible(data.frame) -  with columns for folder, file, and function
#' @export
#'
list.functions<-function(files,saveto=NULL,verbose=T) {
  if(!is.null(saveto)) {
    sink(saveto)
    verbose<-TRUE
  }
  if(missing(files)){
    folders<-c("./code","./R")
    files<-dir(folders,recursive = T,full.names = T,pattern = "*[.]R")
  } else {
    folders<-files[dir.exists(files)]
    files<-files[!dir.exists(files)]
    files<-c(files,dir(folders,recursive = T,full.names = T,pattern = "*[.]R"))
  }

  folders<-gsub("(.*/).*","\\1",files)

  df<-data.frame(stringsAsFactors = F)
  fldr<-""
  mapply(function(folder,file){
    folder<-gsub("\\","/",normalizePath(folder),fixed=T)
    lines<-readLines(con = file)

    file<-gsub("(.*/)(.*[.R])","\\2",file)

    func_lines<-grep("[<]- {0,}function[(]",lines)

    funcs<-lines[func_lines]

    funcs<-funcs[!grepl("#.*function",funcs)]
    funcs<-gsub( " {0,}<- {0,}function","",funcs)

    funcs<-funcs[!grepl("[ms]apply",funcs)]

    funcs<-stringr::str_trim(gsub( " {0,}[{]","",funcs))
    funcs<-sort(funcs)

    if(length(funcs)>0) {
      df<<-rbind(df,data.frame(folder=folder,file=file,func=funcs))
      new_fldr<-fldr!=folder
      if(new_fldr) {
        if(verbose) {
          cat("=================================================================================\n")
          cat(folder,"\n")
        }
        fldr<<-folder
      }
      if(verbose) {
        if(!new_fldr) cat("---------------------------------------------------------\n")

        cat("\t",file,"\n")
        cat(paste0("\t\t",funcs,"\n"))

      }
    }
  },folders,files)

  if(!is.null(saveto)) sink(NULL)

  invisible(df)
}
