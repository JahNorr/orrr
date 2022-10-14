

handle_funcs<-function(folder,file,funcs,sub=NULL,sorted=TRUE, new_fldr = TRUE) {

  if(sorted) funcs<-sort(funcs)

  #new_fldr<-fldr!=folder
  new_file<-TRUE #file!=file_old

  if(new_fldr) {
    cat("=================================================================================\n")
    cat(folder,"\n")
  }

  if(new_file) {
    if(!new_fldr) cat("---------------------------------------------------------\n")
    cat("\t",file,"\n")
  }


  if(!is.null(sub))   {
    cat(paste0("\t\t",sub,"\n"))
  }

  cat(paste0("\t\t\t",funcs,"\n"))

  file_old<<-file
  #fldr<<-folder
}

##    end of handle_funcs function
###############################################################

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
list.functions<-function(files,saveto=NULL,verbose=TRUE, sorted=TRUE) {

  if(!is.null(saveto)) {
    sink(saveto)
    verbose<-TRUE
  }

  if(missing(files)){
    folders<-c("./code","./R")
    files<-dir(folders,recursive = T,full.names = T,pattern = ".*[.]R")
  } else {
    folders<-files[dir.exists(files)]
    files<-files[!dir.exists(files)]
    files<-c(files,dir(folders,recursive = T,full.names = T,pattern = ".*[.]R"))
  }

  folders<-gsub("(.*/).*","\\1",files)

  df<-data.frame(stringsAsFactors = F)
  fldr<-""
  file_old<-""

  mapply(function(folder,file){
    folder<-gsub("\\","/",normalizePath(folder),fixed=T)
    lines<-readLines(con = file)

    isR6<-any(grepl("[<]- {0,}R6Class[(]",lines))

    pvtR6<-0
    pubR6<-0
    actR6<-0

    if(isR6) {
      pvtR6<-grep("private {0,}= {0,}list[(]",lines)
      pubR6<-grep("public {0,}= {0,}list[(]",lines)
      actR6<-grep("active {0,}= {0,}list[(]",lines)

      if (length(pvtR6)==0) pvtR6<-0 else lines[pvtR6] <- "................\n  private\n............"
      if (length(pubR6)==0) pubR6<-0
      if (length(actR6)==0) actR6<-0
    }

    file<-gsub("(.*/)(.*[.R])","\\2",file)

    func_lines<-grep("([<]-|=) {0,}function[(]",lines)
    #   func_lines<-c(func_lines,pvtR6,pubR6,actR6)
    func_lines<-func_lines[func_lines>0]
    func_lines<-sort(func_lines)

    funcs<-lines[func_lines]
    rm<-c(grep("#.*function",funcs),
          grep("[ms]apply",funcs))

    if(length(rm)>0) {
      funcs<-funcs[-rm]
      func_lines<-func_lines[-rm]
    }

    funcs<-gsub( " {0,}<- {0,}function","",funcs)

    # funcs<-funcs[!grepl("[ms]apply",funcs)]
    # funcs<-funcs[!grepl("#.*function",funcs)]

    funcs<-stringr::str_trim(gsub( " {0,}[{]","",funcs))

    if(isR6) {
      begin<-c(pvtR6,pubR6,actR6)
      type<-c("private","public","active")

      kp<-begin>0

      begin<-begin[kp]
      type<-type[kp]

      ord<-order(begin)
      begin<-begin[ord]
      type<-type[ord]
      ntypes<-length(type)

      if(ntypes>1) end<-begin[2:length(begin)] else end<-integer(0)
      end<-c(end,length(lines))

      mapply(function(f0,f1,typ) {
        fls<-which(func_lines>f0 & func_lines<f1)
        funs<-funcs[fls]
        df<<-rbind(df,data.frame(folder=folder,file=typ,func=funs))
        handle_funcs(folder = folder,file = file,funcs = funs,sub=typ, sorted = sorted)
      },begin,end,type)


    } else {
      if(length(funcs)>0) {
        df<<-rbind(df,data.frame(folder=folder,file=file,func=funcs))
        #browser()
        handle_funcs(folder = folder,file = file,funcs = funcs,sorted = sorted, new_fldr = fldr != folder)
        fldr <<- folder
      }
    }


  },folders,files)

  if(!is.null(saveto)) sink(NULL)

  invisible(df)
}
