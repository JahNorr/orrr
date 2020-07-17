
reinstall_old_pkgs<-function(dt_compare=Sys.Date()-2) {

  libnames<-list.files("C:/Users/john/Documents/R/win-library",full.names = T)
  libname<-libnames[length(libnames)]
  file.mtime(libname)

  filenames<-list.files(libname,full.names = T)

  invisible(
    sapply(filenames,function(filename){

      if(as.Date(file.mtime(filename))<dt_compare){
        pkgname<-gsub(".*[/](.*)","\\1",filename)
        cat("Installing ",pkgname,"\n")
        install.packages(pkgname)
        ""
      }
    })
  )

}
