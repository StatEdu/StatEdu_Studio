Sys.setenv(LC_ALL="English_United States.utf8",LANG="English_United States.utf8")
invisible(Sys.setlocale("LC_CTYPE","English_United States.utf8"))
`%||%` <- function(x,y) if(is.null(x)) y else x
source("R/result_export_files.R",encoding="UTF-8")
out <- "tmp/canvas-large-png"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
set.seed(916)
source_file <- file.path(out,"source.png")
png::writePNG(array(runif(640L*640L*3L),dim=c(640L,640L,3L)),source_file)
bytes <- readBin(source_file,"raw",file.info(source_file)$size)
encoded <- base64enc::base64encode(bytes)
stopifnot(nchar(encoded)>1000000L)
for(menu in c("mediation","cfa","sem","pls")) {
  for(style in c("plain","unpadded","escaped")) {
    value <- switch(style,plain=encoded,unpadded=sub("=+$","",encoded),
      escaped=paste0(substr(encoded,1,800000),"%0A",substring(encoded,800001,nchar(encoded))))
    saved <- save_canvas_figure_snapshots(list(list(name="model.png",data=paste0("data:image/png;base64,",value))),out,menu)
    stopifnot(identical(bytes,readBin(saved,"raw",file.info(saved)$size)))
  }
  message("PASS: ",menu," >1M-character PNG saved byte-for-byte, plain/unpadded/escaped")
}
before <- list.dirs(out,recursive=FALSE)
bad <- list(list(name="model.png",data=paste0("data:image/png;base64,",encoded,"!")))
stopifnot(inherits(try(save_canvas_figure_snapshots(bad,out),silent=TRUE),"try-error"),
  identical(before,list.dirs(out,recursive=FALSE)))
message("PASS: corrupt large PNG rejected before creating export folder")
