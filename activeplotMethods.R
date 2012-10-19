plot.activePlot <- function(pageHTML,fileName="survivalPlot.html",...){
  	if(!isServerRunning()) {
    	tools:::startDynamicHelp()
  	}

  	  root.dir <- tempdir()
  	  tempFile <- file.path(root.dir,paste(fileName))
  	  cat(pageHTML,file=tempFile)

  	  browseURL(tempFile)

}

isServerRunning <- function() {
  tools:::httpdPort > 0L
}
