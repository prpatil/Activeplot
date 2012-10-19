writePage <- function(plotTitle="Individualized survival plot", varType=c("continuous","factor","continuous"),varList=list(age=c(10,99),sex=c("male","female"),weight=c(100,300))){
	plotPage <- writeHeader(plotTitle,varType,varList)
	plotPage <- paste(plotPage,writeBody(plotTitle,varType,varList))
	return(plotPage)
}
