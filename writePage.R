writePage <- function(d3_script, d3_css, plotTitle="Individualized survival plot", varType=c("continuous","factor","continuous"),varList=list(age=c(10,99),sex=c("male","female"),weight=c(100,300))){
	plotPage <- writeHeader(d3_css, plotTitle,varType,varList)
	plotPage <- paste(plotPage,writeBody(d3_script,plotTitle,varType,varList))
	return(plotPage)
}
