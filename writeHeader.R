writeHeader <- function(plotTitle="Individualized survival plot", varType=c("continuous","factor","continuous"),varList=list(age=c(10,99),sex=c("male","female"),weight=c(100,300))){
	
	header <- paste('<!DOCTYPE html>\n<html lang="en">\n\t<head>\n\t\t<meta charset="utf-8">"\n')
	header <- paste(header,'\n\t\t\t<title>',plotTitle,'</title>\n')
	

	# May need to change the location of these scripts (Jquery, Jquery Validation Plugin, Twitter Bootstrap)
	header <- paste(header,'\n\t\t\t<link href="http://biostat.jhsph.edu/~jleek/bootstrap/css/bootstrap.css" rel="stylesheet">
							\n\t\t\t<script type="text/javascript" src="http://biostat.jhsph.edu/~jleek/validate/jquery-1.4.4.js"></script>
							\n\t\t\t<script type="text/javascript" src="http://biostat.jhsph.edu/~jleek/validate/jquery.validate.js"></script>')


	# This is where we make the error message red
	header <- paste(header,'\n\n\t\t\t<style>
							\n\t\t\t\tlabel.error{
							\n\t\t\t\t\tfont-weight: bold;
							\n\t\t\t\t\tcolor: red;
  							\n\t\t\t\t\tpadding: 2px 8px;
  							\n\t\t\t\t\tmargin-top: 2px;
							\n\t\t\t\t}
	  						\n\t\t\t</style>')

	# This is the part where we add the form validation script

	header <- paste(header, writeValidate(varType,varList))

	#Close header
	header <- paste(header,'\n\t</head>')

	# In the future, this is where the D3 will go
	return(header)
}