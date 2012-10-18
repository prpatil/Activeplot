writeBody <- function(plotTitle="Individualized survival plot", varType=c("continuous","factor","continuous"),varList=list(age=c(10,99),sex=c("male","female"),weight=c(100,300))){
	
	htmlBody <- paste('\n\t<body>
					   	\n\t\t<div class="container">
					   	\n\t\t\t<div class="page-header">')

	# Add plot title
	htmlBody <- paste(htmlBody,'\n\t\t\t\t<h1>',plotTitle,'</h1>')

	# Add inHealth/D3 links
	htmlBody <- paste(htmlBody,'\n\t\t\t\t<p>created with the <a href="http://web.jhu.edu/administration/provost/initiatives/ihi/">inHealth</a> R package
								 and powered by <a href="http://d3js.org/">Data Driven Documents</a></p>')

	# We will eventually remove the picture of Roger and replace with the svg
	htmlBody <- paste(htmlBody,'\n\t\t\t</div>
								\n\t\t\t<div class="row">
								\n\t\t\t\t<div class="span6">
								\n\t\t\t\t\t <img src="http://biostat.jhsph.edu/~rpeng/peng7.png" height="400px" width="570px">
								\n\t\t\t\t</div>')

	htmlBody <- paste(htmlBody,'\n\t\t\t\t<div class="span3">')

	# Write the web-form

	htmlBody <- paste(htmlBody,writeForm(varType,varList))


	# Close out the divs/body/html
	htmlBody <- paste(htmlBody,'\n\t\t\t\t</div>')
	htmlBody <- paste(htmlBody,'\n\t\t\t</div>')
	htmlBody <- paste(htmlBody,'\n\t\t</div>')
	htmlBody <- paste(htmlBody,'\n\t</body>')
	htmlBody <- paste(htmlBody,'\n</html>')
}