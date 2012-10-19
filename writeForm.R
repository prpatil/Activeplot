writeForm <- function(varType,varList){
	form <- paste('\n\t\t\t\t\t<form class="form-horizontal" action="#" id="covariate-form">
				   \n\t\t\t\t\t\t<fieldset>')

	# Do continuous variables first
	nContinuous <- sum(varType=="continuous")
	whichContinuous <- which(varType=="continuous")
	
	if(nContinuous > 0){
		cVarList <- varList[whichContinuous]	
		
		for(i in 1:nContinuous){
			form <- paste(form,'\n\t\t\t\t\t\t<div class="control-group">
						   \n\t\t\t\t\t\t\t<label class="control-label" for="',names(cVarList)[i],
						   '">',names(cVarList)[i],'</label>',sep="")
			form <- paste(form,'\n\t\t\t\t\t\t\t<div class="controls">')
			form <- paste(form,'\n\t\t\t\t\t\t\t\t<input type="text" id="',names(cVarList)[i],
						  '" name="',names(cVarList[i]),'" value="',cVarList[[i]][1],'">',sep="")
			form <- paste(form,'\n\t\t\t\t\t\t\t</div>')
			form <- paste(form,'\n\t\t\t\t\t\t</div>')
			form <- paste(form,"\n")
		}

	}

	# Then do the factor variables
	nFactor <- sum(varType=="factor")
	whichFactor <- which(varType=="factor")

	if(nFactor > 0){
		fVarList <- varList[whichFactor]

		for(i in 1:nFactor){
			form <- paste(form,'\n\t\t\t\t\t\t<div class="control-group">
						   \n\t\t\t\t\t\t\t<label class="control-label" for="',names(fVarList)[i],
						   '">',names(fVarList)[i],'</label>',sep="")
			form <- paste(form,'\n\t\t\t\t\t\t\t<div class="controls">')
			form <- paste(form,'\n\t\t\t\t\t\t\t\t<select id="',names(fVarList)[i],'" name="',names(fVarList)[i],'">',sep="")
			
			# Add the select box options
			for(j in 1:length(fVarList[[i]])){
				if(j==1){
					form <- paste(form,'\n\t\t\t\t\t\t\t\t\t<option value="',fVarList[[i]][j],'" selected="selected">',fVarList[[i]][j],'</option>',sep="")
				}else{
					form <- paste(form,'\n\t\t\t\t\t\t\t\t\t<option value="',fVarList[[i]][j],'">',fVarList[[i]][j],'</option>',sep="")
				}
			}

			form <- paste(form,'\n\t\t\t\t\t\t\t\t</select>')
			form <- paste(form,'\n\t\t\t\t\t\t\t</div>')
			form <- paste(form,'\n\t\t\t\t\t\t</div>')
			form <- paste(form,"\n")
		}
	}

	# Close the fieldset and the form
 	form <- paste(form,'\n\t\t\t\t\t\t</fieldset>')
 	form <- paste(form,'\n\t\t\t\t\t</form>')

 	return(form)

}
