writeValidate <- function(varType,varList){
	validate <- paste('\n\n\t\t\t<script type="text/javascript">
							\n\t\t\t\t$(document).ready(function(){
							\n\t\t\t\t\t$("#covariate-form").validate({
	  						\n\t\t\t\t\t\trules:{')


	# Write range validation for continous variables 
	nContinuous <- sum(varType=="continuous")
	whichContinuous <- which(varType=="continuous")
	if(nContinuous > 0){
		cVarList <- varList[whichContinuous]

		for(i in 1:nContinuous){
			validate <- paste(validate,'\n\t\t\t\t\t\t\t',names(cVarList)[i],':{')
			validate <- paste(validate,'\n\t\t\t\t\t\t\t\trequired:true,')
			validate <- paste(validate,'\n\t\t\t\t\t\t\t\trange: [',cVarList[[i]][1],',',cVarList[[i]][2],']')
			validate <- paste(validate,'\n\t\t\t\t\t\t\t},')
		}
	}

	# Close the validation script
	validate <- paste(validate,'\n\t\t\t\t\t\t}')
	validate <- paste(validate,"\n\t\t\t\t\t});")
	validate <- paste(validate,"\n\t\t\t\t});")
	validate <- paste(validate,'\n\n\t\t\t</script>')
	return(validate)
}
