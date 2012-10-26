d3cat <- function(dat, c_sort, cnames, vars, menu_type, day_max=1000, line_col="steelblue"){

paste("<script type='text/javascript'>

	function init_coefs(coef_nums, coef_names){
		var coefs = new Array();
		for(var i=0; i < coef_names.length; i++){
		coefs[coef_names[i]] = coef_nums[i];
	}

	return coefs;
	}

	// Updates hazard function by multiplying each coefficient by its new value
	// We need a function that updates the covariate array when a change is made to the form
	function update_hazard(data, coef, covar){
		var tmpdata = JSON.parse(JSON.stringify(data));
		var xb = 0;

		for(key in coef){
			xb = xb + coef[key]*covar[key];
		}
		var prop = Math.exp(xb);
		for(var j=0; j < data.length; j++){
			tmpdata[j].haz = Math.exp(-data[j].haz*prop);
		}
		return tmpdata;
	}

	var init_data = ", dat ,";
	var coef_nums = [", paste(c_sort, collapse=", ") ,"];
	var coef_names = [", paste("'", paste(cnames, collapse="', '"), "'", sep="") ,"];
	var init_vals = [", paste(rep(0, length(c_sort)), collapse=", ") ,"];
	var vlist = [", paste("'", paste(vars, collapse="', '"), "'", sep="") ,"];
	var mtype = [", paste("'", paste(menu_type, collapse="', '"), "'", sep="") ,"];

	// Initialize associative array of coefficients and coef names
	var coef = init_coefs(coef_nums, coef_names);
	// Initialize patient values to 0 to get baseline hazard
	var covar = init_coefs(init_vals, coef_names);
	// Initialize associative array of variable names
	var vtype = init_coefs(mtype, vlist);
	// Initialize baseline hazard function
	var data = update_hazard(init_data, coef, covar);


	var w = 700;
	var h = 400;
	var colors = ['", line_col, "']; // Color of line

	// Scales
	var x = d3.scale.linear().domain([0,", day_max, "]).range([0, w]);
	var y = d3.scale.linear().domain([-0.2,1.1]).range([h, 0]);

	// Base vis layer
	var vis = d3.select('#main')
			.append('svg:svg')
				.attr('width', w)
				.attr('height', h)
			.append('svg:g')
				.attr('transform', 'translate(' + 40 + ',' + 10 + ')');

	// create xAxis
	var xAxis = d3.svg.axis().scale(x).tickSize(-h).tickSubdivide(true);
	// Add the x-axis.
	vis.append('svg:g')
		.attr('class', 'x axis')
		.attr('transform', 'translate(0,' + h-10 + ')')
		.call(xAxis);
	// create left yAxis
	var yAxisLeft = d3.svg.axis().scale(y).ticks(4).orient('left');
	// Add the y-axis to the left
	vis.append('svg:g')
		.attr('class', 'y axis')
		.attr('transform', 'translate(-10,0)')
		.call(yAxisLeft);

	// Line drawer
	var line = d3.svg.line()
			.x(function(d){return x(d.time);})
			.y(function(d){return y(d.haz);})
			.interpolate('step-after');

	// Add path layer

	vis.selectAll('.line')
	.data([data])
	.enter().append('path')
	.attr('class', 'line')
	.style('stroke', function(d,i){return colors[i];})
	.attr('d', line);

	vis.selectAll('circle')
          .data(data)
          .enter()
          .append('svg:circle')
          .attr('cx', function(d) { return x(d.time); })
          .attr('cy', function(d) { return y(d.haz); })
          .attr('r', 3)
          .attr('opacity', 0)
	    .append('svg:title')
	    .text(function(d){return 'Day: '+d.time+'\\nSurvival: '+Math.round(d.haz*1000)/1000;});

	function update_covar(newcov){
		covar = init_coefs(init_vals, coef_names);
		for(var j=0; j < newcov.length; j++){
			if(vtype[newcov[j].name] == 'continuous'){
				covar[newcov[j].name] = newcov[j].value;
			} else {
				covar[(newcov[j].name+newcov[j].value)]=1;
			}
		}
	}

	function fig_update(newcov){
		update_covar(newcov);
		vis.selectAll('path.line')
			.data([update_hazard(init_data, coef, covar)])
			.transition().duration(1800).delay(100).ease('elastic')
			.attr('width', 0)
			.attr('d',line);
	}

	</script>
	", sep="")

}

css_cat <- function(line_size=2, axis_size=1){

	paste("\n\t\t\t<style type='text/css'>
		#main path {
			stroke: #000;
			stroke-width: ", line_size ,"px;
			fill: none;
		}
		.axis {
			shape-rendering: crispEdges;
		}

		.x.axis line {
			stroke: lightgrey;
		}

		.x.axis .minor {
			stroke-opacity: .5;
		}

		.x.axis path {
			display: none;
		}

		.y.axis line {
			stroke: lightgrey;
		}

		.y.axis path {
			//fill: none;
			stroke: #000;
			stroke-width: ", axis_size ,"px;
		}
	</style>
	", sep="")

}

# Create a baseline record to get baseline hazard function
baseline <- function(ds){
	for(i in 1:length(ds)){
		if(class(ds[1,i]) == "factor"){
			ds[1,i] <- levels(ds[1,i])[1]
		} else {
			ds[1,i] <- 0
		}
	}
	ds
}

# Pass this a coxph object
coxap <- function(cobj, data, plotTitle=""){

	if(class(cobj) != "coxph"){
		stop("Object not of class 'coxph'")
	}

	tmpdata <- data # In case we have to modify

	formula <- cobj$formula

	vars <- all.vars(formula[[3]]) # This is a list of all variables on RHS

	day_max <- max(data[[all.vars(formula[[2]])[1]]]) # This might need to be updated

	if(length(vars) > 10){
		stop("Number of covariates exceeds 10")
	}

	# Figure out what kind of menus are needed
	menu_type <- vector("character", length(vars))
	varlist <- list()
	refcats <- c()

	for(i in 1:length(vars)){
		cur <- tmpdata[[vars[i]]]
		if(class(cur) == "numeric" || class(cur) == "integer"){
			if(length(table(cur)) < 5){
				warning("Variable ", vars[[i]], " has fewer than 5 unique values; treating as continuous, but should this be a factor?")
			}
			menu_type[i] <- "continuous"
			varlist[[vars[i]]] <- range(cur)
		} else if(class(cur) == "factor"){
			if(length(levels(cur)) > 10){
				stop("Variable ", vars[[i]], " has too many levels (>10).")
			}
		menu_type[i] <- "factor"
		varlist[[vars[i]]] <- levels(cur)
		refcats <- c(refcats, paste(vars[i], levels(cur)[1], sep=""))
		}
	}

	ds <- tmpdata[1,] # Take the first row of data
	ds <- baseline(ds)
	so <- survfit(cobj, newdata=ds)

	dt <- coxph.detail(cobj)
	data <- cbind(so$time, -log(so$surv))
	colnames(data) <- c("time", "haz")
	data <- apply(data, 1, as.list)
	djs <- toJSON(data)

	c_sort <- c(cobj$coef, sapply(refcats, function(x){assign(x, 0)}))

	c_sort <- c_sort[sort(names(c_sort))]

	outlist <- list("d3_script" = d3cat(djs, c_sort, names(c_sort), vars, menu_type, day_max=day_max), "d3_css" = css_cat(),
				"menu_type" = menu_type, "varlist"=varlist)

	plot.activePlot(writePage(outlist$d3_script, outlist$d3_css, varType=outlist$menu_type, varList=outlist$varlist, plotTitle=plotTitle))
}

