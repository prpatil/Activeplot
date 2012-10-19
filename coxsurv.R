d3cat <- function(dat, c_sort, day_max=1000, line_col="steelblue"){

	paste("<script type='text/javascript'>
function survival(data, init_data, coef){

  var w     = 700;
  var h     = 400;
  var colors = ['", line_col, "'];  // Color of line

  // Scales
  var x  = d3.scale.linear().domain([0,", day_max, "]).range([0, w]);
  var y  = d3.scale.linear().domain([-0.2,1.1]).range([h, 0]);

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
    .data(data)
  .enter().append('path')
    .attr('class', 'line')
      .style('stroke', function(d,i){return colors[i];})
    .attr('d', line);
   
    vis.on('click',function(){  // THIS IS THE TRANSITION FCN...CURRENTLY RELIES ON RANDOM DATA
  vis.selectAll('path.line')
    .data([update_hazard(init_data, coef, rcov(coef.length))])
    .transition().duration(1800).delay(100).ease('elastic')
        .attr('width', 0)
    .attr('d',line);
   });
}

// Make sure covariates and coefficients are in order!
// Updates hazard function by multiplying each coefficient by its new value
// We need a function that updates the covariate array when a change is made to the form
function update_hazard(data, coef, covar){ 
  var tmpdata = JSON.parse(JSON.stringify(data));
  var xb = 0;
  for(var i=0; i < coef.length; i++){
    xb = xb + coef[i]*covar[i];
  }
  
  var prop = Math.exp(xb);
  
  for(var j=0; j < data.length; j++){
    tmpdata[j].haz = Math.exp(-data[j].haz*prop);
  }
  return tmpdata;
}

function rcov(len){
  var cov = [];
  for(var k=0; k < len; k++){
    cov[k] = (Math.random()*20 + 10);
  }
  return cov;
}

function init(){
  var data = ", dat ,";
  var coef = [", paste(c_sort, collapse=", ") ,"];
  var vals = [", paste(rep(0, length(c_sort)), collapse=", ") ,"];
  // Initialize baseline hazard function with 0 for all covar
  var out = update_hazard(data, coef, vals);
  survival([out], data, coef);
}

init();
</script>
", sep="")

}

css_cat <- function(line_size=2, axis_size=1){

	paste("\n\t\t\t<style type='text/css'>
		#main path {
		  stroke: #00b;
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

            .y.axis line, .y.axis path {
              fill: none;
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

	if(length(vars) > 10){
		stop("Number of covariates exceeds 10")
	}

	# Figure out what kind of menus are needed
	menu_type <- vector("character", length(vars))
	varlist <- list()

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
	c_sort <- cobj$coef[sort(names(cobj$coef))]

	outlist <- list("d3_script" = d3cat(djs, c_sort), "d3_css" = css_cat(),
	     "menu_type" = menu_type, "varlist"=varlist)

	plot.activePlot(writePage(outlist$d3_script, outlist$d3_css, varType=outlist$menu_type, varList=outlist$varlist, plotTitle=plotTitle))
}
