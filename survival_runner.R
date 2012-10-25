library(survival)
library(rjson)

source("writeBody.R")
source("writeHeader.R")
source("writeValidate.R")
source("writePage.R")
source("writeForm.R")
source("activeplotMethods.R")
source("coxsurv.R")

# The user will make a coxph object with their data
cobj <- coxph(Surv(time, status)~trt+age+celltype, data=veteran)

# Then they will run our 'plot-like' function
coxap(cobj, veteran, plotTitle="Veteran Data Survival Curve")
