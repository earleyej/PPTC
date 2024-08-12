#rm(list=ls())
parent.dir<-"C:/Users/eearley/Documents/PPTC/for_JAX/"
agent <- "1817"
function.dir<-paste0(parent.dir,"functions/")
study.dir<-paste0(parent.dir,"CNS/",agent,"/")
processed.dir<-paste0(parent.dir,"CNS/processed/")
resources.dir<-paste0(parent.dir,"resources/")
source(paste0(function.dir,"misc.R"))
source(paste0(function.dir,"rawDataFormatting.R"))
source(paste0(function.dir,"outputFigures.R"))
source(paste0(function.dir,"outputTables.R"))


files <- list.files(processed.dir)
files


#f<-files[1]
for (f in files) {

  x <- read.csv(paste0(processed.dir,f), stringsAsFactors = FALSE)
  numgroups <- length(unique(x$cns_group))
  numgroups
  
  # set working directory
  prefix <- gsub("-1-processed\\.csv","",f)
  prefix
  
  if (!file.exists(paste0(study.dir,prefix))) {
    dir.create(paste0(study.dir,prefix))
    print(paste0("created directory ",study.dir,prefix))
  }
  
	config = list()
	### set working directory ###
	config$wd                 <-      paste0(study.dir,prefix)
	config$tumor_panel                <- "CNS"
	config$DateFormat                 <- '%m/%d/%Y'
	config$WeightDateFormat           <- '%m/%d/%Y'
	config$group                      <- data.frame(Group = c("A", "B"),
	                        												nameInTemp = c("Control (vehicle)",
													  			                               "DS-7300a (PPTC 1817)"),
													                        newName = c("Control",
																                              "DS-7300a (PPTC 1817)"),
													                        stringsAsFactors = FALSE)
	#w$group <- config$group$Group[match(w$treatment_scheme, config$group$nameInTemp)]

	### graphing options ###
	config$weight.color               <- TRUE
	config$km.color                   <- TRUE
	config$font.factor                <- 1.5
	config$line.factor                <- 1
	config$km.annot.x                 <- 5

	### table options ###
	config$control                    <- "A"
	config$table.comparisons          <- list(c("A", "B", TRUE))
	config$table.footnote             <- ""

	### other configurations ###
	config$saveData                   <- TRUE

	## start analyses ##
	# set working directory
	setwd(config$wd)


	#### manual changes ####
	
	x$survival_24hr <- TRUE #they are using an older template, and this column is missing from their data
	x$survival_counted <- ! x$survival_counted == "no" #should be logical
	
	
	# load analyses
	source(paste0(function.dir,'pipeline.CNS.R'))

	# output table
	output.CNS.tables(dat=dat, wd = config$wd)


	# output figures
	output.cns.figures.portrait.2groups.1figperpage(dat = dat,
													agent_code = dat$project$agent_code,
													tumor_model = dat$project$tumor_model,
													wd = config$wd,
													weight = FALSE)


	# PNGs
	if (!file.exists("../PNG")) {
	  dir.create("../PNG")
	  print("created directory ../PNG")
	}

	custom.colors<-COLORHEX[1:length(unique(x$cns_group))]

	survival <- survival.plot.png.style(dat, color = config$km.color,
										censored = ! sum(dat$formattedData$survival.event) == nrow(dat$formattedData),
										font.factor = (config$font.factor * 0.5),
										line.factor = config$line.factor,
										custom.colors = custom.colors)
	png(filename = paste0("../PNG/",dat$project$panel_code,".","survival.png"), 
		width = 12, height = 9, units = "cm", res = 600)
	print(survival)
	dev.off()


	# weight <- weight.plot.png.style(dat,config$weight.color,
	#                                  font.factor = (config$font.factor  * 0.5),
	#                                  line.factor = config$line.factor,
	#                                  config = config)
	#  
	# png(filename = paste0("../PNG/",dat$project$panel_code,".","weight.png"), 
	#      width = 12, height = 9, units = "cm", res = 600)
	# print(weight)
	# dev.off()


	save.image("image.RData")
	setwd("../")
}


