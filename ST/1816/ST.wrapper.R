#rm(list=ls())
parent.dir<-"C:/Users/eearley/Documents/PPTC/for_JAX/"
agent <- "1816"
function.dir<-paste0(parent.dir,"functions/")
study.dir<-paste0(parent.dir,"ST/",agent,"/")
processed.dir<-paste0(parent.dir,"ST/processed/")
resources.dir<-paste0(parent.dir,"resources/")
source(paste0(function.dir,"misc.R"))
source(paste0(function.dir,"rawDataFormatting.R"))
source(paste0(function.dir,"outputFigures.R"))
source(paste0(function.dir,"outputTables.R"))


files <- list.files(processed.dir)

#f<-files[1]
for (f in files) {
  x <- read.csv(paste0(processed.dir,f), stringsAsFactors = FALSE)
  numgroups <- length(unique(x$group))
  numgroups
  
  # set working directory
  prefix <- gsub("-1-processed\\.csv","",f)
  prefix
  
  if (!file.exists(paste0(study.dir,prefix))) {
    dir.create(paste0(study.dir,prefix))
    print(paste0("created directory ",study.dir,prefix))
  }
  
  config = list()
  config$wd                 <-      paste0(study.dir,prefix)
  config$tumor_panel        <-      'solid'
  config$DateFormat         <-      '%m/%d/%Y'
  
  ### graphing options ###
  config$km.color                   <- TRUE
  config$km.annot.x                 <- 5
  config$weight.color               <- TRUE
  config$diameter.color             <- TRUE
  config$volume.color               <- TRUE
  config$rtv.color                  <- TRUE
  config$font.factor                <- 1
  config$line.factor                <- 1
  
  
  ### table options ###
  config$control                    <- "A"
    
  if (numgroups == 2) {
    config$table.comparisons			<- list(c("A","B",TRUE))
  }
  if (numgroups == 3) {
    config$table.comparisons			<- list(c("A","B",TRUE),
                                         c("A","C",TRUE),
                                         c("B","C",FALSE))
  }
  if (numgroups == 4) {
	config$table.comparisons			<- list(c("A","B",TRUE),
	                                      c("A","C",TRUE),
												                c("A","D",TRUE),
												                c("B","C",FALSE),
												                c("B","D",FALSE),
												                c("C","D",FALSE))
  }
  if (numgroups == 5) {
	config$table.comparisons			<- list(c("A","B",TRUE),
	                                      c("A","C",TRUE),
	                                      c("A","D",TRUE),
	                                      c("A","E",TRUE),
	                                      c("B","C",FALSE),
	                                      c("B","D",FALSE),
												                c("B","E",FALSE),
												                c("C","D",FALSE),
												                c("C","E",FALSE),
												                c("D","E",FALSE))
  }
 
  config$table.footnote             <- ""
  
  ### other configurations ###
  config$saveData                   <- TRUE
  
  setwd(config$wd)
  
  #### manual corrections ####

  
  ##### statistical analyses #####
  source(paste0(function.dir,"pipeline.ST.R"))
  
  
  
  output.solid.tables(dat=dat,
                      agent_code = dat$project$agent_code,
                      tumor_model = dat$project$tumor_model,
                      wd = config$wd)
  
  #### output figures ####
  fun <- paste0("output.solid.figures.portrait.",numgroups,"groups.1figperpage")
  get(fun)(
    dat = dat,
    agent_code = dat$project$agent_code,
    tumor_model = dat$project$tumor_model,
    wd = config$wd
  )
  
  
  
  
  #### PNGs ####
  if (!file.exists("../PNG")) {
    dir.create("../PNG")
    print("created directory ../PNG")
  }
  
  num_groups<-length(unique(x$group))
  custom.colors <- COLORHEX[1:num_groups]
  
  # KM
  survival <- survival.plot.png.style(dat, color = config$km.color,
                                      censored = ! sum(dat$formattedData$survival.event) == nrow(dat$formattedData),
                                      font.factor = (config$font.factor),
                                      line.factor = config$line.factor,
                                      custom.colors = custom.colors)
  png(filename = paste0("../PNG/",dat$project$panel_code,".","survival.png"), 
      width = 12, height = 9, units = "cm", res = 600)
  print(survival)
  dev.off()
  
  
  
  # RTV
  volume<-volume.rtv.cd45.plot.png.style(dat, column="rtv", color=config$volume.color,
                                         font.factor = (config$font.factor),
                                         line.factor = config$line.factor,
                                         custom.colors = custom.colors)
  png(filename = paste0("../PNG/",dat$project$panel_code,".","rtv.png"), 
      width = 12, height = 9, units = "cm", res = 600)
  print(volume)
  dev.off()
  
  
  #tumor volume vs. time
  volume<-volume.rtv.cd45.plot.png.style(dat, column="volume", color=config$volume.color,
                                         font.factor = (config$font.factor),
                                         line.factor = config$line.factor,
                                         custom.colors = custom.colors)
  png(filename = paste0("../PNG/",dat$project$panel_code,".","volume.png"), 
      width = 12, height = 9, units = "cm", res = 600)
  print(volume)
  dev.off()
  # weight ####
  weight <- weight.plot.png.style(dat,config$weight.color,
                                  font.factor = (config$font.factor),
                                  line.factor = config$line.factor,
                                  config = config,
                                  custom.colors = custom.colors)
  
  png(filename = paste0("../PNG/",dat$project$panel_code,".","weight.png"), 
      width = 12, height = 9, units = "cm", res = 600)
  print(weight)
  dev.off()
  
  
  
  
  save.image("image.RData")
  
  setwd("../")
  
  
}


