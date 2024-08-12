#rm(list=ls())
parent.dir<-"C:/Users/eearley/Documents/PPTC/for_JAX/"
agent <- "1806"
function.dir<-paste0(parent.dir,"functions/")
study.dir<-paste0(parent.dir,"ALL/",agent,"/")
processed.dir<-paste0(parent.dir,"ALL/processed/")
resources.dir<-paste0(parent.dir,"resources/")
source(paste0(function.dir,"misc.R"))
source(paste0(function.dir,"rawDataFormatting.R"))
source(paste0(function.dir,"outputFigures.R"))
source(paste0(function.dir,"outputTables.R"))




files <- list.files(processed.dir)
files <- files[!grepl("weight",files)]


#f <- files[1]
for (f in files) {
  print(f)
  x <- read.csv(paste0(processed.dir,f), stringsAsFactors = FALSE)
  f.weight <- paste0("weight-",f)

  
  w <- read.csv(paste0(processed.dir,f.weight))
  unique(x$arm)
  
  prefix <- gsub("-processed\\.csv","",f)
  if (!file.exists(paste0(study.dir,prefix))) {
    dir.create(prefix)
    print(paste0("created directory ",study.dir,prefix))
  }
  
  
  config = list()
  ### set working directory ###
  config$wd                 <-      paste0(study.dir,prefix)
  ### set REDCap report ID ###
  config$DateFormat               <-      '%m/%d/%Y'
  
  # set study and tumor 
  config$tumor_panel              <-      'ALL'
  
  ### graphing options ###
  config$weight.color               <- TRUE
  config$diameter.color             <- TRUE
  config$volume.color               <- TRUE
  config$rtv.color                  <- TRUE
  
  config$km.color                   <- TRUE
  config$font.factor                <- 1
  config$line.factor                <- 1.5
  config$km.annot.x                 <- 5
  config$cd45.color                 <- TRUE
  
  ### table options ###
  config$control                    <- "A"
  
  numgroups <- length(unique(x$arm))
  if (numgroups == 2) {
	config$table.comparisons		<- list(c("A","B",TRUE))
  } else if (numgroups == 3) {
	config$table.comparisons		<- list(c("A","B",TRUE),
											c("A","C",TRUE),
											c("B","C",FALSE))
  
  
  } else if ( numgroups == 4) {
  config$table.comparisons          <- list(c("A", "B", TRUE), # "group 1", "group 2", "group 1 is control"
                                            c("A", "C", TRUE),
                                            c("A", "D", TRUE),
                                            c("B", "C", FALSE),
                                            c("B", "D", FALSE),
                                            c("C", "D", FALSE))  
  }
  config$table.footnote             <- ""
  
  ### other configurations ###
  config$saveData                   <- TRUE
  
  ##### start analyses ##
  # set working directory
  setwd(config$wd)
  
  
  #### manual edits ####
  # x$date_of_treatment_completion <- as.Date(x$date_of_first_treatment, config$DateFormat) + 11


  ##### statistical analyses #####
  source(paste0(function.dir,"pipeline.ALL.R"))
  

  # output table
  
  output.ALL.tables(dat = dat, wd = config$wd)
  

  # output pdf figures
 
  output.all.figures.portrait.1figperpage(dat = dat,
                                            agent_code = dat$project$agent_code,
                                            tumor_model = dat$project$tumor_model,
                                            wd = config$wd)
  
  
  #### output PNG files ####
  if (!file.exists("../PNG")) {
    dir.create("../PNG")
    print("created directory ../PNG")
  }
  
  
    if ( length(unique(x$arm)) == 2) {
      COLORHEX[1:2] = c("black","red")
      survival <- survival.plot.png.style(dat, color = config$km.color,
                                          censored = ! sum(dat$formattedData$survival.event) == nrow(dat$formattedData),
                                          font.factor = config$font.factor,
                                          line.factor = config$line.factor,
                                          custom.colors = COLORHEX[1:2])
      survival <- survival + theme(axis.line= element_line(size=1,color="black"),
                                   axis.ticks= element_line(size=1,color="black"),
                                   axis.text.x = element_text(colour = "black",face="bold"),axis.text.y = element_text(colour = "black",face="bold"),
                                   axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold")) +
        ylab("EFS Probability (%)") +
        xlab("Days Post Treatment Initiation")
      png(filename = paste0("../PNG/",dat$project$panel_code,".survival.png"),
          width = 12, height = 9, units = "cm", res = 600)
      print(survival)
      dev.off()
     
    
     hcd45 <- volume.rtv.cd45.plot.png.style(dat, "cd45", color = config$volume.color,
                                             font.factor = config$font.factor,
                                             line.factor = config$line.factor,
                                             custom.colors = COLORHEX[1:2])
     hcd45 <- hcd45 + theme(axis.line= element_line(size=1,color="black"),
                            axis.ticks= element_line(size=1,color="black"),
                            axis.text.x = element_text(colour = "black",face="bold"),
                            axis.text.y = element_text(colour = "black",face="bold"),
                            axis.title.x = element_text(face="bold"),
                            axis.title.y = element_text(face="bold")) +
       ylab("% Human CD45+")
     png(filename = paste0("../PNG/",dat$project$panel_code,".hcd45.png"),
         width = 12, height = 9, units = "cm", res = 600)
     print(hcd45)
     dev.off()
     
     
     
     weight <- weight.plot.png.style(dat,config$weight.color,
                                     font.factor = (config$font.factor),
                                     line.factor = config$line.factor,
                                     config = config,
                                     custom.colors = COLORHEX[1:2])
     weight <- weight + theme(axis.line= element_line(size=1,color="black"),
                              axis.ticks= element_line(size=1,color="black"),
                              axis.text.x = element_text(colour = "black",face="bold"),
                              axis.text.y = element_text(colour = "black",face="bold"),
                              axis.title.x = element_text(face="bold"),
                              axis.title.y = element_text(face="bold")) +
       ylab("Average Weight (g)")
     
     png(filename = paste0("../PNG/",dat$project$panel_code,".","weight.png"), 
         width = 12, height = 9, units = "cm", res = 600)
     print(weight)
     dev.off()
     
   } else {
     COLORHEX[1:4] = c("black","red","blue","green4")
     survival <- survival.plot.png.style(dat, color = config$km.color,
                                         censored = ! sum(dat$formattedData$survival.event) == nrow(dat$formattedData),
                                         font.factor = config$font.factor,
                                         line.factor = config$line.factor,
                                         custom.colors = COLORHEX[1:length(unique(x$arm))])
     survival <- survival + theme(axis.line= element_line(size=1,color="black"),
                                  axis.ticks= element_line(size=1,color="black"),
                                  axis.text.x = element_text(colour = "black",face="bold"),axis.text.y = element_text(colour = "black",face="bold"),
                                  axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold")) +
       ylab("EFS Probability (%)") +
       xlab("Days Post Treatment Initiation")
     png(filename = paste0("../PNG/",dat$project$panel_code,".survival.png"),
         width = 12, height = 9, units = "cm", res = 600)
     print(survival)
     dev.off()
     
     hcd45 <- volume.rtv.cd45.plot.png.style(dat, "cd45", color = config$volume.color,
                                             font.factor = config$font.factor,
                                             line.factor = config$line.factor,
                                             custom.colors = COLORHEX[1:length(unique(x$arm))])
     hcd45 <- hcd45 + theme(axis.line= element_line(size=1,color="black"),
                            axis.ticks= element_line(size=1,color="black"),
                            axis.text.x = element_text(colour = "black",face="bold"),
                            axis.text.y = element_text(colour = "black",face="bold"),
                            axis.title.x = element_text(face="bold"),
                            axis.title.y = element_text(face="bold")) +
       ylab("% Human CD45+")
     png(filename = paste0("../PNG/",dat$project$panel_code,".hcd45.png"),
         width = 12, height = 9, units = "cm", res = 600)
     print(hcd45)
     dev.off()
    
    
    
     weight <- weight.plot.png.style(dat,config$weight.color,
                                     font.factor = (config$font.factor),
                                     line.factor = config$line.factor,
                                     config = config,
                                     custom.colors = COLORHEX[1:length(unique(x$arm))])
     weight <- weight + theme(axis.line= element_line(size=1,color="black"),
                              axis.ticks= element_line(size=1,color="black"),
                              axis.text.x = element_text(colour = "black",face="bold"),
                              axis.text.y = element_text(colour = "black",face="bold"),
                              axis.title.x = element_text(face="bold"),
                              axis.title.y = element_text(face="bold")) +
       ylab("Average Weight (g)")
    
     png(filename = paste0("../PNG/",dat$project$panel_code,".","weight.png"), 
         width = 12, height = 9, units = "cm", res = 600)
     print(weight)
     dev.off()
    
   }
 
  save.image("image.RData")
  
  setwd("../")
  
  
}


