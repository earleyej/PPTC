source(paste0(function.dir,"rawDataFormatting.R"))
source(paste0(function.dir,'km.R'))
source(paste0(function.dir,'generateFigure.R'))
source(paste0(function.dir,'generateTable.R'))


if(is.null(config)){stop('No configuration found. Please check...')}

##### Beginning of Pipeline #####

# format data
if (anyNA(x$treatment_end_date)) {
  warning("treatment_end_date is NA. Setting this to \"\"")
  x$treatment_end_date<-""
}
dat <- format.solidTumor.data(x)


# project end date ???????
tmp <- dat$formattedData[ !dat$formattedData$exclude, ]
#dat$project$maxday <- max(tmp$survival.time,na.rm=TRUE)
if (anyNA(tmp$survival.time)) {
  warning("there's an NA in survival.time. Is there a missing data point somewhere?")
}
dat$project$maxday <- max(tmp$survival.time,na.rm=TRUE)
                          
# fit Kaplan-Meier
dat$km <- km_fit(dat$formattedData[!dat$formattedData$exclude, ])


# AOC
#outfile = NULL
outmeds = NULL
meds = as.numeric(tapply(dat$formattedData$aoc,
                         dat$formattedData$group,median))
greater.than = as.logical(1-as.numeric(tapply(dat$formattedData$survival.event,
                                              dat$formattedData$group,
                                              min)))
outmeds = rbind(outmeds,
                data.frame(tumor_model=dat$project$tumor_model,
                           group=unique(dat$formattedData$group),
                           median.aoc=meds,
                           greater.than=greater.than))
dat$formattedData$tumor_model = dat$project$tumor_model


# weight plot
suppressWarnings({
  dat$weight.plot <- weight.plot(dat, color = config$weight.color,
                                 font.factor = config$font.factor,
                                 line.factor = config$line.factor,
                                 config = config)
})

# diameter plot
suppressWarnings({
  dat$diameter.plots <- diameter.plot(dat, color = config$diameter.color,
                                      font.factor = config$font.factor,
                                      line.factor = config$line.factor)
})

# survival plot
suppressWarnings({
  dat$survival.plot <- survival.plot(dat, color = config$km.color,
                                     censored = ! sum(dat$formattedData$survival.event) == nrow(dat$formattedData),
                                     font.factor = config$font.factor,
                                     line.factor = config$line.factor,
                                     config = config)
})

# volume plot
suppressWarnings({
  dat$volume.median <- calc.median(dat, "volume")
  dat$volume.plot <- volume.rtv.cd45.plot(dat, "volume", color = config$volume.color,
                                            font.factor = config$font.factor,
                                            line.factor = config$line.factor)
})

# rtv plot
suppressWarnings({
  dat$rtv.median <- calc.median(dat, "rtv")
  dat$rtv.plot <- volume.rtv.cd45.plot(dat,"rtv", color = config$rtv.color,
                                            font.factor = config$font.factor,
                                            line.factor = config$line.factor)
})

# perform exact log rank test
dat$exact_rank_tests <- list()
for (test in config$table.comparisons){
  dat$exact_rank_tests[[paste(test[1], test[2], sep=" vs ")]] <-
    exact_rank_test(dat$formattedData[!dat$formattedData$exclude, ], groupconf = test)
}

# perform Gehan-Wilcoxon test
dat$wilcoxon_tests <- list()
for (test in config$table.comparisons){
  dat$wilcoxon_tests[[paste(test[1], test[2], sep=" vs ")]] <-
    wilcoxon_test(dat$formattedData[!dat$formattedData$exclude, ], groupconf = test)
}

# format tables
suppressWarnings({
  dat$table <- format.ST.table(dat, config) # original summary table
  dat$table2 <- format.ST.table2(dat, config) #Feb 2021 - end of project revisions to summary table
})




# format redcap tables
suppressWarnings({
  dat$redcap.table <- format.redcap.ST.table(dat, config)
  dat$redcap.table$mice_level <- format.redcap.mice.level.ST.table(dat)
})


# set colors here for PNG figure creation.
# if different colors are needed, set within config.R
numgroups <- length(dat$groupInfo$group)
custom.colors <- COLORHEX[1:numgroups]
names(custom.colors) <- dat$groupInfo$group



# # add p-values to figures
# annot <- "p-values\n"
# for (test in config$table.comparisons){
#   if(test[3]){
#     annot <- paste0(annot, paste(test[1], test[2], sep=" vs "), " : ",
#                     dat$table.efs[paste(test[1], test[2], sep=" vs "), 4], "\n")
#   }
# }
# dat$survival.plot <- dat$survival.plot + 
#   annotate("text", x=config$km.annot.x, y=.35,
#            label=annot, size = 2.5 * config$font.factor)