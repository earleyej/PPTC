source(paste0(function.dir,"rawDataFormatting.R"))
source(paste0(function.dir,'km.R'))
source(paste0(function.dir,'generateFigure.R'))
source(paste0(function.dir,'generateTable.R'))



if(is.null(config)){stop('No configuration found. Please check...')}

##### Beginning of Pipeline #####

# format data
dat <- format.cns.data(x)

if(exists("w")){
  dat <- format.cns.weight(dat, w)
}

# project end date ???????
tmp <- dat$formattedData[ !dat$formattedData$exclude, ]
dat$project$maxday <- max(tmp$survival.time,na.rm=T)

# fit Kaplan-Meier
dat$km <- km_fit(dat$formattedData[!dat$formattedData$exclude, ])

# AOC
#outfile = NULL
#outmeds = NULL
#meds = as.numeric(tapply(dat$formattedData$aoc,
#                         dat$formattedData$group,median))
#greater.than = as.logical(1-as.numeric(tapply(dat$formattedData$survival.event,
#                                              dat$formattedData$group,
#                                              min)))
#outmeds = rbind(outmeds,
#                data.frame(tumor_model=dat$project$tumor_model,
#                           group=unique(dat$formattedData$group),
#                           median.aoc=meds,
#                           greater.than=greater.than))
#dat$formattedData$tumor_model = dat$project$tumor_model


# weight plot not implemented yet
suppressWarnings({
  dat$weight.plot <- weight.plot(dat, color = config$weight.color,
                                 font.factor = config$font.factor,
                                 line.factor = config$line.factor,
                                 config = config)
})

# survival plot
suppressWarnings({
  dat$survival.plot <- survival.plot(dat, color = config$km.color,
                                     censored = sum(dat$formattedData[!dat$formattedData$exclude, ]$survival.event == 0) > 0,
                                     font.factor = config$font.factor,
                                     line.factor = config$line.factor,
                                     config = config)
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
if(exists("w")){
  dat$table <- format.CNS.table(dat, config)
} else {
  dat$table <- format.CNS.table(dat, config, toxicity_eval = FALSE)
  dat$table2 <- format.CNS.table2(dat, config)
}

# dat$table.count <- format.counts(dat, config)
# dat$table.efs <- format.efs(dat, config)




 
 
# # add p-values to figures
# annot <- "p-values\n"
# for (test in config$table.comparisons){
#   if(test[3]){
#     annot <- paste0(annot, paste(test[1], test[2], sep=" vs "), " : ",
#                    dat$table.efs[paste(test[1], test[2], sep=" vs "), 4], "\n")
#   }
# }
# dat$survival.plot <- dat$survival.plot + 
#   annotate("text", x=config$km.annot.x, y=.35,
#            label=annot, size = 2.5 * config$font.factor)