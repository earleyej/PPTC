require(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
require(grid, quietly = TRUE, warn.conflicts = FALSE)
require(GGally, quietly = TRUE, warn.conflicts = FALSE)
source(paste0(function.dir,'ggsurv.R'))
source(paste0(function.dir,'km.R'))

COLORHEX = c('#a50026', '#313695', '#f46d43', '#74add1', '#fdae61', '#d73027', 
             '#4575b4', '#fee090', '#abd9e9', '#ffffbf', '#e0f3f8')
names(COLORHEX) <- LETTERS[1:length(COLORHEX)]

format.p = function(pval) {
  if (pval < 0.001) {return("p < 0.001")
  } else {
    return(paste0("p = ",format(round(pval,3),nsmall=3)))
  }
}

### PDF Plots #####
# EFS Survival plot.
# Input the entire experiment dat object
survival.plot <- function(dat, censored = FALSE, color = TRUE, 
                          font.factor = 1, line.factor = 1, 
                          config = NULL){
  tmp <- dat$formattedData[ !dat$formattedData$exclude, ]
  if(is.null(dat$km)){
    dat$km <- km_fit(dat$formattedData[ !dat$formattedData$exclude, ])
  }
  # Survival plot
  maxday <- dat$project$maxday
  # names(COLORHEX) <- dat$groupInfo$group
  numgroups <- length(dat$groupInfo$group)
  if(censored){
    if(color){
      # p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= COLORHEX[1:numgroups], 
      #             cens.col = 'black', back.white = TRUE,
      #             size.est = 1 * line.factor) 
      p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= COLORHEX[dat$groupInfo$group], 
                  cens.col = 'black', back.white = TRUE,
                  size.est = 1 * line.factor) 
    } else {
      p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= gray.colors(numgroups, start = 0, end = 0.6), 
                  cens.col = 'black', lty.est = c(1:numgroups), back.white = TRUE,
                  size.est = 1 * line.factor) 
    }
  } else {
    if(color){
      # p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= COLORHEX[1:numgroups],
      #             back.white = TRUE, size.est = 1 * line.factor) 
      p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= COLORHEX[dat$groupInfo$group],
                  back.white = TRUE, size.est = 1 * line.factor) 
    } else {
      p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= gray.colors(numgroups, start = 0, end = 0.6), 
                  lty.est = c(1:numgroups), back.white = TRUE,
                  size.est = 1 * line.factor) 
    }
  }
  
  p1 = p1 + xlab("Days Post Treatment Initiation") + ylab("EFS Probability (%)") +
    ggtitle("Time to Event Plot") + xlim(0, 1.1 * maxday) + ylim(0,1) + 
    theme(legend.title=element_blank(), 
          legend.key.width = unit(0.5 * font.factor, "cm"),
          plot.title = element_text(size=10 * font.factor),
          legend.text = element_text(size=10 * font.factor),
          axis.text = element_text(size=10 * font.factor),
          axis.title = element_text(size=10 * font.factor)) #+
    #scale_x_continuous(breaks = seq(0, maxday*1.1, 7))
	if (maxday >= 70 & maxday < 140) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 14))}
	if (maxday >= 140 & maxday < 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 21))}
	if (maxday >= 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 28))}

  if (!is.null(config) & config$tumor_panel == "CNS"){
    p1 <- p1 + xlab("Days Post Treatment Initiation")
  }
  
  return(p1)
}

# Weight by time plot.
# Input the entire experiment dat object
weight.plot <- function(dat, color = TRUE, font.factor = 1, 
                        line.factor = 1, config = config){
  
  if(is.null(dat$weight)){
    return(NULL)
  }
  
  #maxday <- dat$project$maxday # this will fail if there's a discrepancy btwn the weight file and the processed data
  # names(COLORHEX) <- dat$groupInfo$group
  numgroups <- length(dat$groupInfo$group)

  # mean weight and number of mice
  tmp <- aggregate(dat$weight$weight, by=list(dat$weight$group, dat$weight$days), mean, na.rm=TRUE)
  colnames(tmp) <- c("Group", "Days", "Weight")
  tmp2 <- aggregate(dat$weight$weight, by=list(dat$weight$group, dat$weight$days), length)
  colnames(tmp2) <- c("Group", "Days", "Measured_mice")
  tmp <- merge(tmp, tmp2, sort = FALSE)
  tmp$Group <- factor(tmp$Group, levels = LETTERS)
  
  maxday<-max(tmp$Days)
  minday <- min(tmp$Days)
  
  # plot weight by time
  p1 <- ggplot(tmp,aes(x=Days,y=Weight,group=Group))
  if(color){
    p1 <- p1 + geom_line(aes(color = Group), size = 1 * config$line.factor) + 
      scale_color_manual(name = "Group", values = COLORHEX) +
      geom_text(aes(label=Measured_mice),size=2 * config$font.factor)
  } else {
    p1 <- p1 + geom_line(aes(linetype=Group, group=Group), size = 0.6 * config$line.factor) + 
      geom_text(aes(label=Measured_mice),size=2 * config$font.factor) + theme_bw()
  }

  # Aesthetics
  
  p1 <- p1 + xlab("Days") + ylab("Average Weight (g)") +
    ggtitle("Average Mouse Weight by Time") + xlim(minday, 1.05 * maxday) +
    theme(legend.title=element_blank(), 
          legend.key.width = unit(0.5 * config$font.factor, "cm"),
          plot.title = element_text(size=10 * config$font.factor),
          legend.text = element_text(size=10 * config$font.factor),
          axis.text = element_text(size=10 * config$font.factor),
          axis.title = element_text(size=10 * config$font.factor)) #+
    #scale_x_continuous(breaks = seq(0, maxday * 1.1, 7)) + theme_bw()

  # Oct 22, 2018 -- EJE -- don't check maxday; instead check max(tmp$Days).
  # using maxday is an issue when the weight data goes up to 14 (or whatever) and the survival data goes much, much longer.
  # causes the x-axis ticks to be scaled off the margins of plot
  #if (maxday >= 70) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 14))}
	#if (maxday >= 140) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 21))}
	#if (maxday >= 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 28))}
  if (max(tmp$Days) >= 70 & max(tmp$Days) < 140) {p1 = p1 + scale_x_continuous(breaks = seq(0, max(tmp$Days)*1.1, 14))}
  if (max(tmp$Days) >= 140 & max(tmp$Days) < 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, max(tmp$Days)*1.1, 21))}
  if (max(tmp$Days) >= 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, max(tmp$Days)*1.1, 28))}
  
  if (!is.null(config) & (config$tumor_panel == "CNS" || config$tumor_panel == "ALL") ){
    p1 <- p1 + xlab("Days Post Treatment Initiation")
  }
  
  return(p1)
}

# Diameter_x vs diameter_y plot
# Input the entire experiment dat object
diameter.plot <- function(dat, color = TRUE, font.factor = 1, line.factor = 1){
  plots <- list()
  
  # names(COLORHEX) <- dat$groupInfo$group
  numgroups <- length(dat$groupInfo$group)
  
  tmp <- dat$rawData[,c('mouseID', 'group', 'diameter_x', 'diameter_y', 'day')]
  tmp$diameter_1 <- apply(tmp[,c('diameter_x', 'diameter_y')], 1, max)
  tmp$diameter_2 <- apply(tmp[,c('diameter_x', 'diameter_y')], 1, min)
  
  tmp$dot.size <- 1 * line.factor
  tmp$dot.size[ tmp$day == 0 ] <- 2 * line.factor
  for (mouse in unique(tmp$mouse)){
    tmp$dot.size[tmp$mouse == mouse & tmp$day == max(tmp$day[tmp$mouse == mouse])] <- 0.5 * line.factor
  }
  tmp$dot.size[tmp$diameter_x < 1 & tmp$diameter_y < 1] <- 0.5 * line.factor
  
  tmp$dot.shape <- 16
  tmp$dot.shape[tmp$day == 0] <- 21

  for (group in dat$groupInfo$group){
    tmp_trt <- tmp[tmp$group == group, ]
    if(color){
      p1 <- ggplot(tmp_trt,aes(x=diameter_1,y=diameter_2,group=mouseID)) + 
        geom_path(arrow = arrow(angle=20, length = unit(0.1 * line.factor, "inch"), type="open"), color=COLORHEX[group]) + 
        scale_shape_identity() + scale_size_identity() +
        geom_point(aes(size = dot.size, shape=dot.shape), color=COLORHEX[group]) +
        theme_bw() + geom_abline(slope=1, intercept=0, colour='black', size = 1 * line.factor)+
        annotate("text", x=(max(c(tmp$diameter_1, tmp$diameter_2)) + min(c(tmp$diameter_1, tmp$diameter_2))) * 0.5,
                 y=max(c(tmp$diameter_1, tmp$diameter_2)) * 0.93, size = 1.2 * font.factor, 
                 label = "Baseline measurements\nindicated by open circles")
    }else{
      p1 <- ggplot(tmp_trt,aes(x=diameter_1,y=diameter_2,group=mouseID)) + 
        geom_path(arrow = arrow(angle=20, length = unit(0.1 * line.factor, "inch"), type="open")) + 
        scale_shape_identity() + scale_size_identity() +
        geom_point(aes(size = dot.size, shape=dot.shape)) +
        theme_bw() + scale_fill_continuous(guide = guide_legend()) +
        geom_abline(slope=1, intercept=0, colour='gray', size = 1 * line.factor)+
        annotate("text", x=(max(c(tmp$diameter_1, tmp$diameter_2)) + min(c(tmp$diameter_1, tmp$diameter_2))) * 0.5,
                 y=max(c(tmp$diameter_1, tmp$diameter_2)) * 0.93, size = 1.2 * font.factor, 
                 label = paste("Baseline measurements", 
                               "indicated by open circles",
                               paste0("Group: ", group), sep="\n"))
    }
     p1 <- p1 + xlim(min(c(tmp$diameter_1, tmp$diameter_2)) * 0.9, max(c(tmp$diameter_1, tmp$diameter_2)) * 1.05) +
      ylim(min(c(tmp$diameter_1, tmp$diameter_2)) * 0.9, max(c(tmp$diameter_1, tmp$diameter_2)) * 1.05) + 
      coord_fixed(ratio = 1) + xlab("Length (mm)") + ylab("Width (mm)") +
      ggtitle(paste0("Tumor Size Progression (Group: ", group, ")")) + 
      theme(legend.title=element_blank(), 
            plot.title = element_text(size=8 * font.factor),
            legend.text = element_text(size=8 * font.factor),
            axis.text = element_text(size=6 * font.factor),
            axis.title = element_text(size=6 * font.factor))
     plots[[group]] <- p1
  }
  
  return(plots)
}

# Volume, RTV, %hCD45 by time plot
# Input the entire experiment dat object
calc.median <- function(dat, column){
  # subsetting medians till the last day
  ### revision 20170519 ###
  ### adding fake data for median calculation ###
  dummyData <- dat$rawData[,c("mouseID", "group", "day", column)]
  # exclude excluded mouse
  dummyData <- dummyData[ ! dummyData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
  
  # if fewer than half of mice have data at the given date, define the datapoint before it as last day
  # logic here is that if >50% of mice evented, then don't calculate medians based on that day
  mouse.n <- aggregate(! is.na(dummyData[[column]]), by=list(dummyData$group, dummyData$day), sum)
  group.n <- aggregate(dat$formattedData$mouseID[! dat$formattedData$exclude], 
                       by = list(dat$formattedData$group[! dat$formattedData$exclude]), length)
  mouse.n <- merge(mouse.n, group.n, by = "Group.1", sort = FALSE)
  mouse.n <- mouse.n[mouse.n$x.x > mouse.n$x.y/2,]
  colnames(mouse.n) <- c("group", "day", "time.n", "group.n")
  lastDay <- aggregate(as.numeric(mouse.n$day), by=list(mouse.n$group), max)
  colnames(lastDay) <- c("group", "day")
  
  
  #this is John's clever way to coerce data points on different days that are very close together.
  dummyData$timepoint <- round(dummyData$day/1.5)
  # dummyData$timepoint <- round(dummyData$day/3.5)
  dat.time.match <- unique(dummyData[,c("group", "day", "timepoint")])
  dummyData <- dcast(mouseID ~ timepoint, data = dummyData, value.var = column)
  evented <- dat$formattedData$mouseID[dat$formattedData$survival.event == 1]
  # make cd45 for evented mice be 100. This won't directly impact median calculation
  dummyData[dummyData$mouseID %in% evented,][is.na(dummyData[dummyData$mouseID %in% evented,])] <- 100
  dummyData <- melt(data = dummyData, variable.name = "timepoint", value.name = column)
  dummyData$group <- sapply(sapply(dummyData$mouseID, strsplit, ";"), function(x){x[[1]]})
  dummyData <- merge(dummyData, dat.time.match, sort = FALSE)
  dummyData$timepoint <- NULL
  medians <- aggregate(dummyData[[column]], by=list(dummyData$group, dummyData$day), median, na.rm=T)
  names(medians) <- c('group', 'day', column)
  
  medians$keep <- medians$day <= lastDay$day[match(medians$group, lastDay$group)]
  medians <- medians[medians$keep & !is.na(medians[[column]]), c('group', 'day', column)]
  return(medians)
}
volume.rtv.cd45.plot <- function(dat, column, color=TRUE, font.factor = 1, line.factor = 1,
                                 indiv.mouse = TRUE){
  
  maxday <- dat$project$maxday
  # names(COLORHEX) <- dat$groupInfo$group
  numgroups <- length(dat$groupInfo$group)
  
  plotData <- dat$rawData[,c("mouseID", "group", "day", column)]
  # exclude excluded mouse
  plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
  
  medians <- dat[[paste(column, "median", sep=".")]]
  # plot
  if(indiv.mouse){
    if(color){
      p1 = ggplot(plotData,aes_string(x="day",y=column,group="mouseID")) + 
        geom_line(aes(colour = group),alpha=0.2,size=0.2 * line.factor) + 
        geom_line(data = medians, aes(group=group, color = group), size=1 * line.factor) +
        scale_color_manual(values = COLORHEX)
    } else{
      p1 = ggplot(plotData,aes_string(x="day",y=column,group="mouseID")) + 
        geom_line(aes(colour=group, linetype=group),alpha=0.2,size=0.2 * line.factor) +
        scale_color_grey(start = 0.5, end = 0.7) + theme_bw() + 
        geom_line(data = medians, aes(group=group, linetype=group), color = "black", size=1 * line.factor)
    }
  }
  
  p1 <- p1 + theme(legend.title=element_blank(), 
                   plot.title = element_text(size=10 * font.factor),
                   legend.text = element_text(size=6 * font.factor),
                   axis.text = element_text(size=6 * font.factor),
                   axis.title = element_text(size=8 * font.factor)) + theme_bw()
  p1 <- p1 + xlab('Days')  #+ scale_x_continuous(breaks = seq(0, maxday*1.05, 7))
  if (maxday >= 70 & maxday < 140) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 14))}
  if (maxday >= 140 & maxday < 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 21))}
  if (maxday >= 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 28))}
  
  if(column == "volume"){
    p1 <- p1 + ylab(bquote('Tumor Volume ('*cm^3*')')) + ggtitle("Tumor Volume by Time")
  } else if(column == "rtv"){
    p1 <- p1 + ylab('RTV') + ggtitle("Relative Tumor Volume (RTV) by Time") + 
      geom_abline(slope=0, intercept=4, colour='gray', size = 1 * line.factor)
    if(max(dat$rawData$rtv, na.rm=T) > 8){
      p1 <- p1 + coord_cartesian(ylim = c(0,8))
    }
  } else if(column == "cd45"){
    p1 <- p1 + ylab('%hCD45') + ggtitle("%hCD45 by Time") +
      geom_abline(slope=0, intercept=25, colour='gray', size = 1 * line.factor)
  }
  
  return(p1)
}



volume.rtv.cd45.single.mouse.plot <- function(dat, column, font.factor = 1, line.factor = 1){
  
  maxday <- dat$project$maxday
  # names(COLORHEX) <- dat$groupInfo$group
  
  
  plotData <- dat$rawData[,c("mouseID", "group", "day", column)]
  # exclude excluded mouse
  plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
  
  
  # color based on config$groupings (within config.R)
  #numgroups <- length(unique(config$groupings$tumor_type))
  #config$groupings$color <- COLORHEX[as.numeric(as.factor(config$groupings$tumor_type))]
  #expand this to the rawData so lengths match
  colnames(config$groupings)[1]<-"group"
  plotData <- merge(plotData,config$groupings, by="group")
  names(COLORHEX) <- unique(plotData$tumor_type)
  
  
  library(directlabels) #this is used to add pdx_ids to the end of each line
  
  # plot
  p1 = ggplot(plotData,aes_string(x="day",y=column,group="mouseID")) + 
    geom_line(aes(colour = tumor_type),size=line.factor*0.5, alpha=0.7) + 
    scale_color_manual(values = COLORHEX) + geom_dl(aes(label= group), method = list(cex=0.6,"last.points"))
  

  
  
  p1 <- p1 + theme_bw() + 
    theme(legend.title=element_blank())  
  p1 <- p1 + xlab('Days Post Treatment Initiation')  #+ scale_x_continuous(breaks = seq(0, maxday*1.05, 7))
  if (maxday >= 70 & maxday < 140) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 14))}
  if (maxday >= 140 & maxday < 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 21),
                                                                  expand = c(0,0.1,0,0),
                                                                  limits = c(0,maxday*1.1))}
  if (maxday >= 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 28))}
  
  if(column == "volume"){
    p1 <- p1 + ylab('Volume (cm^3)') + ggtitle("Tumor Volume by Time")
  } else if(column == "rtv"){
    p1 <- p1 + ylab('RTV') + ggtitle("Relative Tumor Volume (RTV) by Time") + 
      geom_abline(slope=0, intercept=4, colour='gray', size = 1 * line.factor)
    if(max(dat$rawData$rtv, na.rm=T) > 8){
      p1 <- p1 + coord_cartesian(ylim = c(0,8))
    }
  } else if(column == "cd45"){
    p1 <- p1 + ylab('% Human CD45+') + ggtitle("%hCD45 by Time") +
      geom_abline(slope=0, intercept=25, colour='gray', size = 1 * line.factor)
  }
  
  return(p1)
}


swimmer.plot.single.mouse <- function(dat, font.factor = 1, line.factor = 1) {
  plotData <- dat$formattedData[dat$formattedData$exclude != TRUE,]
  #maxday <- dat$project$maxday
  #add tumor type column
  plotData<-merge(plotData,config$groupings,by="group")

  #sort(desc) within each tumor type
  plotData <- plotData[order(plotData$tumor_type, plotData$survival.time),]
  
  #set pointss for leukemic events; all groups get shape=3, but only code=3 mice get alpha !=0
  plotData$alpha <- ifelse(plotData$code == 3,1,0)
  
  #set date of last treatment
  if (!anyNA(dat$groupInfo$date_of_treatment_completion)) {
    treatment_completion <- unique(as.Date(dat$groupInfo$date_of_treatment_completion) - as.Date(dat$groupInfo$date_of_first_treatment))
  } else {
    treatment_completion <- 21
  }
  
  names(COLORHEX) <- unique(plotData$tumor_type)
  
  p1 = ggplot(plotData,aes(x=group,y=survival.time, fill=tumor_type)) +
    geom_bar(stat="identity") +
    #scale_color_manual(values = COLORHEX) +
    scale_fill_manual(values = COLORHEX) +
    coord_flip() +
    xlab('') +
    ylab("Days Post Treatment Initiation") +
    theme_bw() +
    theme(legend.title = element_blank(),
          axis.line = element_line(color = "black")) +
    scale_x_discrete(limits=plotData$group) + 
    geom_hline(yintercept=treatment_completion, linetype="dashed") #+
    
  p1 = p1 + geom_point(aes(x=group,y=survival.time), shape=17,alpha=plotData$alpha) +
    guides(fill = guide_legend(override.aes = list(shape = NA)))
    
  


  p1
 
  
  return(p1) 
}



waterfall.plot.single.mouse <- function(dat) {
  plotData <- dat$formattedData[!dat$formattedData$exclude == TRUE,]
  # add tumor type
  plotData <- merge(plotData,config$groupings, by="group", all.x=TRUE)
  # add min.cd45
  x<-as.data.frame(dat$table[,c(1,4)])
  colnames(x)<-c("group","min.cd45")
  plotData <- merge(plotData,x,by="group",all.x=TRUE)
  plotData$min.cd45 <- as.numeric(as.character(plotData$min.cd45))
  # calculate percent increase
  #dat$mice_level_min_rtv_rcd45 <- (dat$mice_level_min_rtv_rcd45 - 1) * 100  #percent increase; hence the minus 1
  plotData$percent.increase <- (plotData$min.cd45 - 1) * 100
  # cap at +/- 100%
  plotData$percent.increase <- ifelse(plotData$percent.increase > 100, 100,plotData$percent.increase)
  
  # sort by tumot_type then by percent.increase
  plotData <- plotData[order(-plotData$percent.increase),]
  
  names(COLORHEX) <- sort(unique(plotData$tumor_type))
  
  p1 = ggplot(plotData, aes(x=group, y=percent.increase, fill=tumor_type)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = COLORHEX) +
    theme_bw() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab('Tumor Models') +
    ylab('Minimum %huCD45+\n(% change from baseline, capped at +100%)') +
    scale_x_discrete(limits=plotData$group) #sets the order of x-axis
  
  p1
  
  return(p1)
  
  
}



### PNG plots #####
# EFS Survival plot.
# Input the entire experiment dat object
survival.plot.png.style <- function(dat, censored = FALSE, color = TRUE, font.factor = 1, line.factor = 1, custom.colors=custom.colors){
  tmp <- dat$formattedData[ !dat$formattedData$exclude, ]
  if(is.null(dat$km)){
    dat$km <- km_fit(dat$formattedData[ !dat$formattedData$exclude, ])
  }
  # Survival plot
  maxday <- dat$project$maxday
  # names(COLORHEX) <- dat$groupInfo$group
  numgroups <- length(dat$groupInfo$group)
  
  
  if(censored){
    if(color){
      ## p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= COLORHEX[1:numgroups],
      p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= custom.colors, 
                  cens.col = 'black', back.white = TRUE,
                  size.est = 1 * line.factor) 
    } else {
      p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= gray.colors(numgroups, start = 0, end = 0.6), 
                  cens.col = 'black', lty.est = c(1:numgroups), back.white = TRUE,
                  size.est = 1 * line.factor) 
    }
  } else {
    if(color){
      ## p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= COLORHEX[1:numgroups],
      p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= custom.colors,

                  back.white = TRUE, size.est = 1 * line.factor) 
    } else {
      p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= gray.colors(numgroups, start = 0, end = 0.6), 
                  lty.est = c(1:numgroups), back.white = TRUE,
                  size.est = 1 * line.factor) 
    }
  }
  
  p1 = p1 + xlab("Days Post Treatment Initiation") + ylab("EFS Probability (%)") +
    # ggtitle("Time to Event Plot") + 
    theme(legend.title=element_blank(), 
          legend.key.width = unit(0.5 * font.factor, "cm"),
          #plot.title = element_text(size=12 * font.factor),
          legend.text = element_text(size=12 * font.factor),
          axis.text = element_text(size=12 * font.factor),
          axis.title = element_text(size=12 * font.factor),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          axis.title.y=element_text(margin=margin(0,0,0,0)),
          axis.title.x=element_text(margin=margin(10,0,0,0)),
          #plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
          plot.margin=unit(c(0,0.5,0.5,0.5),"cm"),
          legend.position="none") +
    scale_x_continuous(breaks = seq(0, max(dat$project$maxday * 1.1, 42), 7), expand = c(0,0), #this is here on purpose. Sites requested a min of 42 days for plots
    limits = c(0, max(dat$project$maxday * 1.1, 42))) + 
    # scale_x_continuous(breaks = seq(0, 100, 14), expand = c(0,0),
    #                    limits = c(0, 100)) +
    scale_y_continuous(labels = seq(0,100,25), 
                       breaks = seq(0,1,0.25), expand = c(0,0), limits = c(0, 1.01))
  # if ("cd45.plot" %in% names(dat)){
  #   p1 = p1 + coord_cartesian(xlim = c(0,max(dat$project$maxday, 42)))
  # }
  
   if (maxday >= 70 & maxday < 140) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday * 1.1, 14),limits = c(0, dat$project$maxday * 1.1))}
   if (maxday >= 140 & maxday < 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday * 1.1, 21),limits = c(0, dat$project$maxday * 1.1))}
   if (maxday >= 210 & maxday < 300) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday * 1.1, 28),limits = c(0, dat$project$maxday * 1.1))}
   if (maxday >= 300) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday * 1.1, 42),limits = c(0, dat$project$maxday * 1.1))}
  
  return(p1)
}


# Volume, RTV, %hCD45 by time plot
# Input the entire experiment dat object
volume.rtv.cd45.plot.png.style <- function(dat, column, color=TRUE, font.factor = 1, line.factor = 1,
                                 indiv.mouse = TRUE,log.transform=TRUE, custom.colors = custom.colors){
  
  maxday <- dat$project$maxday
  # names(COLORHEX) <- dat$groupInfo$group
  numgroups <- length(dat$groupInfo$group)
  plotData <- dat$rawData[,c("mouseID", "group", "day", column)]
  # exclude excluded mouse
  plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]

  minday = as.numeric(min(plotData$day))
  medians <- dat[[paste(column, "median", sep=".")]]
  
  # coerce all cd45 > 50 to be =50
   if (column == "cd45") {
     plotData$cd45<-ifelse(plotData$cd45 > 50,50,plotData$cd45)
     medians$cd45<-ifelse(medians$cd45 > 50,50,medians$cd45)
   }
  
  
  if (column == "volume" & log.transform) {
    medians$volume<-ifelse(medians$volume < 0.01, 0.01,medians$volume)
    plotData$volume <- ifelse(plotData$volume < 0.01, 0.01,plotData$volume)
     
   }
  
  # plot
  if(indiv.mouse){
    if(color){
      p1 = ggplot(plotData,aes_string(x="day",y=column,group="mouseID"))  
      
      if(column == "rtv"){
        p1 = p1 + geom_abline(slope=0, intercept=4, colour='gray', size = 1 * line.factor)
      } else if (column == "rtv"){
        p1 = p1  + geom_abline(slope=0, intercept=25, colour='gray', size = 1 * line.factor)
      }
      
      p1 = p1 + geom_line(aes(colour = group),alpha=0.3,size=0.5 * line.factor) + 
        geom_line(data = medians, aes(group=group, color = group), size=1 * line.factor) +
        scale_color_manual(values = custom.colors)
      
    } else{
      p1 = ggplot(plotData,aes_string(x="day",y=column,group="mouseID")) 
      if(column == "rtv"){
        p1 = p1 + geom_abline(slope=0, intercept=4, colour='gray', size = 1 * line.factor)
      } else if (column == "rtv"){
        p1 = p1  + geom_abline(slope=0, intercept=25, colour='gray', size = 1 * line.factor)
      }
      p1 = p1 + geom_line(aes(colour=group, linetype=group),alpha=0.2,size=0.5 * line.factor) +
        scale_color_grey(start = 0.5, end = 0.7) + theme_bw() + 
        geom_line(data = medians, aes(group=group, linetype=group), color = "black", size=1 * line.factor)
    }
  }
  
  p1 <- p1 + theme_bw() +
    theme(legend.title=element_blank(), 
          text = element_text(size=12 * font.factor),
          plot.title = element_text(size=12 * font.factor),
          legend.text = element_text(size=12 * font.factor),
          axis.text = element_text(size=12 * font.factor),
          axis.title = element_text(size=12 * font.factor),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          axis.title.y=element_text(margin=margin(0,10,0,0)),
          axis.title.x=element_text(margin=margin(10,0,0,0)),
          plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
          legend.position="none") 

  if(column == "volume"){
    if (log.transform) {
      p1 <- p1 + ylab(bquote('Tumor Volume ('*cm^3*')')) +
        scale_y_continuous(trans="log10",
                           limits=c(0.01,10),
                           breaks=c(0.01,0.1,1,10),
                           labels=c(0.01,0.1,1,10))
    } else {
      p1 <- p1 + ylab(bquote('Tumor Volume ('*cm^3*')'))  
    }
    

  } else if(column == "rtv"){
    p1 <- p1 + ylab('Relative Tumor Volume')
    if(max(dat$rawData$rtv, na.rm=T) > 8){
      p1 <- p1 + coord_cartesian(ylim = c(0,8))
    }

  } else if(column == "cd45"){
    p1 <- p1 + ylab("%huCD45+") +
      # scale_y_continuous will remove lines with points out of range
      # coord_cartesian will still plot the line
      
      #scale_y_continuous(labels = seq(0,50,10), breaks = seq(0, 50, 10), expand = c(0,0), limits = c(minday,50.5))

      
       scale_y_continuous(labels = seq(0,50,10), breaks = seq(0, 50, 10),
                          expand = c(0.005,0,0,0),
                          limits = c(0,50.5))
      #coord_cartesian(ylim = c(0,50),expand=T) #expand = T (default) will float the x-y intercept. expand=F won't. You can't control x and y separately.
  }
  
  

  
  
  #if (!is.null(dat$project$minday)) {
  #  minday = dat$project$minday
  #} else {
  #  minday = 0
  #}
  p1 <- p1 + xlab('Days Post Treatment Initiation') +  
    scale_x_continuous(breaks = seq(0, max(maxday*1.1, 42), 7), 
                       expand = c(0,0),
                       limits = c(minday, max(maxday*1.1, 42)))
  if (maxday >= 70 & maxday < 140) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 14),expand = c(0,0), limits = c(minday, max(maxday*1.1, 42)))}
  if (maxday >= 140 & maxday < 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 21),expand = c(0,0), limits = c(minday, max(maxday*1.1, 42)))}
  if (maxday >= 210 & maxday < 300) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 28),expand = c(0,0), limits = c(minday, max(maxday*1.1, 42)))}
  if (maxday >= 300) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 42),expand = c(0,0), limits = c(minday, max(maxday*1.1, 42)))}
  
  return(p1)
}




volume.rtv.cd45.single.mouse.plot.png.style <- function(dat, column, font.factor = 1, line.factor = 1){
  
  #column <- 'cd45'
  maxday <- dat$project$maxday
  minday <- 0
  # names(COLORHEX) <- dat$groupInfo$group
  
  
  plotData <- dat$rawData[,c("mouseID", "group", "day", column)]
  # exclude excluded mouse
  plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
  
  
  # color based on config$groupings (within config.R)
  #numgroups <- length(unique(config$groupings$tumor_type))
  #config$groupings$color <- COLORHEX[as.numeric(as.factor(config$groupings$tumor_type))]
  #expand this to the rawData so lengths match
  colnames(config$groupings)[1]<-"group"
  plotData <- merge(plotData,config$groupings, by="group")
  names(COLORHEX) <- unique(plotData$tumor_type)
  
  library(directlabels) #this is used to add pdx_ids to the end of each line
  
  
  # plot
  p1 = ggplot(plotData,aes_string(x="day",y=column,group="mouseID")) + 
    geom_line(aes(colour = tumor_type),size=line.factor*0.6, alpha=0.7) + 
    #geom_line(aes(colour=group, linetype=group),alpha=0.2,size=0.2 * line.factor)
    scale_color_manual(values = COLORHEX) + 
    geom_dl(aes(label= group), method = list(cex=0.4,"last.points"))
  
  
  
  
  
  # set theme
  p1 <- p1 +theme_bw() +
    theme(legend.title=element_blank(),
          legend.position = "none",
          plot.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"))  
  p1 <- p1 + xlab('Days Post Treatment Initiation')  #+ scale_x_continuous(breaks = seq(0, maxday*1.05, 7))
  if (maxday >= 70 & maxday < 140) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 14),
                                                                 expand=c(0,0,0,0))}
  if (maxday >= 140 & maxday < 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 21),
                                                                  expand=c(0,0.1,0.1,0.1))}
  if (maxday >= 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 28),
                                                   expand=c(0,0,0,0))}
  
  if(column == "volume"){
    p1 <- p1 + ylab('Volume (cm^3)') + ggtitle("Tumor Volume by Time")
  } else if(column == "rtv"){
    p1 <- p1 + ylab('RTV') + ggtitle("Relative Tumor Volume (RTV) by Time") + 
      geom_abline(slope=0, intercept=4, colour='gray', size = 1 * line.factor)
    if(max(dat$rawData$rtv, na.rm=T) > 8){
      p1 <- p1 + coord_cartesian(ylim = c(0,8))
    }
  } else if(column == "cd45"){
    p1 <- p1 + ylab('% Human CD45+') +
      coord_cartesian(ylim = c(0,50))
      #geom_abline(slope=0, intercept=25, colour='gray', size = 1 * line.factor) +
      #scale_y_continuous(labels = seq(0,50,10), breaks = seq(0, 50, 10), 
      #                   expand = c(0.02,0,0,0),
      #                   limits = c(minday,50.5))
  }
  
  return(p1)
}






weight.plot.png.style<-function(dat, color = TRUE, font.factor = 1, 
                             line.factor = 1, config = config,
                             custom.colors = custom.colors) {
  # define the color to be used
  #maxday <- dat$project$maxday #fails if weight and processed don't match
  # names(COLORHEX) <- dat$groupInfo$group
  numgroups <- length(dat$groupInfo$group)
  
  # mean weight and number of mice
  tmp <- aggregate(dat$weight$weight, by=list(dat$weight$group, dat$weight$days), mean, na.rm=TRUE)
  colnames(tmp) <- c("Group", "Days", "Weight")
  tmp2 <- aggregate(dat$weight$weight, by=list(dat$weight$group, dat$weight$days), length)
  colnames(tmp2) <- c("Group", "Days", "Measured_mice")
  tmp <- merge(tmp, tmp2, sort = FALSE)
  tmp$Group <- factor(tmp$Group, levels = LETTERS)
  
  maxday<-max(tmp$Days)
  minday<-min(tmp$Days)
  # plot weight by time
  p1 <- ggplot(tmp,aes(x=Days,y=Weight,group=Group))
  p1 <- p1 + geom_line(aes(color = Group), size = 1 * line.factor) + 
    scale_color_manual(name = "Group", values = custom.colors) +
    geom_text(aes(label=Measured_mice),size=4 * font.factor)
  # Aesthetics
  
  p1 <- p1 + xlab("Days Post Treatment Initiation") + ylab("Average Weight (g)") +
    theme_bw() + xlim(0, 1.05 * maxday) +
    theme(legend.title=element_blank(), 
          plot.title = element_text(hjust=0.5),
          axis.text = element_text(size=12 * font.factor),
          axis.title = element_text(size=12 * font.factor),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none",
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")) +
    scale_x_continuous(breaks = seq(0, max(dat$project$maxday * 1.1, 42), 7), #this is here on purpose. Sites requested a min of 42 days for plots
                      limits = c(minday, max(dat$project$maxday * 1.1, 42.1)))
    #scale_x_continuous(breaks = seq(0, 100, 14))
  # use max(tmp$Days) here instead of maxday
  if (max(tmp$Days) >= 70) {p1 = p1 + scale_x_continuous(breaks = seq(0, max(tmp$Days)*1.1, 14))}
  if (max(tmp$Days) >= 140) {p1 = p1 + scale_x_continuous(breaks = seq(0, max(tmp$Days)*1.1, 21))}
  if (max(tmp$Days) >= 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, max(tmp$Days)*1.1, 42))}
  
  
  return(p1)
  
}



swimmer.plot.single.mouse.png.style <- function(dat) {
  plotData <- dat$formattedData[dat$formattedData$exclude != TRUE,]
  maxday <- dat$project$maxday
  #add tumor type column
  plotData<-merge(plotData,config$groupings,by="group")
  
  #sort(desc) within each tumor type
  plotData <- plotData[order(plotData$tumor_type, plotData$survival.time),]
  
  #set pointss for leukemic events; all groups get shape=3, but only code=3 mice get alpha !=0
  plotData$alpha <- ifelse(plotData$code == 3,1,0)
  
  #set date of last treatment
  if (!anyNA(dat$groupInfo$date_of_treatment_completion)) {
    treatment_completion <- unique(as.Date(dat$groupInfo$date_of_treatment_completion) - as.Date(dat$groupInfo$date_of_first_treatment))
  } else {
    treatment_completion <- 21
  }
  
  names(COLORHEX) <- unique(plotData$tumor_type)
  
  p1 = ggplot(plotData,aes(x=group,y=survival.time, fill=tumor_type)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = COLORHEX) +
    coord_flip() +
    xlab('') +
    ylab("Days Post Treatment Initiation") +
    theme(legend.title = element_blank(),
          axis.line = element_line(color = "black"),
          legend.position = "none",
          panel.background = element_blank()) +
    scale_x_discrete(limits=plotData$group) + 
    #scale_y_continuous(limits = c(0,maxday*1.1)) +
    geom_hline(yintercept=treatment_completion, linetype="dashed")
  p1 = p1 + geom_point(aes(x=group,y=survival.time), shape=17,alpha=plotData$alpha) 
  
  

 
  
  
  return(p1) 
}



waterfall.plot.single.mouse.png.style <- function(dat) {
  plotData <- dat$formattedData[!dat$formattedData$exclude == TRUE,]
  # add tumor type
  plotData <- merge(plotData,config$groupings, by="group", all.x=TRUE)
  # add min.cd45
  x<-as.data.frame(dat$table[,c(1,4)])
  colnames(x)<-c("group","min.cd45")
  plotData <- merge(plotData,x,by="group",all.x=TRUE)
  plotData$min.cd45 <- as.numeric(as.character(plotData$min.cd45))
  # calculate percent increase
  #dat$mice_level_min_rtv_rcd45 <- (dat$mice_level_min_rtv_rcd45 - 1) * 100  #percent increase; hence the minus 1
  #plotData$percent.increase <- (plotData$min.cd45 - 1) * 100
  # cap at +/- 100%
  #plotData$percent.increase <- ifelse(plotData$percent.increase > 100, 100,plotData$percent.increase)
  plotData$percent.increase = 100 * (plotData$min.cd45-plotData$baselinecd45) / plotData$baselinecd45
  plotData[plotData$percent.increase > 100,"percent.increase"] <- 100
  
  # sort by tumot_type then by percent.increase
  plotData <- plotData[order(-plotData$percent.increase),]
  
  names(COLORHEX) <- sort(unique(plotData$tumor_type))
  
  p1 = ggplot(plotData, aes(x=group, y=percent.increase, fill=tumor_type)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = COLORHEX) +
    theme_bw() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          axis.line = element_line(color = "black"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    xlab('Tumor Models') +
    ylab('Minimum %huCD45+\n(% change from baseline, capped at +100%)') +
    scale_x_discrete(limits=plotData$group) #sets the order of x-axis
  
  p1
  
  return(p1)
  
  
}







size_progression.plot.png.style<-function(dat,line.factor=1,font.factor=1,config=config) {
  p1 <- list()
  grid <- list()
  plots <- list()
  
  # names(COLORHEX) <- dat$groupInfo$group
  numgroups <- length(dat$groupInfo$group)
  
  tmp <- dat$rawData[,c('mouseID', 'group', 'diameter_x', 'diameter_y', 'day')]
  tmp$diameter_1 <- apply(tmp[,c('diameter_x', 'diameter_y')], 1, max)
  tmp$diameter_2 <- apply(tmp[,c('diameter_x', 'diameter_y')], 1, min)
  
  tmp$dot.size <- 1 * line.factor
  tmp$dot.size[ tmp$day == 0 ] <- 2 * line.factor
  for (mouse in unique(tmp$mouse)){
    tmp$dot.size[tmp$mouse == mouse & tmp$day == max(tmp$day[tmp$mouse == mouse])] <- 0.5 * line.factor
  }
  tmp$dot.size[tmp$diameter_x < 1 & tmp$diameter_y < 1] <- 0.5 * line.factor
  
  tmp$dot.shape <- 16
  tmp$dot.shape[tmp$day == 0] <- 21
  
  for (group in dat$groupInfo$group){
    tmp_trt <- tmp[tmp$group == group, ]
    p1 <- ggplot(tmp_trt,aes(x=diameter_1,y=diameter_2,group=mouseID)) + 
      geom_path(arrow = arrow(angle=20, length = unit(0.1 * line.factor, "inch"), type="open"), color=COLORHEX[group]) + 
      scale_shape_identity() + scale_size_identity() +
      geom_point(aes(size = dot.size, shape=dot.shape), color=COLORHEX[group]) +
      theme_bw() + geom_abline(slope=1, intercept=0, colour='black', size = 1 * line.factor)
    
    p1 <- p1 + xlim(min(c(tmp$diameter_1, tmp$diameter_2)) * 0.9, max(c(tmp$diameter_1, tmp$diameter_2)) * 1.05) +
      ylim(min(c(tmp$diameter_1, tmp$diameter_2)) * 0.9, max(c(tmp$diameter_1, tmp$diameter_2)) * 1.05) + 
      coord_fixed(ratio = 1) + xlab("Length (mm)") + ylab("Width (mm)") +
      #ggtitle(paste0("Tumor Size Progression (Group: ", group, ")")) + 
      theme(axis.text = element_text(size=12* config$font.factor),
            axis.title = element_text(size=12 * config$font.factor),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position="none",
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"))
    
    plots[[group]] <- p1  
  }
  # plot.A <- dat$diameter.plots[['A']]
  # plot.A <- plot.A + theme_bw() +
  
  # 
  # plot.B <- dat$diameter.plots[['B']]
  
  grid<-grid.arrange(plots[['A']],
                     plots[['B']],
                     layout_matrix = matrix(c(1,2),ncol=2,byrow=T))
  return(grid)  
}
  
  