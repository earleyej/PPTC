require(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
require(grid, quietly = TRUE, warn.conflicts = FALSE)
require(GGally, quietly = TRUE, warn.conflicts = FALSE)
source(paste0(function.dir,'ggsurv.R'))
source(paste0(function.dir,'km.R'))
require(reshape2, quietly = TRUE, warn.conflicts = FALSE)

#####################
# # require("extrafont")
# # font_import("C:/Windows/Fonts/", pattern = "arial*")
# # loadfonts()
# 
# # Aesthics design for plots
# hcd45.plot.AACR.yung <- function(dat){
#   numgroups <- length(dat$groupInfo$group)
#   
#   plotData <- dat$rawData[,c("mouseID", "group", "day", "cd45")]
#   # exclude excluded mouse
#   plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
#   
#   # if less than half mouse has data at the given date, define the datapoint before it as last day
#   mouse.n <- aggregate(! is.na(plotData[["cd45"]]), by=list(plotData$group, plotData$day), sum)
#   group.n <- aggregate(dat$formattedData$mouseID[! dat$formattedData$exclude], 
#                        by = list(dat$formattedData$group[! dat$formattedData$exclude]), length)
#   mouse.n <- merge(mouse.n, group.n, by = "Group.1", sort = FALSE)
#   mouse.n <- mouse.n[mouse.n$x.x > mouse.n$x.y/2,]
#   colnames(mouse.n) <- c("group", "day", "time.n", "group.n")
#   lastDay <- aggregate(as.numeric(mouse.n$day), by=list(mouse.n$group), max)
#   colnames(lastDay) <- c("group", "day")
#   
#   # subsetting medians till the last day
#   medians <- aggregate(plotData[["cd45"]], by=list(plotData$group, plotData$day), median, na.rm=T)
#   names(medians) <- c('group', 'day', "cd45")
#   medians$keep <- medians$day <= lastDay$day[match(medians$group, lastDay$group)]
#   medians <- medians[medians$keep & !is.na(medians[["cd45"]]), c('group', 'day', "cd45")]
#   
#   # plot
#   p1 = ggplot(plotData,aes_string(x="day",y="cd45",group="mouseID")) + 
#     geom_line(aes(linetype = factor(plotData$group, levels = c("B", "A"))), size=0.3, colour = "gray") + 
#     theme(legend.position="none") +
#     geom_line(data = medians, aes(group=group, color = group, 
#                                   linetype = factor(medians$group, levels = c("B", "A"))), 
#               size=1) + scale_color_manual(values = c("#000000", "#0000FF")) + theme_bw() + 
#     theme(legend.position="none",
#           text = element_text(size=9, family = "Arial", face = "bold"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(), 
#           axis.line = element_line(colour = "black"),
#           panel.border = element_blank()) + 
#     xlab('Days (post treatment initiation)')  + 
#     scale_x_continuous(breaks = seq(0, 105, 10)) +
#     ylab(expression("%huCD" ~ 45^"+" ~ " in PB")) + 
#     scale_y_continuous(breaks = seq(0, 50, 25)) +
#     geom_abline(slope=0, intercept=25, colour='black', size = 0.5, linetype = 2)
#   
#   return(p1)
#   
# }
# 
# # tmp version - y axis label slightly different
# hcd45.plot.AACR.yung <- function(dat){
#   numgroups <- length(dat$groupInfo$group)
#   
#   plotData <- dat$rawData[,c("mouseID", "group", "day", "cd45")]
#   # exclude excluded mouse
#   plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
#   
#   # if less than half mouse has data at the given date, define the datapoint before it as last day
#   mouse.n <- aggregate(! is.na(plotData[["cd45"]]), by=list(plotData$group, plotData$day), sum)
#   group.n <- aggregate(dat$formattedData$mouseID[! dat$formattedData$exclude], 
#                        by = list(dat$formattedData$group[! dat$formattedData$exclude]), length)
#   mouse.n <- merge(mouse.n, group.n, by = "Group.1", sort = FALSE)
#   mouse.n <- mouse.n[mouse.n$x.x > mouse.n$x.y/2,]
#   colnames(mouse.n) <- c("group", "day", "time.n", "group.n")
#   lastDay <- aggregate(as.numeric(mouse.n$day), by=list(mouse.n$group), max)
#   colnames(lastDay) <- c("group", "day")
#   
#   # subsetting medians till the last day
#   medians <- aggregate(plotData[["cd45"]], by=list(plotData$group, plotData$day), median, na.rm=T)
#   names(medians) <- c('group', 'day', "cd45")
#   medians$keep <- medians$day <= lastDay$day[match(medians$group, lastDay$group)]
#   medians <- medians[medians$keep & !is.na(medians[["cd45"]]), c('group', 'day', "cd45")]
#   
#   # plot
#   p1 = ggplot(plotData,aes_string(x="day",y="cd45",group="mouseID")) + 
#     geom_line(aes(color = group), size=0.3, alpha=0.3) + 
#     theme(legend.position="none") +
#     geom_line(data = medians, aes(group=group, color = group), 
#               size=1) + scale_color_manual(values = c("#000000", "#0000FF")) + theme_bw() + 
#     theme(legend.position="none",
#           text = element_text(size=9, family = "Arial", face = "bold"),
#           plot.title = element_text(size=9, family = "Arial", face = "bold"),
#           legend.text = element_text(size=9, family = "Arial", face = "bold"),
#           axis.text = element_text(size=9, family = "Arial", face = "bold"),
#           axis.title = element_text(size=9, family = "Arial", face = "bold"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(), 
#           axis.line = element_line(colour = "black"),
#           panel.border = element_blank(),
#           axis.title.x=element_text(margin=margin(10,0,0,0))) + 
#     xlab('Days (post treatment initiation)')  + 
#     scale_x_continuous(breaks = seq(0, 105, 10), expand = c(0,0)) +
#     ylab("%huCD45+ in PB") + 
#     scale_y_continuous(breaks = seq(0, 50, 25), expand = c(0,0)) +
#     coord_cartesian(xlim = c(0,105), ylim = c(0,50)) +
#     geom_abline(slope=0, intercept=25, colour='black', size = 0.5, linetype = 2)
#   
#   return(p1)
# }
# 
# # tmp version
# survival.plot.AACR.yung <- function(dat, censored = TRUE){
#   dat$formattedData$group[dat$formattedData$group == "A"] <- "Control"
#   dat$formattedData$group[dat$formattedData$group == "B"] <- "OT-82"
#   dat$km <- km_fit(dat$formattedData[! dat$formattedData$exclude, ])
#   
#   # Survival plot
#   COLORHEX <- c("#000000", "#0000FF")
#   names(COLORHEX) <- c("Control", "OT-82")
#   
#   # percentage
#   
#   if(censored){
#     p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= COLORHEX, 
#                 cens.col = 'black', back.white = TRUE,
#                 size.est = 1 ) 
#   } else {
#     p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= COLORHEX,
#                 back.white = TRUE, size.est = 1) 
#   }
#   
#   p1 = p1 + xlab("EFS (days post treatment initiation)") + 
#     ylab("Percentage survival") +
#     theme(legend.title=element_blank(), 
#           legend.key.width = unit(0.5, "cm"),
#           plot.title = element_text(size=9, family = "Arial", face = "bold"),
#           legend.text = element_text(size=9, family = "Arial", face = "bold"),
#           axis.text = element_text(size=9, family = "Arial", face = "bold"),
#           axis.title = element_text(size=9, family = "Arial", face = "bold"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(), 
#           axis.line = element_line(colour = "black"),
#           panel.border = element_blank(),
#           axis.title.x=element_text(margin=margin(10,0,0,0))) +
#     scale_x_continuous(breaks = seq(0, 105, 10), expand = c(0,0)) + 
#     coord_cartesian(xlim = c(0,105)) +
#     scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.5), expand = c(0,0))
#   
#   return(p1)
# }
# 
# # tmp version
# hcd45.plot.multi.AACR.yung <- function(dat){
#   numgroups <- length(dat$groupInfo$group)
#   
#   plotData <- dat$rawData[,c("mouseID", "group", "day", "cd45")]
#   # exclude excluded mouse
#   plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
#   
#   # if less than half mouse has data at the given date, define the datapoint before it as last day
#   mouse.n <- aggregate(! is.na(plotData[["cd45"]]), by=list(plotData$group, plotData$day), sum)
#   group.n <- aggregate(dat$formattedData$mouseID[! dat$formattedData$exclude], 
#                        by = list(dat$formattedData$group[! dat$formattedData$exclude]), length)
#   mouse.n <- merge(mouse.n, group.n, by = "Group.1", sort = FALSE)
#   mouse.n <- mouse.n[mouse.n$x.x > mouse.n$x.y/2,]
#   colnames(mouse.n) <- c("group", "day", "time.n", "group.n")
#   lastDay <- aggregate(as.numeric(mouse.n$day), by=list(mouse.n$group), max)
#   colnames(lastDay) <- c("group", "day")
#   
#   # subsetting medians till the last day
#   medians <- aggregate(plotData[["cd45"]], by=list(plotData$group, plotData$day), median, na.rm=T)
#   names(medians) <- c('group', 'day', "cd45")
#   medians$keep <- medians$day <= lastDay$day[match(medians$group, lastDay$group)]
#   medians <- medians[medians$keep & !is.na(medians[["cd45"]]), c('group', 'day', "cd45")]
#   
#   plotData$group[plotData$group == "A"] <- "Control"
#   plotData$group[plotData$group == "B"] <- "OT-82"
#   plotData$group[plotData$group == "C"] <- "AraC"
#   plotData$group[plotData$group == "D"] <- "OT-82 + AraC"
#   plotData$group <- factor(plotData$group, 
#                            levels = c("Control", "OT-82", "AraC", "OT-82 + AraC"))
#   
#   medians$group[medians$group == "A"] <- "Control"
#   medians$group[medians$group == "B"] <- "OT-82"
#   medians$group[medians$group == "C"] <- "AraC"
#   medians$group[medians$group == "D"] <- "OT-82 + AraC"
#   medians$group <- factor(medians$group, 
#                           levels = c("Control", "OT-82", "AraC", "OT-82 + AraC"))
#   COLORHEX <- c("#000000", "#0000FF", "#FF0000", "#00FF00")
#   names(COLORHEX) <- c("Control", "OT-82", "AraC", "OT-82 + AraC")
#   
#   # plot
#   p1 = ggplot(plotData,aes_string(x="day",y="cd45",group="mouseID")) + 
#     geom_line(aes(color = group), size=0.3, alpha=0.3) + 
#     geom_line(data = medians, aes(group=group, color = group), 
#               size=1) + 
#     scale_color_manual(values = COLORHEX) + 
#     theme_bw() + 
#     theme(legend.position="none",
#       legend.title = element_blank(),
#       text = element_text(size=9, family = "Arial", face = "bold"),
#       plot.title = element_text(size=9, family = "Arial", face = "bold"),
#       legend.text = element_text(size=9, family = "Arial", face = "bold"),
#       axis.text = element_text(size=9, family = "Arial", face = "bold"),
#       axis.title = element_text(size=9, family = "Arial", face = "bold"),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(), 
#       axis.line = element_line(colour = "black"),
#       panel.border = element_blank(),
#       axis.title.x=element_text(margin=margin(10,0,0,0))) + 
#     xlab('Days (post treatment initiation)')  + 
#     scale_x_continuous(breaks = seq(0, 105, 10), expand = c(0,0)) +
#     ylab("%huCD45+ in PB") + 
#     scale_y_continuous(breaks = seq(0, 50, 25), expand = c(0,0)) +
#     coord_cartesian(xlim = c(0,105), ylim = c(0,50)) +
#     geom_abline(slope=0, intercept=25, colour='black', size = 0.5, linetype = 2)
#   
#   return(p1)
# }
# 
# # tmp version
# survival.plot.multi.AACR.yung <- function(dat, censored = TRUE){
#   dat$formattedData$group[dat$formattedData$group == "A"] <- "Control"
#   dat$formattedData$group[dat$formattedData$group == "B"] <- "OT-82"
#   dat$formattedData$group[dat$formattedData$group == "C"] <- "AraC"
#   dat$formattedData$group[dat$formattedData$group == "D"] <- "OT-82 + AraC"
#   dat$formattedData$group <- factor(dat$formattedData$group, 
#                                     levels = c("Control", "OT-82", "AraC", "OT-82 + AraC"))
#   dat$km <- km_fit(dat$formattedData[! dat$formattedData$exclude, ])
#   
#   # Survival plot
#   COLORHEX <- c("#000000", "#0000FF", "#FF0000", "#00FF00")
#   names(COLORHEX) <- c("Control", "OT-82", "AraC", "OT-82 + AraC")
#   
#   # percentage
#   
#   if(censored){
#     p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= COLORHEX, 
#                 cens.col = 'black', back.white = TRUE,
#                 size.est = 1 ) 
#   } else {
#     p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= COLORHEX,
#                 back.white = TRUE, size.est = 1) 
#   }
#   
#   p1 = p1 + xlab("EFS (days post treatment initiation)") + 
#     ylab("Percentage survival") +
#     theme(legend.title=element_blank(), 
#           legend.key.width = unit(0.5, "cm"),
#           plot.title = element_text(size=9, family = "Arial", face = "bold"),
#           legend.text = element_text(size=9, family = "Arial", face = "bold"),
#           axis.text = element_text(size=9, family = "Arial", face = "bold"),
#           axis.title = element_text(size=9, family = "Arial", face = "bold"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(), 
#           axis.line = element_line(colour = "black"),
#           panel.border = element_blank(),
#           axis.title.x=element_text(margin=margin(10,0,0,0))) +
#     scale_x_continuous(breaks = seq(0, 105, 10), expand = c(0,0)) + 
#     coord_cartesian(xlim = c(0,105)) +
#     scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.5), expand = c(0,0))
#   
#   return(p1)
# }
# 
# # tmp version
# hcd45.plot.multi.AACR.kathryn <- function(dat){
#   numgroups <- length(dat$groupInfo$group)
#   
#   plotData <- dat$rawData[,c("mouseID", "group", "day", "cd45")]
#   # exclude excluded mouse
#   plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
#   
#   # if less than half mouse has data at the given date, define the datapoint before it as last day
#   mouse.n <- aggregate(! is.na(plotData[["cd45"]]), by=list(plotData$group, plotData$day), sum)
#   group.n <- aggregate(dat$formattedData$mouseID[! dat$formattedData$exclude], 
#                        by = list(dat$formattedData$group[! dat$formattedData$exclude]), length)
#   mouse.n <- merge(mouse.n, group.n, by = "Group.1", sort = FALSE)
#   mouse.n <- mouse.n[mouse.n$x.x > mouse.n$x.y/2,]
#   colnames(mouse.n) <- c("group", "day", "time.n", "group.n")
#   lastDay <- aggregate(as.numeric(mouse.n$day), by=list(mouse.n$group), max)
#   colnames(lastDay) <- c("group", "day")
#   
#   # subsetting medians till the last day
#   medians <- aggregate(plotData[["cd45"]], by=list(plotData$group, plotData$day), median, na.rm=T)
#   names(medians) <- c('group', 'day', "cd45")
#   medians$keep <- medians$day <= lastDay$day[match(medians$group, lastDay$group)]
#   medians <- medians[medians$keep & !is.na(medians[["cd45"]]), c('group', 'day', "cd45")]
#   
#   plotData$group[plotData$group == "A"] <- dat$groupInfo$drug[1]
#   plotData$group[plotData$group == "B"] <- dat$groupInfo$drug[2]
#   plotData$group[plotData$group == "C"] <- dat$groupInfo$drug[3]
#   plotData$group[plotData$group == "D"] <- dat$groupInfo$drug[4]
#   plotData$group[plotData$group == "E"] <- dat$groupInfo$drug[5]
#   plotData$group[plotData$group == "F"] <- dat$groupInfo$drug[6]
#   plotData$group <- factor(plotData$group, levels = dat$groupInfo$drug)
#   
#   medians$group[medians$group == "A"] <- dat$groupInfo$drug[1]
#   medians$group[medians$group == "B"] <- dat$groupInfo$drug[2]
#   medians$group[medians$group == "C"] <- dat$groupInfo$drug[3]
#   medians$group[medians$group == "D"] <- dat$groupInfo$drug[4]
#   medians$group[medians$group == "E"] <- dat$groupInfo$drug[5]
#   medians$group[medians$group == "F"] <- dat$groupInfo$drug[6]
#   medians$group <- factor(medians$group, levels = dat$groupInfo$drug)
#   
#   COLORHEX = c('#a50026', '#313695', '#f46d43', '#74add1', '#fdae61', '#d73027', 
#                '#4575b4', '#fee090', '#abd9e9', '#ffffbf', '#e0f3f8')
#   # plot
#   p1 = ggplot(plotData,aes_string(x="day",y="cd45",group="mouseID")) + 
#     geom_line(aes(color = group), size=1, alpha=0.3) + 
#     geom_line(data = medians, aes(group=group, color = group), 
#               size=1) + 
#     scale_color_manual(values = COLORHEX) + 
#     theme_bw() + 
#     theme(# legend.position="none",
#           # legend.title = element_blank(),
#           text = element_text(size=12),
#           plot.title = element_text(size=12),
#           legend.text = element_text(size=12),
#           axis.text = element_text(size=12),
#           axis.title = element_text(size=12),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(), 
#           axis.line = element_line(colour = "black"),
#           panel.border = element_blank(),
#           axis.title.y=element_text(margin=margin(0,10,0,0)),
#           axis.title.x=element_text(margin=margin(10,0,0,0))) + 
#     xlab('Days')  + 
#     scale_x_continuous(breaks = seq(0, max(dat$project$maxday, 42), 7), expand = c(0,0)) +
#     ylab("%huCD45+") + 
#     scale_y_continuous(breaks = seq(0, 50, 10), expand = c(0,0)) +
#     coord_cartesian(xlim = c(0,max(dat$project$maxday, 42)), ylim = c(0,50)) +
#     geom_abline(slope=0, intercept=25, colour='black', size = 0.5, linetype = 2)
#   
#   return(p1)
# }
# 
# # tmp version
# survival.plot.multi.AACR.kathryn <- function(dat, censored = TRUE){
#   dat$formattedData$group[dat$formattedData$group == "A"] <- dat$groupInfo$drug[1]
#   dat$formattedData$group[dat$formattedData$group == "B"] <- dat$groupInfo$drug[2]
#   dat$formattedData$group[dat$formattedData$group == "C"] <- dat$groupInfo$drug[3]
#   dat$formattedData$group[dat$formattedData$group == "D"] <- dat$groupInfo$drug[4]
#   dat$formattedData$group[dat$formattedData$group == "E"] <- dat$groupInfo$drug[5]
#   dat$formattedData$group[dat$formattedData$group == "F"] <- dat$groupInfo$drug[6]
#   dat$formattedData$group <- factor(dat$formattedData$group, levels = dat$groupInfo$drug)
#   dat$km <- km_fit(dat$formattedData[! dat$formattedData$exclude, ])
#   
#   # Survival plot
#   COLORHEX <- c('#a50026', '#313695', '#f46d43', '#74add1', '#fdae61', '#d73027', 
#                '#4575b4', '#fee090', '#abd9e9', '#ffffbf', '#e0f3f8')
#   COLORHEX <- COLORHEX[1:length(dat$groupInfo$group)]
#   names(COLORHEX) <- dat$groupInfo$drug
#   
#   # percentage
#   
#   if(censored){
#     p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= COLORHEX, 
#                 cens.col = 'black', back.white = TRUE,
#                 size.est = 1 ) 
#   } else {
#     p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= COLORHEX,
#                 back.white = TRUE, size.est = 1) 
#   }
#   
#   p1 = p1 + xlab("Days") + 
#     ylab("Survival Probability") +
#     theme(legend.title=element_blank(), 
#           legend.key.width = unit(0.5, "cm"),
#           plot.title = element_text(size=12),
#           legend.text = element_text(size=12),
#           axis.text = element_text(size=12),
#           axis.title = element_text(size=12),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(), 
#           axis.line = element_line(colour = "black"),
#           panel.border = element_blank(),
#           axis.title.y=element_text(margin=margin(0,10,0,0)),
#           axis.title.x=element_text(margin=margin(10,0,0,0))) +
#     scale_x_continuous(breaks = seq(0, max(dat$project$maxday, 42), 7), expand = c(0,0)) + 
#     coord_cartesian(xlim = c(0,max(dat$project$maxday, 42))) +
#     scale_y_continuous(labels = scales::percent, 
#                        breaks = seq(0,1,0.25), expand = c(0,0), limits = c(0, 1.01))
#   
#   return(p1)
# }
# 
# # tmp version
# hcd45.plot.multi.AACR.kathryn.flip34col <- function(dat){
#   numgroups <- length(dat$groupInfo$group)
#   
#   plotData <- dat$rawData[,c("mouseID", "group", "day", "cd45")]
#   # exclude excluded mouse
#   plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
#   
#   # if less than half mouse has data at the given date, define the datapoint before it as last day
#   mouse.n <- aggregate(! is.na(plotData[["cd45"]]), by=list(plotData$group, plotData$day), sum)
#   group.n <- aggregate(dat$formattedData$mouseID[! dat$formattedData$exclude], 
#                        by = list(dat$formattedData$group[! dat$formattedData$exclude]), length)
#   mouse.n <- merge(mouse.n, group.n, by = "Group.1", sort = FALSE)
#   mouse.n <- mouse.n[mouse.n$x.x > mouse.n$x.y/2,]
#   colnames(mouse.n) <- c("group", "day", "time.n", "group.n")
#   lastDay <- aggregate(as.numeric(mouse.n$day), by=list(mouse.n$group), max)
#   colnames(lastDay) <- c("group", "day")
#   
#   # subsetting medians till the last day
#   medians <- aggregate(plotData[["cd45"]], by=list(plotData$group, plotData$day), median, na.rm=T)
#   names(medians) <- c('group', 'day', "cd45")
#   medians$keep <- medians$day <= lastDay$day[match(medians$group, lastDay$group)]
#   medians <- medians[medians$keep & !is.na(medians[["cd45"]]), c('group', 'day', "cd45")]
#   
#   plotData$group[plotData$group == "A"] <- dat$groupInfo$drug[1]
#   plotData$group[plotData$group == "B"] <- dat$groupInfo$drug[2]
#   plotData$group[plotData$group == "C"] <- dat$groupInfo$drug[3]
#   plotData$group[plotData$group == "D"] <- dat$groupInfo$drug[4]
#   plotData$group <- factor(plotData$group, levels = dat$groupInfo$drug)
#   
#   medians$group[medians$group == "A"] <- dat$groupInfo$drug[1]
#   medians$group[medians$group == "B"] <- dat$groupInfo$drug[2]
#   medians$group[medians$group == "C"] <- dat$groupInfo$drug[3]
#   medians$group[medians$group == "D"] <- dat$groupInfo$drug[4]
#   medians$group <- factor(medians$group, levels = dat$groupInfo$drug)
#   
#   COLORHEX = c('#a50026', '#313695', '#74add1', '#f46d43')
#   # plot
#   p1 = ggplot(plotData,aes_string(x="day",y="cd45",group="mouseID")) + 
#     geom_line(aes(color = group), size=1, alpha=0.3) + 
#     geom_line(data = medians, aes(group=group, color = group), 
#               size=1) + 
#     scale_color_manual(values = COLORHEX) + 
#     theme_bw() + 
#     theme(# legend.position="none",
#       # legend.title = element_blank(),
#       text = element_text(size=12),
#       plot.title = element_text(size=12),
#       legend.text = element_text(size=12),
#       axis.text = element_text(size=12),
#       axis.title = element_text(size=12),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(), 
#       axis.line = element_line(colour = "black"),
#       panel.border = element_blank(),
#       axis.title.y=element_text(margin=margin(0,10,0,0)),
#       axis.title.x=element_text(margin=margin(10,0,0,0))) + 
#     xlab('Days')  + 
#     scale_x_continuous(breaks = seq(0, max(dat$project$maxday, 42), 7), expand = c(0,0)) +
#     ylab("%huCD45+") + 
#     scale_y_continuous(breaks = seq(0, 50, 10), expand = c(0,0)) +
#     coord_cartesian(xlim = c(0,max(dat$project$maxday, 42)), ylim = c(0,50)) +
#     geom_abline(slope=0, intercept=25, colour='black', size = 0.5, linetype = 2)
#   
#   return(p1)
# }
# 
# hcd45.plot.multi.AACR.kathryn.flip34col.v2 <- function(dat){
#   maxday <- dat$project$maxday
#   names(COLORHEX) <- dat$groupInfo$group
#   numgroups <- length(dat$groupInfo$group)
#   
#   plotData <- dat$rawData[,c("mouseID", "group", "day", "cd45")]
#   # exclude excluded mouse
#   plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
#   
#   # if less than half mouse has data at the given date, define the datapoint before it as last day
#   mouse.n <- aggregate(! is.na(plotData[["cd45"]]), by=list(plotData$group, plotData$day), sum)
#   group.n <- aggregate(dat$formattedData$mouseID[! dat$formattedData$exclude], 
#                        by = list(dat$formattedData$group[! dat$formattedData$exclude]), length)
#   mouse.n <- merge(mouse.n, group.n, by = "Group.1", sort = FALSE)
#   mouse.n <- mouse.n[mouse.n$x.x > mouse.n$x.y/2,]
#   colnames(mouse.n) <- c("group", "day", "time.n", "group.n")
#   lastDay <- aggregate(as.numeric(mouse.n$day), by=list(mouse.n$group), max)
#   colnames(lastDay) <- c("group", "day")
#   
#   # # subsetting medians till the last day
#   # medians <- aggregate(plotData[["cd45"]], by=list(plotData$group, plotData$day), median, na.rm=T)
#   # names(medians) <- c('group', 'day', "cd45")
#   # medians$keep <- medians$day <= lastDay$day[match(medians$group, lastDay$group)]
#   # medians <- medians[medians$keep & !is.na(medians[["cd45"]]), c('group', 'day', "cd45")]
# 
#   # subsetting medians till the last day
#   ### revision 20170519 ###
#   ### adding fake data for median calculation ###
#   dummyData <- plotData
#   dummyData$timepoint <- round(dummyData$day/3.5)
#   dat.time.match <- unique(dummyData[,c("group", "day", "timepoint")])
#   dummyData <- dcast(mouseID ~ timepoint, data = dummyData, value.var = "cd45")
#   evented <- dat$formattedData$mouseID[dat$formattedData$survival.event == 1]
#   dummyData[dummyData$mouseID %in% evented,][is.na(dummyData[dummyData$mouseID %in% evented,])] <- 100
#   dummyData <- melt(data = dummyData, variable.name = "timepoint", value.name = "cd45")
#   dummyData$group <- sapply(sapply(dummyData$mouseID, strsplit, ";"), function(x){x[[1]]})
#   dummyData <- merge(dummyData, dat.time.match, sort = FALSE)
#   dummyData$timepoint <- NULL
#   medians <- aggregate(dummyData[["cd45"]], by=list(dummyData$group, dummyData$day), median, na.rm=T)
#   names(medians) <- c('group', 'day', "cd45")
#   medians$keep <- medians$day <= lastDay$day[match(medians$group, lastDay$group)]
#   medians <- medians[medians$keep & !is.na(medians[["cd45"]]), c('group', 'day', "cd45")]
#   
# ##  if (dat$project$panel_code == "1622-ALL-29-1") medians <- medians[ medians$day != 37, ]
#   
#   
#   plotData$group[plotData$group == "A"] <- dat$groupInfo$drug[1]
#   plotData$group[plotData$group == "B"] <- dat$groupInfo$drug[2]
#   plotData$group[plotData$group == "C"] <- dat$groupInfo$drug[3]
#   plotData$group[plotData$group == "D"] <- dat$groupInfo$drug[4]
#   plotData$group[plotData$group == "E"] <- dat$groupInfo$drug[5]
#   plotData$group <- factor(plotData$group, levels = dat$groupInfo$drug)
#   
#   medians$group[medians$group == "A"] <- dat$groupInfo$drug[1]
#   medians$group[medians$group == "B"] <- dat$groupInfo$drug[2]
#   medians$group[medians$group == "C"] <- dat$groupInfo$drug[3]
#   medians$group[medians$group == "D"] <- dat$groupInfo$drug[4]
#   medians$group[medians$group == "E"] <- dat$groupInfo$drug[5]
#   medians$group <- factor(medians$group, levels = dat$groupInfo$drug)
#   
#   COLORHEX = c('#a50026', '#313695', '#74add1', '#f46d43')
#   COLORHEX = c('#a50026', '#313695', '#f46d43', '#74add1', '#fdae61', '#d73027', 
#                '#4575b4', '#fee090', '#abd9e9', '#ffffbf', '#e0f3f8')
#   # plot
#   xmarks = seq(0, max(dat$project$maxday, 14), 7)
#   xmarks[seq(2,(2*trunc(length(xmarks)/2)),2)] = ""
#   p1 = ggplot(plotData,aes_string(x="day",y="cd45",group="mouseID")) + 
#     geom_line(aes(color = group), size=1, alpha=0.3) + 
#     geom_line(data = medians, aes(group=group, color = group), 
#               size=1) + 
#     scale_color_manual(values = COLORHEX) + 
#     theme_bw() + 
#     theme(# legend.position="none",
#       # legend.title = element_blank(),
# 	  legend.position="none",
#       text = element_text(size=12),
#       plot.title = element_text(size=12),
#       legend.text = element_text(size=12),
#       axis.text = element_text(size=12),
#       axis.title = element_text(size=12),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(), 
#       axis.line = element_line(colour = "black"),
#       panel.border = element_blank(),
#       axis.title.y=element_text(margin=margin(0,10,0,0)),
#       axis.title.x=element_text(margin=margin(10,0,0,0))) + 
#     xlab('Days Post Treatment Initiation')  + 
#     scale_x_continuous(breaks = seq(0, max(dat$project$maxday, 14), 7), labels=xmarks,
# 	expand = c(0,0)) + ylab("%huCD45+ in the PB") + 
#     scale_y_continuous(breaks = seq(0, 50, 10), expand = c(0,0)) +
#     coord_cartesian(xlim = c(0,max(dat$project$maxday, 14)), ylim = c(0,50)) +
#     geom_abline(slope=0, intercept=25, colour='black', size = 0.5, linetype = 2)
#   
#   return(p1)
# }
# 
# # tmp version
# survival.plot.multi.AACR.kathryn.flip34col <- function(dat, censored = TRUE){
#   dat$formattedData$group[dat$formattedData$group == "A"] <- dat$groupInfo$drug[1]
#   dat$formattedData$group[dat$formattedData$group == "B"] <- dat$groupInfo$drug[2]
#   dat$formattedData$group[dat$formattedData$group == "C"] <- dat$groupInfo$drug[3]
#   dat$formattedData$group[dat$formattedData$group == "D"] <- dat$groupInfo$drug[4]
#   dat$formattedData$group <- factor(dat$formattedData$group, levels = dat$groupInfo$drug)
#   dat$km <- km_fit(dat$formattedData[! dat$formattedData$exclude, ])
#   
#   # Survival plot
#   COLORHEX <- c('#a50026', '#313695', '#74add1', '#f46d43')
#   COLORHEX <- COLORHEX[1:length(dat$groupInfo$group)]
#   names(COLORHEX) <- dat$groupInfo$drug
#   
#   # percentage
#   
#   if(censored){
#     p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= COLORHEX, 
#                 cens.col = 'black', back.white = TRUE,
#                 size.est = 1 ) 
#   } else {
#     p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= COLORHEX,
#                 back.white = TRUE, size.est = 1) 
#   }
#   
#   p1 = p1 + xlab("Days") + 
#     ylab("Survival Probability") +
#     theme(legend.title=element_blank(), 
#           legend.key.width = unit(0.5, "cm"),
#           plot.title = element_text(size=12),
#           legend.text = element_text(size=12),
#           axis.text = element_text(size=12),
#           axis.title = element_text(size=12),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(), 
#           axis.line = element_line(colour = "black"),
#           panel.border = element_blank(),
#           axis.title.y=element_text(margin=margin(0,10,0,0)),
#           axis.title.x=element_text(margin=margin(10,0,0,0))) +
#     scale_x_continuous(breaks = seq(0, max(dat$project$maxday, 42), 7), expand = c(0,0)) + 
#     coord_cartesian(xlim = c(0,max(dat$project$maxday, 42))) +
#     scale_y_continuous(labels = scales::percent, 
#                        breaks = seq(0,1,0.25), expand = c(0,0), limits = c(0, 1.01))
#   
#   return(p1)
# }
# 
# # tmp version
# survival.plot.multi.AACR.kathryn.flip34col.v2 <- function(dat, censored = TRUE){
#   dat$formattedData$group[dat$formattedData$group == "A"] <- dat$groupInfo$drug[1]
#   dat$formattedData$group[dat$formattedData$group == "B"] <- dat$groupInfo$drug[2]
#   dat$formattedData$group[dat$formattedData$group == "C"] <- dat$groupInfo$drug[3]
#   dat$formattedData$group[dat$formattedData$group == "D"] <- dat$groupInfo$drug[4]
#   dat$formattedData$group[dat$formattedData$group == "E"] <- dat$groupInfo$drug[5]
#   dat$formattedData$group <- factor(dat$formattedData$group, levels = dat$groupInfo$drug)
#   dat$km <- km_fit(dat$formattedData[! dat$formattedData$exclude, ])
#   
#   # Survival plot
#   COLORHEX <- c('#a50026', '#313695', '#74add1', '#f46d43')
#   COLORHEX = c('#a50026', '#313695', '#f46d43', '#74add1', '#fdae61', '#d73027', 
#                '#4575b4', '#fee090', '#abd9e9', '#ffffbf', '#e0f3f8')
#   COLORHEX <- COLORHEX[1:length(dat$groupInfo$group)]
#   names(COLORHEX) <- dat$groupInfo$drug
#   
#   # percentage
#   
#   if(censored){
#     p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= COLORHEX, 
#                 cens.col = 'black', back.white = TRUE,
#                 size.est = 1 ) 
#   } else {
#     p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= COLORHEX,
#                 back.white = TRUE, size.est = 1) 
#   }
#   xmarks = seq(0, max(dat$project$maxday, 14), 7)
#   xmarks[seq(2,(2*trunc(length(xmarks)/2)),2)] = ""
#   arrdat = data.frame(x1=c(0,7,14),x2=c(0,7,14),y1=c(.2,.2,.2),y2=c(.4,.4,.4))
#   p1 = p1 + xlab("Days Post Treatment Initiation") + 
#     ylab("Survival Probability") +
#     theme(legend.title=element_blank(),
#           legend.position="none",
#           legend.key.width = unit(0.5, "cm"),
#           plot.title = element_text(size=12),
#           legend.text = element_text(size=12),
#           axis.text = element_text(size=12),
#           axis.title = element_text(size=12),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(), 
#           axis.line = element_line(colour = "black"),
#           panel.border = element_blank(),
#           axis.title.y=element_text(margin=margin(0,10,0,0)),
#           axis.title.x=element_text(margin=margin(10,0,0,0))) +
#     scale_x_continuous(breaks = seq(0, max(dat$project$maxday, 14), 7), labels = xmarks, expand = c(0,0)) + 
#     coord_cartesian(xlim = c(0,max(dat$project$maxday, 14))) +
#     scale_y_continuous(labels = scales::percent, 
#                        breaks = seq(0,1,0.25), expand = c(0,0), limits = c(0, 1.01))  
#   return(p1)
# }
# 
# # pipeline version
# volume.rtv.cd45.plot.kathryn <- function(dat, column, color=TRUE, font.factor = 1, line.factor = 1,
#                                  indiv.mouse = TRUE){
#   
#   maxday <- dat$project$maxday
#   names(COLORHEX) <- dat$groupInfo$group
#   numgroups <- length(dat$groupInfo$group)
#   
#   plotData <- dat$rawData[,c("mouseID", "group", "day", column)]
#   # exclude excluded mouse
#   plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
#   
#   # if less than half mouse has data at the given date, define the datapoint before it as last day
#   mouse.n <- aggregate(! is.na(plotData[[column]]), by=list(plotData$group, plotData$day), sum)
#   group.n <- aggregate(dat$formattedData$mouseID[! dat$formattedData$exclude], 
#                        by = list(dat$formattedData$group[! dat$formattedData$exclude]), length)
#   mouse.n <- merge(mouse.n, group.n, by = "Group.1", sort = FALSE)
#   mouse.n <- mouse.n[mouse.n$x.x > mouse.n$x.y/2,]
#   colnames(mouse.n) <- c("group", "day", "time.n", "group.n")
#   lastDay <- aggregate(as.numeric(mouse.n$day), by=list(mouse.n$group), max)
#   colnames(lastDay) <- c("group", "day")
#   
#   # subsetting medians till the last day
#   ### revision 20170519 ###
#   ### adding fake data for median calculation ###
#   dummyData <- plotData
#   dummyData$timepoint <- round(dummyData$day/3.5)
#   dat.time.match <- unique(dummyData[,c("group", "day", "timepoint")])
#   dummyData <- dcast(mouseID ~ timepoint, data = dummyData, value.var = column)
#   evented <- dat$formattedData$mouseID[dat$formattedData$survival.event == 1]
#   dummyData[dummyData$mouseID %in% evented,][is.na(dummyData[dummyData$mouseID %in% evented,])] <- 100
#   dummyData <- melt(data = dummyData, variable.name = "timepoint", value.name = column)
#   dummyData$group <- sapply(sapply(dummyData$mouseID, strsplit, ";"), function(x){x[[1]]})
#   dummyData <- merge(dummyData, dat.time.match, sort = FALSE)
#   dummyData$timepoint <- NULL
#   medians <- aggregate(dummyData[[column]], by=list(dummyData$group, dummyData$day), median, na.rm=T)
#   names(medians) <- c('group', 'day', column)
#   medians$keep <- medians$day <= lastDay$day[match(medians$group, lastDay$group)]
#   medians <- medians[medians$keep & !is.na(medians[[column]]), c('group', 'day', column)]
#   
#   # plot
#   if(indiv.mouse){
#     if(color){
#       p1 = ggplot(plotData,aes_string(x="day",y=column,group="mouseID")) + 
#         geom_line(aes(colour = group),alpha=0.3,size=1 * line.factor) + 
#         geom_line(data = medians, aes(group=group, color = group), size=1 * line.factor) +
#         scale_color_manual(values = COLORHEX)
#     } else{
#       p1 = ggplot(plotData,aes_string(x="day",y=column,group="mouseID")) + 
#         geom_line(aes(colour=group, linetype=group),alpha=0.2,size=1 * line.factor) +
#         scale_color_grey(start = 0.5, end = 0.7) + theme_bw() + 
#         geom_line(data = medians, aes(group=group, linetype=group), color = "black", size=1 * line.factor)
#     }
#   }
#   
#   if(column == "volume"){
#     p1 <- p1 + ylab('Volume (cm^3)') + ggtitle("Tumor Volume by Time")
#   } else if(column == "rtv"){
#     p1 <- p1 + ylab('RTV') + ggtitle("Relative Tumor Volume (RTV) by Time") + 
#       geom_abline(slope=0, intercept=4, colour='gray', size = 1 * line.factor)
#     if(max(dat$rawData$rtv, na.rm=T) > 8){
#       p1 <- p1 + coord_cartesian(ylim = c(0,8))
#     }
#   } else if(column == "cd45"){
#     p1 <- p1 + ylab("%huCD45+") + ggtitle("%huCD45+ by Time") +
#       geom_abline(slope=0, intercept=25, colour='black', size = 0.5 * line.factor, linetype = 2)
#   }
# 
#   p1 <- p1 + theme_bw() +
#     theme(legend.title=element_blank(), 
#           text = element_text(size=12 * font.factor),
#           plot.title = element_text(size=12 * font.factor),
#           legend.text = element_text(size=12 * font.factor),
#           axis.text = element_text(size=12 * font.factor),
#           axis.title = element_text(size=12 * font.factor),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(), 
#           axis.line = element_line(colour = "black"),
#           panel.border = element_blank(),
#           axis.title.y=element_text(margin=margin(0,10,0,0)),
#           axis.title.x=element_text(margin=margin(10,0,0,0))) 
#   p1 <- p1 + xlab('Days')  + scale_x_continuous(breaks = seq(0, max(dat$project$maxday, 42), 7), expand = c(0,0)) +
#     scale_y_continuous(breaks = seq(0, 50, 10), expand = c(0,0)) +
#     coord_cartesian(xlim = c(0,max(dat$project$maxday, 42)), ylim = c(0,50))
#     
#   return(p1)
# }
##################

# pipeline version
# EFS Survival plot.
# Input the entire experiment dat object
survival.plot.kathryn <- function(dat, censored = FALSE, color = TRUE, font.factor = 1, line.factor = 1){
  tmp <- dat$formattedData[ !dat$formattedData$exclude, ]
  if(is.null(dat$km)){
    dat$km <- km_fit(dat$formattedData[ !dat$formattedData$exclude, ])
  }
  # Survival plot
  maxday <- max(tmp$survival.time)
  # names(COLORHEX) <- dat$groupInfo$group
  numgroups <- length(dat$groupInfo$group)
  custom.colors<-COLORHEX[1:numgroups]
  names(custom.colors)<-unique(dat$formattedData$group)
  if(censored){
    if(color){
      
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
      p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= custom.colors,
                  back.white = TRUE, size.est = 1 * line.factor) 
    } else {
      p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= gray.colors(numgroups, start = 0, end = 0.6), 
                  lty.est = c(1:numgroups), back.white = TRUE,
                  size.est = 1 * line.factor) 
    }
  }
  
  p1 = p1 + xlab("Days") + ylab("Survival Probability") +
    ggtitle("Time to Event Plot") + 
    theme(legend.title=element_blank(), 
          legend.key.width = unit(0.5, "cm"),
          plot.title = element_text(size=12),
          legend.text = element_text(size=12),
          axis.text = element_text(size=12),
          axis.title = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          axis.title.y=element_text(margin=margin(0,10,0,0)),
          axis.title.x=element_text(margin=margin(10,0,0,0))) +
    scale_y_continuous(labels = scales::percent, 
                       breaks = seq(0,1,0.25), expand = c(0,0), limits = c(0, 1.01))
  maxday<-(dat$project$maxday)
  p1 = p1 + scale_x_continuous(breaks = seq(0, max(dat$project$maxday, 42), 7), expand = c(0,0))
 # if ( maxday < 42 ) {  p1 = p1 + scale_x_continuous(breaks = seq(0, max(dat$project$maxday, 42), 7), expand = c(0,0)) }
  #if ( (maxday >= 42) & (maxday < ) )
  
  if (maxday >= 70) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 14))}
  if (maxday >= 140) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 21))}
  if (maxday >= 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 28))}
 
 
    
  return(p1)
}


survival.plot.kathryn.single.mouse <- function(dat, 
                                               #censored = FALSE, 
                                               #color = TRUE, 
                                               font.factor = 1, 
                                               line.factor = 1){
  tmp <- dat$formattedData[ !dat$formattedData$exclude, ]
  # Survival plot
  maxday <- max(tmp$survival.time)
  # names(COLORHEX) <- dat$groupInfo$group
  numgroups <- length(dat$groupInfo$group)
  #custom.colors<-COLORHEX[1:numgroups]
  #names(custom.colors)<-unique(dat$formattedData$group)
  # if(censored){
  #   if(color){
  #     
  #     p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= custom.colors, 
  #                 cens.col = 'black', back.white = TRUE,
  #                 size.est = 1 * line.factor) 
  #   } else {
  #     p1 = ggsurv(dat$km, plot.cens = TRUE, surv.col= gray.colors(numgroups, start = 0, end = 0.6), 
  #                 cens.col = 'black', lty.est = c(1:numgroups), back.white = TRUE,
  #                 size.est = 1 * line.factor) 
  #   }
  # } else {
  #   if(color){
  #     p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= custom.colors,
  #                 back.white = TRUE, size.est = 1 * line.factor) 
  #   } else {
  #     p1 = ggsurv(dat$km, plot.cens = FALSE, surv.col= gray.colors(numgroups, start = 0, end = 0.6), 
  #                 lty.est = c(1:numgroups), back.white = TRUE,
  #                 size.est = 1 * line.factor) 
  #   }
  # }
  
  p1 = ggsurv(dat$km, back.white = TRUE, size.est = line.factor)
  
  
  p1 = p1 + xlab("Days") + ylab("Survival Probability") +
    ggtitle("Time to Event Plot") + 
    theme(legend.title=element_blank(), 
          legend.key.width = unit(0.5, "cm"),
          plot.title = element_text(size=12),
          legend.text = element_text(size=12),
          axis.text = element_text(size=12),
          axis.title = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          axis.title.y=element_text(margin=margin(0,10,0,0)),
          axis.title.x=element_text(margin=margin(10,0,0,0))) +
    scale_y_continuous(labels = scales::percent, 
                       breaks = seq(0,1,0.25), expand = c(0,0), limits = c(0, 1.01))
  maxday<-(dat$project$maxday)
  p1 = p1 + scale_x_continuous(breaks = seq(0, max(dat$project$maxday, 42), 7), expand = c(0,0))
  # if ( maxday < 42 ) {  p1 = p1 + scale_x_continuous(breaks = seq(0, max(dat$project$maxday, 42), 7), expand = c(0,0)) }
  #if ( (maxday >= 42) & (maxday < ) )
  
  if (maxday >= 70 & maxday < 140) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 14))}
  if (maxday >= 140 & maxday < 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 21))}
  if (maxday >= 210) {p1 = p1 + scale_x_continuous(breaks = seq(0, maxday*1.1, 28))}
  
  
  
  return(p1)
}



#### Aug 2018 Lock request ####
#custom color, formatting, etc.

##### HCD45 ####

# # hcd45 <- volume.rtv.cd45.plot.png.style(dat, "cd45", color = config$volume.color,
# #                                         font.factor = config$font.factor,
# #                                         line.factor = config$line.factor)
# maxday <- dat$project$maxday
# # names(COLORHEX) <- dat$groupInfo$group
# numgroups <- length(dat$groupInfo$group)
# 
# plotData <- dat$rawData[,c("mouseID", "group", "day", "cd45")]
# # exclude excluded mouse
# plotData <- plotData[ ! plotData$mouseID %in% dat$formattedData$mouseID[dat$formattedData$exclude], ]
# #medians <- dat[[paste("cd45", "median", sep=".")]]
# plotData$line.type<-1
# #plotData$line.type[grep("^A", plotData$mouseID)]<-1
# #plotData$line.type[grep("^B|^C|^D", plotData$mouseID)]<-1
# plotData$size<-1.5
# 
# #limit to days 0-28
# plotData.lim<-plotData[which (plotData$day <= 28),]
# 
# 
# #line.types.col<-plotData$line.type
# #line.types.four<-c(2,1,1,1)
# 
# #size.col<-plotData$size
# #size.four<-c(1.5,1.5,1.5,1.5)
# 
# column<-"cd45"
# 
# hcd45 = ggplot(plotData.lim,aes_string(x="day",y=column,group="mouseID")) + 
#   geom_line(aes(colour = group,linetype=factor(line.type)),
#             size=1.5) + 
#   scale_color_manual(values = COLORHEX) +
#   theme_bw() +
#   ylab(expression(bold(paste("% huCD45"^"+"~"in PB")))) +
#   scale_x_continuous(breaks=c(0,7,14,21,28,35,42),limits=c(0,42)) +
#   ylim(0,50)
# 
# hcd45 <- hcd45 + theme(axis.line= element_line(size=1,color="black"),
#                        axis.ticks= element_line(size=1,color="black"),
#                        axis.text.x = element_text(colour = "black",face="bold"),
#                        axis.text.y = element_text(colour = "black",face="bold"),
#                        axis.title.x = element_text(face="bold"),
#                        axis.title.y = element_text(face="bold"),
#                        panel.grid.major = element_blank(), 
#                        panel.grid.minor = element_blank(),
#                        legend.position="none",
#                        panel.border = element_blank())
# 
# 
# hcd45

