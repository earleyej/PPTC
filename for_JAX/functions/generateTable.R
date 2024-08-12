require(reshape2, quietly = TRUE, warn.conflicts = FALSE)

##### String formatting function #####
format.p = function(pval) {
  if (is.null(pval)) {return("NA")
  } else {
    if (is.na(pval)) {return("NA")
  } else {
    if (as.numeric(pval) < 0.001) {return("p < 0.001")
  } else {
    return(paste0("p = ", format(round(as.numeric(pval),3), trim=FALSE, nsmall=3)))
  }}}
}
format.day = function(day, maxday) {
  if (sum(is.na(day))>0) {
    maxday <- format(round(maxday,1),trim=FALSE, nsmall=1)
    return(sapply(day, function(x){if(is.na(x)){return(paste(">", maxday))}else{return(format(round(x,4), trim=FALSE, nsmall=4))}})  )
  } else {
    return(format(round(day,1), trim=FALSE, nsmall=1))
  }
}
format.percent <- function(decimal){
  return(paste( round(decimal * 100, 2), "%" ))
}
format.day.diff = function(m_c, m_t, maxday) {
  if(is.na(m_t)){
    if(is.na(m_c)){
      return("NA")
    }else{
      return(paste(">", round(maxday - m_c, 1)))
    }
  } else{
    if(is.na(m_c)){
      return(paste("<", round(m_t - maxday, 1)))
    }else {
      return(round(m_t - m_c, 1))      
    }
  }
}
format.day.ratio = function(m_c, m_t, maxday) {
  if(is.na(m_t)){
    if(is.na(m_c)){
      return("NA")
    }else{
      return(paste(">", round(maxday / m_c, 2)))
    }
  } else{
    if(is.na(m_c)){
      return(paste("<", round(m_t / maxday, 2)))
    }else {
      return(round(m_t / m_c, 2))
    }
  }
}
format.v = function(v) {
  return(format(round(v,3), trim=FALSE, nsmall=3))
}
format.mincd45.ratio = function(m_c, m_t) {
  return(round(m_t / m_c, 2))
}
#########################################
#####      Statistical Report       #####
#########################################

##### Sub-table formatting function #####
format.counts <- function(dat, config){
  output <- NULL
  get.counts.one.group <- function(dat){
    # N
    n <- dim(dat)[1]
    # Nd
    if(!is.null(dat$toxic)){
      nd <- sum(dat$toxic)
    } else {
      nd <- 0
    }
    # Nx
    if(!is.null(dat$exclude)){
      nx <- sum(dat$exclude) - nd
      tmp <- dat[ !dat$exclude, ]
    } else {
      nx <- 0
      tmp <- dat
    }
    
    # Na
    na <- dim(tmp)[1]
    # Ne
    ne <- sum(tmp$survival.event)
    
    if(config$tumor_panel == "solid" | config$tumor_panel == "ALL"){
      return(c(n, nd, nx, na, ne))
    } else {
      return(c(n, nx, na, ne))
    }
  }
  # control
  for(control in config$control){
    ctrl <- dat$formattedData[ dat$formattedData$group == control, ] #  & !dat$formattedData$exclude
    output <- rbind(output, get.counts.one.group(ctrl))
  }
  # treatment
  for(test in config$table.comparisons){
    if(test[3]){
      trt <- dat$formattedData[ dat$formattedData$group == test[2], ] #  & !dat$formattedData$exclude
      output <- rbind(output, get.counts.one.group(trt))
    } else {
      output <- rbind(output, "")
    }
  }
  return(output)
}

format.efs <- function(dat, config){
  output <- NULL
  
  get.efs.one.group <- function(dat, ctrl = FALSE, test){
    km.table <- summary(dat$km)$table
    if(ctrl != FALSE){
      return(c(format.day(km.table[paste0("factor(dat$group)=", ctrl), "median"], dat$project$maxday), "", "", "", ""))
    } else {
      if(is.null(test)){
        stop("Error while formatting EFS: unspecified test row.")
      }
      if(test[3]){
        return(c(format.day(km.table[paste0("factor(dat$group)=", test[2]), "median"], dat$project$maxday),
                 format.day.diff(km.table[paste0("factor(dat$group)=", test[1]), "median"], km.table[paste0("factor(dat$group)=", test[2]), "median"], dat$project$maxday),
                 format.day.ratio(km.table[paste0("factor(dat$group)=", test[1]), "median"], km.table[paste0("factor(dat$group)=", test[2]), "median"], dat$project$maxday),
                 format.p(dat$exact_rank_tests[[paste(test[1], test[2], sep=" vs ")]]),
                 format.p(dat$wilcoxon_tests[[paste(test[1], test[2], sep=" vs ")]])))
      } else {
        return(c("",
                 format.day.diff(km.table[paste0("factor(dat$group)=", test[1]), "median"], km.table[paste0("factor(dat$group)=", test[2]), "median"], dat$project$maxday),
                 format.day.ratio(km.table[paste0("factor(dat$group)=", test[1]), "median"], km.table[paste0("factor(dat$group)=", test[2]), "median"], dat$project$maxday),
                 format.p(dat$exact_rank_tests[[paste(test[1], test[2], sep=" vs ")]]),
                 format.p(dat$wilcoxon_tests[[paste(test[1], test[2], sep=" vs ")]])))
      }
    }
  }
  # control
  for(control in config$control){
    output <- rbind(output, get.efs.one.group(dat, ctrl = control))
    rownames(output)[dim(output)[1]] <- control
  }
  # treatment
  for(test in config$table.comparisons){
    output <- rbind(output, get.efs.one.group(dat, test = test))
    rownames(output)[dim(output)[1]] <- paste(test[1], test[2], sep=" vs ")
  }
  return(output)
}

{
  # format.tve <- function(dat, config){  # Tumor Volume Evaluation
  #   output <- NULL
  #   
  #   get.v0 <- function(dat, ctrl = FALSE, test){
  #     if(ctrl != FALSE){
  #       tmp <- dat$rawData[dat$rawData$group == ctrl, ]
  #       tmp <- tmp[tmp$day == min(tmp$day), ]
  #       return(paste0(format.v(mean(tmp$volume), na.rm=TRUE),
  #                     "\u00B1",
  #                     format.v(sd(tmp$volume), na.rm=TRUE)))
  #     } else {
  #       asdf
  #     }
  #   }
  #   # control
  #   for(control in config$control){
  #     output <- rbind(output, get.efs.one.group(dat, ctrl = control))
  #     rownames(output)[dim(output)[1]] <- control
  #   }
  #   # treatment
  #   for(test in config$table.comparisons){
  #     output <- rbind(output, get.efs.one.group(dat, test = test))
  #     rownames(output)[dim(output)[1]] <- paste(test[1], test[2], sep=" vs ")
  #   }
  #   return(output)
  # }
}

format.cd45 <- function(dat, config){
  output <- NULL
  
  get.hcd45.0 <- function(dat, group){
    tmp <- dat$rawData[dat$rawData$group == group & 
                         dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
    tmp <- tmp[tmp$day == min(tmp$day), ]
    return(paste0(format.v(mean(tmp$cd45, na.rm=TRUE)),
                  "\u00B1",
                  format.v(sd(tmp$cd45, na.rm=TRUE))))
  }
  
  get.hcd45.0.p <- function(dat, group1, group2){
    tmp <- dat$rawData[dat$rawData$group %in% c(group1, group2) & 
                         dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
    tmp <- tmp[tmp$day == min(tmp$day), ]
    return(format.p(wilcox.test(tmp$cd45 ~ as.factor(tmp$group))$p.value))
  }
  
  get.min.hcd45 <- function(dat, group){
    tmp <- dat$rawData[dat$rawData$group == group & 
                         dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
    tmp <- tmp[tmp$day != min(tmp$day), ]
    tmp <- aggregate(tmp$cd45, by = list(tmp$mouseID), min)
    names(tmp) <- c("mouseID", "cd45")
    return(paste0(format.v(mean(tmp$cd45, na.rm=TRUE)),
                  "\u00B1",
                  format.v(sd(tmp$cd45, na.rm=TRUE))))
  }
  
  get.min.hcd45.div.p <- function(dat, group1, group2){
    tmp <- dat$rawData[dat$rawData$group %in% c(group1, group2) & 
                         dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
    tmp <- tmp[tmp$day != min(tmp$day), ]
    tmp <- aggregate(tmp$cd45, by = list(tmp$mouseID, tmp$group), min)
    names(tmp) <- c("mouseID", "group", "cd45")
	  if(sum(tmp$cd45 != 0) == 0){
	  	return(c("NA", 1))
  	} else {
	      return(c(format.mincd45.ratio(mean(tmp$cd45[tmp$group == group1], na.rm=T), 
                                   mean(tmp$cd45[tmp$group == group2], na.rm=T)),
               format.p(wilcox.test(tmp$cd45 ~ as.factor(tmp$group))$p.value)))
	  }
  }
  
  get.delta.hcd45.baseline <- function(dat, group){
    tmp <- dat$rawData[dat$rawData$group == group & 
                         dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
    tmp <- dcast(tmp, mouseID ~ day, value.var = "cd45")
    if(dim(tmp)[2] == 2){
      stop("Only one time point available.")
    } else if(dim(tmp)[2] == 3){
      tmpData <- tmp[,3]
    } else {
      tmpData <- apply(tmp[,3:(dim(tmp)[2])], 1, min, na.rm=T)
    }
    tmpData[is.infinite(tmpData)] <- NA
    tmp <- tmpData / tmp[,2] - 1
    q <- quantile(tmp, probs=c(0.25,0.5,0.75), na.rm=T)
    return(paste0(format(round(q[2],2),nsmall=2),
                  '(',
                  format(round(q[1],2),nsmall=2),
                  ',',
                  format(round(q[3],2),nsmall=2),
                  ')'))
  }
  
  get.hcd45.one.group <- function(dat, ctrl = FALSE, test){
    if(ctrl != FALSE){
      return(c(get.hcd45.0(dat, ctrl), "", get.min.hcd45(dat, ctrl), "", "", get.delta.hcd45.baseline(dat, ctrl)))
    } else {
      if(is.null(test)){
        stop("Error while formatting EFS: unspecified test row.")
      }
      if(test[3]){
        return(c(get.hcd45.0(dat, group = test[2]), 
                 get.hcd45.0.p(dat, group1 = test[1], group2 = test[2]), 
                 get.min.hcd45(dat, group = test[2]), 
                 get.min.hcd45.div.p(dat, group1 = test[1], group2 = test[2]), 
                 get.delta.hcd45.baseline(dat, group = test[2])))
      } else{
        return(c("", 
                 get.hcd45.0.p(dat, group1 = test[1], group2 = test[2]), 
                 "", 
                 get.min.hcd45.div.p(dat, group1 = test[1], group2 = test[2]), 
                 ""))
      }
    }
  }
  
  # control
  for(control in config$control){
    output <- rbind(output, get.hcd45.one.group(dat, ctrl = control))
    rownames(output)[dim(output)[1]] <- control
  }
  # treatment
  for(test in config$table.comparisons){
    #test <- config$table.comparisons[[1]]
    output <- rbind(output, get.hcd45.one.group(dat, test = test))
    rownames(output)[dim(output)[1]] <- paste(test[1], test[2], sep=" vs ")
  }
  return(output)
}

format.ALL.response <- function(dat, config){
  output <- NULL
  
  format.ALL.response.one.group <- function(dat, group){
    # extract data for the group
    tmp <- dat$rawData[(dat$rawData$group == group) &
                         (dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude]), ]
    tmp <- dcast(tmp, mouseID ~ day, value.var = "cd45")
    tmp <- merge(tmp, dat$formattedData[! dat$formattedData$exclude,
                                        c("mouseID", "survival.time", "survival.event")], sort = FALSE)
    
    response <- data.frame(mouseID = tmp$mouseID, PD = "", PD1 = "", 
                           PD2 = "", SD = "", PR = "", CR = "", MCR = "")
    
    if(dim(tmp)[2] == 4){
      warning("Only one time point available.")
    } else if(dim(tmp)[2] == 5){
      tmp.min <- tmp[,3]
    } else {
      tmp.min <- apply(tmp[,3:(dim(tmp)[2]-2)], 1, min, na.rm=T)
      tmp.min[is.infinite(tmp.min)] <- NA
    }
    
    # median km for control group
    km.table <- summary(dat$km)$table
    ctrl.km.med <- km.table[paste0("factor(dat$group)=", config$control),"median"]
    
    # PD when hCD45 never < 1% during study period and mouse reaches event 
    # ( hCD45 > 25%) at some point during the study period
    response$PD <- (tmp.min >= 1) & (tmp$survival.event == 1)
    # PD1 when PD, and the mouse's time to event <= 200% the 
    #   KM median time-to-event in control group
    response$PD1 <- response$PD & (tmp$survival.time <= ctrl.km.med * 2) & (tmp$survival.event == 1)
    # PD2 when PD, but the mouse's time-to-event > 200% the 
    #   KM median time-to-event in control group
    response$PD2 <- response$PD & (tmp$survival.time > ctrl.km.med * 2)
    # SD when hCD45 never < 1% and mouse never reaches 
    #   event during the study period
    response$SD <- (tmp.min >= 1) & (tmp$survival.event == 0)
    
    # PR when hCD45 < 1% at least once during the study period, but not CR
    # CR when hCD45 < 1% for at least 2 consecutive weekly readings during the study period, 
    #   regardless of whether event is reached at a later time-point
    # MCR when hCD45 < 1% for at least 3 consecutive weekly readings 
    #   at any time after treatment has been completed
    if(dim(tmp)[2] <= 5){
      response$CR <- FALSE
      response$MCR <- FALSE
      response$PR <- tmp[,3] < 1
    } else {
      lessThanOnePercent <- tmp[,3:(dim(tmp)[2]-2)] < 1
      longCons <- apply(apply(lessThanOnePercent, 1, 
                              function(x){ 
                                x * unlist(lapply(rle(x)$lengths, seq_len)) 
                              }),
                        2, max, na.rm=T)
      response$CR <- longCons >= 2
      
      # determin the columns after treatment finishes
      tmp <- dat$rawData[dat$rawData$group == group & 
                           dat$rawData$measureDate > dat$rawData$treatment_end_date  &
                           (dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude]), ]
      if(length(unique(tmp$day)) < 3){
        response$MCR <- FALSE
      } else{
        tmp <- dcast(tmp, mouseID ~ day, value.var = "cd45")
        lessThanOnePercent <- tmp[,2:(dim(tmp)[2])] < 1
        longCons <- apply(apply(lessThanOnePercent, 1, 
                                function(x){ 
                                  x * unlist(lapply(rle(x)$lengths, seq_len)) 
                                }),
                          2, max, na.rm=T)
        response$MCR <- FALSE
        response$MCR[match(tmp$mouseID, response$mouseID)] <- longCons >= 3
      }
      
      # CR mutully exclusive with MCR
      response$CR <- response$CR & (!response$MCR)
      response$PR <- (tmp.min < 1) & (! response$CR) & (! response$MCR)
    }
    
    summaryResponse <- colSums(response[, c("PD", "PD1", "PD2", "SD", "PR", "CR", "MCR")], na.rm=T)
    tmp <- dat$formattedData[dat$formattedData$group == group,]
    tmp <- tmp[!tmp$exclude, ]
    summaryResponse["RespRate"] = paste(round(100*(
      (summaryResponse["PR"]+summaryResponse["CR"]+summaryResponse["MCR"])/
        (dim(tmp)[1])),0),"%",sep="")
    
    if(group %in% config$control){
      summaryResponse["PD1"] <- ""
      summaryResponse["PD2"] <- ""
    }
    
    # median response evaluation - how is this defined ?
    mouseScore <- data.frame(all_mouse = response$mouseID,
                             score = 0)
    mouseScore$score[response$PD1] <- 0
    mouseScore$score[response$PD2] <- 2
    mouseScore$score[response$SD] <- 4
    mouseScore$score[response$PR] <- 6
    mouseScore$score[response$CR] <- 8
    mouseScore$score[response$MCR] <- 10
    groupScore <- median(mouseScore$score, na.rm=T)
    if(groupScore == 10){
      groupCategory <- "MCR"
    } else if (groupScore < 10 & groupScore >= 8){
      groupCategory <- "CR"
    } else if (groupScore < 8 & groupScore >= 6){
      groupCategory <- "PR"
    } else if (groupScore < 6 & groupScore >= 4){
      groupCategory <- "SD"
    } else if (groupScore < 4 & groupScore >= 2){
      groupCategory <- "PD2"
    } else if (groupScore < 2){
      groupCategory <- "PD1"
    }
    
    if(group %in% config$control){
      summaryResponse["PD1"] <- ""
      summaryResponse["PD2"] <- ""
    }
    
    return(c(summaryResponse, groupCategory))
  }
  
  # control
  for(control in config$control){
    output <- rbind(output, format.ALL.response.one.group(dat, group = control))
    rownames(output)[dim(output)[1]] <- control
  }
  # treatment
  for(test in config$table.comparisons){
    if(test[3]){
      output <- rbind(output, format.ALL.response.one.group(dat, group = test[2]))
      
    } else {
      output <- rbind(output, rep("", 9))
    }
    rownames(output)[dim(output)[1]] <- paste(test[1], test[2], sep=" vs ")
  }
  return(output)
}

format.volume <- function(dat, config){
  output <- NULL
  
  get.volume.0 <- function(dat, group){
    tmp <- dat$rawData[dat$rawData$group == group & 
                         dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
    tmp <- tmp[tmp$day == min(tmp$day), ]
    return(paste0(format.v(mean(tmp$volume, na.rm=TRUE)),
                  "\u00B1",
                  format.v(sd(tmp$volume, na.rm=TRUE))))
  }
  
  get.volume.0.p <- function(dat, group1, group2){
    tmp <- dat$rawData[dat$rawData$group %in% c(group1, group2) & 
                         dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
    # this will fail if there is no day 0 for a group
    #tmp <- tmp[abs(tmp$day - min(tmp$day)) <= 1, ]
    
    # edit - Nov 13, 2020 - Eric Earley
    # pull out the rows with the smalled day for each group
    for.test<-NULL
    group1.min.day <- min(tmp[tmp$group == group1,"day"])
    group2.min.day <- min(tmp[tmp$group == group2,"day"])
    for.test<-rbind(for.test,tmp[tmp$group == group1 & tmp$day == group1.min.day,])
    for.test<-rbind(for.test,tmp[tmp$group == group2 & tmp$day == group2.min.day,])
    
    return(format.p(wilcox.test(for.test$volume ~ as.factor(for.test$group))$p.value))
  }
  
  get.min.rtv <- function(dat, group){
    tmp <- dat$rawData[dat$rawData$group == group & 
                         dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
    tmp <- tmp[tmp$day != min(tmp$day), ]
    tmp <- aggregate(tmp$rtv, by = list(tmp$mouseID), min)
    names(tmp) <- c("mouseID", "rtv")
    return(paste0(format.v(mean(tmp$rtv, na.rm=TRUE)),
                  "\u00B1",
                  format.v(sd(tmp$rtv, na.rm=TRUE))))
  }
  
  get.min.rtv.p <- function(dat, group1, group2){
    tmp <- dat$rawData[dat$rawData$group %in% c(group1, group2) & 
                         dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
    tmp <- tmp[tmp$day != min(tmp$day), ]
    tmp <- aggregate(tmp$rtv, by = list(tmp$mouseID, tmp$group), min)
    names(tmp) <- c("mouseID", "group", "rtv")
    return(format.p(wilcox.test(tmp$rtv ~ as.factor(tmp$group))$p.value))
  }
  
  get.volume.one.group <- function(dat, ctrl = FALSE, test){
    if(ctrl != FALSE){
      return(c(get.volume.0(dat, ctrl), "", get.min.rtv(dat, ctrl), ""))
    } else {
      if(is.null(test)){
        stop("Error while formatting EFS: unspecified test row.")
      }
      if(test[3]){
        return(c(get.volume.0(dat, group = test[2]), 
                 get.volume.0.p(dat, group1 = test[1], group2 = test[2]), 
                 get.min.rtv(dat, group = test[2]), 
                 get.min.rtv.p(dat, group1 = test[1], group2 = test[2])))
      } else{
        return(c("", 
                 get.volume.0.p(dat, group1 = test[1], group2 = test[2]), 
                 "", 
                 get.min.rtv.p(dat, group1 = test[1], group2 = test[2])))
      }
    }
  }
  
  # control
  for(control in config$control){
    output <- rbind(output, get.volume.one.group(dat, ctrl = control))
    rownames(output)[dim(output)[1]] <- control
  }
  # treatment
  for(test in config$table.comparisons){
    output <- rbind(output, get.volume.one.group(dat, test = test))
    rownames(output)[dim(output)[1]] <- paste(test[1], test[2], sep=" vs ")
  }
  return(output)
}

format.ST.response <- function(dat, config){
  output <- NULL
  
  format.ST.response.one.group <- function(dat, group){
    # extract data for the group
    rawData.group <- dat$rawData[dat$rawData$group == group & 
                                   ! dat$rawData$mouseID %in% dat$formattedData$mouseID[ dat$formattedData$exclude ], ]
    reshape.rtv <- dcast(rawData.group, mouseID ~ day, value.var = "rtv")
    reshape.v <- dcast(rawData.group, mouseID ~ day, value.var = "volume")
    reshape.rtv <- merge(reshape.rtv, dat$formattedData[,c("mouseID", "survival.time", "survival.event")], sort = FALSE)
    reshape.v <- merge(reshape.v, dat$formattedData[,c("mouseID", "survival.time", "survival.event")], sort = FALSE)
    
    response <- data.frame(mouseID = reshape.rtv$mouseID, PD = NA, PD1 = NA, 
                           PD2 = NA, SD = NA, PR = NA, CR = NA, MCR = NA,
                           stringsAsFactors = FALSE)
    
    if(dim(reshape.v)[2] == 4){
      warning("Only one time point available.")
    } else if(dim(reshape.v)[2] == 5){
      reshape.rtv.min <- reshape.rtv[,3]
      reshape.rtv.last <- reshape.rtv[,3]
    } else {
      reshape.rtv.min <- apply(reshape.rtv[,3:(dim(reshape.rtv)[2]-2)], 1, min, na.rm=T)
      reshape.rtv.min[is.infinite(reshape.rtv.min)] <- NA
      reshape.rtv.last <- apply(reshape.rtv[,3:(dim(reshape.rtv)[2]-2)], 1, 
                                function(x){tmp <- x[!is.na(x)]; tmp[length(tmp)]})
    }
    
    # median km for control group
    km.table <- summary(dat$km)$table
    ctrl.km.med <- km.table[paste0("factor(dat$group)=", config$control),"median"]
    
    # PD when <50% tumor regression throughout study and 
    #   >25% tumor growth at end of study,
    response$PD <- (reshape.rtv.min >= 0.5) & (reshape.rtv.last > 1.25)
    
    # PD1 when PD and the mouse's time to event <= 200% 
    #   the KM median time-to-event in control group,
    response$PD1 <- response$PD & (reshape.rtv$survival.time <= ctrl.km.med * 2) & (reshape.rtv$survival.event == 1)
    
    # PD2 when PD but, additionally, time-to-event is > 200% 
    #   of the Kaplan-Meier (KM) median time-to-event in control group,
    response$PD2 <- response$PD & (reshape.rtv$survival.time > ctrl.km.med * 2)
    
    # SD when <50% tumor regression throughout study and 
    #   <= 25% tumor growth at end of study,
    response$SD <- (reshape.rtv.min >= 0.5) & (reshape.rtv.last <= 1.25)
    
    # PR when >= 50% tumor regression at any point during study, 
    #   but measurable tumor throughout study period,
    # CR when disappearance of measurable tumor mass during the study period, and
    # MCR when no measureable tumor mass for at least 3 consecutive weekly 
    #   readings at any time after treatment has been completed.
    if(dim(reshape.v)[2] <= 5){
      if(dim(reshape.v)[2] <= 4){stop("Only one time point available.")}
      response$CR <- FALSE
      response$MCR <- FALSE
      response$PR <- (reshape.rtv[,3] < 0.5) & (reshape.v[,3] > 5.24E-07)
    } else {
      lessThanMeasurable <- reshape.v[,3:(dim(reshape.v)[2]-2)] <= 5.24E-07
      response$CR <- rowSums(lessThanMeasurable, na.rm=T) > 0
      
      # determin the columns after treatment finishes
      aft.trt <- dat$rawData[dat$rawData$group == group & 
                               dat$rawData$measureDate > dat$rawData$treatment_end_date &
                               ! dat$rawData$mouseID %in% dat$formattedData$mouseID[ dat$formattedData$exclude ], ]
      if(length(unique(aft.trt$day)) < 3){
        response$MCR <- FALSE
      } else{
        # MCR
        aft.trt.v <- dcast(aft.trt, mouseID ~ day, value.var = "volume")
        lessThanMeasurable <- aft.trt.v[,2:(dim(aft.trt.v)[2])] <= 5.24E-07
        ### old scripts ###
        # longCons <- apply(apply(lessThanMeasurable, 1, 
        #                         function(x){ 
        #                           x * unlist(lapply(rle(x)$lengths, seq_len)) 
        #                         }),
        #                   2, max, na.rm=T)
        # response$MCR[match(aft.trt.v$mouseID, response$mouseID)] <- longCons >= 3
        ### old scripts ###
        longCons <- apply(lessThanMeasurable, 1, function(x){
          days <- as.numeric(names(x))
          cons.true = FALSE
          for(d in days){
            if(d <= max(days) - 15){
              tmp.cons <- x[which(days >= d & days <= d + 15)]
              if(sum(tmp.cons, na.rm=T) == length(tmp.cons)){
                cons.true = TRUE
              }
            }
          }
          return(cons.true)
        })
        response$MCR[match(aft.trt.v$mouseID, response$mouseID)] <- longCons
      }
      response$MCR[is.na(response$MCR) | response$MCR == ""] <- FALSE
      
      # CR mutully exclusive with MCR
      response$CR <- response$CR & (! response$MCR)
      response$PR <- (reshape.rtv.min < 0.5) & 
        (rowSums(reshape.v[,3:(dim(reshape.v)[2]-2)] < 5.24E-07, 
                 na.rm = TRUE) == 0) & 
        (! response$CR) & (! response$MCR)
    }
    
    summaryResponse <- colSums(response[, c("PD", "PD1", "PD2", "SD", "PR", "CR", "MCR")], na.rm=T)
    tmp <- dat$formattedData[dat$formattedData$group == group,]
    tmp <- tmp[!tmp$exclude, ]
    summaryResponse["RespRate"] = paste(round(100*(
      (summaryResponse["PR"]+summaryResponse["CR"]+summaryResponse["MCR"])/
        (dim(tmp)[1])),0),"%",sep="")
    
    if(group %in% config$control){
      summaryResponse["PD1"] <- ""
      summaryResponse["PD2"] <- ""
    }
    
    # median response evaluation - how is this defined ?
    mouseScore <- data.frame(mouseID = response$mouseID,
                             score = 0)
    mouseScore$score[response$PD1] <- 0
    mouseScore$score[response$PD2] <- 2
    mouseScore$score[response$SD] <- 4
    mouseScore$score[response$PR] <- 6
    mouseScore$score[response$CR] <- 8
    mouseScore$score[response$MCR] <- 10
    groupScore <- median(mouseScore$score, na.rm=T)
    if(groupScore == 10){
      groupCategory <- "MCR"
    } else if (groupScore < 10 & groupScore >= 8){
      groupCategory <- "CR"
    } else if (groupScore < 8 & groupScore >= 6){
      groupCategory <- "PR"
    } else if (groupScore < 6 & groupScore >= 4){
      groupCategory <- "SD"
    } else if (groupScore < 4 & groupScore >= 2){
      groupCategory <- "PD2"
    } else if (groupScore < 2){
      groupCategory <- "PD1"
    }
    if(group %in% config$control & groupCategory %in% c("PD1", "PD2")){
      groupCategory <- "PD"
    }
    return(c(summaryResponse, groupCategory))
  }
  
  # control
  for(control in config$control){
    output <- rbind(output, format.ST.response.one.group(dat, group = control))
    rownames(output)[dim(output)[1]] <- control
  }
  # treatment
  for(test in config$table.comparisons){
    if(test[3]){
      output <- rbind(output, format.ST.response.one.group(dat, group = test[2]))
      
    } else {
      output <- rbind(output, rep("", 9))
    }
    rownames(output)[dim(output)[1]] <- paste(test[1], test[2], sep=" vs ")
  }
  return(output)
}

format.toxicity <- function(dat, config){
  output <- NULL
  
  tmp <- aggregate(dat$weight$weight, by=list(dat$weight$group, dat$weight$days), mean, na.rm=TRUE)
  colnames(tmp) <- c("Group", "Days", "Weight")
  tmp2 <- aggregate(dat$weight$weight, by=list(dat$weight$group, dat$weight$days), length)
  colnames(tmp2) <- c("Group", "Days", "Measured_mice")
  tmp <- merge(tmp, tmp2, sort = FALSE)
  
  format.toxicity.one.group <- function(tmp, dat, group){
    this.group <- tmp[tmp$Group == group, ]
    N <- dim(dat$formattedData[ dat$formattedData$group == group, ])[1]
    #this.group <- this.group[this.group$Measured_mice >= 0.75 * N,]
    return(format.percent( 1 - min(this.group$Weight,na.rm=T) / this.group$Weight[1])) # added 'na.rm=T' to account for NAs in weight (e.g. "found dead" instead of a number)
  }
  
  # control
  for(control in config$control){
    output <- rbind(output, format.toxicity.one.group(tmp, dat, group = control))
    rownames(output)[dim(output)[1]] <- control
  }
  # treatment
  for(test in config$table.comparisons){
    if(test[3]){
      output <- rbind(output, format.toxicity.one.group(tmp, dat, group = test[2]))
    } else {
      output <- rbind(output, "")
    }
    
    rownames(output)[dim(output)[1]] <- paste(test[1], test[2], sep=" vs ")
  }
  return(output)
}


format.ALL.toxicity <- function(dat, config){
  output <- NULL
  
  ##per mouse
  #convert all weights to relative weights. (That is, for each mouse, divide weights by weight at baseline.) 
  #Then compute the average RW at each timepoint. Then choose the timepoint with the minimum value. 
  #Finally, convert to max avg % weight loss by multiplying by 100 then subtracting from 100
  for (control in config$control) {
    max_pct_loss <- 0
    tmp1 <- dat$weight[dat$weight$group == control,]
    tmp1$rel.weight <- NA
    #convert to relative weights per mouse within group
    for (mouse in unique(dat$weight[dat$weight$group == control,"mouseID"])) {
      tmp2 <- dat$weight[dat$weight$group == control & dat$weight$mouseID == mouse,]
      tmp2$rel.weight <- tmp2$weight / tmp2$weight[1]
      tmp1[tmp1$group == control & tmp1$mouseID == mouse,"rel.weight"] <- tmp2$rel.weight
    }
    #avereage of relative weights per day
    tmp3<-aggregate(tmp1$rel.weight, by=list(tmp1$days), mean, na.rm=TRUE)
    #find minimum avg relative weight and convert to %loss
    max_pct_loss <- 1 - min(tmp3[,2])
    #format and output
    output<-rbind(output,
                  format.percent(max_pct_loss))
    rownames(output)[dim(output)[1]] <- control
    
  }
  
 
  for (test in config$table.comparisons) {
    
    if(test[3]){
      
      group = test[2]
      max_pct_loss <- 0
      tmp1 <- dat$weight[dat$weight$group == group,]
      tmp1$rel.weight <- NA
      #convert to relative weights per mouse within group
      for (mouse in unique(dat$weight[dat$weight$group == group,"mouseID"])) {
        tmp2 <- dat$weight[dat$weight$group == group & dat$weight$mouseID == mouse,]
        tmp2$rel.weight <- tmp2$weight / tmp2$weight[1]
        tmp1[tmp1$group == group & tmp1$mouseID == mouse,"rel.weight"] <- tmp2$rel.weight
      }
      #avereage of relative weights per day
      tmp3<-aggregate(tmp1$rel.weight, by=list(tmp1$days), mean, na.rm=TRUE)
      #find minimum avg relative weight and convert to %loss
      max_pct_loss <- 1 - min(tmp3[,2])
      output<-rbind(output,
                    format.percent(max_pct_loss))
      
    } else {
      output <- rbind(output, "")
    }
    
    rownames(output)[dim(output)[1]] <- paste(test[1], test[2], sep=" vs ")    
  }
 
  return(output)
}
##### entire table formatting function #####
format.CNS.table <- function(dat, config, toxicity_eval = TRUE){
  output <- matrix("", length(config$control) + length(config$table.comparisons), 15)
  # study
  output[1,1] <- paste(dat$project$agent_code, dat$project$agent_name, sep="-")
  # tumor
  output[1,2] <- dat$project$tumor_model
  # group
  tmp <- config$control
  for(test in config$table.comparisons){
    if(test[3]){
      tmp <- c(tmp, test[2])
    } else {
      tmp <- c(tmp, paste(test[1], test[2], sep=" vs "))
    }
  }
  output[,3] <- tmp
  # count
  output[,4:7] <- format.counts(dat, config)
  # efs
  output[,9:13] <- format.efs(dat, config)
  
  if(toxicity_eval){
    output[,15] <- format.toxicity(dat, config)
  }
  
  return(output)
}
format.CNS.table2 <- function(dat, config){
	# Feb 2021 - Eric Earley
	# anticipating end of PPTC this will create the final stats summary table for the experiment.
	# then will be consolidated into one big file
  
  

  # 
  output <- matrix("", length(config$control) + length(config$table.comparisons), 19)
  
  colnames(output) <- c("Panel_code", #1
                        "Agent_code", #2
                        "Agent_name", #3
                        "Agent_class", #4
                        "Chembl_ID", #5
                        "Drugbank_ID", #6
                        "Agent_dose", #7
                        "Agent_schedule", #8
                        "Model", #9
                        "Group", #10
                        "N", #11
                        "N_deaths",#12
                        "N_analyzed", #13
                        "N_evented", #14
                        "KM_median", #15
                        "EFS_T-C", #16
                        "EFS_T/C", #17
                        "p_exact_log_rank_efs", #18
                        "p_gehan_wilcoxon_efs") #19
                       
  
  # experiment/panel code
  output[,1] <- dat$project$panel_code
  
  # agent code
  output[,2] <- dat$project$agent_code
  
  # agent name
  agent.dict <- read.csv(paste0(resources.dir,"PPTC_Agents_v1.2.csv"),header=T,stringsAsFactors = F)
  colnames(agent.dict)[1] <- "agent_number"
  for (i in 1:length(dat$groupInfo$group)) {
	if (i == 1) {
		output[i,3] <- "Control"
	} else if ( i >= 2) {
		output[i,3] <- agent.dict[agent.dict$agent_number == as.integer(dat$project$agent_code),"agent_name"]
	} #else {
		# group B is usually agent code (e.g. 1607)
		# but groups C and on are usually strings (e.g. disatinib), so no need to look the name up!
		# or possibly dose responses of main agent
		#output[i,3] <- dat$groupInfo$drug[i]
	#}
  
  
  
  }
    
  # agent class
  output[1,4] <- NA
  output[2,4] <- agent.dict[agent.dict$agent_number == as.integer(dat$project$agent_code),"agent_class"]
  

  # chembl ID
  output[1,5] <- NA
  output[2,5] <- agent.dict[agent.dict$agent_number == as.integer(dat$project$agent_code),"CHEMBLID"]
  
  # drugbank ID
  output[1,6] <- NA
  output[2,6] <- agent.dict[agent.dict$agent_number == as.integer(dat$project$agent_code),"DRUGBANKID"]
  
  # agent dose
  output[1,7] <- dat$groupInfo$dose[1]
  output[2,7] <- dat$groupInfo$dose[2]
  if (length(dat$groupInfo$group) > 2) {
	  for (i in 3:length(dat$groupInfo$group)) {
		output[i,7] <- dat$groupInfo$dose[i]
	  }
  }
  # agent schedule
  output[1,8] <- dat$groupInfo$dose_schedule[1]
  output[2,8] <- dat$groupInfo$dose_schedule[2]
  if (length(dat$groupInfo$group) > 2) {
	  for (i in 3:length(dat$groupInfo$group)) {
		output[i,8] <- dat$groupInfo$dose_schedule[i]
	  }
  }
  
  # model
  output[,9] <- dat$project$tumor_model
  
  
  # group
  tmp <- config$control
  for(test in config$table.comparisons){
    if(test[3]){
      tmp <- c(tmp, test[2])
    } else {
      tmp <- c(tmp, paste(test[1], test[2], sep=" vs "))
    }
  }
  output[,10] <- tmp
  
  # counts
  output[,11:14] <- format.counts(dat, config)
  
  
  # efs
  #output[,10:14] <- format.efs(dat, config)
  output[,15:19] <- format.efs(dat, config)
  
  # aoc
  #library(dplyr)
  #suppressWarnings({tmp<-dat$formattedData %>%
  #  group_by(group) %>%
  #  summarize(median = median ( as.numeric(aoc) ) )})
  #output[1:length(unique(dat$formattedData$group)),21] <-format(round(tmp$median, 1), nsmall = 1)  
  
  
  # this will fail if N=1
  # tumor volume evaluation
  #output[,22:25] <- format.volume(dat, config)
  # response evaluation
  #output[,26:34] <- format.ST.response(dat, config)
  
  
  
  #output[,35] <- format.toxicity(dat, config)
  
  return(output)
}










format.ALL.table <- function(dat, config){
  output <- matrix("", length(config$control) + length(config$table.comparisons), 34)
  # study
  output[1,1] <- dat$project$panel_code # paste(dat$project$agent_code, dat$project$agent_name, sep="-")
  # tumor
  output[1,2] <- dat$project$tumor_model
  # group
  tmp <- config$control
  for(test in config$table.comparisons){
    if(test[3]){
      tmp <- c(tmp, test[2])
    } else {
      tmp <- c(tmp, paste(test[1], test[2], sep=" vs "))
    }
  }
  output[,3] <- tmp
  # count
  output[,4:8] <- format.counts(dat, config)
  # efs
  output[,10:14] <- format.efs(dat, config)
  # aoc
  library(dplyr)
  tmp<-dat$formattedData %>%
    group_by(group) %>%
    summarize(median = median ( as.numeric(aoc) ) )
  output[1:length(unique(dat$formattedData$group)),15] <-format(round(tmp$median, 1), nsmall = 1)  
  
  # %hcd45
  if (is.null(config$SMT)) {config$SMT = FALSE}
  if (config$SMT == FALSE) {
    output[,17:22] <- format.cd45(dat, config)
  }
  # response evaluation
  output[,24:32] <- format.ALL.response(dat, config)
  
  
  # weight should be calculated on a per-mouse basis
  if (is.null(dat$weight)) {
    output[,34] <- NA
  } else {
    #output[,34] <- format.toxicity(dat, config)  
    output[,34] <- format.ALL.toxicity(dat, config)  
    
    
  }
  
  
  return(output)
}



format.ALL.table2 <- function(dat, config){
  # Jan 2021 - Eric Earley
  # anticipating end of PPTC this will create the final stats summary table for the experiment.
  # then will be consolidated into one big ALL file
  
  

  # 39 columns
  output <- matrix("", length(config$control) + length(config$table.comparisons), 39)
  
  colnames(output) <- c("Panel_code", #1
                        "Agent_code", #2
                        "Agent_name", #3
                        "Agent_class", #4
                        "Chembl_ID", #5
                        "Drugbank_ID", #6
                        "Agent_dose", #7
                        "Agent_schedule", #8
                        "Model", #9
                        "Group", #10
                        "N", #11
                        "N_deaths",#12
                        "N_toxic_deaths", #13
                        "N_analyzed", #14
                        "N_evented", #15
                        "KM_median", #16
                        "EFS_T-C", #17
                        "EFS_T/C", #18
                        "p_exact_log_rank_efs", #19
                        "p_gehan_wilcoxon_efs",
                        "AOC_median",
                        "Baseline_CD45_SD",
                        "p_baseline_CD45",
                        "minCD45_mean_SD",
                        "minCD45_T/C",
                        "p_minCD45_mean",
                        "deltaCD45/baseline_Median(IQR)",
                        "PD",
                        "PD1",
                        "PD2",
                        "SD",
                        "PR",
                        "CR",
                        "MCR",
                        "Resp_rate",
                        "Med_resp",
                        "Max_avg_pct_weight_loss",
						"Tx_start", #earliest treatment initiation
						"Exp_end") #last date of experiment
  
  # experiment/panel code
  output[,1] <- dat$project$panel_code
  
  # agent code
  if (grepl("1...",dat$groupInfo$drug[2])) {
	output[,2] <- dat$groupInfo$drug[2]
  } else {
	output[,2] <- ""
  }
  
  
  # agent name
  agent.dict <- read.csv(paste0(resources.dir,"PPTC_Agents_v1.2.csv"),header=T,stringsAsFactors = F)
  colnames(agent.dict)[1] <- "agent_number"
  for (i in 1:length(dat$groupInfo$group)) {
	if (i == 1) {
		output[i,3] <- "Control"
	#} else if ( i == 2) {
	} else if ( grepl("1...$",dat$groupInfo$drug[i],ignore.case=T)) {
		output[i,3] <- agent.dict[agent.dict$agent_number == as.integer(dat$groupInfo$drug[i]),"agent_name"]
	} else {
		# group B is usually agent code (e.g. 1607)
		# but groups C and on are usually strings (e.g. disatinib), so no need to look the name up!
		output[i,3] <- dat$groupInfo$drug[i]
	}
  
  
  
  }
  
  
  
  
  # agent class
  for (i in 1:length(dat$groupInfo$group)) {
	if (i == 1) {
		output[i,4] <- NA	
	} else if ( grepl("1...$",dat$groupInfo$drug[i],ignore.case=T)) {
		output[i,4] <- agent.dict[agent.dict$agent_number == as.integer(dat$groupInfo$drug[i]),"agent_class"]
	} 
  }

  # chembl ID
  for (i in 1:length(dat$groupInfo$group)) {
	if (i == 1) {
		output[1,5] <- NA
	} else if ( grepl("1...$",dat$groupInfo$drug[i],ignore.case=T)) {
		output[i,5] <- agent.dict[agent.dict$agent_number == as.integer(dat$groupInfo$drug[i]),"CHEMBLID"]
	}
  }
  
  # drugbank ID
  for (i in 1:length(dat$groupInfo$group)) {
	if (i == 1) {
		output[1,6] <- NA
	} else if (grepl("1...$",dat$groupInfo$drug[i],ignore.case=T)) {
		output[i,6] <- agent.dict[agent.dict$agent_number == as.integer(dat$groupInfo$drug[i]),"DRUGBANKID"]
	}
  }
  
  # agent dose
  output[1,7] <- dat$groupInfo$dose[1]
  output[2,7] <- dat$groupInfo$dose[2]
  if (length(dat$groupInfo$drug) > 2) {
	  for (i in 3:length(dat$groupInfo$drug)) {
		output[i,7] <- dat$groupInfo$dose[i]
	  }
  }
  # agent schedule
  output[1,8] <- dat$groupInfo$schedule[1]
  output[2,8] <- dat$groupInfo$schedule[2]
  if (length(dat$groupInfo$drug) > 2) {
	  for (i in 3:length(dat$groupInfo$drug)) {
		output[i,8] <- dat$groupInfo$schedule[i]
	  }
  }
  
  # model
  output[,9] <- dat$project$tumor_model
  
  
  # group
  tmp <- config$control
  for(test in config$table.comparisons){
    if(test[3]){
      tmp <- c(tmp, test[2])
    } else {
      tmp <- c(tmp, paste(test[1], test[2], sep=" vs "))
    }
  }
  output[,10] <- tmp
  
  # counts
  output[,11:15] <- format.counts(dat, config)
  
  
  # efs
  #output[,10:14] <- format.efs(dat, config)
  output[,16:20] <- format.efs(dat, config)
  
  # aoc
  library(dplyr)
  suppressWarnings({tmp<-dat$formattedData %>%
    group_by(group) %>%
    summarize(median = median ( as.numeric(aoc) ) )})
  output[1:length(unique(dat$formattedData$group)),21] <-format(round(tmp$median, 1), nsmall = 1)  
  
  # %hcd45
  if (is.null(config$SMT)) {config$SMT = FALSE}
  if (config$SMT == FALSE) {
    suppressWarnings({output[,22:27] <- format.cd45(dat,config)})
  }
  
  # response evaluation
  #output[,24:32] <- format.ALL.response(dat, config)
  output[,28:36] <- format.ALL.response(dat, config)
  
  # weight 
  if (is.null(dat$weight)) {
    output[,37] <- NA
  } else {
    #output[,34] <- format.toxicity(dat, config)  
    output[,37] <- format.ALL.toxicity(dat, config) #should be calculated on a per-mouse basis
    
    
  }
  
  #Tx_start
  output[,38] <- as.character(min(dat$rawData$measureDate))
  
  #Exp_end
  output[,39] <- as.character(max(dat$rawData$measureDate))
  
  return(output)
}






format.ALL.single.mouse.table <- function(dat, config){
  # 6 columns
  # print all tested mice, but need to address excluded mice
  #   for now, exluded mice get NA for all rows except group/pdx_id
  
  
  output <- matrix("", length(unique(dat$formattedData$group)), 6)
  #group/pdx_id
  output[,1]<-dat$formattedData$group
  #survival.time
  output[,2]<-ifelse(dat$formattedData$exclude == FALSE,
                     format(round(dat$formattedData$survival.time,2),nsmall=2),
                     "NA")
  #cd45 baseline
  output[,3]<-ifelse(dat$formattedData$exclude == FALSE,
                     format(round(dat$rawData$cd45[dat$rawData$day <= 0],2),nsmall=1),
                     "NA")
  #cd45 min
  output[,4]<-ifelse(dat$formattedData$exclude == FALSE,
###                     format(round(tapply(dat$rawData$cd45[dat$rawData$day>0],dat$rawData$group[dat$rawData$day>0],min)[order = dat$formattedData$group],2),nsmall=1),
					 format(round(tapply(dat$rawData$cd45[dat$rawData$day>0],dat$rawData$group[dat$rawData$day>0],min)[order = dat$formattedData$group],2),nsmall=1),
                     "NA")
  #cd45 (min - baseline)/baseline
  #output[,5]<- ifelse(!is.na(output[,4]),(output[,4] - output[,3]) / output[,3],NA)
  output[,5] <- format(round(    ((as.numeric(output[,4]) - as.numeric(output[,3])) / as.numeric(output[,3])), 2), nsmall=2)
  
  
  
  calc.OR.single.mouse <- function(dat,group) {
    tmp <- dat$rawData[(dat$rawData$group == group) &
                         (dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude]), ]
	tx_dur = as.numeric(dat$rawData$treatment_end_date[1]) - as.numeric(dat$rawData$measureDate[1]) - as.numeric(dat$rawData$day[1])
    tmp <- dcast(tmp, mouseID ~ day, value.var = "cd45")
    tmp <- merge(tmp, dat$formattedData[! dat$formattedData$exclude,
                                        c("mouseID", "survival.time", "survival.event")], sort = FALSE)
    # if tmp has 5 columns, then mouse evented at the first timepoint. It's PD.
    # Otherwise, check for consecutive instances of %huCD45 < 1%
    if (dim(tmp)[2] > 5) {
      tmp.min <- apply(tmp[,3:(dim(tmp)[2]-2)], 1, min, na.rm=T)
      tmp.min[is.infinite(tmp.min)] <- "NA"  

    
    
      # PD when hCD45 never < 1% during study period and mouse reaches event 
      # ( hCD45 > 25%) at some point during the study period
      if (  (tmp.min >= 1) & (tmp$survival.event == 1)  ) {
        response <- "PD"
      }
      # SD when hCD45 never < 1% and mouse never reaches 
      #   event during the study period
      if ( (tmp.min >= 1) & (tmp$survival.event == 0)) {
        response <- "SD"
      }
      # PR when hCD45 < 1% at least once during the study period, but not CR
      # CR when hCD45 < 1% for at least 2 consecutive weekly readings during the study period, 
      #   regardless of whether event is reached at a later time-point
      # MCR when hCD45 < 1% for at least 3 consecutive weekly readings 
      #   at any time after treatment has been completed
      
      lessThanOnePercent <- tmp[,3:(dim(tmp)[2]-2)] < 1
      longCons <- apply(apply(lessThanOnePercent, 1, 
                              function(x){ 
                                x * unlist(lapply(rle(x)$lengths, seq_len)) 
                              }),
                        2, max, na.rm=T)
      
      if (longCons >= 3) {
        # 3 or more consecutive weeks after 21 days. 21 days is a crude estimate for end treatment date
        res<-tmp[,3:(dim(tmp)[2]-2)]
        post.end.treat.lessThanOnePercent <- res[,as.numeric(names(res)) > 21] < 1
        post.end.treat.consec <- apply(apply(post.end.treat.lessThanOnePercent, 1, 
                                             function(x){ 
                                               x * unlist(lapply(rle(x)$lengths, seq_len)) 
                                             }),
                                       2, max, na.rm=T)
        if (post.end.treat.consec >= 3) {
          response <- "MCR" #  
        } else {
          response <- "CR"
        }
        
      } else if (longCons >= 2 & longCons < 3) {
        response <- "CR"
      } else if (longCons >= 1 & longCons < 2) {
        response <- "PR"
      }
    
    } else {
      response <- "PD"
    }
      
    return(response)
  }
  
  
  
  # extract data for the group
  for (g in 1:length(dat$formattedData$group)) {
    if (dat$formattedData[dat$formattedData$group == dat$formattedData$group[g],"exclude"] == FALSE) {
      output[g,6] <- calc.OR.single.mouse(dat,group = dat$formattedData$group[g])    
    } else {
      output[g,6] <- NA
    }
    
  }
    
  
  
    
  
  
  
  return(output)
}



format.ST.table <- function(dat, config){
  #output <- matrix('', length(config$control) + length(config$table.comparisons), 31)
  output <- matrix('', length(config$control) + length(config$table.comparisons), 32)
  # study
  output[1,1] <- paste(dat$project$agent_code, dat$project$agent_name, sep="-")
  # tumor
  output[1,2] <- dat$project$tumor_model
  # group
  tmp <- config$control
  for(test in config$table.comparisons){
    if(test[3]){
      tmp <- c(tmp, test[2])
    } else {
      tmp <- c(tmp, paste(test[1], test[2], sep=" vs "))
    }
  }
  output[,3] <- tmp
  # count
  output[,4:8] <- format.counts(dat, config)
  output[,4:8] <- as.numeric(output[,4:8])
  # efs
  output[,10:14] <- format.efs(dat, config)
  # aoc - median by group
  library(dplyr)
  tmp<-dat$formattedData %>%
    group_by(group) %>%
    summarize(median = median ( as.numeric(aoc) ) )
  output[1:length(unique(dat$formattedData$group)),15] <-format(round(tmp$median, 1), nsmall = 1)  
  
  # this will fail if N=1
  # tumor volume evaluation
  output[,17:20] <- format.volume(dat, config)
  # response evaluation
  output[,22:30] <- format.ST.response(dat, config)
  
  
  
  output[,32] <- format.toxicity(dat, config)
  
  return(output)
}




format.ST.table2 <- function(dat, config){
	# Feb 2021 - Eric Earley
	# anticipating end of PPTC this will create the final stats summary table for the experiment.
	# then will be consolidated into one big ALL file
  
  

  # 
  output <- matrix("", length(config$control) + length(config$table.comparisons), 37)
  
  colnames(output) <- c("Panel_code", #1
                        "Agent_code", #2
                        "Agent_name", #3
                        "Agent_class", #4
                        "Chembl_ID", #5
                        "Drugbank_ID", #6
                        "Agent_dose", #7
                        "Agent_schedule", #8
                        "Model", #9
                        "Group", #10
                        "N", #11
                        "N_deaths",#12
                        "N_toxic_deaths", #13
                        "N_analyzed", #14
                        "N_evented", #15
                        "KM_median", #16
                        "EFS_T-C", #17
                        "EFS_T/C", #18
                        "p_exact_log_rank_efs", #19
                        "p_gehan_wilcoxon_efs", #20
                        "AOC_median", #21
                        "V0_mean_SD", #22
                        "V0_p", #23
                        "minRTV_mean_SD", #24
                        "minRTV_p", #25
                        "PD", #26
                        "PD1", #27
                        "PD2", #28
                        "SD", #29
                        "PR", #30
                        "CR", #31
                        "MCR", #32
                        "Resp_rate", #33
                        "Med_resp", #34
                        "Max_avg_pct_weight_loss", #35
						"Tx_start", #36
						"Exp_end") #37
  
  # experiment/panel code
  output[,1] <- dat$project$panel_code
  
  # agent code
  output[,2] <- dat$project$agent_code
  
  # agent name
  agent.dict <- read.csv(paste0(resources.dir,"PPTC_Agents_v1.2.csv"),header=T,stringsAsFactors = F)
  colnames(agent.dict)[1] <- "agent_number"
  for (i in 1:length(dat$groupInfo$group)) {
	if (i == 1) {
		output[i,3] <- "Control"
	} else if ( i >= 2) {
		output[i,3] <- agent.dict[agent.dict$agent_number == as.integer(dat$project$agent_code),"agent_name"]
	} #else {
		# group B is usually agent code (e.g. 1607)
		# but groups C and on are usually strings (e.g. disatinib), so no need to look the name up!
		# or possibly dose responses of main agent
		#output[i,3] <- dat$groupInfo$drug[i]
	#}
  
  
  
  }
    
  # agent class
  output[1,4] <- NA
  output[2,4] <- agent.dict[agent.dict$agent_number == as.integer(dat$project$agent_code),"agent_class"]
  

  # chembl ID
  output[1,5] <- NA
  output[2,5] <- agent.dict[agent.dict$agent_number == as.integer(dat$project$agent_code),"CHEMBLID"]
  
  # drugbank ID
  output[1,6] <- NA
  output[2,6] <- agent.dict[agent.dict$agent_number == as.integer(dat$project$agent_code),"DRUGBANKID"]
  
  # agent dose
  output[1,7] <- dat$groupInfo$dose[1]
  output[2,7] <- dat$groupInfo$dose[2]
  if (length(dat$groupInfo$group) > 2) {
	  for (i in 3:length(dat$groupInfo$group)) {
		output[i,7] <- dat$groupInfo$dose[i]
	  }
  }
  # agent schedule
  output[1,8] <- dat$groupInfo$schedule[1]
  output[2,8] <- dat$groupInfo$schedule[2]
  if (length(dat$groupInfo$group) > 2) {
	  for (i in 3:length(dat$groupInfo$group)) {
		output[i,8] <- dat$groupInfo$schedule[i]
	  }
  }
  
  # model
  output[,9] <- dat$project$tumor_model
  
  
  # group
  tmp <- config$control
  for(test in config$table.comparisons){
    if(test[3]){
      tmp <- c(tmp, test[2])
    } else {
      tmp <- c(tmp, paste(test[1], test[2], sep=" vs "))
    }
  }
  output[,10] <- tmp
  
  # counts
  output[,11:15] <- format.counts(dat, config)
  
  
  # efs
  #output[,10:14] <- format.efs(dat, config)
  output[,16:20] <- format.efs(dat, config)
  
  # aoc
  library(dplyr)
  suppressWarnings({tmp<-dat$formattedData %>%
    group_by(group) %>%
    summarize(median = median ( as.numeric(aoc) ) )})
  output[1:length(unique(dat$formattedData$group)),21] <-format(round(tmp$median, 1), nsmall = 1)  
  
  
  # this will fail if N=1
  # tumor volume evaluation
  output[,22:25] <- format.volume(dat, config)
  # response evaluation
  output[,26:34] <- format.ST.response(dat, config)
  
  
  
  output[,35] <- format.toxicity(dat, config)
  
  output[,36] <- as.character(min(dat$rawData$measureDate))
  output[,37] <- as.character(max(dat$rawData$measureDate))
  
  return(output)
}
#########################################
#####        REDCap results         #####
#########################################

##### Sub-table formatting function #####
format.redcap.counts.one.group <- function(dat, group){
  this.dat <- dat$formattedData[ dat$formattedData$group == group, ]
  
  # N
  n <- dim(this.dat)[1]
  
  # Nd
  if(!is.null(this.dat$toxic)){
    nd <- sum(this.dat$toxic)
  } else {
    nd <- 0
  }
  
  # Nx
  if(!is.null(this.dat$exclude)){
    nx <- sum(this.dat$exclude) - nd
    tmp <- this.dat[ !this.dat$exclude, ]
  } else {
    nx <- 0
    tmp <- this.dat
  }
  
  # Na
  na <- dim(tmp)[1]
  # Ne
  ne <- sum(tmp$survival.event)
  
  return(c(n, nd, nx, na, ne))
}

format.redcap.efs.one.group <- function(dat, group){
  km.table <- summary(dat$km)$table
  if(paste0("factor(dat$group)=", group) %in% rownames(km.table)) {
    return(c(format.day(km.table[paste0("factor(dat$group)=", group), "median"], dat$project$maxday)))
  } else { return(NA) }
}

format.redcap.efs.one.pair <- function(dat, group1, group2){
  km.table <- summary(dat$km)$table
  return(c(format.day.diff(km.table[paste0("factor(dat$group)=", group1), "median"], km.table[paste0("factor(dat$group)=", group2), "median"], dat$project$maxday),
           format.day.ratio(km.table[paste0("factor(dat$group)=", group1), "median"], km.table[paste0("factor(dat$group)=", group2), "median"], dat$project$maxday),
           format.p(dat$exact_rank_tests[[paste(group1, group2, sep=" vs ")]]),
           format.p(dat$wilcoxon_tests[[paste(group1, group2, sep=" vs ")]])))
}

format.redcap.cd45.one.group <- function(dat, group){
  # baseline mean, sd
  output <- NULL
  this.dat <- dat$rawData[dat$rawData$group == group & 
                            dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
  tmp <- this.dat[this.dat$day == min(this.dat$day), ]
  output <- c(output, format.v(mean(tmp$cd45, na.rm=TRUE)), format.v(sd(tmp$cd45, na.rm=TRUE)))
  
  # minCD45 mean, sd
  tmp <- this.dat[this.dat$day != min(this.dat$day), ]
  tmp <- aggregate(tmp$cd45, by = list(tmp$mouseID), min)
  names(tmp) <- c("mouseID", "cd45")
  output <- c(output, format.v(mean(tmp$cd45, na.rm=TRUE)), format.v(sd(tmp$cd45, na.rm=TRUE)))
  
  # delta cd45 baseline median, IQR
  tmp <- dcast(this.dat, mouseID ~ day, value.var = "cd45")
  if(dim(tmp)[2] == 2){
    stop("Only one time point available.")
  } else if(dim(tmp)[2] == 3){
    tmpData <- tmp[,3]
  } else {
    tmpData <- apply(tmp[,3:(dim(tmp)[2])], 1, min, na.rm=T)
  }
  tmpData[is.infinite(tmpData)] <- NA
  tmp <- tmpData / tmp[,2] - 1
  q <- quantile(tmp, probs=c(0.25,0.5,0.75), na.rm=T)
  output <- c(output, 
              format(round(q[2],2),nsmall=2), 
              format(round(q[1],2),nsmall=2), 
              format(round(q[3],2),nsmall=2))
  return(output)
}

format.redcap.cd45.one.pair <- function(dat, group1, group2){
  output <- NULL
  
  # baseline p value  
  this.dat <- dat$rawData[dat$rawData$group %in% c(group1, group2) & 
                            dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
  tmp <- this.dat[this.dat$day == min(this.dat$day), ]
  output <- c(output, format.p(wilcox.test(tmp$cd45 ~ as.factor(tmp$group))$p.value))
  
  # min hcd45 ratio and p value
  tmp <- this.dat[this.dat$day != min(this.dat$day), ]
  tmp <- aggregate(tmp$cd45, by = list(tmp$mouseID, tmp$group), min)
  names(tmp) <- c("mouseID", "group", "cd45")
  if(sum(tmp$cd45 != 0) == 0){
	output <- c(output,c("NA", 1))
  } else {   
    output <- c(output, 
                format.mincd45.ratio(mean(tmp$cd45[tmp$group == group1], na.rm=T), 
                                     mean(tmp$cd45[tmp$group == group2], na.rm=T)),
                format.p(wilcox.test(tmp$cd45 ~ as.factor(tmp$group))$p.value))
  }			  
			  
			  
  
  return(output)
}

format.redcap.volume.one.group <- function(dat, group){
  output <- NULL
  
  # V0 mean, sd
  this.dat <- dat$rawData[dat$rawData$group == group & 
                            dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
  tmp <- this.dat[this.dat$day == min(this.dat$day), ]
  output <- c(output, 
              format.v(mean(tmp$volume, na.rm=TRUE)), 
              format.v(sd(tmp$volume, na.rm=TRUE)))
  
  # minRTV mean, sd
  tmp <- this.dat[this.dat$day != min(this.dat$day), ]
  tmp <- aggregate(tmp$rtv, by = list(tmp$mouseID), min)
  names(tmp) <- c("mouseID", "rtv")
  output <- c(output, 
              format.v(mean(tmp$rtv, na.rm=TRUE)),
              format.v(sd(tmp$rtv, na.rm=TRUE)))
  
  return(output)
}

format.redcap.volume.one.pair <- function(dat, group1, group2){
  output <- NULL
  
  # V0 p value
  this.dat <- dat$rawData[dat$rawData$group %in% c(group1, group2) & 
                            dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
  # this fails if there is no day 0 for a group
  #tmp <- this.dat[abs(this.dat$day - min(this.dat$day)) <= 1, ]
  #edit Nov 13, 2020 - Eric Earley
  tmp<-NULL
  group1.min.day<-min(this.dat[this.dat$group == group1,"day"])
  group2.min.day<-min(this.dat[this.dat$group == group2,"day"])
  tmp<-rbind(tmp,this.dat[this.dat$group == group1 & this.dat$day == group1.min.day,])
  tmp<-rbind(tmp,this.dat[this.dat$group == group2 & this.dat$day == group2.min.day,])
  output <- c(output, 
              format.p(wilcox.test(tmp$volume ~ as.factor(tmp$group))$p.value))
  
  # minRTV p value
  #tmp <- this.dat[this.dat$day != min(this.dat$day), ]
  tmp<-NULL
  tmp<-rbind(tmp,this.dat[this.dat$group == group1 & this.dat$day != group1.min.day,])
  tmp<-rbind(tmp,this.dat[this.dat$group == group2 & this.dat$day != group2.min.day,])
  tmp <- aggregate(tmp$rtv, by = list(tmp$mouseID, tmp$group), min)
  names(tmp) <- c("mouseID", "group", "rtv")
  output <- c(output, 
              format.p(wilcox.test(tmp$rtv ~ as.factor(tmp$group))$p.value))
  
  return(output)
}

format.redcap.ALL.response.one.group <- function(dat, group, config){
  output <- format.ALL.response(dat = dat, config = config)
  if(group %in% rownames(output)){
    output <- as.vector(output[group, ])
  } else {
    output <- as.vector(output[paste(config$control, "vs", group), ])
  }
  return(output)
}

format.redcap.ST.response.one.group <- function(dat, group, config){
  output <- format.ST.response(dat = dat, config = config)
  if(group %in% rownames(output)){
    output <- as.vector(output[group, ])
  } else {
    output <- as.vector(output[paste(config$control, "vs", group), ])
  }
  return(output)
}

format.redcap.toxicity.one.group <- function(dat, group){
  output <- NULL
  
  tmp <- aggregate(dat$weight$weight, by=list(dat$weight$group, dat$weight$days), mean, na.rm=TRUE)
  colnames(tmp) <- c("Group", "Days", "Weight")
  tmp2 <- aggregate(dat$weight$weight, by=list(dat$weight$group, dat$weight$days), length)
  colnames(tmp2) <- c("Group", "Days", "Measured_mice")
  tmp <- merge(tmp, tmp2, sort = FALSE)
  
  this.group <- tmp[tmp$Group == group, ]
  N <- dim(dat$formattedData[ dat$formattedData$group == group, ])[1]
  this.group <- this.group[this.group$Measured_mice >= 0.75 * N,]
  output <- c(output, format.percent( 1 - min(this.group$Weight) / this.group$Weight[1]))
  
  return(output)
}

##### single row formatting function #####
format.redcap.CNS.table.one.group <- function(dat, group, config){
  output <- list()
  # study panel code, agent, model
  tmp <- strsplit(dat$project$panel_code, "-")[[1]]
  output[["stat_result_panel_code"]] <- dat$project$panel_code
  output[["stat_result_agent_code"]] <- tmp[1]
  output[["stat_result_tumor_model"]] <- paste(tmp[2:(length(tmp)-1)], collapse = "-")

  # group
  output[["stat_result_group1"]] <- group
  output[["stat_result_type"]] <- "Group"
  output[["record_id"]] <- paste(dat$project$panel_code,
                                 "Group", group, sep = "-")
  
  # count
  tmp <- format.redcap.counts.one.group(dat, group)
  output[["stat_result_n"]] <- tmp[1]
  output[["stat_result_nd"]] <- tmp[2]
  output[["stat_result_nx"]] <- tmp[3]
  output[["stat_result_na"]] <- tmp[4]
  output[["stat_result_nev"]] <- tmp[5]
  
  # efs
  tmp <- format.redcap.efs.one.group(dat, group)
  output[["stat_result_km_median"]] <- tmp[1]
  
  output[["stat_result_weight_loss"]] <- format.redcap.toxicity.one.group(dat, group)
  return(as.data.frame(output, stringsAsFactors = FALSE))
}
format.redcap.CNS.table.one.pair <- function(dat, group1, group2){
  output <- list()
  # study panel code, agent, model
  tmp <- strsplit(dat$project$panel_code, "-")[[1]]
  output[["stat_result_panel_code"]] <- dat$project$panel_code
  output[["stat_result_agent_code"]] <- tmp[1]
  output[["stat_result_tumor_model"]] <- paste(tmp[2:(length(tmp)-1)], collapse = "-")
  
  # group
  output[["stat_result_group1"]] <- group1
  output[["stat_result_group2"]] <- group2
  output[["stat_result_type"]] <- "Comparison"
  output[["record_id"]] <- paste(dat$project$panel_code,
                                 "Compare", group1, group2, sep = "-")
  
  # efs
  tmp <- format.redcap.efs.one.pair(dat, group1, group2)
  output[["stat_result_efs_t_minus_c"]] <- tmp[1]
  output[["stat_result_efs_t_c_ratio"]] <- tmp[2]
  output[["stat_result_efs_exact_rank_p"]] <- tmp[3]
  output[["stat_result_efs_wilcoxon_p"]] <- tmp[4]
  
  return(as.data.frame(output, stringsAsFactors = FALSE))
}

format.redcap.ALL.table.one.group <- function(dat, group, config){
  output <- list()
  # study panel code, agent, model
  tmp <- strsplit(dat$project$panel_code, "-")[[1]]
  output[["stat_result_panel_code"]] <- dat$project$panel_code
  output[["stat_result_agent_code"]] <- tmp[1]
  output[["stat_result_tumor_model"]] <- paste(tmp[2:(length(tmp)-1)], collapse = "-")
  
  # group
  output[["stat_result_group1"]] <- group
  output[["stat_result_type"]] <- "Group"
  output[["record_id"]] <- paste(dat$project$panel_code,
                                 "Group", group, sep = "-")
  
  # count
  tmp <- format.redcap.counts.one.group(dat, group)
  output[["stat_result_n"]] <- tmp[1]
  output[["stat_result_nd"]] <- tmp[2]
  output[["stat_result_nx"]] <- tmp[3]
  output[["stat_result_na"]] <- tmp[4]
  output[["stat_result_nev"]] <- tmp[5]
  
  # efs
  tmp <- format.redcap.efs.one.group(dat, group)
  output[["stat_result_km_median"]] <- tmp[1]
  
  # %hcd45
  tmp <- format.redcap.cd45.one.group(dat, group)
  output[["stat_result_baseline_mean"]] <- tmp[1]
  output[["stat_result_baseline_sd"]] <- tmp[2]
  output[["stat_result_min_rtv_cd45_mean"]] <- tmp[3]
  output[["stat_result_min_rtv_cd45_sd"]] <- tmp[4]
  output[["stat_result_delta_cd45_median"]] <- tmp[5]
  output[["stat_result_delta_cd45_iqr_25"]] <- tmp[6]
  output[["stat_result_delta_cd45_iqr_75"]] <- tmp[7]
  
  # response evaluation
  tmp <- format.redcap.ALL.response.one.group(dat, group, config)
  output[["stat_result_response_pd"]] <- tmp[1]
  output[["stat_result_response_pd1"]] <- tmp[2]
  output[["stat_result_response_pd2"]] <- tmp[3]
  output[["stat_result_response_sd"]] <- tmp[4]
  output[["stat_result_response_pr"]] <- tmp[5]
  output[["stat_result_response_cr"]] <- tmp[6]
  output[["stat_result_response_mcr"]] <- tmp[7]
  output[["stat_result_response_rate"]] <- tmp[8]
  output[["stat_result_response_median"]] <- tmp[9]
  
  if(exists("w")){
    output[["stat_result_weight_loss"]] <- format.redcap.toxicity.one.group(dat, group)
  } else {
    output[["stat_result_weight_loss"]]<-NA
  }
  
  return(as.data.frame(output, stringsAsFactors = FALSE))
}
format.redcap.ALL.table.one.pair <- function(dat, group1, group2){
  output <- list()
  # study panel code, agent, model
  tmp <- strsplit(dat$project$panel_code, "-")[[1]]
  output[["stat_result_panel_code"]] <- dat$project$panel_code
  output[["stat_result_agent_code"]] <- tmp[1]
  output[["stat_result_tumor_model"]] <- paste(tmp[2:(length(tmp)-1)], collapse = "-")
  
  # group
  output[["stat_result_group1"]] <- group1
  output[["stat_result_group2"]] <- group2
  output[["stat_result_type"]] <- "Comparison"
  output[["record_id"]] <- paste(dat$project$panel_code,
                                 "Compare", group1, group2, sep = "-")
  
  # efs
  tmp <- format.redcap.efs.one.pair(dat, group1, group2)
  output[["stat_result_efs_t_minus_c"]] <- tmp[1]
  output[["stat_result_efs_t_c_ratio"]] <- tmp[2]
  output[["stat_result_efs_exact_rank_p"]] <- tmp[3]
  output[["stat_result_efs_wilcoxon_p"]] <- tmp[4]
  
  # %hcd45
  tmp <- format.redcap.cd45.one.pair(dat, group1, group2)
  output[["stat_result_baseline_p"]] <- tmp[1]
  output[["stat_result_min_t_c_ratio"]] <- tmp[2]
  output[["stat_result_min_p"]] <- tmp[3]
  
  return(as.data.frame(output, stringsAsFactors = FALSE))
}

format.redcap.ST.table.one.group <- function(dat, group, config){
  output <- list()
  # study panel code, agent, model
  tmp <- strsplit(dat$project$panel_code, "-")[[1]]
  output[["stat_result_panel_code"]] <- dat$project$panel_code
  output[["stat_result_agent_code"]] <- tmp[1]
  output[["stat_result_tumor_model"]] <- paste(tmp[2:(length(tmp)-1)], collapse = "-")
  
  # group
  output[["stat_result_group1"]] <- group
  output[["stat_result_type"]] <- "Group"
  output[["record_id"]] <- paste(dat$project$panel_code,
                                 "Group", group, sep = "-")
  
  # count
  tmp <- format.redcap.counts.one.group(dat, group)
  output[["stat_result_n"]] <- tmp[1]
  output[["stat_result_nd"]] <- tmp[2]
  output[["stat_result_nx"]] <- tmp[3]
  output[["stat_result_na"]] <- tmp[4]
  output[["stat_result_nev"]] <- tmp[5]
  
  # efs
  tmp <- format.redcap.efs.one.group(dat, group)
  output[["stat_result_km_median"]] <- tmp[1]
  
  # %hcd45
  tmp <- format.redcap.volume.one.group(dat, group)
  output[["stat_result_baseline_mean"]] <- tmp[1]
  output[["stat_result_baseline_sd"]] <- tmp[2]
  output[["stat_result_min_rtv_cd45_mean"]] <- tmp[3]
  output[["stat_result_min_rtv_cd45_sd"]] <- tmp[4]

  # response evaluation
  tmp <- format.redcap.ST.response.one.group(dat, group, config)
  output[["stat_result_response_pd"]] <- tmp[1]
  output[["stat_result_response_pd1"]] <- tmp[2]
  output[["stat_result_response_pd2"]] <- tmp[3]
  output[["stat_result_response_sd"]] <- tmp[4]
  output[["stat_result_response_pr"]] <- tmp[5]
  output[["stat_result_response_cr"]] <- tmp[6]
  output[["stat_result_response_mcr"]] <- tmp[7]
  output[["stat_result_response_rate"]] <- tmp[8]
  output[["stat_result_response_median"]] <- tmp[9]
  
  output[["stat_result_weight_loss"]] <- format.redcap.toxicity.one.group(dat, group)
  
  output[["pptc_statistical_analyses_results_complete"]] <- 2
  
  return(as.data.frame(output, stringsAsFactors = FALSE))
}
format.redcap.ST.table.one.pair <- function(dat, group1, group2){
  output <- list()
  # study panel code, agent, model
  tmp <- strsplit(dat$project$panel_code, "-")[[1]]
  output[["stat_result_panel_code"]] <- dat$project$panel_code
  output[["stat_result_agent_code"]] <- tmp[1]
  output[["stat_result_tumor_model"]] <- paste(tmp[2:(length(tmp)-1)], collapse = "-")
  
  # group
  output[["stat_result_group1"]] <- group1
  output[["stat_result_group2"]] <- group2
  output[["stat_result_type"]] <- "Comparison"
  output[["record_id"]] <- paste(dat$project$panel_code,
                                 "Compare", group1, group2, sep = "-")
  
  # efs
  tmp <- format.redcap.efs.one.pair(dat, group1, group2)
  output[["stat_result_efs_t_minus_c"]] <- tmp[1]
  output[["stat_result_efs_t_c_ratio"]] <- tmp[2]
  output[["stat_result_efs_exact_rank_p"]] <- tmp[3]
  output[["stat_result_efs_wilcoxon_p"]] <- tmp[4]
  
  # tve
  tmp <- format.redcap.volume.one.pair(dat, group1, group2)
  output[["stat_result_baseline_p"]] <- tmp[1]
  output[["stat_result_min_p"]] <- tmp[2]
  
  output[["pptc_statistical_analyses_results_complete"]] <- 2
  
  return(as.data.frame(output, stringsAsFactors = FALSE))
}

##### single row formatting function #####
format.redcap.CNS.table <- function(dat, config){
  output <- list()
  
  for(group in dat$groupInfo$group){
    output$single_group <- rbind(output[["single_group"]], 
                                      format.redcap.CNS.table.one.group(dat, group, config))
  }
  
  for(comparison in config$table.comparisons){
    output$comparison <- rbind(output[["comparison"]], 
                                    format.redcap.CNS.table.one.pair(dat, comparison[1], comparison[2]))
  }
  
  return(output)
}

format.redcap.ALL.table <- function(dat, config){
  output <- list()
  
  for(group in dat$groupInfo$group){
    output$single_group <- rbind(output[["single_group"]], 
                                 format.redcap.ALL.table.one.group(dat, group, config))
  }
  
  for(comparison in config$table.comparisons){
    output$comparison <- rbind(output[["comparison"]], 
                               format.redcap.ALL.table.one.pair(dat, comparison[1], comparison[2]))
  }
  
  return(output)
}

format.redcap.ST.table <- function(dat, config){
  output <- list()
  
  for(group in dat$groupInfo$group){
    output$single_group <- rbind(output[["single_group"]], 
                                 format.redcap.ST.table.one.group(dat, group, config))
  }
  
  for(comparison in config$table.comparisons){
    output$comparison <- rbind(output[["comparison"]], 
                               format.redcap.ST.table.one.pair(dat, comparison[1], comparison[2]))
  }
  
  return(output)
}

######################################################
#####        REDCap Single mouse results         #####
######################################################

##### single mice formatting function #####
format.redcap.mice.level.ST.table.one.group <- function(dat, group){
  tmp <- dat$rawData[dat$rawData$group == group & 
                       dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
  tmp <- tmp[tmp$day != min(tmp$day), ]
  output <- aggregate(tmp$rtv, by = list(tmp$mouseID), min)
  names(output) <- c("mice_level_mouse_id", "mice_level_min_rtv_rcd45")

  # study panel code, agent, model
  tmp <- strsplit(dat$project$panel_code, "-")[[1]]
  output[["mice_level_result_panel_code"]] <- dat$project$panel_code
  output[["mice_level_result_agent_code"]] <- tmp[1]
  output[["mice_level_result_tumor_model"]] <- paste(tmp[2:(length(tmp)-1)], collapse = "-")
  
  # group
  output[["mice_level_result_group"]] <- group
  output[["record_id"]] <- paste(dat$project$panel_code,
                                   "Mouse", output$mice_level_mouse_id, sep = "-")
  output[["pptc_mice_level_results_complete"]] <- 2
  return(output)
}

format.redcap.mice.level.ST.table <- function(dat){
  output <- NULL
  
  for(group in dat$groupInfo$group){
    output <- rbind(output, format.redcap.mice.level.ST.table.one.group(dat, group))
  }
  
  return(output)
}

format.redcap.mice.level.ALL.table.one.group <- function(dat, group){
  tmp <- dat$rawData[dat$rawData$group == group & 
                       dat$rawData$mouseID %in% dat$formattedData$mouseID[!dat$formattedData$exclude], ]
  day0 <- tmp[tmp$day == min(tmp$day), ]
  tmp <- tmp[tmp$day != min(tmp$day), ]
  output <- aggregate(tmp$cd45, by = list(tmp$mouseID), min)
  # calculate relative cd45
  output$x <- output$x / day0$cd45[match(output$Group.1, day0$mouseID)]
  names(output) <- c("mice_level_mouse_id", "mice_level_min_rtv_rcd45")
  
  # study panel code, agent, model
  tmp <- strsplit(dat$project$panel_code, "-")[[1]]
  output[["mice_level_result_panel_code"]] <- dat$project$panel_code
  output[["mice_level_result_agent_code"]] <- tmp[1]
  output[["mice_level_result_tumor_model"]] <- paste(tmp[2:(length(tmp)-1)], collapse = "-")
  
  # group
  output[["mice_level_result_group"]] <- group
  output[["record_id"]] <- paste(dat$project$panel_code,
                                   "Mouse", output$mice_level_mouse_id, sep = "-")
  output[["pptc_mice_level_results_complete"]] <- 2
  return(output)
}

format.redcap.mice.level.ALL.table <- function(dat){
  output <- NULL
  
  for(group in dat$groupInfo$group){
    output <- rbind(output, format.redcap.mice.level.ALL.table.one.group(dat, group))
  }
  
  return(output)
}
