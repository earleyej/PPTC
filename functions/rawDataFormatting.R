require(survival, quietly = TRUE, warn.conflicts = FALSE)
require(reshape2, quietly = TRUE, warn.conflicts = FALSE)
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)


##### functions used in this module #####
# function checks a column of a dataframe for value consistency
check.column.unique <- function(dat, col, fileConn){
  colToCheck <- dat[[col]]
  if(length(unique(colToCheck)) != 1){
    writeLines(paste(col, ":", "Not Unique !!!"), fileConn)
    writeLines(unique(colToCheck), fileConn)
  } else {
    writeLines(paste(col, ":", "Unique - Great!"), fileConn)
  }
  writeLines('\n#######################################################\n',fileConn)
}
check.column.non.unique <- function(dat, col, fileConn){
  colToCheck <- dat[[col]]
  if(length(unique(colToCheck)) == 1){
    writeLines(paste(col, ":", "Unique !!!"), fileConn)
    writeLines(unique(colToCheck), fileConn)
  } else {
    writeLines(paste(col, ":", "Not Unique - Great!"), fileConn)
  }
  writeLines('\n#######################################################\n',fileConn)
}

# function check a column of a dataframe for value duplications
check.column.duplicate <- function(dat, col, fileConn){
  colToCheck <- dat[[col]]
  if(length(unique(colToCheck)) != length(colToCheck)){
    writeLines(paste(col, ":", "Contains duplicates !!!"), fileConn)
    writeLines(table(), fileConn)
  } else {
    writeLines(paste(col, ":", "No duplicates - Great!"), fileConn)
  }
  writeLines('\n#######################################################\n',fileConn)
}

# function check a date column for format
check.column.date.format <- function(dat, col, fileConn, format = config$DateFormat){
  tmpDate <- try(as.Date(dat[[col]], format))
  if(class(tmpDate) == "try-error" || sum(is.na(tmpDate)) > 0 ) {
    writeLines(paste(col, ":", "Format incorrect !!!"), fileConn)
  } else{
    writeLines(paste(col, ":", "format correct - Great!") , fileConn)
  }
  writeLines('\n#######################################################\n',fileConn)
}

##### data checking and data formatting pipeline #####

format.solidTumor.data <- function(processed){
  if("data_comment" %in% names(processed) && sum(!is.na(processed$data_comment)) != 0){
    stop("Comment exists. Manually review the comment and delete to run pipeline.")
  }
  
  ### checking and keeping useful data ###
  dat <- list(project = list(),
              groupInfo = NULL,
              rawData = NULL,
              weight = NULL)
  if (length(unique(processed$st_panel_code)) > 1 ) {
    print("!!!!! panel code is not unique. Check for character replacements (0 vs. O) or extra spaces !!!!")
    print(unique(processed$st_panel_code))
  }

  dat$project[["panel_code"]] <- unique(processed$st_panel_code)
  tmp <- panel_code_to_name(dat$project$panel_code)
  dat$project[["agent_code"]] <- tmp$agentCode
  dat$project[["agent_name"]] <- tmp$agentName
  

  dat$project[["tumor_model"]] <- unique(processed$tumor)
  

  dat$project[["transplant_date"]] <- as.Date(unique(processed$transplant_date), format = config$DateFormat)
  

  tmp <- processed[,c("group", "mouse")]
  tmp <- unique(tmp)

  if(is.null(processed$treatment_date) || is.na(processed$treatment_date) ||(length(unique(processed$treatment_date)) == 1 && unique(processed$treatment_date) == "")){
   tmp <- aggregate(as.Date(dataset_date, format = config$DateFormat) ~ group, data = processed, FUN = min)
   processed$treatment_date <- tmp[,2][match(processed$group, tmp$group)]
   warning(paste0("Set treatment date as: ",
                  paste(unique(processed$treatment_date), collapse = "; ")))
  }
  
  if(is.null(processed$treatment_end_date) || (length(unique(processed$treatment_end_date)) == 1 && unique(processed$treatment_end_date) == "")){
    processed$treatment_end_date <- as.Date(processed$treatment_date, format = config$DateFormat) + 21
    warning(paste0("Set treatment end date as: ",
                   paste(unique(processed$treatment_end_date), collapse = "; ")))
  }
  

  # keep mouseID, group, measure date altogether
  dat$rawData <- data.frame(mouseID = paste(processed$group, processed$mouse, sep=";"),
                            group = processed$group,
                            measureDate = as.Date(processed$dataset_date, format = config$DateFormat),
                            treatment_date = as.Date(processed$treatment_date, format = config$DateFormat),
                            treatment_end_date = as.Date(processed$treatment_end_date, format = config$DateFormat),
                            stringsAsFactors = FALSE)
  
  
  dat$rawData$day <- dat$rawData$measureDate - dat$rawData$treatment_date
  #if (sum(dat$rawData$day < 0) > 1 ) {
  #  stop("there are some negative values for 'day' vector. Are there some dataset_date values earlier than treatment_date?")
  #  
  #}

  
  
  tmp <- processed[,c("group", "dose", "schedule")]
  tmp <- unique(tmp)
    
  
  
  # sort groupInfo in case processed data is in odd order
  tmp <- tmp[order(tmp$group),]
  dat$groupInfo <- tmp
  
  
  # check volume calculation from diameters and keep diameters
  {
    v_from_diameter <- apply(processed[,c("diameter_x","diameter_y")],
                             1,
                             function(x) (pi/6)*(mean(x)^3)/1000)
    diff_volume <- v_from_diameter - dat$st_volume
  
  }
  
  dat$rawData$diameter_x <- processed$diameter_x
  dat$rawData$diameter_y <- processed$diameter_y
  dat$rawData$volume <- v_from_diameter
  
  # check percent_change, rtv, x4, lt50 and discard
  {
    # tmp <- as.data.frame(
    #   processed[,c("group", "mouse", "day", "st_volume", "percent_change", "rtv", "x4", "lt50")],
    #   stringsAsFactors = FALSE)
    tmp <- dat$rawData[,c("group", "mouseID", "day", "volume")]
    
    # some experiments have weekly datapoints, some have every 2/3 days.
    # if weekly, divide timepoint by 3 
    # if 2/3 days, don't
    # calculate the within-group min. time difference between measurements
    df <- tmp %>%
      group_by(group) %>%
      summarize(min(diff(sort(unique(day)))))
    
    d <- min(df$`min(diff(sort(unique(day))))`)
    
    
    if (d > 3) {
      tmp$timepoint <- round(tmp$day / 3.5)    
    } else {
      tmp$timepoint <- tmp$day
    }
    
    
  
    
    volume <- dcast(tmp, group+mouseID~timepoint, value.var="volume")
    
    
    #original
    #rtv_fromVolume <- volume[,3:dim(volume)[2]] / volume[,3] #this will fail if there's an offset in start dates. E.g., group A's first day is 0, but group B's first day is 3
    
    
    ## edits - Nov 13, 2020 - Eric Earley
    ## this should catch rolling enrollment experiments
    # divide by the earliest column in volume that's not NA
    rtv_fromVolume <- data.frame(matrix(nrow = nrow(volume), 
                                        ncol=dim(volume)[2] - 2))
    colnames(rtv_fromVolume) <- colnames(volume[,3:dim(volume)[2]])
    for (i in 1:nrow(volume)) {
    #  #first non-NA column in volume
      NonNAindex <- which(!is.na(volume[i,3:dim(volume)[2]]))
      firstNonNA <- min(NonNAindex)
      c<-2 + firstNonNA
      rtv_fromVolume[i,] <- volume[i,3:dim(volume)[2]] / volume[i,c]
      
    }
    
    
    
    
    
    rtv_fromVolume <- cbind(volume[,c("group", "mouseID")], rtv_fromVolume)
    rtv_long <- melt(rtv_fromVolume, id.vars = c("group", "mouseID"), variable.name = "timepoint")
    rtv_long <- merge(tmp, rtv_long, sort=FALSE)
    # # x4
    # rtv_long$x4_fromRTV <- 'no'
    # rtv_long$x4_fromRTV[rtv_long$rtv > 4] <- 'yes'
    # rtv_long$x4_fromRTV[is.na(rtv_long$rtv)] <- NA
    # x4_diff <- which(rtv_long$x4 != rtv_long$x4_fromRTV)
    # if(length(x4_diff) == 0){
    #   writeLines("Checking column x4: Correct - Great!", fileConn)
    # } else {
    #   writeLines("Checking column x4: Incorrect !!!", fileConn)
    # }
    # writeLines('#######################################################',fileConn)
    # 
    # # lt50
    # rtv_long$lt50_fromRTV <- 'no'
    # rtv_long$lt50_fromRTV[rtv_long$rtv < 0.5] <- 'yes'
    # lt50_diff <- which(rtv_long$lt50 != rtv_long$lt50_fromRTV)
    # if(length(lt50_diff) == 0){
    #   writeLines("Checking column lt50: Correct - Great!", fileConn)
    # } else {
    #   writeLines("Checking column lt50: Incorrect !!!", fileConn)
    # }
    # writeLines('#######################################################',fileConn)
  }
  # rtv_long$mouseID <- paste(rtv_long$group, rtv_long$mouse, sep=";")
  rtv_keep <- rtv_long[,c("group", "mouseID", "day", "value")]
  names(rtv_keep)[4] <- "rtv"
  dat$rawData <- merge(dat$rawData, rtv_keep, sort=FALSE)
  
  # check number measured mice and keep
  {
    tmp <- aggregate(processed$weight, by=list(processed$group, processed$day), length)
    names(tmp) <- c("group", "day", "counted_mice")
    if("measured_mice" %in% names(processed)){
      tmp2 <- unique(processed[,c("group", "day", "measured_mice")])
      tmp <- merge(tmp, tmp2, all=TRUE)
      tmp <- tmp[which(tmp$counted_mice != tmp$measured_mice),]
      #writeLines("Mouse count differs for:", fileConn)
      #writeLines(names(tmp), fileConn, sep="\t")
      #writeLines('',fileConn)
      # for (i in 1:dim(tmp)[1]){
      #   writeLines(as.character(tmp[i,]), fileConn, sep="\t")
      #   writeLines('',fileConn)
      # }
      # writeLines('#######################################################',fileConn)
    } else{
      processed$measured_mice <- tmp$counted_mice[match(paste(processed$group, processed$day, sep = ";"),
                                                        paste(tmp$group, tmp$day, sep = ";"))]
    }
  }
  
  # weight and keep raw_weight
    #if(sum(!is.na(processed$weight)) > 0){
    #  tmp <- aggregate(processed$weight, 
    #                   by=list(processed$group, substr(processed$mouse,1,1), processed$day), 
    #                   sum)
    #  names(tmp) <- c("group", "cage", "day", "cage_weight")
    #  tmp2 <- processed[,c("group", "day", "cage_a_wt", "cage_b_wt")]
    #  tmp2 <- as.data.frame(unique(tmp2), stringsAsFactors = FALSE)
    #  tmp2 <- melt(tmp2, id.vars = c("group", "day"), variable.name = "cage")
    #  names(tmp2)[3] <- "cage"
    #  tmp2$cage <- toupper(sapply(strsplit(as.character(tmp2$cage), "_"), function(x){x[2]}))
    #  tmp <- merge(tmp, tmp2, all=TRUE)
    #  tmp <- tmp[which(abs(tmp$cage_weight - tmp$value) > 1E-4),]
      #writeLines("Cage weight differs for:", fileConn)
      #writeLines(names(tmp), fileConn, sep="\t")
      #writeLines('',fileConn)
      # for (i in 1:dim(tmp)[1]){
      #   writeLines(as.character(tmp[i,]), fileConn, sep="\t")
      #   writeLines('',fileConn)
      # }
    #} else {
      #writeLines("No individual mouse weight for checking. \n Average cage weight used instead.", fileConn)
      
      
      # enhancement - July 18, 2019 - EJE
      # labs will often have cage weight = 0 when it's clearly a data entry error. Check this here
      # update Oct 2019 - EJE - this doesn't work... there will be 0s for cage B pretty often at experiment's end.
      # processed[,c("group","day","cage_a_wt","cage_b_wt")]
      # check_b <- processed$cage_b_wt == 0
      # processed[check_b,c("group","day","cage_b_wt")]  
      
      #if ( sum(processed$cage_a_wt == 0) == 0 & sum(processed$cage_b_wt == 0) == 0 ) {
      
      # bug fix - when N<5 there will probably be NAs for cageB, causing processed$weight to have just NAs
      # if all cageB is NA, then just use cageA
      if (sum(!is.na(processed$cage_a_wt) & !is.na(processed$cage_b_wt)) == 0 ) {
        processed$weight <- (processed$cage_a_wt + processed$cage_b_wt) / processed$measured_mice  
      }
      #if (sum(is.na(processed$cage_b_wt)) == nrow(processed) ) {
      #  processed$weight <- processed$cage_a_wt / processed$measured_mice  
      #}
      #}
      # processed$weight <- ifelse(processed$cage_b_wt == 0 & processed$cage_a_wt > 0,
      #                              processed$cage_a_wt / (processed$measured_mice/2),
      #                              ifelse(processed$cage_a_wt == 0 & processed$cage_b_wt > 0,
      #                                     processed$cage_b_wt / (processed$measured_mice/2),
      #                                     (processed$cage_a_wt + processed$cage_b_wt) / processed$measured_mice))
    #}
      

    #writeLines('#######################################################',fileConn)

  
  dat$weight <- data.frame(mouseID = paste(processed$group, processed$mouse, sep=";"),
                           group = processed$group,
                           measureDate = as.Date(processed$dataset_date, format = config$DateFormat),
                           days = processed$day,
                           weight = processed$weight,
                           stringsAsFactors = FALSE)
  dat$weight <- dat$weight[!is.na(dat$weight$weight),]
  
  if(nrow(dat$weight) == 0) { # no individual mouse weight
    dat$weight <- dat$weight <- data.frame(mouseID = paste(processed$group, processed$mouse, sep=";"),
                                           group = processed$group,
                                           measureDate = as.Date(processed$dataset_date, format = config$DateFormat),
                                           days = processed$day,
                                           weight = (processed$cage_a_wt + processed$cage_b_wt) / processed$measured_mice,
                                           stringsAsFactors = FALSE)
    dat$weight <- dat$weight[!is.na(dat$weight$weight),]
  }
  
  # percent_day_0 and discard
  #{
    # tmp <- unique(processed[,c("group", "day", "cage_a_wt", "cage_b_wt")])
    # tmp <- melt(tmp, id.vars = c("group", "day"), variable.name = "cage")
    # tmp <- aggregate(tmp$value, by=list(tmp$group, tmp$day), sum)
    # names(tmp) <- c("group", "day", "cages_weight")
    # tmp <- dcast(tmp, group~day, value.var="cages_weight")
    # tmp[,2:dim(tmp)[2]] <- tmp[,2:dim(tmp)[2]] / tmp[,2]
    # tmp <- melt(tmp, id.vars = c("group"), variable.name = "day")
    # names(tmp)[2] <- "day"
    # tmp2 <- unique(processed[,c("group", "day", "percent_day_0")])
    # tmp <- merge(tmp, tmp2, all=TRUE)
    # tmp <- tmp[which(tmp$value - tmp$percent_day_0 > 1E-4),]
    # writeLines("Percent_day_0 differs for:", fileConn)
    # writeLines(names(tmp), fileConn, sep="\t")
    # writeLines('',fileConn)
    # for (i in 1:dim(tmp)[1]){
    #   writeLines(as.character(tmp[i,]), fileConn, sep="\t")
    #   writeLines('',fileConn)
    # }
    # writeLines('#######################################################',fileConn)
  #}
  
  #close(fileConn)
  
  #### checking completed, starting formatting ####
  
  # exclude data after tumor quarduple
  if (TRUE) {
    rowsToRemove <- NULL
    for (mouse in unique(dat$rawData$mouseID)){
      tmp <- dat$rawData[dat$rawData$mouseID == mouse, ]
      tmp <- tmp[order(tmp$day),]
      x4 <- which(tmp$rtv > 4)[1] # first quarduple time point
      if(!is.na(x4) & x4 != nrow(tmp)){
        rowsToRemove <- c(rowsToRemove, rownames(tmp)[(x4+1):nrow(tmp)]) # do not exclude first quarduple time point
      }
    }
    dat$rawData <- dat$rawData[! rownames(dat$rawData) %in% rowsToRemove,]
  }

  # process data by mouse
  per.mouse <- unique(dat$rawData[,c('mouseID', 'group')])

  # construct formated output
  output <- data.frame(mouseID = per.mouse$mouse,
                       group = per.mouse$group,
                       toxic = FALSE,
                       exclude = FALSE,
                       survival.time = NA,
                       survival.event = NA,
					   baselinevol = NA,
					   minvol = NA,
                       stringsAsFactors = FALSE)
  
  
  
  
  # fill in Days and Events
  for (mouse in per.mouse$mouseID){
    tmp <- dat$rawData[(dat$rawData$mouseID == mouse), c('day', 'volume', 'rtv')]
    tmp <- tmp[order(tmp$day),]
### SWE 2019-12-27
	tmp$daytmp = tmp$day
	tmp$daytmp[tmp$daytmp < 0] = 0
	#minvol is min vol after day 3
	#baselinevol is min vol before day 3
  	# if there is only one timepoint, and it is < 3, then mincvol = baselinevol
	if (max(tmp$daytmp) > 3) {
		output[output$mouseID == mouse, 'minvol'] = min(tmp$volume[tmp$daytmp > 3])
		output[output$mouseID == mouse, 'baselinevol'] = min(tmp$volume[tmp$daytmp < 3])	
	}
	if (max(tmp$daytmp) < 3) {
		output[output$mouseID == mouse, 'minvol'] = min(tmp$volume[tmp$daytmp < 3])	
		output[output$mouseID == mouse, 'baselinevol'] = min(tmp$volume[tmp$daytmp < 3])	
	}
    ###
	
	
	
	
    if(max(tmp$rtv, na.rm=TRUE) < 4){ # no Event
      output[output$mouseID == mouse, 'survival.time'] <- tmp$day[dim(tmp)[1]]
      output[output$mouseID == mouse, 'survival.event'] <- 0
	  output[output$mouseID == mouse, 'aoc'] <- sum(diff(tmp$daytmp)*((4-tmp$rtv[-1])+(4-tmp$rtv[-length(tmp$rtv)]))/2)
    }else{ # Event = 1
      index <- min(which(tmp$rtv > 4)) # index of first cd45 > 25%     
      output[output$mouseID == mouse, 'survival.time'] <- tmp$day[index-1] + 
        (tmp$day[index] - tmp$day[index-1]) *
        (log( 4 / tmp$rtv[index-1] ) / log( tmp$rtv[index]/tmp$rtv[index-1] ))
      output[output$mouseID == mouse, 'survival.event'] <- 1
	  tmp$rtvtmp = tmp$rtv
	  tmp$rtvtmp[tmp$rtvtmp>4] = 4
	  tmp$daytmp[tmp$rtvtmp==4] = output[output$mouseID == mouse, 'survival.time']
	  output[output$mouseID == mouse, 'aoc'] <- sum(diff(tmp$daytmp)*((4-tmp$rtvtmp[-1])+(4-tmp$rtvtmp[-length(tmp$rtvtmp)]))/2)
    }
  }
  output$aoc = output$aoc/4
### SWE
  
  # toxic death
  tmp <- processed[!is.na(processed$unexpect_event) & 
                     (processed$unexpect_event == 1 | trimws(processed$unexpect_event) == "1 - Death/euthanasia possibly treatment related"), 
                   c("group", "mouse")]
  if(dim(tmp)[1] > 0){
    output[output$mouseID %in% apply(tmp, 1, paste, collapse=";"), "toxic"] <- TRUE
    output[output$mouseID %in% apply(tmp, 1, paste, collapse=";"), "exclude"] <- TRUE
  }
  
  # failed engraftment
  tmp <- processed[!is.na(processed$unexpect_event) & 
                     (processed$unexpect_event == 3 | processed$unexpect_event == "3 - Failed engraftment"), 
                   c("group", "mouse")]
  if(dim(tmp)[1] > 0){
    output[output$mouseID %in% apply(tmp, 1, paste, collapse=";"), "exclude"] <- TRUE
  }
  
  dat$formattedData <- output
  
  # format tumor volume data
  # dat$volumevolumeData <- dcast(dat$rawData, mouseID ~ day, value.var="volume")
  # dat$rtvData <- dcast(dat$rawData, mouseID ~ day, value.var = "rtv")
  
  return(dat)
}

format.cns.data <- function(processed){
  if("data_comment" %in% names(processed) && sum(!is.na(processed$data_comment)) != 0){
    stop("Comment exists. Manually review the comment and delete to run pipeline.")
  }
  
  ### checking and keeping useful data ###
  dat <- list(project = list(),
              groupInfo = NULL,
              rawData = NULL,
              weight = NULL)
  #fileConn <- file("log.txt", 'w')
  
  # check unique-ness of panel_code, and keep as panel_code
  #check.column.unique(processed, "cns_panel_code", fileConn)
  panel.code <- unique(processed$cns_panel_code)
  panel.code <- strsplit(panel.code, "-")[[1]]
  dat$project[["panel_code"]] <- unique(processed$cns_panel_code)
  dat$project[["agent_code"]] <- panel.code[1]
  dat$project[["agent_name"]] <- project_code_to_name(dat$project[["agent_code"]]) #
  dat$project[["tumor_model"]] <- paste(panel.code[2:(length(panel.code)-1)], collapse = "-")
  
  dat$project$maxday <- max(processed$survival_time_days, na.rm=T)
  
  # check unique-ness of treatment_drug, and keep as panel_code
  #check.column.non.unique(processed, "treatment_drug", fileConn)
  # if(is.null(config$panel_code)){
  #   dat$project[["agent_code"]] <- unique(processed$treatment_drug)
  # }
  # dat$project[["agent_name"]] <- project_code_to_name(dat$project[["agent_code"]])
  
  # groupInfo, check later with mouse ID
  dat$groupInfo$group <- config$group$Group
  dat$groupInfo$dose_schedule <- config$group$newName
  dat$groupInfo$drug_dose <- config$group$newName

  # check unique-ness of mouse id and keep
  #{
    #writeLines("Treatment, mouse combinations:", fileConn)
    #tmp <- processed[,c("treatment_drug", "mouse_number")]
    #tmp <- table(tmp$treatment_drug)
    #writeLines(c("group", "number"), fileConn, sep="\t")
    #writeLines('',fileConn)
    #for (i in 1:dim(tmp)[1]){
      #writeLines(c(LETTERS[i], as.character(tmp[i])), 
      #           fileConn, sep="\t")
      #writeLines('',fileConn)
    #}
    #writeLines('#######################################################',fileConn)
  #}
  
  # check unique-ness of cns_model_id and keep as tumor_model
  #check.column.unique(processed, "cns_model_id", fileConn)
  #if (dat$project[["tumor_model"]] != unique(processed$cns_model_id)){
  #  writeLines("tumor panel code differ from tumor model:", fileConn)
  #  writeLines(dat$project[["tumor_model"]], fileConn)
  #  writeLines(unique(processed$cns_model_id), fileConn)
  #  writeLines('',fileConn)
  #  writeLines('#######################################################',fileConn)
  #}
  
  # check treatment date format and keep
  #check.column.date.format(processed, "date_treatment_started", fileConn)
  #check.column.unique(processed, "date_treatment_started", fileConn)
  dat$project[["treatment_date"]] <- as.Date(unique(processed$date_treatment_started), format = config$DateFormat)
  
  # check ended date format and keep
  #check.column.date.format(processed, "date_ended", fileConn)
  
  # keep mouseID, group, measure date altogether
  dat$rawData <- data.frame(mouseID = paste(config$group$Group[match(processed$treatment_drug, config$group$nameInTemp)], 
                                            processed$mouse_number, sep=";"),
                            group = config$group$Group[match(processed$treatment_drug, config$group$nameInTemp)],
                            measureDate = as.Date(processed$date_ended, format = config$DateFormat),
                            treatmentDate = as.Date(processed$date_treatment_started, format = config$DateFormat),
                            stringsAsFactors = FALSE)
  #check.column.duplicate(dat$rawData, "mouseID", fileConn)
  if (!is.null(processed$censored)) {
    dat$rawData$censored = processed$censored
  }
  
  # check days with survival time
  dat$rawData$day <- as.numeric(dat$rawData$measureDate - 
                                  as.Date(processed$date_treatment_started, config$DateFormat))
  #### dat$rawData$day <- as.numeric(dat$rawData$measureDate - 
  ####                                as.Date(processed$date_of_injection, config$DateFormat))
  day.from.injection <- as.numeric(dat$rawData$measureDate - as.Date(processed$date_of_injection, config$DateFormat))
  #{
  #  writeLines("Survival time difference for:", fileConn)
  #  for (i in which(day.from.injection != processed$survival_time_days)){
  #    writeLines(as.character(dat$rawData[i,]), fileConn, sep="\t")
  #    writeLines('',fileConn)
  #  }
  # writeLines('#######################################################',fileConn)
  #}

  #close(fileConn)
  
  ### checking complated, starting formatting ###
  dat$formattedData <- dat$rawData[,c("mouseID", "group")]

  # survival
  dat$formattedData$survival.time <- dat$rawData$day
  dat$formattedData$survival.event <- 1

  # set survival.event to 0 if censored
  dat$formattedData$survival.event[dat$rawData$censored] = 0
    
  # exclude the ones didn't survive the initial 24 hours, 
  # and the ones shouldn't be counted for survival analyses
  dat$formattedData$exclude <- ! (processed$survival_24hr & processed$survival_counted) 
  
  return(dat)
}

format.all.data <- function(processed){
  #processed<-x
  if("data_comment" %in% names(processed) && sum(!is.na(processed$data_comment)) != 0){
    stop("Comment exists. Manually review the comment and delete to run pipeline.")
  }

  ### checking and keeping useful data ###
  dat <- list(project = list(),
              groupInfo = NULL,
              rawData = NULL,
              weight = NULL)
  

  dat$project$panel_code <- unique(processed$panel_code)
  ## tmp <- panel_code_to_name(dat$project$panel_code)
  ## dat$project$agent_code <- tmp$agentCode
  ## dat$project$agent_name <- tmp$agentName

  dat$project[["panel_code"]] <- unique(processed$panel_code)
  dat$project$tumor_model <- unique(processed$description)

  # dose, schedule, group
  if (is.null(config$group)) {
    processed$group <- LETTERS[as.factor(processed$arm)]
  } else {
    processed$group <- gsub("\\)","",gsub("Treatment \\(","",processed$arm))
    processed$group <- gsub("\\)","",gsub("Control \\(","",processed$group))
  }
  tmp <- unique(processed[,c("all_dose", "all_schedule", "arm_testing", "group")])
  dat$groupInfo$group <- tmp$group
  dat$groupInfo$drug <- tmp$arm_testing
  dat$groupInfo$dose <- tmp$all_dose
  dat$groupInfo$schedule <- tmp$all_schedule
  dat$groupInfo$drug_dose <- paste0(dat$groupInfo$drug, " (", dat$groupInfo$dose, ")")
  dat$groupInfo$dose_schedule <- paste0(dat$groupInfo$dose, " (", dat$groupInfo$schedule, ")")

  processed$date_of_innoculation <- as.Date(processed$date_of_innoculation, format = config$DateFormat)
  dat$project$treatment_date <- as.Date(unique(processed$date_of_first_treatment), config$DateFormat)
  processed$date_of_first_treatment <- as.Date(processed$date_of_first_treatment, format = config$DateFormat)

  dat$project$treatment_end_date <- as.Date(unique(processed$date_of_treatment_completion), config$DateFormat)
  processed$date_of_treatment_completion <- as.Date(processed$date_of_treatment_completion, format = config$DateFormat)

  processed$date_of_randomization <- as.Date(processed$date_of_randomization, format = config$DateFormat)
  processed$date_of_bleed <- as.Date(processed$date_of_bleed, format = config$DateFormat)
  
  #tmp <- processed[,c("all_dose", "arm")]
  #tmp$dose_arm <- paste(tmp$all_dose, tmp$arm, sep = "_")
  
  #tmp <- processed[,c("all_schedule", "arm")]
  #tmp$schedule_arm <- paste(tmp$all_schedule, tmp$arm, sep = "_")
  
  # check order of event happening
  # tmp <- (as.Date(processed$date_of_randomization, format=config$DateFormat) >=
  #           as.Date(processed$date_of_innoculation, format=config$DateFormat)) &
  #   (as.Date(processed$date_of_treatment_completion, format=config$DateFormat) >=
  #      as.Date(processed$date_of_first_treatment, format=config$DateFormat)) &
  #   (as.Date(processed$date_of_first_treatment, format=config$DateFormat) >=
  #      as.Date(processed$date_of_innoculation, format=config$DateFormat)) &
  #   (as.Date(processed$date_of_bleed, format=config$DateFormat) >=
  #      as.Date(processed$date_of_first_treatment, format=config$DateFormat) - 1)
  # 
  
  processed$mouseID <- paste(processed$group, processed$all_mouse, sep = ";")

  

  # keep mouseID, group, measure date, and cd45 altogether
  dat$rawData <- data.frame(mouseID = paste(processed$group, processed$all_mouse, sep=";"),
                            group = processed$group,
                            measureDate = as.Date(processed$date_of_bleed, format = config$DateFormat),
                            cd45 = as.numeric(processed$cd45),
                            day = as.numeric(as.Date(processed$date_of_bleed, format = config$DateFormat) -
                                               as.Date(processed$date_of_first_treatment, format = config$DateFormat)),
                            treatment_end_date = as.Date(processed$date_of_treatment_completion, format = config$DateFormat),
                            stringsAsFactors = FALSE)

  # in case there is no date_of_treatment_completion
  # ifelse keeps converting date to numeric...
  # dat$rawData$treatment_end_date <- as.Date(ifelse(is.na(dat$rawData$treatment_end_date),
  #                                          as.Date(as.Date(processed$date_of_first_treatment, format = config$DateFormat) + 21,format="%Y-%m-%d"),
  #                                          dat$rawData$treatment_end_date), format="%Y-%m-%d")
  if (anyNA(dat$rawData$treatment_end_date)) {
    calc.treatment.end.date <- as.Date(as.Date(processed$date_of_first_treatment, format = config$DateFormat) + 21, format="%Y-%m-%d")  
    dat$rawData$treatment_end_date <- calc.treatment.end.date
  }
  
  
  
  
  dat$project$maxday <- max(dat$rawData$day, na.rm = T)
  if(sum(!is.na(processed$death_date)) > 0){
    dat$project$maxday <- max(dat$project$maxday, max(as.Date(processed$death_date, format = config$DateFormat) -
                                                        as.Date(processed$date_of_first_treatment, format = config$DateFormat),
                                                      na.rm = TRUE), na.rm = TRUE)
  }


  # check n
  tmp <- unique(processed[,c("arm", "all_mouse", "n")])
  summary1 <- aggregate(tmp$all_mouse, by=list(tmp$arm), length)
  summary2 <- aggregate(tmp$n, by=list(tmp$arm), unique)
  tmp <- merge(summary1, summary2, by="Group.1")
  

  # check thydym
  tmp <- unique(processed[,c("arm", "all_mouse", "code", "summary_thydym")])
  summary1 <- aggregate(tmp$code, by=list(tmp$arm, tmp$all_mouse), unique)
  names(summary1) <- c("arm", "all_mouse", "code")
  summary1$thydym <- summary1$code == "2"
  #  summary1 <- aggregate(summary1$thydym, by=list(summary1$arm), sum)
  summary1 <- aggregate(summary1$thydym, by=list(summary1$arm), function(x) sum(x,na.rm=TRUE))
  summary2 <- aggregate(tmp$summary_thydym, by=list(tmp$arm), unique)
  tmp <- merge(summary1, summary2, by="Group.1")
  

  # check toxic
  tmp <- unique(processed[,c("arm", "all_mouse", "code", "summary_toxic")])
  summary1 <- aggregate(tmp$code, by=list(tmp$arm, tmp$all_mouse), unique)
  names(summary1) <- c("arm", "all_mouse", "code")
  summary1$toxic <- summary1$code == "1"
  #  summary1 <- aggregate(summary1$toxic, by=list(summary1$arm), sum)
  summary1 <- aggregate(summary1$toxic, by=list(summary1$arm), function(x) sum(x,na.rm=TRUE))
  summary2 <- aggregate(tmp$summary_toxic, by=list(tmp$arm), unique)
  tmp <- merge(summary1, summary2, by="Group.1")
  

  # check additional exclusion
  tmp <- unique(processed[,c("arm", "all_mouse", "code")])
  summary1 <- aggregate(tmp$code == "4", by = list(tmp$arm), sum)
  names(summary1) <- c("arm", "summary_addEx")
  processed$summary_addEx <- summary1$summary_addEx[match(processed$arm, summary1$arm)]
  

  # check evaluable
  # tmp <- processed$n - processed$summary_thydym - processed$summary_toxic -
  #   processed$summary_evaluable - processed$summary_addEx
  

  # formatting data tables

  # construct output
  output <- unique(processed[,c('all_mouse', 'group', 'code')])
  names(output) <- c("mouseID", "group", "code")
  output$mouseID <- paste(output$group, output$mouseID, sep = ";")
  output$toxic <- output$code == "1"
  output$exclude <- output$code %in% c("1", "4")
  output$survival.time <- NA
  output$survival.event <- NA

  # fill in Days and Events
  for (mouse in output$mouseID){
    if("death_date" %in% names(processed)){
      tmp <- processed[processed$mouseID == mouse, c('date_of_bleed', 'cd45', 'code', 'date_of_first_treatment', 'death_date')]
    } else {
      tmp <- processed[processed$mouseID == mouse, c('date_of_bleed', 'cd45', 'code', 'date_of_first_treatment')]
    }
    tmp <- tmp[order(tmp$date_of_bleed),]

    ### SWE 2019-12-27
	# added to calculate mincd45 and baselinecd45
    tmp$daytmp = as.numeric(tmp$date_of_bleed) - as.numeric(tmp$date_of_first_treatment)
	  tmp$daytmp[tmp$daytmp < 0] = 0
	#mincd45 is min cd45 after day 3
	#baselinecd45 is min cd45 before day 3
  	# if there is only one timepoint, and it is < 3, then mincd45 = baselinecd45
	if (max(tmp$daytmp) > 3) {
		output[output$mouseID == mouse, 'mincd45'] = min(tmp$cd45[tmp$daytmp > 3])
		output[output$mouseID == mouse, 'baselinecd45'] = min(tmp$cd45[tmp$daytmp < 3])	
	}
	if (max(tmp$daytmp) < 3) {
		output[output$mouseID == mouse, 'mincd45'] = min(tmp$cd45[tmp$daytmp < 3])	
		output[output$mouseID == mouse, 'baselinecd45'] = min(tmp$cd45[tmp$daytmp < 3])	
	}
    ###
   
    if(max(tmp$cd45) < 25 & max(unique(tmp$code),0,na.rm=TRUE) != 3){ # no Event
      output[output$mouseID == mouse, 'survival.time'] <-
        as.numeric(tmp$date_of_bleed[dim(tmp)[1]] - dat$project$treatment_date)
      output[output$mouseID == mouse, 'survival.event'] <- 0
	  output[output$mouseID == mouse, 'aoc'] <- sum(diff(tmp$daytmp)*((25-tmp$cd45[-1])+(25-tmp$cd45[-length(tmp$cd45)]))/2)
    } else if(unique(tmp$code == 3)){ # Leukemia/Thymoma
      output[output$mouseID == mouse, 'survival.time'] <-
        unique(as.Date(tmp$death_date, format = config$DateFormat) - tmp$date_of_first_treatment)
      output[output$mouseID == mouse, 'survival.event'] <- 1
	  output[output$mouseID == mouse, 'aoc'] <- sum(diff(tmp$daytmp)*((25-tmp$cd45[-1])+(25-tmp$cd45[-length(tmp$cd45)]))/2)    
    }else {# Event = 1
      index <- min(which(tmp$cd45 >= 25)) # index of first cd45 > 25%
      if(index == 1){
        warning("First time point cd45 exceeds 25%.")
      } else {
        output[output$mouseID == mouse, 'survival.time'] <-
          (log(25) - log(tmp$cd45[index-1])) * (tmp$date_of_bleed[index] - tmp$date_of_bleed[index-1]) / (log(tmp$cd45[index]) - log(tmp$cd45[index-1])) +
          (tmp$date_of_bleed[index-1] - tmp$date_of_first_treatment[1])
        output[output$mouseID == mouse, 'survival.event'] <- 1
		tmp$cd45tmp = tmp$cd45
		tmp$cd45tmp[tmp$cd45tmp > 25] = 25
		tmp$daytmp[tmp$cd45tmp == 25] = output[output$mouseID == mouse, 'survival.time']
		output[output$mouseID == mouse, 'aoc'] <- sum(diff(tmp$daytmp)*((25-tmp$cd45tmp[-1])+(25-tmp$cd45tmp[-length(tmp$cd45tmp)]))/2)
      }
    }
  }
  output$aoc = output$aoc/25
### SWE

  dat$formattedData <- output

  return(dat)
}


format.all.single.mouse.data <- function(processed){
  #processed<-x
  if("data_comment" %in% names(processed) && sum(!is.na(processed$data_comment)) != 0){
    stop("Comment exists. Manually review the comment and delete to run pipeline.")
  }
  
  ### checking and keeping useful data ###
  dat <- list(project = list(),
              groupInfo = NULL,
              rawData = NULL,
              weight = NULL)
  
  
  dat$project$panel_code <- unique(paste0(processed$panel_code, "_" ,processed$testing))
  #tmp <- panel_code_to_name(dat$project$panel_code)
  #dat$project$agent_code <- tmp$agentCode
  #dat$project$agent_name <- tmp$agentName
  dat$project$agent_code <- unique(processed$testing)
  dat$project$agent_name <- unique(processed$testing)
  
  
  
  
  
  #dat$project[["panel_code"]] <- unique(processed$panel_code)
  #dat$project$tumor_model <- unique(processed$description)
  
  
  # dose, schedule, group
  #if (is.null(config$group)) {
  #  processed$group <- LETTERS[as.factor(processed$arm)]
  #} else {
  #  processed$group <- gsub("\\)","",gsub("Treatment \\(","",processed$arm))
  #  processed$group <- gsub("\\)","",gsub("Control \\(","",processed$group))
  #}
  #tmp <- unique(processed[,c("all_dose", "all_schedule", "arm_testing", "group")])
  
  tmp <- unique(processed[,c("all_dose","all_schedule","arm_testing","pdx_id")])
  tmp
  
  dat$groupInfo$group <- tmp$pdx_id
  dat$groupInfo$drug <- tmp$arm_testing
  dat$groupInfo$dose <- tmp$all_dose
  dat$groupInfo$schedule <- tmp$all_schedule
  dat$groupInfo$drug_dose <- paste0(dat$groupInfo$drug, " (", dat$groupInfo$dose, ")")
  dat$groupInfo$dose_schedule <- paste0(dat$groupInfo$dose, " (", dat$groupInfo$schedule, ")")
  # this may need to be revisited someday
  dat$groupInfo$date_of_treatment_completion <- processed$date_of_treatment_completion
  dat$groupInfo$date_of_first_treatment <- processed$date_of_first_treatment

  
  processed$date_of_innoculation <- as.Date(processed$date_of_innoculation, format = config$DateFormat)
  dat$project$treatment_date <- as.Date(unique(processed$day_0_date), config$DateFormat)
  processed$date_of_first_treatment <- as.Date(processed$day_0_date, format = config$DateFormat)
  
  
  if(is.null(processed$treatment_end_date) || (length(unique(processed$treatment_end_date)) == 1 && unique(processed$treatment_end_date) == "")){ #this if statement makes no sense to me... EJE Aug 27, 2019
    if (is.null(processed$treatment_date)) {
      processed$treatment_end_date <- as.Date(processed$day_0_date, format = config$DateFormat) + 21
      warning(paste0("Set treatment end date as: ",
                     paste(unique(processed$treatment_end_date), collapse = "; ")))
    } else {
      processed$treatment_end_date <- as.Date(processed$treatment_date, format = config$DateFormat) + 21
      warning(paste0("Set treatment end date as: ",
                     paste(unique(processed$treatment_end_date), collapse = "; ")))  
    }
    
  } else {
    dat$project$treatment_end_date <- as.Date(unique(processed$date_of_treatment_completion), config$DateFormat)
    processed$date_of_treatment_completion <- as.Date(processed$date_of_treatment_completion, format = config$DateFormat)  
  }
  
  
  
  processed$date_of_randomization <- as.Date(processed$date_of_randomization, format = config$DateFormat)
  processed$date_of_bleed <- as.Date(processed$date_of_bleed, format = config$DateFormat)
  processed$mouseID <- paste(processed$pdx_id, processed$all_mouse, sep = ";")
  if (sum(!is.na(processed$day_offset) == 0)) {
    processed$day_offset = as.numeric(processed$date_of_bleed) - as.numeric(processed$date_of_first_treatment)
  }
  ## keep mouseID, group, measure date, and cd45 altogether
  dat$rawData <- data.frame(mouseID = paste(processed$pdx_id, processed$all_mouse, sep=";"),
                            group = processed$pdx_id,
                            measureDate = as.Date(processed$date_of_bleed, format = config$DateFormat), #lab providing day on their own
                            cd45 = as.numeric(processed$cd45),
                            day = as.numeric(processed$day_offset),
                            treatment_end_date = as.Date(processed$date_of_treatment_completion, format = config$DateFormat), #not present
                            stringsAsFactors = FALSE)
  
  dat$project$maxday <- max(dat$rawData$day, na.rm = T)
  # if(sum(!is.na(processed$death_date)) > 0){ # there is no date_of_first_treatment, just day_offset
  #   dat$project$maxday <- max(dat$project$maxday, max(as.Date(processed$death_date, format = config$DateFormat) -
  #                                                       as.Date(processed$date_of_first_treatment, format = config$DateFormat),
  #                                                     na.rm = TRUE), na.rm = TRUE)
  # }
  
  
  # check n
  # tmp <- unique(processed[,c("arm", "all_mouse", "n")])
  # summary1 <- aggregate(tmp$all_mouse, by=list(tmp$arm), length)
  # summary2 <- aggregate(tmp$n, by=list(tmp$arm), unique)
  # tmp <- merge(summary1, summary2, by="Group.1")
  # 
  
  # check thydym
  # tmp <- unique(processed[,c("arm", "all_mouse", "code", "summary_thydym")])
  # summary1 <- aggregate(tmp$code, by=list(tmp$arm, tmp$all_mouse), unique)
  # names(summary1) <- c("arm", "all_mouse", "code")
  # summary1$thydym <- summary1$code == "2"
  # #  summary1 <- aggregate(summary1$thydym, by=list(summary1$arm), sum)
  # summary1 <- aggregate(summary1$thydym, by=list(summary1$arm), function(x) sum(x,na.rm=TRUE))
  # summary2 <- aggregate(tmp$summary_thydym, by=list(tmp$arm), unique)
  # tmp <- merge(summary1, summary2, by="Group.1")
  
  
  # check toxic
  tmp <- unique(processed[,c("pdx_id", "all_mouse", "code", "summary_toxic")]) 
  summary1 <- aggregate(tmp$code, by=list(tmp$pdx_id, tmp$all_mouse), unique)
  names(summary1) <- c("pdx_id", "all_mouse", "code")
  summary1$toxic <- summary1$code == "1"
  summary1 <- aggregate(summary1$toxic, by=list(summary1$pdx_id), function(x) sum(x,na.rm=TRUE))
  summary2 <- aggregate(tmp$summary_toxic, by=list(tmp$pdx_id), unique)
  tmp <- merge(summary1, summary2, by="Group.1")
  
  
  # check additional exclusion
  tmp <- unique(processed[,c("pdx_id", "all_mouse", "code")])
  summary1 <- aggregate(tmp$code == "4", by = list(tmp$pdx_id), sum)
  names(summary1) <- c("pdx_id", "summary_addEx")
  processed$summary_addEx <- summary1$summary_addEx[match(processed$pdx_id, summary1$pdx_id)]
  
  
  # check evaluable
  # tmp <- processed$n - processed$summary_thydym - processed$summary_toxic -
  #   processed$summary_evaluable - processed$summary_addEx
  
  
  # formatting data tables
  
  # construct output
  output <- unique(processed[,c('all_mouse', 'pdx_id', 'code')])
  names(output) <- c("mouseID", "group", "code")
  output$mouseID <- paste(output$group, output$mouseID, sep = ";")
  output$toxic <- output$code == "1"
  output$exclude <- output$code %in% c("1", "4")
  output$survival.time <- NA
  output$survival.event <- NA
  
  # fill in Days and Events
  for (mouse in output$mouseID){
    if("death_date" %in% names(processed)){
      tmp <- processed[processed$mouseID == mouse, c('day_offset', 
                                                     'cd45', 
                                                     'code', 
                                                     'date_of_first_treatment',
                                                     'death_date')]
    } else {
      tmp <- processed[processed$mouseID == mouse, c('day_offset', 'cd45', 'code')]
    }
    tmp <- tmp[order(tmp$day_offset),]
### SWE 2019-12-27
    tmp$daytmp = tmp$day_offset
	tmp$daytmp[tmp$daytmp < 0] = 0
	output[output$mouseID == mouse, 'mincd45'] = min(tmp$cd45[tmp$day_offset > 3])
	output[output$mouseID == mouse, 'baselinecd45'] = min(tmp$cd45[tmp$daytmp < 3])	
	
	#tmp$daytmp = as.numeric(tmp$date_of_bleed) - as.numeric(tmp$date_of_first_treatment)
	#tmp$daytmp[tmp$daytmp < 0] = 0
  	#output[output$mouseID == mouse, 'mincd45'] = min(tmp$cd45[tmp$daytmp > 3])
  	
##    tmp
    if(max(tmp$cd45) < 25 & max(unique(tmp$code),0,na.rm=TRUE) != 3){ # no Event
      output[output$mouseID == mouse, 'survival.time'] <-
        #as.numeric(tmp$date_of_bleed[dim(tmp)[1]] - dat$project$treatment_date)
        max(tmp$day_offset)
      output[output$mouseID == mouse, 'survival.event'] <- 0
      output[output$mouseID == mouse, 'aoc'] <- sum(diff(tmp$daytmp)*((25-tmp$cd45[-1])+(25-tmp$cd45[-length(tmp$cd45)]))/2)
    } else if(unique(tmp$code == 3)){ # Leukemia/Thymoma
      output[output$mouseID == mouse, 'survival.time'] <-
        unique(as.Date(tmp$death_date, format = config$DateFormat) - tmp$date_of_first_treatment)
        #max(tmp$day_offset)
      output[output$mouseID == mouse, 'survival.event'] <- 1
      output[output$mouseID == mouse, 'aoc'] <- sum(diff(tmp$daytmp)*((25-tmp$cd45[-1])+(25-tmp$cd45[-length(tmp$cd45)]))/2)
    }else {# Event = 1
      index <- min(which(tmp$cd45 >= 25)) # index of first cd45 > 25%
      if(index == 1){
        warning("First time point cd45 exceeds 25%.")
      } else {
        output[output$mouseID == mouse, 'survival.time'] <-
          (log(25) - log(tmp$cd45[index-1])) * (tmp$day_offset[index] - tmp$day_offset[index-1]) / (log(tmp$cd45[index]) - log(tmp$cd45[index-1])) +
          (tmp$day_offset[index-1]) ## - tmp$day_offset[1])
        output[output$mouseID == mouse, 'survival.event'] <- 1
		tmp$cd45tmp = tmp$cd45
		tmp$cd45tmp[tmp$cd45tmp > 25] = 25
		tmp$daytmp[tmp$cd45tmp == 25] = output[output$mouseID == mouse, 'survival.time']
		output[output$mouseID == mouse, 'aoc'] <- sum(diff(tmp$daytmp)*((25-tmp$cd45tmp[-1])+(25-tmp$cd45tmp[-length(tmp$cd45tmp)]))/2)
      }
    }
  }
  output$aoc = output$aoc/25  
### SWE
  
  dat$formattedData <- output
  
  return(dat)
}  






format.all.weight <- function(all.data, weight){
  dat <- all.data
  if (is.null(config$group)) {  
    dat$weight <- data.frame(mouseID = weight$all_mouse,
                           group = LETTERS[as.factor(weight$arm)],
                           measureDate = as.Date(weight$date_of_weight, format = config$DateFormat),
                           days = as.numeric(as.Date(weight$date_of_weight, format = config$DateFormat) - as.Date(weight$date_of_first_treatment, format = config$DateFormat)),
                           weight = weight$all_weight,
                           stringsAsFactors = FALSE)
  } else {
    weight$group <- gsub("\\)","",gsub("Treatment \\(","",weight$arm))
    weight$group <- gsub("\\)","",gsub("Control \\(","",weight$group))
    dat$weight <- data.frame(mouseID = weight$all_mouse,
                           #group = config$group$Group[match(weight$arm, config$group$nameInTemp)],
                           group = weight$group,
                           measureDate = as.Date(weight$date_of_weight, format = config$DateFormat),
                           days = as.numeric(as.Date(weight$date_of_weight, format = config$DateFormat) - as.Date(weight$date_of_first_treatment, format = config$DateFormat)),
                           weight = weight$all_weight,
                           stringsAsFactors = FALSE)
  }						   
  return(dat)
}

format.cns.weight <- function(cns.data, weight){
  #weight<-w
  #cns.data <- dat
  dat <- cns.data
  
  # weight$cns_mouse is a list of dates... that doesn't seem like what "mouseID" should be. Reset this to be mouse_number (EJE - Dec 2018)
  # dat$weight <- data.frame(mouseID = weight$cns_mouse,
  #                          measureDate = as.Date(weight$cns_mouse_date, format = config$WeightDateFormat),
  #                          weight = weight$cns_weight,
  #                          stringsAsFactors = FALSE)
  dat$weight <- data.frame(mouseID = weight$mouse_number,
                           measureDate = as.Date(weight$cns_mouse_date, format = config$WeightDateFormat),
                           weight = weight$cns_weight,
                           stringsAsFactors = FALSE)
  if("cns_group" %in% names(weight)) {
  #if("group" %in% names(weight)) {
    dat$weight$group <- weight$cns_group
    #dat$weight$cns_group <- weight$cns_group
  } else {
    dat$weight$group = LETTERS[as.factor(weight$treatment_scheme)]
  }
  dat$weight$days = as.numeric(as.Date(weight$cns_mouse_date, format = config$WeightDateFormat) - 
                                 dat$rawData$treatmentDate[match(dat$weight$group, dat$rawData$group)])
  if (sum(is.na(dat$weight$days)) > 0 ) {
    warning("some days are NA. Check for date formatting issues")
  }
  
  if (sum(dat$weight$days < 0) > 0) {
    warning("Some days are negative for weight. Calculation is 'weight$cns_mouse_date' - 'dat$rawData$treatmentDate' ")
  }
  
  # dat$weight <- dat$weight[ !grepl("dead", dat$weight$weight), ]
  dat$weight$weight <- as.numeric(dat$weight$weight)
  return(dat)
}
