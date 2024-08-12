require(XLConnect, quietly = TRUE, warn.conflicts = FALSE)
output.solid.tables <- function(dat, agent_code, tumor_model, wd, sheet="SummaryTables"){
  require(XLConnect)
  #panel code will be the file name, but need to replace the first '-' with a '_'
  filename<-sub("-","_",dat$project$panel_code)
  
  tableTemplate <- paste0(resources.dir,'tableTemplate_solid_v5.xlsx')
  file.copy(tableTemplate, paste0(config$wd,"/summary.xlsx"), overwrite=TRUE)
  wb  <- loadWorkbook(paste0(config$wd,"/summary.xlsx"))
  # setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
  setStyleAction(wb,XLC$"STYLE_ACTION.PREDEFINED")
  
  # table
  writeWorksheet(wb, dat$table, sheet=sheet,     # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=3, startCol=1,            # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  # footnote, if any
  writeWorksheet(wb, config$table.footnote,
                 sheet=sheet,     # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=3+length(config$control) + length(config$table.comparisons),
                 startCol=1,            # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  # treatment groups
  writeWorksheet(wb, paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "),
                 sheet=sheet,     # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=4+length(config$control) + length(config$table.comparisons),
                 startCol=1,            # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  # time table generated
  writeWorksheet(wb, paste0("Tables generated at: ", Sys.time()),
                 sheet=sheet,     # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=4+length(config$control) + 
                   length(config$table.comparisons) + length(dat$groupInfo$group),
                 startCol=1,            # specify the cell to write to
                 header=FALSE)                      # do not write a header row

  saveWorkbook(wb)
  
  # # csv output for KM - EXCEL FORMAT
  #write.csv(kmdat,file=paste0("KM_forExcel.csv"),row.names=FALSE,na="")
  if (!file.exists("../CSV")) {
    dir.create("../CSV")
    print("created directory ../CSV")
  }
  kmdat = dat$survival.plot$data[,c("time","surv","cens","group")]
  kmdat$group = as.character(kmdat$group)
  kmdattmp = NULL
  for (grp in unique(kmdat$group)) {   #group by group
    tmp1 = kmdat[kmdat$group==grp,]    
    tmp2 = tmp1
    tmp3 = tmp2[-1,]  #
    tmp3$surv = tmp2$surv[1:dim(tmp3)[1]]
    tmp3 = rbind(tmp1,tmp3)
    kmdattmp = rbind(kmdattmp,tmp3[order(tmp3$time,-tmp3$surv),])  
    rm(tmp1,tmp2,tmp3)
  }
  kmdat = kmdattmp
  kmdat$grptmp = NULL
  rm(kmdattmp,grp)  
  for (grp in unique(kmdat$group)) {
    kmdat[kmdat$group==grp,grp] = kmdat[kmdat$group==grp,"surv"]
  }
  kmdat$days = kmdat$time
  kmdat$Days = kmdat$days
  kmdat$Censored = kmdat$cens
  kmdat = kmdat[,c("days",unique(kmdat$group),"cens")]
  write.csv(kmdat,file=paste0("../CSV/",filename,".KM_forExcel.csv"),row.names=FALSE,na="")
  
  # # csv output for KM - GRAPHPAD FORMAT
  t2e = data.frame(Days = dat$formattedData$survival.time,
                   Event = dat$formattedData$survival.event,
                   Group = paste0("",dat$formattedData$group))
  groups = names(table(t2e$Group))
  for (nm in groups) {
    t2e[,nm] = NA
    t2e[t2e$Group==nm,nm] = t2e$Event[t2e$Group==nm]
  }
  t2e$Event = NULL
  t2e$Group = NULL
  write.csv(t2e,file=paste0("../CSV/",filename,".KM_forGraphPad.csv"),na="",row.names=FALSE)
  
  
  # # csv for Tumor Volume
  volume = dat$volume.median
  volume$mouseID = paste0(volume$group,";median")
  volume = rbind(volume,dat$volume.plot$data)
  volume$Days = as.numeric(gsub(" days","",volume$day))
  rout = data.frame(Days = sort(unique(volume$Days)))
  for (mouse in sort(unique(volume$mouseID))) {
    tmp = data.frame(Days=volume$Days[volume$mouseID==mouse])
    tmp[,mouse] = volume$volume[volume$mouseID==mouse]
    rout = merge(rout,tmp,all.x=TRUE,by="Days")
    rm(tmp)
  }
  #write.csv(rout,file="TumorVolume.csv",na="",row.names=FALSE)
  if (!file.exists("../CSV")) {
    dir.create("../CSV")
    print("created directory ../CSV")
  }
  write.csv(rout,file=paste0("../CSV/",filename,".TumorVolume.csv"),row.names=FALSE,na="")
  
  
  # # csv for RTV
  rtv = dat$rtv.median
  rtv$mouseID = paste0(rtv$group,";median")
  rtv = rbind(rtv,dat$rtv.plot$data)
  rtv$Days = as.numeric(gsub(" days","",rtv$day))
  rout = data.frame(Days = sort(unique(rtv$Days)))
  for (mouse in sort(unique(rtv$mouseID))) {
    tmp = data.frame(Days=rtv$Days[rtv$mouseID==mouse])
    tmp[,mouse] = rtv$rtv[rtv$mouseID==mouse]
    rout = merge(rout,tmp,all.x=TRUE,by="Days")
    rm(tmp)
  }
  #write.csv(rout,file="RTV.csv",na="",row.names=FALSE)
  if (!file.exists("../CSV")) {
    dir.create("../CSV")
    print("created directory ../CSV")
  }
  write.csv(rout,file=paste0("../CSV/",filename,".RTV.csv"),row.names=FALSE,na="")
  
  
  
  #table2 output
  #write.csv(dat$table2, file="summary2.csv",row.names=F, quote=F) #don't use csv since schedules may have commas
  write.table(dat$table2, file="summary2.txt",row.names=F,col.names=T,quote=F,sep="\t")
  
}





output.ALL.tables <- function(dat, wd){
  require(XLConnect)
  
  filename<-sub("-","_",dat$project$panel_code)
  
  tableTemplate <- paste0(resources.dir,'tableTemplate_A.L.L._v5.xlsx')
  file.copy(tableTemplate, paste0(config$wd,"/summary.xlsx"), overwrite=TRUE)
  wb  <- loadWorkbook(paste0(config$wd,"/summary.xlsx"))
  setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
  
  # table
  writeWorksheet(wb, dat$table, sheet="SummaryTables",     # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=3, startCol=1,            # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  # footnote, if any
  writeWorksheet(wb, config$table.footnote,
                 sheet="SummaryTables",                    # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=3+length(config$control) + length(config$table.comparisons),
                 startCol=1,                        # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  # # treatment groups
  writeWorksheet(wb, paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "),
                 sheet="SummaryTables",                    # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=4+length(config$control) + length(config$table.comparisons),
                 startCol=1,                        # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  # # time table generated
  writeWorksheet(wb, paste0("Tables generated at: ", Sys.time()),
                 sheet="SummaryTables",                    # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=4+length(unique(dat$groupInfo$group)) +length(config$control) + length(config$table.comparisons),
                 startCol=1,                        # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  saveWorkbook(wb)
  
  
  # # csv output for KM - EXCEL FORMAT
  #write.csv(kmdat,file=paste0("KM_forExcel.csv"),row.names=FALSE,na="")
  if (!file.exists("../CSV")) {
    dir.create("../CSV")
    print("created directory ../CSV")
  }
  kmdat = dat$survival.plot$data[,c("time","surv","cens","group")]
  kmdat$group = as.character(kmdat$group)
  kmdattmp = NULL
  for (grp in unique(kmdat$group)) {   #group by group
    tmp1 = kmdat[kmdat$group==grp,]    
    tmp2 = tmp1
    tmp3 = tmp2[-1,]  #
    tmp3$surv = tmp2$surv[1:dim(tmp3)[1]]
    tmp3 = rbind(tmp1,tmp3)
    kmdattmp = rbind(kmdattmp,tmp3[order(tmp3$time,-tmp3$surv),])  
    rm(tmp1,tmp2,tmp3)
  }
  kmdat = kmdattmp
  kmdat$grptmp = NULL
  rm(kmdattmp,grp)  
  for (grp in unique(kmdat$group)) {
    kmdat[kmdat$group==grp,grp] = kmdat[kmdat$group==grp,"surv"]
  }
  kmdat$days = kmdat$time
  kmdat$Days = kmdat$days
  kmdat$Censored = kmdat$cens
  kmdat = kmdat[,c("days",unique(kmdat$group),"cens")]
  write.csv(kmdat,file=paste0("../CSV/",filename,".KM_forExcel.csv"),row.names=FALSE,na="")
  
  # # csv output for KM - GRAPHPAD FORMAT
  t2e = data.frame(Days = dat$formattedData$survival.time,
                   Event = dat$formattedData$survival.event,
                   Group = paste0("",dat$formattedData$group))
  groups = names(table(t2e$Group))
  for (nm in groups) {
    t2e[,nm] = NA
    t2e[t2e$Group==nm,nm] = t2e$Event[t2e$Group==nm]
  }
  t2e$Event = NULL
  t2e$Group = NULL
  write.csv(t2e,file=paste0("../CSV/",filename,".KM_forGraphPad.csv"),na="",row.names=FALSE)
  
  # # csv output for CD45
  cd45 = dat$cd45.median
  cd45$mouseID = paste0(cd45$group,";median")
  cd45 = rbind(cd45,dat$cd45.plot$data)
  cd45$Days = as.numeric(gsub(" days","",cd45$day))
  rout = data.frame(Days = sort(unique(cd45$Days)))
  for (mouse in sort(unique(cd45$mouseID))) {
    tmp = data.frame(Days=cd45$Days[cd45$mouseID==mouse])
    tmp[,mouse] = cd45$cd45[cd45$mouseID==mouse]
    rout = merge(rout,tmp,all.x=TRUE,by="Days")
    rm(tmp)
  }
  #write.csv(rout,file="CD45.csv",na="",row.names=FALSE)
  if (!file.exists("../CSV")) {
    dir.create("../CSV")
    print("created directory ../CSV")
  }
  write.csv(rout,file=paste0("../CSV/",filename,".CD45.csv"),row.names=FALSE,na="")
  
  
  
  #table2 output
  #write.csv(dat$table2, file="summary2.csv",row.names=F, quote=F) #don't use csv since schedules may have commas
  write.table(dat$table2, file="summary2.txt",row.names=F,col.names=T,quote=F,sep="\t")
  
  

}


output.ALL.single.mouse.tables <- function(dat, wd){
  require(XLConnect)
  
  filename<-sub("-","_",dat$project$panel_code)
  
  tableTemplate <- paste0(resources.dir,'tableTemplate_A.L.LsingleMouse._v1.xlsx')
  file.copy(tableTemplate, paste0(config$wd,"summary.xlsx"), overwrite=TRUE)
  wb  <- loadWorkbook(paste0(config$wd,"summary.xlsx"))
  setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
  
  # table
  writeWorksheet(wb, dat$table, sheet="SummaryTables",     # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=2, startCol=1,            # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  # footnote, if any
  # writeWorksheet(wb, config$table.footnote,
  #                sheet="SummaryTables",                    # write outtab to sheet 'Sheet1' in workbook 'wb'
  #                startRow=3+length(config$control) + length(config$table.comparisons),
  #                startCol=1,                        # specify the cell to write to
  #                header=FALSE)                      # do not write a header row
  
  # # treatment groups
  # this assumes one drug at the same dose for all models.
  writeWorksheet(wb, unique(dat$groupInfo$drug_dose),
                 sheet="SummaryTables",             # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=2 + nrow(dat$table),
                 startCol=1,                        # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  # # time table generated
  writeWorksheet(wb, paste0("Tables generated at: ", Sys.time()),
                 sheet="SummaryTables",                    # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=2 + nrow(dat$table) + length(unique(dat$groupInfo$drug_dose)),
                 startCol=1,                        # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  saveWorkbook(wb)
  
  
  # # csv output for KM - EXCEL FORMAT
  #write.csv(kmdat,file=paste0("KM_forExcel.csv"),row.names=FALSE,na="")
  # if (!file.exists("../CSV")) {
  #   dir.create("../CSV")
  #   print("created directory ../CSV")
  # }
  # kmdat = dat$survival.plot$data[,c("time","surv","cens","group")]
  # kmdat$group = as.character(kmdat$group)
  # kmdattmp = NULL
  # for (grp in unique(kmdat$group)) {   #group by group
  #   tmp1 = kmdat[kmdat$group==grp,]    
  #   tmp2 = tmp1
  #   tmp3 = tmp2[-1,]  #
  #   tmp3$surv = tmp2$surv[1:dim(tmp3)[1]]
  #   tmp3 = rbind(tmp1,tmp3)
  #   kmdattmp = rbind(kmdattmp,tmp3[order(tmp3$time,-tmp3$surv),])  
  #   rm(tmp1,tmp2,tmp3)
  # }
  # kmdat = kmdattmp
  # kmdat$grptmp = NULL
  # rm(kmdattmp,grp)  
  # for (grp in unique(kmdat$group)) {
  #   kmdat[kmdat$group==grp,grp] = kmdat[kmdat$group==grp,"surv"]
  # }
  # kmdat$days = kmdat$time
  # kmdat$Days = kmdat$days
  # kmdat$Censored = kmdat$cens
  # kmdat = kmdat[,c("days",unique(kmdat$group),"cens")]
  # write.csv(kmdat,file=paste0("../CSV/",filename,".KM_forExcel.csv"),row.names=FALSE,na="")
  # 
  # # # csv output for KM - GRAPHPAD FORMAT
  # t2e = data.frame(Days = dat$formattedData$survival.time,
  #                  Event = dat$formattedData$survival.event,
  #                  Group = paste0("",dat$formattedData$group))
  # groups = names(table(t2e$Group))
  # for (nm in groups) {
  #   t2e[,nm] = NA
  #   t2e[t2e$Group==nm,nm] = t2e$Event[t2e$Group==nm]
  # }
  # t2e$Event = NULL
  # t2e$Group = NULL
  # write.csv(t2e,file=paste0("../CSV/",filename,".KM_forGraphPad.csv"),na="",row.names=FALSE)
  # 
  # # # csv output for CD45
  # cd45 = dat$cd45.median
  # cd45$mouseID = paste0(cd45$group,";median")
  # cd45 = rbind(cd45,dat$cd45.plot$data)
  # cd45$Days = as.numeric(gsub(" days","",cd45$day))
  # rout = data.frame(Days = sort(unique(cd45$Days)))
  # for (mouse in sort(unique(cd45$mouseID))) {
  #   tmp = data.frame(Days=cd45$Days[cd45$mouseID==mouse])
  #   tmp[,mouse] = cd45$cd45[cd45$mouseID==mouse]
  #   rout = merge(rout,tmp,all.x=TRUE,by="Days")
  #   rm(tmp)
  # }
  # #write.csv(rout,file="CD45.csv",na="",row.names=FALSE)
  # if (!file.exists("../CSV")) {
  #   dir.create("../CSV")
  #   print("created directory ../CSV")
  # }
  # write.csv(rout,file=paste0("../CSV/",filename,".CD45.csv"),row.names=FALSE,na="")
  
}






output.CNS.tables <- function(dat, wd){
  require(XLConnect)
  #panel code will be the file name, but need to replace the first '-' with a '_'
  filename<-sub("-","_",dat$project$panel_code)
  
  tableTemplate <- paste0(resources.dir,'tableTemplate_CNS_v2.xlsx')
  file.copy(tableTemplate, paste0(config$wd,"/summary.xlsx"), overwrite=TRUE)
  wb  <- loadWorkbook(paste0(config$wd,"/summary.xlsx"))
  setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
  
  # table
  writeWorksheet(wb, dat$table, sheet="SummaryTables",     # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=3, startCol=1,            # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  # footnote, if any
  writeWorksheet(wb, config$table.footnote,
                 sheet="SummaryTables",                    # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=3+length(config$control) + length(config$table.comparisons),
                 startCol=1,                        # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  # # treatment groups
  writeWorksheet(wb, paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "),
                 sheet="SummaryTables",                    # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=4+length(config$control) + length(config$table.comparisons),
                 startCol=1,                        # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  # # time table generated
  writeWorksheet(wb, paste0("Tables generated at: ", Sys.time()),
                 sheet="SummaryTables",                    # write outtab to sheet 'Sheet1' in workbook 'wb'
                 startRow=4+length(unique(dat$groupInfo$group)) +length(config$control) + length(config$table.comparisons),
                 startCol=1,                        # specify the cell to write to
                 header=FALSE)                      # do not write a header row
  saveWorkbook(wb)
  
  # # csv output for KM - EXCEL FORMAT
  #write.csv(kmdat,file=paste0("KM_forExcel.csv"),row.names=FALSE,na="")
  if (!file.exists("../CSV")) {
    dir.create("../CSV")
    print("created directory ../CSV")
  }
  kmdat = dat$survival.plot$data[,c("time","surv","cens","group")]
  kmdat$group = as.character(kmdat$group)
  kmdattmp = NULL
  for (grp in unique(kmdat$group)) {   #group by group
    tmp1 = kmdat[kmdat$group==grp,]    
    tmp2 = tmp1
    tmp3 = tmp2[-1,]  #
    tmp3$surv = tmp2$surv[1:dim(tmp3)[1]]
    tmp3 = rbind(tmp1,tmp3)
    kmdattmp = rbind(kmdattmp,tmp3[order(tmp3$time,-tmp3$surv),])  
    rm(tmp1,tmp2,tmp3)
  }
  kmdat = kmdattmp
  kmdat$grptmp = NULL
  rm(kmdattmp,grp)  
  for (grp in unique(kmdat$group)) {
    kmdat[kmdat$group==grp,grp] = kmdat[kmdat$group==grp,"surv"]
  }
  kmdat$days = kmdat$time
  kmdat$Days = kmdat$days
  kmdat$Censored = kmdat$cens
  kmdat = kmdat[,c("days",unique(kmdat$group),"cens")]
  write.csv(kmdat,file=paste0("../CSV/",filename,".KM_forExcel.csv"),row.names=FALSE,na="")
  
  # # csv output for KM - GRAPHPAD FORMAT
  t2e = data.frame(Days = dat$formattedData$survival.time,
                   Event = dat$formattedData$survival.event,
                   Group = paste0("",dat$formattedData$group))
  groups = names(table(t2e$Group))
  for (nm in groups) {
    t2e[,nm] = NA
    t2e[t2e$Group==nm,nm] = t2e$Event[t2e$Group==nm]
  }
  t2e$Event = NULL
  t2e$Group = NULL
  write.csv(t2e,file=paste0("../CSV/",filename,".KM_forGraphPad.csv"),na="",row.names=FALSE)
  
  write.table(dat$table2, file="summary2.txt",row.names=F,col.names=T,quote=F,sep="\t")
  
  
}
