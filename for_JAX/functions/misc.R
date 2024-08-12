
project_ID_to_name <- function(id){
  metaInfo <- read.csv(paste0(resources.dir,"data_file.csv"), stringsAsFactors = FALSE)
  return(list(agentCode = metaInfo$project_number[metaInfo$record_id_1 == id],
         agentName = metaInfo$project_title[metaInfo$record_id_1 == id]))
}

project_code_to_name <- function(code){
  metaInfo <- read.csv(paste0(resources.dir,"data_file.csv"), stringsAsFactors = FALSE)
  return(metaInfo$project_title[metaInfo$project_number %in% c(code, paste0(code, "P"))])
}

panel_code_to_name <- function(panel_code){
  tmp <- strsplit(panel_code, "-")[[1]]
  return(list(agentCode = tmp[1],
              agentName = project_code_to_name(tmp[1]),
              tumorModel = paste(tmp[2:(length(tmp)-1)], sep = "-")))
}