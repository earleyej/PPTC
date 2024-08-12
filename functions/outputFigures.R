require('gridExtra', quietly = TRUE, warn.conflicts = FALSE)

output.solid.figures.portrait.10groups.1figperpage <- function(dat, agent_code, tumor_model, wd){
  filename<-sub("-","_",dat$project$panel_code)
  pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
  #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height=9.5, width=7, paper="US")
  
  # weight plot 
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$weight.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # diameter plots
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$diameter.plots[['A']],
               dat$diameter.plots[['B']],
               dat$diameter.plots[['C']],
               dat$diameter.plots[['D']],
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,2,3,4,5,8,8,9,9), ncol=2, byrow=TRUE),
               heights = c(1.5,3,3,1.5,0.5))
  
  # diameter plots
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$diameter.plots[['E']],
               dat$diameter.plots[['F']],
               dat$diameter.plots[['G']],
               dat$diameter.plots[['H']],
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,2,3,4,5,8,8,9,9), ncol=2, byrow=TRUE),
               heights = c(1.5,3,3,1.5,0.5))
  
  # diameter plots
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$diameter.plots[['I']],
               dat$diameter.plots[['J']],
               textGrob(""),
               textGrob(""),
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,2,3,4,5,8,8,9,9), ncol=2, byrow=TRUE),
               heights = c(1.5,3,3,1.5,0.5))
  
  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # volume plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$volume.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$rtv.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
}

output.solid.figures.portrait.8groups.1figperpage <- function(dat, agent_code, tumor_model, wd){
  filename<-sub("-","_",dat$project$panel_code)
  pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
  #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height=9.5, width=7, paper="US")
  
  # weight plot 
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$weight.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # diameter plots
  
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               # dat$diameter.plots[['A']],
               # dat$diameter.plots[['B']],
               # dat$diameter.plots[['C']],
               # dat$diameter.plots[['D']],
               dat$diameter.plots[[1]],
               dat$diameter.plots[[2]],
               dat$diameter.plots[[3]],
               dat$diameter.plots[[4]],
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,2,3,4,5,8,8,9,9), ncol=2, byrow=TRUE),
               heights = c(1.5,3,3,1.5,0.5))
  
  # diameter plots
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               # dat$diameter.plots[['E']],
               # dat$diameter.plots[['F']],
               # dat$diameter.plots[['G']],
               # dat$diameter.plots[['H']],
               dat$diameter.plots[[5]],
               dat$diameter.plots[[6]],
               dat$diameter.plots[[7]],
               dat$diameter.plots[[8]],
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,2,3,4,5,8,8,9,9), ncol=2, byrow=TRUE),
               heights = c(1.5,3,3,1.5,0.5))
  
  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # volume plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$volume.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$rtv.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
}



output.solid.figures.portrait.7groups.1figperpage <- function(dat, agent_code, tumor_model, wd){
  filename<-sub("-","_",dat$project$panel_code)
  pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
  #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height=9.5, width=7, paper="US")
  
  # weight plot 
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$weight.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # diameter plots
  
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               # dat$diameter.plots[['A']],
               # dat$diameter.plots[['B']],
               # dat$diameter.plots[['C']],
               # dat$diameter.plots[['D']],
               dat$diameter.plots[[1]],
               dat$diameter.plots[[2]],
               dat$diameter.plots[[3]],
               dat$diameter.plots[[4]],
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,2,3,4,5,8,8,9,9), ncol=2, byrow=TRUE),
               heights = c(1.5,3,3,1.5,0.5))
  
  # diameter plots
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               # dat$diameter.plots[['E']],
               # dat$diameter.plots[['F']],
               # dat$diameter.plots[['G']],
               # dat$diameter.plots[['H']],
               dat$diameter.plots[[5]],
               dat$diameter.plots[[6]],
               dat$diameter.plots[[7]],
               textGrob(""),
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,2,3,4,5,8,8,9,9), ncol=2, byrow=TRUE),
               heights = c(1.5,3,3,1.5,0.5))
  
  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # volume plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$volume.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$rtv.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
}









output.solid.figures.portrait.6groups.1figperpage <- function(dat, agent_code, tumor_model, wd){
  filename<-sub("-","_",dat$project$panel_code)
  pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
  #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height=9.5, width=7, paper="US")
  
  # weight plot 
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$weight.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # diameter plots
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$diameter.plots[['A']],
               dat$diameter.plots[['B']],
               dat$diameter.plots[['C']],
               dat$diameter.plots[['D']],
               dat$diameter.plots[['E']],
               dat$diameter.plots[['F']],
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,1,2,3,4,5,6,7,8,8,8,9,9,9), ncol=3, byrow=TRUE),
               heights = c(1.5,3,3,1.5,0.5))
  
  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # volume plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$volume.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$rtv.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
}

output.solid.figures.portrait.5groups.1figperpage <- function(dat, agent_code, tumor_model, wd){
  filename<-sub("-","_",dat$project$panel_code)
  pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
  #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height=9.5, width=7, paper="US")
  # weight plot 
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$weight.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # diameter plots
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$diameter.plots[['A']],
               dat$diameter.plots[['B']],
               dat$diameter.plots[['C']],
               dat$diameter.plots[['D']],
               dat$diameter.plots[['E']],
               textGrob(""),
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,1,2,3,4,5,6,7,8,8,8,9,9,9), ncol=3, byrow=TRUE),
               heights = c(1.5,3,3,1.5,0.5))
  
  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # volume plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$volume.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$rtv.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename<-sub("-","_",dat$project$panel_code),'.pdf'),overwrite=TRUE)
}

output.solid.figures.portrait.4groups.1figperpage <- function(dat, agent_code, tumor_model, wd){
  filename<-sub("-","_",dat$project$panel_code)
  pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
  #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height=9.5, width=7, paper="US")
  
  # weight plot 
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$weight.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # diameter plots
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$diameter.plots[[dat$groupInfo$group[1]]],
               dat$diameter.plots[[dat$groupInfo$group[2]]],
               dat$diameter.plots[[dat$groupInfo$group[3]]],
               dat$diameter.plots[[dat$groupInfo$group[4]]],
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,2,3,4,5,6,6,7,7), ncol=2, byrow=TRUE),
               heights = c(1.5,3,3,1.5,0.5))
  
  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # volume plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$volume.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$rtv.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
}

output.solid.figures.portrait.3groups.1figperpage <- function(dat, agent_code, tumor_model, wd){
  filename<-sub("-","_",dat$project$panel_code)
  pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
  #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height=9.5, width=7, paper="US")
  
  # weight plot 
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$weight.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # diameter plots
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$diameter.plots[[dat$groupInfo$group[1]]],
               dat$diameter.plots[[dat$groupInfo$group[2]]],
               dat$diameter.plots[[dat$groupInfo$group[3]]],
               textGrob(""),
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,2,3,4,5,6,6,7,7), ncol=2, byrow=TRUE),
               heights = c(1.5,3,3,1.5,0.5))
  
  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # volume plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$volume.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$rtv.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
}

output.solid.figures.portrait.2groups.1figperpage <- function(dat, agent_code, tumor_model, wd){
  filename<-sub("-","_",dat$project$panel_code)
  pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
  #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height = 9.5, width = 7, paper = "US") #updated 6/27 EJE. Steve requested I make the pdf in the parent dir and rename it
  
  
  # weight plot 
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$weight.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # diameter plots
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$diameter.plots[['A']],
               dat$diameter.plots[['B']],
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               layout_matrix = matrix(c(1,1,2,3,4,4,5,5), ncol=2, byrow=TRUE),
               heights = c(1.5,4,1.5,1.5))
  
  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # volume plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$volume.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$rtv.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
  
  
}

output.all.figures.portrait.1figperpage <- function(dat = dat, agent_code, tumor_model, wd){
  filename<-sub("-","_",dat$project$panel_code)
  pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
  #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height = 9.5, width = 7, paper = "US") #updated 6/27 EJE. Steve requested I make the pdf in the parent dir and rename it
  # weight plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$weight.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))

  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # cd45 plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$cd45.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
}

output.all.figures.noweight.portrait.1figperpage <- function(dat = dat, agent_code, tumor_model, wd){
  filename<-sub("-","_",dat$project$panel_code)
  pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
  #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height=9.5, width=7, paper="US")
  
  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # cd45 plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$cd45.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
}

output.all.figures.portrait.1figperpage <- function(dat = dat, agent_code, tumor_model, wd){
  filename<-sub("-","_",dat$project$panel_code)
  pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
  #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height=9.5, width=7, paper="US")
  
  # weight plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$weight.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # cd45 plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$cd45.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
}


output.all.figures.portrait.single.mouse.1figperpage <- function(dat = dat, agent_code, tumor_model, wd){
   filename<-sub("-","_",dat$project$panel_code)
   pdf(paste0(wd, 'plots.pdf'), height = 9.5, width = 7, paper = "US")
   #pdf(paste0("../",dat$project$agent_code,"_",dat$project$tumor_model,".pdf"), height=9.5, width=7, paper="US")
  # 
  # # weight plot
  # grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
  #              dat$weight.plot,
  #              textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
  #                       x=0, just = "left"),
  #              textGrob(paste0("Figures generated at: ", Sys.time()),
  #                       gp = gpar(fontsize = 12),
  #                       x = 1, just="right"),
  #              nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  # 
  # # survival plot
  # grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
  #              dat$survival.plot,
  #              textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
  #                       x=0, just = "left"),
  #              textGrob(paste0("Figures generated at: ", Sys.time()),
  #                       gp = gpar(fontsize = 12),
  #                       x = 1, just="right"),
  #              nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  # 
  # # cd45 plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$cd45.plot,
               # textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
               #          x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))

  #dev.off()
  # swimmer plot
  grid.arrange(textGrob(paste(agent_code), gp = gpar(fontsize = 19)),
               dat$swimmer.plot,
               # textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
               #          x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  # waterfall plot
  grid.arrange(textGrob(paste(agent_code), gp = gpar(fontsize = 19)),
               dat$waterfall.plot,
               # textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$drug_dose, sep = ": "), collapse="\n"),
               #          x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  
  
  
  dev.off()
  
  
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
}


output.cns.figures.portrait.2groups.1figperpage <- function(dat, agent_code, tumor_model, wd, weight = TRUE){
  filename<-sub("-","_",dat$project$panel_code)
  #pdf(paste0("../", dat$project$agent_code,"_",dat$project$tumor_model,'.pdf'), height = 9.5, width = 7, paper = "US")
  pdf(paste0(wd,'plots.pdf'), height = 9.5, width = 7, paper = "US")
  if(weight){
    # weight plot 
    grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
                 dat$weight.plot,
                 textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose_schedule, sep = ": "), collapse="\n"),
                          x=0, just = "left"),
                 textGrob(paste0("Figures generated at: ", Sys.time()),
                          gp = gpar(fontsize = 12),
                          x = 1, just="right"),
                 nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  } else {
    
  }

  # survival plot
  grid.arrange(textGrob(paste(agent_code, tumor_model), gp = gpar(fontsize = 19)),
               dat$survival.plot,
               textGrob(paste(paste(dat$groupInfo$group, dat$groupInfo$dose_schedule, sep = ": "), collapse="\n"),
                        x=0, just = "left"),
               textGrob(paste0("Figures generated at: ", Sys.time()),
                        gp = gpar(fontsize = 12),
                        x = 1, just="right"),
               nrow = 4, ncol = 1, heights = c(1,6,2,0.5))
  
  dev.off()
  #copy the plots.pdf to the parent dir
  file.copy("plots.pdf",paste0("../", filename,'.pdf'),overwrite=TRUE)
}
