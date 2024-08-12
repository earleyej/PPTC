require(survival, quietly = TRUE, warn.conflicts = FALSE)
require(exactRankTests, quietly = TRUE, warn.conflicts = FALSE)

# Kaplan-Meier #####
km_fit <- function(dat){
  # dat is a formatted data frame with at least 
  # survival.time
  # survival.event
  # group
  
  if(! is.null(dat$exclude)){
    dat <- dat[ !dat$exclude, ]
  }

  if(is.null(dat$survival.time)){
    stop("Error in Kaplan-Meier fit function: No column with name 'Days'")
  }
  if(is.null(dat$survival.event)){
    stop("Error in Kaplan-Meier fit function: No column with name 'Event'")
  }
  if(is.null(dat$group)){
    stop("Error in Kaplan-Meier fit function: No column with name 'Group'")
  }
  
  survdat <- Surv(dat$survival.time,event=dat$survival.event)
  kmFit <- survfit(survdat ~ factor(dat$group))
  
  return(kmFit)
}

# exact rank test
exact_rank_test <- function(dat, numMCrep = 1000000, groupconf = NULL){
  
  if(! is.null(dat$exclude)){
    dat <- dat[ !dat$exclude, ]
  }
  
  if( is.null(groupconf)){
    stop("Error in exact rank test: groupconf can not be empty.")
  } else if(length(groupconf) != 3){
    stop("Error in exact rank test: incorrect groupconf length.")
  }
  
  tmp <- dat[dat$group %in% groupconf[1:2], ]
  
  lsc <- cscores(Surv(time=tmp$survival.time,event=tmp$survival.event), int=FALSE)
  
  ctrl <- tmp$group == groupconf[1]
  trt <- tmp$group == groupconf[2]
  sumRank_ctrl <- sum(lsc[ctrl])
  exactrank_p = pperm(sumRank_ctrl, lsc[ctrl|trt], sum(ctrl), alternative="two.sided", simulate=TRUE, B = numMCrep)
  return(exactrank_p)
}

# Gehan-Wilcoxon test #####

wilcoxon_test <- function(dat, groupconf = NULL){

  if(! is.null(dat$exclude)){
    dat <- dat[ !dat$exclude, ]
  }
  
  if( is.null(groupconf)){
    stop("Error in exact rank test: groupconf can not be empty.")
  } else if(length(groupconf) != 3){
    stop("Error in exact rank test: incorrect groupconf length.")
  }
  
  tmp <- dat[dat$group %in% groupconf[1:2], ]
  
  sdf <- survdiff(Surv(survival.time, survival.event) ~ group, data=tmp, rho=1)
  wilcoxon_p <- 1 - pchisq(sdf$chisq, length(sdf$n) - 1)
  
  return(wilcoxon_p)
}