trial_1_stats_function <- function(dat){
  c(min=min(dat$volume),
  max=max(dat$volume)
  )
}

trial_2_stats_function <- function(dat){
  long <- dat %>% select(op_npi.new, surgeon_period, volume, death.percent_lead1)
  list <- pblapply(unique(long$op_npi.new), function(x) {
    long$volume[long$op_npi.new==x] 
  }) %>% unique() # unique entries *only* for this function
  
  threepeat <- pblapply(1:length(list), 
                        function(x){
                          temp <- table(list[x]) %>% {.[.>=3]} %>% names(.)
                          temp2 <- ifelse(length(temp)==0,NA,temp)
                          return(temp2)
                        }
  )
  threepeat[!is.na(threepeat)] %>% unlist %>% as.numeric %>% {c("min"=min(.),
                                                                "max"=max(.)
  )}
}

trial_3_stats_function <- function(dat){
  c(min=min(dat$change),
    max=max(dat$change)
  )
}

trial_4_stats_function <- function(dat){
  long <- dat %>% select(op_npi.new, surgeon_period, death.percent_lead1, change)
  list <- pblapply(unique(long$op_npi.new), function(x) {
    long$change[long$op_npi.new==x] 
  }) %>% unique() # unique entries *only* for this function
  
  threepeat <- pblapply(1:length(list), 
                        function(x){
                          temp <- table(list[x]) %>% {.[.>=3]} %>% names(.)
                          temp2 <- ifelse(length(temp)==0,NA,temp)
                          return(temp2)
                        }
  )
  threepeat[!is.na(threepeat)] %>% unlist %>% as.numeric %>% {c("min"=min(.),
                                                                "max"=max(.)
  )}
}



