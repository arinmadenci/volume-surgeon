
### ------------------------
# whole bunch of utility functions for the trials
##### ----------------------
### TRIAL 1
# Non-parametric for Trial 1: *any* eligibility
np_trial_1_any_eligibility_function <- function(dat){
  tt_1.np1_any <- dat %>% select(op_npi, surgeon_period, volume, death.percent_lead1)
  tt_1.np2_any <- tt_1.np1_any %>% 
    mutate(volume_cat=case_when(volume>=20 & volume<25~"20-24",
                                volume>=25 & volume<30~"25-29",
                                volume>=30~"$\\geq$30",
                                TRUE~as.character(volume)))
  np.trial1_any <- tt_1.np2_any %>%
    group_by(volume_cat) %>% summarise(n=n(), 
                                       mean=mean(death.percent_lead1, na.rm=TRUE)) %>% 
    ungroup()
  np.trial1_any$volume_cat <- fct_relevel(np.trial1_any$volume_cat, 
                                          c(as.character(0:19),"20-24", "25-29", "$\\geq$30"))
  np.trial1_any_sorted <- np.trial1_any %>% arrange(match(volume_cat, 
                                                          levels(fct_relevel(np.trial1_any$volume_cat, 
                                                                             c(as.character(0:19),"20-24", "25-29", "$\\geq$30")))))
  table <- merge(np.trial1_any_sorted, data.frame(volume_cat=as.factor(c(as.character(0:19),"20-24", "25-29", "$\\geq$30"))), all.y=TRUE)
  table$n <- ifelse(is.na(table$n),0,table$n)
  table$volume_cat <- fct_relevel(table$volume_cat, 
                                                       c(as.character(0:19),"20-24", "25-29", "$\\geq$30"))
  table$mean <- sprintf("%.3f", table$mean)
  table <- table %>% arrange(volume_cat)
  return(table)
}



### TRIAL 2
# non-parametric for Trial 2: *any* eligibility
np_trial_2_any_eligibility_function <- function(dat){
  long <- dat %>% select(op_npi.new, surgeon_period, volume, death.percent_lead1)
  long2 <- long %>% mutate(volume_cat=case_when(volume<5~volume, # <5
                                                volume>=5 & volume <7~5, # 5-6
                                                volume>=7 & volume <9~6, # 7-8
                                                volume>=9 & volume <11~7, # 9-10
                                                volume>=11 & volume <15~8, # 11-14
                                                volume>=15 & volume <20~9, # 15-19
                                                volume>=20 & volume<25~10, # 20-24
                                                volume>=25~11 # ge25
                                                ))
  list <- pblapply(unique(long2$op_npi.new), function(x) {
    long2$volume_cat[long2$op_npi.new==x] 
  })
  threepeat <- pblapply(1:length(list), 
                        function(x){
                          temp <- table(list[x]) %>% {.[.>=3]} %>% names(.)
                          temp2 <- ifelse(length(temp)==0,NA,temp)
                          return(temp2)
                        }
  ) %>% unlist %>% as.numeric
  temp <- data.frame(op_npi.new=unique(long2$op_npi.new),
                     assignment=threepeat)
  temp2 <- merge(temp, long2, by="op_npi.new") %>% select(-c(volume, volume_cat))
  temp3 <- temp2 %>% group_by(op_npi.new) %>% 
    filter(surgeon_period==max(surgeon_period)) %>% 
    ungroup() %>% 
    filter(!is.na(assignment)) %>% 
    group_by(assignment) %>% summarise(n=n(), 
                                       mean=mean(death.percent_lead1, na.rm=TRUE))
  temp4 <- merge(data.frame(assign=c(0:4, "5-6", "7-8", "9-10",
                                     "11-14", "15-19", "20-24", "$\\geq$25"),
                            assignment=0:11),
                 temp3,
                 all.x=TRUE) %>% 
    mutate(n=ifelse(is.na(n),0,n)) %>% 
    select(-assignment)
  temp4$assign <- forcats::fct_relevel(as.character(c(0:4, "5-6", "7-8", "9-10",
                                                      "11-14", "15-19", "20-24", "$\\geq$25")))
  temp4$mean <- sprintf("%.3f", temp4$mean)
  return(temp4)
}



# TRIAL 3: *any* eligibility
np_trial_3_any_eligibility_function <- function(dat){
  tt_1.np1_any <- dat %>% select(op_npi, surgeon_period, volume, volume_lag1, death.percent_lead1, change)
  np.trial_any <- tt_1.np1_any %>%
    group_by(change) %>% summarise(n=n(), 
                                   mean=mean(death.percent_lead1, na.rm=TRUE)) %>% 
    ungroup()
  table <- merge(data.frame(change=-30:30,
                            change.assignment=-30:30),
                 np.trial_any,
                 all.x=TRUE) %>% 
    mutate(n=ifelse(is.na(n),0,n)) %>% 
    select(-change) %>% rename(change=change.assignment)
  table$mean <- sprintf("%.3f", table$mean)
  return(table)
}



### TRIAL 4
# non-parametric for Trial 4: *any* eligibility
np_trial_4_any_eligibility_function <- function(dat){
  long <- dat %>% select(op_npi.new, surgeon_period, change, death.percent_lead1)
  list <- pblapply(unique(long$op_npi.new), function(x) {
    long$change[long$op_npi.new==x] 
  })
  threepeat <- pblapply(1:length(list), 
                        function(x){
                          temp <- table(list[x]) %>% {.[.>=3]} %>% names(.)
                          temp2 <- ifelse(length(temp)==0,NA,temp)
                          return(temp2)
                        }
  ) %>% unlist %>% as.numeric
  temp <- data.frame(op_npi.new=unique(long$op_npi.new),
                     assignment=threepeat)
  temp2 <- merge(temp, long, by="op_npi.new") %>% select(-c(change))
  temp3 <- temp2 %>% group_by(op_npi.new) %>% 
    filter(surgeon_period==max(surgeon_period)) %>% 
    ungroup() %>% 
    filter(!is.na(assignment)) %>% 
    group_by(assignment) %>% summarise(n=n(),
                                       mean=mean(death.percent_lead1, na.rm=TRUE) # removing censored surgeons lost to follow-up
                                       )
  table <- merge(data.frame(change=-30:30,
                            assignment=-30:30),
                 temp3,
                 by="assignment",
                 all.x=TRUE) %>% 
    mutate(n=ifelse(is.na(n),0,n)) %>% 
    select(-assignment)
  table$mean <- sprintf("%.3f", table$mean)
  return(table)
}


# non-parametric for Trial 4: *any* eligibility (POSITIVITY)
np.trial_4_any_eligibility.positivity_funtion.pre <- function(dat){
  long <- dat %>% select(op_npi.new, surgeon_period, baseline_volume, change, death.percent_lead1)
  list <- pblapply(unique(long$op_npi.new), function(x) {
    long$change[long$op_npi.new==x] 
  })
  threepeat <- pblapply(1:length(list), 
                        function(x){
                          temp <- table(list[x]) %>% {.[.>=3]} %>% names(.)
                          temp2 <- ifelse(length(temp)==0,NA,temp)
                          return(temp2)
                        }
  ) %>% unlist %>% as.numeric
  temp <- data.frame(op_npi.new=unique(long$op_npi.new),
                     assignment=threepeat)
  temp2 <- merge(temp, long, by="op_npi.new") %>% select(-c(change))
  temp3 <- temp2 %>% group_by(op_npi.new) %>% 
    filter(surgeon_period==max(surgeon_period)) %>% 
    ungroup() %>% 
    filter(!is.na(assignment))
  return(temp3)
}
np.trial_4_any_eligibility.positivity_funtion <- function(input_4, baseline_vol){
  table <- input_4
  table2 <- table %>% filter(baseline_volume %in% 0:4) %>% select(-surgeon_period)
  table2 %>% group_by(assignment) %>% summarise(n=n(),
                                                mean=mean(death.percent_lead1, na.rm=TRUE))
}



####### ---------------
# combined functions
####### ---------------
combined_non_parametric_function <- function(dat_1, dat_4){
  np.tables <- list()
  # non-parametric for Trial 1 (any eligibility)
  np.tables$np.trial1_any <- np_trial_1_any_eligibility_function(dat = dat_1)
  print(xtable(np.tables$np.trial1_any %>% 
                 rename("Operations per interval"=volume_cat, "Number of surgeons adhering"=n, "Unadjusted 90-day mortality (next interval)"=mean), 
               align=c("llrr"),digits=c(0,0,0,3), 
               caption="Number of surgeons fulfilling assignment criteria for Target Trial \\#1 (after any eligibility)",
               label="table:np-trial-1-any"),
        include.rownames=FALSE,
        caption.placement="top",
        type="latex", sanitize.text.function = function(x){x}, 
        tabular.environment="longtable", 
        file=here::here("tables","non-parametric","surgeon-np-trial-1.tex"), floating=FALSE
  )
  
  # non-parametric for Trial 2 (any eligibility)
  np.tables$np.trial2_any <- np_trial_2_any_eligibility_function(dat = dat_4)
  print(xtable(np.tables$np.trial2_any %>% 
                 rename("Number of surgeons adhering"=n,
                        "Unadjusted 90-day mortality (next interval)"=mean,
                        "Operations per interval"=assign), 
               align=c("llrr"), digits=c(0,0,0,3),
               caption="Number of surgeons fulfilling assignment criteria for Target Trial \\#2",
               label="table:np-trial-2"),
        include.rownames=TRUE,
        caption.placement="top",
        type="latex", sanitize.text.function = function(x){x}, 
        tabular.environment="longtable", 
        file=here::here("tables","non-parametric","surgeon-np-trial-2.tex"), floating=FALSE
  )
  
  # non-parametric for Trial 3 (any eligibility)
  np.tables$np.trial3_any <- np_trial_3_any_eligibility_function(dat = dat_1) %>% filter(change %in% c(-5:5))
  print(xtable(np.tables$np.trial3_any %>% 
                 rename("Change in volume"=change, 
                        "Number of surgeons adhering"=n, 
                        "Unadjusted 90-day mortality (next interval)"=mean), align=c("llrr"), digits=c(0,0,0,3),
               caption="Number of surgeons fulfilling assignment criteria for Target Trial \\#3",
               label="table:np-trial-3"),
        include.rownames=FALSE,
        caption.placement="top",
        type="latex", sanitize.text.function = function(x){x}, 
        tabular.environment="longtable", 
        file=here::here("tables","non-parametric","surgeon-np-trial-3.tex"), floating=FALSE
  )
  
  # non-parametric for Trial 4 (any eligibility)
  np.tables$np.trial4_any <- np_trial_4_any_eligibility_function(dat = dat_4) %>% as.data.frame %>% filter(change %in% c(-5:5))
  print(xtable(np.tables$np.trial4_any %>% 
                 rename("Change in volume"=change,
                        "Number of surgeons adhering"=n,
                        "Unadjusted 90-day mortality (next interval)"=mean), 
               align=c("llrr"), digits=c(0,0,0,3),
               caption="Number of surgeons fulfilling assignment criteria for Target Trial \\#4",
               label="table:np-trial-4"), 
        include.rownames=FALSE,
        caption.placement="top",
        type="latex", sanitize.text.function = function(x){x}, 
        tabular.environment="longtable", 
        file=here::here("tables","non-parametric","surgeon-np-trial-4.tex"), floating=FALSE
  )
  return(np.tables)
}

combined_positivity_function <- function(dat_1, dat_4){
  source(here::here("descriptive-summaries_scripts","non-parametric_functions.R"))
  # Positivity checks
  positivity.tables <- list()
  # trial 1: positivity
  positivity.tables$trial_1_baseline_vol$"0-4" <- np_trial_1_any_eligibility_function(dat=dat_1 %>% filter(baseline_volume %in% 0:4))
  positivity.tables$trial_1_baseline_vol$"5-9" <- np_trial_1_any_eligibility_function(dat=dat_1 %>% filter(baseline_volume %in% 5:9))
  positivity.tables$trial_1_baseline_vol$"10-14" <- np_trial_1_any_eligibility_function(dat=dat_1 %>% filter(baseline_volume %in% 10:4))
  positivity.tables$trial_1_baseline_vol$"ge15" <- np_trial_1_any_eligibility_function(dat=dat_1 %>% filter(baseline_volume %in% 15:max(dat_1$baseline_volume)))
  
  # trial 2: positivity
  positivity.tables$trial_2_baseline_vol$"0-4" <- np_trial_2_any_eligibility_function(dat=dat_4 %>% filter(baseline_volume %in% 0:4))
  positivity.tables$trial_2_baseline_vol$"5-9" <- np_trial_2_any_eligibility_function(dat=dat_4 %>% filter(baseline_volume %in% 5:9))
  positivity.tables$trial_2_baseline_vol$"10-14" <- np_trial_2_any_eligibility_function(dat=dat_4 %>% filter(baseline_volume %in% 10:14))
  positivity.tables$trial_2_baseline_vol$"ge15" <- np_trial_2_any_eligibility_function(dat=dat_4 %>% filter(baseline_volume %in% 15:max(dat_4$baseline_volume)))
  
  # trial 3: positivity
  positivity.tables$trial_3_baseline_vol$"0-4" <- np_trial_3_any_eligibility_function(dat=dat_1 %>% filter(baseline_volume %in% 0:4)) %>% 
    filter(change %in% -5:5)
  positivity.tables$trial_3_baseline_vol$"5-9" <- np_trial_3_any_eligibility_function(dat=dat_1 %>% filter(baseline_volume %in% 5:9)) %>% 
    filter(change %in% -5:5)
  positivity.tables$trial_3_baseline_vol$"10-14" <- np_trial_3_any_eligibility_function(dat=dat_1 %>% filter(baseline_volume %in% 10:14)) %>% 
    filter(change %in% -5:5)
  positivity.tables$trial_3_baseline_vol$"ge15" <- np_trial_3_any_eligibility_function(dat=dat_1 %>% filter(baseline_volume %in% 15:max(dat_1$baseline_volume))) %>% 
    filter(change %in% -5:5)
  
  # trial 4: positivity
  positivity.tables$trial_4_baseline_vol$"0-4" <- np_trial_4_any_eligibility_function(dat=dat_4 %>% filter(baseline_volume %in% 0:4)) %>% 
    filter(change %in% -5:5)
  positivity.tables$trial_4_baseline_vol$"5-9" <- np_trial_4_any_eligibility_function(dat=dat_4 %>% filter(baseline_volume %in% 5:9)) %>% 
    filter(change %in% -5:5)
  positivity.tables$trial_4_baseline_vol$"10-14" <- np_trial_4_any_eligibility_function(dat=dat_4 %>% filter(baseline_volume %in% 10:14)) %>% 
    filter(change %in% -5:5)
  positivity.tables$trial_4_baseline_vol$"ge15" <- np_trial_4_any_eligibility_function(dat=dat_4 %>% filter(baseline_volume %in% 15:max(dat_4$baseline_volume))) %>% 
    filter(change %in% -5:5)
  
  # output
  positivity.tables$output_1 <- positivity.tables$trial_1_baseline_vol %>% bind_cols(.) %>% select(`Volume category`=volume_cat, `History: 0 to 4`=n, `History: 5 to 9`=n1, `History: 10 to 14`=n2,
                                                                                                   `History: 15+`=n3)
  positivity.tables$output_2 <- positivity.tables$trial_2_baseline_vol %>% bind_cols(.) %>% select(`Volume category`=assign, `History: 0 to 4`=n, `History: 5 to 9`=n1, `History: 10 to 14`=n2,
                                                                                                   `History: 15+`=n3)
  positivity.tables$output_3 <- positivity.tables$trial_3_baseline_vol %>% bind_cols(.) %>% select(`Volume category`=change, `History: 0 to 4`=n, `History: 5 to 9`=n1, `History: 10 to 14`=n2,
                                                                                                   `History: 15+`=n3)
  positivity.tables$output_4 <- positivity.tables$trial_4_baseline_vol %>% bind_cols(.) %>% select(`Volume category`=change, `History: 0 to 4`=n, `History: 5 to 9`=n1, `History: 10 to 14`=n2,
                                                                                                   `History: 15+`=n3)
  
  print(xtable(positivity.tables$output_1, 
               align=c("llrrrr"), digits=rep(0,6),
               caption="Positivity check for Target Trial \\#1",
               label="table:pos-trial-1"), 
        include.rownames=FALSE,
        caption.placement="top",
        type="latex", sanitize.text.function = function(x){x}, 
        tabular.environment="longtable", 
        file=here::here("tables","positivity-check","pos-trial-1.tex"), floating=FALSE
  )
  print(xtable(positivity.tables$output_2, 
               align=c("llrrrr"), digits=rep(0,6),
               caption="Positivity check for Target Trial \\#2",
               label="table:pos-trial-2"), 
        include.rownames=FALSE,
        caption.placement="top",
        type="latex", sanitize.text.function = function(x){x}, 
        tabular.environment="longtable", 
        file=here::here("tables","positivity-check","pos-trial-2.tex"), floating=FALSE
  )
  print(xtable(positivity.tables$output_3, 
               align=c("llrrrr"), digits=rep(0,6),
               caption="Positivity check for Target Trial \\#3",
               label="table:pos-trial-3"), 
        include.rownames=FALSE,
        caption.placement="top",
        type="latex", sanitize.text.function = function(x){x}, 
        tabular.environment="longtable", 
        file=here::here("tables","positivity-check","pos-trial-3.tex"), floating=FALSE
  )
  print(xtable(positivity.tables$output_4, 
               align=c("llrrrr"), digits=rep(0,6),
               caption="Positivity check for Target Trial \\#4",
               label="table:pos-trial-4"), 
        include.rownames=FALSE,
        caption.placement="top",
        type="latex", sanitize.text.function = function(x){x}, 
        tabular.environment="longtable", 
        file=here::here("tables","positivity-check","pos-trial-4.tex"), floating=FALSE
  )
  return(positivity.tables)
}
