# restriction-and-formatting-part-3_functions for discrete time structure

# -------------
# utility functions
# -------------
# dummy columns
create.dummies_function <- function(dataset){
  dataset2 <- dummy_cols(dataset, select_columns=c("hosp.teaching", "hosp.controltype"), remove_first_dummy=TRUE, ignore_na=TRUE)
  dataset3 <-dummy_cols(dataset2, select_columns=c("admission.type"), remove_first_dummy=FALSE, ignore_na=TRUE)
  return(dataset3)
}

add.period.volumes_function <- function(dataset){
  surgeon.period.vols <- dataset %>% 
    group_by(op_npi, period) %>% 
    summarise(volume=n(),
              death.count=sum(death90)) %>% 
    mutate(death.count_prev=lag(death.count,1),
           death.percent=ifelse(volume!=0, death.count/volume, 0),
           death.percent_prev=lag(death.percent,1),
           death.percent_prev2=lag(death.percent,2)
           ) %>% 
    ungroup()
  dataset2 <- merge(dataset, surgeon.period.vols, by=c("op_npi", "period"))
  return(dataset2)
}

add.covariate.means_function <- function(dataset){
  covariate.means <- dataset %>% group_by(op_npi, period) %>% 
    summarise_at(.vars=vars(c("age", "comorb_ami", "comorb_dementia","comorb_afib","comorb_ckd","comorb_copd",      
                              "comorb_chf","comorb_diabetes","comorb_cad","comorb_stroketia",
                              "inpatient", "hospital.volume", "hosp.totalbeds", "hosp.p_medicare",
                              "hosp.cicu", "hosp.teaching_minor", "hosp.teaching_nonteaching", 
                              "hosp.controltype_notforprofit", "hosp.controltype_gov.nonfed", "hosp.controltype_gov.fed",
                              "admission.type_Elective"
    )),
    .funs=list(mean=~ mean(., na.rm=TRUE))) %>% ungroup()
  dataset2 <- merge(dataset %>% select(op_npi, orgnpinm, period, volume, death.count, death.count_prev,
                                    death.percent, death.percent_prev, death.percent_prev2) %>% 
                    group_by(op_npi, period) %>% filter(row_number()==n()) %>% ungroup(),
                  covariate.means,
                  by=c("op_npi", "period")) %>% 
    mutate_at(vars(contains("_mean")),
              list("prev"=lag))
  return(dataset2)
}

create.and.merge.grid_function <- function(dataset){
  dataset2 <- dataset %>%
    group_by(op_npi) %>% mutate(surgeon_period = as.numeric(period)-min(as.numeric(period))+1) %>% ungroup() %>% 
    mutate_at(vars(contains("_prev")),
              funs(ifelse(is.na(.) & surgeon_period==1,0,.))) %>%
    group_by(op_npi) %>% mutate(first_period=ifelse(surgeon_period==1,1,NA)) %>% 
    fill(first_period, .direction="down") %>% ungroup() %>% 
    filter(!is.na(first_period))
  
  grid <- expand.grid(op_npi=unique(dataset2$op_npi),
                          surgeon_period=1:max(dataset2$surgeon_period)) %>% arrange(op_npi)
  
  dataset3 <- merge(dataset2 %>% mutate(obs_surgeon_period=surgeon_period), grid, all.y=TRUE)
  return(dataset3)
}

fill.and.format_function <- function(dataset){
  dataset2 <- dataset %>% 
    mutate(no_cases=ifelse(is.na(volume),1,0),
           volume=ifelse(is.na(volume),0,volume)) %>% 
    group_by(op_npi) %>%
    mutate(volume_prev = ifelse(surgeon_period!=1, lag(volume),0),
           volume_prev2 = ifelse(surgeon_period>2, lag(volume,2),0),
           cumave.volume = cumsum(volume)/surgeon_period,
           cumave.volume_prev = case_when(surgeon_period>1 ~ cumsum(volume_prev)/(surgeon_period-1),
                                          surgeon_period==1 ~ 0)) %>% 
    ungroup()
  dataset3 <- dataset2 %>% group_by(op_npi) %>% 
    fill(c("orgnpinm",
           "age_mean", "comorb_ami_mean", 
           "comorb_dementia_mean", "comorb_afib_mean", "comorb_ckd_mean", 
           "comorb_copd_mean", "comorb_chf_mean", "comorb_diabetes_mean", 
           "comorb_cad_mean", "comorb_stroketia_mean",
           "inpatient_mean", "hospital.volume_mean", "hosp.totalbeds_mean", "hosp.p_medicare_mean",
           "hosp.cicu_mean", "hosp.teaching_minor_mean", "hosp.teaching_nonteaching_mean", 
           "hosp.controltype_notforprofit_mean", "hosp.controltype_gov.nonfed_mean", "hosp.controltype_gov.fed_mean",
           "admission.type_Elective_mean"), 
         .direction="down") %>% 
    fill(c("age_mean_prev", "comorb_ami_mean_prev", 
           "comorb_dementia_mean_prev", "comorb_afib_mean_prev", "comorb_ckd_mean_prev", 
           "comorb_copd_mean_prev", "comorb_chf_mean_prev", "comorb_diabetes_mean_prev", 
           "comorb_cad_mean_prev", "comorb_stroketia_mean_prev",
           "inpatient_mean_prev", "hospital.volume_mean_prev", "hosp.totalbeds_mean_prev", "hosp.p_medicare_mean_prev",
           "hosp.cicu_mean_prev", "hosp.teaching_minor_mean_prev", "hosp.teaching_nonteaching_mean_prev", 
           "hosp.controltype_notforprofit_mean_prev", "hosp.controltype_gov.nonfed_mean_prev", "hosp.controltype_gov.fed_mean_prev",
           "admission.type_Elective_mean_prev"), 
         .direction="down") %>% ungroup()
  dataset4 <- dataset3 %>% 
    mutate(observed=ifelse(is.na(obs_surgeon_period),0,1),
           observed_prev=lag(observed,1), no_cases_prev=lag(no_cases,1),
           observed_prev=ifelse(surgeon_period==1,0,observed_prev), no_cases_prev=ifelse(surgeon_period==1,1,no_cases_prev),
           period=ifelse(is.na(period),99,period),
           volume.zero=ifelse(volume==0,1,0)) %>% 
    select(-c(obs_surgeon_period, first_period))
  return(dataset4)
}

merge.surgeon.baseline.covariates_function <- function(dataset, dat=cabg.base){
  surgeon.baseline.covariates <- dat %>% 
    select(op_npi, date_or, surgeon.age, md_female, hosp.totalbeds) %>% 
    arrange(op_npi, date_or) %>% 
    group_by(op_npi) %>% filter(row_number()==1) %>% 
    ungroup() %>% select(-date_or)
  dataset2 <- merge(dataset, surgeon.baseline.covariates, 
        by="op_npi", all.x=TRUE)
  return(dataset2)
}

outcome_function <- function(dataset){
  dataset %>% mutate(death.percent=ifelse(volume==0,0,death.percent),
                     death.count=ifelse(volume==0,0,death.count),
                     death.percent_prev=ifelse(volume_prev==0,0,lag(death.percent)),
                     death.count_prev=ifelse(volume_prev==0,0,lag(death.count)),
                     death.percent_prev2=ifelse(volume_prev2==0,0,lag(death.percent,2)))
}

complete.case_function <- function(dat){
  dat %>% filter(!(op_npi %in% {unique(.[!complete.cases(.),]$op_npi)})) # FILTER: complete case
}

# -------------
# combined/compiling function
# -------------
# discrete time setup
discrete.time_function <- function(dat, ...){
  if(!require("pacman",character.only=T)) {install.packages("pacman")}
  pacman::p_load(tidyverse, survival, lubridate, splines, here, fastDummies)
  dat %>% create.dummies_function(.) %>% 
    add.period.volumes_function(.) %>% 
    add.covariate.means_function(.) %>% 
    create.and.merge.grid_function(.) %>% 
    fill.and.format_function(.) %>% 
    merge.surgeon.baseline.covariates_function(dataset=., ...) %>% 
    outcome_function(.) %>% 
    complete.case_function(.)
}

