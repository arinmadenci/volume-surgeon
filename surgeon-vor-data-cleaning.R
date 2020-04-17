# source(here::here("vor-data-cleaning-scripts", "merge-inpt-cc-denom-md_functions.R"))
# base <- compile_function()
# save(base, file=here::here("objects", "base.rda"))
# 

load("D:/Projects/Arin/vor/objects/base.rda")

base.2011 <- base$base2011
base.2012 <- base$base2012
base.2013 <- base$base2013
base.2014 <- base$base2014
base.2015 <- base$base2015
base.2016 <- base$base2016; rm(base)

### formatting and restrictions:
    # - cabg
    # - cardiac/thoracic surgeon
    # - non-HMO (fee-for-service only)
    # - >65yo
    # - first observed operation of patients
    # - complete case

source(here::here("vor-data-cleaning_scripts", "operative-volume_functions.R"))
cabg.all <- procedure.pack(procedure="cabg", cores=15) # 
# save(cabg.all, file=here::here("objects", "cabg-all.rda"))

source(here::here("vor-data-cleaning_scripts", "restriction-and-formatting_functions.R"))
cabg.base <- combo.pack(dat=cabg.all, abbrv="cabg", exclude.nonelective=FALSE, exclude.emergent=FALSE)
save(cabg.base, file=here::here("objects", "cabg-base.rda"))

source(here::here("vor-data-cleaning_scripts", "discrete-time_functions.R"))
d <- discrete.time_function(dat=cabg.base)
save(d, file=here("objects","d.Rda"))

source(here::here("vor-data-cleaning_scripts", "nested-trial_function.R"))
nested_1_interval <- nested_function(dat=d, K=1) # n=35,593
nested_4_interval <- nested_function(dat=d, K=4) # n=133,063
save(nested_1_interval, file=here("objects","nested-1-interval.Rda"))
save(nested_4_interval, file=here("objects","nested-4-interval.Rda"))



# -------------------------------------
# sensitivity analysis
sensitivity_nested_4_interval <- nested_4_interval %>% group_by(op_npi.new) %>% 
  mutate(exclude = ifelse(lag(censor_lead1)==1 | lag(censor_lead1,2)==1 | lag(censor_lead1,3)==1, 1, NA)) %>% 
  fill(exclude, .direction = "down") %>% ungroup() %>% 
  filter(is.na(exclude))



surgeons.at.highvolhosp <- cabg.base %>% group_by(op_npi) %>% arrange(date_or) %>% 
  filter(row_number()==1) %>% filter(hospital.volume>24) %>% select(op_npi) %>% ungroup() %>% unique
tt_1.high <- tt_1_interval %>% filter(op_npi %in% surgeons.at.highvolhosp$op_npi)
tt_4.high <- tt_1_interval %>% filter(op_npi %in% surgeons.at.highvolhosp$op_npi)
