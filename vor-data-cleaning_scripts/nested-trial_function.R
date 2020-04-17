# nested trial functions

nested_function <- function(dat=d, K){
  dat <- dat %>% group_by(op_npi) %>% arrange(surgeon_period) %>% mutate(period_prev = lag(period,1)) %>% ungroup()
  d.nested.base <- bind_rows(pblapply(1:max(dat$surgeon_period), function(x){
    dat %>% group_by(op_npi) %>% 
      filter(row_number() >= x) %>% 
      mutate(surgeon_period=surgeon_period - x + 1, # re-number surgeon_period
             op_npi.new=paste0(op_npi,"_",ifelse(x>=10, x, paste0("0",x))),
             death.percent_lead1=ifelse(lead(volume)!=0,lead(death.percent),NA),
             death.count_lead1=ifelse(lead(volume)!=0,lead(death.count),NA),
             volume_lead1=lead(volume),
             volume_lag1=lag(volume,1),
             volume_lag1=ifelse(is.na(volume_lag1),0,volume_lag1),
             volume_lag2=lag(volume,2),
             volume_lag2=ifelse(is.na(volume_lag2),0,volume_lag2),
             volume_lag3=lag(volume,3),
             volume_lag3=ifelse(is.na(volume_lag3),0,volume_lag3)) %>% 
      ungroup()
  }
  )) %>% filter(surgeon_period <= K + 2)  # FILTER: end follow-up at K periods
  
  noneligible <- d.nested.base %>% filter((surgeon_period==1 | surgeon_period==2) & volume==0) %>% .$op_npi.new %>% unique # flag for meeting eligibility criteria
  
  d.nested <- d.nested.base %>% filter(!(op_npi.new %in% noneligible)) %>% # FILTER: applies eligibility criteria
    group_by(op_npi.new) %>% 
    mutate(death.count_lag1 = death.count_prev,
           censor_lead1=ifelse(is.na(death.count_lead1),1,0),
           zero=ifelse(no_cases==1,1,NA)) %>% 
    ungroup() %>% 
    filter(surgeon_period>=3) %>% 
    mutate(surgeon_period=surgeon_period-2) %>% 
    mutate(baseline_volume=ifelse(surgeon_period==1, volume_lag1, NA)) %>% 
    group_by(op_npi.new) %>% 
    fill(baseline_volume, .direction="down") %>% ungroup() %>% 
    mutate(change=volume - baseline_volume) %>% 
    group_by(op_npi.new) %>% 
    mutate(change_lag1 = lag(change,1),
           change_lag2 = lag(change,2),
           change_lag3 = lag(change,3)) %>% 
    mutate(enter_period = ifelse(surgeon_period==1, period_prev, NA)) %>% 
    fill(enter_period, .direction = "down") %>% 
    ungroup() %>% 
    select(-period_prev) %>% 
    arrange(op_npi.new, surgeon_period) %>% 
    mutate(censor.Kplus1=ifelse(surgeon_period==4, censor_lead1, 0))
  return(d.nested)
}

