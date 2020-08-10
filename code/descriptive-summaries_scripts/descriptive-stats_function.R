# descriptive statistics functions
desc_stats_function <- function(dat){
  pacman::p_load(dplyr)
  dat <- dat %>% group_by(op_npi) %>% 
    mutate(death.percent_lead1=ifelse(lead(volume)!=0,lead(death.percent),NA),
           death.count_lead1=ifelse(lead(volume)!=0,lead(death.count),NA),
           volume_lead1=lead(volume)) %>%
    ungroup() %>% 
    filter(op_npi %in% 
    {d %>% filter(volume != 0 & volume_lag1 != 0 & surgeon_period==2) %>% .$op_npi}) # only surgeons meeting eligibility criteria
  baseline.periods.dat <- dat %>% filter(surgeon_period %in% c(1,2))
  second.baseline.dat <- dat %>% filter(surgeon_period ==2 )
  desc.results <- list()
  # Number of surgeons in each analysis
  desc.results$n.surgeons_tt1 <- unique(dat$op_npi) %>% length
  
  # (First) baseline operative volume quantiles
  desc.results$first_baseline_volume <- second.baseline.dat %>% group_by(op_npi) %>% summarise(mean=mean(volume)) %>% {
    c("mean"=mean(.$mean),
      "min"=min(.$mean),
      "max"=max(.$mean),
      quantile(.$mean, probs=c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999)))
  } %>% data.frame(value=.)
  
  # Overall mean volume
  desc.results$overall_mean_volume <- d %>% group_by(op_npi) %>% summarise(mean=mean(volume)) %>% {c(mean=mean(.$mean), max=max(.$mean),
                                                                                                     quantile(.$mean, probs=c(0.95, 0.99)))}
  
  # Overall surgeon-specific mean mortality, measured among patients who underwent operations during the two baseline periods
  desc.results$baseline_mean_mortality <- baseline.periods.dat %>% group_by(op_npi) %>% 
    select(op_npi, surgeon_period, death.count, volume) %>%  
    summarise(mean.death=sum(death.count)/sum(volume)) %>% 
    {mean(.$mean.death)} # mean
  
  # Mortality by year / period
  desc.results$mortality_by_year <- dat %>% filter(no_cases==0) %>% 
    mutate(year = case_when(period %in% 1:4 ~ 2011,
                            period %in% 5:8 ~ 2012,
                            period %in% 9:12 ~ 2013,
                            period %in% 13:16 ~ 2014,
                            period %in% 17:20 ~ 2015,
                            period %in% 21:24 ~ 2016)) %>% 
    group_by(year, op_npi) %>% summarise(mean.death=sum(death.count)/sum(volume)) %>% ungroup() %>% 
    group_by(year) %>% summarise(mean=mean(mean.death))
  # Mean surgeon-specific mortality across all periods
  desc.results$mortality_overall <- dat %>% filter(no_cases==0) %>% group_by(op_npi) %>% summarise(death.mean.overall=sum(death.count)/sum(volume)) %>% ungroup() %>% {mean(.$death.mean.overall)}
  
  return(desc.results)
}
