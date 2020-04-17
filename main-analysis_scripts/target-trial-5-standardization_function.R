tt_5_standardization_function <- function(dat=nested_1_interval){
  # current period volume
  m <- geepack::geeglm(data = dat %>% filter(!is.na(death.percent_lead1)), 
                       family = "binomial", 
                       cbind(death.count_lead1, volume_lead1 - death.count_lead1) ~ 
                         ns(volume,
                            knots=quantile(volume, probs=c(0.35, 0.65)),
                            Boundary.knots=quantile(volume, probs=c(0.05,0.95))
                         )*ns(hospital.volume_mean_prev,
                              knots=quantile(hospital.volume_mean_prev, probs=c(0.35, 0.65)),
                              Boundary.knots=quantile(hospital.volume_mean_prev, probs=c(0.05,0.95))) +
                         ns(enter_period,
                            knots=quantile(enter_period, probs=c(0.35, 0.65)),
                            Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
                         ns(hosp.totalbeds_mean,
                            knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)),
                            Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95))) +
                         ns(admission.type_Elective_mean_prev,
                            knots=quantile(admission.type_Elective_mean_prev, probs=c(0.35, 0.65)),
                            Boundary.knots=quantile(admission.type_Elective_mean_prev, probs=c(0.05,0.95))) +
                         ns(cumave.volume_prev,
                            knots=quantile(cumave.volume_prev, probs=c(0.35, 0.65)),
                            Boundary.knots=quantile(cumave.volume_prev, probs=c(0.05,0.95))) +
                         ns(death.percent_prev2,
                            knots=c(0.03, 0.06, 0.1),
                            Boundary.knots=c(0, 1)) +
                         md_female + surgeon.age + hosp.p_medicare_mean_prev +
                         comorb_ami_mean_prev + comorb_dementia_mean_prev + comorb_afib_mean_prev + comorb_ckd_mean_prev +
                         comorb_copd_mean_prev +  comorb_chf_mean_prev +  comorb_diabetes_mean_prev + comorb_stroketia_mean_prev,
                       id=op_npi,
                       corstr="unstructured"
  )
  print("outcome model fitting: complete")
  tt5.regimes <- -5:5
  median.vol <- quantile(dat$volume_lag1, probs=0.5) %>% as.numeric
  matrix.tt5.current <- pblapply(tt5.regimes, 
                                 function(x) predict(m, newdata = dat %>% 
                                                       mutate(volume = case_when(baseline_volume<median.vol ~ baseline_volume + x,
                                                                                 baseline_volume>=median.vol ~ baseline_volume - x),
                                                              volume = ifelse(volume<0,0,volume)), 
                                                     type="response") %>% mean) %>% unlist
  table.tt5.current <- data.frame(tt5.regimes, 
                                  round(matrix.tt5.current,4)) %>% as.matrix
  colnames(table.tt5.current) <- c("Change in volume (added for $\\textless$ median, subtracted for $\\geq$ median)",
                                   "90-day mortality")
  results <- list()
  results$model <- m
  results$table <- table.tt5.current
  return(results)
}
