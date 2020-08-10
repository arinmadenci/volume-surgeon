# standardization

tt_3_standardization_function <- function(dat=nested_1_interval, save.model){
  # current period volume
  m <- glm(data = dat %>% filter(!is.na(death.percent_lead1)), 
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
                                 ns(volume_prev,
                                    knots=quantile(volume_prev, probs=c(0.35, 0.65)),
                                    Boundary.knots=quantile(volume_prev, probs=c(0.05,0.95))) +
                                 ns(death.percent_prev2,
                                    knots=c(0.03, 0.06, 0.1),
                                    Boundary.knots=c(0, 1)) +
                                 md_female + surgeon.age + hosp.p_medicare_mean_prev +
                                 comorb_ami_mean_prev + comorb_dementia_mean_prev + comorb_afib_mean_prev + comorb_ckd_mean_prev +
                                 comorb_copd_mean_prev +  comorb_chf_mean_prev +  comorb_diabetes_mean_prev + comorb_stroketia_mean_prev
  )
  print("outcome model fitting: complete")
  tt3.regimes <- -5:5
  matrix.tt3.current <- pblapply(tt3.regimes, 
                                 function(x) predict(m, newdata = dat %>% 
                                                       mutate(volume = volume_lag1 + x,
                                                              volume = ifelse(volume<0,0,volume)), 
                                                     type="response") %>% mean) %>% unlist
  table.tt3.current <- data.frame(tt3.regimes, 
                                  round(matrix.tt3.current,3)) %>% as.matrix
  colnames(table.tt3.current) <- c("Change in volume",
                                   "90-day mortality")
  results <- list()
  if (save.model==TRUE){
  results$model <- m
  }
  results$table <- table.tt3.current
  return(results)
}
