
tt_3_models_function <- function(dat = nested_1_interval){
  p_boot(); pacman::p_install(MASS, force=FALSE); pacman::p_load(splines)
  
  model.formulas <- list()
  model.formulas$num_t <- "ns(volume_lag1, knots=quantile(volume_lag1, probs=c(0.35, 0.65)), Boundary.knots=quantile(volume_lag1, probs=c(0.05, 0.95)))"
  model.formulas$denom_t <- "ns(volume_lag1, knots=quantile(volume_lag1, probs=c(0.35, 0.65)), Boundary.knots=quantile(volume_lag1, probs=c(0.05, 0.95))) +
  ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
  comorb_ami_mean_prev + comorb_dementia_mean_prev + comorb_afib_mean_prev + comorb_ckd_mean_prev + comorb_copd_mean_prev +  comorb_chf_mean_prev + comorb_diabetes_mean_prev + comorb_stroketia_mean_prev + 
  ns(admission.type_Elective_mean_prev, knots=quantile(admission.type_Elective_mean_prev, probs=c(0.35, 0.65)), Boundary.knots=quantile(admission.type_Elective_mean_prev, probs=c(0.05,0.95))) +
  ns(hospital.volume_mean_prev, knots=quantile(hospital.volume_mean_prev, probs=c(0.35, 0.65)), Boundary.knots=quantile(hospital.volume_mean_prev, probs=c(0.05,0.95))) +
  md_female + surgeon.age + hosp.p_medicare_mean_prev + 
  ns(hosp.totalbeds_mean, knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95))) +
  ns(death.percent_prev2, knots=c(0.03, 0.06, 0.1), Boundary.knots=c(0, 1))"
  model.formulas$num_c <- "ns(volume, knots=quantile(volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(volume, probs=c(0.05, 0.95)))"
  model.formulas$denom_c <- "ns(volume, knots=quantile(volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(volume, probs=c(0.05, 0.95))) +
  ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
  comorb_ami_mean + comorb_dementia_mean + comorb_afib_mean + comorb_ckd_mean + comorb_copd_mean +  comorb_chf_mean + comorb_diabetes_mean + comorb_stroketia_mean + 
  ns(admission.type_Elective_mean, knots=quantile(admission.type_Elective_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(admission.type_Elective_mean, probs=c(0.05,0.95))) +
  ns(hospital.volume_mean, knots=quantile(hospital.volume_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hospital.volume_mean, probs=c(0.05,0.95))) +
  md_female + surgeon.age + hosp.p_medicare_mean + 
  ns(hosp.totalbeds_mean, knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95))) +
  ns(death.percent_prev, knots=c(0.03, 0.06, 0.1), Boundary.knots=c(0, 1))"
  
  weight.models <- list()
  # treatment: mdoels
  print("treatment models")
  weight.models$num_t.negbin <- MASS::glm.nb(data=dat, 
                                             formula = as.formula(paste0("volume ~ ", model.formulas$num_t)))
  weight.models$num_t.dev <- glm(data=dat,
                                 family="Gamma"(link=log),
                                 formula = as.formula(paste0("(weight.models$num_t.negbin$residuals)^2 ~ ", model.formulas$num_t)))
  
  weight.models$denom_t.negbin <- MASS::glm.nb(data=dat,
                                               formula = as.formula(paste0("volume ~ ", model.formulas$denom_t)))
  weight.models$denom_t.dev <- glm(data=dat,
                                   family="Gamma"(link=log),
                                   formula = as.formula(paste0("(weight.models$num_t.negbin$residuals)^2 ~ ", model.formulas$denom_t)))
  
  # censoring: models
  print("censoring models")
  weight.models$num_c <- glm(data=dat,
                             family="binomial",
                             formula = as.formula(paste0("censor_lead1==0 ~ ", model.formulas$num_c)))
  weight.models$denom_c <- glm(data=dat,
                               family="binomial",
                               formula = as.formula(paste0("censor_lead1==0 ~ ", model.formulas$denom_c)))
  return(weight.models)
}



tt_3_weights_function <- function(dat = nested_1_interval, weight.models = trial_3$models){
  # treatment: weights
  dat$num_t.preds <- predict(weight.models$num_t.negbin, newdata=dat, type="response")
  dat$num_t.theta <- predict(weight.models$num_t.dev, newdata=dat, type="response")
  dat$n_t <- dnbinom(dat$volume, mu=dat$num_t.preds, size=dat$num_t.theta)
  dat$denom_t.preds <- predict(weight.models$denom_t.negbin, newdata=dat, type="response")
  dat$denom_t.theta <- predict(weight.models$denom_t.dev, newdata=dat, type="response")
  dat$d_t <- dnbinom(dat$volume, mu=dat$denom_t.preds, size=dat$denom_t.theta)
  
  # censoring: weights
  dat$n_c <- predict(weight.models$num_c, newdata=dat, type="response")
  dat$d_c <- predict(weight.models$denom_c, newdata=dat, type="response")
  
  # combine weights
  dat <- dat %>% group_by(op_npi.new) %>% 
    mutate(w_t = n_t / d_t,
           w_c = n_c / d_c
    ) %>% ungroup()
}



tt_3_regimes_infinite_function <- function(dat = trial_3$basedata, x, natural=FALSE){ # applies the same dynamic regime to all
  dat <- dat %>% group_by(op_npi.new) %>% 
    mutate(w_t_x = case_when(volume==max(baseline_volume+x, 0) ~ w_t,
                             volume!=max(baseline_volume+x, 0) ~ 0)
           )
  if (natural==TRUE){dat <- dat %>% mutate(w_t_x=1)}
  dat <- dat %>% group_by(op_npi.new) %>% 
    mutate(w = cumprod(w_t_x*w_c)) %>% 
    ungroup()
  print(c("x"=x, "99.9th %ile"=quantile(dat$w, probs=0.999),
          "max"=max(dat$w)))
  return(dat)
}



tt_3_outcome_function <- function(dat, totalvisit=1){
  pred <- weighted.mean(x=dat %>% filter(censor_lead1==0) %>% .$death.percent_lead1,
                        w=dat %>% filter(censor_lead1==0) %>% .$w)
  outcome <- list()
  outcome$estimate <- pred
  outcome$weights <- c(mean=mean(dat$w), quantile(dat$w, probs=0.999), max=max(dat$w))
  return(outcome)
}



tt_3_outcome_parametric_function <- function(dat, totalvisit=1){
  model_last <- glm(data=dat %>% filter(censor_lead1==0),
                    family="binomial",
                    weights=w,
                    cbind(death.count_lead1, volume_lead1 - death.count_lead1) ~
                      ns(x,
                         knots=quantile(x, probs=c(0.35, 0.65)),
                         Boundary.knots=quantile(x, probs=c(0.05,0.95)))
  )
  output <- list()
  output$model <- model_last
  output$weights <- c(mean=mean(dat$w), quantile(dat$w, probs=0.999), max=max(dat$w))
  output$estimate <- predict(model_last, type="response", newdata=data.frame(x=-5:5))
  return(output)
}
