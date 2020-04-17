# functions for sensitivity analysis tables

tt_4_models_sensitivity_function <- function(dat, ...){
  pacman::p_install(MASS, force = FALSE); pacman::p_load(splines)
  if(!require("pacman",character.only=T)){install.packages("pacman")}
  
  model.formulas <- list()
  model.formulas$num_t <- "ns(volume_lag1, knots=quantile(volume_lag1, probs=c(0.35, 0.65)), Boundary.knots=quantile(volume_lag1, probs=c(0.05, 0.95))) + 
  md_female + surgeon.age + 
  ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
  ns(hosp.totalbeds_mean, knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95))) +
  as.factor(surgeon_period)"
  model.formulas$denom_t <- "ns(volume_lag1, knots=quantile(volume_lag1, probs=c(0.35, 0.65)), Boundary.knots=quantile(volume_lag1, probs=c(0.05, 0.95))) +
  ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
  comorb_ami_mean_prev + comorb_dementia_mean_prev + comorb_afib_mean_prev + comorb_ckd_mean_prev + comorb_copd_mean_prev +  comorb_chf_mean_prev + comorb_diabetes_mean_prev + comorb_stroketia_mean_prev + 
  ns(admission.type_Elective_mean_prev, knots=quantile(admission.type_Elective_mean_prev, probs=c(0.35, 0.65)), Boundary.knots=quantile(admission.type_Elective_mean_prev, probs=c(0.05,0.95))) +
  ns(hospital.volume_mean_prev, knots=quantile(hospital.volume_mean_prev, probs=c(0.35, 0.65)), Boundary.knots=quantile(hospital.volume_mean_prev, probs=c(0.05,0.95))) +
  md_female + surgeon.age + hosp.p_medicare_mean_prev + 
  ns(hosp.totalbeds_mean, knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95))) +
  ns(death.percent_prev, knots=c(0.03, 0.06, 0.1), Boundary.knots=c(0, 1)) +
  as.factor(surgeon_period)"
  model.formulas$num_c <- "ns(volume, knots=quantile(volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(volume, probs=c(0.05, 0.95))) +
  md_female + surgeon.age + ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
  ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
  ns(hosp.totalbeds_mean, knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95))) +
  as.factor(surgeon_period)"
  model.formulas$denom_c <- "ns(volume, knots=quantile(volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(volume, probs=c(0.05, 0.95))) +
  ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
  comorb_ami_mean + comorb_dementia_mean + comorb_afib_mean + comorb_ckd_mean + comorb_copd_mean +  comorb_chf_mean + comorb_diabetes_mean + comorb_stroketia_mean + 
  ns(admission.type_Elective_mean, knots=quantile(admission.type_Elective_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(admission.type_Elective_mean, probs=c(0.05,0.95))) +
  ns(hospital.volume_mean, knots=quantile(hospital.volume_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hospital.volume_mean, probs=c(0.05,0.95))) +
  md_female + surgeon.age + hosp.p_medicare_mean + 
  ns(hosp.totalbeds_mean, knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95))) +
  ns(death.percent, knots=c(0.03, 0.06, 0.1), Boundary.knots=c(0, 1)) +
  as.factor(surgeon_period)"
  
  # WEIGHT MODELS
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



tt_4_regimes_infinite_sensitivity_function <- function(dat=sensitivity_trial_4$basedata, x, natural=FALSE){ 
  dat <- dat %>% mutate(assigned=pmax(baseline_volume+x, 0)) %>% 
    group_by(op_npi.new) %>% 
    mutate(w_t_x = case_when(surgeon_period==1 ~ 1, # PERIOD 1
                             surgeon_period==2 & volume_lag1==assigned ~ 1, # PERIOD 2
                             surgeon_period==2 & volume_lag1!=assigned & volume==assigned ~ w_t,
                             surgeon_period==2 & volume_lag1!=assigned & volume!=assigned ~ 0,
                             surgeon_period==3 & ( as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==2 ~ 1, # PERIOD 3
                             surgeon_period==3 & ( as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==1 & volume==assigned ~ w_t,
                             surgeon_period==3 & ( as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==1 & volume!=assigned ~ 0,
                             surgeon_period==3 & ( as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )<1 ~ 0,
                             surgeon_period==4 & ( as.numeric(volume_lag3==assigned) + as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==3 ~ 1, # PERIOD 4
                             surgeon_period==4 & ( as.numeric(volume_lag3==assigned) + as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==2 & volume==assigned ~ w_t,
                             surgeon_period==4 & ( as.numeric(volume_lag3==assigned) + as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==2 & volume!=assigned ~ 0,
                             surgeon_period==4 & ( as.numeric(volume_lag3==assigned) + as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )<2 ~ 0
    ),
    w_c = case_when(censor_lead1==1~0,
                    censor_lead1==0~w_c))
  if (natural==TRUE){dat <- dat %>% mutate(w_t_x=1)}
  dat <- dat %>% group_by(op_npi.new) %>% 
    mutate(w = cumprod(w_t_x*w_c)) %>% 
    ungroup()
  print(c("x"=x, "99.9th %ile"=quantile(dat$w, probs=0.999),
          "max"=max(dat$w)))
  return(dat)
}



tt_4_outcome_sensitivity_function <- function(dat, totalvisit=4){
  # time-fixed covariate dataset
  data_for_risk_estimates0 <- data.frame(op_npi.new=rep(unique(dat$op_npi.new),
                                                        each=totalvisit),
                                         surgeon_period=as.factor(rep(1:totalvisit, times=length(unique(dat$op_npi.new)))),
                                         stringsAsFactors = FALSE)
  data_baseline <- dat %>% filter(surgeon_period==1) %>%
    select("op_npi.new", "md_female", "surgeon.age","hosp.totalbeds_mean", "enter_period", 
           baseline_volume="volume")
  data_for_risk_estimates <- left_join(data_for_risk_estimates0, data_baseline, by="op_npi.new")
  # outcome model
  model_last <- glm(data=dat %>% filter(censor_lead1==0),
                    family="binomial",
                    weights=w,
                    cbind(death.count_lead1, volume_lead1 - death.count_lead1) ~
                      ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
                      md_female +
                      ns(baseline_volume,
                         knots=quantile(baseline_volume, probs=c(0.35, 0.65)),
                         Boundary.knots=quantile(baseline_volume, probs=c(0.05,0.95))) +
                      ns(surgeon.age,
                         knots=quantile(surgeon.age, probs=c(0.35, 0.65)),
                         Boundary.knots=quantile(surgeon.age, probs=c(0.05,0.95))) +
                      ns(hosp.totalbeds_mean,
                         knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)),
                         Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95))) +
                      as.factor(surgeon_period)
  )
  # standardized estimates
  pred <- predict(model_last, type="response", newdata=data_for_risk_estimates %>% filter(surgeon_period==4)) %>% mean
  outcome <- list()
  outcome$model <- model_last
  outcome$estimate <- pred
  outcome$weights <- c(mean=mean(dat$w), quantile(dat$w, probs=0.999), max=max(dat$w))
  return(outcome)
}


tt_4_outcome_parametric_sensitivity_function <- function(dat, totalvisit=4, range){
  # time-fixed covariate dataset
  data_for_risk_estimates0 <- data.frame(op_npi.new=rep(unique(dat$op_npi.new),
                                                        each=totalvisit),
                                         surgeon_period=as.factor(rep(1:totalvisit, times=length(unique(dat$op_npi.new)))),
                                         stringsAsFactors = FALSE)
  data_baseline <- dat %>% 
    select("op_npi.new", "md_female", "surgeon.age","hosp.totalbeds_mean", "enter_period",
           "baseline_volume")
  data_for_risk_estimates <- left_join(data_for_risk_estimates0, data_baseline, by="op_npi.new")
  # outcome model
  model_last <- glm(data=dat %>% filter(censor_lead1==0),
                    family="binomial",
                    weights=w,
                    cbind(death.count_lead1, volume_lead1 - death.count_lead1) ~
                      md_female +
                      ns(baseline_volume, knots=quantile(baseline_volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(baseline_volume, probs=c(0.05,0.95)))*
                      ns(x, knots=quantile(x, probs=c(0.35, 0.65)), Boundary.knots=quantile(x, probs=c(0.05,0.95))) +
                      as.factor(surgeon_period)*ns(x, knots=quantile(x, probs=c(0.35, 0.65)), Boundary.knots=quantile(x, probs=c(0.05,0.95)))*
                      ns(x, knots=quantile(x, probs=c(0.35, 0.65)), Boundary.knots=quantile(x, probs=c(0.05,0.95))) +
                      ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95)))*
                      ns(x, knots=quantile(x, probs=c(0.35, 0.65)), Boundary.knots=quantile(x, probs=c(0.05,0.95))) +
                      ns(surgeon.age, knots=quantile(surgeon.age, probs=c(0.35, 0.65)), Boundary.knots=quantile(surgeon.age, probs=c(0.05,0.95)))*
                      ns(x, knots=quantile(x, probs=c(0.35, 0.65)), Boundary.knots=quantile(x, probs=c(0.05,0.95))) +
                      ns(hosp.totalbeds_mean, knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95)))*
                      ns(x, knots=quantile(x, probs=c(0.35, 0.65)), Boundary.knots=quantile(x, probs=c(0.05,0.95)))
  )
  output <- list()
  output$model <- model_last
  output$weights <- c(mean=mean(dat$w), quantile(dat$w, probs=0.999), max=max(dat$w))
  # standardized estimates
  preds <- pblapply(setNames(range, range), function(assign) {
    predict(model_last, type="response", newdata=data_for_risk_estimates %>% filter(surgeon_period==4) %>% mutate(x = assign)
    ) %>% mean
  }) %>% unlist
  output$estimate <- preds
  return(output)
}

tt_4_parallel_sensitivity_function <- function(cores, range){
  if(!require("pacman",character.only=T)){install.packages("pacman")}
  pacman::p_load(parallel, ParallelLogger, dplyr, pbapply, lubridate, here)
  t1 <- Sys.time()
  cluster <- makeCluster(numberOfThreads = cores)
  clusterEvalQ(cluster, {library(dplyr); library(magrittr); library(lubridate); library(splines)})
  clusterExport(cluster, c("sensitivity_trial_4", "tt_4_regimes_infinite_sensitivity_function", "tt_4_outcome_sensitivity_function"))
  output <- parLapply(cl=cluster,
                      setNames(range, range), function(assign) {tt_4_regimes_infinite_sensitivity_function(x=assign) %>% tt_4_outcome_sensitivity_function(dat=.) %>% .$estimate} ) %>% unlist() %>% {data.frame("assign"=range, "preds"=.)}
  t2 <- Sys.time()
  print(t2-t1)
  on.exit(stopCluster(cluster))
  return(output)
}

tt_4_parametric_parallel_sensitivity_function <- function(cores, range){
  if(!require("pacman",character.only=T)){install.packages("pacman")}
  pacman::p_load(parallel, ParallelLogger, dplyr, pbapply, lubridate, here)
  t1 <- Sys.time()
  cluster <- makeCluster(numberOfThreads = cores)
  clusterEvalQ(cluster, {library(dplyr); library(magrittr); library(lubridate); library(splines)})
  clusterExport(cluster, c("sensitivity_trial_4", "tt_4_regimes_infinite_sensitivity_function", "tt_4_outcome_parametric_sensitivity_function"))
  output <- parLapply(cl=cluster,
                      setNames(range, range), function(assign) {tt_4_regimes_infinite_sensitivity_function(x=assign) %>% mutate(x=assign)}
  ) %>% bind_rows(.) %>% tt_4_outcome_parametric_sensitivity_function(dat=., range=range)
  t2 <- Sys.time()
  print(t2-t1)
  on.exit(stopCluster(cluster))
  return(output)
}
