
tt_4_models_function <- function(dat, ...){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_install(MASS, force=FALSE); pacman::p_load(splines)
  
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
  ns(death.percent_prev2, knots=c(0.03, 0.06, 0.1), Boundary.knots=c(0, 1)) +
  as.factor(surgeon_period)"
  model.formulas$num_c <- "ns(volume, knots=quantile(volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(volume, probs=c(0.05, 0.95))) +
  md_female + surgeon.age + ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
  ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
  ns(hosp.totalbeds_mean, knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95)))"
  model.formulas$denom_c <- "ns(volume, knots=quantile(volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(volume, probs=c(0.05, 0.95))) +
  ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95))) +
  comorb_ami_mean + comorb_dementia_mean + comorb_afib_mean + comorb_ckd_mean + comorb_copd_mean +  comorb_chf_mean + comorb_diabetes_mean + comorb_stroketia_mean + 
  ns(admission.type_Elective_mean, knots=quantile(admission.type_Elective_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(admission.type_Elective_mean, probs=c(0.05,0.95))) +
  ns(hospital.volume_mean, knots=quantile(hospital.volume_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hospital.volume_mean, probs=c(0.05,0.95))) +
  md_female + surgeon.age + hosp.p_medicare_mean + 
  ns(hosp.totalbeds_mean, knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)), Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95))) +
  ns(death.percent_prev, knots=c(0.03, 0.06, 0.1), Boundary.knots=c(0, 1))"
  
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
  weight.models$num_c <- glm(data=dat %>% filter(surgeon_period==4),
                             family="binomial",
                             formula = as.formula(paste0("censor.Kplus1==0 ~ ", model.formulas$num_c)))
  weight.models$denom_c <- glm(data=dat %>% filter(surgeon_period==4),
                               family="binomial",
                               formula = as.formula(paste0("censor.Kplus1==0 ~ ", model.formulas$denom_c)))
  return(weight.models)
}


tt_4_weights_function <- function(dat, weight.models=trial_4$models, ...){
  # treatment: weights
  dat$num_t.preds <- predict(weight.models$num_t.negbin, newdata=dat, type="response")
  dat$num_t.theta <- predict(weight.models$num_t.dev, newdata=dat, type="response")
  dat$n_t <- dnbinom(dat$volume, mu=dat$num_t.preds, size=dat$num_t.theta)
  dat$denom_t.preds <- predict(weight.models$denom_t.negbin, newdata=dat, type="response")
  dat$denom_t.theta <-predict(weight.models$denom_t.dev, newdata=dat, type="response")
  dat$d_t <- dnbinom(dat$volume, mu=dat$denom_t.preds, size=dat$denom_t.theta)
  
  # censoring: weights
  dat$n_c <- predict(weight.models$num_c, newdata=dat, type="response")
  dat$d_c <- predict(weight.models$denom_c, newdata=dat, type="response")

  # combine weights
  dat <- dat %>% 
    mutate(w_t = n_t / d_t,
           w_c = n_c / d_c)
}

tt_4_regimes_infinite_function <- function(dat=trial_4$basedata, x, natural=FALSE){ # applies the same dynamic regime to all
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
           w_c = case_when(surgeon_period<4~1,
                           surgeon_period==4 & censor.Kplus1==1~0,
                           surgeon_period==4 & censor.Kplus1==0~w_c))
  if (natural==TRUE){dat <- dat %>% mutate(w_t_x=1)}
  dat$w <- with(dat, ave(w_t_x*w_c, op_npi.new, FUN = cumprod))
  print(c("x"=x, "99.9th %ile"=quantile(dat$w, probs=0.999),
          "max"=max(dat$w)))
  return(dat)
}



tt_4_outcome_function <- function(dat, save.model){
  # time-fixed covariate dataset
  data_for_risk_estimates0 <- data.frame(op_npi.new=rep(unique(dat$op_npi.new),
                                                        each=1),
                                         stringsAsFactors = FALSE)
  data_baseline <- dat %>% filter(surgeon_period==1) %>%
    select("op_npi.new", "md_female", "surgeon.age","hosp.totalbeds_mean", "enter_period",
           "baseline_volume") %>% group_by(op_npi.new) %>% filter(row_number()==1)
  data_for_risk_estimates <- left_join(data_for_risk_estimates0, data_baseline, by="op_npi.new")
  # outcome model
  model_last <- glm(data=dat %>% filter(censor.Kplus1==0 & surgeon_period==4),
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
                         Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95)))
  )
  # standardized estimates
  pred <- predict(model_last, type="response", newdata=data_for_risk_estimates) %>% mean
  outcome <- list()
  if (save.model==TRUE){
  outcome$model <- model_last
  outcome$weights <- c(mean=mean(dat$w), quantile(dat$w, probs=0.999), max=max(dat$w))
  }
  outcome$estimate <- pred
  return(outcome)
}


tt_4_outcome_parametric_function <- function(dat, save.model){
  # time-fixed covariate dataset
  data_for_risk_estimates0 <- data.frame(op_npi.new=rep(unique(dat$op_npi.new),
                                                        each=1),
                                         stringsAsFactors = FALSE)
  data_baseline <- dat %>% filter(surgeon_period==1) %>%
    select("op_npi.new", "md_female", "surgeon.age","hosp.totalbeds_mean", "enter_period",
           "baseline_volume")
  data_for_risk_estimates <- left_join(data_for_risk_estimates0, data_baseline, by="op_npi.new")
  # outcome model
  model_last <- glm(data=dat %>% filter(censor.Kplus1==0 & surgeon_period==4),
                    family="binomial",
                    weights=w,
                    cbind(death.count_lead1, volume_lead1 - death.count_lead1) ~
                      md_female +
                      ns(baseline_volume,
                         knots=quantile(baseline_volume, probs=c(0.35, 0.65)),
                         Boundary.knots=quantile(baseline_volume, probs=c(0.05,0.95)))*
                      ns(x, knots=quantile(x, probs=c(0.35, 0.65)), Boundary.knots=quantile(x, probs=c(0.05,0.95))) +
                      ns(enter_period, knots=quantile(enter_period, probs=c(0.35, 0.65)), Boundary.knots=quantile(enter_period, probs=c(0.05,0.95)))*
                      ns(x, knots=quantile(x, probs=c(0.35, 0.65)), Boundary.knots=quantile(x, probs=c(0.05,0.95))) +
                      ns(surgeon.age,
                         knots=quantile(surgeon.age, probs=c(0.35, 0.65)),
                         Boundary.knots=quantile(surgeon.age, probs=c(0.05,0.95)))*
                      ns(x, knots=quantile(x, probs=c(0.35, 0.65)), Boundary.knots=quantile(x, probs=c(0.05,0.95))) +
                      ns(hosp.totalbeds_mean,
                         knots=quantile(hosp.totalbeds_mean, probs=c(0.35, 0.65)),
                         Boundary.knots=quantile(hosp.totalbeds_mean, probs=c(0.05,0.95)))*
                      ns(x, knots=quantile(x, probs=c(0.35, 0.65)), Boundary.knots=quantile(x, probs=c(0.05,0.95)))
  )
  output <- list()
  if (save.model==TRUE){
  output$model <- model_last
  output$weights <- c(mean=mean(dat$w), quantile(dat$w, probs=0.999), max=max(dat$w))
  }
  # standardized estimates
  output$estimate <- pblapply(setNames(-5:5, -5:5), function(assign) {
    predict(model_last, type="response", newdata=data_for_risk_estimates %>% mutate(x = assign)
    ) %>% mean
  }) %>% unlist
  return(output)
}

tt_4_parallel_function <- function(cores, range, ...){
  if (!require("pacman")) install.packages("pacman"); library(pacman); gc()
  pacman::p_load(parallel, ParallelLogger, dplyr, pbapply, lubridate, here)
  t1 <- Sys.time()
  cluster <- makeCluster(numberOfThreads = cores)
  clusterEvalQ(cluster, {library(dplyr); library(magrittr); library(lubridate); library(splines)})
  clusterExport(cluster, c("trial_4", "tt_4_regimes_infinite_function", "tt_4_outcome_function"))
  output <- parLapply(cl=cluster,
            setNames(range, range), function(assign) {tt_4_regimes_infinite_function(x=assign) %>% tt_4_outcome_function(dat=., ...) %>% .$estimate} ) %>% unlist() %>% {data.frame("assign"=range, "preds"=.)}
  t2 <- Sys.time()
  print(t2-t1)
  on.exit(stopCluster(cluster))
  return(output)
}

tt_4_parametric_parallel_function <- function(cores, range, ...){
  p_boot(); pacman::p_load(parallel, ParallelLogger, dplyr, pbapply, lubridate, here)
  t1 <- Sys.time()
  cluster <- makeCluster(numberOfThreads = cores)
  clusterEvalQ(cluster, {library(dplyr); library(magrittr); library(lubridate); library(splines)})
  clusterExport(cluster, c("trial_4", "tt_4_regimes_infinite_function", "tt_4_outcome_parametric_function"))
  output <- parLapply(cl=cluster,
                      setNames(range, range), function(assign) {tt_4_regimes_infinite_function(x=assign) %>% mutate(x=assign)}
                      ) %>% bind_rows(.) %>% tt_4_outcome_parametric_function(dat=., ...)
  t2 <- Sys.time()
  print(t2-t1)
  on.exit(stopCluster(cluster))
  return(output)
}
