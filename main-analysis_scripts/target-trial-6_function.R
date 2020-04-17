# target trial 6 functions
source(here::here("main-analysis_scripts","target-trial-4_function.R"))
source(here::here("main-analysis_scripts","dynamic-regimes_function.R"))
tt_6_regimes_finite_function <- function(dat=trial_4$basedata, x, natural=FALSE){ 
  dat <- dynamic_assignment_function(dat=dat, x=x)
  dat <- dat %>% mutate(assigned=pmax(baseline_volume + x, 0), # assigned is only for participants below the median
                        neg.assigned=ifelse(x>=-1, 
                                            pmax(baseline_volume - x, 0), # neg.assigned is only for participants above the median
                                            baseline_volume + num.available)) %>%
    group_by(op_npi.new) %>% 
    mutate(w_t_x = case_when(baseline_volume <  median.vol & surgeon_period==1 ~ 1, # PERIOD 1
                             baseline_volume <  median.vol & surgeon_period==2 & volume_lag1==assigned ~ 1, # PERIOD 2
                             baseline_volume <  median.vol & surgeon_period==2 & volume_lag1!=assigned & volume==assigned ~ w_t,
                             baseline_volume <  median.vol & surgeon_period==2 & volume_lag1!=assigned & volume!=assigned ~ 0,
                             baseline_volume <  median.vol & surgeon_period==3 & ( as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==2 ~ 1, # PERIOD 3
                             baseline_volume <  median.vol & surgeon_period==3 & ( as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==1 & volume==assigned ~ w_t,
                             baseline_volume <  median.vol & surgeon_period==3 & ( as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==1 & volume!=assigned ~ 0,
                             baseline_volume <  median.vol & surgeon_period==3 & ( as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )<1 ~ 0,
                             baseline_volume <  median.vol & surgeon_period==4 & ( as.numeric(volume_lag3==assigned) + as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==3 ~ 1, # PERIOD 4
                             baseline_volume <  median.vol & surgeon_period==4 & ( as.numeric(volume_lag3==assigned) + as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==2 & volume==assigned ~ w_t,
                             baseline_volume <  median.vol & surgeon_period==4 & ( as.numeric(volume_lag3==assigned) + as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )==2 & volume!=assigned ~ 0,
                             baseline_volume <  median.vol & surgeon_period==4 & ( as.numeric(volume_lag3==assigned) + as.numeric(volume_lag2==assigned) + as.numeric(volume_lag1==assigned) )<2 ~ 0,
                             
                             baseline_volume >= median.vol & surgeon_period==1 ~ 1, # PERIOD 1
                             baseline_volume >= median.vol & surgeon_period==2 & volume_lag1==neg.assigned ~ 1, # PERIOD 2
                             baseline_volume >= median.vol & surgeon_period==2 & volume_lag1!=neg.assigned & volume==neg.assigned ~ w_t,
                             baseline_volume >= median.vol & surgeon_period==2 & volume_lag1!=neg.assigned & volume!=neg.assigned ~ 0,
                             baseline_volume >= median.vol & surgeon_period==3 & ( as.numeric(volume_lag2==neg.assigned) + as.numeric(volume_lag1==neg.assigned) )==2 ~ 1, # PERIOD 3
                             baseline_volume >= median.vol & surgeon_period==3 & ( as.numeric(volume_lag2==neg.assigned) + as.numeric(volume_lag1==neg.assigned) )==1 & volume==neg.assigned ~ w_t,
                             baseline_volume >= median.vol & surgeon_period==3 & ( as.numeric(volume_lag2==neg.assigned) + as.numeric(volume_lag1==neg.assigned) )==1 & volume!=neg.assigned ~ 0,
                             baseline_volume >= median.vol & surgeon_period==3 & ( as.numeric(volume_lag2==neg.assigned) + as.numeric(volume_lag1==neg.assigned) )<1 ~ 0,
                             baseline_volume >= median.vol & surgeon_period==4 & ( as.numeric(volume_lag3==neg.assigned) + as.numeric(volume_lag2==neg.assigned) + as.numeric(volume_lag1==neg.assigned) )==3 ~ 1, # PERIOD 4
                             baseline_volume >= median.vol & surgeon_period==4 & ( as.numeric(volume_lag3==neg.assigned) + as.numeric(volume_lag2==neg.assigned) + as.numeric(volume_lag1==neg.assigned) )==2 & volume==neg.assigned ~ w_t,
                             baseline_volume >= median.vol & surgeon_period==4 & ( as.numeric(volume_lag3==neg.assigned) + as.numeric(volume_lag2==neg.assigned) + as.numeric(volume_lag1==neg.assigned) )==2 & volume!=neg.assigned ~ 0,
                             baseline_volume >= median.vol & surgeon_period==4 & ( as.numeric(volume_lag3==neg.assigned) + as.numeric(volume_lag2==neg.assigned) + as.numeric(volume_lag1==neg.assigned) )<2 ~ 0
    ),
    w_c = case_when(surgeon_period<4~1,
                    surgeon_period==4 & censor.Kplus1==1~0,
                    surgeon_period==4 & censor.Kplus1==0~w_c))
  if (natural==TRUE){dat <- dat %>% mutate(w_t_x=1)}
  dat <- dat %>% group_by(op_npi.new) %>% 
    mutate(w = cumprod(w_t_x*w_c)) %>% 
    ungroup()
  print(c("x"=x, "99.9th %ile"=quantile(dat$w, probs=0.999),
          "max"=max(dat$w)))
  return(dat)
}



tt_6_parallel_function <- function(cores, range){
  if(!require("pacman",character.only=T)){install.packages("pacman")}
  pacman::p_load(parallel, ParallelLogger, dplyr, pbapply, lubridate, here)
  t1 <- Sys.time()
  cluster <- makeCluster(numberOfThreads = cores)
  clusterEvalQ(cluster, {library(dplyr); library(magrittr); library(lubridate); library(splines)})
  clusterExport(cluster, c("trial_4", "tt_6_regimes_finite_function", "tt_4_outcome_function", "dynamic_assignment_function"))
  output <- parLapply(cl=cluster,
                      setNames(range, range), function(assign) {tt_6_regimes_finite_function(x=assign) %>% tt_4_outcome_function(dat=.) %>% .$estimate} ) %>% unlist() %>% {data.frame("assign"=range, "preds"=.)}
  t2 <- Sys.time()
  print(t2-t1)
  on.exit(stopCluster(cluster))
  return(output)
}

tt_6_parametric_parallel_function <- function(cores, range){
  if(!require("pacman",character.only=T)){install.packages("pacman")}
  pacman::p_load(parallel, ParallelLogger, dplyr, pbapply, lubridate, here)
  t1 <- Sys.time()
  cluster <- makeCluster(numberOfThreads = cores)
  clusterEvalQ(cluster, {library(dplyr); library(magrittr); library(lubridate); library(splines)})
  clusterExport(cluster, c("trial_4", "tt_6_regimes_finite_function", "tt_4_outcome_parametric_function", "dynamic_assignment_function"))
  output <- parLapply(cl=cluster,
                      setNames(range, range), function(assign) {tt_6_regimes_finite_function(x=assign) %>% mutate(x=assign)}
  ) %>% bind_rows(.) %>% tt_4_outcome_parametric_function(dat=.)
  t2 <- Sys.time()
  print(t2-t1)
  on.exit(stopCluster(cluster))
  return(output)
}