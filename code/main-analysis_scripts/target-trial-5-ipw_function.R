# target trial 5 ipw function

tt_5_regimes_finite_function <- function(dat = trial_5$basedata, x, natural=FALSE){ # applies the same dynamic regime to all
  source(here::here("main-analysis_scripts","target-trial-3-ipw_function.R"))
  source(here::here("main-analysis_scripts","dynamic-regimes_function.R"))
  dat <- dynamic_assignment_function(dat=dat, x=x)
  dat <- dat %>% mutate(assigned=pmax(baseline_volume + x, 0), # assigned is only for participants below the median
                        neg.assigned=ifelse(x>=-1, 
                                            pmax(baseline_volume - x, 0), # neg.assigned is only for participants above the median
                                            baseline_volume + num.available)) %>% 
    group_by(op_npi.new) %>% 
    mutate(w_t_x = case_when(baseline_volume < median.vol & volume==assigned ~ w_t,
                             baseline_volume < median.vol & volume!=assigned ~ 0,
                             baseline_volume >= median.vol & volume==neg.assigned ~ w_t,
                             baseline_volume >= median.vol & volume!=neg.assigned ~ 0)
    )
  if (natural==TRUE){dat <- dat %>% mutate(w_t_x=1)}
  dat <- dat %>% group_by(op_npi.new) %>% 
    mutate(w = cumprod(w_t_x*w_c)) %>% 
    ungroup()
  print(c("x"=x, "99.9th %ile"=quantile(dat$w, probs=0.999),
          "max"=max(dat$w)))
  return(dat)
}
