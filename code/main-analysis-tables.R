# main analysis tables
pacman::p_load(xtable)

load(here::here("objects","nested-1-interval.Rda"))
load(here::here("objects","nested-4-interval.Rda"))

source(here("main-analysis_scripts","target-trial-3-standardization_function.R"))
source(here("main-analysis_scripts","target-trial-3-ipw_function.R"))
source(here("main-analysis_scripts", "target-trial-4_function.R"))
source(here("main-analysis_scripts","target-trial-5-standardization_function.R"))
source(here("main-analysis_scripts","target-trial-5-ipw_function.R"))
source(here("main-analysis_scripts", "target-trial-6_function.R"))

# Results
results <- list()

# Estimates from target trial #3
    ### Standardization
results$trial_3_standardization <- tt_3_standardization_function(dat=nested_1_interval)

  ### IP weights
trial_3 <- list()
trial_3$models <- tt_3_models_function(dat=nested_1_interval)
trial_3$basedata <- tt_3_weights_function(dat=nested_1_interval, weight.models=trial_3$models)
results$trial_3_ipw <- pblapply(setNames(-5:5, -5:5), function(assign) {tt_3_regimes_infinite_function(x=assign) %>% 
    tt_3_outcome_function(dat=.) %>% .$estimate} ) %>% unlist() %>% {data.frame("assign"=-5:5, "preds"=.)}
results$trial_3_ipw_parametric <- pblapply(setNames(-5:5, -5:5), function(assign) {tt_3_regimes_infinite_function(x=assign) %>% mutate(x=assign)}) %>% 
  bind_rows(.) %>% tt_3_outcome_parametric_function(dat=.)



# Estimates from target trial #4
trial_4 <- list()
trial_4$models <- tt_4_models_function(dat=nested_4_interval)
trial_4$basedata <- tt_4_weights_function(dat=nested_4_interval, weight.models=trial_4$models)
save(trial_4, file=here("objects","trial-4-pre-results.Rda"))

results$trial_4 <- tt_4_parallel_function(cores=11, range=-5:5)
results$trial_4_parametric <- tt_4_parametric_parallel_function(cores=11, range=-5:5)
results$sustained.natural <- tt_4_regimes_infinite_function(x=0, natural=TRUE) %>% tt_4_outcome_function(dat=.) %>% {data.frame("assign"=0, "preds"=.$estimate)}




# Estimates from target trial #5
    ### Standardization
results$trial_5_standardization <- tt_5_standardization_function(dat=nested_1_interval)

    ### IP weights
trial_5 <- list()
trial_5$models <- tt_3_models_function(dat=nested_1_interval) # same as 3
trial_5$basedata <- tt_3_weights_function(dat=nested_1_interval, weight.models=trial_3$models) # same as 3
results$trial_5_ipw <- pblapply(setNames(-5:5, -5:5), function(assign) {tt_5_regimes_finite_function(x=assign) %>% 
    tt_3_outcome_function(dat=.) %>% .$estimate} ) %>% unlist() %>% {data.frame("assign"=-5:5, "preds"=.)} # same as 3
results$trial_5_ipw_parametric <- pblapply(setNames(-5:5, -5:5), function(assign) {tt_5_regimes_finite_function(x=assign) %>% mutate(x=assign)}) %>% 
  bind_rows(.) %>% tt_3_outcome_parametric_function(dat=.) # same as 3


# Estimates from target trial #6 (finite)
results$trial_6 <- tt_6_parallel_function(cores=11, range=-5:5)
results$trial_6_parametric <- tt_6_parametric_parallel_function(cores=11, range=-5:5)


save(results, file=here("objects","results.Rda"))



# round(cbind(results$trial_4, results$trial_4_parametric$estimate), 3)
# round(cbind(results$trial_6, results$trial_6_parametric$estimate),3)
