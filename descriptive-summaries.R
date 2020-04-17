# descriptive statistics for manuscript
source(here::here("descriptive-summaries_scripts", "descriptive-stats_function.R"))
source(here::here("descriptive-summaries_scripts", "trial-stats_functions.R"))
source(here::here("descriptive-summaries_scripts", "table1-surgeon-cabg_function.R"))
source(here::here("descriptive-summaries_scripts", "non-parametric_functions.R"))

load(here::here("objects","d.Rda"))

    ### miscellaneous descriptive statistics
desc.results <- desc_stats_function(dat=d %>% mutate(volume_lag1=lag(volume,1),
                                                     volume_lag1=ifelse(is.na(volume_lag1),0,volume_lag1)))

    ### min and max operative regimes observed for each trial
minmax <- list()
minmax$trial_1 <- trial_1_stats_function(dat=nested_1_interval)
minmax$trial_2 <- trial_2_stats_function(dat=nested_4_interval)
minmax$trial_3 <- trial_3_stats_function(dat=nested_1_interval)
minmax$trial_4 <- trial_4_stats_function(dat=nested_4_interval)

    ### table 1
table_1 <- surgeon.table1.fun(dat=d %>% mutate(volume_lag1=lag(volume,1),
                                    volume_lag1=ifelse(is.na(volume_lag1),0,volume_lag1)), title="CABG")

    ### number of surgeons following each regime (all trials)
np.tables <- combined_non_parametric_function(dat_1 = nested_1_interval, dat_4=nested_4_interval)
  
    ### number of surgeons following each regime conditional on covariate history (all trials)
positivity.tables <- combined_positivity_function(dat_1 = nested_1_interval, dat_4=nested_4_interval)
