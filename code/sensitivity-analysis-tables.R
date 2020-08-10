# sensitivity analysis
source(here::here("sensitivity-analysis_scripts","sensitivity-target-trial-4_functions.R"))
source(here::here("sensitivity-analysis_scripts","sensitivity-target-trial-6_functions.R"))
source(here::here("main-analysis_scripts", "target-trial-4_function.R"))
source(here::here("main-analysis_scripts", "target-trial-6_function.R"))
load("D:/Projects/Arin/vor/vor-surgeon/objects/sensitivity-nested-4-interval.Rda")
sens.results <- list()
# Estimates from target trial #4 - snsitivity analysis with not allowing zeros 
sensitivity_trial_4 <- list()
sensitivity_trial_4$models <- tt_4_models_sensitivity_function(dat=sensitivity_nested_4_interval)
sensitivity_trial_4$basedata <- tt_4_weights_function(dat=sensitivity_nested_4_interval, weight.models=sensitivity_trial_4$models) # weight assignment is same; uses different models
save(sensitivity_trial_4, file=here("objects","sensitivity-trial-4-pre-results.Rda"))

sens.results$sensitivity_trial_4 <- tt_4_parallel_sensitivity_function(cores=11, range=-5:5)
sens.results$sensitivity_trial_4_parametric <- tt_4_parametric_parallel_sensitivity_function(cores=11, range=-5:5)
# print(xtable(results$sensitivity_trial_4 %>% rename("Change in volume" = assign, "90-day mortality" = preds), align=c("llr"), digits=c(0,0,3),
#              caption="Mortality estimates for Target Trial \\#4 (IPW; each arm separate): Sensitivity analysis (censored for performing zero operations/interval)",
#              label="table:sensitivity-ests-trial-4-ipw-np"),
#       include.rownames=FALSE,
#       caption.placement="top",
#       type="latex", sanitize.text.function = function(x){x},
#       tabular.environment="longtable",
#       file=here::here("tables","sensitivity-estimates","sensitivity-ests-trial-4-ipw-np.tex"), floating=FALSE
# )
# print(xtable(data.frame(assign=-5:5, preds=results$sensitivity_trial_4_parametric$estimate) %>% 
#                rename("Change in volume" = assign, "90-day mortality" = preds), align=c("llr"), digits=c(0,0,3),
#              caption="Mortality estimates for Target Trial \\#4 (IPW; parametric): Sensitivity analysis (censored for performing zero operations/interval)",
#              label="table:sensitivity-ests-trial-4-ipw-parametric"),
#       include.rownames=FALSE,
#       caption.placement="top",
#       type="latex", sanitize.text.function = function(x){x},
#       tabular.environment="longtable",
#       file=here::here("tables","sensitivity-estimates","sensitivity-ests-trial-4-ipw-parametric.tex"), floating=FALSE
# )



# Estimates from target trial #6 - sensitivity analysis with not allowing zeros 
sens.results$sensitivity_trial_6 <- tt_6_parallel_sensitivity_function(cores=11, range=-5:5)
sens.results$sensitivity_trial_6_parametric <- tt_6_parametric_parallel_sensitivity_function(cores=11, range=-5:5)
# print(xtable(results$sensitivity_trial_6 %>% rename("Change in volume (added for \\textless median, subtracted for $\\geq$ median)" = assign, "90-day mortality" = preds), 
#              align=c("llr"), digits=c(0,0,3),
#              caption="Mortality estimates for Target Trial \\#6 (IPW; each arm separate): Sensitivity analysis (censored for performing zero operations/interval)",
#              label="table:sensitivity-ests-trial-6-ipw-np"),
#       include.rownames=FALSE,
#       caption.placement="top",
#       type="latex", sanitize.text.function = function(x){x},
#       tabular.environment="longtable",
#       file=here::here("tables","sensitivity-estimates","sensitivity-ests-trial-6-ipw-np.tex"), floating=FALSE
# )
# print(xtable(data.frame(assign=-5:5, preds=results$sensitivity_trial_6_parametric$estimate) %>% 
#                rename("Change in volume (added for \\textless median, subtracted for $\\geq$ median)" = assign, "90-day mortality" = preds), 
#              align=c("llr"), digits=c(0,0,3),
#              caption="Mortality estimates for Target Trial \\#6 (IPW; parametric): Sensitivity analysis (censored for performing zero operations/interval)",
#              label="table:sensitivity-ests-trial-6-ipw-parametric"),
#       include.rownames=FALSE,
#       caption.placement="top",
#       type="latex", sanitize.text.function = function(x){x},
#       tabular.environment="longtable",
#       file=here::here("tables","sensitivity-estimates","sensitivity-ests-trial-6-ipw-parametric.tex"), floating=FALSE
# )
save(sens.results, file=here::here("objects","sens-results.Rda"))

# round(cbind(results$sensitivity_trial_4, results$sensitivity_trial_4_parametric$estimate), 3)
# round(cbind(results$sensitivity_trial_6, results$sensitivity_trial_6_parametric$estimate),3)
