surgeon.table1.fun <- function(dat, title, file.tex){
  if(!require("pacman",character.only=T)){install.packages("pacman")}
  pacman::p_load(tableone, xtable)
  allVars <- c("volume", "age_mean",
               "comorb_ami_mean", 
               "comorb_dementia_mean", "comorb_afib_mean", "comorb_ckd_mean", 
               "comorb_copd_mean", "comorb_chf_mean", "comorb_diabetes_mean", 
               "comorb_stroketia_mean", 
               "hospital.volume_mean", "hosp.totalbeds_mean",
               "hosp.p_medicare_mean",
               "surgeon.age", 
               "md_female")
  myVars <- allVars
  catVars <- c("md_female")
  binaryVars <- c("md_female")
  continuous <- c("volume", "age_mean",
                  "comorb_ami_mean", 
                  "comorb_dementia_mean", "comorb_afib_mean", "comorb_ckd_mean", 
                  "comorb_copd_mean", "comorb_chf_mean", "comorb_diabetes_mean", 
                  "comorb_stroketia_mean", 
                  "hospital.volume_mean", "hosp.totalbeds_mean",
                  "hosp.p_medicare_mean",
                  "surgeon.age")
  hospital.ids <- dat %>% filter(op_npi %in% {dat %>% filter(volume != 0 & volume_prev != 0 & surgeon_period==2) %>% .$op_npi}) %>% .$orgnpinm %>% unique()
  hospitals <- dat %>% filter(orgnpinm %in% hospital.ids & surgeon_period == 3) %>% # surgeon_period ==3 because hosp.volume is volume prior to 3 (i.e., in 1 and 2)
    summarise(mean.hosp.volume={paste0(round(mean(hospital.volume_mean),1), " (", round(sd(hospital.volume_mean),1), ")")},
              mean.hosp.beds={paste0(round(mean(hosp.totalbeds_mean),1), " (", round(sd(hosp.totalbeds_mean),1), ")")},
              mean.hosp.medicare={paste0(round(mean(hosp.p_medicare_mean),1), " (", round(sd(hosp.p_medicare_mean),1), ")")}) %>% 
    as.character()
  
  dat <- dat %>% group_by(op_npi) %>% 
    mutate(death.percent_lead1=ifelse(lead(volume)!=0,lead(death.percent),NA),
           death.count_lead1=ifelse(lead(volume)!=0,lead(death.count),NA),
           volume_lead1=lead(volume)) %>%
    ungroup() %>% 
    filter(op_npi %in% 
    {dat %>% filter(volume != 0 & volume_prev != 0 & surgeon_period==2) %>% .$op_npi}) # FILTER: restricted to during baseline
  
  tab1 <- CreateTableOne(vars = myVars,
                         data = dat %>% filter(surgeon_period<=2), 
                         factorVars = catVars)
  tab1.p <- print(tab1, #nonnormal=continuous, 
                  quote=FALSE, test=FALSE, noSpaces=TRUE, printToggle = FALSE, contDigits=1, catDigits=0)
  tab1.p[1] <- sum(dat$volume[dat$surgeon_period<=2]) # number of patients
  mean.surgeon.agesex <- dat %>%
    group_by(op_npi) %>% filter(row_number()==1) %>% ungroup() %>% 
    summarise(mean.age={paste0(round(mean(surgeon.age),1), " (", round(sd(surgeon.age),1), ")")},
              num.female={paste0(round(sum(md_female),1), " (", round(100*mean(md_female),1), ")")}) %>% 
    as.character()
  tab1.p[c(15:16)] <- as.matrix(mean.surgeon.agesex)
  num.hospitals <- dat %>% filter(surgeon_period <=2) %>% {length(unique(.$orgnpinm))} # among eligible surgeons during the baseline period
  total.num.hospitals <- dat %>% filter(surgeon_period <=2) %>% {length(unique(.$orgnpinm))} # during baseline
  tab1.p2 <- c("",
               length(unique(dat$op_npi)),  # number of surgeons
                   tab1.p[c(2,15:16),], # surgeon characteristics
               "",
               num.hospitals,  # number of hospitals
                   hospitals, # hospital characteristics
               "",
                   tab1.p[c(1,3:11),] # patient characteristics
                   ) %>% as.matrix()
  rownames(tab1.p2) <- c("Surgeon characteristics",
                         "    Total number of surgeons",
                         names(tab1.p[c(2,15:16),]),
                         "Hospital characteristics",
                         "    Total number of hospitals",
                         names(tab1.p[12:14,]),
                         "Case mix characteristics",
                         names(tab1.p[c(1,3:11),]))
  tab1.format <- t(t(as.matrix(tab1.p2)) %>% as.data.frame %>% rename(
    "    Total number of patients"="n",
    "    Mean count of CABG operations per 90 days"="volume (mean (SD))",
    "    Number of female surgeons"="md_female = 1 (%)",
    "    Mean surgeon age, years"="surgeon.age (mean (SD))",
    # "    Proportion of patient mortality"="death.percent (mean (SD))",
    "    Mean patient age, years"="age_mean (mean (SD))",
    "    Proportion of patients with AMI"="comorb_ami_mean (mean (SD))",
    "    Proportion of patients with atrial fibrillation"="comorb_afib_mean (mean (SD))",
    "    Proportion of patients with CKD"="comorb_ckd_mean (mean (SD))",
    "    Proportion of patients with COPD"="comorb_copd_mean (mean (SD))",
    "    Proportion of patients with CHF"="comorb_chf_mean (mean (SD))",
    "    Proportion of patients with dementia"="comorb_dementia_mean (mean (SD))",
    "    Proportion of patients with diabetes"="comorb_diabetes_mean (mean (SD))",
    "    Proportion of patients with stroke or TIA"="comorb_stroketia_mean (mean (SD))",
    "    Mean hospital annual volume"="hospital.volume_mean (mean (SD))",
    "    Hospital proportion of patients with Medicare"="hosp.p_medicare_mean (mean (SD))",
    "    Mean hospital number of beds"="hosp.totalbeds_mean (mean (SD))",
    "    Mean hospital volume (operations per 90-day interval)"="hospital.volume_mean (mean (SD))"
  ))
  
  named1 <- rownames(tab1.format)
  tags1 <- grepl("^ ", rownames(tab1.format))
  rownames(tab1.format) <- c(ifelse(tags1==FALSE, named1, paste("\\hskip .5cm", named1, sep=' ')))
  colnames(tab1.format) <- "Number (\\%) or mean (s.d.)"
  print(xtable(tab1.format, align=c("lr"),
               caption=paste0("Baseline characteristics (during the previous 6 months) of ", 
                              length(unique(dat$op_npi))," eligible surgeons who performed CABG fo U.S. Medicare beneficiaries"),
               label="table:surgeon-characteristics"),
        caption.placement="top",
        type="latex", sanitize.text.function = function(x){x}, 
        tabular.environment="longtable", 
        file=here::here("tables","table-1","surgeon-cabg-table1.tex"), floating=FALSE
  )
  tab1.format
}

