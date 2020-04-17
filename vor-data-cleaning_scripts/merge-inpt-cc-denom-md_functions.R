# -------------------
# utility functions
# -------------------

## Operating physician
md_function <- function(year){
  read_csv(paste("data/preprocessing/mdppas",year,".csv",sep=""),
           col_types=cols(.default = col_character())) %>% 
    select(NPI, SPEC_BROAD, SPEC_PRIM_1, SPEC_PRIM_1_NAME, SPEC_PRIM_2, SPEC_PRIM_2_NAME, SEX, BIRTH_DT) %>% 
    rename(MD_SEX=SEX, MD_BIRTH_DT=BIRTH_DT) %>% mutate_at(vars(contains("_DT")),
                                                           list(lubridate::ymd))
}

## Hospitals
aha.load <- function(year){
  read_csv(paste("data/preprocessing/aha",year,".csv",sep=""),
           col_types=cols(.default = col_character())) %>% rename_all(tolower) %>% 
    select(hosp.zip=mloczip, hosp.fips=fcounty, hosp.medsurg.beds=genbd, hosp.totalbeds=bdtot, hosp.urban=urban, hosp.state=mstate,
           #hosp.fullyear=fyr, hosp.beg_dt=dtbeg, hosp.end_dt=dtend, hosp.daysopen=dcov, 
           #hosp.numoperations=suropip, hosp.ruca=ruca_level, orgnpinum=npinum, hosp.nurse_ratio=nurse_ratio, 
           hosp.num=provider, hosp.controltype=profit2, hosp.teaching=teaching, hosp.p_medicare=p_medicare, 
           hosp.bedcat=hospsize, hosp.micu=micu, hosp.cicu=cicu) %>% 
    mutate(hosp.controltype=fct_recode(as.factor(hosp.controltype), 
                                       forprofit="1", notforprofit="2", gov.nonfed="3", gov.fed="4"),
           # hosp.ruca=fct_recode(as.factor(hosp.ruca),
           #                      urban="1", suburban="2", largeruraltown="3", isolatedrural="4"),
           hosp.teaching=fct_recode(as.factor(hosp.teaching),
                                    major="1", minor="2", nonteaching="3"),
           hosp.bedcat=fct_recode(as.factor(hosp.bedcat),
                                  small1_99="1", medium100_399="2", large400="3")
    )
}

## MBSF: chronic conditions with earliest date of occurrence
mbsf_function <- function(year){
  dat <- read_csv(paste("data/preprocessing/cc",year,".csv",sep=""),
                  col_types=cols(.default = col_character())) %>%
    rename(AMI_DT=AMIE, ALZH_DT=ALZHE, ALZHDMT_DT=ALZHDMTE, ATRIALF_DT=ATRIALFE,
           CATARACT_DT=CATARCTE, CHRNKDN_DT=CHRNKDNE, COPD_DT=COPDE, CHF_DT=CHFE,
           DIABETES_DT=DIABTESE, GLAUCOMA_DT=GLAUCMAE, HIPFRAC_DT=HIPFRACE,
           ISCHMCH_DT=ISCHMCHE, DEPRESSN_DT=DEPRSSNE, OSTEOPR_DT=OSTEOPRE,
           RA_OA_DT=RA_OA_E, STRKTIA_DT=STRKTIAE, CNCRBRS_DT=CNCRBRSE,
           CNCRCLR_DT=CNCRCLRE, CNCRPRS_DT=CNCRPRSE, CNCRLNG_DT=CNCRLNGE,
           CNCRENDM_DT=CNCENDME, ANEMIA_DT=ANEMIA_EVER, ASTHMA_DT=ASTHMA_EVER,
           HYPERL_DT=HYPERL_EVER, HYPERP_DT=HYPERP_EVER, HYPERT_DT=HYPERT_EVER, HYPOTH_DT=HYPOTH_EVER) %>% 
    mutate_at(vars(contains("_DT")),
              list(lubridate::dmy))
  dat1 <- dat %>% mutate(lastcc=pmax(AMI_DT, ALZH_DT, ALZHDMT_DT, ATRIALF_DT, CATARACT_DT, CHRNKDN_DT,
                                     COPD_DT, CHF_DT, DIABETES_DT, GLAUCOMA_DT, HIPFRAC_DT, ISCHMCH_DT,
                                     DEPRESSN_DT, OSTEOPR_DT, RA_OA_DT, STRKTIA_DT, CNCRBRS_DT,
                                     CNCRCLR_DT, CNCRPRS_DT, CNCRLNG_DT, CNCRENDM_DT, ANEMIA_DT,
                                     ASTHMA_DT, HYPERL_DT, HYPERP_DT, HYPERT_DT, HYPOTH_DT,
                                     na.rm=T)) %>% 
    arrange(BENE_ID, desc(lastcc)) %>% distinct(BENE_ID, .keep_all=TRUE) %>%  # For duplicates, takes the most recent (*distinct()* preserves the first row)
    mutate_at(.vars = vars("AMI","ALZH","ALZHDMTA","ATRIALFB", "CATARACT", "CHRNKIDN", "COPD", "CHF",
                           "DIABETES", "GLAUCOMA", "HIPFRAC", "ISCHMCHT", "DEPRESSN", "OSTEOPRS",
                           "RA_OA", "STRKETIA", "CNCRBRST", "CNCRCLRC", "CNCRPRST", "CNCRLUNG",
                           "CNCRENDM", "ANEMIA", "ASTHMA", "HYPERL", "HYPERP", "HYPERT", "HYPOTH"),
              list(~ case_when(. %in% c(1,3)~1,
                               . %in% c(0,2)~0)))
  return(dat1)
}


# -------------------
# compiling function
# -------------------
compile_function <- function(){
  if(!require("pacman",character.only=T)) {install.packages("pacman")}
  pacman::p_load(tidyverse, parallel, pbapply, ParallelLogger, lubridate, here)
  
  print("reading in ancillary files")
  md <- pblapply(setNames(2011:2016, paste0("md",2011:2016)), function(x) {md_function(year=x)})
  aha <- pblapply(setNames(2011:2016, paste0("aha",2011:2016)), function(x) aha.load(year=x))
  mbsf <- pblapply(setNames(2011:2016, paste0("mbsf",2011:2016)), function(x) {mbsf_function(year=x)})
  mortality <- read_csv("data/preprocessing/mortality.csv",
                        col_types = list("first_2010"=col_double(), "first_2011"=col_double(), "first_2012"=col_double(), "first_2013"=col_double(),
                                         "first_2014"=col_double(), "first_2015"=col_double(), "first_2016"=col_double(), "death_dt"=col_character())
  ) %>% mutate(DEATH_DT = lubridate::dmy(death_dt)) %>% select(-death_dt) %>% rename(BENE_ID=bene_id) %>% 
    mutate_at(vars(contains("first_")),
              ~ ifelse(.==0,NA,.))
  
  # Read in inpatient claims
  source("inpt-load-functions.R", local=TRUE); source("icd.R", local=TRUE)
  print("reading in inpatient claims")
  inpt.restricted <- list()
  inpt.restricted$inpt2011 <- inpt.function.2011(year=2011)
  inpt.restricted$inpt2012 <- inpt.function.2012_2014(year=2012)
  inpt.restricted$inpt2013 <- inpt.function.2012_2014(year=2013)
  inpt.restricted$inpt2014 <- inpt.function.2012_2014(year=2014)
  inpt.restricted$inpt2015 <- inpt.function.2015(year=2015)
  inpt.restricted$inpt2016 <- inpt.function.2016(year=2016)
  
  # Merge inpatient with MDPPAS 
  print("merging with MDPPAS")
  inpt.md <- list()
  inpt.md$inpt2011 <- merge(inpt.restricted$inpt2011, md$md2011, by.x="OP_NPI", by.y="NPI")
  inpt.md$inpt2012 <- merge(inpt.restricted$inpt2012, md$md2012, by.x="OP_NPI", by.y="NPI")
  inpt.md$inpt2013 <- merge(inpt.restricted$inpt2013, md$md2013, by.x="OP_NPI", by.y="NPI")
  inpt.md$inpt2014 <- merge(inpt.restricted$inpt2014, md$md2014, by.x="OP_NPI", by.y="NPI")
  inpt.md$inpt2015 <- merge(inpt.restricted$inpt2015, md$md2015, by.x="OP_NPI", by.y="NPI")
  inpt.md$inpt2016 <- merge(inpt.restricted$inpt2016, md$md2016, by.x="OP_NPI", by.y="NPI")
  rm(inpt.restricted); gc()
  
  # Merge with chronic conditions / MBSF
  print("merging with MBSF")
  inpt.md.cc <- list()
  inpt.md.cc$inpt2011 <- merge(inpt.md$inpt2011, mbsf$mbsf2011, by="BENE_ID")
  inpt.md.cc$inpt2012 <- merge(inpt.md$inpt2012, mbsf$mbsf2012, by="BENE_ID")
  inpt.md.cc$inpt2013 <- merge(inpt.md$inpt2013, mbsf$mbsf2013, by="BENE_ID")
  inpt.md.cc$inpt2014 <- merge(inpt.md$inpt2014, mbsf$mbsf2014, by="BENE_ID")
  inpt.md.cc$inpt2015 <- merge(inpt.md$inpt2015, mbsf$mbsf2015, by="BENE_ID")
  inpt.md.cc$inpt2016 <- merge(inpt.md$inpt2016, mbsf$mbsf2016, by="BENE_ID")
  rm(inpt.md); gc()
  
  # Merge with AHA 
  print("merging with AHA")
  inpt.md.cc.aha <- list()
  inpt.md.cc.aha$inpt2011 <- merge(inpt.md.cc$inpt2011, aha$aha2011, by.x="HOSP.NUM", by.y="hosp.num")
  inpt.md.cc.aha$inpt2012 <- merge(inpt.md.cc$inpt2012, aha$aha2012, by.x="HOSP.NUM", by.y="hosp.num")
  inpt.md.cc.aha$inpt2013 <- merge(inpt.md.cc$inpt2013, aha$aha2013, by.x="HOSP.NUM", by.y="hosp.num")
  inpt.md.cc.aha$inpt2014 <- merge(inpt.md.cc$inpt2014, aha$aha2014, by.x="HOSP.NUM", by.y="hosp.num")
  inpt.md.cc.aha$inpt2015 <- merge(inpt.md.cc$inpt2015, aha$aha2015, by.x="HOSP.NUM", by.y="hosp.num")
  inpt.md.cc.aha$inpt2016 <- merge(inpt.md.cc$inpt2016, aha$aha2016, by.x="HOSP.NUM", by.y="hosp.num")
  rm(inpt.md.cc); gc()
  
  # Merge with mortality 
  print("merging with mortality")
  base <- list()
  base$base2011 <- merge(inpt.md.cc.aha$inpt2011, mortality, by="BENE_ID") %>% 
    mutate(hmo_drop = case_when(year(ADMSN_DT)==2010 & first_2010 <= month(ADMSN_DT) ~ 1,
                                year(ADMSN_DT)==2011 & first_2011 <= month(ADMSN_DT) ~ 1,
                                TRUE ~ 0))
  base$base2012 <- merge(inpt.md.cc.aha$inpt2012, mortality, by="BENE_ID") %>% 
    mutate(hmo_drop = case_when(year(ADMSN_DT)==2011 & first_2011 <= month(ADMSN_DT) ~ 1,
                                year(ADMSN_DT)==2012 & first_2012 <= month(ADMSN_DT) ~ 1,
                                TRUE ~ 0))
  base$base2013 <- merge(inpt.md.cc.aha$inpt2013, mortality, by="BENE_ID") %>% 
    mutate(hmo_drop = case_when(year(ADMSN_DT)==2012 & first_2012 <= month(ADMSN_DT) ~ 1,
                                year(ADMSN_DT)==2013 & first_2013 <= month(ADMSN_DT) ~ 1,
                                TRUE ~ 0))
  base$base2014 <- merge(inpt.md.cc.aha$inpt2014, mortality, by="BENE_ID") %>% 
    mutate(hmo_drop = case_when(year(ADMSN_DT)==2013 & first_2013 <= month(ADMSN_DT) ~ 1,
                                year(ADMSN_DT)==2014 & first_2014 <= month(ADMSN_DT) ~ 1,
                                TRUE ~ 0))
  base$base2015 <- merge(inpt.md.cc.aha$inpt2015, mortality, by="BENE_ID") %>% 
    mutate(hmo_drop = case_when(year(ADMSN_DT)==2014 & first_2014 <= month(ADMSN_DT) ~ 1,
                                year(ADMSN_DT)==2015 & first_2015 <= month(ADMSN_DT) ~ 1,
                                TRUE ~ 0))
  base$base2016 <- merge(inpt.md.cc.aha$inpt2016, mortality, by="BENE_ID") %>% 
    mutate(hmo_drop = case_when(year(ADMSN_DT)==2015 & first_2015 <= month(ADMSN_DT) ~ 1,
                                year(ADMSN_DT)==2016 & first_2016 <= month(ADMSN_DT) ~ 1,
                                TRUE ~ 0))
  rm(mbsf_function, md_function, aha.load, envir=.GlobalEnv); gc() # functions

  return(base)
}