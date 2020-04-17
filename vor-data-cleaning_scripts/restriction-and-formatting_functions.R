### restrict by procedure/diagnosis and formatting functions


# -------------------
# utility restriction functions
# -------------------
    # Require lack of exclusion codes (for certain operations)
exclude.dx.fun <- function(data, abbrv){
  source("icd-cabg.R", local = TRUE)
  diagnosis.codes <- c(eval(parse(text=paste("icd9.exclude.",abbrv,sep=""))),
                       eval(parse(text=paste("icd10.exclude.",abbrv,sep=""))))
  filter_at(data,
            vars(contains("icd_dgns")),
            any_vars(!(. %in% diagnosis.codes)))
}

    # Restricts to first operation for a given BENE_ID
remove.dups_function <- function(dat){
  dat %>% arrange(admsn_dt) %>% group_by(bene_id) %>% mutate(adm_order=row_number()) %>% ungroup() %>% 
    filter(adm_order==1) %>% select(-adm_order)
}

    # Restricts by admission type
restrict.admission_function <- function(dat, exclude.nonelective=FALSE, exclude.emergent=FALSE, abbrv){
  if (exclude.nonelective==TRUE) {
    dat <- dat %>% filter(type_adm==3)
  }
  if (exclude.emergent==TRUE) {
    dat <- dat %>% filter(type_adm != 1)
  }
  return(dat)
}

# Restricts by no pre-operative chemotherapy
restrict.preop.chemo_function <- function(dat){
  dat %>% filter(sum.chemo==0)
}


    # Restricts to known discharge date
        # Deals with clerical issues about admission that begin at the end of the calendar year
        # Do not need to use censoring weights because restrict population to those with 30-day follow-up (i.e., before 12/2016)
restrict.discharge <- function(dat){
  dat %>% filter(dec16==0) %>% filter(!is.na(dschrg_dt)) 
}

    # only allows admissions in 2011 or later (for year 2011 medicare, some admissions are from late 2010 for pts discharged in 2011)
restrict.after.year_function <- function(dat, after.year){
  dat %>% filter(year(admsn_dt)>after.year)
}

# -------------------
# utility formatting/cleaning functions
# -------------------
    # Creates covariates for 30- and 90-day mortality (based on t0 of operation date)
death <- function(data){
  data %>% mutate(death30=case_when(death_dt <= (lubridate::ymd(date_or) + lubridate::duration(30, "days"))~1,
                                    TRUE~0),
                  death90=case_when(death_dt <= (lubridate::ymd(date_or) + lubridate::duration(90, "days"))~1,
                                    TRUE~0))
}

    # Creates indicator variables for relevant comorbidities
comorbidities <- function(data){
  data %>% mutate(comorb_ami=case_when(ami_dt<date_or~1,
                                       ami_dt>date_or~0,
                                       is.na(ami_dt)~0),
                  comorb_dementia=case_when(alzhdmt_dt<=date_or~1,
                                            alzhdmt_dt>date_or~0,
                                            is.na(alzhdmt_dt)~0),
                  comorb_afib=case_when(atrialf_dt<=date_or~1,
                                        atrialf_dt>date_or~0,
                                        is.na(atrialf_dt)~0),
                  comorb_ckd=case_when(chrnkdn_dt<=date_or~1,
                                       chrnkdn_dt>date_or~0,
                                       is.na(chrnkdn_dt)~0),
                  comorb_copd=case_when(copd_dt<=date_or~1,
                                        copd_dt>date_or~0,
                                        is.na(copd_dt)~0),
                  comorb_chf=case_when(chf_dt<=date_or~1,
                                       chf_dt>date_or~0,
                                       is.na(chf_dt)~0),
                  comorb_diabetes=case_when(diabetes_dt<=date_or~1,
                                            diabetes_dt>date_or~0,
                                            is.na(diabetes_dt)~0),
                  comorb_cad=case_when(ischmch_dt<=date_or~1,
                                       ischmch_dt>date_or~0,
                                       is.na(ischmch_dt)~0),
                  comorb_stroketia=case_when(strktia_dt<=date_or~1,
                                             strktia_dt>date_or~0,
                                             is.na(strktia_dt)~0))
}



    # Creates multiple membership indicator
mmem.fun <- function(dat){
  dat2 <- with(dat, table(op_npi, orgnpinm))
  dat3 <- apply(dat2, 2, function(x) ifelse(x>0,1,0))
  dat4 <- apply(dat3, 1, function(x) sum(x)) 
  dat5 <- dat4[dat4>1] %>% as.data.frame %>% rename("mmem.num"=".")
  dat5$op_npi <- rownames(dat5)
  mmemtable <- dat5 %>% mutate(mmem=ifelse(mmem.num>1,1,0)) %>% select(op_npi, mmem)
  merge(x=dat, y=mmemtable, by="op_npi", all.x=TRUE) %>% 
    mutate(mmem=case_when(mmem==1~1, TRUE~0))
}

    # Miscellaneous formatting
formatting.fun <- function(dat, years=2011:2016){
  dat <- dat %>% rename(date_or=date) %>% mutate(month_or=month(date_or), year_or=year(date_or))
  dat$age <-  as.numeric(difftime(dat$date_or, dat$dob_dt, units="days"))/365.24219
  dat$death.postopdays <- as.numeric(difftime(dat$death_dt, dat$date_or, units="days"))
  dat$surgeon.age <- as.numeric(difftime(dat$date_or, dat$md_birth_dt, units="days")/365.24219)
  dat %>%  mutate(female=ifelse(gndr_cd==2,1,0),
           race=case_when(race_cd==1~"white",
                          race_cd==2~"black",
                          race_cd==0~NA_character_,
                          race_cd %in% 3:6~"other"),
           race=fct_relevel(race, c("black","white","other")),
           afam=ifelse(race_cd==2,1,0),
           year_source=case_when(source==1~"2011",
                                 source==2~"2012",
                                 source==3~"2013",
                                 source==4~"2014",
                                 source==5~"2015a",
                                 source==6~"2015b",
                                 source==7~"2016"),
           admission.type=case_when(type_adm==1~"Emergency", 
                                    type_adm==2~"Urgent", 
                                    type_adm==3~"Elective", 
                                    type_adm==5~"Trauma", 
                                    type_adm==9~"Unknown"),
           dec16=ifelse(year_or==2016 & month_or==12,1,0),
           inpatient=ifelse(as.numeric(difftime(date_or, admsn_dt))>1,1,0),
           hospital.volume=as.numeric(hospital.volume),
           hosp.cicu=as.numeric(hosp.cicu),
           hosp.totalbeds=as.numeric(hosp.totalbeds),
           hosp.p_medicare=as.numeric(hosp.p_medicare),
           md_female=ifelse(md_sex=="F",1,0)
    ) %>% select(-c(gndr_cd, race_cd, source)) %>% 
    mutate(period = cut(as.Date(date_or, format="%Y/%m/%d"),
                        breaks = as.Date(c(lapply(years, function(x) paste0(x, c("-01-01", "-04-01", "-07-01", "-10-01"))) %>% unlist, 
                                           "2017-01-01")),
                        labels = 1:24))
}


# -------------------
# combined/compiling functions
# -------------------
    # This function combines the above restriction and formatting/cleaning functions
combo.pack <- function(dat, ...){
  source("icd-cabg.R", local = TRUE)
  dat %>% rename_all(tolower) %>% formatting.fun(dat=.) %>% mmem.fun(dat=.) %>% comorbidities(data=.) %>% death(data=.) %>%
    restrict.admission_function(dat=., ...) %>%  restrict.discharge(dat=.) %>% restrict.after.year_function(dat=., after.year = 2010) %>% 
    remove.dups_function(dat=.) %>% 
      # RESTRICTS: apply exclusion criteria
    filter(age>=65 & 
             (spec_prim_1==33 | spec_prim_1==78 | spec_prim_2==33 | spec_prim_2==78) & 
             hmo_drop==0) %>%  
    filter(!(period %in% 23:24)) %>%  # FILTER: censored from 90-day mortality outcome (b/c no 90-day follow-up for K+1 interval)
    filter(!(admission.type %in% c("Trauma", "Unknown")))
}
  
