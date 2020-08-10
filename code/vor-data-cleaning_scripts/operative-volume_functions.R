# operative volume funcions
# -------------------
# utility functions
# -------------------
    # Creates new covariates containing the dates coorresponding to ICD procedure codes
find.pr.date <- function(data, abbrv){
  source("icd-cabg.R", local=TRUE) # ICD code vectors
  procedure.codes <- c(eval(parse(text=paste("icd9.",abbrv,sep=""))), 
                       eval(parse(text=paste("icd10.",abbrv,sep=""))))
  data %>% mutate(date1=case_when(ICD_PRCDR10_CD1 %in% procedure.codes | ICD_PRCDR_CD1 %in% procedure.codes ~ PRCDR_DT1),
                  date2=case_when(ICD_PRCDR10_CD2 %in% procedure.codes | ICD_PRCDR_CD2 %in% procedure.codes ~ PRCDR_DT2),
                  date3=case_when(ICD_PRCDR10_CD3 %in% procedure.codes | ICD_PRCDR_CD3 %in% procedure.codes ~ PRCDR_DT3),
                  date4=case_when(ICD_PRCDR10_CD4 %in% procedure.codes | ICD_PRCDR_CD4 %in% procedure.codes ~ PRCDR_DT4),
                  date5=case_when(ICD_PRCDR10_CD5 %in% procedure.codes | ICD_PRCDR_CD5 %in% procedure.codes ~ PRCDR_DT5),
                  date6=case_when(ICD_PRCDR10_CD6 %in% procedure.codes | ICD_PRCDR_CD6 %in% procedure.codes ~ PRCDR_DT6),
                  date7=case_when(ICD_PRCDR10_CD7 %in% procedure.codes | ICD_PRCDR_CD7 %in% procedure.codes ~ PRCDR_DT7),
                  date8=case_when(ICD_PRCDR10_CD8 %in% procedure.codes | ICD_PRCDR_CD8 %in% procedure.codes ~ PRCDR_DT8),
                  date9=case_when(ICD_PRCDR10_CD9 %in% procedure.codes | ICD_PRCDR_CD9 %in% procedure.codes ~ PRCDR_DT9),
                  date10=case_when(ICD_PRCDR10_CD10 %in% procedure.codes | ICD_PRCDR_CD10 %in% procedure.codes ~ PRCDR_DT10),
                  date11=case_when(ICD_PRCDR10_CD11 %in% procedure.codes | ICD_PRCDR_CD11 %in% procedure.codes ~ PRCDR_DT11),
                  date12=case_when(ICD_PRCDR10_CD12 %in% procedure.codes | ICD_PRCDR_CD12 %in% procedure.codes ~ PRCDR_DT12),
                  date13=case_when(ICD_PRCDR10_CD13 %in% procedure.codes | ICD_PRCDR_CD13 %in% procedure.codes ~ PRCDR_DT13),
                  date14=case_when(ICD_PRCDR10_CD14 %in% procedure.codes | ICD_PRCDR_CD14 %in% procedure.codes ~ PRCDR_DT14),
                  date15=case_when(ICD_PRCDR10_CD15 %in% procedure.codes | ICD_PRCDR_CD15 %in% procedure.codes ~ PRCDR_DT15),
                  date16=case_when(ICD_PRCDR10_CD16 %in% procedure.codes | ICD_PRCDR_CD16 %in% procedure.codes ~ PRCDR_DT16),
                  date17=case_when(ICD_PRCDR10_CD17 %in% procedure.codes | ICD_PRCDR_CD17 %in% procedure.codes ~ PRCDR_DT17),
                  date18=case_when(ICD_PRCDR10_CD18 %in% procedure.codes | ICD_PRCDR_CD18 %in% procedure.codes ~ PRCDR_DT18),
                  date19=case_when(ICD_PRCDR10_CD19 %in% procedure.codes | ICD_PRCDR_CD19 %in% procedure.codes ~ PRCDR_DT19),
                  date20=case_when(ICD_PRCDR10_CD20 %in% procedure.codes | ICD_PRCDR_CD20 %in% procedure.codes ~ PRCDR_DT20),
                  date21=case_when(ICD_PRCDR10_CD21 %in% procedure.codes | ICD_PRCDR_CD21 %in% procedure.codes ~ PRCDR_DT21),
                  date22=case_when(ICD_PRCDR10_CD22 %in% procedure.codes | ICD_PRCDR_CD22 %in% procedure.codes ~ PRCDR_DT22),
                  date23=case_when(ICD_PRCDR10_CD23 %in% procedure.codes | ICD_PRCDR_CD23 %in% procedure.codes ~ PRCDR_DT23),
                  date24=case_when(ICD_PRCDR10_CD24 %in% procedure.codes | ICD_PRCDR_CD24 %in% procedure.codes ~ PRCDR_DT24),
                  date25=case_when(ICD_PRCDR10_CD25 %in% procedure.codes | ICD_PRCDR_CD25 %in% procedure.codes ~ PRCDR_DT25))
}
    # Restrict to relevant ICD procedure and diagnosis codes
procedure.fun <- function(abbrv,year){
  source("icd-cabg.R", local=TRUE) # ICD code vectors
  procedure.codes <- c(eval(parse(text=paste("icd9.",abbrv,sep=""))), # procedure codes only
                       eval(parse(text=paste("icd10.",abbrv,sep=""))))
  data.year <- eval(parse(text=paste("base.",year,sep=""))) %>% 
    filter_at(vars(contains("_PRCDR")),
              any_vars(. %in% procedure.codes))
}
    # Surgeon and hospital volume functions
# surgeon.volume.fun <- function(x, n, data){
#   data %>% filter(OP_NPI==data[x,"OP_NPI"]) %>%
#     filter(date %within% interval(data[x,"date"] - lubridate::duration(n, "days"),
#                                   data[x,"date"])) %>% summarise(vol=n()-1)
# }
hospital.volume.fun <- function(x, n, data){
  data %>% filter(ORGNPINM==data[x,"ORGNPINM"]) %>%
    filter(date %within% interval(data[x,"date"] - lubridate::duration(n, "days"),
                                  data[x,"date"])) %>% summarise(vol=n()-1)
}




# -------------------
# combined/compiling functions
# -------------------
    # This function restricts to relevant ICD codes ("procedure.fun") and measures surgeon ("surgeon.volume.fun") & hospital ("hospital.volume.fun") volume
procedure.pack <- function(procedure, cores=5, ...){
  if(!require("pacman",character.only=T)){install.packages("pacman")}
  pacman::p_load(parallel, ParallelLogger, dplyr, pbapply, lubridate, here)
  source("icd-cabg.R", local=TRUE) # ICD code vectors
  print("2011")
  dat.2011 <- procedure.fun(abbrv=procedure, year="2011") %>% find.pr.date(data=., abbrv=procedure)
  dat.2011$date <- suppressWarnings(pbapply(dat.2011 %>% select(sapply(1:25, function(x) paste("date",x,sep=""))), 1, min, na.rm=T)) %>% lubridate::ymd(.)
#check this w/o suppress Warnings !! []
  print("2012")
  dat.2012 <- procedure.fun(abbrv=procedure, year="2012") %>% find.pr.date(data=., abbrv=procedure)
  dat.2012$date <- suppressWarnings(pbapply(dat.2012 %>% select(sapply(1:25, function(x) paste("date",x,sep=""))), 1, min, na.rm=T)) %>% lubridate::ymd(.)
# if procedure date for procedure of interest is missing, uses date of hospital admission:
  # [] check why this is only for 2012
  dat.2012[is.na(dat.2012$date),]$date <- dat.2012[is.na(dat.2012$date),]$ADMSN_DT
  print("2013")
  dat.2013 <- procedure.fun(abbrv=procedure, year="2013") %>% find.pr.date(data=., abbrv=procedure)
  dat.2013$date <- apply(dat.2013 %>% select(sapply(1:25, function(x) paste("date",x,sep=""))), 1, min, na.rm=T) %>% lubridate::ymd(.)
  print("2014")
  dat.2014 <- procedure.fun(abbrv=procedure, year="2014") %>% find.pr.date(data=., abbrv=procedure) 
  dat.2014$date <- apply(dat.2014 %>% select(sapply(1:25, function(x) paste("date",x,sep=""))), 1, min, na.rm=T) %>% lubridate::ymd(.)
  print("2015")
  dat.2015 <- procedure.fun(abbrv=procedure, year="2015") %>% find.pr.date(data=., abbrv=procedure) 
  dat.2015$date <- apply(dat.2015 %>% select(sapply(1:25, function(x) paste("date",x,sep=""))), 1, min, na.rm=T) %>% lubridate::ymd(.)
  print("2016")
  dat.2016 <- procedure.fun(abbrv=procedure, year="2016") %>% find.pr.date(data=., abbrv=procedure) 
  dat.2016$date <- apply(dat.2016 %>% select(sapply(1:25, function(x) paste("date",x,sep=""))), 1, min, na.rm=T) %>% lubridate::ymd(.)
  dat.base <- bind_rows(dat.2011, dat.2012, dat.2013, dat.2014, dat.2015, dat.2016, .id = "source")
  rm(dat.2011, dat.2012, dat.2013, dat.2014, dat.2015, dat.2016); gc()
  
    cluster <- makeCluster(numberOfThreads = cores)
    clusterEvalQ(cluster, {library(dplyr); library(magrittr); library(lubridate)})
    t1 <- Sys.time()
    # print("counting surgeon volume")
    # dat.base$surgeon.volume <- parSapply(cl=cluster, 1:dim(dat.base)[1], surgeon.volume.fun, n=365, data=dat.base) %>% unlist
    print("counting hospital volume")
    dat.base$hospital.volume <- parSapply(cl=cluster, 1:dim(dat.base)[1], hospital.volume.fun, n=90, data=dat.base) %>% unlist
    t2 <- Sys.time()
    print(t2-t1)
    on.exit(stopCluster(cluster))
  return(dat.base)
}
