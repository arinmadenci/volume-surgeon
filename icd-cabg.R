# This file creates an overall list of all ICD-9 and ICD-10 procedure codes of interest (exports as .csv to load into SAS)
packages.icd <- c("icd", "icd.data", "lubridate", "here", "tidyverse")
for(p in packages.icd){
  if(!require(p,character.only=T))
    install.packages(p)
  library(p,character.only=T)
}
rm(p, packages.icd)


# Procedure codes ICD-9
csv.fun <- function(name.ext, def=FALSE){
  children(read_csv(paste0("D:/Projects/Arin/vor/data/icd/", name.ext), col_types=cols(.default="c"))$code, short_code=TRUE, defined=def)
}

icd9.cabg <- csv.fun("cabg-icd9.csv")

# Procedure codes ICD-10
my.icd10.children.pre <- function(my.icd10.code){
  anchored.code <- paste("^",my.icd10.code,sep="")
  icd10_pcs[[1]] %>% filter(str_detect(code,anchored.code)) %>% .$code
}
my.icd10.children <- function(name.ext){
  codes <- read_csv(paste0("D:/Projects/Arin/vor/data/icd/", name.ext), col_types=cols(.default="c"))$code
  as.vector(sapply(1:length(codes), function(x) my.icd10.children.pre(my.icd10.code = codes[x]))) %>% unlist
}

icd10.cabg <- my.icd10.children("cabg-icd10.csv")

# Diagnosis codes ICD
# Restrict by Diagnosis codes ICD-9
icd9.dx.cabg.pre <- csv.fun("diagnosis/cabg-dx-icd9.csv", def=TRUE) 
icd9.dx.cabg <- icd9.dx.cabg.pre %>% .[sapply(1:length(icd9.dx.cabg.pre), 
                                              function(x) icd9.dx.cabg.pre[x] %>% str_length) == 5] %>% {.[!endsWith(.,"2")]} # removes incomplete codes and codes ending with "2" (pre-existing)


# Restrict by Diagnosis codes ICD-10
icd10.dx.cabg <- csv.fun("diagnosis/cabg-dx-icd10.csv", def=TRUE)
