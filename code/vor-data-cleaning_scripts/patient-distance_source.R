if(!require("pacman",character.only=T)){install.packages(p)}
pacman::p_load(parallel, ParallelLogger, dplyr, pbapply, lubridate, here)

# running this separately: distance functions in parallel

# Read-in distance/pancreas.time.Rda
load(file=here("data","distance","pancreas-time.rda"))
pancreas.time <- pancreas.time_1 %>% rename(bene.zip=bene.zipcode, hosp.zip=zipcode)

max.volume.close.fun <- function(x, distance.dataset, dataset, limit, n=365){
  temp.beneid <- dataset %>% arrange(date_or) %>% filter(year_or >= 2012) %>% select(bene_id) %>% .[x,]
  temp.benezip <- dataset %>% arrange(date_or) %>% filter(year_or >= 2012) %>% select(zip_cd) %>% .[x,]
  temp.benedate <- dataset %>% arrange(date_or) %>% filter(year_or >= 2012) %>% select(date_or) %>% .[x,] %>% .$date_or
  temp0 <- data.frame("bene.zip"=substr(temp.benezip, start = 1, stop = 5),
                      "hosp.zip"=dataset %>% arrange(date_or) %>% filter(year_or >= 2011) %>% filter(date_or<temp.benedate) %>% select(hosp.zip) %>% 
                        mutate(hosp.zip=substr(hosp.zip, start = 1, stop = 5)) %>% unique())
  temp1 <- merge(temp0, distance.dataset, by=c("bene.zip", "hosp.zip"))
  close.hospital.zips <- temp1 %>% filter(max <= limit) %>% select(hosp.zip) %>% .$hosp.zip
  
  close.surgeons <- dataset %>% arrange(date_or) %>% filter(year_or >= 2011) %>% 
    filter(date_or %within% interval(temp.benedate - lubridate::duration(n, "days"),
                                     temp.benedate)) %>% 
    mutate(hosp.zip=substr(hosp.zip, start = 1, stop = 5)) %>% 
    filter(hosp.zip %in% close.hospital.zips) %>% {unique(.$op_npi)}
  close.hospitals <- dataset %>% arrange(date_or) %>% filter(year_or >= 2011) %>% 
    filter(date_or %within% interval(temp.benedate - lubridate::duration(n, "days"),
                                     temp.benedate)) %>% 
    mutate(hosp.zip=substr(hosp.zip, start = 1, stop = 5)) %>% 
    filter(hosp.zip %in% close.hospital.zips) %>% {unique(.$orgnpinm)}
  if(identical(close.surgeons,character(0))==TRUE){max.volume.close.surgeon <- NA}
  if(identical(close.surgeons,character(0))==FALSE){
  max.volume.close.surgeon <- sapply(1:length(close.surgeons), function(s){
    dataset %>% filter(op_npi==close.surgeons[s]) %>% 
      filter(date_or %within% interval(temp.benedate - lubridate::duration(n, "days"),
                                       temp.benedate)) %>% summarise(vol=n()) %>% .$vol
  }
  ) %>% max(.)}
  if(identical(close.hospitals,character(0))==TRUE){max.volume.close.hospital <- NA}
  if(identical(close.hospitals,character(0))==FALSE){
  max.volume.close.hospital <- sapply(1:length(close.hospitals), function(h){
    dataset %>% filter(orgnpinm==close.hospitals[h]) %>% 
      filter(date_or %within% interval(temp.benedate - lubridate::duration(n, "days"),
                                       temp.benedate)) %>% summarise(vol=n()) %>% .$vol
  }
  ) %>% max(.)}
  print(x)
  c("bene_id"=as.character(temp.beneid), 
             "max.close.surgeon.volume"=as.numeric(max.volume.close.surgeon), 
             "max.close.hospital.volume"=as.numeric(max.volume.close.hospital))
}




# PARALLEL
cluster <- makeCluster(numberOfThreads = 10)
clusterExport(cluster, c("pancreas.base","pancreas.time"))
clusterEvalQ(cluster, {library(dplyr); library(magrittr); library(lubridate)})
print("90 minutes")
pancreas.max.volumes_90min <- parSapply(cl=cluster, 1:dim(pancreas.base %>% filter(year_or %in% 2012:2016))[1],
                               max.volume.close.fun, n=365, limit=90,
                               dataset=pancreas.base, 
                               distance.dataset=pancreas.time) %>% t() %>% as.data.frame
print("180 minutes")
pancreas.max.volumes_180min <- parSapply(cl=cluster, 1:dim(pancreas.base %>% filter(year_or %in% 2012:2016))[1],
                                        max.volume.close.fun, n=365, limit=180,
                                        dataset=pancreas.base, 
                                        distance.dataset=pancreas.time) %>% t() %>% as.data.frame
print("360 minutes")
pancreas.max.volumes_360min <- parSapply(cl=cluster, 1:dim(pancreas.base %>% filter(year_or %in% 2012:2016))[1],
                                        max.volume.close.fun, n=365, limit=360,
                                        dataset=pancreas.base, 
                                        distance.dataset=pancreas.time) %>% t() %>% as.data.frame
stopCluster(cluster)
closeAllConnections()

pancreas.max.volumes_90min$max.close.surgeon.volume_num <- as.double(levels(pancreas.max.volumes_90min$max.close.surgeon.volume)[pancreas.max.volumes_90min$max.close.surgeon.volume])
pancreas.max.volumes_90min$max.close.hospital.volume_num <- as.double(levels(pancreas.max.volumes_90min$max.close.hospital.volume)[pancreas.max.volumes_90min$max.close.hospital.volume])
pancreas.max.volumes_90min <- pancreas.max.volumes_90min %>% 
  select(bene_id, max.close.surgeon.volume_num_90 = max.close.surgeon.volume_num, max.close.hospital.volume_num_90 = max.close.hospital.volume_num)

pancreas.max.volumes_180min$max.close.surgeon.volume_num <- as.double(levels(pancreas.max.volumes_180min$max.close.surgeon.volume)[pancreas.max.volumes_180min$max.close.surgeon.volume])
pancreas.max.volumes_180min$max.close.hospital.volume_num <- as.double(levels(pancreas.max.volumes_180min$max.close.hospital.volume)[pancreas.max.volumes_180min$max.close.hospital.volume])
pancreas.max.volumes_180min <- pancreas.max.volumes_180min %>% 
  select(bene_id, max.close.surgeon.volume_num_180 = max.close.surgeon.volume_num, max.close.hospital.volume_num_180 = max.close.hospital.volume_num)

pancreas.max.volumes_360min$max.close.surgeon.volume_num <- as.double(levels(pancreas.max.volumes_360min$max.close.surgeon.volume)[pancreas.max.volumes_360min$max.close.surgeon.volume])
pancreas.max.volumes_360min$max.close.hospital.volume_num <- as.double(levels(pancreas.max.volumes_360min$max.close.hospital.volume)[pancreas.max.volumes_360min$max.close.hospital.volume])
pancreas.max.volumes_360min <- pancreas.max.volumes_360min %>% 
  select(bene_id, max.close.surgeon.volume_num_360 = max.close.surgeon.volume_num, max.close.hospital.volume_num_360 = max.close.hospital.volume_num)

save(pancreas.max.volumes_90min, file=here("data", "distance", "pancreas-max-volumes-90.rda"))
save(pancreas.max.volumes_180min, file=here("data", "distance", "pancreas-max-volumes-180.rda"))
save(pancreas.max.volumes_360min, file=here("data", "distance", "pancreas-max-volumes-360.rda"))
closeAllConnections()
showConnections()







# This is a simplification because does not account for surgeon and hospital jointly being over s and h volume/year.
# [] consider changing for each volume intervention (i.e., surgeon and hospital)
