dynamic_assignment_function <- function(x, dat){
  median.vol <- quantile(dat$baseline_volume, probs=0.5) %>% as.numeric
  dat <- dat %>% mutate(median.vol=median.vol, x=x)
  group.new <- dat %>% filter(baseline_volume>=median.vol) %>% .$op_npi.new
  d <- dat %>% filter(baseline_volume < median.vol) %>% mutate(sum.x = baseline_volume + x) %>% select(op_npi.new, baseline_volume, sum.x) %>% 
    mutate(num.available = case_when(sum.x >= 0 ~ x*-1,
                                     sum.x < 0 ~ baseline_volume)) %>% select(op_npi.new, num.available) %>% 
    group_by(op_npi.new) %>% summarise(group.numbers = first(num.available)) %>% ungroup()
  set.seed(1)
  d2 <- data.frame(group.old=rep(d$op_npi.new,2), # (rep(.,2) to allow for ties in taking median)
                   num.available=rep( sample(d$group.numbers),2)) # shuffle 
  d3 <- d2 %>% filter(row_number() <= length(unique(group.new))) # (warning not relevant)
  d4 <- cbind(d3, "group.new" = unique(group.new))
  dataset <- left_join(dat, d4, by=c("op_npi.new" = "group.new"))
  return(dataset)
}
