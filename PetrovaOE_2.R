library(tidyverse)
library(stringr)
eddy=read_csv("eddypro.csv", skip=1, na=c(" ","NA","-9999","-9999.0"), comment=c("[")); 
eddy=eddy[-1,]
eddy=select(eddy,-(roll))
eddy=eddy %>% mutate_if(is.character, factor)

names(eddy) = names(eddy) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]","_quest_") %>%
  str_replace_all("[*]","_star_") %>%
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_slash_") %>%
  str_replace_all("[%]","_pecent_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_")
glimpse(eddy)

eddy_num=filter(eddy, daytime==FALSE)[,sapply(eddy, is.numeric)]
eddy_non_num=eddy_filter[,!sapply(eddy, is.numeric)]

cor_td=cor(drop_na(eddy_num)) %>% as.data.frame %>% select(co2_flux)
cor_td=cbind(cor_td,adjusted=sqrt(cor_td$co2_flux^2))
vars=row.names(cor_td)[cor_td$co2_flux^2>.1] %>% na.exclude()
correlation_formula=as.formula(paste("co2_flux~",paste(vars,collapse="+"),sep=" "))

model1=lm(correlation_formula,data=as.data.frame(eddy_num))

