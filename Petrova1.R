library(tidyverse)
library(rnoaa)
library(lubridate)

station_data = ghcnd_stations()
write.csv(station_data, file = "station_data.csv")
station_data=read.csv('station_data.csv',header = TRUE,sep=",",dec=".")
samara=data.frame(id="SAMARA",latitude=53.20007,longitude=50.15)
Samara_around=meteo_nearby_stations(lat_lon_df = Samara, station_data = station_data,
                                      limit=19, var = c("PRCP","TAVG"),
                                      year_min = 1990, year_max=2004)
All_samara_data=list()
for(i in 1:19){
  Samara_id=Samara_around[["SAMARA"]][["id"]][i]
  All_samara_data[i]=list(meteo_tidy_ghcnd(stationid = Samara_id,
                                             var=c("PRCP","TAVG"),
                                             date_min = "1990-04-01",
                                             date_max = "2004-10-31"))}

act_sum5=vector()
d_coeff=vector()

for(year in 1990:2004){
  for(month in 4:10){
    month_sort=vector()
    for(station in 1:19){
      month_sort=(filter(All_samara_data[[station]],
                         date>=ymd(paste(toString(year),
                                         toString(month),"01",sep="-"))
                         &date<ymd(paste(toString(year),toString(month+1),"01",sep="-"))))
      c[station]=sum(month_sort[month_sort>50],na.rm = TRUE)/10
    }
    act_sum5=c(act_sum5,mean(a))
    d_coeff=c(d_coeff,length(month_sort[month_sort>70]/(length(month_sort[station]))))
  }
}
