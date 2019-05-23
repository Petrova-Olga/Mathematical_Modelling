setwd("F:/R")
getwd()
# ��������� ����������:
library(tidyverse)
library(rnoaa)
library(lubridate)

#�������� ������� � ������� ��� �������: а:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
df = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 # ����������� ������������� ���
Qj = 1600 # ������������ ������ ��������
Lj = 2.2 # ����� ������ �������� � �������� ���������
Ej = 25 # ����������� ��������� �������� 

#station_data = ghcnd_stations() 
# write.csv(station_data, "station_data.csv") 
station_data = read.csv("station_data.csv")
#����� ��������� ������� ���� �������, �������� ������ ������� ��������� � ������� ������ �������,������ ������� � ������ ������� � ������������ ��� �������
samara = data.frame(id = "SAMARA", latitude = 53.250, longitude = 50.2165)
samara
#������ �������, ��������������� ���������
samara_around = meteo_nearby_stations(lat_lon_df = samara, station_data = station_data,
                                             limit = 19, var = c("PRCP", "TAVG"),
                                             year_min = 1990, year_max = 2005)
samara_around
# ��������� ������ �������
samara_id = samara_around[["SAMARA"]][["id"]]

summary(samara_id)
str(samara_id)

# ��������� ��� 2 ������������
# samara_id [3]
# 
# dataAA = meteo_tidy_ghcnd(stationid = samara_id [3],
# var="TAVG",
# date_min="1990-01-01",
# date_max="2005-12-31")
# dataAA

#�������� ������� 
all_data = tibble()
for (i in 1:length(samara_around$SAMARA[,1]))
  
{
  
  print(i)
  print(samara_id)
  # �������� ������ ��� �������:
  
  data = meteo_tidy_ghcnd(stationid = samara_id[i],
                          var="TAVG",
                          date_min="1990-01-01",
                          date_max="2005-12-31")
  print(data)
  #��������� ������ � �������
  all_data = bind_rows(
    all_data, data %>%
      #������� ������� ��� ����������� �� ���� � ������
      mutate(year = year(date), month = month(date)) %>%
      group_by(month, year) %>%
      mutate(tavg=tavg/10)%>%
      filter(tavg>5)%>%
      #������ ��������� ������� �������� ����������� �� ������ �� ������ ��� ��� �������
      summarise (sum = sum(tavg))
  )
}

write.csv(all_data, "all_samara_data.csv")

# ��������� � ������� ���������� � ������� clean_data.
clean_data = all_data %>%
  
  # ������� ������� month ��� ����������� ������:
  group_by(month) %>%
  
  # ������ �������� d � c���� �������� ��������� ��� ������ �������:
  summarise(s = mean(sum, na.rm = TRUE)) %>%
  
  #������� ������ �� ������� � ����������� d
  # ������� ������� ��� �������:
  mutate(a = af[3:11], b = bf[3:11], d = df[3:11]) %>%
  
  # ���������� ����������� ��� ������� ������:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )
#�������� �������, ����������� ������� � ������ � 1990 �� 2005 ��� � ��������� ������� ��������� (�/��):
Yield = sum(clean_data$fert)
Yield
