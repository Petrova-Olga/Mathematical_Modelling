#Петрова О.Е. - Группа 122 – для региона 63 рассчитайте урожайность пшеницы в 2005 году, 
#взяв для рассчета средние суммы активных температур за предыдущие 15 лет, 
#с 19 ближайших метеостанций

library(tidyverse)
library(rnoaa)
library(lubridate)

#Данные для расчета:
ai = c(32.11, 26.31,25.64,23.20,18.73,16.30,13.83, 0.00, 0.00)# константа по табл. 1. Создаем вектор
bi = c(11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)# константа по табл. 1. Создаем вектор
di = c(0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)# отношение числа дней i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце,константа по табл. 1.
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

# Загрузка данных из сохраненного файла с данными метеостанций
station_data = read.csv('station_data.csv',header = TRUE,sep=",",dec=".")

# Создаём таблицу с именем региона и координатами его столицы (Самарская область - г. Самара, координаты: n 53.2500, e 50.2165)
Samara = data.frame(id = "SAMARA",latitude=53.250,longitude=50.2165)

# Список ближайших 19-ти метеостанций.
Samara_around=meteo_nearby_stations(lat_lon_df = Samara, station_data = station_data,
                                    limit = 19, var = c("PRCP", "TAVG"),
                                    year_min = 1990, year_max = 2005)
for (i in 1:19) {
Samara_id = Samara_around[["SAMARA"]][["id"]][1]
all_Samara_data = meteo_tidy_ghcnd(stationid = Samara_id)
all_Samara_data = bind_rows(Samara, all_Samara_data) 
}

write.csv(all_Samara_data, "all_Samara_data.csv")

all_Samara_data = read.csv('all_Samara_data.csv',header = TRUE,sep=",",dec=".")

# x %in% y  -  выберет каждую строку, где x является одним из значений y. 
# mutate() - добавляет новые столбцы, которые являются функциями существующих столбцов.
# Filter () - позволяет выбрать подмножество наблюдений(строк), основанных на их значениях(по значениям в отдельных колонках). 
all_Samara_data = all_Samara_data %>% mutate(year = date %>% year()) # выделяем колонку с годом

all_Samara_data = all_Samara_data %>% mutate(month = date %>% month())# выделяем колонку с номером месяца года
all_Samara_data = all_Samara_data %>% mutate(yday = date%>% yday())# выделяем колонку с номером месяца года
all_Samara_data = all_Samara_data %>% mutate(tavg = tavg/10) # приводим значени температуры к нормальным значениям 

all_Samara_data = filter(all_Samara_data,year > 1989)# выборка по заданным годам
all_Samara_data = filter(all_Samara_data,year < 2006)# выборка по заданным годам

all_Samara_data = all_Samara_data %>%  group_by(id,month,year) # группировка по месяцам года

all_Samara_data = filter(all_Samara_data,tavg>5) #выборка среднесуточной температуры выше 5 градусов


new_data = all_Samara_data%>% summarize(sum_temp=sum(tavg, na.rm=T)) # сумма температур выше 5 градусов по месяцам года
new_data = new_data %>% group_by(month) # группировка по месяцам
new_data = new_data %>% summarize(sum_temp = mean(sum_temp,na.rm=T)) # суммируем и выводим среднее значение суммы температур выше 5 градусов 
# по месяцам за перид 15 лет (по условиям задания)

new_data = new_data%>% mutate (a = ai, b = bi, d = di) # добавляем векторы (значения) коэффициентов к таблице

new_data = new_data%>% mutate (Fi = ((a + b * 1.0 * sum_temp) * d * Kf) / (Qj * Lj * (100-Ej))) # расчитываем формулу по месяцам

yeild = sum(new_data$Fi) # по сумме выражений по месяцам получаем урожайность 13.94 ц/га

