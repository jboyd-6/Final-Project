#Jamie Boyd
#Final Data Project

#Data Citation 

# "All Consumption Estimates", 2025 Version, EIA Data, 2023, Accessed: April 2026, https://www.eia.gov/state/seds/seds-data-complete.php?sid=US#Consumption
# "

#Name of data set, version (if available), name/entity of data, year, date access (April 2026)

#Finding right dataset

install.packages(c("ggplot2", "dplyr", "olsrr", "PerformanceAnalytics", "lubridate", "forecast"))

library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(lubridate)
library(forecast)
library(tidyr)

#Load in Data 
Consumption = read.csv("/cloud/project/finalproject/use_NY.csv")
Names = read_xlsx("/cloud/project/finalproject/Codes_and_Descriptions.xlsx", sheet = "MSN descriptions", skip = 10) #Used internet to help here and get directly to Rows for descpritions

#Cleaning Data
names(Consumption)

Consumption = Consumption %>%
  select(-Data_Status, -State) #Select just the years and not data or state

names(Consumption) = sub("X", "", names(Consumption)) #Take X out from years
names(Consumption)

consumption_long = pivot_longer(Consumption,cols = -MSN, names_to = "year", values_to = "consumption") 
consumption_long

# Matching the MSN to Energy Source in Consumption Long
consumption_long$source = Names$Description [match(consumption_long$MSN, Names$MSN)]

# Graphing Total Energy Sources
Energy_Total = consumption_long$MSN %in% c("CLTCB", "NGTCB", "PATCB", "RETCB", "NUETB", "TETCB")

#Total Energy Sources ----
ggplot(consumption_long[
  consumption_long$MSN %in% c("CLTCB", "NGTCB", "PATCB", "RETCB", "NUETB", "TETCB"),],
  aes(x = as.numeric(year), y = as.numeric(consumption), color = MSN)) +
  geom_line() +
  labs(title="NY Energy Consumption Source Over Time", y ="Total Consumption", x="Year", color = "Energy Source")+
  scale_color_manual(values = c("brown", "black", "green", "blue", "orange", "purple"),labels = c("All Petroleum", "Coal Total", "Natural Gas Total", "Nuclear Total", "Renewables Total", "Total Energy"))

#Renewable Energy Sources ----
ggplot(consumption_long[
  consumption_long$MSN %in% c("BMTCB", "GETCB", "HYTCB", "SOTCB","WYTCB"), ],
  aes(x = as.numeric(year), y = as.numeric(consumption), color = MSN)) +
  geom_line() +
  labs(title="NY Renewable Energy Counsumption Source Over Time", y ="Total Consumption (BBTU)", x="Year", color = "Energy Source")+
  scale_color_manual(values = c("blue3", "pink", "orange", "red", "purple"),
    labels = c("Biomass", "Geothermal", "Hydro", "Solar", "Wind"))

#Forecasting Total Energy Sources ----
Energy_Total = consumption_long[consumption_long$MSN %in% c("CLTCB", "NGTCB", "PATCB", "RETCB", "NUETB", "TETCB"),]

Energy_Total$year = as.numeric(Energy_Total$year)
Energy_Total$consumption = as.numeric(Energy_Total$consumption)

#Coal Forecast ----
#Doing both Arima (covered in class) and ETS (found in online research for timeseries anaylsis)
Coal = consumption_long[consumption_long$MSN %in% c("CLTCB"),]

Coal$year = as.numeric(Coal$year)
Coal$consumption = as.numeric(Coal$consumption)

Coal_ts = ts(Coal$consumption,start = min(Coal$year),frequency = 1)

acf(na.omit(Coal_ts),lag.max = 20)
pacf.plot = pacf(na.omit(Coal_ts))
Coal_y = na.omit(Coal_ts)

model1_Coal = arima(Coal_y,order = c(1,0,0))
model1_Coal

model4_Coal = arima(Coal_y, order = c(10,0,0))
model4_Coal

ETS_Coal = ets(Coal_ts)
ETS_Coal

#calculate fit
AR_fit1_Coal = Coal_y - residuals(model1_Coal)
AR_fit4_Coal = Coal_y - residuals(model4_Coal)
ETS_fit_Coal = Coal_y - residuals(ETS_Coal)
#plot data
plot(Coal_y)

#plot fit
points(AR_fit1_Coal, type = "l", col = "tomato3", lty = 2, lwd = 2)
points(AR_fit4_Coal, type = "l", col = "darkgoldenrod4", lty = 2, lwd = 2)
points(ETS_fit_Coal, type = "l", col = "blue", lty = 2, lwd = 2)

legend("topleft", c("data", "AR1", "AR4", "ETS"),
       lty = c(1,2,2), lwd = c(1,2,2),
       col = c("black", "tomato3", "darkgoldenrod4", "blue"),
       bty = "n")

newCoal = forecast(AR_fit4_Coal, h = 10)
newCoal

newCoalETS = forecast(ETS_fit_Coal, h = 10)
newCoalETS

#make dataframe for plotting
newCoalF = data.frame(newCoal)
newCoalETSF = data.frame(newCoalETS)

#set up future years
newCoalF$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newCoalETSF$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)

#make a plot with data and predictions including a prediction interval for ARIMA
ggplot() +
  geom_line(data = Coal, aes(x = year, y = consumption)) +
  xlim(min(Coal$year), newCoalF$yearF[10]) +
  geom_line(data = newCoalF, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newCoalF, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Consumption", title = "NY Coal Consumption 10 Year Prediction (ARIMA)")

#make a plot with data and predictions including a prediction interval for ETS
ggplot() +
  geom_line(data = Coal, aes(x = year, y = consumption)) +
  xlim(min(Coal$year), newCoalETSF$yearF[10]) +
  geom_line(data = newCoalETSF, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newCoalETSF, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Consumption", title = "NY Coal Consumption 10 Year Prediction (ETS)")

#Natural Gas Forecast ----

NG = consumption_long[consumption_long$MSN %in% c("NGTCB"),]

NG$year = as.numeric(NG$year)
NG$consumption = as.numeric(NG$consumption)

NG_ts = ts(NG$consumption,start = min(NG$year),frequency = 1)

acf(na.omit(NG_ts),lag.max = 20)
pacf.plot = pacf(na.omit(NG_ts))

NG_y = na.omit(NG_ts)

model1_NG = arima(NG_y,order = c(1,0,0))
model1_NG

model4_NG = arima(NG_y, order = c(10,0,0))
model4_NG

ETS_NG = ets(NG_y)
ETS_NG

#calculate fit
AR_fit1_NG = NG_y - residuals(model1_NG)
AR_fit4_NG = NG_y - residuals(model4_NG)
ETS_fit_NG = NG_y - residuals(ETS_NG)

#plot data
plot(NG_y)

#plot fit
points(AR_fit1_NG, type = "l", col = "tomato3", lty = 2, lwd = 2)
points(AR_fit4_NG, type = "l", col = "darkgoldenrod4", lty = 2, lwd = 2)
points(ETS_fit_NG, type = "l", col = "blue", lty = 2, lwd = 2)


legend("topleft", c("data", "AR1", "AR4", "ETS"),
       lty = c(1,2,2), lwd = c(1,2,2),
       col = c("black", "tomato3", "darkgoldenrod4", "blue"),
       bty = "n")

newNG = forecast(model4_NG, h = 10)
newNG

newNGETS = forecast(ETS_fit_NG, h = 10)
newNGETS

#make dataframe for plotting
newNGf = data.frame(newNG)
newNGETSf = data.frame(newNGETS)

#set up future years
newNGf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newNGETSf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)

#make a plot with data and predictions including a prediction interval for ARIMA
ggplot() +
  geom_line(data = NG, aes(x = year, y = consumption)) +
  xlim(min(NG$year), newNGf$yearF[10]) +
  geom_line(data = newNGf, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newNGf, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Consumption", title = "NY Natural Gas Consumption 10 Year Prediction (ARIMA)")

#make a plot with data and predictions including a prediction interval for ETS
ggplot() +
  geom_line(data = NG, aes(x = year, y = consumption)) +
  xlim(min(NG$year), newNGETSf$yearF[10]) +
  geom_line(data = newNGETSf, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newNGETSf, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Consumption", title = "NY Natural Gas Consumption 10 Year Prediction (ETS)")

#Renewable Forecast ----

RE = consumption_long[consumption_long$MSN %in% c("RETCB"),]

RE$year = as.numeric(RE$year)
RE$consumption = as.numeric(RE$consumption)

RE_ts = ts(RE$consumption, start = min(RE$year), frequency = 1)

acf(na.omit(RE_ts),lag.max = 20)
pacf.plot = pacf(na.omit(RE_ts))

RE_y = na.omit(RE_ts)

model1_RE = arima(RE_y, order = c(1,0,0))
model1_RE

model4_RE = arima(RE_y, order = c(10,0,0))
model4_RE

RE_ETS = ets(RE_y)
RE_ETS

#calculate fit
AR_fit1_RE = RE_y - residuals(model1_RE)
AR_fit4_RE = RE_y - residuals(model4_RE)
ETS_RE = RE_y - residuals(RE_ETS)

#plot data
plot(RE_y)

#plot fit
points(AR_fit1_RE, type = "l", col = "tomato3", lty = 2, lwd = 2)
points(AR_fit4_RE, type = "l", col = "darkgoldenrod4", lty = 2, lwd = 2)
points(ETS_RE, type = "l", col = "blue", lty = 2, lwd = 2)

legend("topleft", c("data", "AR1", "AR4", "ETS"),
       lty = c(1,2,2), lwd = c(1,2,2),
       col = c("black", "tomato3", "darkgoldenrod4", "blue"),
       bty = "n")

newRE = forecast(model4_RE, h = 10)
newRE

newRE_ETS = forecast(ETS_RE, h = 10)
newRE_ETS

#make dataframe for plotting
newREf = data.frame(newRE)
newRE_ETSf = data.frame(newRE_ETS)

#set up future years
newREf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newRE_ETSf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)

#make a plot with data and predictions including a prediction interval ARIMA
ggplot() +
  geom_line(data = RE, aes(x = year, y = consumption)) +
  xlim(min(RE$year), newREf$yearF[10]) +
  geom_line(data = newREf, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newREf, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Consumption", title = "NY Renewable Energy Consumption 10 Year Prediction (ARIMA)")

#make a plot with data and predictions including a prediction interval for ETS
ggplot() +
  geom_line(data = RE, aes(x = year, y = consumption)) +
  xlim(min(RE$year), newRE_ETSf$yearF[10]) +
  geom_line(data = newRE_ETSf, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newRE_ETSf, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Consumption", title = "NY Renewable Energy Consumption 10 Year Prediction (ETS)")

#Petrolium Forcast ----

PET = consumption_long[consumption_long$MSN %in% c("PATCB"),]

PET$year = as.numeric(PET$year)
PET$consumption = as.numeric(PET$consumption)

PET_ts = ts(PET$consumption, start = min(PET$year),frequency = 1)

pacf(na.omit(PET_ts),
    lag.max = 20)

pacf.plot = pacf(na.omit(PET_ts))

PET_y = na.omit(PET_ts)

model1_PET = arima(PET_y, order = c(1,0,0))
model1_PET

model4_PET = arima(PET_y, order = c(10,0,0))
model4_PET

PET_ETS = ets(PET_y)
PET_ETS

#calculate fit
AR_fit1_PET = PET_y - residuals(model1_PET)
AR_fit4_PET = PET_y - residuals(model4_PET)
ETS_fit_PET = PET_y - residuals(PET_ETS)

#plot data
plot(PET_y)

#plot fit
points(AR_fit1_PET, type = "l", col = "tomato3", lty = 2, lwd = 2)
points(AR_fit4_PET, type = "l", col = "darkgoldenrod4", lty = 2, lwd = 2)
points(ETS_fit_PET, type = "l", col = "blue", lty = 2, lwd = 2)

legend("topleft", c("data", "AR1", "AR4", "ETS"), 
       lty = c(1,2,2), lwd = c(1,2,2),
       col = c("black", "tomato3", "darkgoldenrod4", "blue"), 
       bty = "n")

newPET = forecast(model4_PET, h = 10)
newPET

newPET_ETS = forecast(ETS_fit_PET, h = 10)
newPET_ETS

#make dataframe for plotting
newPETf = data.frame(newPET)
newPET_ETSf = data.frame(newPET_ETS)

#set up future years
newPETf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newPET_ETSf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)

#make a plot with data and predictions including a prediction interval for ETS
ggplot() +
  geom_line(data = PET, aes(x = year, y = consumption)) + 
  xlim(min(PET$year), newPETf$yearF[10]) +
  geom_line(data = newPETf, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newPETf, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Consumption", title = "NY Petroleum Consumption 10 Year Prediction (ARIMA)")

#make a plot with data and predictions including a prediction interval for ETS
ggplot() +
  geom_line(data = PET, aes(x = year, y = consumption)) + 
  xlim(min(PET$year), newPET_ETSf$yearF[10]) +
  geom_line(data = newPET_ETSf, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newPET_ETSf, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Consumption", title = "NY Petroleum Consumption 10 Year Prediction (ETS)")

#Nuclear Prediction ----

NUC = consumption_long[consumption_long$MSN %in% c("NUETB"),]

NUC$year = as.numeric(NUC$year)
NUC$consumption = as.numeric(NUC$consumption)

NUC_ts = ts(NUC$consumption, start = min(NUC$year), frequency = 1)

acf(na.omit(NUC_ts), lag.max = 20)
pacf.plot = pacf(na.omit(NUC_ts))

NUC_y = na.omit(NUC_ts)

model1_NUC = arima(NUC_y, order = c(1,0,0))
model1_NUC

model4_NUC = arima(NUC_y, order = c(10,0,0))
model4_NUC

NUC_ETS = ets(NUC_y)
NUC_ETS

#calculate fit
AR_fit1_NUC = NUC_y - residuals(model1_NUC)
AR_fit4_NUC = NUC_y - residuals(model4_NUC)
ETS_fit_NUC = NUC_y - residuals(NUC_ETS)

#plot data
plot(NUC_y)

#plot fit
points(AR_fit1_NUC, type = "l", col = "tomato3", lty = 2, lwd = 2)
points(AR_fit4_NUC, type = "l", col = "darkgoldenrod4", lty = 2, lwd = 2)
points(ETS_fit_NUC, type = "l", col = "blue", lty = 2, lwd = 2)

legend("topleft", c("data", "AR1", "AR4", "ETS"),
       lty = c(1,2,2), lwd = c(1,2,2),
       col = c("black", "tomato3", "darkgoldenrod4", "blue"),
       bty = "n")

newNUC = forecast(model4_NUC, h = 10)
newNUC

newNUC_ETS = forecast(ETS_fit_NUC, h = 10)
newNUC_ETS

#make dataframe for plotting
newNUCf = data.frame(newNUC)
newNUC_ETSf = data.frame(newNUC_ETS)

#set up future years
newNUCf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newNUC_ETSf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)

#make a plot with data and predictions including a prediction interval ARIMA
ggplot() +
  geom_line(data = NUC, aes(x = year, y = consumption)) +
  xlim(min(NUC$year), newNUCf$yearF[10]) +
  geom_line(data = newNUCf, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newNUCf, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Consumption", title = "NY Nuclear Energy Consumption 10 Year Prediction")


#make a plot with data and predictions including a prediction interval ETS
ggplot() +
  geom_line(data = NUC, aes(x = year, y = consumption)) +
  xlim(min(NUC$year), newNUC_ETSf$yearF[10]) +
  geom_line(data = newNUC_ETSf, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newNUC_ETSf, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Consumption", title = "NY Nuclear Energy Consumption 10 Year Prediction")

#Total Energy Prediction
total = consumption_long[ consumption_long$MSN %in% c("TETCB"),]

total$year = as.numeric(total$year)
total$consumption = as.numeric(total$consumption)

total_ts = ts(total$consumption, start = min(total$year), frequency = 1)

acf(na.omit(total_ts), lag.max = 20)
pacf.plot = pacf(na.omit(total_ts))

total_y = na.omit(total_ts)

model1_total = arima(total_y, order = c(1,0,0))
model1_total

model4_total = arima(total_y, order = c(10,0,0))
model4_total

ETS_total = ets(total_y)
ETS_total

#calculate fit
AR_fit1_total = total_y - residuals(model1_total)
AR_fit4_total = total_y - residuals(model4_total)
ETS_fit_total = total_y - residuals(ETS_total)

#plot data
plot(total_y)

#plot fit
points(AR_fit1_total, type = "l", col = "tomato3", lty = 2, lwd = 2)
points(AR_fit4_total, type = "l", col = "darkgoldenrod4", lty = 2, lwd = 2)
points(ETS_fit_total, type = "l", col = "blue", lty = 2, lwd = 2)

legend("topleft", c("data", "AR1", "AR4", "ETS"),
       lty = c(1,2,2), lwd = c(1,2,2),
       col = c("black", "tomato3", "darkgoldenrod4", "blue"),
       bty = "n")

newtotal = forecast(model4_total, h = 10)
newtotal

newtotal_ETS = forecast(ETS_total, h = 10)
newtotal_ETS

#make dataframe for plotting
newtotalf = data.frame(newtotal)
newtotal_ETSf = data.frame(newtotal_ETS)

#set up future years
newtotalf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newtotal_ETSf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)

#make a plot with data and predictions including a prediction interval ARIMA
ggplot() +
  geom_line(data = total, aes(x = year, y = consumption)) +
  xlim(min(total$year), newtotalf$yearF[10]) +
  geom_line(data = newtotalf, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newtotalf, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Energy Consumption", title = "NY Total Energy Consumption 10 Year Prediction (ARIMA)")

#make a plot with data and predictions including a prediction interval ETS
ggplot() +
  geom_line(data = total, aes(x = year, y = consumption)) +
  xlim(min(total$year), newtotal_ETSf$yearF[10]) +
  geom_line(data = newtotal_ETSf, aes(x = yearF, y = Point.Forecast), col = "red") +
  geom_ribbon(data = newtotal_ETSf, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total Energy Consumption", title = "NY Total Energy Consumption 10 Year Prediction (ETS)")

# Total forecast with ARIMA ----

#Simplifying source name
Coal$source = "Coal"
NG$source = "Natural Gas"
RE$source = "Renewables"
PET$source = "Petroleum"
NUC$source = "Nuclear"
total$source = "Total Energy"

#Creating historical data set, all combined together
all_hist_arima = rbind(Coal, NG)
all_hist_arima = rbind(all_hist_arima, RE)
all_hist_arima = rbind(all_hist_arima, PET)
all_hist_arima = rbind(all_hist_arima, NUC)
all_hist_arima = rbind(all_hist_arima, total)

#Simplifying source name
newCoalF$source = "Coal"
newNGf$source = "Natural Gas"
newREf$source = "Renewables"
newPETf$source = "Petroleum"
newNUCf$source = "Nuclear"
newtotalf$source = "Total Energy"

#Creating forecasted data set, all combined together
all_forecast_arima = rbind(newCoalF, newNGf)
all_forecast_arima = rbind(all_forecast_arima, newREf)
all_forecast_arima = rbind(all_forecast_arima, newPETf)
all_forecast_arima = rbind(all_forecast_arima, newNUCf)
all_forecast_arima = rbind(all_forecast_arima, newtotalf)

#combined graph
ggplot() +
  geom_line(data = all_hist_arima, aes(x = year, y = consumption/1000000, color = source)) +
  geom_line(data = all_forecast_arima, aes(x = yearF, y = Point.Forecast/1000000, color = source), linetype = "dashed", linewidth = .4) +
  theme_classic() +
  labs(title = "New York Energy Consumption Forecasts (ARIMA)", x = "Year", y = "Total Consumption (Per Million BBTU)", color = "Energy Source")

# TOTAL FORECAST with ETS ----

#Creating forecasted data set, all combined together
newCoalETSF$source = "Coal"
newNGETSf$source = "Natural Gas"
newRE_ETSf$source = "Renewables"
newPETf$source = "Petroleum"
newNUC_ETSf$source = "Nuclear"
newtotal_ETSf$source = "Total Energy"

#Combining all forecasted values
all_forecast_ets = rbind(newCoalETSF, newNGETSf)
all_forecast_ets = rbind(all_forecast_ets, newRE_ETSf)
all_forecast_ets = rbind(all_forecast_ets, newPETf)
all_forecast_ets = rbind(all_forecast_ets, newNUC_ETSf)
all_forecast_ets = rbind(all_forecast_ets, newtotal_ETSf)

#combined graph
ggplot() +
  geom_line(data = all_hist_arima, aes(x = year, y = consumption/100000, color = source)) +
  geom_line(data = all_forecast_ets, aes(x = yearF, y = Point.Forecast/100000, color = source), linetype = "dashed", linewidth = .4) +
  theme_classic() +
  labs(title = "New York Energy Consumption Forecasts (ETS)", x = "Year", y = "Total Consumption (Per Million BBTU)", color = "Energy Source")

#Renewable Energy Specific Growth ----

#Creating percentage
re_percent = (RE$consumption / total$consumption) * 100
re_percent_df = data.frame(year= RE$year, re_percent = re_percent)

#Graph of RE Percent over time
ggplot(re_percent_df, aes(x = year, y = re_percent)) +
  geom_line(color = "lightblue3")+
  theme_classic()+
  labs(title = "Renewable Energy Share of NY Total Energy Consumption", x = "Year", y = "Share of Total Energy Consumption (%)")

#Predicting future values w arima and ets
re_percent_ts = ts(re_percent_df$re_percent, start = min(re_percent_df$year), frequency = 1)

re_arima = arima(re_percent_ts, order = c(20,0,0))
re_future_ets = ets(re_percent_ts)

new_arima = forecast(re_arima, h = 10)
new_ets_future_re = forecast(re_future_ets, h = 10)

new_arima_re = data.frame(new_arima)
new_ets_re = data.frame(new_ets_future_re)

new_arima_re$year = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
new_ets_re$year = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)

ggplot() + 
  geom_line(data = re_percent_df, aes(x = year, y = re_percent), color = "lightblue3")+
  geom_line(data = new_ets_re, aes(x = year, y = Point.Forecast, color = "ETS"), linetype = "dashed")+
  geom_line(data = new_arima_re, aes(x = year, y = Point.Forecast, color = "ARIMA"), linetype = "dashed")+
  geom_ribbon(data = new_ets_re, aes( x = year, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5))+
  geom_ribbon(data = new_arima_re, aes( x = year, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5))+
  labs(title = "NY Renewable Energy Share Forecast",  x = "Year", y = "Share of Total Energy Consumption (%)", color = "Model")

#RE Forecast by source ----

#Biomass
Bio = consumption_long[consumption_long$MSN %in% c("BMTCB"),]

Bio$year = as.numeric(Bio$year)
Bio$consumption = as.numeric(Bio$consumption)

Bio_ts = ts(Bio$consumption, start = min(Bio$year), frequency = 1)
Bio_ETS = ets(Bio_ts)
newBio = forecast(Bio_ETS, h = 10)

newBiof = data.frame(newBio)
newBiof$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newBiof$source = "Biomass"

#Geothermal
Geo = consumption_long[consumption_long$MSN %in% c("GETCB"),]

Geo$year = as.numeric(Geo$year)
Geo$consumption = as.numeric(Geo$consumption)

Geo_ts = ts(Geo$consumption, start = min(Geo$year), frequency = 1)
Geo_ETS = ets(Geo_ts)
newGeo = forecast(Geo_ETS, h = 10)

newGeof = data.frame(newGeo)
newGeof$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newGeof$source = "Geothermal"

#Hydro
Hydro = consumption_long[consumption_long$MSN %in% c("HYTCB"),]

Hydro$year = as.numeric(Hydro$year)
Hydro$consumption = as.numeric(Hydro$consumption)

Hydro_ts = ts(Hydro$consumption, start = min(Hydro$year), frequency = 1)
Hydro_ETS = ets(Hydro_ts)
newHydro = forecast(Hydro_ETS, h = 10)

newHydrof = data.frame(newHydro)
newHydrof$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newHydrof$source = "Hydro"

#Solar
Solar = consumption_long[consumption_long$MSN %in% c("SOTCB"),]

Solar$year = as.numeric(Solar$year)
Solar$consumption = as.numeric(Solar$consumption)

Solar_ts = ts(Solar$consumption, start = min(Solar$year), frequency = 1)
Solar_ETS = ets(Solar_ts)
newSolar = forecast(Solar_ETS, h = 10)

newSolarf = data.frame(newSolar)
newSolarf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newSolarf$source = "Solar"

#Wind
Wind = consumption_long[consumption_long$MSN %in% c("WYTCB"),]

Wind$year = as.numeric(Wind$year)
Wind$consumption = as.numeric(Wind$consumption)

Wind_ts = ts(Wind$consumption, start = min(Wind$year), frequency = 1)
Wind_ETS = ets(Wind_ts)
newWind = forecast(Wind_ETS, h = 10)

newWindf = data.frame(newWind)
newWindf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newWindf$source = "Wind"

#Simplifying source name 
Bio$source = "Biomass"
Geo$source = "Geothermal"
Hydro$source = "Hydro"
Solar$source = "Solar"
Wind$source = "Wind"

#Putting all into forecasted data frame
all_renewable_forecast = rbind(newBiof, newGeof)
all_renewable_forecast = rbind(all_renewable_forecast, newHydrof)
all_renewable_forecast = rbind(all_renewable_forecast, newSolarf)
all_renewable_forecast = rbind(all_renewable_forecast, newWindf)


#Putting all into historic data frame
all_renewable_hist = rbind(Bio, Geo)
all_renewable_hist = rbind(all_renewable_hist, Hydro)
all_renewable_hist = rbind(all_renewable_hist, Solar)
all_renewable_hist = rbind(all_renewable_hist, Wind)

#Plotting/Graphing
ggplot() +
  geom_line(data = all_renewable_hist, aes(x = year, y = consumption, color = source)) +
  geom_line(data = all_renewable_forecast,aes(x = yearF, y = Point.Forecast, color = source), linetype = "dashed") +
  theme_classic() +
  labs(title = "NY Renewable Energy Source Forecasts (ETS)", x = "Year", y = "Total Consumption (Per BBTU)", color = "Renewable Source")


#Looking at CO2 Emissions forecast ----

#Cleaning Data

CO2emmissions = read.csv("/cloud/project/finalproject/co2_NY.csv")

CO2emmissions = CO2emmissions %>%
  select(-Data_Status, -State) #Select just the years and not data or state

names(CO2emmissions) = sub("X", "", names(CO2emmissions)) #Take X out from years
names(CO2emmissions)

CO2emmissions_long = pivot_longer(CO2emmissions,cols = -MSN, names_to = "year", values_to = "consumption") 
CO2emmissions_long

CO2emmissions_long$Description = Names$Description[match(CO2emmissions_long$MSN, Names$MSN)] 

#Create Forecast for total CO2 Emissions
CO2 = CO2emmissions_long[CO2emmissions_long$MSN %in% c("TETCE"),]

CO2$year = as.numeric(CO2$year)
CO2$consumption = as.numeric(CO2$consumption)

CO2_ts = ts(CO2$consumption, start = min(CO2$year), frequency = 1)

CO2_arima = arima(CO2_ts, order = c(20,0,0))
CO2_ETS = ets(CO2_ts)

newCO2_arima = forecast(CO2_arima, h = 10)
newCO2 = forecast(CO2_ETS, h = 10)

newCO2_arimaf = data.frame(newCO2_arima)
newCO2f = data.frame(newCO2)

newCO2_arimaf$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
newCO2f$yearF = c(2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)

newCO2_arimaf$source = "CO2 Emmissions"
newCO2f$source = "CO2 Emmissions"

#Plotting forecast

ggplot() +
  geom_line(data = CO2, aes(x = year, y = consumption)) + 
  xlim(min(CO2$year), newCO2f$yearF[10]) +
  geom_line(data = newCO2_arimaf, aes(x = yearF, y = Point.Forecast, color = "ARIMA"), linetype = "dashed") +
  geom_line(data = newCO2f, aes(x = yearF, y = Point.Forecast, color = "ETS"), linetype = "dashed") +
  geom_ribbon(data = newCO2f, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  geom_ribbon(data = newCO2_arimaf, aes(x = yearF, ymin = Lo.95, ymax = Hi.95), fill = rgb(0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  labs(x = "Year", y = "Total C02 Emissions (Million Metric Tons)", title = "NY CO2 Emission 10 Year Prediction", color = "Model")
