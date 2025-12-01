# === Load packages ===
library(ggplot2)
library(dplyr)
library(lubridate)

# === 1. Read MeteoSwiss CSV files ===
zurich <- read.csv("Weather Data/ogd-smn_sma_d_historical.csv", sep = ";", stringsAsFactors = FALSE)
basel  <- read.csv("Weather Data/ogd-smn_bas_d_historical.csv", sep = ";", stringsAsFactors = FALSE)
lugano <- read.csv("Weather Data/ogd-smn_lug_d_historical.csv", sep = ";", stringsAsFactors = FALSE)
davos  <- read.csv("Weather Data/ogd-smn_dav_d_historical.csv", sep = ";", stringsAsFactors = FALSE)

# === 2. Convert date column ===
fmt <- c("%d.%m.%Y %H:%M", "%Y-%m-%d", "%d.%m.%Y")
zurich$Date <- as.Date(zurich$reference_timestamp, tryFormats = fmt)
basel$Date  <- as.Date(basel$reference_timestamp,  tryFormats = fmt)
lugano$Date <- as.Date(lugano$reference_timestamp, tryFormats = fmt)
davos$Date  <- as.Date(davos$reference_timestamp,  tryFormats = fmt)

# === 3. Keep only daily maximum temperature ===
zurich <- zurich[, c("Date", "tre200dx")]
basel  <- basel[,  c("Date", "tre200dx")]
lugano <- lugano[, c("Date", "tre200dx")]
davos  <- davos[,  c("Date", "tre200dx")]

colnames(zurich)[2] <- "Tmax_Zurich"
colnames(basel)[2]  <- "Tmax_Basel"
colnames(lugano)[2] <- "Tmax_Lugano"
colnames(davos)[2]  <- "Tmax_Davos"

# === 4. Merge stations ===
data <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE),
               list(zurich, basel, lugano, davos))

# === 5. Average Tmax ===
data$Tmax_mean <- rowMeans(data[, c("Tmax_Zurich", "Tmax_Basel", "Tmax_Lugano", "Tmax_Davos")], na.rm = TRUE)

# === 6. Identify heat days (≥ 30°C) ===
data$HeatDay <- ifelse(data$Tmax_mean >= 30, 1, 0)

# === 7. Time filter ===
data <- subset(data, Date >= as.Date("1950-01-01"))

# === 8. Annual aggregation ===
data$Year <- as.numeric(format(data$Date, "%Y"))

annual_heat <- aggregate(
  cbind(HeatDay, Tmax_mean) ~ Year,
  data = data,
  FUN = function(x) c(sum = sum(x, na.rm = TRUE), mean = mean(x, na.rm = TRUE))
)

annual_heat$HeatDays  <- annual_heat$HeatDay[, "sum"]
annual_heat$Mean_Tmax <- annual_heat$Tmax_mean[, "mean"]

annual_heat <- annual_heat[, c("Year", "HeatDays", "Mean_Tmax")]

# === 9. Filter years >= 1975 ===
annual_heat_75 <- annual_heat[annual_heat$Year >= 1975, ]

# === 10. Plot ===
ggplot(annual_heat_75, aes(x = Year)) +
  geom_line(aes(y = HeatDays, color = "Heat Days (≥30°C)"), linewidth = 1) +
  geom_smooth(aes(y = HeatDays, color = "Heat Days Trend"),
              method = "lm", se = FALSE, linetype = "dotted") +
  geom_line(aes(y = Mean_Tmax, color = "Mean Tmax (°C)"), linewidth = 1) +
  geom_smooth(aes(y = Mean_Tmax, color = "Mean Tmax Trend"),
              method = "lm", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c(
    "Heat Days (≥30°C)" = "#008C44",
    "Heat Days Trend"   = "#008C44",
    "Mean Tmax (°C)"    = "#00583C",
    "Mean Tmax Trend"   = "#00583C"
  )) +
  scale_y_continuous(
    name = "Heat Days (Count)",
    sec.axis = sec_axis(~., name = "Mean Daily Maximum Temperature (°C)")
  ) +
  labs(
    title = "Annual Mean Temperature and Heat Days in Switzerland (≥30°C)",
    subtitle = "Years 1975–present",
    x = "Year",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

