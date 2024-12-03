library(dplyr)
library(tidyr)
library(ggplot2)
# install.packages("ggiraphExtra")
library(ggiraphExtra)
library(fmsb)
library(readxl)

# read in datasets
covid_data <- read.csv("datasets/COVID-19.csv")
cholera_data <- read.csv("datasets/Cholera.csv")
dengue_data <- read_excel("datasets/Dengue.xlsx")

covid_data <- covid_data %>%
  mutate(Date_reported = as.Date(Date_reported, format = "%Y-%m-%d"))

cholera_data <- cholera_data %>%
  mutate(Year = as.character(Year),
         Date = as.Date(paste0(Year, "-01-01"), format = "%Y-%m-%d"))

dengue_data <- dengue_data %>%
  mutate(date = as.Date(date, origin = "1970-01-01"))

covid_data <- covid_data %>%
  rename(Country = Country, Mortality = New_deaths, Cases = New_cases)

cholera_data <- cholera_data %>%
  rename(Country = Country, Mortality = Deaths, Cases = Total_Cases)

dengue_data <- dengue_data %>%
  rename(Country = country, Mortality = deaths, Cases = cases)





pandemic_summary <- data.frame(
  Pandemic = c("COVID-19", "Cholera", "Dengue"),
  Incubation_Period = c(5, 2, 7), 
  Mortality_Rate = c(2.5, 5, 0.1),
  Illness_Duration = c(14, 7, 10)
)

max_vals <- c(Incubation_Period = 10, Mortality_Rate = 10, Illness_Duration = 15)
min_vals <- c(Incubation_Period = 0, Mortality_Rate = 0, Illness_Duration = 0)

radar_data_combined <- rbind(max_vals, min_vals, pandemic_summary[,-1])

#Combined Radar Chart
radarchart(
  radar_data_combined,
  axistype = 1,
  pcol = c("blue", "red", "green"),
  pfcol = c(rgb(0, 0, 1, 0.2), rgb(1, 0, 0, 0.2), rgb(0, 1, 0, 0.2)),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = seq(0, 10, 2),
  vlcex = 0.8,
  title = "Combined Radar Chart for All Pandemics"
)
legend(
  "right",
  inset=c(-0.5,0),
  legend = c("COVID-19", "Cholera", "Dengue"),
  col = c("blue", "red", "green"),
  pch = 15,
  bty = "n"
)

for (i in 1:nrow(pandemic_summary)) {
  # Assign color based on the pandemic
  color <- ifelse(pandemic_summary$Pandemic[i] == "COVID-19", "blue",
                  ifelse(pandemic_summary$Pandemic[i] == "Cholera", "red", "green"))
  
  radar_data <- rbind(max_vals, min_vals, pandemic_summary[i, -1]) 
  
  #Seperate radar chart
  radarchart(
    radar_data,
    axistype = 1,
    pcol = color,
    pfcol = rgb(col2rgb(color)[1] / 255, col2rgb(color)[2] / 255, col2rgb(color)[3] / 255, 0.3),
    plwd = 2,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    caxislabels = seq(0, 10, 2),
    vlcex = 0.8,
    title = paste("Radar Chart for", pandemic_summary$Pandemic[i])
  )
}


