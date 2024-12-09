#Q2: 

#Load all the required packages
 
library(readr)
library(readxl)
library(tidyverse)
library(leaflet)
library(shiny)
library(sf)
library(ggplot2)
library(lubridate)
library(plotly)
 

# #Step 1: Load all datasets

 
getwd()
covid_data <- read_csv("../datasets/COVID-19.csv")
cholera_data <- read_csv("../datasets/Cholera.csv")
dengue_data <- read_excel("../datasets/Dengue.xlsx")
 

# #Step2: Preprocess the data sets

 
# COVID-19 data
covid_preprocessed <- covid_data %>%
  rename(
    region = WHO_region,
    report_date = Date_reported, 
    covid_cases = New_cases        
  ) %>%
  mutate(
    report_date = as.Date(report_date), 
    region = as.character(region),
    Country = as.character(Country)
  )

head(covid_preprocessed)
 

 
# Cholera data
cholera_preprocessed <- cholera_data %>%
  rename(
    region = WHO_Region,
    year = Year,                  
    cholera_cases = Total_Cases  
  ) %>%
  mutate(
    date = as.Date(paste0(year, "-01-01")),  # Convert Year to Date
    region = as.character(region),
    Country = as.character(Country)
  )


head(cholera_preprocessed)
 

 
#Dengue data
str(dengue_data$date_lab)
dengue_preprocessed <- dengue_data %>%
  rename(
    region = who_region_long,
    Country = country,
    record_date = date_lab,       
    dengue_cases = confirmed_cases  
  ) %>%
  mutate(
    record_date = as.Date(paste0(record_date, "-01"), format = "%b %Y-%d"),  
    region = as.character(region),
    Country = as.character(Country)
  )

head(dengue_preprocessed)
 

# #Step 3: Data Transformations - Aggregate the case count based on date and country


 
# COVID-19 Aggregation
covid_aggregated <- covid_preprocessed %>%
  mutate(year = as.character(year(report_date))) %>%                    
  group_by(Country, year) %>%
  summarize(total_cases = sum(covid_cases, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(agg_type = "yearly")                             

covid_total <- covid_preprocessed %>%
  group_by(Country) %>%
  summarize(total_cases = sum(covid_cases, na.rm = TRUE)) %>%
  mutate(year = "All", agg_type = "total")                

covid_combined <- bind_rows(covid_aggregated, covid_total)
 

 
# Cholera Aggregation
cholera_aggregated <- cholera_preprocessed %>%
  mutate(year = as.character(year)) %>%
  group_by(Country, year) %>%
  summarize(total_cases = sum(cholera_cases, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(agg_type = "yearly")

cholera_total <- cholera_preprocessed %>%
  group_by(Country) %>%
  summarize(total_cases = sum(cholera_cases, na.rm = TRUE)) %>%
  mutate(year = "All", agg_type = "total")

cholera_combined <- bind_rows(cholera_aggregated, cholera_total)
 

 
# Dengue Aggregation
dengue_aggregated <- dengue_preprocessed %>%
  mutate(year = as.character(year(record_date))) %>%                    
  group_by(Country, year) %>%
  summarize(total_cases = sum(dengue_cases, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(agg_type = "yearly")

dengue_total <- dengue_preprocessed %>%
  group_by(Country) %>%
  summarize(total_cases = sum(dengue_cases, na.rm = TRUE)) %>%
  mutate(year = "All", agg_type = "total")

dengue_combined <- bind_rows(dengue_aggregated, dengue_total)
 

 #Step 4: Data preparation for Visualization

 
library(rnaturalearth)
library(rnaturalearthdata)

world_boundaries <- ne_countries(scale = "medium", returnclass = "sf")
#print("World boundaries naming conventions:")
#print(unique(world_boundaries$name))
 

 #Step 4.1: Standardize the Country names

 
#world map
world_map <- world_boundaries %>%
  mutate(name = tolower(trimws(name)))

# covid-19
covid_combined <- covid_combined %>%
  mutate(Country = tolower(trimws(Country)))
# Cholera
cholera_combined <- cholera_combined %>%
  mutate(Country = tolower(trimws(Country)))
#Dengue
dengue_combined <- dengue_combined %>%
  mutate(Country = tolower(trimws(Country)))


head(dengue_combined)
head(cholera_combined)
head(covid_combined)
head(world_map$name)
 

 #Step 4.2: Merge the data

 
# Combine all diseases into a single dataset
all_diseases_combined <- bind_rows(
  covid_combined %>% mutate(disease = "COVID"),
  cholera_combined %>% mutate(disease = "Cholera"),
  dengue_combined %>% mutate(disease = "Dengue")
)

all_diseases_combined
 

 #Step5: Visualize the data

 #Step 5.1: Define UI for the Shiny App

 
ui <- fluidPage(
  titlePanel("Global Disease Spread - Heatmap"),
  sidebarLayout(
    sidebarPanel(
      selectInput("disease", "Select Disease:", 
                  choices = c("COVID", "Cholera", "Dengue"), 
                  selected = "COVID"),
      selectInput("year", "Select Year:", 
                  choices = NULL,  # This will be dynamically populated
                  selected = "All")
    ),
    mainPanel(
      plotOutput("heatmapPlot")
    )
  )
)
 

 #Step 5.2: Define server logic

 
server <- function(input, output, session) {
  
  # Updates year options based on selected disease
  observe({
    available_years <- all_diseases_combined %>%
      filter(disease == input$disease) %>%
      pull(year) %>%
      unique() %>%
      sort()
    updateSelectInput(session, "year",
                      choices = c("All", available_years),
                      selected = "All")
  })
  
  
  output$heatmapPlot <- renderPlot({
    # Filter data based on user inputs
    filtered_data <- all_diseases_combined %>%
      filter(disease == input$disease) %>%
      filter(year == input$year | input$year == "All")
    
    # Merge with world map
    map_data <- world_map %>%
      left_join(filtered_data, by = c("name" = "Country"))
    
    ggplot(map_data) +
      geom_sf(aes(fill = total_cases), color = "white") +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
      labs(title = paste("Heatmap of spread of ", input$disease,
                         if (input$year != "All") paste("(", input$year, ")") else ""),
           fill = "Total Cases") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))
  })
}
 

 #Step 5.3: Run the App
 
shinyApp(ui = ui, server = server)
 
 
#write.csv(all_diseases_combined, "combined.csv", row.names = FALSE)

 


