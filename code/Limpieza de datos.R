library(readr)

library(dplyr)

library(ggplot2)

#Primero se filtra la base de datos para dejar solo variables de interés, e incluir las delimitaciones espaciales y temporales correspondientes. 

url <- "https://raw.githubusercontent.com/eetefy2311/Data-proyecto-individual/refs/heads/main/global_bleaching_environmental%20(1).csv"

temp.file <- tempfile(fileext = ".csv")

download.file(url, destfile = temp.file, mode = "wb")

database <- read_csv(temp.file)

View(database)

base.filtrada <- database %>%
  select(Site_ID, Latitude_Degrees, Longitude_Degrees, Ocean_Name, Country_Name,
  Ecoregion_Name, Realm_Name, Exposure, Distance_to_Shore, Date_Year, Temperature_Mean, 
  Temperature_Maximum, SSTA_Mean, SSTA_Maximum, TSA_Mean, TSA_Maximum, Turbidity, Cyclone_Frequency, 
  Windspeed, Percent_Cover, Percent_Bleaching
  ) %>% 
  filter(
    Ocean_Name == "Atlantic", Date_Year >= 2000, Date_Year <= 2020)

View(base.filtrada)

#Manejo de datos faltantes y outliers

#Observación general de los datos

#Graficos planteados

#Ver el cambio en el blanqueamiento durante los ultimos años
base.filtrada %>% 
  group_by(Date_Year) %>% 
  summarise(blanq.prom = mean(Percent_Bleaching, na.rm = TRUE)) %>% 
  mutate(Date_Year = as.numeric(Date_Year)) %>% 
  ggplot(aes(x = Date_Year, y = blanq.prom)) +
  geom_line(color = "steelblue", linewidth = 1) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 2)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +  
  labs(
    title = "Tendencia del blanqueamiento coralino (2000–2020)",
    x = "Año",
    y = "Porcentaje de blanqueamiento (%)"
  ) +
  theme_minimal()
