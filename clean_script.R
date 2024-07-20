
library(pacman)

p_load(tidyverse,
       openair,
       openairmaps,
       readxl,
       janitor,
       lubridate)

clear_data_station <- function(my_station,my_parameter){
  
  my_file <- paste0(my_station,"_",my_parameter,"_2024.csv")
  
  x <- read_csv(
    paste0(getwd(),"/main_presentation_files/database-presentation/nuevo_leon/",my_file)
    ) |> 
    janitor::remove_empty(
      "rows"
      ) |> 
    mutate(
      hora = str_split(Hora,"-")
      ) |> 
    unnest_wider(
      hora,names_sep = ""
      ) |> 
    mutate(
      date_convert = paste(Fecha,hora2),
      date = dmy_hm(date_convert),
      station = my_station
      ) |> 
    rename(
      parameter = Parametro,
           concentration = `Concentraciones horarias`
      ) |> 
    select(
      c(date,parameter,station,concentration)
      )

return(x)

}

pm10_apodaca <- clear_data_station("apodaca","pm10") |> 
  rename(pm10 = concentration) |> 
  select(-parameter)

pm2.5_apodaca <- clear_data_station("apodaca","pm25") |> 
  rename(pm2.5 = concentration)|> 
  select(-parameter)

o3_apodaca <- clear_data_station("apodaca","o3") |> 
  rename(o3 = concentration)|> 
  select(-parameter)

so2_apodaca <- clear_data_station("apodaca","so2") |> 
  rename(so2 = concentration)|> 
  select(-parameter)

no2_apodaca <- clear_data_station("apodaca","no2") |> 
  rename(no2 = concentration)|> 
  select(-parameter)

co_apodaca <- clear_data_station("apodaca","co") |> 
  rename(co = concentration)|> 
  select(-parameter)

pollutant_apodaca <- pm10_apodaca |> 
  left_join(pm2.5_apodaca)

openair::aqStats(pm10_apodaca,pollutant = "pm10")

openair::timePlot(pollutant_apodaca,pollutant = c("pm10","pm2.5"),avg.time = "hour",group = T)

openair::calendarPlot(pm10_apodaca,pollutant = "pm10")

