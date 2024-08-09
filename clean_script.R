
library(pacman)

p_load(tidyverse,
       openair,
       openairmaps,
       readxl,
       janitor,
       lubridate,
       fs)



path <- paste0(getwd(),"/main_presentation_files/database-presentation/nl_data/")

data_list <- path |> 
  dir_ls() |> 
  map(
    .f= function(path){
      read_csv(
        path
      )
      }
  )

table_data_list <- data_list |> 
  set_names(dir_ls(path)) |> 
  bind_rows(.id = "file_path") |> 
  rename(
    nombre_estacion = "Nombre de la estación",
    clave_estacion = "Clave de la estación")


pollution_call <- function(my_pol){
  
  x <- table_data_list |> 
    filter(
      parametro == my_pol
    ) 
  
  return(x)
}

eda_pm10 <- pollution_call("PM10") |> 
  openair::aqStats(pollutant = "concentracion",type="nombre_estacion")

station_calendar_plot <- function(my_pol){
  
  my_pol_break <- case_when(
    my_pol=='pm10'~c(0,30,45,95,135,200),
    my_pol=='pm2.5'~c(0,20,35,45,95,200)
  )
  
  
  x <- pollutant_apodaca |> 
    openair::calendarPlot(
      pollutant = my_pol,
      breaks = my_pol_break,
      cols = c("green","yellow","orange","red","purple"))
  
  return(x)
  
}

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


