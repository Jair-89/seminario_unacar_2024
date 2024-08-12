
library(pacman)

p_load(tidyverse,
       openair,
       openairmaps,
       readxl,
       janitor,
       lubridate,
       fs)



path <- paste0(getwd(),"/main_presentation_files/database-presentation/cdmx/")

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
  bind_rows(.id = "file_path")


pollution_call <- function(my_pol){
  
  x <- table_data_list |> 
    filter(
      parametro == my_pol
    ) |> 
    tibble("{my_pol}":= concentracion) |> 
    select(-c(file_path,concentracion))
  
  return(x) 
  
}



station_calendar_plot <- function(my_pol){
  
  my_pol_break <- case_when(
    my_pol=='pm10'~c(0,30,45,95,135,200),
    my_pol=='pm2.5'~c(0,20,35,45,95,200)
  )
  
  
  x <- pollution_call(my_pol) |> 
    openair::calendarPlot(
      pollutant = my_pol,
      breaks = my_pol_break,
      cols = c("green","yellow","orange","red","purple"))
  
  return(x)
  
}

