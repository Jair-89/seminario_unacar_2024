library(pacman)

p_load(tidyverse,
       openair,
       openairmaps,
       readxl,
       janitor,
       lubridate)

url <- "C:/Users/jair.carrillo/OneDrive - Instituto Fonacot/Escritorio/FONACOT_Jair/Jair/"

diccionario_sinaica <- read_excel(
  paste0(
    url,"Anexo SISAI 143/Diccionario de datos SISAI.xlsx"
    )
  )

data_sinaica_2020 <- read_csv(
  paste0(
    url,"Anexo SISAI 143/Contaminantes Datos validados 2020.csv"
  )
) |> 
  rename(id_estacion="estacionesId")


data_sinaica_2021 <- read_csv(
  paste0(
    url,"Anexo SISAI 143/Contaminantes Datos validados 2021.csv"
    )
) |> 
  rename(id_estacion="estacionesId")

data_sinaica_2022 <- read_csv(
  paste0(
    url,"Anexo SISAI 143/Contaminantes Datos validados 2022.csv"
  )
) |> 
  rename(id_estacion="estacionesId")

data_sinaica_meteo <- read_csv(
  paste0(
    url,"Anexo SISAI 143/Meteorología Datos NO validados 2020-2024.csv"
  )
) |> 
  rename(id_estacion="estacionesId")

data_sinaica_20_22 <- data_sinaica_2020 |> 
  bind_rows(data_sinaica_2021) |> 
  bind_rows(data_sinaica_2022)

names(clean_diccionario_sinaica)

clean_diccionario_sinaica <- diccionario_sinaica |> 
  select(SMCA,"Red de monitoreo","Municipio","idEstación",
         "Nombre de la estación","Clave de la estación") |> 
  rename(id_estacion = "idEstación")

union_dic_data_sinaica_20_22 <- data_sinaica_20_22 |> 
  full_join(clean_diccionario_sinaica,by=c("id_estacion"))

unique(union_dic_data_sinaica_20_22$parametro)

monterrey_sinaica <- union_dic_data_sinaica_20_24 |> 
  filter(SMCA == "Nuevo León",
         parametro %in% c("PM10","PM2.5","O3","NO2","CO")) |> 
  mutate(
    date = ymd_h(paste(fecha,hora)),
    valorAct = as.numeric(valorAct),
    year_date = year(date)
  ) |> 
  rename(concentracion = valorAct) |> 
  filter(year_date == 2020) |> 
  select(-c(fecha,hora,nivelValidacion,SMCA,year_date,'Red de monitoreo',Municipio,id_estacion)) |> 
  write_csv(
    file = paste0(
      url,"Anexo SISAI 143/data_monterrey_2020.csv"))
  

monterrey_sinaica <- union_dic_data_sinaica_20_24 |> 
  filter(SMCA == "Nuevo León",
         parametro %in% c("PM10","PM2.5","O3","NO2","CO")) |> 
  mutate(
    date = ymd_h(paste(fecha,hora)),
    valorAct = as.numeric(valorAct),
    year_date = year(date)
  ) |> 
  rename(concentracion = valorAct) |> 
  filter(year_date == 2021) |> 
  select(-c(fecha,hora,nivelValidacion,SMCA,year_date,'Red de monitoreo',Municipio,id_estacion)) |> 
  write_csv(
    file = paste0(
      url,"Anexo SISAI 143/data_monterrey_2021.csv"))

monterrey_sinaica <- union_dic_data_sinaica_20_24 |> 
  filter(SMCA == "Nuevo León",
         parametro %in% c("PM10","PM2.5","O3","NO2","CO")) |> 
  mutate(
    date = ymd_h(paste(fecha,hora)),
    valorAct = as.numeric(valorAct),
    year_date = year(date)
  ) |> 
  rename(concentracion = valorAct) |> 
  filter(year_date == 2022) |> 
  select(-c(fecha,hora,nivelValidacion,SMCA,year_date,'Red de monitoreo',Municipio,id_estacion)) |> 
  write_csv(
    file = paste0(
      url,"Anexo SISAI 143/data_monterrey_2022.csv"))


unique(data_sinaica_meteo$validoOrig)

clean_data_sinaica_meteo <- data_sinaica_meteo |> 
  full_join(clean_diccionario_sinaica,by=c("id_estacion"))

monterrey_meteo_sinaica <- clean_data_sinaica_meteo |> 
  filter(codigo == "MTY",
         parametro %in% c("DV", "HR", "PP", "RS", "VV")) |> 
  mutate(
    date = ymd_h(paste(fecha,hora)),
    valorOrig  = as.numeric(valorOrig ),
    year_date = year(date)
  ) |> 
  rename(concentracion = valorOrig) |> 
  filter(year_date == 2022) |> 
  select(-c(fecha,hora,SMCA,year_date,'Red de monitoreo',Municipio,id_estacion,codigo,validoOrig)) |> 
  write_csv(
    file = paste0(
      url,"Anexo SISAI 143/data_monterrey_meteo_2022.csv"))


monterrey_sinaica

unique(monterrey_sinaica$nivelValidacion)

