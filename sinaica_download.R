# 0. Llamar las paqueterías 

library(pacman)

p_load(fs,
       tidyverse,
       openair,
       openairmaps,
       readxl,
       janitor,
       lubridate)

# 1. Descargar la base de datos

#url <- "C:/Users/jair.carrillo/OneDrive - Instituto Fonacot/Escritorio/FONACOT_Jair/Jair/"

url <- "/Users/jair_carrillo/Desktop/Carpeta_R/" # Dirección de trabajo en casa


path <- paste0(url,"Anexo SISAI 143/bd_contaminantes/") #Dirección donde se encuentran los datos

# Función para obtener el diccionario del SINAICA

diccionario_sinaica <- read_excel(
  paste0(
    url,"Anexo SISAI 143/Diccionario de datos SISAI.xlsx"
    )
  ) |>
  rename(estacionesId = idEstación)

# Función para obtener los datos geográficos de la RAMA

coord_station_sinaica <- read_csv(
  paste0(
    getwd(),"/main_presentation_files/database-presentation/cat_estacion.csv"
  ),
  ) |> 
  select(
    -c(id_station,nom_estac)
    )|> 
  rename(
    clave_estacion = cve_estac
  )

# Función para descargar las bases de datos de 2020-2024 del SINAICA

data_list <- path |> 
  dir_ls() |> 
  map(
    .f= function(path){
      read_csv(
        path,
        col_types = "dccddd"
        )
    }
  )

# Función para unir las bases de datos con el diccionario del SINAICA

table_data_list <- data_list |> 
  set_names(dir_ls(path)) |> 
  bind_rows(.id = "file_path") |> 
  left_join(diccionario_sinaica)

# Función para seleccionar la red de monitoreo y año de interés y guardar en un csv

dl_station_pol <- function(anio_inicio, anio_fin, my_station, my_pol) {
  
  # Asegurarse de que los años se manejen correctamente como números
  anio_inicio <- as.numeric(anio_inicio)
  anio_fin <- as.numeric(anio_fin)
  
  # Filtrar y manipular los datos
  x <- table_data_list |> 
    filter(
      SMCA == my_station,
      parametro == my_pol
    ) |> 
    mutate(
      date = ymd_h(paste(fecha, hora)),
      year_date = year(date)
    ) |> 
    rename(
      concentracion = valorAct,
      clave_estacion = 'Clave de la estación',
      nombre_estacion = 'Nombre de la estación'
    ) |>
    filter(
      year_date >= anio_inicio & year_date <= anio_fin
    ) |>  
    select(
      -c(file_path, nivelValidacion, 'Red de monitoreo', 
         year_date,fecha, hora,O3, CO, SO2, NO2, PM2.5, 
         PM10, VV, DV, TMP,HR, PP, RS)
    ) |> 
    left_join(coord_station_sinaica) |> 
    select(-c(alt,obs_estac,SMCA,Municipio))
  
  # Formatear la columna date
  x$date <- format(x$date, "%Y-%m-%d %H:%M:%S")
  
  # Escribir el resultado en un archivo CSV
  write.csv(
    x,
    file = paste0(
      getwd(), "/main_presentation_files/database-presentation/cdmx/data_", 
      my_pol, "_", my_station, "_", anio_inicio, "_to_", anio_fin, ".csv"
    ),
    fileEncoding = "UTF-8"
  )
  
  # Devolver el data frame filtrado y manipulado
  return(x)
}

dl_station_pol(2022,2022,"Ciudad de México","PM10")

# Función para obtener la base de datos meteorologicos del SINAICA #

data_sinaica_meteo <- read_csv(
  paste0(
    url,"Anexo SISAI 143/bd_meteo/Meteorología Datos NO validados 2020-2024.csv")
  ) |> 
  rename(id_estacion="estacionesId") |> 
  
  full_join(clean_diccionario_sinaica,by=c("id_estacion"))

unique(data_sinaica_meteo$codigo)

dl_station_meteo <- function(my_station,my_year){
  
  x <- data_sinaica_meteo |> 
  filter(SMCA == my_station,
         parametro %in% c("DV", "HR", "TMP", "RS", "VV")) |> 
  mutate(
    date = ymd_h(paste(fecha,hora)),
    valorOrig  = as.numeric(valorOrig ),
    year_date = year(date)
  ) |> 
  rename(concentracion = valorOrig,
         clave_estacion = 'Clave de la estación',
         nombre_estacion = 'Nombre de la estación') |> 
  filter(year_date == my_year) |> 
  select(-c(fecha,hora,SMCA,year_date,'Red de monitoreo',
            Municipio,nombre_estacion,clave_estacion,codigo,validoOrig))
  
  # Formatear la columna date
  x$date <- format(x$date, "%Y-%m-%d %H:%M:%S")
  
  write.csv(
    x,
    file = paste0(
      getwd(),"/main_presentation_files/database-presentation/cdmx/data_",
      my_station,"_meteo_",my_year,".csv"
      ),
    fileEncoding = "UTF-8"
    )
  
  return(x)
}

dl_station_meteo("Ciudad de México",2022)

