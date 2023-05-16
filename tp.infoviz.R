
options(scipen = 999)
datos <- read.csv("ManicTimeData_2023-05-12.csv")
library(lubridate)
library(dplyr)
library(hms)
library(stringr)
library(tidyr)

#2023-05-08T15:45:14
datos$Duration
datos <- datos %>% 
  mutate(Duration2=lubridate::hms(Duration)) %>% 
  mutate(Duration2=period_to_seconds(Duration2)) %>% 
  mutate(Start=as.POSIXct(.$Start, format = "%Y-%m-%dT%H:%M:%OS"),
         End=as.POSIXct(.$Start, format = "%Y-%m-%dT%H:%M:%OS"),
         start_fecha=format(Start, "%Y-%m-%d"),
         end_fecha=format(Start, "%Y-%m-%d")) %>% 
  filter(!is.na(Duration2))


tabla_resumen <- datos %>% 
                group_by(start_fecha, Process ) %>% 
                summarise(tiempo_seg=sum(Duration2),
                          tiempo_horas=tiempo_seg/3600) %>% 
  select(-tiempo_seg) %>% 
  spread(Process, tiempo_horas)

View(tabla_resumen)
writexl::write_xlsx(tabla_resumen, "tabla_resumen.xlsx")

tabla_resumen_detalle <- datos %>% 
  group_by(start_fecha, Process, Name ) %>% 
  summarise(tiempo_seg=sum(Duration2),
            tiempo_horas=tiempo_seg/3600)

View(tabla_resumen_detalle) 


tabla_resumen_sin_fecha <- datos %>% 
  group_by( Process ) %>% 
  summarise(tiempo_seg=sum(Duration2),
            tiempo_horas=tiempo_seg/3600)
View(tabla_resumen_sin_fecha)

tabla_resumen_solo_fecha <- datos %>% 
  group_by(start_fecha) %>% 
  summarise(tiempo_seg=sum(Duration2),
            tiempo_horas=tiempo_seg/3600)
View(tabla_resumen_solo_fecha)

tabla_resumen_solo_fecha$start_fecha <- as.Date(tabla_resumen_solo_fecha$start_fecha)
tabla_resumen_solo_fecha <- tabla_resumen_solo_fecha %>%
  complete(start_fecha = seq(min(start_fecha), max(start_fecha), by = "day")) %>% 
  group_by(start_fecha) %>%
  fill(tiempo_seg,tiempo_horas)

View(datos_completos)















