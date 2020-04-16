# Gets and prepares data for the dashboard.

library(tidyverse)
library(readxl)
library(sf)

# Spain -------------------------------------------------------------------

nacional_covid19 <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19.csv") %>% 
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>% 
  mutate(activos = casos - altas - fallecimientos) %>% 
  mutate(rownum = row_number()) %>% 
  mutate(casos_anteriores = ifelse(rownum == 1, NA, dplyr::lag(casos))) %>% 
  mutate(variacion_casos = casos - casos_anteriores) %>% 
  mutate(variacion_uci = ifelse(rownum == 1, NA, 
                                ingresos_uci - dplyr::lag(ingresos_uci))) %>% 
  mutate(variacion_activos = ifelse(rownum == 1, NA, 
                                    activos - dplyr::lag(activos))) %>% 
  mutate(variacion_altas = ifelse(rownum == 1, NA, 
                                  altas - dplyr::lag(altas))) %>% 
  mutate(variacion_fallecimientos = ifelse(rownum == 1, NA, 
                                           fallecimientos - dplyr::lag(fallecimientos))) %>% 
  select(fecha, casos, variacion_casos, ingresos_uci, variacion_uci, altas,
         variacion_altas, fallecimientos, variacion_fallecimientos, activos,
         variacion_activos) %>% 
  dplyr::arrange(desc(fecha))

write.csv(nacional_covid19, file = "data/interim/nacional_covid.csv")

spain_age <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19_rango_edad.csv")

#Creating a dataset for polygons with weekend dates
weekends_spain <- data.frame(fecha = as.Date(nacional_covid19$fecha),
                             day = weekdays(as.Date(nacional_covid19$fecha),
                                            abbreviate = FALSE)) %>%
  filter(day %in% c("Saturday", "Sunday")) %>%
  pivot_wider(names_from = day, values_from = fecha) %>%
  unnest()


# Spain CCAA --------------------------------------------------------------

ccaa_covid19_altas <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_altas_long.csv", colClasses = 'character') %>% 
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>% 
  mutate(cod_ine = as.factor(cod_ine)) %>% 
  mutate(CCAA = as.factor(CCAA)) %>% 
  mutate(altas = as.numeric(total)) %>% 
  select(-total)

ccaa_covid19_casos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos_long.csv", colClasses = 'character') %>% 
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>% 
  mutate(cod_ine = as.factor(cod_ine)) %>% 
  mutate(CCAA = as.factor(CCAA)) %>% 
  mutate(casos = as.numeric(total)) %>% 
  select(-total)
  
ccaa_covid19_fallecidos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos_long.csv",  colClasses = 'character') %>% 
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>% 
  mutate(cod_ine = as.factor(cod_ine)) %>% 
  mutate(CCAA = as.factor(CCAA)) %>% 
  mutate(fallecidos = as.numeric(total)) %>% 
  select(-total)

ccaa_covid19_uci <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_uci_long.csv",  colClasses = 'character') %>% 
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>% 
  mutate(cod_ine = as.factor(cod_ine)) %>% 
  mutate(CCAA = as.factor(CCAA)) %>% 
  mutate(uci = as.numeric(total)) %>% 
  select(-total) 

ccaa_covid19_hospitalizados <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_hospitalizados_long.csv",  colClasses = 'character') %>% 
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>% 
  mutate(cod_ine = as.factor(cod_ine)) %>% 
  mutate(CCAA = as.factor(CCAA)) %>% 
  mutate(hospitalizados = as.numeric(total)) %>% 
  select(-total) 

ccaa_covid19 <- list(ccaa_covid19_casos, ccaa_covid19_altas, ccaa_covid19_uci, 
             ccaa_covid19_hospitalizados, ccaa_covid19_fallecidos) %>% 
  reduce(left_join, by = c("fecha", "cod_ine", "CCAA")) %>% 
  mutate(activos = casos - altas - fallecidos) %>% 
  mutate(rownum = row_number()) %>% 
  mutate(casos_anteriores = ifelse(rownum == 1, NA, dplyr::lag(casos))) %>% 
  mutate(variacion_casos = casos - casos_anteriores) %>% 
  mutate(variacion_uci = ifelse(rownum == 1, NA, 
                                uci - dplyr::lag(uci))) %>% 
  mutate(variacion_activos = ifelse(rownum == 1, NA, 
                                    activos - dplyr::lag(activos))) %>% 
  mutate(variacion_altas = ifelse(rownum == 1, NA, 
                                  altas - dplyr::lag(altas))) %>% 
  mutate(variacion_fallecidos = ifelse(rownum == 1, NA,
                                       fallecidos - dplyr::lag(fallecidos))) %>% 
  select(fecha, cod_ine, CCAA, casos, variacion_casos, uci, variacion_uci, altas,
         variacion_altas, fallecidos, variacion_fallecidos, activos,
         variacion_activos) %>% 
  arrange(desc(fecha))



#print today's date
today <- format(Sys.Date(), format = "%Y-%m-%d")

spain <- st_read("data/raw/ign_spain_ccaa.geojson")

ccaa_covid19_sp <- sp::merge(spain, ccaa_covid19, by = "cod_ine", all = F) %>% 
  filter(fecha == ccaa_covid19$fecha[1])


# UK ----------------------------------------------------------------------



# data_uk <- read.csv("https://www.arcgis.com/sharing/rest/content/items/ca796627a2294c51926865748c4a56e8/data")
# 
# data_uk2 <-  read_excel("https://www.arcgis.com/sharing/rest/content/items/e5fd11150d274bebaaf8fe2a7a2bda11/data")

