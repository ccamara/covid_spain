# Gets and prepares data for the dashboard.

library(tidyr)
library(dplyr)
library(readxl)

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


ccaa_covid19_altas <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_altas.csv")
ccaa_covid19_casos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos.csv")
ccaa_covid19_fallecidos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos.csv")
ccaa_covid19_uci <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_uci.csv") 


# UK ----------------------------------------------------------------------



# data_uk <- read.csv("https://www.arcgis.com/sharing/rest/content/items/ca796627a2294c51926865748c4a56e8/data")
# 
# data_uk2 <-  read_excel("https://www.arcgis.com/sharing/rest/content/items/e5fd11150d274bebaaf8fe2a7a2bda11/data")

