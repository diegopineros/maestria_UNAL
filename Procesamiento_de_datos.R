##############################################################################'#
################################  Paquetes  ################################,####
##############################################################################'#

library(tidyverse)
library(data.table)
library(haven)

##############################################################################'#
#############################  Procesamiento  ##################################
##############################################################################'#

for (i in c(2019)){#2018, 2019, 2020, 2021, 2022
  
  print(i)
  base_poblacion  <- read_dta(paste(getwd(),'Data',i,'Caracteristicas y composicion del hogar.dta', sep='/'))
  base_vivienda <- read_dta(paste(getwd(),'Data',i,"Datos de la vivienda.dta", sep='/'))
  base_educ <- read_dta(paste(getwd(),'Data',i,"Educacion.dta", sep='/'))
  base_hogar <- read_dta(paste(getwd(),'Data',i,"Servicios del hogar.dta", sep='/'))
  
# P1_DEPARTAMENTO
  if (i == 2018) {
    base_vivienda$P1_DEPARTAMENTO <- base_vivienda$DPTO
  }
  
  fla_hog <- base_hogar %>% select(DIRECTORIO,SECUENCIA_P,ORDEN,P5666,P8536,P1698,
                                   I_HOGAR,I_UGASTO,I_OU,CANT_PERSONAS_HOGAR)#FEX_C, P1698S1, PERCAPITA
  fla_hog$SECUENCIA_ENCUESTA <- fla_hog$SECUENCIA_P
  
  fila_viv <- base_vivienda %>% select(DIRECTORIO,SECUENCIA_ENCUESTA,ORDEN,
                                       P1_DEPARTAMENTO,CLASE,P1070,P8520S1)#,FEX_C
  
  
  fila_viv <- base_vivienda %>% select(DIRECTORIO,SECUENCIA_ENCUESTA,ORDEN,P1_DEPARTAMENTO,CLASE, P8520S1)
  fla_hog <- base_hogar %>% select(DIRECTORIO,SECUENCIA_ENCUESTA,SECUENCIA_P,ORDEN,FEX_C,P5666,P8536,P1698,P1698S1, 
                                   I_HOGAR,I_UGASTO, PERCAPITA,I_OU,CANT_PERSONAS_HOGAR)
  fla_hog$SECUENCIA_ENCUESTA_A <- fla_hog$SECUENCIA_ENCUESTA
  fla_hog$SECUENCIA_ENCUESTA <- fla_hog$SECUENCIA_P
  
  base_combustible <- inner_join(fila_viv,fla_hog, by= c("DIRECTORIO", "SECUENCIA_ENCUESTA"))
  
  base_combustible$SECUENCIA_P <- base_combustible$SECUENCIA_ENCUESTA_A
  
  educ <- base_educ %>% select(DIRECTORIO,SECUENCIA_ENCUESTA,SECUENCIA_P,P8587,P8587S1)
  pobl <- base_poblacion %>% select(DIRECTORIO,SECUENCIA_ENCUESTA,P6020,P6040,P6051)
  
  base_pop <- inner_join(x = educ,y = pobl,by=c("DIRECTORIO","SECUENCIA_ENCUESTA"))
  
  base_pop <- base_pop %>% group_by(DIRECTORIO,SECUENCIA_P) %>% mutate(avg_y=mean(P6040), n_mie=n())
  
  base_pop <- as.data.table(base_pop)
  base_pop <- base_pop[P6051==1,jefe_sex:=P6020]
  base_pop <- base_pop[P6051!=1,jefe_sex:=0]
  base_pop <- base_pop[P6051==1,jefe_y:=P6040]
  base_pop <- base_pop[P6051!=1,jefe_y:=0]
  base_pop <- base_pop[P6040<=18,menores:=1]
  base_pop <- base_pop[P6040>18,menores:=0]
  base_pop <- base_pop[P6051==1,jefe_educ:=P8587]
  base_pop <- base_pop[P6051!=1,jefe_educ:=0]
  base_pop <- base_pop[P6051==1,jefe_educ_a:=P8587S1]
  base_pop <- base_pop[P6051!=1,jefe_educ_a:=0]
  
  base_pop <- base_pop %>% group_by(DIRECTORIO,SECUENCIA_P) %>% 
    summarise(jefe_sex=max(jefe_sex),jefe_y=max(jefe_y),
              menores=sum(menores),jefe_educ=max(jefe_educ),
              jefe_educ_a=max(jefe_educ_a),avg_y=mean(avg_y),
              n_mie=mean(n_mie))
  
  
  base_combustible <- inner_join(base_combustible,base_pop, by= c("DIRECTORIO", "SECUENCIA_P"))
  
  write.table(base_combustible,paste("./base_paper", i, '.csv', sep=''), sep = ";",dec=".",row.names = FALSE)
  print(paste("./base_paper", i, '.csv', sep=''))
}

##############################################################################'
##############################################################################'
##############################################################################'
##############################################################################'

# - ***Microdatos ENCV 2018*** https://microdatos.dane.gov.co/index.php/catalog/607/get_microdata
# - ***Microdatos ENCV 2019*** https://microdatos.dane.gov.co/index.php/catalog/678/get_microdata 
# - ***Microdatos ENCV 2020*** https://microdatos.dane.gov.co/index.php/catalog/718/get_microdata
# - ***Microdatos ENCV 2021*** https://microdatos.dane.gov.co/index.php/catalog/734/get-microdata
# - ***Microdatos ENCV 2022*** https://microdatos.dane.gov.co/index.php/catalog/793/get-microdata