##############################################################################'#
################################  Paquetes  ################################,####
##############################################################################'#

library(tidyverse)
library(data.table)
library(haven)

##############################################################################'#
#############################  Procesamiento  ##################################
##############################################################################'#

for (i in c(2018, 2020, 2022)){#2018, 2019, 2020, 2021, 2022
  
  print(i)
  ##############################################################################'#
  #########################  Cargue bases de datos  ##############################
  ##############################################################################'#
  
  base_poblacion  <- read_dta(paste(getwd(),'Data',i,'Caracteristicas y composicion del hogar.dta', sep='/'))
  base_vivienda <- read_dta(paste(getwd(),'Data',i,"Datos de la vivienda.dta", sep='/'))
  base_educ <- read_dta(paste(getwd(),'Data',i,"Educacion.dta", sep='/'))
  base_hogar <- read_dta(paste(getwd(),'Data',i,"Servicios del hogar.dta", sep='/'))
  
  # P1_DEPARTAMENTO
  if (i == 2018) {
    base_vivienda$P1_DEPARTAMENTO <- base_vivienda$DPTO
  }
  
  ##############################################################################'#
  ###########################  Cargue de hogares  ################################
  ##############################################################################'#
  
  fila_viv <- base_vivienda %>% select(DIRECTORIO,ORDEN,
                                       P1_DEPARTAMENTO,CLASE,P1070,P8520S1)#,FEX_C
  
  fla_hog <- base_hogar %>% select(DIRECTORIO,SECUENCIA_ENCUESTA,P5666,
                                   P8536,P1698,P1698S1,I_HOGAR,I_UGASTO,PERCAPITA,
                                   I_OU,CANT_PERSONAS_HOGAR)
  

  
  ##############################################################################'#
  ##########################  Agrupación hogares  ###############################
  ##############################################################################'#
  
  base_combustible <- inner_join(fila_viv,fla_hog, by= c("DIRECTORIO"))
  base_combustible <- base_combustible %>% filter(CLASE==2)
  
  ##############################################################################'#
  ############################  Cargue personas  #################################
  ##############################################################################'#
  
  educ <- base_educ %>% select(DIRECTORIO,SECUENCIA_ENCUESTA,SECUENCIA_P,
                               P8587,P8587S1)
  pobl <- base_poblacion %>% select(DIRECTORIO,SECUENCIA_ENCUESTA,SECUENCIA_P,
                                    P6020,P6040,P6051)
  
  base_pop <- inner_join(x = educ,y = pobl,by=c("DIRECTORIO","SECUENCIA_ENCUESTA",
                                                "SECUENCIA_P"))
  
  base_pop <- base_pop %>% group_by(DIRECTORIO,SECUENCIA_P) %>% mutate(avg_edad=mean(P6040))
  base_pop$Jefe_educ_sec <- 0
  base_pop$Jefe_educ_ter <- 0
  base_pop$menores <- 0
  base_pop$Jefe_edad <- 0
  base_pop$Jefe_sex <- 0
  base_pop$personas <-1

  base_pop <- as.data.table(base_pop)
  base_pop <- base_pop[P6051==1,Jefe_sex:=P6020]
  base_pop <- base_pop[P6051==1,Jefe_edad:=P6040]
  base_pop <- base_pop[P6040<=18,menores:=1]
  base_pop <- base_pop[P8587==4|P8587==5&P6051==1,Jefe_educ_sec:=1]
  base_pop <- base_pop[P8587>=6&P6051==1,Jefe_educ_ter:=1]
  
  ##############################################################################'#
  ##########################  Agrupacion personas  ###############################
  ##############################################################################'#
  
  base_pop_clear <- base_pop %>% group_by(DIRECTORIO,SECUENCIA_P) %>% 
    summarise(Jefe_sex=max(Jefe_sex),Jefe_edad=max(Jefe_edad),menores=sum(menores),
              Jefe_educ_sec=max(Jefe_educ_sec),Jefe_educ_ter=max(Jefe_educ_ter),
              avg_edad=mean(avg_edad), personas=sum(personas))
  
  base_pop_clear$SECUENCIA_ENCUESTA <- base_pop_clear$SECUENCIA_P
  
  base_pop_clear <- base_pop_clear %>% select(-SECUENCIA_P)
  
  base <- inner_join(base_combustible,base_pop_clear,by=c("DIRECTORIO","SECUENCIA_ENCUESTA"))

  ##############################################################################'#
  ######################  Ajuste nombres de variables  ###########################
  ##############################################################################'#
  rename(base_combustible,Gas_red=P5666,Y=P8536,Alt_Y=P1698,N_hogar=CANT_PERSONAS_HOGAR,
         Tipo_viv=P1070,Energia=P8520S1)
  
  
  ##############################################################################'#
  ##########################  Export datos modelo  ###############################
  ##############################################################################'#
  
  write.table(base,paste("./base_paper",i,'.csv',sep=''),sep = ";",dec=".",row.names=FALSE)
  print(paste("./base_paper",i,'.csv',sep=''))
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