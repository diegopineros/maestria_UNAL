##############################################################################'#
################################  Paquetes  ################################,####
##############################################################################'#

library(tidyverse)
library(data.table)
library(haven)

##############################################################################'#
#############################  Procesamiento  ##################################
##############################################################################'#

for (i in c(2018,2019,2021)){
  
  print(i)
  base_poblacion  <- read_dta(paste(getwd(),'Data',i,'Caracteristicas y composicion del hogar.dta', sep='/'))
  base_vivienda <- read_dta(paste(getwd(),'Data',i,"Datos de la vivienda.dta", sep='/'))
  base_educ <- read_dta(paste(getwd(),'Data',i,"Educación.dta", sep='/'))
  base_hogar <- read_dta(paste(getwd(),'Data',i,"Servicios del hogar.dta", sep='/'))
  
# P1_DEPARTAMENTO
  if (i == 2018) {
    base_vivienda$P1_DEPARTAMENTO <- base_vivienda$DPTO
  }
  
  if (i == 2021) {
    base_poblacion$DIRECTORIO <- base_poblacion$directorio
    base_poblacion$SECUENCIA_P <- base_poblacion$secuencia_p
    base_poblacion$SECUENCIA_ENCUESTA <- base_poblacion$secuencia_encuesta
    base_poblacion$FEX_C <- base_poblacion$fex_c
    base_poblacion$P6020 <- base_poblacion$p6020
    base_poblacion$P6040 <- base_poblacion$p6040
    base_poblacion$P6051 <- base_poblacion$p6051
    base_vivienda$DIRECTORIO <- base_vivienda$directorio
    base_vivienda$SECUENCIA_ENCUESTA <- base_vivienda$secuencia_encuesta
    base_vivienda$ORDEN <- base_vivienda$orden
    base_vivienda$P1_DEPARTAMENTO <- base_vivienda$p1_departamento
    base_vivienda$CLASE <- base_vivienda$clase
    base_vivienda$P8520S1 <- base_vivienda$p8520s1
    base_vivienda$FEX_C <- base_vivienda$fex_c
    base_educ$DIRECTORIO <- base_educ$directorio
    base_educ$SECUENCIA_ENCUESTA <- base_educ$secuencia_encuesta
    base_educ$SECUENCIA_P <- base_educ$secuencia_p
    base_educ$P8587 <- base_educ$p8587
    base_educ$P8587S1 <- base_educ$p8587s1
    base_hogar$DIRECTORIO <- base_hogar$directorio
    base_hogar$SECUENCIA_P <- base_hogar$secuencia_p
    base_hogar$SECUENCIA_ENCUESTA <- base_hogar$secuencia_encuesta
    base_hogar$ORDEN <- base_hogar$orden
    base_hogar$P5666 <- base_hogar$p5666
    base_hogar$P8536 <- base_hogar$p8536
    base_hogar$P1698 <- base_hogar$p1698
    base_hogar$FEX_C <- base_hogar$fex_c
    base_hogar$P1698S1 <- base_hogar$p1698s1
    base_hogar$I_HOGAR <- base_hogar$i_hogar
    base_hogar$I_UGASTO <- base_hogar$i_ugasto
    base_hogar$PERCAPITA <- base_hogar$percapita
    base_hogar$I_OU <- base_hogar$i_ou
    base_hogar$CANT_PERSONAS_HOGAR <- base_hogar$cant_personas_hogar
  }
  
  fila_viv <- base_vivienda %>% select(DIRECTORIO,SECUENCIA_ENCUESTA,ORDEN,P1_DEPARTAMENTO,CLASE, P8520S1,FEX_C)
  fla_hog <- base_hogar %>% select(DIRECTORIO,SECUENCIA_P,ORDEN,P5666,P8536,P1698,P1698S1,#FEX_C 
                                   I_HOGAR,I_UGASTO, PERCAPITA,I_OU,CANT_PERSONAS_HOGAR)
  fla_hog$SECUENCIA_ENCUESTA <- fla_hog$SECUENCIA_P
  
  fla_pobla <- base_poblacion %>% select(DIRECTORIO,SECUENCIA_P, FEX_C, P6020, P6040, P6051)
  fla_pobla$SECUENCIA_ENCUESTA <- fla_pobla$SECUENCIA_P
  
  base_combustible <- inner_join(fila_viv,fla_hog, by= c("DIRECTORIO", "SECUENCIA_ENCUESTA")) -> alterna
  base_combustible <- inner_join(base_combustible, fla_pobla, by= c("DIRECTORIO", "SECUENCIA_ENCUESTA"))
  
  head(base_poblacion)
  
  
  base_res <- base_combustible %>% filter(P6020==2 & P6051==1) %>% 
    group_by(DIRECTORIO, SECUENCIA_P.x) %>% summarise(expansion=sum(FEX_C.x))
  
  base_res <- merge(base_res, alterna, by.x = c("DIRECTORIO", "SECUENCIA_P.x"), 
                    by.y = c("DIRECTORIO", "SECUENCIA_P"))
  
  table(base_res$P8536,base_res$P1698S1)
  
  table(base_res$P8536)
  table(base_res$P1698s1)
  
  base_res %>% group_by(P8536, CLASE) %>% summarise(exponencial=sum(expansion))

  #####################################################################################################c#
  ####################################### exportar resultados ###########################################
  #####################################################################################################c#
  
  
  #write.table(base_res,"./resultado_cbeza_mujeres.csv",sep = ";",dec=".",row.names = false)
  
  #################################################################################}
  
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
#
##############################################################################'
##############################################################################'
##############################################################################'
##############################################################################'
