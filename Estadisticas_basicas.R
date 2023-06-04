##############################################################################'#
################################  Paquetes  ####################################
##############################################################################'#

library(readr)
library(tidyverse)
library(data.table)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(haven)

##############################################################################'#
#############################  Carga de datos  #################################
##############################################################################'#

base_2018 <- read_delim("base_paper2018.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
#base_2019 <- read_delim("base_paper2019.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
base_2020 <- read_delim("base_paper2020.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
#base_2021 <- read_delim("base_paper2021.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
base_2022 <- read_delim("base_paper2022.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

unique(base_2018$P1_DEPARTAMENTO)
#unique(base_2019$P1_DEPARTAMENTO)
unique(base_2020$P1_DEPARTAMENTO)
#unique(base_2021$P1_DEPARTAMENTO)
unique(base_2022$P1_DEPARTAMENTO)