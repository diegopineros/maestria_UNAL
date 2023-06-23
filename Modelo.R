##############################################################################'#
################################  Paquetes  ####################################
##############################################################################'#

library(readr)
library(nnet)
library(tidyverse)

# library(data.table)
# library(foreign)
# library(nnet)
# library(ggplot2)
# library(reshape2)

##############################################################################'#
#############################  Carga de datos  #################################
##############################################################################'#
i <- 2022
#for (i in c(2018, 2020, 2022)){
base <- read_delim(paste('base_paper',i,".csv", sep = ''), delim = ";",
                   escape_double = FALSE, trim_ws = TRUE)

##############################################################################'#
##########################  Ajustes preliminares  ###############################
##############################################################################'#

base$Y <- as.factor(base$Y)
base$Y2 <- relevel(base$Y, ref = "Leña")
test <- multinom(Y2~Energia+Gas_red+ln_PERCAPITA+N_hogar+Jefe_sex+Jefe_edad+menores+Jefe_educ_sec,data=base, model = TRUE)
summary(test)

##### Test de WOLF
z <- summary(test)$coefficients/summary(test)$standard.errors
z
 
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

## extract the coefficients from the model and exponentiate
exp(coef(test))
head(pp <- fitted(test))

# dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
# predict(test, newdata = dses, "probs")
# 
# dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), write = rep(c(30:70),
#                                                                                    3))
# ## store the predicted probabilities for each value of ses and write
# pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))
# 
# ## calculate the mean probabilities within each level of ses
# by(pp.write[, 3:5], pp.write$ses, colMeans)
# 
# ## melt data set to long for ggplot2
# lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
# head(lpp)  # view first few rows
# 
# ## plot predicted probabilities across write values for each level of ses
# ## facetted by program type
# ggplot(lpp, aes(x = write, y = probability, colour = ses)) + geom_line() + facet_grid(variable ~
#                                                                                         ., scales = "free")


#}
################################################################################ =======
## >>>>>>> 2840dd3cf0d81498adf2fbb8140caae538a20fd5
