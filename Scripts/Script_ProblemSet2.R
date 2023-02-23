################################################################
# Problem Set 2: Predicting Poverty
# Authors: Yilmer Palacios, Betina Cortés, Lida Jimena Rivera,
# Nelson Fabián López
################################################################


# Loading Libraries -------------------------------------------------------

rm(list = ls()) 

#install.packages("pacman")
require("pacman")
p_load("tidyverse","rvest","rio","skimr","caret","ggplot2","stargazer","readr")

# Importing Dataset -------------------------------------------------------

# Se importan los 4 archivos a usar

test_hogares <- read_csv("Downloads/uniandes-bdml-20231-ps2/test_hogares.csv")
train_hogares <- read_csv("Downloads/uniandes-bdml-20231-ps2/train_hogares.csv")
test_personas <- read_csv("Downloads/uniandes-bdml-20231-ps2/test_personas.csv")
train_personas <- read_csv("Downloads/uniandes-bdml-20231-ps2/train_personas.csv")

# Los datos se guardan como un archivo binario R (rds) usando saveRDS()
# para hacer más eficiente la carga cuando sea necesario

saveRDS(GEIH, file = "GEIH1.rds")
GEIH<-readRDS("GEIH1.Rds")