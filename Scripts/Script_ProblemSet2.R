################################################################
# Problem Set 2: Predicting Poverty
# Authors: Yilmer Palacios, Betina Cortés, Lida Jimena Rivera,
# Nelson Fabián López
################################################################


# Loading Libraries -------------------------------------------------------

rm(list = ls()) 

#install.packages("pacman")
#install.packages("httr")


require("pacman")
p_load("httr","tidyverse","rvest","rio","skimr","caret","ggplot2","stargazer","readr")


# Importing Dataset -------------------------------------------------------

# Se importan los 4 archivos a usar

#test_hogares <- read_csv("Downloads/uniandes-bdml-20231-ps2/test_hogares.csv")
#train_hogares <- read_csv("Downloads/uniandes-bdml-20231-ps2/train_hogares.csv")
#test_personas <- read_csv("Downloads/uniandes-bdml-20231-ps2/test_personas.csv")
#train_personas <- read_csv("Downloads/uniandes-bdml-20231-ps2/train_personas.csv")

#Compu Yilmer
test_hogares <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/test_hogares.csv")
train_hogares <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/train_hogares.csv")
test_personas <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/test_personas.csv")
train_personas <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/train_personas.csv")
#sample_sub <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/sample_submission.csv")


# Los datos se guardan como un archivo binario R (rds) usando saveRDS()
# para hacer más eficiente la carga cuando sea necesario

#saveRDS(GEIH, file = "GEIH1.rds")
#GEIH<-readRDS("GEIH1.Rds")

#hacemos los merged 

m_test <- merge(test_hogares, test_personas, by = "id")
merged_train <- merge(train_hogares, train_personas, by = "id")
rm(test_hogares, test_personas,train_hogares, train_personas)



# Classification Problem -------------------------------------------------------

set.seed(1011)



