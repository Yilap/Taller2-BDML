################################################################
# Problem Set 2: Predicting Poverty
# Authors: Yilmer Palacios, Betina Cortés, Lida Jimena Rivera,
# Nelson Fabián López
################################################################


# Loading Libraries -------------------------------------------------------

rm(list = ls()) 

#install.packages("pacman")
#install.packages("httr")


library("pacman") # para cargar paquetes
p_load("httr","tidyverse","rvest","rio","skimr","caret","ggplot2","stargazer","readr","AER","MLmetrics","smotefamily")


# Importing Dataset -------------------------------------------------------

# Se importan los 4 archivos a usar

#Compu Betina
#test_hogares <- read_csv("Downloads/uniandes-bdml-20231-ps2/test_hogares.csv")
#train_hogares <- read_csv("Downloads/uniandes-bdml-20231-ps2/train_hogares.csv")
#test_personas <- read_csv("Downloads/uniandes-bdml-20231-ps2/test_personas.csv")
#train_personas <- read_csv("Downloads/uniandes-bdml-20231-ps2/train_personas.csv")

#Compu Yilmer
test_hogares <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/test_hogares.csv")
train_hogares <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/train_hogares.csv")
test_personas <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/test_personas.csv")
train_personas <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/train_personas.csv")
# sample_sub <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/sample_submission.csv")


# Los datos se guardan como un archivo binario R (rds) usando saveRDS()
# para hacer más eficiente la carga cuando sea necesario

#saveRDS(GEIH, file = "GEIH1.rds")
#GEIH<-readRDS("GEIH1.Rds")

#hacemos los merged 

#m_test <- merge(test_hogares, test_personas, by = "id")
#m_train <- merge(train_hogares, train_personas, by = "id")
#rm(test_hogares, test_personas,train_hogares, train_personas)



# Classification Problem -------------------------------------------------------

set.seed(1234)

train_hogares <- train_hogares %>%
  mutate(Pobre = factor(train_hogares$Pobre, 
                          levels = c(0, 1),
                          labels = c("No pobre", "Pobre")))
                            
# Hacemos gráfica para comparar cuantos pobres y no pobres hay, se puede observar que la base de entrenamiento es
#desbalanceada pues hay muchos menos pobres que no pobres
ggplot(train_hogares, aes(x = Pobre))+ 
  geom_bar(fill = "darkblue")+
  theme_bw()+
  labs(x= "", y = "Frecuencia", title = "el hogar es pobre")


# nos muestra que la muestra es desbalanceada, tenemos que balancearla pues 80% son no pobres y 20% pobres, si la dejamos
#como está, nuestros modelos tendrán sesgo y tenderán a predecir que todos son "no pobres" pues es la categoría predominante
prop.table(table(train_hogares$Pobre))

glimpse(train_hogares)





