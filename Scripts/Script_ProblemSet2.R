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
p_load("dplyr","httr","tidyverse","rvest","rio","skimr","caret","ggplot2","stargazer","readr","AER","MLmetrics","smotefamily")


# Importing Dataset -------------------------------------------------------

# Se importan los 4 archivos a usar

#Compu Betina
test_hogares <- read_csv("Downloads/uniandes-bdml-20231-ps2/test_hogares.csv")
train_hogares <- read_csv("Downloads/uniandes-bdml-20231-ps2/train_hogares.csv")
test_personas <- read_csv("Downloads/uniandes-bdml-20231-ps2/test_personas.csv")
train_personas <- read_csv("Downloads/uniandes-bdml-20231-ps2/train_personas.csv")

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

m_test <- merge(test_hogares, test_personas, by = "id")
m_train <- merge(train_hogares, train_personas, by = "id")
#rm(test_hogares, test_personas,train_hogares, train_personas)

length(unique(m_test$id))
length(unique(m_train$id))


#Como vamos dummificar, vamos a fusionar las dos bases de datos (training & test) para prevenir que se quede alguna
#categoría por fuera en alguna de las muestras, adicionalmente, vamos a crear nuevas variables, entonces lo ideal es
#que queden creadas en ambas bases de datos


#para la base de datos training, creamos una variable llamada "train" que vale 1, para la de test, la variable "train" vale 0,
#esto para discriminarlas una vez las fusionemos
#m_train$train <- rep(1, nrow(m_train))
#m_test$train <- rep(0, nrow(m_test))

#removemos las variables no comunes entre las bases de datos

# primero seleccionamos las variables de interés
#m_train <- subset(m_train, select = c("Clase.x","Fex_c.x","Lp","P5000","P6040","P6430","P6580","P7050","P7160","Clase.y","Fex_c.y","P5010","P6050","P6585s3","P6600","P6630s2","P7310","P7510s2","P7510s7","Depto.x","Fex_dpto.x","Nper","P5090","P6090","P6510","P7090","P7350","P7500s2","Depto.y","Fex_dpto.y","Ina","Npersug","P5100","P6100","P6585s1","P6610","P6630s3","P6800","P7110","P7422","P7510s3","Pet","Des","id","P5130","P6210","P6585s4","P6870","P7120","P7500s3","Dominio.x","Oc","P5140","P6210s1","P6545","P6620","P6630s4","P6920","P7472","P7510s5","train","Dominio.y","Oficio","P6240","P6585s2","P7040","P7505","Li","Orden","P6020","P6426","P6590","P6630s1","P6630s6","P7045","P7150","P7495","P7510s1","P7510s6"))

#fusionamos la base
#totaldata <- rbind(m_train, m_test)


#empezamos con la creación de variables

#dicotómica Cabecera = 1 si vive en una cabecera municipal, 0 si vive en la zona rural

m_test$ViveEnCabecera <- ifelse(m_test$Clase.x == 1, 1, 
                        ifelse(m_test$Clase.x == 2, 0, 0
                              ))

m_test <- m_test %>%
  mutate(ViveEnCabecera = factor(m_test$ViveEnCabecera, 
                        levels = c(0,1),
                        labels = c("No vive en Cabecera", "Vive en Cabecera")))



m_train$ViveEnCabecera <- ifelse(m_train$Clase.x == 1, 1, 
                          ifelse(m_train$Clase.x == 2, 0, 0
                          ))

m_train <- m_train %>%
  mutate(ViveEnCabecera = factor(m_train$ViveEnCabecera, 
                                 levels = c(0,1),
                                 labels = c("No vive en Cabecera", "Vive en Cabecera")))



# Dicotómica que muestra si el Jefe de hogar es mujer,  si = 1

m_test$JefeMujer <- ifelse(m_test$P6050 == 1 & m_test$P6020 == 2, 1, 0)
m_train$JefeMujer <- ifelse(m_train$P6050 == 1 & m_train$P6020 == 2, 1, 0)


m_test <- m_test %>%
  mutate(JefeMujer = factor(m_test$JefeMujer, 
                            levels = c(0,1),
                            labels = c("Jefe de hogar no es mujer", "Jefe de Hogar es Mujer")))
m_train <- m_train %>%
  mutate(JefeMujer = factor(m_train$JefeMujer, 
                            levels = c(0,1),
                            labels = c("Jefe de hogar no es mujer", "Jefe de Hogar es Mujer")))


# Relación personas/dormitorios

m_test$PersonaPorCuarto <-  m_test$Nper / m_test$P5010
m_train$PersonaPorCuarto <-  m_train$Nper / m_train$P5010

# Tipo de vivienda

m_test <- m_test %>%
  mutate(P5090 = factor(m_test$P5090, 
                        levels = c(1,2,3,4,5,6),
                        labels = c("Propia totalmente pagada", "Propia, la están pagando", "Arriendo o subarriendo", "Usufructo", "posesión sin título", "Otra")))


m_train <- m_train %>%
  mutate(P5090 = factor(m_train$P5090, 
                        levels = c(1,2,3,4,5,6),
                        labels = c("Propia totalmente pagada", "Propia, la están pagando", "Arriendo o subarriendo", "Usufructo", "posesión sin título", "Otra")))

m_test <- rename(m_test, TipoVivienda = P5090)
m_train <- rename(m_train, TipoVivienda = P5090)



# Regímen contributivo & Subsidiado

m_test$P6100[m_test$P6090 == 2 | m_test$P6090 == 9] <- 0
m_train$P6100[m_train$P6090 == 2 | m_train$P6090 == 9] <- 0

m_test$RegimenSalud <- ifelse(m_test$P6100 == 1 | m_test$P6100 == 2 , 1, 0)
m_train$RegimenSalud <- ifelse(m_train$P6100 == 1 | m_train$P6100 == 2 , 1, 0)

m_test <- m_test %>%
  mutate(RegimenSalud = factor(m_test$RegimenSalud, 
                            levels = c(0,1),
                            labels = c("No Pertenece al régimen contributivo o especial", "Pertenece al régimen contributivo o especial")))

m_train <- m_train %>%
  mutate(RegimenSalud = factor(m_train$RegimenSalud, 
                               levels = c(0,1),
                               labels = c("No Pertenece al régimen contributivo o especial", "Pertenece al régimen contributivo o especial")))


# Variable años de educación en función de max grado alcanzado

m_test$P6210[is.na(m_test$P6210)] <- 0 # los NA en teoría corresponden a personas que no reportan grado, los ajustamos a cero.
m_train$P6210[is.na(m_train$P6210)] <- 0


m_train$Añoseduc <- ifelse(m_train$P6210 == 3, 5, 
                        ifelse(m_train$P6210  == 4, 9, 
                               ifelse(m_train$P6210  == 5, 11, 
                                      ifelse(m_train$P6210  == 6, 16, 
                                             ifelse(m_train$P6210  == 9, 0, 0)))))

m_test$Añoseduc <- ifelse(m_test$P6210 == 3, 5, 
                           ifelse(m_test$P6210  == 4, 9, 
                                  ifelse(m_test$P6210  == 5, 11, 
                                         ifelse(m_test$P6210  == 6, 16, 
                                                ifelse(m_test$P6210  == 9, 0, 0)))))


missing_count <- colSums(is.na(m_train))
print(missing_count)

# Calcular la suma de los años de educación para cada hogar que tenga al menos una persona en edad de trabajar
#sum_añoseduc <- aggregate(m_train$Añoseduc[m_train$Pet == 1], by = list(m_train$id[m_train$Pet == 1]), sum)
#colnames(sum_añoseduc) <- c("id", "total_añoseduc") # Renombrar las columnas
#m_train <- merge(m_train, sum_añoseduc, by = "id")

#sum_añoseduc2 <- aggregate(m_test$Añoseduc[m_test$Pet == 1], by = list(m_test$id[m_test$Pet == 1]), sum)
#colnames(sum_añoseduc2) <- c("id", "total_añoseduc") # Renombrar las columnas
#m_test <- merge(m_test, sum_añoseduc2, by = "id")

#rm(sum_añoseduc, sum_añoseduc2)

# Creamos la nueva variable que cuenta las personas que están ocupadas en el hogar, y hallamos el porcentaje de personas
#ocupadas en el hogar

m_test$Oc[is.na(m_test$Oc)] <- 0
m_train$Oc[is.na(m_train$Oc)] <- 0

m_test <- m_test %>%
  group_by(id) %>%
  mutate(Suma_Ocu = sum(Oc))
m_test$PorcentajeOcupados <-  m_test$Suma_Ocu / m_test$Nper

m_train <- m_train %>%
  group_by(id) %>%
  mutate(Suma_Ocu = sum(Oc))
m_train$PorcentajeOcupados <-  m_train$Suma_Ocu / m_train$Nper


# Creamos la nueva variable que suma los años para cada valor repetido de id
m_test <- m_test %>%
  group_by(id) %>%
  mutate(suma_años = sum(Añoseduc))
m_test$EducaciónPromedio <-  m_test$suma_años / m_test$Nper

m_train <- m_train %>%
  group_by(id) %>%
  mutate(suma_años = sum(Añoseduc))
m_train$EducaciónPromedio <-  m_train$suma_años / m_train$Nper


length(unique(m_test$id))  #validamos que no hallamos perdido hogares en el proceso
length(unique(m_train$id)) #validamos que no hallamos perdido hogares en el proceso



# Renombramos variable P6426 a Antiguedad en el actual trabajo

m_train <- rename(m_train, AntiguedadTrabajo = P6426)
m_test <- rename(m_test, AntiguedadTrabajo = P6426)

m_train$AntiguedadTrabajo[m_train$Oc == 0] <- 0
m_test$AntiguedadTrabajo[m_test$Oc == 0] <- 0


# Volvemos la categórica P6430 "Tipo de trabajo" a dicótomas

m_train$P6430[m_train$Oc == 0] <- 0
m_test$P6430[m_test$Oc == 0] <- 0

m_test <- m_test %>%
  mutate(P6430 = factor(P6430, 
                            levels = c(0,1,2,3,4,5,6,7,8,9),
                            labels = c("No trabaja",
                                       "Empleado Empresa Particular",
                                       "Empleado del Gobierno",
                                       "Empleado Doméstico",
                                       "Cuenta Propia",
                                       "Empleador",
                                       "Trabajador Familiar sin Remuneración",
                                       "Trabajador sin remuneración en empresas",
                                       "Jornalero o Peón",
                                       "Otro")))

m_train <- m_train %>%
  mutate(P6430 = factor(P6430, 
                                levels = c(0,1,2,3,4,5,6,7,8,9),
                                labels = c("No trabaja",
                                           "Empleado Empresa Particular",
                                           "Empleado del Gobierno",
                                           "Empleado Doméstico",
                                           "Cuenta Propia",
                                           "Empleador",
                                           "Trabajador Familiar sin Remuneración",
                                           "Trabajador sin remuneración en empresas",
                                           "Jornalero o Peón",
                                           "Otro")))


m_train <- rename(m_train, TipoDeTrabajo = P6430)
m_test <- rename(m_test, TipoDeTrabajo = P6430)



## Ya tenemos todas las variables, las operaciones que provienen de personas las asignamos para todo el hogar, especificamente
##para el Jefe de Hogar, por lo tanto, procedemos a generar las data por hogar nuevamente.

m_train <- rename(m_train, JefeHogar = P6050)
m_test <- rename(m_test, JefeHogar = P6050)
m_train <- m_train %>% filter(JefeHogar == 1)
m_test <- m_test %>% filter(JefeHogar == 1)

train_final <-subset(m_train, select = c("PorcentajeOcupados","ViveEnCabecera","JefeMujer","PersonaPorCuarto","TipoVivienda","RegimenSalud","EducaciónPromedio","AntiguedadTrabajo","TipoDeTrabajo","Pobre","Lp","Ingtotugarr")) 
test_final <-subset(m_test, select = c("PorcentajeOcupados","ViveEnCabecera","JefeMujer","PersonaPorCuarto","TipoVivienda","RegimenSalud","EducaciónPromedio","AntiguedadTrabajo","TipoDeTrabajo")) 


# Identificamos los NA para las bases de datos
missing_count <- colSums(is.na(train_final))
print(missing_count) #solo tenemos un missing value en Regimen de Salud, no afecta nuestro poder estadistico

rm("m_test","m_train","test_hogares","test_personas","train_hogares","train_personas","missing_count")



# Estadísticas descriptivas -----------------------------------------------

#"amigos," a partir de aquí tienen dos bases de datos, train y test, hagan descriptivas a partir de la base de datos train_final
#Gracias.















# Classification Problem -------------------------------------------------------



set.seed(1234)

                            
# Hacemos gráfica para comparar cuantos pobres y no pobres hay, se puede observar que la base de entrenamiento es
#desbalanceada pues hay muchos menos pobres que no pobres
ggplot(train_hogares, aes(x = Pobre))+ 
  geom_bar(fill = "darkblue")+
  theme_bw()+
  labs(x= "", y = "Frecuencia", title = "el hogar es pobre")


# nos muestra que la muestra es desbalanceada, tenemos que balancearla pues 80% son no pobres y 20% pobres, si la dejamos
#como está, nuestros modelos tendrán sesgo y tenderán a predecir que todos son "no pobres" pues es la categoría predominante
prop.table(table(train_hogares$Pobre))

glimpse(train_hogares) # Nos muestra los primeros datos de las variables, esto nos sirve para identificar las categóricas
                       #aunque  lo podemos hacer con  el diccionario


