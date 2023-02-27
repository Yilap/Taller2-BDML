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
p_load("GGally","psych","rpart.plot","ROCR","gamlr","modelsummary","gtsummary","naniar","PerformanceAnalytics","pastecs",
       "writexl","dplyr","httr","tidyverse","rvest","rio","skimr","caret","ggplot2","stargazer",
       "readr","AER","MLmetrics","smotefamily","pROC","smotefamily","rpart","randomForest","rpart", "Metrics",
       "rattle")

# Importing Dataset -------------------------------------------------------

# Se importan los 4 archivos a usar

test_hogares <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/test_hogares.csv")
train_hogares <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/train_hogares.csv")
test_personas <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/test_personas.csv")
train_personas <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/train_personas.csv")
sample_sub <- read_csv("C:/Users/Yilmer Palacios/Desktop/BaseDatosT2/sample_submission.csv")

# Unimos la base de datos de personas y hogares con merge usando el id del hogar
m_test <- merge(test_hogares, test_personas, by = "id")
m_train <- merge(train_hogares, train_personas, by = "id")
#rm(test_hogares, test_personas,train_hogares, train_personas)

length(unique(m_test$id))
length(unique(m_train$id))


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



# Regimen contributivo & Subsidiado

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

# Renombramos la variable Ingreso per cápita
m_train <- rename(m_train, IngresoPerCapita = Ingpcug)

## Ya tenemos todas las variables, las operaciones que provienen de personas las asignamos para todo el hogar, especificamente
##para el Jefe de Hogar, por lo tanto, procedemos a generar las data por hogar nuevamente.

m_train <- rename(m_train, JefeHogar = P6050)
m_test <- rename(m_test, JefeHogar = P6050)
m_train <- m_train %>% filter(JefeHogar == 1)
m_test <- m_test %>% filter(JefeHogar == 1)

train_final <-subset(m_train, select = c("id","PorcentajeOcupados","ViveEnCabecera","JefeMujer","PersonaPorCuarto","TipoVivienda","RegimenSalud","EducaciónPromedio","AntiguedadTrabajo","TipoDeTrabajo","Pobre","Lp","IngresoPerCapita")) 
test_final <-subset(m_test, select = c("id","PorcentajeOcupados","ViveEnCabecera","JefeMujer","PersonaPorCuarto","TipoVivienda","RegimenSalud","EducaciónPromedio","AntiguedadTrabajo","TipoDeTrabajo","Lp")) 


# Identificamos los NA para las bases de datos
missing_count <- colSums(is.na(train_final))
print(missing_count) #solo tenemos un missing value en Regimen de Salud, lo eliminamos pues no afecta nuestro poder estadistico

train_final$RegimenSalud <- ifelse(is.na(train_final$RegimenSalud), 0, train_final$RegimenSalud)
test_final$RegimenSalud <- ifelse(is.na(test_final$RegimenSalud), 0, test_final$RegimenSalud)

missing_count <- colSums(is.na(train_final))
missing_count2 <- colSums(is.na(test_final))
print(missing_count) #Train_final sin NA
print(missing_count2) #Test_final sin NA


rm("m_test","m_train","test_hogares","test_personas","train_hogares","train_personas","missing_count","missing_count2")

# Grabamos las bases de datos finales
save(train_final, file = "train_final.RData")
save(test_final, file = "test_final.RData")

#Cargamos las bases de datos limpias
load("train_final.RData")
load("test_final.RData")

# Estadísticas descriptivas -----------------------------------------------

### Estadisticas descriptivas base de training ###


library(psych) # cargar paquete psych para usar la función describe()

# Seleccionar variables categóricas y numéricas de train_final
train_final_desc <- train_final[, c("ViveEnCabecera", "JefeMujer", "PersonaPorCuarto", "TipoVivienda", "RegimenSalud",
                                    "TipoDeTrabajo", "Pobre", "PorcentajeOcupados", "AntiguedadTrabajo", "Lp",
                                    "IngresoPerCapita", "EducaciónPromedio")]

# Calcular estadísticas descriptivas con la función describe()
describe(train_final_desc)


library(ggplot2)

# Gráfico de barras para variables categóricas
ggplot(train_final, aes(x = ViveEnCabecera)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de ViveEnCabecera")

ggplot(train_final, aes(x = JefeMujer)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de JefeMujer")

ggplot(train_final, aes(x = TipoVivienda)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de TipoVivienda")

ggplot(train_final, aes(x = RegimenSalud)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de RegimenSalud")

ggplot(train_final, aes(x = TipoDeTrabajo)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de TipoDeTrabajo")

ggplot(train_final, aes(x = Pobre)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de Pobre")

# Histograma para variables numéricas
ggplot(train_final, aes(x = PorcentajeOcupados)) +
  geom_histogram(fill = "steelblue", bins = 20) +
  labs(title = "Distribución de PorcentajeOcupados")

ggplot(train_final, aes(x = AntiguedadTrabajo)) +
  geom_histogram(fill = "steelblue", bins = 20) +
  labs(title = "Distribución de AntiguedadTrabajo")

ggplot(train_final, aes(x = Lp)) +
  geom_histogram(fill = "steelblue", bins = 20) +
  labs(title = "Distribución de Lp")

ggplot(train_final, aes(x = IngresoPerCapita)) +
  geom_histogram(fill = "steelblue", bins = 20) +
  labs(title = "Distribución de IngresoPerCapita")

ggplot(train_final, aes(x = EducaciónPromedio)) +
  geom_histogram(fill = "steelblue", bins = 20) +
  labs(title = "Distribución de EducaciónPromedio")

# Boxplot para variables numéricas
ggplot(train_final, aes(y = PersonaPorCuarto)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot de PersonaPorCuarto")

# Gráfico de densidad para variables numéricas
ggplot(train_final, aes(x = PersonaPorCuarto)) +
  geom_density(fill = "steelblue") +
  labs(title = "Distribución de PersonaPorCuarto")


# Clasificación -----------------------------------------------------------

# Hacemos gráfica para comparar cuantos hogares pobres y no pobres hay: 
ggplot(train_hogares, aes(x = factor(Pobre))) + 
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(x= "", y = "Frecuencia", title = "Pobreza en los hogares") +
  scale_x_discrete(labels = c("Hogar no pobre", "Hogar pobre"))

# Se observa que la base de entrenamiento es desbalanceada, pues hay muchos menos hogares pobres que no pobres

prop.table(table(train_hogares$Pobre))

# El 80% de los hogares no son pobres y 20% sí lo son. Esto ocasiona que los modelos tengan sesgo y tenderán 
# a predecir que todos los hogares son "no pobres", pues es la categoría predominante

dbtrain <- train_final
dbtest <- test_final

# Verificación de que la variable Pobre es una dicótoma 
dbtrain$Pobre <- factor(dbtrain$Pobre, levels = c(0,1), labels = c("No", "Si"))
levels(dbtrain$Pobre)

glimpse(dbtrain)
glimpse(dbtest)
prop.table(table(dbtrain$Pobre))


# Set seed para asegurar replicabilidad del ejercicio
set.seed(101)

# Para empezar, se realiza la partición de la base de datos train en tres partes (training, test y evaluación)
# Esta división será útil para el cálculo del ROC (Receiver Operating Characteristic), el cual servirá 
# para evaluar la calidad del modelo de clasificación binario, pues presenta la relación entre la tasa de verdaderos 
# positivos (TPR) y la tasa de falsos positivos (FPR) a diferentes umbrales de clasificación.

split1 <- createDataPartition(dbtrain$Pobre , p = 0.7)[[1]]
length(split1)
training = dbtrain[split1,]
other <- dbtrain[-split1,]

split2 <- createDataPartition(other$Pobre , p = 1/3)[[1]]
evaluation <- other[split2,]
testing <- other[-split2,]

dim(training)
dim(testing)
dim(evaluation)

# Se valida la partición realizada
prop.table(table(training$Pobre))
prop.table(table(testing$Pobre))
prop.table(table(evaluation$Pobre))

predict <- stats::predict

# Predicción

# Se evaluarán tres especificaciones, presentadas a continuación:

M1 <- as.formula("Pobre ~ PorcentajeOcupados + JefeMujer + PersonaPorCuarto + TipoVivienda + ViveEnCabecera + TipoDeTrabajo + RegimenSalud + EducaciónPromedio + AntiguedadTrabajo")
M2 <- as.formula("Pobre ~ PorcentajeOcupados + JefeMujer + PersonaPorCuarto + TipoVivienda + TipoDeTrabajo + RegimenSalud + EducaciónPromedio")
M3 <- as.formula("Pobre ~ PorcentajeOcupados + JefeMujer + PersonaPorCuarto + TipoVivienda + RegimenSalud + EducaciónPromedio")


# Primero, se realiza a manera de control un 5-fold cross-validation
# Se hace Model Tuning para maximizar la capacidad predictiva del modelo. Para contrarrestar los efectos negativos del desequilibrio de clases se ajusta el modelo para maximizar 
# la precisión de las clases minoritarias, es decir, los hogares pobres. En este caso, ajustar el modelo para maximizar la sensibilidad puede ayudar a desensibilizar el proceso 
# de entrenamiento al alto porcentaje de datos "no pobres" en el conjunto de entrenamiento.

ffcv<-function(...)c(twoClassSummary(...), defaultSummary(...))

Control <- trainControl(method = "cv",
                        number = 5,
                        summaryFunction = ffcv,
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

# Ajustar el modelo de regresión logística para el M1
logitM1 <- train(
  M1,
  data = training,
  method = "glm",
  trControl = Control,
  family = "binomial",
  preProcess = c("center", "scale")
)

logitM1

# Ajustar el modelo de regresión logística para el M2
logitM2 <- train(
  M2,
  data = training,
  method = "glm",
  trControl = Control,
  family = "binomial",
  preProcess = c("center", "scale")
)

logitM2

# Ajustar el modelo de regresión logística para el M3
logitM3 <- train(
  M3,
  data = training,
  method = "glm",
  trControl = Control,
  family = "binomial",
  preProcess = c("center", "scale")
)

logitM3

# Lasso para M1 
grid <- 10^seq(-4, 0.01, length = 200)

lasso1 <- train(
  M1,
  data = training,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=grid),
  preProcess = c("center", "scale")
)

lasso1 

# Lasso para M2
lasso2 <- train(
  M2,
  data = training,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=grid),
  preProcess = c("center", "scale")
)

lasso2

# Lasso para M3
lasso3 <- train(
  M3,
  data = training,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=grid),
  preProcess = c("center", "scale")
)

lasso3

# Lasso - up sampling

upSampledTrain <- upSample(x = training,
                           y = training$Pobre,
                           yname = "Pobre")
dim(training)

dim(upSampledTrain)

# Se observa que se balancean hacia arriba 
table(upSampledTrain$Pobre)

# Lasso - up sampling para M1
lasso_upsample1 <- train(
  M1,
  data = upSampledTrain,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=grid),
  preProcess = c("center", "scale")
)

lasso_upsample1

# Lasso - up sampling para M2
lasso_upsample2 <- train(
  M2,
  data = upSampledTrain,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=grid),
  preProcess = c("center", "scale")
)

lasso_upsample2

# Lasso - up sampling para M3
lasso_upsample3 <- train(
  M3,
  data = upSampledTrain,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=grid),
  preProcess = c("center", "scale")
)

lasso_upsample3

# Lasso - down sampling

downSampledTrain <- downSample(x = training,
                               y = training$Pobre,
                               yname = "Pobre")

# Se observa que los datos se redondean hacia abajo
table(downSampledTrain$Pobre)

# Lasso - down sampling para M1
lasso_downsample1 <- train(
  M1,
  data = downSampledTrain,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=grid),
  preProcess = c("center", "scale")
)

lasso_downsample1

# Lasso - down sampling para M2
lasso_downsample2 <- train(
  M2,
  data = downSampledTrain,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=grid),
  preProcess = c("center", "scale")
)

lasso_downsample2

# Lasso - down sampling para M3
lasso_downsample3 <- train(
  M3,
  data = downSampledTrain,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0,lambda=grid),
  preProcess = c("center", "scale")
)

lasso_downsample3

# Logit - Elastic Net

# Elastic Net para M1
elasticnet1 <- train(
  M1,
  data = training,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Acc",
  preProcess = c("center", "scale")
)

elasticnet1

# Elastic Net para M2
elasticnet2<- train(
  M2,
  data = training,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  preProcess = c("center", "scale")
)

elasticnet2

# Elastic Net para M3
elasticnet3<- train(
  M3,
  data = training,
  method = "glmnet",
  trControl = Control,
  family = "binomial",
  metric = "Sens",
  preProcess = c("center", "scale")
)

elasticnet3

# Lasso - ROC

# Lasso - ROC para M1
lasso_roc1 <- train(
  M1, 
  data = training, 
  method = "glmnet",
  trControl = Control,
  family = "binomial", 
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=grid), 
  preProcess = c("center", "scale")
)
lasso_roc1

# Lasso - ROC para M2
lasso_roc2 <- train(
  M2, 
  data = training, 
  method = "glmnet",
  trControl = Control,
  family = "binomial", 
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=grid), 
  preProcess = c("center", "scale")
)
lasso_roc2

# Lasso - ROC para M3
lasso_roc3 <- train(
  M3, 
  data = training, 
  method = "glmnet",
  trControl = Control,
  family = "binomial", 
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0,lambda=grid), 
  preProcess = c("center", "scale")
)
lasso_roc3

# Alternative Cutoffs
p_load("pROC")

# Para M1 (lasso_roc1) #
evalResults <- data.frame(Pobre = evaluation$Pobre)
evalResults$Roc1 <- predict(lasso_roc1,
                            newdata = evaluation,
                            type = "prob")[,1]
head(evalResults)

#Se calcula el ROC para el M1
rf_ROC1 <- roc(evalResults$Pobre, evalResults$Roc1, levels = rev(levels(evalResults$Pobre)))
rf_ROC1

#Se calcula el Cutoff
rf_Thresh1 <- coords(rf_ROC1, x = "best", best.method = "closest.topleft")
rf_Thresh1

#Se evalúan los resultados
evalResults<-evalResults %>% mutate(lasso1hat05=ifelse(evalResults$Roc>0.5,"Si","No"),
                                    lasso1rf_Thresh=ifelse(evalResults$Roc>rf_Thresh1$threshold,"Si","No"))

# Caso en el que el threshold es = 0.5 (Bayes)
with(evalResults,table(Pobre,lasso1hat05))

# Caso en el que el threshold se obtiene del ROC
with(evalResults,table(Pobre,lasso1rf_Thresh))

# Para M2 (lasso_roc2) #
evalResults <- data.frame(Pobre = evaluation$Pobre)
evalResults$Roc2 <- predict(lasso_roc2,
                            newdata = evaluation,
                            type = "prob")[,1]
head(evalResults)

#Se calcula el ROC para el M2
rf_ROC2 <- roc(evalResults$Pobre, evalResults$Roc2, levels = rev(levels(evalResults$Pobre)))
rf_ROC2

#Se calcula el Cutoff
rf_Thresh2 <- coords(rf_ROC2, x = "best", best.method = "closest.topleft")
rf_Thresh2

#Se evalúan los resultados
evalResults<-evalResults %>% mutate(lasso2hat05=ifelse(evalResults$Roc>0.5,"Si","No"),
                                    lasso2rf_Thresh=ifelse(evalResults$Roc>rf_Thresh2$threshold,"Si","No"))

# Caso en el que el threshold es = 0.5 (Bayes)
with(evalResults,table(Pobre,lasso2hat05))

# Caso en el que el threshold se obtiene del ROC
with(evalResults,table(Pobre,lasso2rf_Thresh))

# Para M3 (lasso_roc3) #
evalResults <- data.frame(Pobre = evaluation$Pobre)
evalResults$Roc3 <- predict(lasso_roc3,
                            newdata = evaluation,
                            type = "prob")[,1]
head(evalResults)

#Se calcula el ROC para el M3
rf_ROC3 <- roc(evalResults$Pobre, evalResults$Roc3, levels = rev(levels(evalResults$Pobre)))
rf_ROC3

#Se calcula el Cutoff
rf_Thresh3 <- coords(rf_ROC3, x = "best", best.method = "closest.topleft")
rf_Thresh3

#Se evalúan los resultados
evalResults<-evalResults %>% mutate(lasso3hat05=ifelse(evalResults$Roc>0.5,"Si","No"),
                                    lasso3rf_Thresh=ifelse(evalResults$Roc>rf_Thresh3$threshold,"Si","No"))

# Caso en el que el threshold es = 0.5 (Bayes)
with(evalResults,table(Pobre,lasso3hat05))

# Caso en el que el threshold se obtiene del ROC
with(evalResults,table(Pobre,lasso3rf_Thresh))
head(evalResults)

# Presentar los resultados por modelo (M1, M2 y M3) y por enfoque

# Para el modelo M1
testResults <- data.frame(Pobre = testing$Pobre)
testResults

testResults$logitM1<- predict(logitM1,
                              newdata = testing, 
                              type = "prob")[,1]

testResults$lasso1<- predict(lasso1,
                             newdata = testing,
                             type = "prob")[,1]

testResults$lasso_downsample1 <- predict(lasso_downsample1,
                                         newdata = testing,
                                         type = "prob")[,1]

testResults$lasso_upsample1 <- predict(lasso_upsample1,
                                       newdata = testing,
                                       type = "prob")[,1]

testResults$elasticnet1 <- predict(elasticnet1,
                                   newdata = testing,
                                   type = "prob")[,1]

testResults$lasso_roc1 <- predict(lasso_roc1,
                                  newdata = testing,
                                  type = "prob")[,1]

testResults<-testResults %>%
  mutate(logitM1=ifelse(logitM1>0.5,"Si","No"),
         lasso1=ifelse(lasso1>0.5,"Si","No"),
         lasso_roc1=ifelse(lasso_roc1>rf_Thresh1$threshold,"Si","No"),
         lasso_downsample1=ifelse(lasso_downsample1>0.5,"Si","No"),
         lasso_upsample1=ifelse(lasso_upsample1>0.5,"Si","No"),
         elasticnet1=ifelse(elasticnet1>0.5,"Si","No")
  )

with(testResults,table(Pobre,logitM1))
with(testResults,table(Pobre,lasso1))
with(testResults,table(Pobre,lasso_roc1))
with(testResults,table(Pobre,lasso_downsample1))
with(testResults,table(Pobre,lasso_upsample1))
with(testResults,table(Pobre,elasticnet1))

# Para el modelo M2

testResults$logitM2<- predict(logitM2,
                              newdata = testing, 
                              type = "prob")[,1]

testResults$lasso2<- predict(lasso2,
                             newdata = testing,
                             type = "prob")[,1]

testResults$lasso_downsample2 <- predict(lasso_downsample2,
                                         newdata = testing,
                                         type = "prob")[,1]

testResults$lasso_upsample2 <- predict(lasso_upsample2,
                                       newdata = testing,
                                       type = "prob")[,1]

testResults$elasticnet2 <- predict(elasticnet2,
                                   newdata = testing,
                                   type = "prob")[,1]

testResults$lasso_roc2 <- predict(lasso_roc2,
                                  newdata = testing,
                                  type = "prob")[,1]

testResults<-testResults %>%
  mutate(logitM2=ifelse(logitM2>0.5,"Si","No"),
         lasso2=ifelse(lasso2>0.5,"Si","No"),
         lasso_roc2=ifelse(lasso_roc2>rf_Thresh2$threshold,"Si","No"),
         lasso_downsample2=ifelse(lasso_downsample2>0.5,"Si","No"),
         lasso_upsample2=ifelse(lasso_upsample2>0.5,"Si","No"),
         elasticnet2=ifelse(elasticnet2>0.5,"Si","No")
  )

with(testResults,table(Pobre,logitM2))
with(testResults,table(Pobre,lasso2))
with(testResults,table(Pobre,lasso_roc2))
with(testResults,table(Pobre,lasso_downsample2))
with(testResults,table(Pobre,lasso_upsample2))
with(testResults,table(Pobre,elasticnet2))

# Para el modelo M3

testResults$logitM3<- predict(logitM3,
                              newdata = testing, 
                              type = "prob")[,1]

testResults$lasso3<- predict(lasso3,
                             newdata = testing,
                             type = "prob")[,1]

testResults$lasso_downsample3 <- predict(lasso_downsample3,
                                         newdata = testing,
                                         type = "prob")[,1]

testResults$lasso_upsample3 <- predict(lasso_upsample3,
                                       newdata = testing,
                                       type = "prob")[,1]

testResults$elasticnet3 <- predict(elasticnet3,
                                   newdata = testing,
                                   type = "prob")[,1]

testResults$lasso_roc3 <- predict(lasso_roc3,
                                  newdata = testing,
                                  type = "prob")[,1]

testResults<-testResults %>%
  mutate(logitM3=ifelse(logitM3>0.5,"Si","No"),
         lasso3=ifelse(lasso3>0.5,"Si","No"),
         lasso_roc3=ifelse(lasso_roc3>rf_Thresh3$threshold,"Si","No"),
         lasso_downsample3=ifelse(lasso_downsample3>0.5,"Si","No"),
         lasso_upsample3=ifelse(lasso_upsample3>0.5,"Si","No"),
         elasticnet3=ifelse(elasticnet3>0.5,"Si","No")
  )

with(testResults,table(Pobre,logitM3))
with(testResults,table(Pobre,lasso3))
with(testResults,table(Pobre,lasso_roc3))
with(testResults,table(Pobre,lasso_downsample3))
with(testResults,table(Pobre,lasso_upsample3))
with(testResults,table(Pobre,elasticnet3))

logitM1
logitM2
logitM3
lasso1
lasso2
lasso3
lasso_upsample1
lasso_upsample2
lasso_upsample3
lasso_downsample1
lasso_downsample2
lasso_downsample3
elasticnet1
elasticnet2
elasticnet3
lasso_roc1
lasso_roc2
lasso_roc3

## data test
data_submit <- test_final
df_coeficientes <- coef(lasso_roc1$finalModel, c(lasso_roc1$finalModel$lambdaOpt, lasso_roc1$finalModel$a0)) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

write_xlsx(df_coeficientes, file = "/Users/betinacortes/clasification_model_submit_LR1.csv") # se exporta a excel tabla con las estadísticas descriptivas

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col(fill = "#556B2F") + 
  labs(title = "Coeficientes del modelo Lasso1, maximizando el ROC", y = "Coeficientes", x = "Variables predictoras") + # Añadir labels en X e Y
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 90))


## prediction
data_submit$prediction <- predict(lasso_roc1, data_submit , type="prob")[,1]

data_submit = data_submit %>% mutate(pobre=ifelse(prediction>0.5,1,0))

submit = data_submit %>% select(id,pobre)

prop.table(table(submit$pobre)) 

## export results
saveRDS(submit, file = "clasification_model_submit.rds")

write.csv(submit, file = "/Users/betinacortes/clasification_model_submit_EN1.csv", row.names = FALSE)

