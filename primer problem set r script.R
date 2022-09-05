
#1. Data. 
##(a)	Data acquisition

rm(list = ls())
pacman::p_load(rvest, dplyr, tidy, caret, kableExtra, tidyverse)
url_test <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")
geih <- data.frame()
for (url in url_test) {
  print(url)
  source_table <- read_html(url) %>%
    html_table()
  temp <- as.data.frame(source_table)
  print(nrow(temp))
  geih <- rbind(geih, temp)
}
view(geih)


##(b)	Data Cleaning 
#seleccionamos las variables relevantes para nuestro estudio
#seleccionamos todas las variables relevantes

geih2018 <- geih %>% select(directorio, mes, ingtotob, age, sex, p6210, dsi, maxEducLevel,estrato1)

geihmodificado <- geih2018 %>% filter(age > "18")
view(geihmodificado)

baselista <- geihmodificado %>% filter(dsi != "0")
view(baselista)

base2018 <- na.omit(baselista)
view(base2018)

#estadisticas descriptivas
summary(base2018)
sd(base2018$ingtotob)
#podemos estimar el modelo 

mco1 = lm(yingtotob ~ 0 + age + sex + p6210 + dsi + maxEducLevel + estrato1, data = base2018)
view(mco1)

#resultados de la estimación
summary(mco)

##2.	Age-earnings profile. 

punto2 <- geih %>% select(directorio, mes, ingtotob, y_salary_m, age, sex, p6210, dsi, maxEducLevel,estrato1)
basep2 <- na.omit(punto2)
view(basep2)

#estimamos el modelos por MCO

mco = lm(y_salary_m ~ 0 + age + age^2,data = basep2)

#resultados de la estimación
summary(mco)

#ahora vamos a hacer los intervalos de confianza con bootstrap

library(tidyverse)
library(caret)
library(knitr)
library(mlbench)
##HASTA ACA TODO PERFECTO, ACA ME ENREDE UN POCO CON BOOTSTRAP PERO LO INTENTE LO MEJOR POSIBLE
#definición del entrenamiento del modelo


train.control <- train.control(method = "boot", number = 100)

#entrenando el modelo usando la función train

model <- train(y_salary_m ~ .,data = basep2, method = "lm",
               trControl = train.control)



##(3)	The gender earnings 
library(mlogit)
library(dplyr)
library(tidyr)

#creo la variable female

view(base2018)
base2018$femalee = as.integer(base2018$sex == "0")

#como sex es dicotoma cuando sex=0 es mujer
modelo.logit <- glm(ingtotob ~ 0 + femalee, data = base2018, family = "binomial")
summary(modelo.logit)

library(ggplot2)
ggplot(base2018, aes(x = femalee, y = ingtotob, color = abandona)) + geom_point()


require("tidyverse")
require("fabricatr")

set.seed(101010)
db <- fabricate(
  N = 10000,
  ability=rnorm(N,mean=.5,sd=2),
  schooling = round(runif(N, 2, 14)),
  schooling = round(ceiling(schooling+1*ability)),
  logwage =rnorm(N, mean=7+.15*schooling+.25*ability, sd=2)
)
head(db)


##4.	Predicting earnings
install.packages("caret")
library(caret)
data("base2018")
head("base2018")
head(base2018)

set.seed(10101)

indice=createDataPartition(base2018$ingtotob, p=0.70, list=FALSE)
entrenamiento <- base2018[indice,]
prueba <- base2018[-indice,]
dim(entrenamiento)
dim(prueba)

#veamos ahora la distribución 
plot(entrenamiento$ingtotob)
plot(prueba$ingtotob)

summary(entrenamiento)
summary(prueba)

#para evaluar la calidad de los modelos generados, utilizaremos la precisión que es el número de casos acertados entre el total de número de casos múltipplicados por 100
metrica <- "Accuracy"

#Para entrenar y evaluar los distintios modelos con pocos datos es interesante utilizar una técnica denominada validación cruzada
control <- trainControl(method="cv", number=10)

qset.seed(10101)
modelo.lda <- train(ingtotob~., data=entrenamiento,method="lda", metric=metrica, trControl=control)

set.seed(10101)
modelo.knn <- train(ingtotob~., data=entrenamiento,method="knn", metric=metrica, trControl=control)

set.seed(10101)
modelo.c50 <- train(ingtotob~., data=entrenamiento,method="c5.0", metric=metrica, trControl=control)
set.seed(10101)
modelo.svm <- train(ingtotob~., data=entrenamiento,method="svmRadial", metric=metrica, trControl=control)

#Podemos hacer una matriz con los resultados
print(modelo.lda$results)
print(modelo.knn$results)
print(modelo.c50$results)
print(modelo.svm$results)

