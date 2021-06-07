## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
data <- read.csv("water_potability.csv", sep = ",", strip.white = TRUE, header = TRUE, na.strings = "")
#data_plot <- read.csv("water_potability.csv", sep = ",", strip.white = TRUE, header = TRUE, na.strings = "")

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(skimr)
library(Hmisc)

skim(data)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
describe(as.factor(data$Potability))


## ----fig.height=12, fig.width=12, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------
library(GGally)
ggpairs(data, columns = 1:9, ggplot2::aes(colour=as.factor(Potability)), progress = FALSE) 


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(reshape2)
data_m <- melt(data, id.vars = "Potability")

## ----fig.height=12, fig.width=12, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------
ggplot(data = data_m, aes(x=variable, y=value, fill=as.factor(Potability))) + 
             geom_boxplot() + facet_wrap(~variable, scales="free")

## ----fig.height=12, fig.width=12----------------------------------------------------------------------------------------------------------------------------
boxplot(scale(data[,1:9]))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
cor_mat <- cor(data[1:9], use="complete.obs")
cor_mat


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggcorrplot)
ggcorrplot(cor_mat, hc.order = TRUE, type = "lower", lab = TRUE, insig = "blank")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(y = Potability, fill = factor(Potability))) + geom_bar() + labs(title = "Proporción de registros por clase") + geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))), nudge_x = 40)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(plyr)

for (i in names(data)){
  plt <- ggplot(data, aes_string(x=i)) + 
         geom_histogram(aes(y=..density..), colour="black", fill="white")+
         geom_density(alpha=.2, fill="#FF6666") + facet_wrap(~Potability, scales="free")
  print(plt)
}


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
max(rowSums(is.na(data)))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
sum(!complete.cases(data))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
data_na_count <- data
data_na_count$na_count <- apply(data, 1, function(x) sum(is.na(x)))
head(data_na_count %>% slice_max(na_count, n = 20), 20)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(VIM)
marginplot(data[c(1,5)])

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
marginplot(data[c(1,8)])

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
marginplot(data[c(5,8)])


## ----fig.width=14-------------------------------------------------------------------------------------------------------------------------------------------
library(mice)
md.pattern(data)


## ----fig.width=10-------------------------------------------------------------------------------------------------------------------------------------------
mice_plot <- aggr(data, col=c('navyblue','yellow'),
             numbers=TRUE, sortVars=TRUE,
             labels=names(data), cex.axis=.7,
             gap=3, ylab=c("Missing data","Pattern"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(QuantPsyc)
mult.norm(data)$mult.test


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(energy)

complete_data <- data[complete.cases(data),]
mvnorm.etest(complete_data, R=100)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(MVN)
result <- mvn(complete_data, multivariatePlot = "qq", showOutliers = TRUE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
result$multivariateNormality


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
result$univariateNormality

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
result$Descriptives


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
outliers_Tukey <- function(x) {

  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1

 upper_limit = Q3 + (iqr*1.5)
 lower_limit = Q1 - (iqr*1.5)

 x > upper_limit | x < lower_limit
}

remove_outliers_Tukey <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers_Tukey(df[[col]]),]
  }
  df
}

outliers_sd <- function(x) {
  
    upper_limit = mean(x) + 3*sd(x)
    lower_limit = mean(x) - 3*sd(x)
    
    x > upper_limit | x < lower_limit
}

remove_outliers_sd <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers_sd(df[[col]]),]
  }
  df
}


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
data_Tukey_outliers <- remove_outliers_Tukey(complete_data)
data_sd_outliers <- remove_outliers_sd(complete_data)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
result <- mvn(data_Tukey_outliers, multivariatePlot = "qq", showOutliers = TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
result$multivariateNormality


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
summary(data_Tukey_outliers)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(scale(data_Tukey_outliers[,1:9]))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
result <- mvn(data_sd_outliers, multivariatePlot = "qq", showOutliers = TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
result$multivariateNormality


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
summary(data_sd_outliers)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(scale(data_sd_outliers[,1:9]))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
cat("data_sd_outliers: ",nrow(data_sd_outliers), "\n")
cat("data_Tukey_outliers: ",nrow(data_Tukey_outliers))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(plyr)
library(ggplot2)

data_test <- data
data_test$na_count <- apply(data, 1, function(x) sum(is.na(x)))
data_test$na_count[data_test$na_count>0] <- 1

for (i in names(data)){
  plt <- ggplot(data_test, aes_string(x=i)) + 
         geom_histogram(aes(y=..density..), colour="black", fill="white")+
         geom_density(alpha=.2, fill="#FF6666") + facet_wrap(~na_count, scales="free")
  print(plt)
}


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
summary(data_test[data_test$na_count==0,])
summary(data_test[data_test$na_count==1,])


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
apply(data, 2, function(col)sum(is.na(col))/length(col))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(missForest)
library(tidyverse)

set.seed(564165)
complete_data <- data[complete.cases(data),]
head(complete_data)

complete_data_ph <- complete_data[,c("ph")]
complete_data_Sulfate <- complete_data[,c("Sulfate")]
complete_data_Triha <- complete_data[,c("Trihalomethanes")]

complete_data_noNAcols <- complete_data[,c("Hardness", "Solids", "Chloramines", "Conductivity", "Organic_carbon", "Turbidity", "Potability")]

complete_data_ph <- prodNA(as.data.frame(complete_data_ph), noNA = 0.14987790)
complete_data_Sulfate <- prodNA(as.data.frame(complete_data_Sulfate), noNA = 0.23840049)
complete_data_Triha <- prodNA(as.data.frame(complete_data_Triha), noNA = 0.04945055)

complete_data_NAcols <- cbind(complete_data_ph, complete_data_Sulfate)
complete_data_NAcols <- cbind(complete_data_NAcols, complete_data_Triha)

complete_data_NAs <- cbind(complete_data_NAcols, complete_data_noNAcols)
complete_data_NAs <- complete_data_NAs[, c(1, 4, 5, 6, 2, 7, 8, 3, 9, 10)]

names(complete_data_NAs)[names(complete_data_NAs) == "complete_data_ph"] <- "ph"
names(complete_data_NAs)[names(complete_data_NAs) == "complete_data_Sulfate"] <- "Sulfate"
names(complete_data_NAs)[names(complete_data_NAs) == "complete_data_Triha"] <- "Trihalomethanes"

head(complete_data_NAs)


## ----fig.height=12, fig.width=12----------------------------------------------------------------------------------------------------------------------------
md.pattern(complete_data_NAs)

## ----fig.width=10-------------------------------------------------------------------------------------------------------------------------------------------
library(VIM)
library(mice)
set.seed(100)
mice_plot <- aggr(complete_data_NAs, col=c('navyblue','yellow'),
             numbers=TRUE, sortVars=TRUE,
             labels=names(complete_data_NAs), cex.axis=.7,
             gap=3, ylab=c("Missing data","Pattern"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(VIM)
marginplot(complete_data_NAs[c(1,5)])

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
marginplot(complete_data_NAs[c(1,8)])

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
marginplot(complete_data_NAs[c(5,8)])


## ----echo=TRUE, results='hide', include=FALSE---------------------------------------------------------------------------------------------------------------
library(mice)
set.seed(100)
complete_data_NAs_MICE <- mice(complete_data_NAs,m=5, maxit = 100, method="pmm",seed=245435, print=FALSE)


## ----fig.width=20, fig.height=12----------------------------------------------------------------------------------------------------------------------------
plot(complete_data_NAs_MICE)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
densityplot(complete_data_NAs_MICE)


## ----fig.height=12, fig.width=12----------------------------------------------------------------------------------------------------------------------------
stripplot(complete_data_NAs_MICE, pch = 20, cex = 1.2)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
summary(data)
summary(mice::complete(complete_data_NAs_MICE, "long"))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
MICE_test <- mice::complete(complete_data_NAs_MICE, "long")
fit <- with(complete_data_NAs_MICE, lm(Sulfate~ Hardness+Solids+Chloramines+Conductivity+Organic_carbon+Turbidity+Trihalomethanes+Potability))
summary(mice::pool(fit))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
imp_2 <- mice(complete_data_NAs[,-c(4,6,9,10)],m=5, maxit = 100, method="pmm",seed=245435, print=FALSE)
fit_2 <- with(imp_2, lm(Sulfate~ Hardness+Solids+Organic_carbon+Trihalomethanes))
summary(mice::pool(fit_2))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
imp_3 <- mice(complete_data_NAs[,-c(4,6,8,9,10)],m=5, maxit = 100, method="pmm",seed=245435, print=FALSE)
fit_3 <- with(imp_3, lm(Sulfate~ Hardness+Solids+Organic_carbon))
summary(mice::pool(fit_3))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
mice::pool(fit)
mice::pool(fit_2)
mice::pool(fit_3)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(Metrics)
a=0
for (i in 1:5) { 
  predicted <- mice::complete(complete_data_NAs_MICE, i)[is.na(complete_data_NAs$Sulfate),]$Sulfate
  actual <- complete_data[is.na(complete_data_NAs$Sulfate),]$Sulfate
  
  cat(Metrics::rmse(actual, predicted), "\n")
  
  a= a + (Metrics::rmse(actual, predicted))
}
cat("mean RMSE: ",a/5)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
a=0
for (i in 1:5) { 
  predicted <- mice::complete(imp_2, i)[is.na(complete_data_NAs$Sulfate),]$Sulfate
  actual <- complete_data[is.na(complete_data_NAs$Sulfate),]$Sulfate
  
  cat(Metrics::rmse(actual, predicted), "\n")
  
  a= a + (Metrics::rmse(actual, predicted))
}
cat("mean RMSE: ",a/5)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
a=0
for (i in 1:5) { 
  predicted <- mice::complete(imp_3, i)[is.na(complete_data_NAs$Sulfate),]$Sulfate
  actual <- complete_data[is.na(complete_data_NAs$Sulfate),]$Sulfate
  
  cat(Metrics::rmse(actual, predicted), "\n")
  
  a= a + (Metrics::rmse(actual, predicted))
}
cat("mean RMSE: ",a/5)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(Metrics)
a=0
for (i in 1:5) { 
  predicted <- mice::complete(complete_data_NAs_MICE, i)[is.na(complete_data_NAs$ph),]$ph
  actual <- complete_data[is.na(complete_data_NAs$ph),]$ph
  
  cat(Metrics::rmse(actual, predicted), "\n")
  
  a= a + (Metrics::rmse(actual, predicted))
}
cat("mean RMSE: ",a/5)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
a=0
for (i in 1:5) { 
  predicted <- mice::complete(imp_2, i)[is.na(complete_data_NAs$ph),]$ph
  actual <- complete_data[is.na(complete_data_NAs$ph),]$ph
  
  cat(Metrics::rmse(actual, predicted), "\n")
  
  a= a + (Metrics::rmse(actual, predicted))
}
cat("mean RMSE: ",a/5)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
a=0
for (i in 1:5) { 
  predicted <- mice::complete(imp_3, i)[is.na(complete_data_NAs$ph),]$ph
  actual <- complete_data[is.na(complete_data_NAs$ph),]$ph
  
  cat(Metrics::rmse(actual, predicted), "\n")
  
  a= a + (Metrics::rmse(actual, predicted))
}
cat("mean RMSE: ",a/5)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(Metrics)
a=0
for (i in 1:5) { 
  predicted <- mice::complete(complete_data_NAs_MICE, i)[is.na(complete_data_NAs$Trihalomethanes),]$Trihalomethanes
  actual <- complete_data[is.na(complete_data_NAs$Trihalomethanes),]$Trihalomethanes
  
  cat(Metrics::rmse(actual, predicted), "\n")
  
  a= a + (Metrics::rmse(actual, predicted))
}
cat("mean RMSE: ",a/5,"\n")

a=0
for (i in 1:5) { 
  predicted <- mice::complete(imp_2, i)[is.na(complete_data_NAs$Trihalomethanes),]$Trihalomethanes
  actual <- complete_data[is.na(complete_data_NAs$Trihalomethanes),]$Trihalomethanes
  
  cat(Metrics::rmse(actual, predicted), "\n")
  
  a= a + (Metrics::rmse(actual, predicted))
}
cat("mean RMSE: ",a/5, "\n")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
res <- "Resultado"
name <- "Model"
add_value <- function(result, new_name) {
  res <<- c(res, result)
  name <<- c(name, new_name)
}
add_value(2.183171, "result_MICE_ph")
add_value(55.8299, "result_MICE_Sulfate")
add_value(22.33897 , "result_MICE_Trihalomethanes")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(missForest)
set.seed(100)
complete_data_NAs_MISSFOREST_1 <- missForest(complete_data_NAs, verbose=TRUE)
complete_data_NAs_MISSFOREST <- complete_data_NAs_MISSFOREST_1$ximp
head(complete_data_NAs_MISSFOREST)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)
complete_data_NAs_MISSFOREST_2 <- missForest(complete_data_NAs, verbose=TRUE, ntree=500)
complete_data_NAs_MISSFOREST_test <- complete_data_NAs_MISSFOREST_2$ximp
head(complete_data_NAs_MISSFOREST)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Realizamos los mismos pasos que con el modelo de imputación MICE para comparar los vectores de cada variable.
actual = complete_data[is.na(complete_data_NAs$ph), ]$ph         
predicted = complete_data_NAs_MISSFOREST_test[is.na(complete_data_NAs$ph), ]$ph      
result_MISSFOREST_ph = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Sulfate), ]$Sulfate
predicted = complete_data_NAs_MISSFOREST_test[is.na(complete_data_NAs$Sulfate), ]$Sulfate      
result_MISSFOREST_Sulfate = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes
predicted = complete_data_NAs_MISSFOREST_test[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes      
result_MISSFOREST_Trihalomethanes = Metrics::rmse(actual, predicted)

result_MISSFOREST_ph
result_MISSFOREST_Sulfate
result_MISSFOREST_Trihalomethanes


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Realizamos los mismos pasos que con el modelo de imputación MICE para comparar los vectores de cada variable.
actual = complete_data[is.na(complete_data_NAs$ph), ]$ph         
predicted = complete_data_NAs_MISSFOREST[is.na(complete_data_NAs$ph), ]$ph      
result_MISSFOREST_2_ph = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Sulfate), ]$Sulfate
predicted = complete_data_NAs_MISSFOREST[is.na(complete_data_NAs$Sulfate), ]$Sulfate      
result_MISSFOREST_2_Sulfate = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes
predicted = complete_data_NAs_MISSFOREST[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes      
result_MISSFOREST_2_Trihalomethanes = Metrics::rmse(actual, predicted)

result_MISSFOREST_2_ph
result_MISSFOREST_2_Sulfate
result_MISSFOREST_2_Trihalomethanes


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
add_value(result_MISSFOREST_ph, "result_MISSFOREST_ph")
add_value(result_MISSFOREST_Sulfate, "result_MISSFOREST_Sulfate")
add_value(result_MISSFOREST_Trihalomethanes, "result_MISSFOREST_Trihalomethanes")
res
name


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(Hmisc)
set.seed(100)

complete_data_NAs_HMISC_areg <- aregImpute(~ Sulfate + Hardness + Solids + Chloramines + ph + Conductivity + Organic_carbon + Trihalomethanes + Turbidity + Potability, data = complete_data_NAs, n.impute = 5, type= "pmm", nk=3, burnin=10)
print(complete_data_NAs_HMISC_areg)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(Hmisc)
set.seed(100)

complete_data_NAs_HMISC_areg_test <- aregImpute(~ Sulfate + Hardness + Solids + Chloramines + ph + Organic_carbon + Trihalomethanes + Turbidity + Potability, data = complete_data_NAs, n.impute = 10, type= "pmm", nk=c(0,3:5), tlinear = FALSE)
print(complete_data_NAs_HMISC_areg_test)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
for (i in 1:10){
  temp_data <- impute.transcan(complete_data_NAs_HMISC_areg_test, imputation = i, data = complete_data_NAs, list.out = TRUE,
                           pr = FALSE, check = FALSE)
  temp_data <- data.frame(temp_data)
  actual = complete_data[is.na(complete_data_NAs$ph), ]$ph         
  predicted = temp_data[is.na(complete_data_NAs$ph), ]$ph      
  result_HMISC_ph = Metrics::rmse(actual, predicted)

  actual = complete_data[is.na(complete_data_NAs$Sulfate), ]$Sulfate
  predicted = temp_data[is.na(complete_data_NAs$Sulfate), ]$Sulfate      
  result_HMISC_Sulfate = Metrics::rmse(actual, predicted)

  actual = complete_data[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes
  predicted = temp_data[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes      
  result_HMISC_Trihalomethanes = Metrics::rmse(actual, predicted)

  cat("ph RMSE:\t\t", result_HMISC_ph, "\n")
  cat("Sulfate RMSE:\t\t", result_HMISC_Sulfate, "\n")
  cat("Trihalomethanes RMSE:   ", result_HMISC_Trihalomethanes, "\n")
  cat("----------------------------------\n")
}


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Realizamos los mismos pasos que con los modelos de imputación MICE y MISSFOREST para comparar los vectores de cada variable.
complete_data_NAs_HMISC <- impute.transcan(complete_data_NAs_HMISC_areg_test, imputation = 10, data = complete_data_NAs, list.out = TRUE,
                           pr = FALSE, check = FALSE)
complete_data_NAs_HMISC <- data.frame(complete_data_NAs_HMISC)


actual = complete_data[is.na(complete_data_NAs$ph), ]$ph         
predicted = complete_data_NAs_HMISC[is.na(complete_data_NAs$ph), ]$ph      
result_HMISC_ph = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Sulfate), ]$Sulfate
predicted = complete_data_NAs_HMISC[is.na(complete_data_NAs$Sulfate), ]$Sulfate      
result_HMISC_Sulfate = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes
predicted = complete_data_NAs_HMISC[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes      
result_HMISC_Trihalomethanes = Metrics::rmse(actual, predicted)

result_HMISC_ph
result_HMISC_Sulfate
result_HMISC_Trihalomethanes


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
add_value(result_HMISC_ph, "result_HMISC_ph")
add_value(result_HMISC_Sulfate, "result_HMISC_Sulfate")
add_value(result_HMISC_Trihalomethanes, "result_HMISC_Trihalomethanes")
res
name


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(mi)
set.seed(100)
complete_data_NAs_MI_mi <- mi(complete_data_NAs)
summary(complete_data_NAs_MI_mi)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
plot(complete_data_NAs_MI_mi, ask=FALSE)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
complete_data_NAs_MI <- mi::complete(complete_data_NAs_MI_mi, m=1)

# Realizamos los mismos pasos que con los modelos anteriores para comparar los vectores de cada variable.
actual = complete_data[is.na(complete_data_NAs$ph), ]$ph         
predicted = complete_data_NAs_MI[is.na(complete_data_NAs$ph), ]$ph      
result_MI_ph = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Sulfate), ]$Sulfate
predicted = complete_data_NAs_MI[is.na(complete_data_NAs$Sulfate), ]$Sulfate      
result_MI_Sulfate = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes
predicted = complete_data_NAs_MI[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes      
result_MI_Trihalomethanes = Metrics::rmse(actual, predicted)

result_MI_ph
result_MI_Sulfate
result_MI_Trihalomethanes


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
add_value(result_MI_ph, "result_MI_ph")
add_value(result_MI_Sulfate, "result_MI_Sulfate")
add_value(result_MI_Trihalomethanes, "result_MI_Trihalomethanes")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)
complete_data_NAs_kNN <- kNN(complete_data_NAs, k=3)
complete_data_NAs_kNN <- complete_data_NAs_kNN[, 1:10]


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Realizamos los mismos pasos que con los modelos anteriores para comparar los vectores de cada variable.
actual = complete_data[is.na(complete_data_NAs$ph), ]$ph         
predicted = complete_data_NAs_kNN[is.na(complete_data_NAs$ph), ]$ph      
result_kNN_ph = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Sulfate), ]$Sulfate
predicted = complete_data_NAs_kNN[is.na(complete_data_NAs$Sulfate), ]$Sulfate      
result_kNN_Sulfate = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes
predicted = complete_data_NAs_kNN[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes      
result_kNN_Trihalomethanes = Metrics::rmse(actual, predicted)

result_kNN_ph
result_kNN_Sulfate
result_kNN_Trihalomethanes


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)
complete_data_NAs_kNN <- kNN(complete_data_NAs, k=100)
complete_data_NAs_kNN <- complete_data_NAs_kNN
head(complete_data_NAs_kNN)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Realizamos los mismos pasos que con los modelos anteriores para comparar los vectores de cada variable.
actual = complete_data[is.na(complete_data_NAs$ph), ]$ph         
predicted = complete_data_NAs_kNN[is.na(complete_data_NAs$ph), ]$ph      
result_kNN_ph = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Sulfate), ]$Sulfate
predicted = complete_data_NAs_kNN[is.na(complete_data_NAs$Sulfate), ]$Sulfate      
result_kNN_Sulfate = Metrics::rmse(actual, predicted)

actual = complete_data[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes
predicted = complete_data_NAs_kNN[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes      
result_kNN_Trihalomethanes = Metrics::rmse(actual, predicted)

result_kNN_ph
result_kNN_Sulfate
result_kNN_Trihalomethanes


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret)
scaled_test <- scale(complete_data_NAs)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)
complete_data_NAs_kNN <- kNN(scaled_test, k=100)
complete_data_NAs_kNN <- as.data.frame(complete_data_NAs_kNN[, 1:10])

# Realizamos los mismos pasos que con los modelos anteriores para comparar los vectores de cada variable.
actual = scale(complete_data[is.na(complete_data_NAs$ph), ]$ph)        
predicted = complete_data_NAs_kNN[is.na(complete_data_NAs$ph), ]$ph      
result_kNN_ph_scale = Metrics::rmse(actual, predicted)

actual = scale(complete_data[is.na(complete_data_NAs$Sulfate), ]$Sulfate)
predicted = complete_data_NAs_kNN[is.na(complete_data_NAs$Sulfate), ]$Sulfate      
result_kNN_Sulfate_scale = Metrics::rmse(actual, predicted)

actual = scale(complete_data[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes)
predicted = complete_data_NAs_kNN[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes      
result_kNN_Trihalomethanes_scale = Metrics::rmse(actual, predicted)

result_kNN_ph_scale
result_kNN_Sulfate_scale
result_kNN_Trihalomethanes_scale


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(missForest)
set.seed(100)
complete_data_NAs_MISSFOREST_scale <- missForest(scale(complete_data_NAs), verbose=TRUE)
complete_data_NAs_MISSFOREST_scaled <- as.data.frame(complete_data_NAs_MISSFOREST_scale$ximp)

# Realizamos los mismos pasos que con el modelo de imputación MICE para comparar los vectores de cada variable.
actual = scale(complete_data[is.na(complete_data_NAs$ph), ]$ph)         
predicted = complete_data_NAs_MISSFOREST_scaled[is.na(complete_data_NAs$ph), ]$ph      
result_MISSFOREST_ph_scale = Metrics::rmse(actual, predicted)

actual = scale(complete_data[is.na(complete_data_NAs$Sulfate), ]$Sulfate)
predicted = complete_data_NAs_MISSFOREST_scaled[is.na(complete_data_NAs$Sulfate), ]$Sulfate      
result_MISSFOREST_Sulfate_scale = Metrics::rmse(actual, predicted)

actual = scale(complete_data[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes)
predicted = complete_data_NAs_MISSFOREST_scaled[is.na(complete_data_NAs$Trihalomethanes), ]$Trihalomethanes      
result_MISSFOREST_Trihalomethanes_scale = Metrics::rmse(actual, predicted)

result_MISSFOREST_ph_scale
result_MISSFOREST_Sulfate_scale
result_MISSFOREST_Trihalomethanes_scale


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
add_value(result_kNN_ph, "result_kNN_ph")
add_value(result_kNN_Sulfate, "result_kNN_Sulfate")
add_value(result_kNN_Trihalomethanes, "result_kNN_Trihalomethanes")

add_value(result_kNN_ph_scale, "result_kNN_ph_scale")
add_value(result_kNN_Sulfate_scale, "result_kNN_Sulfate_scale")
add_value(result_kNN_Trihalomethanes_scale, "result_kNN_Trihalomethanes_scale")

add_value(result_MISSFOREST_ph_scale, "result_MISSFOREST_ph_scale")
add_value(result_MISSFOREST_Sulfate_scale, "result_MISSFOREST_Sulfate_scale")
add_value(result_MISSFOREST_Trihalomethanes_scale, "result_MISSFOREST_Trihalomethanes_scale")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
cat(paste0(data.frame(cbind(name, res))[1,1], "\t\t\t\t"))
cat(paste0(data.frame(cbind(name, res))[1,2], "\n"))
cat("--------------------------------------------------------------\n")
for (i in c(2,5,8,11,14,17,20)){
  cat(paste0(data.frame(cbind(name, res))[i,1], ":  "))
  cat(paste0(data.frame(cbind(name, res))[i,2], "\n"))
  cat(paste0(data.frame(cbind(name, res))[i+1,1], ":  "))
  cat(paste0(data.frame(cbind(name, res))[i+1,2], "\n"))
  cat(paste0(data.frame(cbind(name, res))[i+2,1], ":  "))
  cat(paste0(data.frame(cbind(name, res))[i+2,2], "\n"))
  cat("--------------------------------------------------------------\n")
}
  



## -----------------------------------------------------------------------------------------------------------------------------------------------------------
outliers_Tukey_na <- function(x) {

  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1

 upper_limit = Q3 + (iqr*1.5)
 lower_limit = Q1 - (iqr*1.5)
 cat(upper_limit,"\t",lower_limit,"\n")
}


outliers_sd_na <- function(x) {
  
    upper_limit = mean(x) + 3*sd(x)
    lower_limit = mean(x) - 3*sd(x)
    
    cat(upper_limit,"\t",lower_limit,"\n")
}



## -----------------------------------------------------------------------------------------------------------------------------------------------------------
outliers_Tukey_na(data[complete.cases(data$ph),]$ph)
outliers_Tukey_na(data[complete.cases(data$Hardness),]$Hardness)
outliers_Tukey_na(data[complete.cases(data$Solids),]$Solids)
outliers_Tukey_na(data[complete.cases(data$Chloramines),]$Chloramines)
outliers_Tukey_na(data[complete.cases(data$Sulfate),]$Sulfate)
outliers_Tukey_na(data[complete.cases(data$Conductivity),]$Conductivity)
outliers_Tukey_na(data[complete.cases(data$Organic_carbon),]$Organic_carbon)
outliers_Tukey_na(data[complete.cases(data$Trihalomethanes),]$Trihalomethanes)
outliers_Tukey_na(data[complete.cases(data$Turbidity),]$Turbidity)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
test <- data
head(test)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
summary(test)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
test <- subset(test, (ph < 11.01553 & ph > 3.139631) | is.na(ph))
test <- subset(test, Hardness < 276.3928 & Hardness > 117.1252)
test <- subset(test, Solids < 44831.87 & Solids > -1832.417)
test <- subset(test, Chloramines < 11.09609 & Chloramines > 3.146221)
test <- subset(test, (Sulfate < 438.3262 & Sulfate > 229.3235) | is.na(Sulfate))
test <- subset(test, Conductivity < 655.8791 & Conductivity > 191.6476)
test <- subset(test, Organic_carbon < 23.29543 & Organic_carbon > 5.328026)
test <- subset(test, (Trihalomethanes < 109.5769 & Trihalomethanes > 23.60513) | is.na(Trihalomethanes))
test <- subset(test, Turbidity < 6.091233 & Turbidity > 1.848797)
no_ouliers_incomplete_Tukey <- test
head(no_ouliers_incomplete_Tukey)
nrow(no_ouliers_incomplete_Tukey)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
summary(no_ouliers_incomplete_Tukey)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
outliers_sd_na(data[complete.cases(data$ph),]$ph)
outliers_sd_na(data[complete.cases(data$Hardness),]$Hardness)
outliers_sd_na(data[complete.cases(data$Solids),]$Solids)
outliers_sd_na(data[complete.cases(data$Chloramines),]$Chloramines)
outliers_sd_na(data[complete.cases(data$Sulfate),]$Sulfate)
outliers_sd_na(data[complete.cases(data$Conductivity),]$Conductivity)
outliers_sd_na(data[complete.cases(data$Organic_carbon),]$Organic_carbon)
outliers_sd_na(data[complete.cases(data$Trihalomethanes),]$Trihalomethanes)
outliers_sd_na(data[complete.cases(data$Turbidity),]$Turbidity)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
test <- data
test <- subset(test, (ph < 11.86375 & ph > 2.297836) | is.na(ph))
test <- subset(test, Hardness < 295.0088 & Hardness > 97.73021)
test <- subset(test, Solids < 48319.81 & Solids > -4291.62)
test <- subset(test, Chloramines < 11.87153 & Chloramines > 2.373022)
test <- subset(test, (Sulfate < 458.0263 & Sulfate > 209.5253) | is.na(Sulfate))
test <- subset(test, Conductivity < 668.6773 & Conductivity > 183.7329)
test <- subset(test, Organic_carbon < 24.20946 & Organic_carbon > 4.360484)
test <- subset(test, (Trihalomethanes < 114.9213 & Trihalomethanes > 17.87127) | is.na(Trihalomethanes))
test <- subset(test, Turbidity < 6.307933 & Turbidity > 1.625639)
no_ouliers_incomplete_sd <- test
head(no_ouliers_incomplete_sd)
nrow(no_ouliers_incomplete_sd)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
summary(no_ouliers_incomplete_sd)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(scale(data[,1:9]))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(scale(no_ouliers_incomplete_Tukey[,1:9]))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(scale(no_ouliers_incomplete_sd[,1:9]))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
sort(boxplot.stats(data$ph)$out)
cat("--------------------------------------------------------------------------------------------------------------------\n")
sort(boxplot.stats(data$Hardness)$out)
cat("--------------------------------------------------------------------------------------------------------------------\n")
sort(boxplot.stats(data$Solids)$out)
cat("--------------------------------------------------------------------------------------------------------------------\n")
sort(boxplot.stats(data$Chloramines)$out)
cat("--------------------------------------------------------------------------------------------------------------------\n")
sort(boxplot.stats(data$Sulfate)$out)
cat("--------------------------------------------------------------------------------------------------------------------\n")
sort(boxplot.stats(data$Conductivity)$out)
cat("--------------------------------------------------------------------------------------------------------------------\n")
sort(boxplot.stats(data$Organic_carbon)$out)
cat("--------------------------------------------------------------------------------------------------------------------\n")
sort(boxplot.stats(data$Trihalomethanes)$out)
cat("--------------------------------------------------------------------------------------------------------------------\n")
sort(boxplot.stats(data$Turbidity)$out)
cat("--------------------------------------------------------------------------------------------------------------------\n")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
test <- data
test <- subset(test, (ph < 11.0278799 & ph > 3.1020756) | is.na(ph))
test <- subset(test, Hardness < 276.69976 & Hardness > 117.05731)
test <- subset(test, Solids < 44868.46 & Solids > -4291.62)
test <- subset(test, Chloramines < 11.1016281 & Chloramines > 3.1395527)
test <- subset(test, (Sulfate < 439.7879 & Sulfate > 227.6656) | is.na(Sulfate))
test <- subset(test, Conductivity < 656.9241 & Conductivity > 181.4838)
test <- subset(test, Organic_carbon < 23.317699 & Organic_carbon > 5.315287)
test <- subset(test, (Trihalomethanes < 110.431080 & Trihalomethanes > 23.136611) | is.na(Trihalomethanes))
test <- subset(test, Turbidity < 6.099632 & Turbidity > 1.844372)
no_ouliers_incomplete_boxplot <- test
head(no_ouliers_incomplete_boxplot)
nrow(no_ouliers_incomplete_boxplot)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(scale(no_ouliers_incomplete_boxplot[,1:9]))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)
imp_1 <- missForest(data, verbose=TRUE)
mF_complete <- imp_1$ximp
imp_2 <- missForest(no_ouliers_incomplete_Tukey, verbose=TRUE)
mF_Tukey <- imp_2$ximp
imp_3 <- missForest(no_ouliers_incomplete_sd, verbose=TRUE)
mF_sd <- imp_3$ximp


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
nrow(mF_complete)
nrow(mF_Tukey)
nrow(mF_sd)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
nrow(remove_outliers_Tukey(mF_complete))

nrow(remove_outliers_sd(mF_complete))

nrow(remove_outliers_Tukey(mF_Tukey))

nrow(remove_outliers_sd(mF_sd))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
mF_complete_Tuk <- remove_outliers_Tukey(mF_complete)

mF_complete_sd <- (mF_complete)

mF_Tukey_Tuk <- remove_outliers_Tukey(mF_Tukey)

mF_sd_sd <- remove_outliers_sd(mF_sd)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

data_scaled = as.data.frame(cbind(scale(data[,1:9]), data[,10]))
names(data_scaled)[names(data_scaled) == "V10"] <- "Potability"

data_scaled_tuk = as.data.frame(cbind(scale(no_ouliers_incomplete_Tukey[,1:9]), no_ouliers_incomplete_Tukey[,10]))
names(data_scaled_tuk)[names(data_scaled_tuk) == "V10"] <- "Potability"

data_scaled_sd = as.data.frame(cbind(scale(no_ouliers_incomplete_sd[,1:9]), no_ouliers_incomplete_sd[,10]))
names(data_scaled_sd)[names(data_scaled_sd) == "V10"] <- "Potability"

imp_1 <- kNN(data_scaled, k=100)
KNN_complete <- imp_1[,1:10]
imp_2 <- kNN(data_scaled_tuk, k=100)
KNN_Tukey <- imp_2[,1:10]
imp_3 <- kNN(data_scaled_sd, k=100)
KNN_sd <- imp_3[,1:10]


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
nrow(KNN_complete)
nrow(KNN_Tukey)
nrow(KNN_sd)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
nrow(remove_outliers_Tukey(KNN_complete))

nrow(remove_outliers_sd(KNN_complete))

nrow(remove_outliers_Tukey(KNN_Tukey))

nrow(remove_outliers_sd(KNN_sd))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
KNN_complete_Tuk <- remove_outliers_Tukey(KNN_complete)

KNN_complete_sd <- (KNN_complete)

KNN_Tukey_Tuk <- remove_outliers_Tukey(KNN_Tukey)

KNN_sd_sd <- remove_outliers_sd(KNN_sd)


## ----fig.height=12, fig.width=12, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------
library(GGally)
ggpairs(mF_Tukey_Tuk, columns = 1:9, ggplot2::aes(colour=as.factor(Potability)), progress = FALSE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(scale(mF_Tukey_Tuk[,1:9]))


## ----fig.height=12, fig.width=12, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------
library(GGally)
ggpairs(KNN_Tukey_Tuk, columns = 1:9, ggplot2::aes(colour=as.factor(Potability)), progress = FALSE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(scale(KNN_Tukey_Tuk[,1:9]))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(MVN)
result <- mvn(mF_sd_sd, multivariatePlot = "qq", showOutliers = TRUE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
result$multivariateNormality


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
result$univariateNormality


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(stats)
fligner.test(ph ~ as.factor(Potability), data=mF_sd_sd)
fligner.test(Hardness ~ as.factor(Potability), data=mF_sd_sd)
fligner.test(Solids ~ as.factor(Potability), data=mF_sd_sd)
fligner.test(Chloramines ~ as.factor(Potability), data=mF_sd_sd)
fligner.test(Sulfate ~ as.factor(Potability), data=mF_sd_sd)
fligner.test(Conductivity ~ as.factor(Potability), data=mF_sd_sd)
fligner.test(Organic_carbon ~ as.factor(Potability), data=mF_sd_sd)
fligner.test(Trihalomethanes ~ as.factor(Potability), data=mF_sd_sd)
fligner.test(Turbidity ~ as.factor(Potability), data=mF_sd_sd)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(mF_sd_sd,aes(x=as.factor(Potability),y=ph, col=ph)) + geom_boxplot() +
  geom_jitter(position=position_jitter(0.1)) + guides(col=FALSE)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(mF_sd_sd,aes(x=as.factor(Potability),y=Trihalomethanes, col=Trihalomethanes)) + geom_boxplot() +
  geom_jitter(position=position_jitter(0.1)) + guides(col=FALSE)

## ----fig.width=12-------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,1))                 # enable two panels per plot
  stripchart(ph ~ Potability, data=mF_sd_sd, pch="|", ylim=c(.5, 2.5))   # narrow plotting symbol
  stripchart(ph ~ Potability, data=mF_sd_sd, meth="j", ylim=c(.5, 2.5))  # jittered to mitigate overplotting
par(mfrow=c(1,1))                 # return to single-panel plotting

## ----fig.width=12-------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,1))                 # enable two panels per plot
  stripchart(Trihalomethanes  ~ Potability, data=mF_sd_sd, pch="|", ylim=c(.5, 2.5))   # narrow plotting symbol
  stripchart(Trihalomethanes  ~ Potability, data=mF_sd_sd, meth="j", ylim=c(.5, 2.5))  # jittered to mitigate overplotting
par(mfrow=c(1,1)) 


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(DescTools)

x.norm <- BoxCox(mF_sd_sd$Chloramines, lambda = BoxCoxLambda(mF_sd_sd$Chloramines))

par(mfrow=c(2,2))
qqnorm(mF_sd_sd$Chloramines, main="Original")
qqline(mF_sd_sd$Chloramines,col=2)
qqnorm(x.norm, main="Box-Cox")
qqline(x.norm,col=2)
hist(mF_sd_sd$Chloramines,main="Original")
hist(x.norm, main="Box-Cox")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
shapiro.test(mF_sd_sd$Chloramines)
shapiro.test(x.norm)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
fligner.test(Chloramines ~ as.factor(Potability), data=mF_sd_sd)
fligner.test(x.norm ~ as.factor(mF_sd_sd$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(MASS)
library(rcompanion)

Box = boxcox(mF_sd_sd$Chloramines ~ 1,              
             lambda = seq(-6,6,0.1)     
             )

Cox = data.frame(Box$x, Box$y)           

Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] 

Cox2[1,]                                 

lambda = Cox2[1, "Box.x"]

T_box = (mF_sd_sd$Chloramines ^ lambda - 1)/lambda   

plotNormalHistogram(mF_sd_sd$Chloramines)
plotNormalHistogram(T_box)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
shapiro.test(T_box)
fligner.test(T_box ~ as.factor(mF_sd_sd$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(rminer)
set.seed(100)

h<-holdout(mF_complete_Tuk$Potability,ratio=2/3,mode="stratified")
mF_complete_Tuk_train<-mF_complete_Tuk[h$tr,]
mF_complete_Tuk_test<-mF_complete_Tuk[h$ts,]
print(table(mF_complete_Tuk_train$Potability))
print(table(mF_complete_Tuk_test$Potability))

h<-holdout(mF_complete_sd$Potability,ratio=2/3,mode="stratified")
mF_complete_sd_train<-mF_complete_sd[h$tr,]
mF_complete_sd_test<-mF_complete_sd[h$ts,]
print(table(mF_complete_sd_train$Potability))
print(table(mF_complete_sd_test$Potability))

h<-holdout(mF_Tukey_Tuk$Potability,ratio=2/3,mode="stratified")
mF_Tukey_Tuk_train<-mF_Tukey_Tuk[h$tr,]
mF_Tukey_Tuk_test<-mF_Tukey_Tuk[h$ts,]
print(table(mF_Tukey_Tuk_train$Potability))
print(table(mF_Tukey_Tuk_test$Potability))

h<-holdout(mF_sd_sd$Potability,ratio=2/3,mode="stratified")
mF_sd_sd_train<-mF_sd_sd[h$tr,]
mF_sd_sd_test<-mF_sd_sd[h$ts,]
print(table(mF_sd_sd_train$Potability))
print(table(mF_sd_sd_test$Potability))

h<-holdout(KNN_complete_Tuk$Potability,ratio=2/3,mode="stratified")
KNN_complete_Tuk_train<-KNN_complete_Tuk[h$tr,]
KNN_complete_Tuk_test<-KNN_complete_Tuk[h$ts,]
print(table(KNN_complete_Tuk_train$Potability))
print(table(KNN_complete_Tuk_test$Potability))

h<-holdout(KNN_complete_sd$Potability,ratio=2/3,mode="stratified")
KNN_complete_sd_train<-KNN_complete_sd[h$tr,]
KNN_complete_sd_test<-KNN_complete_sd[h$ts,]
print(table(KNN_complete_sd_train$Potability))
print(table(KNN_complete_sd_test$Potability))

h<-holdout(KNN_Tukey_Tuk$Potability,ratio=2/3,mode="stratified")
KNN_Tukey_Tuk_train<-KNN_Tukey_Tuk[h$tr,]
KNN_Tukey_Tuk_test<-KNN_Tukey_Tuk[h$ts,]
print(table(KNN_Tukey_Tuk_train$Potability))
print(table(KNN_Tukey_Tuk_test$Potability))

h<-holdout(KNN_sd_sd$Potability,ratio=2/3,mode="stratified")
KNN_sd_sd_train<-KNN_sd_sd[h$tr,]
KNN_sd_sd_test<-KNN_sd_sd[h$ts,]
print(table(KNN_sd_sd_train$Potability))
print(table(KNN_sd_sd_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret)
library(parallel)
library(doParallel)

set.seed(100)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

model_1 <- caret::train(mF_complete_Tuk_train[, -10], as.factor(mF_complete_Tuk_train$Potability), 
                 method = "rf", trControl = caret::trainControl(method = "cv", p = 0.8, number = 5))

stopCluster(cl)
model_1


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict <- predict(model_1, mF_complete_Tuk_test)
confusionMatrix(predict, as.factor(mF_complete_Tuk_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
plot(model_1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
plot(varImp(model_1, scale = FALSE)) 


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest)

set.seed(100)

model_1.1<-randomForest(as.factor(Potability)~.,mF_complete_Tuk_train,ntree=10000)
model_1.1


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf_2 <- predict(model_1.1, mF_complete_Tuk_test)
cm <- confusionMatrix(predict_rf_2, as.factor(mF_complete_Tuk_test$Potability))
cm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
overall <- cm$overall
overall.accuracy <- overall['Accuracy'] 
accuracy_data <- data.frame("Nombre"="model_1", "accuracy"=c(overall.accuracy))
accuracy_data


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

model_2<-randomForest(as.factor(Potability)~.,mF_complete_sd_train,ntree=10000)
model_2


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_2, mF_complete_sd_test)
cm <- confusionMatrix(predict_rf, as.factor(mF_complete_sd_test$Potability))
cm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
overall <- cm$overall
overall.accuracy <- overall['Accuracy'] 
temp <- data.frame("Nombre"="model_2", "accuracy"=c(overall.accuracy))
accuracy_data <- rbind(accuracy_data, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

model_3<-randomForest(as.factor(Potability)~.,mF_Tukey_Tuk_train,ntree=10000)
model_3


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_3, mF_Tukey_Tuk_test)
cm <- confusionMatrix(predict_rf, as.factor(mF_Tukey_Tuk_test$Potability))
cm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
overall <- cm$overall
overall.accuracy <- overall['Accuracy'] 
temp <- data.frame("Nombre"="model_3", "accuracy"=c(overall.accuracy))
accuracy_data <- rbind(accuracy_data, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

model_4<-randomForest(as.factor(Potability)~.,mF_sd_sd_train,ntree=10000)
model_4


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_4, mF_sd_sd_test)
cm <- confusionMatrix(predict_rf, as.factor(mF_sd_sd_test$Potability))
cm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
overall <- cm$overall
overall.accuracy <- overall['Accuracy'] 
temp <- data.frame("Nombre"="model_4", "accuracy"=c(overall.accuracy))
accuracy_data <- rbind(accuracy_data, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

model_5<-randomForest(as.factor(Potability)~.,KNN_complete_Tuk_train,ntree=10000)
model_5


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_5, KNN_complete_Tuk_test)
cm <- confusionMatrix(predict_rf, as.factor(KNN_complete_Tuk_test$Potability))
cm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
overall <- cm$overall
overall.accuracy <- overall['Accuracy'] 
temp <- data.frame("Nombre"="model_5", "accuracy"=c(overall.accuracy))
accuracy_data <- rbind(accuracy_data, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

model_6<-randomForest(as.factor(Potability)~.,KNN_complete_sd_train,ntree=10000, mtry=3)
model_6


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_6, KNN_complete_sd_test)
cm <- confusionMatrix(predict_rf, as.factor(KNN_complete_sd_test$Potability))
cm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
overall <- cm$overall
overall.accuracy <- overall['Accuracy'] 
temp <- data.frame("Nombre"="model_6", "accuracy"=c(overall.accuracy))
accuracy_data <- rbind(accuracy_data, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

model_7<-randomForest(as.factor(Potability)~.,KNN_Tukey_Tuk_train,ntree=10000)
model_7


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_7, KNN_Tukey_Tuk_test)
cm <- confusionMatrix(predict_rf, as.factor(KNN_Tukey_Tuk_test$Potability))
cm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
overall <- cm$overall
overall.accuracy <- overall['Accuracy'] 
temp <- data.frame("Nombre"="model_7", "accuracy"=c(overall.accuracy))
accuracy_data <- rbind(accuracy_data, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

model_8<-randomForest(as.factor(Potability)~.,KNN_sd_sd_train,ntree=10000)
model_8


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_8, KNN_sd_sd_test)
cm <- confusionMatrix(predict_rf, as.factor(KNN_sd_sd_test$Potability))
cm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
overall <- cm$overall
overall.accuracy <- overall['Accuracy'] 
temp <- data.frame("Nombre"="model_8", "accuracy"=c(overall.accuracy))
accuracy_data <- rbind(accuracy_data, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
h<-holdout(mF_Tukey$Potability,ratio=2/3,mode="stratified")
mF_Tukey_train<-mF_Tukey[h$tr,]
mF_Tukey_test<-mF_Tukey[h$ts,]
print(table(mF_Tukey_train$Potability))
print(table(mF_Tukey_test$Potability))

h<-holdout(mF_sd$Potability,ratio=2/3,mode="stratified")
mF_sd_train<-mF_sd[h$tr,]
mF_sd_test<-mF_sd[h$ts,]
print(table(mF_sd_train$Potability))
print(table(mF_sd_test$Potability))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

model_test_1<-randomForest(as.factor(Potability)~.,mF_Tukey_train,ntree=10000)
model_test_1


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_test_1, mF_Tukey_test)
confusionMatrix(predict_rf, as.factor(mF_Tukey_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

model_test_2<-randomForest(as.factor(Potability)~.,mF_sd_train,ntree=10000)
model_test_2


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_test_2, mF_sd_test)
confusionMatrix(predict_rf, as.factor(mF_sd_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
varImpPlot(model_6)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
KNN_complete_sd_train$Potability <- as.factor(KNN_complete_sd_train$Potability)
KNN_complete_sd_test$Potability <- as.factor(KNN_complete_sd_test$Potability)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(ranger)
library(tidymodels)
library(parallel)
library(doParallel)

# DEFINICIÓN DEL MODELO Y DE LOS HIPERPARÁMETROS A OPTIMIZAR
# ==============================================================================
modelo <- rand_forest(
             mode  = "classification",
             mtry  = tune(),
             trees = tune()
          ) %>%
          set_engine(
            engine     = "ranger",
            max.depth  = tune(),
            importance = "none",
            seed       = 100
          )

control <- control_resamples(save_pred = TRUE)
# DEFINICIÓN DEL PREPROCESADO
# ==============================================================================
# En este caso no hay preprocesado, por lo que el transformer solo contiene
# la definición de la fórmula y los datos de entrenamiento.
transformer <- recipe(
                  formula = Potability ~ .,
                  data    =  KNN_complete_sd_train
               )

# DEFINICIÓN DE LA ESTRATEGIA DE VALIDACIÓN Y CREACIÓN DE PARTICIONES
# ==============================================================================
set.seed(100)
cv_folds <- vfold_cv(
              data    = KNN_complete_sd_train,
              v       = 5,
              strata  = Potability
            )

# WORKFLOW
# ==============================================================================
workflow_modelado <- workflow() %>%
                     add_recipe(transformer) %>%
                     add_model(modelo)
                     

# GRID DE HIPERPARÁMETROS
# ==============================================================================
hiperpar_grid <- expand_grid(
                  'trees'     = c(100, 500, 1000, 2000),
                  'mtry'      = c( 4, 5),
                  'max.depth' = c(1, 3, 10, 20, 40, 60, 80, 100)
                 )

# EJECUCIÓN DE LA OPTIMIZACIÓN DE HIPERPARÁMETROS
# ==============================================================================
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

grid_fit <- tune_grid(
              object    = workflow_modelado,
              resamples = cv_folds,
              metrics   = metric_set(yardstick::accuracy),
              grid      = hiperpar_grid,
              control = control
            )

stopCluster(cl)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
grid_fit %>% collect_metrics(summarize = FALSE) %>% head()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggpubr)
p1 <- ggplot(
        data = grid_fit %>% collect_metrics(summarize = FALSE),
        aes(x = .estimate, fill = .metric)) +
      geom_density(alpha = 0.5) +
      theme_bw() 
p2 <- ggplot(
        data = grid_fit %>% collect_metrics(summarize = FALSE),
        aes(x = .metric, y = .estimate, fill = .metric, color = .metric)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.1) +
      geom_jitter(width = 0.05, alpha = 0.3) +
      coord_flip() +
      theme_bw() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
ggarrange(p1, p2, nrow = 2, common.legend = TRUE, align = "v") %>% 
annotate_figure(
  top = text_grob("Distribución errores de validación cruzada", size = 15)
)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
show_best(grid_fit, metric="accuracy")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
mejores_hiperpar <- select_best(grid_fit, metric="accuracy")

modelo_final_fit <- finalize_workflow(
                        x = workflow_modelado,
                        parameters = mejores_hiperpar
                    ) %>%
                    fit(
                      data = KNN_complete_sd_train
                    ) %>%
                    pull_workflow_fit()


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predicciones <- modelo_final_fit %>%
                predict(new_data = KNN_complete_sd_test)

predicciones <- predicciones %>% 
                bind_cols(KNN_complete_sd_test %>% dplyr::select(Potability))

accuracy_test  <- accuracy(
                     data     = predicciones,
                     truth    = Potability,
                     estimate = .pred_class,
                     na_rm    = TRUE
                  )
accuracy_test


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
mat_confusion <- predicciones %>%
                 conf_mat(
                   truth     = Potability,
                   estimate  = .pred_class
                 )
mat_confusion


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest)
set.seed(100)
model_6_1<-randomForest(as.factor(Potability)~Sulfate+ph+Hardness+Solids+Chloramines+Turbidity,KNN_complete_sd_train,ntree=2000, mtry=4)
model_6_1


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_6_1, KNN_complete_sd_test)
confusionMatrix(predict_rf, as.factor(KNN_complete_sd_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest)
set.seed(100)
model_6_2<-randomForest(as.factor(Potability)~Sulfate+ph+Hardness+Solids+Chloramines+Organic_carbon,KNN_complete_sd_train,ntree=2000, mtry=4)
model_6_2

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_6_2, KNN_complete_sd_test)
cm <- confusionMatrix(predict_rf, as.factor(KNN_complete_sd_test$Potability))
cm

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
overall <- cm$overall
overall.accuracy <- overall['Accuracy'] 
temp <- data.frame("Nombre"="model_6_mod", "accuracy"=c(overall.accuracy))
accuracy_data <- rbind(accuracy_data, temp)



## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest)
set.seed(100)
model_6_3<-randomForest(as.factor(Potability)~Sulfate+ph+Hardness+Solids+Chloramines,KNN_complete_sd_train,ntree=2000, mtry=4)
model_6_3

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
predict_rf <- predict(model_6_3, KNN_complete_sd_test)
confusionMatrix(predict_rf, as.factor(KNN_complete_sd_test$Potability))


## ----fig.width=10-------------------------------------------------------------------------------------------------------------------------------------------
ggplot(accuracy_data, aes(x=as.factor(Nombre), y=accuracy, fill=as.factor(Nombre))) + 
  geom_bar(stat = "identity")  + geom_text(aes(label=round(accuracy*100, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(rminer)
set.seed(100)

h<-holdout(mF_complete_Tuk$Potability,ratio=2/3,mode="stratified")
mF_complete_Tuk_train<-mF_complete_Tuk[h$tr,]
mF_complete_Tuk_test<-mF_complete_Tuk[h$ts,]
print(table(mF_complete_Tuk_train$Potability))
print(table(mF_complete_Tuk_test$Potability))

h<-holdout(mF_complete_sd$Potability,ratio=2/3,mode="stratified")
mF_complete_sd_train<-mF_complete_sd[h$tr,]
mF_complete_sd_test<-mF_complete_sd[h$ts,]
print(table(mF_complete_sd_train$Potability))
print(table(mF_complete_sd_test$Potability))

h<-holdout(mF_Tukey_Tuk$Potability,ratio=2/3,mode="stratified")
mF_Tukey_Tuk_train<-mF_Tukey_Tuk[h$tr,]
mF_Tukey_Tuk_test<-mF_Tukey_Tuk[h$ts,]
print(table(mF_Tukey_Tuk_train$Potability))
print(table(mF_Tukey_Tuk_test$Potability))

h<-holdout(mF_sd_sd$Potability,ratio=2/3,mode="stratified")
mF_sd_sd_train<-mF_sd_sd[h$tr,]
mF_sd_sd_test<-mF_sd_sd[h$ts,]
print(table(mF_sd_sd_train$Potability))
print(table(mF_sd_sd_test$Potability))

h<-holdout(KNN_complete_Tuk$Potability,ratio=2/3,mode="stratified")
KNN_complete_Tuk_train<-KNN_complete_Tuk[h$tr,]
KNN_complete_Tuk_test<-KNN_complete_Tuk[h$ts,]
print(table(KNN_complete_Tuk_train$Potability))
print(table(KNN_complete_Tuk_test$Potability))

h<-holdout(KNN_complete_sd$Potability,ratio=2/3,mode="stratified")
KNN_complete_sd_train<-KNN_complete_sd[h$tr,]
KNN_complete_sd_test<-KNN_complete_sd[h$ts,]
print(table(KNN_complete_sd_train$Potability))
print(table(KNN_complete_sd_test$Potability))

h<-holdout(KNN_Tukey_Tuk$Potability,ratio=2/3,mode="stratified")
KNN_Tukey_Tuk_train<-KNN_Tukey_Tuk[h$tr,]
KNN_Tukey_Tuk_test<-KNN_Tukey_Tuk[h$ts,]
print(table(KNN_Tukey_Tuk_train$Potability))
print(table(KNN_Tukey_Tuk_test$Potability))

h<-holdout(KNN_sd_sd$Potability,ratio=2/3,mode="stratified")
KNN_sd_sd_train<-KNN_sd_sd[h$tr,]
KNN_sd_sd_test<-KNN_sd_sd[h$ts,]
print(table(KNN_sd_sd_train$Potability))
print(table(KNN_sd_sd_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret)

# Seleccionamos el método, Cross Validation y el número de folds, 5.
trctrl <- trainControl(method = "cv", number = 5)

# Lanzamos múltiples modelos con distintos valores para cada uno de los parámetros.
set.seed(100)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

tune_grid <- expand.grid(nrounds=c(100,300,500), 
                         max_depth = c(5, 10, 15),
                         eta = c(0.05, 0.2),
                         gamma = c(0.01, 1),
                         colsample_bytree = c(1),
                         subsample = c(1),
                         min_child_weight = c(1))

model_xgb_1 <- train(as.factor(Potability) ~., data = mF_complete_Tuk_train, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

stopCluster(cl)

# Resultados gráficos de los modelos, que nos permitirán elegir los mejores valores de los parámetros.
plot(model_xgb_1)

# Test y matriz de confusión
test_predict <- predict(model_xgb_1, mF_complete_Tuk_test)
confusionMatrix(test_predict, as.factor(mF_complete_Tuk_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(xgboost)

set.seed(100)

train.data = as.matrix(mF_complete_Tuk_train[, -10])
train.label = mF_complete_Tuk_train[, 10]
test.data = as.matrix(mF_complete_Tuk_test[, -10])
test.label = mF_complete_Tuk_test[, 10]

xgb.train = xgb.DMatrix(data=train.data,label=(train.label))
xgb.test = xgb.DMatrix(data=test.data,label=(test.label))

# Definición de los parámetros seleccionados
num_class = length(unique(mF_complete_Tuk_train$Potability))
params = list(
  booster="gbtree",
  eta = 0.05,
  max_depth = 15,
  gamma = 1,
  subsample = 1,
  colsample_bytree = 1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Entrenamiento del modelo
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds = 100
)

# Resultados
xgb.fit

# Predicción
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = unique(mF_complete_Tuk_train$Potability)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = unique(mF_complete_Tuk_train$Potability)[test.label+1]

# Calculate the final accuracy
result_xgb1 = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result_xgb1)))

accuracy_data2<- data.frame("Nombre"="model_xgb_1", "accuracy"=result_xgb1)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Seleccionamos el método, Cross Validation y el número de folds, 5.
trctrl <- trainControl(method = "cv", number = 5)

# Lanzamos múltiples modelos con distintos valores para cada uno de los parámetros.
set.seed(100)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

tune_grid <- expand.grid(nrounds=c(100,300,500), 
                         max_depth = c(5, 10, 15),
                         eta = c(0.05, 0.2),
                         gamma = c(0.01, 1),
                         colsample_bytree = c(1),
                         subsample = c(1),
                         min_child_weight = c(1))

model_xgb_2 <- train(as.factor(Potability) ~., data = mF_complete_sd_train, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

stopCluster(cl)

# Resultados de los modelos.
plot(model_xgb_2)

# Test y matriz de confusión
test_predict <- predict(model_xgb_2, mF_complete_sd_test)
confusionMatrix(test_predict, as.factor(mF_complete_sd_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

train.data = as.matrix(mF_complete_sd_train[, -10])
train.label = mF_complete_sd_train[, 10]
test.data = as.matrix(mF_complete_sd_test[, -10])
test.label = mF_complete_sd_test[, 10]

xgb.train = xgb.DMatrix(data=train.data,label=(train.label))
xgb.test = xgb.DMatrix(data=test.data,label=(test.label))

# Definición de los parámetros seleccionados
num_class = length(unique(mF_complete_sd_train$Potability))
params = list(
  booster="gbtree",
  eta = 0.05,
  max_depth = 15,
  gamma = 0.01,
  subsample = 1,
  colsample_bytree = 1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Entrenamiento del modelo
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds = 100
)

# Resultados
xgb.fit

# Predicción
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = unique(mF_complete_sd_train$Potability)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = unique(mF_complete_sd_train$Potability)[test.label+1]

# Calculate the final accuracy
result_xgb2 = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result_xgb2)))

temp <- data.frame("Nombre"="model_xgb_2", "accuracy"=c(result_xgb2))
accuracy_data2 <- rbind(accuracy_data2, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Seleccionamos el método, Cross Validation y el número de folds, 5.
trctrl <- trainControl(method = "cv", number = 5)

# Lanzamos múltiples modelos con distintos valores para cada uno de los parámetros.
set.seed(100)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

tune_grid <- expand.grid(nrounds=c(100,300,500), 
                         max_depth = c(5, 10, 15),
                         eta = c(0.05, 0.2),
                         gamma = c(0.01, 1),
                         colsample_bytree = c(1),
                         subsample = c(1),
                         min_child_weight = c(1))

model_xgb_3 <- train(as.factor(Potability) ~., data = mF_Tukey_Tuk_train, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

stopCluster(cl)

# Resultados de los modelos.
plot(model_xgb_3)

# Test y matriz de confusión
test_predict <- predict(model_xgb_3, mF_Tukey_Tuk_test)
confusionMatrix(test_predict, as.factor(mF_Tukey_Tuk_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

train.data = as.matrix(mF_Tukey_Tuk_train[, -10])
train.label = mF_Tukey_Tuk_train[, 10]
test.data = as.matrix(mF_Tukey_Tuk_test[, -10])
test.label = mF_Tukey_Tuk_test[, 10]

xgb.train = xgb.DMatrix(data=train.data,label=(train.label))
xgb.test = xgb.DMatrix(data=test.data,label=(test.label))

# Definición de los parámetros seleccionados
num_class = length(unique(mF_Tukey_Tuk_train$Potability))
params = list(
  booster="gbtree",
  eta = 0.05,
  max_depth = 15,
  gamma = 0.01,
  subsample = 1,
  colsample_bytree = 1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Entrenamiento del modelo
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds = 500
)

# Resultados
xgb.fit

# Predicción
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = unique(mF_Tukey_Tuk_train$Potability)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = unique(mF_Tukey_Tuk_train$Potability)[test.label+1]

# Calculate the final accuracy
result_xgb3 = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result_xgb3)))

temp <- data.frame("Nombre"="model_xgb_3", "accuracy"=c(result_xgb3))
accuracy_data2 <- rbind(accuracy_data2, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Seleccionamos el método, Cross Validation y el número de folds, 5.
trctrl <- trainControl(method = "cv", number = 5)

# Lanzamos múltiples modelos con distintos valores para cada uno de los parámetros.
set.seed(100)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

tune_grid <- expand.grid(nrounds=c(100,300,500), 
                         max_depth = c(5, 10, 15),
                         eta = c(0.05, 0.2),
                         gamma = c(0.01, 1),
                         colsample_bytree = c(1),
                         subsample = c(1),
                         min_child_weight = c(1))

model_xgb_4 <- train(as.factor(Potability) ~., data = mF_sd_sd_train, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

stopCluster(cl)

# Resultados de los modelos.
plot(model_xgb_4)

# Test y matriz de confusión
test_predict <- predict(model_xgb_4, mF_sd_sd_test)
confusionMatrix(test_predict, as.factor(mF_sd_sd_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

train.data = as.matrix(mF_sd_sd_train[, -10])
train.label = mF_sd_sd_train[, 10]
test.data = as.matrix(mF_sd_sd_test[, -10])
test.label = mF_sd_sd_test[, 10]

xgb.train = xgb.DMatrix(data=train.data,label=(train.label))
xgb.test = xgb.DMatrix(data=test.data,label=(test.label))

# Definición de los parámetros seleccionados
num_class = length(unique(mF_sd_sd_train$Potability))
params = list(
  booster="gbtree",
  eta = 0.2,
  max_depth = 15,
  gamma = 1,
  subsample = 1,
  colsample_bytree = 1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Entrenamiento del modelo
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds = 500
)

# Resultados
xgb.fit

# Predicción
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = unique(mF_sd_sd_train$Potability)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = unique(mF_sd_sd_train$Potability)[test.label+1]

# Calculate the final accuracy
result_xgb4 = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result_xgb4)))

temp <- data.frame("Nombre"="model_xgb_4", "accuracy"=c(result_xgb4))
accuracy_data2 <- rbind(accuracy_data2, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Seleccionamos el método, Cross Validation y el número de folds, 5.
trctrl <- trainControl(method = "cv", number = 5)

# Lanzamos múltiples modelos con distintos valores para cada uno de los parámetros.
set.seed(100)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

tune_grid <- expand.grid(nrounds=c(100,300,500), 
                         max_depth = c(5, 10, 15),
                         eta = c(0.05, 0.2),
                         gamma = c(0.01, 1),
                         colsample_bytree = c(1),
                         subsample = c(1),
                         min_child_weight = c(1))

model_xgb_5 <- train(as.factor(Potability) ~., data = KNN_complete_Tuk_train, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

stopCluster(cl)

# Resultados de los modelos.
plot(model_xgb_5)

# Test y matriz de confusión
test_predict <- predict(model_xgb_5, KNN_complete_Tuk_test)
confusionMatrix(test_predict, as.factor(KNN_complete_Tuk_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

train.data = as.matrix(KNN_complete_Tuk_train[, -10])
train.label = KNN_complete_Tuk_train[, 10]
test.data = as.matrix(KNN_complete_Tuk_test[, -10])
test.label = KNN_complete_Tuk_test[, 10]

xgb.train = xgb.DMatrix(data=train.data,label=(train.label))
xgb.test = xgb.DMatrix(data=test.data,label=(test.label))

# Definición de los parámetros seleccionados
num_class = length(unique(KNN_complete_Tuk_train$Potability))
params = list(
  booster="gbtree",
  eta = 0.05,
  max_depth = 10,
  gamma = 1,
  subsample = 1,
  colsample_bytree = 1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Entrenamiento del modelo
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds = 500
)

# Resultados
xgb.fit

# Predicción
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = unique(KNN_complete_Tuk_train$Potability)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = unique(KNN_complete_Tuk_train$Potability)[test.label+1]

# Calculate the final accuracy
result_xgb5 = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result_xgb5)))

temp <- data.frame("Nombre"="model_xgb_5", "accuracy"=c(result_xgb5))
accuracy_data2 <- rbind(accuracy_data2, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Seleccionamos el método, Cross Validation y el número de folds, 5.
trctrl <- trainControl(method = "cv", number = 5)

# Lanzamos múltiples modelos con distintos valores para cada uno de los parámetros.
set.seed(100)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

tune_grid <- expand.grid(nrounds=c(100,300,500), 
                         max_depth = c(5, 10, 15),
                         eta = c(0.05, 0.2),
                         gamma = c(0.01, 1),
                         colsample_bytree = c(1),
                         subsample = c(1),
                         min_child_weight = c(1))

model_xgb_6 <- train(as.factor(Potability) ~., data = KNN_complete_sd_train, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

stopCluster(cl)

# Resultados de los modelos.
plot(model_xgb_6)

# Test y matriz de confusión
test_predict <- predict(model_xgb_6, KNN_complete_sd_test)
confusionMatrix(test_predict, as.factor(KNN_complete_sd_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

train.data = as.matrix(KNN_complete_sd_train[, -10])
train.label = KNN_complete_sd_train[, 10]
test.data = as.matrix(KNN_complete_sd_test[, -10])
test.label = KNN_complete_sd_test[, 10]

xgb.train = xgb.DMatrix(data=train.data,label=(train.label))
xgb.test = xgb.DMatrix(data=test.data,label=(test.label))

# Definición de los parámetros seleccionados
num_class = length(unique(KNN_complete_sd_train$Potability))
params = list(
  booster="gbtree",
  eta = 0.05,
  max_depth = 10,
  gamma = 0.01,
  subsample = 1,
  colsample_bytree = 1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Entrenamiento del modelo
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds = 500
)

# Resultados
xgb.fit

# Predicción
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = unique(KNN_complete_sd_train$Potability)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = unique(KNN_complete_sd_train$Potability)[test.label+1]

# Calculate the final accuracy
result_xgb6 = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result_xgb6)))

temp <- data.frame("Nombre"="model_xgb_6", "accuracy"=c(result_xgb6))
accuracy_data2 <- rbind(accuracy_data2, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Seleccionamos el método, Cross Validation y el número de folds, 5.
trctrl <- trainControl(method = "cv", number = 5)

# Lanzamos múltiples modelos con distintos valores para cada uno de los parámetros.
set.seed(100)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

tune_grid <- expand.grid(nrounds=c(100,300,500), 
                         max_depth = c(5, 10, 15),
                         eta = c(0.05, 0.2),
                         gamma = c(0.01, 1),
                         colsample_bytree = c(1),
                         subsample = c(1),
                         min_child_weight = c(1))

model_xgb_7 <- train(as.factor(Potability) ~., data = KNN_Tukey_Tuk_train, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

stopCluster(cl)

# Resultados de los modelos.
plot(model_xgb_7)

# Test y matriz de confusión
test_predict <- predict(model_xgb_7, KNN_Tukey_Tuk_test)
confusionMatrix(test_predict, as.factor(KNN_Tukey_Tuk_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

train.data = as.matrix(KNN_Tukey_Tuk_train[, -10])
train.label = KNN_Tukey_Tuk_train[, 10]
test.data = as.matrix(KNN_Tukey_Tuk_test[, -10])
test.label = KNN_Tukey_Tuk_test[, 10]

xgb.train = xgb.DMatrix(data=train.data,label=(train.label))
xgb.test = xgb.DMatrix(data=test.data,label=(test.label))

# Definición de los parámetros seleccionados
num_class = length(unique(KNN_Tukey_Tuk_train$Potability))
params = list(
  booster="gbtree",
  eta = 0.05,
  max_depth = 15,
  gamma = 0.01,
  subsample = 1,
  colsample_bytree = 1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Entrenamiento del modelo
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds = 500
)

# Resultados
xgb.fit

# Predicción
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = unique(KNN_Tukey_Tuk_train$Potability)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = unique(KNN_Tukey_Tuk_train$Potability)[test.label+1]

# Calculate the final accuracy
result_xgb7 = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result_xgb7)))

temp <- data.frame("Nombre"="model_xgb_7", "accuracy"=c(result_xgb7))
accuracy_data2 <- rbind(accuracy_data2, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Seleccionamos el método, Cross Validation y el número de folds, 5.
trctrl <- trainControl(method = "cv", number = 5)

# Lanzamos múltiples modelos con distintos valores para cada uno de los parámetros.
set.seed(100)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

tune_grid <- expand.grid(nrounds=c(100,300,500), 
                         max_depth = c(5, 10, 15),
                         eta = c(0.05, 0.2),
                         gamma = c(0.01, 1),
                         colsample_bytree = c(1),
                         subsample = c(1),
                         min_child_weight = c(1))

model_xgb_8 <- train(as.factor(Potability) ~., data = KNN_sd_sd_train, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

stopCluster(cl)

# Resultados de los modelos.
plot(model_xgb_8)

# Test y matriz de confusión
test_predict <- predict(model_xgb_8, KNN_sd_sd_test)
confusionMatrix(test_predict, as.factor(KNN_sd_sd_test$Potability))


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)

train.data = as.matrix(KNN_sd_sd_train[, -10])
train.label = KNN_sd_sd_train[, 10]
test.data = as.matrix(KNN_sd_sd_test[, -10])
test.label = KNN_sd_sd_test[, 10]

xgb.train = xgb.DMatrix(data=train.data,label=(train.label))
xgb.test = xgb.DMatrix(data=test.data,label=(test.label))

# Definición de los parámetros seleccionados
num_class = length(unique(KNN_sd_sd_train$Potability))
params = list(
  booster="gbtree",
  eta = 0.05,
  max_depth = 15,
  gamma = 1,
  subsample = 1,
  colsample_bytree = 1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Entrenamiento del modelo
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds = 100
)

# Resultados
xgb.fit

# Predicción
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = unique(KNN_sd_sd_train$Potability)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = unique(KNN_sd_sd_train$Potability)[test.label+1]

# Calculate the final accuracy
result_xgb8 = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result_xgb8)))

temp <- data.frame("Nombre"="model_xgb_8", "accuracy"=c(result_xgb8))
accuracy_data2 <- rbind(accuracy_data2, temp)

## ----fig.width=10-------------------------------------------------------------------------------------------------------------------------------------------
ggplot(accuracy_data2, aes(x=as.factor(Nombre), y=accuracy, fill=as.factor(Nombre))) + 
  geom_bar(stat = "identity") + geom_text(aes(label=round(accuracy*100, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)

train.data  <- mF_complete_Tuk_train
test.data <- mF_complete_Tuk_test

# Creamos el modelo
model <- glm( Potability ~., data = train.data, family = binomial)

# Mostramos el resumen de los resultados del modelo
summary(model)

# Creamos las predicciones
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Observamos la exactitud del modelo
result <- mean(predicted.classes == test.data$Potability)
result

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
accuracy_data_3<- data.frame("Nombre"="model_1_reg", "accuracy"=result)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)

train.data  <- mF_complete_sd_train
test.data <- mF_complete_sd_test

# Creamos el modelo
model <- glm( Potability ~., data = train.data, family = binomial)

# Mostramos el resumen de los resultados del modelo
summary(model)

# Creamos las predicciones
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Observamos la exactitud del modelo
result <- mean(predicted.classes == test.data$Potability)
result

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
temp <- data.frame("Nombre"="model_2_reg", "accuracy"=c(result))
accuracy_data_3 <- rbind(accuracy_data_3, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)

train.data  <- mF_Tukey_Tuk_train
test.data <- mF_Tukey_Tuk_test

# Creamos el modelo
model <- glm( Potability ~., data = train.data, family = binomial)

# Mostramos el resumen de los resultados del modelo
summary(model)

# Creamos las predicciones
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Observamos la exactitud del modelo
result <- mean(predicted.classes == test.data$Potability)
result

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
temp <- data.frame("Nombre"="model_3_reg", "accuracy"=c(result))
accuracy_data_3 <- rbind(accuracy_data_3, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)

train.data  <- mF_sd_sd_train
test.data <- mF_sd_sd_test

# Creamos el modelo
model <- glm( Potability ~., data = train.data, family = binomial)

# Mostramos el resumen de los resultados del modelo
summary(model)

# Creamos las predicciones
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Observamos la exactitud del modelo
result <- mean(predicted.classes == test.data$Potability)
result

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
temp <- data.frame("Nombre"="model_4_reg", "accuracy"=c(result))
accuracy_data_3 <- rbind(accuracy_data_3, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)

train.data  <- KNN_complete_Tuk_train
test.data <- KNN_complete_Tuk_test

# Creamos el modelo
model <- glm( Potability ~., data = train.data, family = binomial)

# Mostramos el resumen de los resultados del modelo
summary(model)

# Creamos las predicciones
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Observamos la exactitud del modelo
result <- mean(predicted.classes == test.data$Potability)
result

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
temp <- data.frame("Nombre"="model_5_reg", "accuracy"=c(result))
accuracy_data_3 <- rbind(accuracy_data_3, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)

train.data  <- KNN_complete_sd_train
test.data <- KNN_complete_sd_test

# Creamos el modelo
model <- glm( Potability ~., data = train.data, family = binomial)

# Mostramos el resumen de los resultados del modelo
summary(model)

# Creamos las predicciones
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Observamos la exactitud del modelo
result <- mean(predicted.classes == test.data$Potability)
result

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
temp <- data.frame("Nombre"="model_6_reg", "accuracy"=c(result))
accuracy_data_3 <- rbind(accuracy_data_3, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)

train.data  <- KNN_Tukey_Tuk_train
test.data <- KNN_Tukey_Tuk_test

# Creamos el modelo
model <- glm( Potability ~., data = train.data, family = binomial)

# Mostramos el resumen de los resultados del modelo
summary(model)

# Creamos las predicciones
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Observamos la exactitud del modelo
result <- mean(predicted.classes == test.data$Potability)
result

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
temp <- data.frame("Nombre"="model_7_reg", "accuracy"=c(result))
accuracy_data_3 <- rbind(accuracy_data_3, temp)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)

train.data  <- KNN_sd_sd_train
test.data <- KNN_sd_sd_test

# Creamos el modelo
model <- glm( Potability ~., data = train.data, family = binomial)

# Mostramos el resumen de los resultados del modelo
summary(model)

# Creamos las predicciones
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Observamos la exactitud del modelo
result <- mean(predicted.classes == test.data$Potability)
result

## -----------------------------------------------------------------------------------------------------------------------------------------------------------
temp <- data.frame("Nombre"="model_8_reg", "accuracy"=c(result))
accuracy_data_3 <- rbind(accuracy_data_3, temp)

## ----fig.width=10-------------------------------------------------------------------------------------------------------------------------------------------
ggplot(accuracy_data_3, aes(x=as.factor(Nombre), y=accuracy, fill=as.factor(Nombre))) + 
  geom_bar(stat = "identity") + geom_text(aes(label=round(accuracy*100, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25)


## ----fig.width=10-------------------------------------------------------------------------------------------------------------------------------------------
ggplot(accuracy_data, aes(x=as.factor(Nombre), y=accuracy, fill=as.factor(Nombre))) + 
  geom_bar(stat = "identity")  + geom_text(aes(label=round(accuracy*100, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25)

## ----fig.width=10-------------------------------------------------------------------------------------------------------------------------------------------
ggplot(accuracy_data2, aes(x=as.factor(Nombre), y=accuracy, fill=as.factor(Nombre))) + 
  geom_bar(stat = "identity")  + geom_text(aes(label=round(accuracy*100, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25)

## ----fig.width=10-------------------------------------------------------------------------------------------------------------------------------------------
ggplot(accuracy_data_3, aes(x=as.factor(Nombre), y=accuracy, fill=as.factor(Nombre))) + 
  geom_bar(stat = "identity") + geom_text(aes(label=round(accuracy*100, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
accuracy_data_final <- data.frame("Nombre"= c(accuracy_data[9,1], accuracy_data2[6,1], accuracy_data_3[1,1]),
                                  "accuracy"= c(accuracy_data[9,2], accuracy_data2[6,2], accuracy_data_3[1,2]))

ggplot(accuracy_data_final, aes(x=as.factor(Nombre), y=accuracy, fill=as.factor(Nombre))) + 
  geom_bar(stat = "identity") + geom_text(aes(label=round(accuracy*100, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25)

