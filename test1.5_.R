#carregando bibliotecas
library(e1071)
library(caret)
library(dplyr)
library(randomForest)
library(xgboost)

#Carrega datasets
df <- read.csv("Dataset_model.csv",sep=',')
df <- na.exclude(df) 
submit <- read.csv("Submit.csv",sep=';')

#Utilizando Random Forest para obter importância das variáveis
modelo_RF <- randomForest(acertou ~ .,
                          data = df[c(1:5000),], 
                          ntree = 50, nodesize = 10, importance = T)
varImpPlot(modelo_RF)

#Count da variável 'difficulty' (teoricamente a mais importante)  
table(df$difficulty)

#Processa e separa em train e test com base nos níveis
df1 <- filter(df,difficulty==5)
df1_test <- df1[c(1200:1247),]
table(df1$difficulty)
table(df1_test$difficulty)

df12 <- filter(df,difficulty==4)
df12_test <- df12[c(85146:85528),]
table(df12$difficulty)
table(df12_test$difficulty)

df13 <- filter(df,difficulty==3)
indexes <- sample(1:nrow(df13), size = 2500)
df13_test <- df13[indexes,]
df13 <- df13[-indexes,]
table(df13$difficulty)
table(df13_test$difficulty)

df3 <- filter(df,difficulty!=5 & difficulty!=4 & difficulty!=3)
table(df3$difficulty)

indexes <- sample(1:nrow(df3), size = 0.9 * nrow(df3))
df_test <- df3[-indexes,]
df3 <- df3[indexes,]
table(df3$difficulty)

df_train <- bind_rows(df3,df13[c(1:462500),],df12[c(1:85145),],df1[c(1:1199),])
df_test <- bind_rows(df_test,df13_test,df12_test,df1_test)

#Construindo modelo XGBOOST
X_train = data.matrix(df_train[,-16])             
y_train = df_train[,16] 

X_test = data.matrix(df_test[,-16])                   
y_test = df_test[,16]  

xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)


model <- xgboost(data = xgboost_train,                      
                 max.depth=3, ,                           
                 nrounds=20)

# Gerando previsoes nos dados de teste
result_previsto_NB <- data.frame( atual = as.factor(df_test$acertou),
                                  previsto = as.factor(round(predict(model, xgboost_test))))

# Gerando Confusion Matrix com o Caret
confusionMatrix(result_previsto_NB$atual, result_previsto_NB$previsto, mode = "everything", positive = '1')

#Gerando respostas com novos dados
X_sub = data.matrix(submit[,-16])                   
y_sub = df_test[,16] 

xgboost_test = xgb.DMatrix(data=X_sub)

submit_resp = data.frame(as.factor(round(predict(model, xgboost_test))))

conv <- function(x){
  if  (x == '0'){
    return(0)
  }else{
    return(1)
  }
}

#Exportando dados
submit_resp2 <- sapply(submit_resp$as.factor.round.predict.model..xgboost_test...,conv)

write.table (submit_resp2, "resposta5.csv",row.names = FALSE, col.names = FALSE)

