#install.packages("e1071")
#install.packages("caret")
#install.packages("dplyr")
#install.packages("randomForest")
#install.packages("xgboost")

#carregando bibliotecas
library(e1071)
library(caret)
library(dplyr)
library(randomForest)
library(xgboost)

#Carrega datasets
df <- read.csv("Dataset_model.csv",sep=',')
dfs <-  read.csv("subjects_questions.csv",sep=',')
df <- na.exclude(df) 

submit <- read.csv("Submit.csv",sep=';')

new <-dfs[!duplicated(dfs[c("novo_question_id")]),]
df <- left_join(df, new %>% select(novo_question_id, subject_id),by= "novo_question_id")
submit <- left_join(submit, new %>% select(novo_question_id, subject_id),by= "novo_question_id")


#dfs2 <- dfs %>% select(subject_id,novo_question_id) %>% 



#Utilizando Random Forest para obter importância das variáveis
modelo_RF <- randomForest(acertou ~ .,
                          data = df[c(1:5000),], 
                          ntree = 50, nodesize = 10, importance = T)
varImpPlot(modelo_RF)

#Count da variável 'difficulty' (teoricamente a mais importante)  
tbl <- table(df$difficulty)
cbind(tbl,prop.table(tbl))

#Processa e separa em train e test com base nos níveis
df1 <- filter(df,difficulty==5)
df1_test <- df1[c(1247:1247),]
table(df1$difficulty)
table(df1_test$difficulty)

df12 <- filter(df,difficulty==4)
df12_test <- df12[c(85146:85528),]
table(df12$difficulty)
table(df12_test$difficulty)

df3 <- filter(df,difficulty!=5 & difficulty!=4)

indexes <- sample(1:nrow(df3), size = 0.4 * nrow(df3))
df_test <- df3[-indexes,]
df_test <- df_test[c(1:9000),]
df3 <- df3[indexes,]
table(df3$difficulty)

df_train <- bind_rows(df3,df12[c(1:85528),],df1[c(1:1247),])
df_test <- bind_rows(df_test,df12_test,df1_test)

table(df_train$difficulty)
table(df_test$difficulty)

#Transformar variável 'difficulty'
df_train$difficulty <- df_train$difficulty^(10)
df_test$difficulty <- df_test$difficulty^(10)
submit$difficulty <- submit$difficulty^(10)

#Construindo modelo XGBOOST
X_train = data.matrix(df_train[,-16])             
y_train = df_train[,16] 

X_test = data.matrix(df_test[,-16])                   
y_test = df_test[,16]  

xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)


model <- xgboost(data = xgboost_train,                      
                 max.depth=1, booster = "gbtree", eta = 0.3 , makeIntegerParam("max_depth"),                    
                 nrounds=20)

# Gerando previsoes nos dados de teste
result_previsto_xg <- data.frame( atual = as.factor(df_test$acertou),
                                  previsto = as.factor(round(predict(model, xgboost_test))))

# Gerando Confusion Matrix com o Caret
confusionMatrix(result_previsto_xg$atual, result_previsto_xg$previsto, mode = "everything", positive = '1')

#Gerando respostas com novos dados
X_sub = data.matrix(submit[,-16])                   
y_sub = submit[,16] 

xgboost_sub = xgb.DMatrix(data=X_sub)

submit_resp = data.frame(as.factor(round(predict(model, xgboost_sub))))
table(submit_resp)

conv <- function(x){
  if  (x == '0'){
    return(0)
  }else{
    return(1)
  }
}

#Exportando dados
submit_resp <- sapply(submit_resp$as.factor.round.predict.model..xgboost_sub...,conv)

write.table (submit_resp, "resposta6.csv",row.names = FALSE, col.names = FALSE)


table(read.csv("resposta6.csv"))
