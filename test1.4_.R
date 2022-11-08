#carregando bibliotecas
library(e1071)
library(caret)
library(dplyr)
library(randomForest)

#Carrega arquivos
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
indexes <- sample(1:nrow(df13), size = 2940)
df13_test <- df13[indexes,]
df13 <- df13[-indexes,]
table(df13$difficulty)
table(df13_test$difficulty)

df14 <- filter(df,difficulty==2)
indexes <- sample(1:nrow(df14), size = 5250)
df14_test <- df14[indexes,]
df14 <- df14[-indexes,]
table(df14$difficulty)
table(df14_test$difficulty)

df15 <- filter(df,difficulty==1)
indexes <- sample(1:nrow(df15), size = 4150)
df15_test <- df15[indexes,]
df15 <- df15[-indexes,]
table(df15$difficulty)
table(df15_test$difficulty)

df_train <- bind_rows(df15[c(1:259000),],df14[c(1:328000),],df13[c(1:191000),],df12[c(1:85145),],df1[c(1:1199),])
df_test <- bind_rows(df15_test,df14_test,df13_test,df12_test,df1_test)

table(df_train$difficulty)
table(df_test$difficulty)

tbl <- table(df_train$difficulty)
cbind(tbl,prop.table(tbl))

tbl <- table(df_test$difficulty)
cbind(tbl,prop.table(tbl))

#Transformar variável 'difficulty'
df_train$difficulty <- df_train$difficulty^(4)
df_test$difficulty <- df_test$difficulty^(4)
submit$difficulty <- submit$difficulty^(4)

#Construindo modelo com NAIVE BAYES
modelo_NB <- naiveBayes( acertou~ difficulty
                         ,novo_question_id
                         ,creat_at
                         ,novo_user_id
                         ,device_type
                         ,publication_year
                         ,device
                         ,city
                         ,gp.previous.experience
                         ,examining_board_id
                         ,institute_id
                         ,modality_id
                         ,right_answer
                         ,gp.college.type
                         ,os
                         ,gp.carrers
                         ,row
                         ,region
                         ,discipline_id
                         ,knowledge_area_id,
                         data = df_train)



#print(modelo_NB)

# Gerando previsoes nos dados de teste
result_previsto_NB <- data.frame( atual = as.factor(df_test$acertou),
                                  previsto = predict(modelo_NB, df_test[,-16]))

# Gerando Confusion Matrix com o Caret
confusionMatrix(result_previsto_NB$atual, result_previsto_NB$previsto, mode = "everything", positive = '1')

#Gerando respostas com novos dados
submit_resp = data.frame(predict(modelo_NB, submit[,-16]))

conv <- function(x){
  if  (x == '0'){
    return(0)
  }else{
    return(1)
  }
}

#Exportando dados
submit_resp2 <- sapply(submit_resp$predict.modelo_NB..submit....16..,conv)

write.table (submit_resp2, "resposta4.csv",row.names = FALSE, col.names = FALSE)

