rm(list=ls()) # remove o que tiver na memória

# Pacotes
library(tidyverse)
library(caret) # para cross-validation
library(randomForest) # Random Forest
library(C50) # C5.0
library(xgboost)# XGBoost

# ----------------------------------------------------

# Conjunto de dados ISLR::OJ
# Objetivo: Predizer qual marca de suco de laranja (Citrus Hill or Minute Maid)
# o cliente vai comprar baseado em 17 preditores
# Variável resposta: Purchase

dados <- ISLR::OJ
skimr::skim(dados)

# *****
# Separando os dados em treino (80%) e teste (20%)
# A variável resposta é income
set.seed(12345)
indices <- caret::createDataPartition(dados$Purchase, p = .8, list = F)
dados_treino <- dados[indices, ]
dados_teste <- dados[-indices, ]

# Verificando se há desbalanceamento nos dados de treino
janitor::tabyl(dados_treino$Purchase) %>%
  janitor::adorn_pct_formatting()

# Vamos testar os algoritmos sem balancear os dados
# Nesse caso a métrica de comparação mais adequada a problemas de classificação
# é a curva ROC

# Controles para o treino
controle <- trainControl(
  method = "cv",             # k-fold cross-validation
  number = 10,               # 10 subconjuntos no cross-validation
  savePredictions = "final", # salvar as predições
  classProbs = TRUE,         # adicionar as probabilidades de pertencimento às classes para usar curva ROC
  summaryFunction = twoClassSummary # indicando que há apenas duas classes
)

# *****
# Algoritmo CART
set.seed(1234)
modelo_CART <- train(
  Purchase ~ .,         # fórmula especificando a resposta
  data = dados_treino,  # dados de treino
  method = "rpart",     # algoritmo CART
  tuneLength = 5,       # testar 5 combinações diferentes do hiperparâmetro (para CART o parâmetro é cp)
  trControl = controle, # controla o treino
  metric = "ROC"        # métrica de avaliação: curva ROC, pois é robusta a dados desbalanceados
)
modelo_CART

# como cp = 0.005988024 teve maior AUC, vamos testar alguns valores de cp
set.seed(1234)
modelo_CART <- train(
  Purchase ~ .,         # fórmula especificando a resposta
  data = dados_treino,  # dados de treino
  method = "rpart",     # algoritmo bagged CART
  tuneGrid = expand.grid(cp = seq(from = 0.001, to = 0.015, length = 16)), # gerando 11 valores de cp
  trControl = controle, # controla o treino
  metric = "ROC"        # métrica de avaliação: curva ROC
)
modelo_CART
plot(modelo_CART) # melhor cp = 0.004733333

rpart.plot::rpart.plot(modelo_CART$finalModel) # árvore

# Matriz de confusão nos dados de teste
modelo <- modelo_CART  # forneça aqui o modelo
pred_CART <- bind_cols(
  predict(modelo, newdata = dados_teste, type = "prob"),  # calcula as probabilidades de pertencer a uma classe nos dados de teste
  Predicao = predict(modelo, newdata = dados_teste, type = "raw"),  # predizer classificação
  Real = dados_teste$Purchase  # classificação real
)
pred <- pred_CART  # forneça aqui a matriz com as predições e valores reais
MatConf_CART <- confusionMatrix(pred$Predicao, reference = pred$Real)
MatConf_CART

# Curva ROC
CART_auc <- Metrics::auc(actual = pred$Real == "CH", pred$CH)  # área sob a curva
yardstick::roc_curve(pred, Real, CH) %>%  # dados para curva ROC
  autoplot() +  # curva ROC
  labs(   # Texto na imagem
    title = "Curva ROC do CART ",
    subtitle = paste0("AUC = ", round(CART_auc, 4))
  )

# Variáveis mais importantes
plot(varImp(modelo), main="Importância das variáveis no CART")


# *****
# Algoritmo C5.0
set.seed(1234)
modelo_C5.0<- train(
  Purchase ~ .,         # fórmula especificando a resposta
  data = dados_treino,  # dados de treino
  method = "C5.0",      # algoritmo  C5.0
  trControl = controle, # controla o treino
  tuneGrid = expand.grid(.winnow = TRUE, # especifica se deve remover atributos irrelevantes
                         .trials = 1, # número de iterações boosting
                         .model = "tree"), # tipo de modelo
  metric = "ROC"        # métrica de avaliação: curva ROC, pois é robusta a dados desbalanceados
)
modelo_C5.0

# Matriz de confusão nos dados de teste
modelo <- modelo_C5.0  # forneça aqui o modelo
pred_C5.0 <- bind_cols(
  predict(modelo, newdata = dados_teste, type = "prob"),  # calcula as probabilidades de pertencer a uma classe nos dados de teste
  Predicao = predict(modelo, newdata = dados_teste, type = "raw"),  # predizer classificação
  Real = dados_teste$Purchase  # classificação real
)
pred <- pred_C5.0  # forneça aqui a matriz com as predições e valores reais
MatConf_C5.0 <- confusionMatrix(pred$Predicao, reference = pred$Real)
MatConf_C5.0

# Curva ROC
C5.0_auc <- Metrics::auc(actual = pred$Real == "CH", pred$CH)  # área sob a curva
yardstick::roc_curve(pred, Real, CH) %>%  # dados para curva ROC
  autoplot() +  # curva ROC
  labs(   # Texto na imagem
    title = "Curva ROC do C5.0 ",
    subtitle = paste0("AUC = ", round(C5.0_auc, 4))
  )

# Variáveis mais importantes
plot(varImp(modelo), main="Importância das variáveis no CART")


# ----------------------------------------------------
# Bagging
# ----------------------------------------------------

# Algoritmo Bagged CART
# Esse algoritmo não tem hiperparâmetros para serem ajustados
set.seed(1234)
modelo_bCART <- train(
  Purchase ~ .,          # explicita a variável resposta
  data = dados_treino,   # dados de treino
  method = "treebag",    # algoritmo bagged CART
  trControl = controle,  # controla o treino
  metric = "ROC"         # métrica de avaliação: curva ROC, pois é robusta a dados desbalanceados
)
modelo_bCART$finalModel
modelo_bCART

# Matriz de confusão nos dados de teste
modelo <- modelo_bCART  # forneça aqui o modelo
pred_bCART <- bind_cols(
  predict(modelo, newdata = dados_teste, type = "prob"),  # calcula as probabilidades de pertencer a uma classe nos dados de teste
  Predicao = predict(modelo, newdata = dados_teste, type = "raw"),  # predizer classificação
  Real = dados_teste$Purchase  # classificação real
)
pred <- pred_bCART  # forneça aqui a matriz com as predições e valores reais
MatConf_bCART <- confusionMatrix(pred$Predicao, reference = pred$Real)
MatConf_bCART

# Curva ROC
bCART_auc <- Metrics::auc(actual = pred$Real == "CH", pred$CH)  # área sob a curva
yardstick::roc_curve(pred, Real, CH) %>%  # dados para curva ROC
  autoplot() +  # curva ROC
  labs(   # Texto na imagem
    title = "Curva ROC do Bagged CART",
    subtitle = paste0("AUC = ", round(bCART_auc, 4))
  )

# Variáveis mais importantes
plot(varImp(modelo), main="Importância das variáveis no Bagged CART")


# ----------------------------------------------------
# Random Forest
# ----------------------------------------------------

# No random forest o hiperparâmetro é mtry, o número de variáveis selecionadas
# aleatoriamente em cada árvore

set.seed(1234)
modelo_RF <- train(
  Purchase ~ .,         # fórmula especificando a resposta
  data = dados_treino,  # dados de treino
  method = "rf",        # algoritmo random forest
  metric = "ROC",       # métrica de avaliação: curva ROC, pois é robusta a dados desbalanceados
  tuneGrid = expand.grid(mtry = 1:10), # buscando mtry de 1 a 10
  trControl = controle  # controla o treino
)
modelo_RF
plot(modelo_RF)


# Matriz de confusão nos dados de teste
modelo <- modelo_RF  # forneça aqui o modelo
pred_RF <- bind_cols(
  predict(modelo, newdata = dados_teste, type = "prob"),  # calcula as probabilidades de pertencer a uma classe nos dados de teste
  Predicao = predict(modelo, newdata = dados_teste, type = "raw"),  # predizer classificação
  Real = dados_teste$Purchase  # classificação real
)
pred <- pred_RF  # forneça aqui a matriz com as predições e valores reais
MatConf_RF <- confusionMatrix(pred$Predicao, reference = pred$Real)
MatConf_RF

# Curva ROC
RF_auc <- Metrics::auc(actual = pred$Real == "CH", pred$CH)  # área sob a curva
yardstick::roc_curve(pred, Real, CH) %>%  # dados para curva ROC
  autoplot() +  # curva ROC
  labs(   # Texto na imagem
    title = "Curva ROC do Random Forest",
    subtitle = paste0("AUC = ", round(RF_auc, 4))
  )

# Variáveis mais importantes
plot(varImp(modelo), main="Importância das variáveis no Random Forest")


# ----------------------------------------------------
# Boosting
# ----------------------------------------------------

# Algoritmo C5.0 com boosting
# No algoritmo C5.0, o número de iterações boosting é controlado pelo argumento "trials"

grid <- expand.grid( .winnow = c(TRUE,FALSE), # especifica se deve remover atributos irrelevantes
                     .trials=c(1,5,10,15,20), # número de iterações boosting
                     .model="tree" )          # tipo de modelo: árvore

# Dados desbalanceados
set.seed(1234)
modelo_c5.0b <- train(
  Purchase ~ .,         # fórmula especificando a resposta
  data = dados_treino,  # dados de treino
  method = "C5.0",      # algoritmo C5.0
  metric = "ROC",       # métrica de avaliação: curva ROC, pois é robusta a dados desbalanceados
  tuneGrid = grid,      # hiperparâmetros do C5.0
  trControl = controle  # controla o treino
)
modelo_c5.0b
plot(modelo_c5.0b)

# Matriz de confusão nos dados de teste
modelo <- modelo_c5.0b  # forneça aqui o modelo
pred_c5.0b <- bind_cols(
  predict(modelo, newdata = dados_teste, type = "prob"),  # calcula as probabilidades de pertencer a uma classe nos dados de teste
  Predicao = predict(modelo, newdata = dados_teste, type = "raw"),  # predizer classificação
  Real = dados_teste$Purchase  # classificação real
)
pred <- pred_c5.0b  # forneça aqui a matriz com as predições e valores reais
MatConf_c5.0b <- confusionMatrix(pred$Predicao, reference = pred$Real)
MatConf_c5.0b

# Curva ROC
c5.0b_auc <- Metrics::auc(actual = pred$Real == "CH", pred$CH)  # área sob a curva
yardstick::roc_curve(pred, Real, CH) %>%  # dados para curva ROC
  autoplot() +  # curva ROC
  labs(   # Texto na imagem
    title = "Curva ROC do C5.0 com boosting",
    subtitle = paste0("AUC = ", round(c5.0b_auc, 4))
  )

# Variáveis mais importantes
plot(varImp(modelo), main="Importância das variáveis no c5.0")


# *****
# XGBoost (Extreme Gradient Boosting)
# Esse algoritmo possui os seguintes hiperparâmetros:
# - nrounds: número de iterações boosting
# - max_depth: profundidade máxima da árvore
# - eta: taxa de aprendizado (contribuição de cada árvore no agrupamento)
# - gamma: redução mínima da perda para fazer divisão nos dados
# - colsamle_bytree: define a fração de atributos que serão amostrados em cada árvore
# - min_child_weight: peso mínimo das instâncias em um nó da árvore (evita dividir muito os dados)
# - substample: taxa de subamostragem de linhas

# parâmetros ajustáveis
#gridxgb <- expand.grid(
#  nrounds = seq(from = 50, to = 1000, by = 50),
#  eta = c(0.025, 0.05, 0.1, 0.3),
#  max_depth = c(2, 3, 4, 5, 6),
#  gamma = 0,
#  colsample_bytree = 1,
#  min_child_weight = 1,
#  subsample = 1
#)

set.seed(1234)
modelo_xgb <- train(
  Purchase ~ .,         # fórmula especificando a resposta
  data = dados_treino,  # dados de treino
  method = "xgbTree",   # algoritmo XGBoost
  metric = "ROC",       # métrica de avaliação: curva ROC, pois é robusta a dados desbalanceados
  #tuneGrid = gridxgb,   # hiperparâmetros do XGBoost
  tuneLength = 5,       # testar 5 combinações de hiperparâmetros
  trControl = controle, # controla o treino
  verbosity = 0         # suprimir os warnings
)
modelo_xgb
plot(modelo_xgb)

# Matriz de confusão nos dados de teste
modelo <- modelo_xgb  # forneça aqui o modelo
pred_xgb <- bind_cols(
  predict(modelo, newdata = dados_teste, type = "prob"),  # calcula as probabilidades de pertencer a uma classe nos dados de teste
  Predicao = predict(modelo, newdata = dados_teste, type = "raw"),  # predizer classificação
  Real = dados_teste$Purchase  # classificação real
)
pred <- pred_xgb  # forneça aqui a matriz com as predições e valores reais
MatConf_xgb <- confusionMatrix(pred$Predicao, reference = pred$Real)
MatConf_xgb

# Curva ROC
xgb_auc <- Metrics::auc(actual = pred$Real == "CH", pred$CH)  # área sob a curva
yardstick::roc_curve(pred, Real, CH) %>%  # dados para curva ROC
  autoplot() +  # curva ROC
  labs(   # Texto na imagem
    title = "Curva ROC do XGBoost",
    subtitle = paste0("AUC = ", round(xgb_auc, 4))
  )

# Variáveis mais importantes
plot(varImp(modelo), main="Importância das variáveis no XGBoost")



# ----------------------------------------------------
# Acurácia dos modelos
# ----------------------------------------------------
rbind(
  data.frame(Modelo = "CART",
             Acuracia = MatConf_CART$overall["Accuracy"],
             Sensibilidade = MatConf_CART$byClass["Sensitivity"],
             Especificidade = MatConf_CART$byClass["Specificity"],
             Kappa = MatConf_CART$overall["Kappa"],
             AUC = CART_auc,
             row.names = NULL),
  data.frame(Modelo = "C5.0",
             Acuracia = MatConf_C5.0$overall["Accuracy"],
             Sensibilidade = MatConf_C5.0$byClass["Sensitivity"],
             Especificidade = MatConf_C5.0$byClass["Specificity"],
             Kappa = MatConf_C5.0$overall["Kappa"],
             AUC = C5.0_auc,
             row.names = NULL),
  data.frame(Modelo = "Bagged CART",
             Acuracia = MatConf_bCART$overall["Accuracy"],
             Sensibilidade = MatConf_bCART$byClass["Sensitivity"],
             Especificidade = MatConf_bCART$byClass["Specificity"],
             Kappa = MatConf_bCART$overall["Kappa"],
             AUC = bCART_auc,
             row.names = NULL),
  data.frame(Modelo = "Random Forest",
             Acuracia = MatConf_RF$overall["Accuracy"],
             Sensibilidade = MatConf_RF$byClass["Sensitivity"],
             Especificidade = MatConf_RF$byClass["Specificity"],
             Kappa = MatConf_RF$overall["Kappa"],
             AUC = RF_auc,
             row.names = NULL),
  data.frame(Modelo = "Boosted C5.0",
             Acuracia = MatConf_c5.0b$overall["Accuracy"],
             Sensibilidade = MatConf_c5.0b$byClass["Sensitivity"],
             Especificidade = MatConf_c5.0b$byClass["Specificity"],
             Kappa = MatConf_c5.0b$overall["Kappa"],
             AUC = c5.0b_auc,
             row.names = NULL)#,
  data.frame(Modelo = "XGBoost",
             Acuracia = MatConf_xgb$overall["Accuracy"],
             Sensibilidade = MatConf_xgb$byClass["Sensitivity"],
             Especificidade = MatConf_xgb$byClass["Specificity"],
             Kappa = MatConf_xgb$overall["Kappa"],
             AUC = xgb_auc,
             row.names = NULL)
) %>% arrange(desc(Acuracia))
