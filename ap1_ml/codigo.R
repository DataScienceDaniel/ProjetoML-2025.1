# Carregar pacotes
library(tidyverse)
library(ggplot2)
library(nortest)
library(caret)
library(plumber)
library(dplyr)
library(corrplot)

# Verificar NAs
sum(is.na(heart))

# Remover os dados duplicados
heart <- heart %>% distinct()

# Verificar estrutura e dados
str(heart)
summary(heart)
sum(is.na(heart))

# Visualizações simples
ggplot(heart, aes(x = Age)) + geom_histogram(bins = 30, fill = "blue", alpha = 0.7)
ggplot(heart, aes(x = Age, y = Cholesterol)) + geom_point() + geom_smooth(method = "lm")

# Teste de normalidade para Age
shapiro.test(heart$Age)

# Correlação entre variáveis numéricas
numericas <- heart %>% select(where(is.numeric))
cor_matrix <- cor(numericas, use = "complete.obs", method = "pearson")
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7)

# Correlação entre duas variáveis numéricas usando pearson
cor.test(heart$Age, heart$Cholesterol, method = "pearson")

# Regressão linear - Previsão de Colesterol
modelo_lm <- lm(Cholesterol ~ Age + MaxHR + ChestPainType, data = heart)
summary(modelo_lm)

# Avaliação do modelo linear
pred_lm <- predict(modelo_lm)
residuos <- heart$Cholesterol - pred_lm
mae <- mean(abs(residuos))
rmse <- sqrt(mean(residuos^2))
r2 <- summary(modelo_lm)$r.squared
cat("MAE:", mae, "\nRMSE:", rmse, "\nR²:", r2)

# Gráfico de dispersão entre Idade e Colesterol com Linha de Regressão
ggplot(heart, aes(x = Age, y = Cholesterol)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(
    title = "Dispersão entre Idade e Colesterol com Linha de Regressão",
    x = "Idade",
    y = "Colesterol"
  ) +
  theme_minimal()

# Regressão logística
modelo_log <- glm(HeartDisease ~ Age + MaxHR + ChestPainType + Cholesterol, data = heart, family = "binomial")
summary(modelo_log)

# Prever as probabilidades
prob <- predict(modelo_log, type = "response")

# Converter em classe (0 ou 1)
classe_predita <- ifelse(prob > 0.5, 1, 0)

# Converter para fator
classe_predita <- as.factor(classe_predita)
classe_real <- as.factor(heart$HeartDisease)

# Calcular matriz de confusão   
matriz <- confusionMatrix(classe_predita, classe_real, positive = "1")

# Exibir matriz e métricas
print(matriz)