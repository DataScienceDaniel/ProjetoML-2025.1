# Carregar pacotes
library(tidyverse)
library(ggplot2)
library(nortest)
library(caret)
library(plumber)

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
cor.test(heart$Age, heart$Cholesterol, method = "pearson")

# Regressão linear múltipla
modelo_lm <- lm(Cholesterol ~ Age + MaxHR + ChestPainType, data = heart)
summary(modelo_lm)

# Avaliação
pred_lm <- predict(modelo_lm)
residuos <- heart$Cholesterol - pred_lm
mae <- mean(abs(residuos))
rmse <- sqrt(mean(residuos^2))
r2 <- summary(modelo_lm)$r.squared
cat("MAE:", mae, "\nRMSE:", rmse, "\nR²:", r2)

# Gráfico Predito vs Real
ggplot(heart, aes(x = pred_lm, y = Cholesterol)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predição de Colesterol - Regressão Linear")

# Regressão logística
modelo_log <- glm(HeartDisease ~ Age + MaxHR + ChestPainType + Cholesterol, data = heart, family = "binomial")
summary(modelo_log)

# Avaliação
prob <- predict(modelo_log, type = "response")
classe_predita <- ifelse(prob > 0.5, 1, 0)
conf <- confusionMatrix(as.factor(classe_predita), as.factor(heart$HeartDisease))
acuracia_modelo <- conf$overall["Accuracy"]

# Print da acurácia
print(acuracia_modelo)

