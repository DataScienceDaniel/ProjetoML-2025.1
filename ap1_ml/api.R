#* @apiTitle Predição de Colesterol (Regressão Linear) e Doença Cardíaca (Regressão Logística)

#* Previsão de colesterol com base em idade, frequência cardíaca máxima e tipo de dor no peito.
#* Utiliza um modelo de **regressão linear múltipla**.
#*
#* @param Age Idade do paciente (ex: 54)
#* @param MaxHR Frequência cardíaca máxima atingida (Entre 60-202)
#* @param ChestPainType Tipo de dor no peito Opções: "ATA" = Angina Típica, "NAP" = Angina Atípica, "ASY" = Assintomático, TA"  = Dor não anginosa
#* @get /predicao
function(Age, MaxHR, ChestPainType){
  ChestPainType <- factor(ChestPainType, levels = levels(heart$ChestPainType))
  
  if (is.na(ChestPainType)) {
    return(list(erro = "ChestPainType inválido. Use: 'ATA', 'NAP', 'ASY', 'TA'"))
  }
  
  new_data <- data.frame(
    Age = as.numeric(Age),
    MaxHR = as.numeric(MaxHR),
    ChestPainType = ChestPainType
  )
  
  pred <- predict(modelo_lm, newdata = new_data)
  valor <- as.numeric(pred)
  
  nivel <- if (valor < 200) {
    "Desejável"
  } else if (valor >= 200 & valor < 240) {
    "Limítrofe"
  } else {
    "Alto"
  }
  
  return(list(
    colesterol_previsto = round(valor, 1),
    classificacao = nivel
  ))
}

#* Classificação de risco de doença cardíaca com base em idade, frequência cardíaca máxima, tipo de dor no peito e nível de colesterol.
#* Utiliza um modelo de **regressão logística** para prever a probabilidade de presença de doença cardíaca.
#* @param Age Idade do paciente (ex: 54)
#* @param MaxHR Frequência cardíaca máxima atingida (Entre 60-202)
#* @param ChestPainType Tipo de dor no peito Opções: "ATA" = Angina Típica, "NAP" = Angina Atípica, "ASY" = Assintomático, TA"  = Dor não anginosa
#* @param Cholesterol Nível de colesterol total (mg/dL, ex: 220)
#* @get /classificacao
function(Age, MaxHR, ChestPainType, Cholesterol){
  ChestPainType <- factor(ChestPainType, levels = levels(heart$ChestPainType))
  
  if (is.na(ChestPainType)) {
    return(list(erro = "ChestPainType inválido. Use: 'ATA', 'NAP', 'ASY', 'TA'"))
  }
  
  new_data <- data.frame(
    Age = as.numeric(Age),
    MaxHR = as.numeric(MaxHR),
    ChestPainType = ChestPainType,
    Cholesterol = as.numeric(Cholesterol)
  )
  
  prob <- predict(modelo_log, newdata = new_data, type = "response")
  classe <- ifelse(prob > 0.5, 1, 0)
  
  return(list(
    risco = classe,
    probabilidade = paste0(round(prob * 100, 1), "%")
  ))
}
