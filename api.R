#* @apiTitle Predição de colesterol e Doença Cardíaca

#* @param Age Número
#* @param MaxHR Número
#* @param ChestPainType Texto
#* @param Cholesterol Número
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
  return(list(risco = classe, probabilidade = prob))
}

#* @param Age Número
#* @param MaxHR Número
#* @param ChestPainType Texto (ex: "ATA", "NAP", "ASY", "TA")
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
  
  # Classificação do nível de colesterol
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

#* Classificação de risco cardíaco
#* @param Age Número
#* @param MaxHR Número
#* @param ChestPainType Texto
#* @param Cholesterol Número
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
  return(list(risco = classe, probabilidade = prob))
}
