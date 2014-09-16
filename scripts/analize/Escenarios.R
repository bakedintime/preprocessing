normalize<-function(frame)
{
  normal <- (frame-min(frame))/(max(frame)-min(frame))
  return(normal)
}

preprocessTipoProd <- function()
{
  # Distribucion con tipo de producto agrupado por dia del mes
  tipoProdDf <- data.frame(trx_data[,c("C87567", "VWJEFECHAD", "VWJEFECHAM", "C87601")])
  names(tipoProdDf)[1] <- "tipoProd"
  names(tipoProdDf)[2] <- "mes"
  names(tipoProdDf)[3] <- "dia"
  names(tipoProdDf)[4] <- "indicador"
  
  # Distribucion en base a la data
  summary(tipoProdDf)
  
  normalMes <- normalize(tipoProdDf$mes)
  normalDia <- normalize(tipoProdDf$dia)
  tipoProd <- normalize(as.numeric(tipoProdDf$tipoProd))
  indicador <- normalize(as.numeric(tipoProdDf$indicador))
  
  hist(tipoProd)
  
  normalizedDS <- cbind(tipoProd, normalMes, normalDia, indicador)
  return(normalizedDS)
}

preprocessMCC <- function()
{
  # Distribucion con tipo de producto agrupado por dia del mes
  dframe <- data.frame(trx_data[,c("C87510", "VWJEFECHAD", "VWJEFECHAM", "C87601")])
  names(dframe)[1] <- "mcc"
  names(dframe)[2] <- "mes"
  names(dframe)[3] <- "dia"
  names(dframe)[4] <- "indicador"
  
  # Distribucion en base a la data
  summary(dframe)
  hist(dframe[[names(dframe)[1]]])
  
  normalMes <- normalize(dframe[[names(dframe)[2]]])
  normalDia <- normalize(dframe[[names(dframe)[3]]])
  mcc <- normalize(dframe[[names(dframe)[1]]])
  indicador <- normalize(as.numeric(dframe[[names(dframe)[4]]]))
  
  hist(mcc)
  
  normalizedDS <- cbind(mcc, normalMes, normalDia, indicador)
  return(normalizedDS)
}

preprocessFeriado <- function()
{
  # Distribucion con tipo de producto agrupado por dia del mes
  dframe <- data.frame(trx_data[,c("C87594","C87593", "VWJEFECHAD", "VWJEFECHAM", "C87601")])
  names(dframe)[1] <- "feriado"
  names(dframe)[2] <- "semana"
  names(dframe)[3] <- "mes"
  names(dframe)[4] <- "dia"
  names(dframe)[5] <- "indicador"
  
  # Distribucion en base a la data
  summary(dframe)
  hist(as.numeric(dframe[[names(dframe)[1]]]))
  
  normalMes <- normalize(dframe[[names(dframe)[3]]])
  normalDia <- normalize(dframe[[names(dframe)[4]]])
  feriado <- normalize(as.numeric(dframe[[names(dframe)[1]]]))
  semana <- normalize(dframe[[names(dframe)[2]]])
  indicador <- normalize(as.numeric(dframe[[names(dframe)[5]]]))
  
  hist(feriado)
  
  normalizedDS <- cbind(feriado, semana, normalMes, normalDia, indicador)
  return(normalizedDS)
}


preprocessBIN <- function()
{
  # Distribucion con tipo de producto agrupado por dia del mes
  dframe <- data.frame(trx_data[,c("C87535", "VWJEFECHAD", "VWJEFECHAM", "C87601")])
  names(dframe)[1] <- "bin"
  names(dframe)[2] <- "mes"
  names(dframe)[3] <- "dia"
  names(dframe)[4] <- "indicador"
  
  # Distribucion en base a la data
  summary(dframe)
  hist(as.numeric(dframe[[names(dframe)[1]]]))
  
  normalMes <- normalize(dframe[[names(dframe)[2]]])
  normalDia <- normalize(dframe[[names(dframe)[3]]])
  bin <- normalize(dframe[[names(dframe)[1]]])
  indicador <- normalize(as.numeric(dframe[[names(dframe)[4]]]))
  
  hist(bin)
  
  normalizedDS <- cbind(bin, normalMes, normalDia, indicador)
  return(normalizedDS)
}

preprocessMarca <- function()
{
  # Distribucion con tipo de producto agrupado por dia del mes
  dframe <- data.frame(trx_data[,c("C87566", "VWJEFECHAD", "VWJEFECHAM", "C87601")])
  names(dframe)[1] <- "marca"
  names(dframe)[2] <- "mes"
  names(dframe)[3] <- "dia"
  names(dframe)[4] <- "indicador"
  
  # Distribucion en base a la data
  summary(dframe)
  hist(as.numeric(dframe[[names(dframe)[1]]]))
  
  normalMes <- normalize(dframe[[names(dframe)[2]]])
  normalDia <- normalize(dframe[[names(dframe)[3]]])
  marca <- normalize(as.numeric(dframe[[names(dframe)[1]]]))
  indicador <- normalize(as.numeric(dframe[[names(dframe)[4]]]))
  
  hist(marca)
  
  normalizedDS <- cbind(marca, normalMes, normalDia, indicador)
  return(normalizedDS)
}