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

preprocessPais <- function()
{
  # Distribucion con tipo de producto agrupado por dia del mes
  dframe <- data.frame(trx_data[,c("C87543", "C87519", "VWJEFECHAD", "VWJEFECHAM", "C87601")])
  names(dframe)[1] <- "paisComercio"
  names(dframe)[2] <- "paisTrx"
  names(dframe)[3] <- "dia"
  names(dframe)[4] <- "mes"
  names(dframe)[5] <- "indicador"
  
  # Distribucion en base a la data
  summary(dframe)
  hist(as.numeric(dframe[[names(dframe)[2]]]), breaks=150)
  
  normalMes <- normalize(dframe[[names(dframe)[4]]])
  normalDia <- normalize(dframe[[names(dframe)[3]]])
  paisTrx <- normalize(as.numeric(dframe[[names(dframe)[2]]])) 
  paisComercio <- normalize(dframe[[names(dframe)[1]]])
  indicador <- normalize(as.numeric(dframe[[names(dframe)[4]]]))
  
  hist(paisComercio)
  
  normalizedDS <- cbind(paisComercio, paisTrx, normalMes, normalDia, indicador)
  return(normalizedDS)
}

preprocessHora <- function()
{
  # Distribucion con tipo de producto agrupado por dia del mes
  dframe <- data.frame(trx_data[,c("VWJEHORA", "VWJEFECHAD", "VWJEFECHAM", "C87601")])
  names(dframe)[1] <- "hora"
  names(dframe)[2] <- "mes"
  names(dframe)[3] <- "dia"
  names(dframe)[4] <- "indicador"
  
  dframe$hora <- strptime(formatC(as.numeric(dframe$hora), width = 6, format = 'd', flag = '0'), "%H%M%S")
  dtPOSIXct <- as.POSIXct(dframe$hora)
  
  # extract time of 'date+time' (POSIXct) in hours as numeric
  dframe$hora <- as.numeric(dtPOSIXct - trunc(dtPOSIXct, "days"))
  class(dframe$hora) <- "POSIXct"
  
  p <- qplot(dframe$hora, binwidth=5000) + xlab("Time slot") +
    scale_x_datetime(labels = date_format("%H:00"), breaks = date_breaks("5000 sec"))
  print(p)
  
  # Distribucion en base a la data
  summary(dframe)
  hist(dframe[[names(dframe)[1]]], breaks=25)
  
  normalMes <- normalize(dframe[[names(dframe)[2]]])
  normalDia <- normalize(dframe[[names(dframe)[3]]])
  hora <- normalize(dframe[[names(dframe)[1]]])
  indicador <- normalize(as.numeric(dframe[[names(dframe)[4]]]))
  
  hist(hora)
  
  normalizedDS <- cbind(hora, normalMes, normalDia, indicador)
  return(normalizedDS)
}

preprocessMontoSumAvgByClient<-function()
{
  # Field: e.g. C87503
  # Fieldname: e.g. "monto"
  moneyDf <- data.frame(trx_data[,c("C87503","C87584", "VWJEFECHAD")])
  names(moneyDf)[1] <- "monto"
  names(moneyDf)[2] <- "idCliente"
  names(moneyDf)[3] <- "dia"
  promVentaCliente <- ddply(moneyDf,c("idCliente","dia"),summarise,mean=mean(monto),sum=sum(monto))
  
  normalDia <- normalize(promVentaCliente$dia)
  normalPromedio <- normalize(promVentaCliente$mean)
  normalTotal <- normalize(promVentaCliente$sum)
  idCliente <- promVentaCliente$idCliente
  
  normalizedDS <- cbind(normalDia, idCliente, normalPromedio, normalTotal)
  
  ggplot(promVentaCliente,aes(x=dia,y=mean))+geom_bar(stat='identity')
  
  # Con este codigo se genera el archivo preprocesado para la red neuronal
  
  moneyDf <- data.frame(trx_data[,c("C87503","C87584", "VWJEFECHAD", "C87601")])
  names(moneyDf)[1] <- "monto"
  names(moneyDf)[2] <- "idCliente"
  names(moneyDf)[3] <- "dia"
  names(moneyDf)[4] <- "indicador"
  #promVentaCliente <- ddply(moneyDf,c("idCliente","dia"),summarise,mean=mean(monto),sum=sum(monto))
  
  normalDia <- normalize(moneyDf$dia)
  normalMonto <- normalize(moneyDf$monto)
  idCliente <- moneyDf$idCliente
  indicador <- normalize(as.numeric(moneyDf$indicador))
  
  normalizedDS <- cbind(normalDia, idCliente, normalMonto, indicador)
  
  
  return(promVentaCliente)
}

preprocessFreqPaisComercioByClient<-function()
{
  # Field: e.g. C87503
  # Fieldname: e.g. "monto"
  moneyDf <- data.frame(trx_data[,c("C87543","C87584", "VWJEFECHAD")])
  names(moneyDf)[1] <- "paisComercio"
  names(moneyDf)[2] <- "idCliente"
  names(moneyDf)[3] <- "dia"
  promVentaCliente <- ddply(moneyDf,c("idCliente","dia"),summarise,freq=length(paisComercio))
  
  normalDia <- normalize(promVentaCliente$dia)
  normalFreq <- normalize(promVentaCliente$freq)
  idCliente <- promVentaCliente$idCliente
  
  normalizedDS <- cbind(normalDia, normalFreq, idCliente)
  
  ggplot(promVentaCliente,aes(x=idCliente,y=freq))+geom_bar(stat='identity')
  
  # Con este codigo se genera el archivo preprocesado para la red neuronal
  moneyDf <- data.frame(trx_data[,c("C87543","C87584", "VWJEFECHAD", "C87601")])
  names(moneyDf)[1] <- "paisComercio"
  names(moneyDf)[2] <- "idCliente"
  names(moneyDf)[3] <- "dia"
  names(moneyDf)[4] <- "indicador"
  #promVentaCliente <- ddply(moneyDf,c("idCliente","dia"),summarise,freq=length(paisComercio))
  
  normalDia <- normalize(moneyDf$dia)
  normalPais <- normalize(moneyDf$paisComercio)
  idCliente <- moneyDf$idCliente
  indicador <- normalize(as.numeric(moneyDf$indicador))
  
  normalizedDS <- cbind(normalDia, normalPais, idCliente, indicador)
  
  return(promVentaCliente)
}




