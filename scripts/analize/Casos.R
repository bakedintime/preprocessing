MontoSumAvgByClient<-function()
{
  # Field: e.g. C87503
  # Fieldname: e.g. "monto"
  moneyDf <- data.frame(trx_data[,c("C87503","C87584")])
  names(moneyDf)[1] <- "monto"
  names(moneyDf)[2] <- "idCliente"
  promVentaCliente <- ddply(moneyDf,~idCliente,summarise,mean=mean(monto),sum=sum(monto))
  return(promVentaCliente)
}

#filtrar
outputF  <- df[sapply(df, function(x) x=="N")]