# frequencias de MCC con mayor probabilidad de tener fraudes
mccDf <- data.frame(trx_data[,c("C87510", "C87601")])
names(mccDf)[1] <- "mcc"
names(mccDf)[2] <- "indicador"
mccFraude <- subset(mccDf, mccDf$indicador=="S")

# se agrupa cantidad de fraudes por tipo de MCC
#frecMcc <- ddply(mccFraude, .(mcc), summarise,freq=length(mcc))

fraudeTipoProd <- function()
{
  # frequencias de fraude por tipo de producto
  tipoProdDf <- data.frame(trx_data[,c("C87567", "C87601")])
  names(tipoProdDf)[1] <- "tipoProd"
  names(tipoProdDf)[2] <- "indicador"
  tipoProdFraude <- subset(tipoProdDf, tipoProdDf$indicador=="S")
  
  # Se agrupa cantidad de fraudes por tipo de producto
  frecTipoProd <- ddply(tipoProdFraude, .(tipoProd), summarise, freq=length(tipoProd))
}

distCLDia <-function()
{
  # Distribucion con tipo de producto CL agrupado por dia del mes
  tipoProdDf <- data.frame(trx_data[,c("C87567", "C87601", "VWJEFECHAD")])
  names(tipoProdDf)[1] <- "tipoProd"
  names(tipoProdDf)[2] <- "indicador"
  names(tipoProdDf)[3] <- "dia"
  tipoProdFraude <- subset(tipoProdDf, tipoProdDf$indicador=="S" & tipoProdDf$tipoProd=="CL")
   
  # Distribucion en base a la data
  hist(tipoProdFraude$dia, freq=TRUE)

  # Se agrupan los casos de CL por dia
  #frecTipoProd <- ddply(tipoProdFraude, .(dia), summarise, freq=length(dia))
  # Se genera un histograma de dist normal con los valores de la dist en base a los datos
  #n_dist <- rnorm(length(tipoProdFraude$dia), mean(tipoProdFraude$dia), sd(frecTipoProd$dia))
  #hist(n_dist)

  summary(tipoProdFraude$dia)
  quantile(tipoProdFraude$dia, probs=c(0.01, 0.99))
  
  minimum <- min(tipoProdFraude$mes)
  maximum <- max(tipoProdFraude$mes)
  
  normalized <- (tipoProdFraude$mes-minimum)/(maximum-minimum)
  hist(normalized,xlab="Normalized Data",col="lightblue",main="")
  summary(normalized)
  
  # Agregar el detalle del quartile a cada fila
  tipoProdFraude <- within(tipoProdFraude, quartile <- as.integer(cut(dia, quantile(dia, probs=0:4/4), include.lowest=TRUE)))
  
  normalized <- within(data.frame(normalized),
    quartile <- as.integer(
      cut(
        (
          tipoProdFraude$dia-min(tipoProdFraude$dia)/(max(tipoProdFraude$dia)-min(tipoProdFraude$dia))
          ), quantile(tipoProdFraude$dia, probs=0:4/4), include.lowest=TRUE)))
  normalized$indicador <- tipoProdFraude$indicador
  
}

hist(as.numeric(tipoProdFraude$tipoProd), freq=TRUE, breaks=30)
as.numeric(tipoProdFraude$tipoProd)

#filtrar
outputF  <- df[sapply(df, function(x) x=="N")]
#contar cuantos mcc son 0
length(trx_data[,c("C87510")][sapply(trx_data[,c("C87510")], function(x) x==0)])