LoadData<-function()
{
  # Seleccionar directorio temporal para archivos de ffdf
  options(fftempdir = choose.dir())
  # Seleccionar archivo de transacciones en formato .csv
  trx_data <- read.csv.ffdf(file=choose.files(multi=FALSE), header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)
  return(trx_data)
}
