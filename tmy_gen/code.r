rm(list=ls())
library(tcltk)
setwd(tclvalue(tkchooseDirectory()))

#Datos de entrada
data <- read.table(tk_choose.files(), header=TRUE, sep=",")
source("metadata.r")

initime=Sys.time()
#ajuste de formatos para fechas
data[,date_time_column_number]<-as.POSIXct(data[,date_time_column_number])
data$year <- format(data[,date_time_column_number], "%Y")
data$month <- format(data[,date_time_column_number], "%Y-%m")
data$day <- format(data[,date_time_column_number], "%Y-%m-%d")

##calcular minima, media y maxima de las variables
#crear vectores para indices de columnas
meanColNum=c(temperature_column_number,humidity_column_number,wind_speed_column_number)
maxColNum=c(temperature_column_number,humidity_column_number,wind_speed_column_number)
minColNum=c(temperature_column_number,humidity_column_number)
sumColNum=c(radiation_column_number)

#generar tablas de valores diarios por separado para medias, maximas, minimas y suma
meandailydata<-aggregate(data[,meanColNum],by=list(data$day), mean)
maxdailydata<-aggregate(data[,maxColNum],by=list(data$day), max)
mindailydata<-aggregate(data[,minColNum],by=list(data$day), min)
sumdailydata<-aggregate(data[,sumColNum],by=list(data$day), sum)

#cosntruir tabla de valores diarios
dailydata<-cbind(meandailydata[,1:2], maxdailydata[,2], mindailydata[,2], meandailydata[,3],
      maxdailydata[,3], mindailydata[,3], meandailydata[,4],maxdailydata[,4],sumdailydata[,2])
colnames <- c("date","TAavg","TAmax","TAmin","RHavg","RHmax","RHmin","WSavg","WSmax","GHRsum")
names(dailydata) <- colnames
dailydata$date <- as.Date(dailydata$date)
dailydata$month <- format(dailydata$date, "%Y-%m")
dailydata$month_day <- format(dailydata$date, "%m-%d")


#calcular el año promedio
dailydatamean <- aggregate(dailydata[,2:10],by=list(paste("2000",dailydata$month_day,sep="-")), mean)
names(dailydatamean)[1] <- "date"
dailydatamean$date <- as.Date(dailydatamean$date)
dailydatamean$month <- format(dailydatamean$date, "%Y-%m")
dailydatamean$month_day <- format(dailydatamean$date, "%m-%d")

#obtener las distribucion de frecuencia acumulada (CDF) de cada mes de cada año
dailycdf<-NULL
for (i in 2:10){
    tmpj<-NULL
    for (j in unique(dailydata$month)){
        fun <- ecdf(dailydata[dailydata$month==j,i])
        tmp <- fun(dailydata[dailydata$month==j,i])
        tmpj <- c(tmpj,tmp)
    }
    dailycdf <- cbind(dailycdf,tmpj)
}

#obtener las distribucion de frecuencia acumulada (CDF) de cada mes del año medio
dailycdfmean<-NULL
for (i in 2:10){
    tmpj<-NULL
    for (j in unique(dailydatamean$month)){
        fun <- ecdf(dailydatamean[dailydatamean$month==j,i])
        tmp <- fun(dailydatamean[dailydatamean$month==j,i])
        tmpj <- c(tmpj,tmp)
    }
    dailycdfmean <- cbind(dailycdfmean,tmpj)
}

##Calcular estadistico FS para cada mes
#Calcular diferencias de CDF de cada año con cdf año medio y recostruir tabla
cdfdiff <- dailycdf-rbind(dailycdfmean, dailycdfmean,dailycdfmean,dailycdfmean)
cdfdiff <- as.data.frame(cdfdiff)
names(cdfdiff)<-colnames[-1]
cdfdiff$date <- dailydata$date
cdfdiff$month <- format(cdfdiff$date, "%Y-%m")

#estadistico FS
FS <- aggregate(abs(cdfdiff[,1:9]), by=list(paste(cdfdiff$month,"01", sep="-")), mean)
names(FS)[1] <- "date"
FS$date <- as.Date(FS$date)

#Promedio ponderado WFS
weights <- cbind(W_TAmean, W_TAmax, W_TAmin, W_RHmean, W_RHmax, W_RHmin, 
            W_WindSpeedmean, W_WindSpeedmax, W_Global_Rad)/100
WFS <- data.frame(date=FS$date, ws=rowSums(FS[,2:10]*rep(weights,nrow(FS))))
WFS$date <- as.Date(WFS$date)
WFS$monthnum <- as.numeric(format(WFS$date, "%m"))

#Selección de fechas según los tres valores con WFS más bajo.
#Para esto se comparan entre los meses equivalentes de todos los años
selecMonth=NULL
for (i in unique(WFS$monthnum)){
    tmp <- data.frame(ws=sort(WFS[WFS$monthnum==i,2])[1:3],order=1:3)
    tmp <- merge(tmp,WFS, by="ws",all.x=TRUE)
    selecMonth <- rbind(selecMonth,tmp)
}
selecMonth$month <- format(selecMonth$date, "%Y-%m")

#Calcular el RMSD para cada mes seleccionado
##Año promedio horario
data$month_day_hour <- format(data[,date_time_column_number], "%m-%d %H")
indexHourlyMean <- c(temperature_column_number, humidity_column_number, wind_speed_column_number,radiation_column_number)
hourlydatamean <- aggregate(data[,indexHourlyMean],by=list(data$month_day_hour), mean)

hourlyError <- data[,indexHourlyMean] - do.call("rbind", 
                replicate(year_to-year_from+1,hourlydatamean[,-1], 
                simplify = FALSE))
hourlyError$date <- data[,date_time_column_number]
hourlyError$month <- format(hourlyError$date, "%Y-%m")

#Función para Root mean square error
rmsd <- function(error){
    sqrt(sum(error^2)/length(error))
}

#Calcular RMSD mensual para todos los años
RMSDmonth=NULL
for (i in unique(hourlyError$month)){
    tmp <- sapply(hourlyError[hourlyError$month==i,1:4],rmsd)
    RMSDmonth <- rbind(RMSDmonth,tmp)
}
rownames(RMSDmonth)<-NULL
RMSDmonth<-as.data.frame(RMSDmonth)
RMSDmonth$date<-as.Date(paste(unique(hourlyError$month),1,sep="-"))
RMSDmonth$month<-format(RMSDmonth$date, "%Y-%m")
RMSDmonth$monthnum<-format(RMSDmonth$date, "%m")

RMSDmonthMin <- aggregate(RMSDmonth[,1:4], by=list(RMSDmonth$monthnum), min)

SxTotal <- do.call("rbind", replicate(year_to-year_from+1,RMSDmonthMin[,-1], 
                simplify = FALSE))/RMSDmonth[,1:4]
SxTotal$date<-as.Date(RMSDmonth$date)
SxTotal$month<-format(SxTotal$date, "%Y-%m")

#Promedio ponderado WSx
weightsWSx <- cbind(TAw=sum(W_TAmean, W_TAmax, W_TAmin), RHw=sum(W_RHmean, W_RHmax, W_RHmin), 
            WSw=sum(W_WindSpeedmean, W_WindSpeedmax), GHRw=W_Global_Rad)/100
WSx <- data.frame(month=SxTotal$month, wsx=rowSums(SxTotal[,1:4]*rep(weightsWSx,nrow(SxTotal))))

#Subseting de los valores WSx para los meses preseleccionados
selecMonth <- merge(selecMonth, WSx, by="month", all.x=TRUE)
selecMonth <- selecMonth[,c("date","month","monthnum","order","ws","wsx")] #Re-ordenar columnas
selecMonth <- selecMonth[order(selecMonth$monthnum),]

wsxselect <- data.frame(monthnum=aggregate(selecMonth[,6], by=list(selecMonth$monthnum), max)[,1], wsx=aggregate(selecMonth[,6], by=list(selecMonth$monthnum), max)[,2])

selecYear <- merge(wsxselect, selecMonth[,c(2,6)], by="wsx", all.x=TRUE)

#Consulta de datos originales por meses tipicos
TMYdata <- data[data$month %in% selecYear$month, 1:(total_variables+1)]
TMYdata<-TMYdata[order(format(TMYdata$date, "%m")),]

#Exportar a la tabla en formato CSV
write.table(TMYdata, "TMYrawdata.csv", sep=",",row.names = FALSE)
print(paste("Tiempo de procesamiento:",difftime(Sys.time(),initime,units="secs"), "seg"))
