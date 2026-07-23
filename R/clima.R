#options(java.parameters = "-Xmx8g")
if(require(RJDBC)==FALSE){install.packages("RJDBC",dependencies = TRUE)}
if(require(rJava)==FALSE){install.packages("rJava",dependencies = TRUE)}
if(require(dplyr)==FALSE){install.packages("dplyr",dependencies = TRUE)}
if(require(reshape2)==FALSE){install.packages("reshape2",dependencies = TRUE)}
if(require(stringr)==FALSE){install.packages("stringr",dependencies = TRUE)}

driver = RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver","C:/driver/ojdbc11.jar")

#DATA CLIMA
#conexion_ruta = dbConnect(driver, "jdbc:oracle:thin:@dwimsa.magdalena.imsa:1521/dwimsa","OPERAINTAGRICOLA","OPERAINTAGRICOLA")
#consulta_lluvia = dbGetQuery(conexion_ruta,"SELECT * FROM RTO_AGR_DATOS_CLIMA WHERE TEMPORADA IN ('2023/2024', '2024/2025','2025/2026','2026/2027')")# WHERE TEMPORADA IN ('2023/2024', '2024/2025')
#write.csv(consulta_lluvia, "//CSCTFLDT/Investigacion/NDVI_CORREGIDO/DATOS_SEGUIMIENTO_CULTIVO/CLIMA_HISTORICO_RTOAGR.csv", row.names = FALSE)

consulta_lluvia <- read.csv("//CSCTFLDT/Investigacion/NDVI_CORREGIDO/DATOS_SEGUIMIENTO_CULTIVO/CLIMA_HISTORICO_RTOAGR.csv")

conexion <- dbConnect(driver, "jdbc:oracle:thin:@IMSAPST:1521/IMSAPSTIA","USR_INVES","sfDezcRHhC")
consulta_lluvia2 = dbGetQuery(conexion,"SELECT * FROM sdeusr.rto_agr_datos_clima")

#consulta_lluvia <- subset(consulta_lluvia, TEMPORADA %in% c("2023/2024", "2024/2025", "2025/2026"))

#dresumen <- consulta_lluvia %>% group_by(TEMPORADA, COD_FINCA, COD_SECTOR, COD_LOTE)%>% slice_max(EDAD_CULTIVO, n = 10)
#clipr::write_clip(dresumen)

#library(ggplot2)
#consulta_lluvia2 %>% select(TEMPORADA, FECHA, PRECIPITACION_PLUVIOMETRO)%>% na.omit()%>%group_by(TEMPORADA,FECHA)%>%summarise(n=length(PRECIPITACION_PLUVIOMETRO))%>%
#  ggplot(aes(x=as.Date(FECHA), y=n, col = TEMPORADA))+
#  geom_point()+
#  labs(x= "Fecha", y = "Número de lotes",
#       title = "Conteo de lotes con valor de precipitacion pluviometro")

#consulta_lluvia %>% select(TEMPORADA, FECHA, TEMPERATURA)%>% na.omit()%>%group_by(TEMPORADA,FECHA)%>%summarise(n=length(TEMPERATURA))%>%
#  ggplot(aes(x=as.Date(FECHA), y=n, col = TEMPORADA))+
#  geom_point()+
#  labs(x= "Fecha", y = "Número de lotes",
#       title = "Conteo de lotes con valor de temperatura")
#tt <- consulta_lluvia %>% select( FECHA, PRECIPITACION_PLUVIOMETRO)%>% na.omit()%>%group_by(FECHA)%>%summarise(n=length(PRECIPITACION_PLUVIOMETRO))

consulta_lluvia2 <- consulta_lluvia2[,colnames(consulta_lluvia)]
consulta <- rbind(consulta_lluvia, consulta_lluvia2)

consulta <- subset(consulta, EDAD_CULTIVO > 0)

consulta_general <- consulta

#write.table(consulta, "C:/Users/jpec/OneDrive - Ingenio Magdalena, S.A/Documentación/13. Datos boletas digitales/Clima/clima.txt", row.names = FALSE)
#consulta %t>% subset(COD_FINCA==328 & COD_SECTOR == 1 & COD_LOTE ==3)%>%
#  ggplot(aes(FECHA, PRECIPITACION_PLUVIOMETRO))+
#  geom_line()
#consulta$FECHA <- as.Date(consulta$FECHA)

consulta$EDAD_MES <- floor(as.numeric(consulta$EDAD_CULTIVO)/30)+1

consulta <- consulta %>%
  group_by(TEMPORADA, COD_FINCA, COD_SECTOR, COD_LOTE, EDAD_MES) %>%
  summarise(TEMPERATURA = mean(TEMPERATURA, na.rm = T),
            PRECIPITACION = sum(PRECIPITACION_PLUVIOMETRO, na.rm = T),
            RADIACION = mean(RADIACION, na.rm = T),
            TEMP_MAX = mean(TEMPERATURA_MAXIMA, na.rm = T),
            TEMP_MIN = mean(TEMPERATURA_MINIMA, na.rm = T),
            AMPLITUD_TERMICA = mean((TEMPERATURA_MAXIMA-TEMPERATURA_MINIMA), na.rm = T),
            SST = mean(ANOMALIA_SST, na.rm = T)            )



consulta$LOTE <- paste0(consulta$COD_FINCA, "0", consulta$COD_SECTOR,
                       ifelse(consulta$COD_LOTE>9, consulta$COD_LOTE, paste0("0", consulta$COD_LOTE)))


driver <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver","C:/driver/ojdbc7.jar")
conexion <- dbConnect(driver, "jdbc:oracle:thin:@IMSAPST:1521/IMSAPSTIA","USR_INVES","sfDezcRHhC")
lotes_activos <- dbGetQuery(conexion,"select region, finca, c_finca from sdeusr.lotes_imsa_gis")
datosf <- distinct(lotes_activos)
colnames(datosf)[3] <- "COD_FINCA"
datosf$COD_FINCA <- as.double(datosf$COD_FINCA)



MAESTRO <- read.csv("//CSCTFLDT/Investigacion/NDVI_CORREGIDO/DATOS_SEGUIMIENTO_CULTIVO/MAESTRO.csv")

resumenf <- inner_join(MAESTRO, consulta, by = c("COD_FINCA", "COD_SECTOR", "COD_LOTE"))
resumenf <- inner_join(resumenf, datosf, by = "COD_FINCA")

resumenf <- resumenf[,c("TEMPORADA","REGION", "FINCA", "COD_FINCA", "COD_SECTOR", "COD_LOTE", "LOTE", "VARIEDAD_AC", "AREA_AC", "EDAD_MES", "MES_COSECHA_AC",
                        "PRECIPITACION","RADIACION", "TEMP_MAX", "TEMP_MIN", "AMPLITUD_TERMICA", "TEMPERATURA", "SST")]

resumenf <- melt(resumenf, id.vars = c("TEMPORADA", "REGION", "FINCA", "COD_FINCA", "COD_SECTOR", "COD_LOTE", "LOTE", "VARIEDAD_AC", "AREA_AC", "EDAD_MES", "MES_COSECHA_AC"))

resumenf <- resumenf %>%
  subset(!(MES_COSECHA_AC == "noviembre" & EDAD_MES %in% c(13:14)))%>%
  subset(!(MES_COSECHA_AC == "diciembre" & EDAD_MES %in% c(13:14)))%>%
  subset(!(MES_COSECHA_AC == "enero" & EDAD_MES %in% c(13:14)))%>%
  subset(!(MES_COSECHA_AC == "febrero" & EDAD_MES %in% c(13:14)))%>%
  subset(!(MES_COSECHA_AC == "marzo" & EDAD_MES %in% c(12:14)))%>%
  subset(!(MES_COSECHA_AC == "abril" & EDAD_MES %in% c(11:14)))

resumenf$variable <- as.character(resumenf$variable)
resumenf$variable <- ifelse(resumenf$variable == "TEMP_MAX", "TEMPERATURA MAXIMA", resumenf$variable)
resumenf$variable <- ifelse(resumenf$variable == "TEMP_MIN", "TEMPERATURA MINIMA", resumenf$variable)
resumenf$variable <- ifelse(resumenf$variable == "AMPLITUD_TERMICA", "AMPLITUD TERMICA", resumenf$variable)
resumenf$variable <- ifelse(resumenf$variable == "SST", "SST NINO3.4 (ONI)", resumenf$variable)
resumenf <- subset(resumenf, !is.na(value))
resumenf <- subset(resumenf, EDAD_MES <14)

write.csv(resumenf, "//CSCTFLDT/Investigacion/NDVI_CORREGIDO/DATOS_SEGUIMIENTO_CULTIVO/DATOS_CLIMA.csv", row.names = FALSE)
