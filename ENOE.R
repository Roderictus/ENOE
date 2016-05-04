library("readr")
library("dplyr")
library("stringr")
library("downloader")
library("rgeos")
library("rgdal") 
library("ggplot2")
library("viridis")
library("raster")
library("leaflet")
library("htmltools")
library("htmlTable")
library("scales")
library("broom")
library("tidyr")
library("maptools")
library("utils")
library("foreign") #Si algun paquete no esta instalado
library("RColorBrewer")
library("survey")
#library(choroplethr)

getwd()
# Bajar 10 años de enoe
#Sociodemográficos

for(i in 2005: 2015)
{
      for (j in 1:4) 
      {
            print(str_c("Bajando año-trimestre:  ",(str_c(i,j))))
            download(str_c(str_c(str_c(str_c(URL,i),"/trim"),str_c(j,"/sdemt")), str_pad(str_c(j,substring (i,3,4)), 2, "left", "0"), ".zip&ht=02"), str_c("data/", str_c(i,j)), mode = "wb")
            unzip(str_c("data/", str_c(i,j)), exdir = "data")
            #file.remove(str_c("data/"), str_c(i,j))#esto no funciona por ahora 
      }
}

########################################### Lo viejo
setwd("E:/Proyectos R/ENOE/data")
ENOE <- read.dbf("sdemt415.dbf")
setwd("E:/Proyectos R/ENOE")

#UNIVERSO para el uso de las variables precodificadas se deben de utilizar 
#los siguientes criterios R_DEF=00 Y C_RES=1 ó 3 y EDA 15 a 98 AÑOS
ENOE <- filter(ENOE, R_DEF == "00") #00 Resultado definitivo de la entrevista, entrevista completa
ENOE$EDA<-as.numeric(ENOE$EDA)
ENOE <- filter(ENOE, EDA >= 15 | EDA <=98)
ENOE <- filter(ENOE, C_RES == 1 | C_RES == 3) #Condici?n de la residencia, 1 Residente Habitual, 3 Nuevo residente 
# Nuevas variables
ENOE<- mutate(ENOE, INF = ifelse(TUE2==5,1,0))
ENOE<- mutate(ENOE, INF_POB = INF * FAC)
ENOE<- mutate(ENOE, Estado = as.factor(ENT))
#nombres de los Estados
ENOE$Estado<- factor(ENOE$Estado, labels = c("AGS", "BC", "BCS","CAMP", "COAH", "COL", "CHIS", "CHIH", "DF", "DGO", "GTO","GRO", "HGO", "JAL", "MEX","MICH", "MOR", "NAY", "NL", "OAX","PUE", "QRO", "QR", "SLP", "SIN", "SON", "TAB", "TAMS", "TLAX", "VER", "YUC", "ZAC"))
ENOE$SEX<- factor(ENOE$SEX, labels = c("HOMBRE","MUJER")) #Sexo
ENOE$TUE2<- factor(ENOE$TUE2, labels = c("No Clasif","EmpSocCop","Neg. noConst", "Priv", "Pub", "Sect. Informal", "TDR", "AgSubs")) 
ENOE$RAMA_EST2<- factor(ENOE$RAMA_EST2, labels = c("No Clasif","Agricultura GSCP","Extractiva y Electricidad", "Manufactura", "Construcción", "Comercio", "Restaurantes y ServAloj", "Transportes y Com...", "Serv. Prof", "Serv. Sociales", "Sevr. Diversos", "Gobierno y Org. Int.")) 
ENOE$E_CON <- factor(ENOE$E_CON, labels = c ("Unión libre", "Separado(a)", "Divorciado(a)", "Viudo(a)", "Casado(a)", "Soltero(a)", "No sabe"))
ENOE$CLASE1 <- factor(ENOE$CLASE1, labels = c("No Clasificado", "PEA", "No PEA" ))
ENOE$ING7C<- factor(ENOE$ING7C, labels =c("No Clas","Hasta 1 SM", "1-2 SM", "2-3 SM", "3-5 SM", "Más de 5 SM", "No recibe ingresos", "No especificado"))  #clasificación de la población ocupada por nivel de ingreso 
ENOE$MEDICA5C <- factor(ENOE$MEDICA5C, labels = c("No Clas", "Sin prestaciones", "Solo acceso a instituciones de salud", "Acceso a inst. de salud y otras prest.", "no tiene acceso, pero si otras prestaciones", "No especificado" ))
ENOE$AMBITO2 <- factor (ENOE$AMBITO2, labels = c("no clas", "Sin Establecimiento" , "Con establecimiento", "Pequeños establecimientos" , "Medianos Establecimientos", "Grandes Establecimientos", "Gobierno", "Otros") )

table(ENOE$AMBITO2)

ENOE<- mutate(ENOE, Mujer = ifelse(SEX == "MUJER",1,0)) #Mujeres
ENOE<- mutate(ENOE, Mujer_Pob = FAC * Mujer) #Mujeres multiplicadas por el factor de poblacion
ENOE<- mutate(ENOE,Hombre = ifelse(SEX == "HOMBRE",1,0)) #Hombres
ENOE<- mutate(ENOE, Hombre_Pob = FAC * Hombre) #Hombres multiplicados por el factor de poblacion
ENOE<- mutate(ENOE, ING_Pob_HR = ING_X_HRS * FAC)
ENOE<- mutate(ENOE, OCUPADA = ifelse(CLASE2 ==1,1,0)) #Poblacion ocupada, para sacar ingresos promedio
ENOE<- mutate(ENOE, OCUPADA_Pob = OCUPADA * FAC) #Poblacion ocupada, para sacar ingresos promedio
ENOE<- mutate(ENOE, HRSOCUP_Pob = HRSOCUP * FAC)
ENOE<- mutate(ENOE, ANIOS_ESC_Pob = ANIOS_ESC * FAC)
ENOE$CS_P13_1<- factor(ENOE$CS_P13_1, labels = c("Ninguna", "Preescolar", "Primaria", "Secundaria","Preparatoria o bach", "Normal", "Carrera técnica", "Profesional", "Maestría", "Doctorado", "No sabe"))
ENOE$ <- mutate(ENOE, ESC_NINGUNA   =  #Escolaridad de la población
                      ENOE$ <- mutate(ENOE, ESC_NINGUNA   =  #Escolaridad de la población
                                            
                                            CS_P13_1[which(ENOE$CS_P13_1==1)] 
                                      table(ENOE$CS_P13_1, exclude = NULL) #tabla con los datos sin el factor de expansión
                                      
                                      ENOE$Preescolar<-ifelse(ENOE$CS_P13_1 == "Preescolar",1,0)
                                      ENOE$Preescolar<-ENOE$Preescolar * ENOE$FAC
                                      
                                      sum(ENOE$Preescolar, na.rm = TRUE)
                                      table(ENOE$CS_P13_1)
                                      sum(ENOE$FAC)
                                      
                                      
                                      #tablas de escolaridad de la población con porcentaje 
                                      
                                      sum(
                                            
                                            
                                      )
                                      ENOE$Preescolar<- ENOE$Preescolar * ENOE$FAC
                                      
                                      
                                      
                                      ENOE$Preescolar
                                      
                                      
                                      ENOE$Preescolar<-    ifelse(ENOE$CS_P13_1 == "Preescolar",1,0)
                                      table(ENOE$Preescolar)
                                      
                                      
                                      
                                      ENOE$CS_P13_1                        
                                      
                                      ENOE<- mutate(ENOE, SUB_O_POB = SUB_O * FAC)
                                      
                                      #Ingresos por edad y escolaridad
                                      #INGOCUP Ingreso mensual
                                      #HRSOCUP Horas trabajadas a la semana 
                                      IngEdaEsc <-ggplot(ENOE[ENOE$CS_P13_1=1], aes(EDA, INGOCUP, fill = factor (SEX))) #edad, Ingreso mensual, sexo, para personas con preescolar
                                      
                                      table(ENOE$CS_P13_1)
                                      
                                      
                                      #empleo por escolaridad
                                      
                                      
                                      
                                      #Boxplot de ingreso por hora a nivel nacional utilizando logaritmos, el logaritmo muestra valores menores a uno como negativos y no muestra a los que reportan ingreso de cero.   El boxplot muestra la mediana y los cuartiles, cada cuartil contiene alrededor del 25% de la poblaci?n. 
                                      
                                      NacLog <- ggplot(ENOE, aes(CS_P13_1, log(ING_X_HRS), fill = factor(SEX)))
                                      NacLog <- NacLog +geom_boxplot(aes(weight = FAC)) #El factor de expansi?n lo calcula ggplot 
                                      NacLog <- NacLog + labs(title = "Ingreso por hora (Log), a nivel Nacional" , x = "Escolaridad" , y = "Ingreso (log)")
                                      ggsave("graphs/NacLog.png", plot = NacLog, dpi = 500, width = 14, height = 11)
                                      
                                      
                                      #Podemos obtener estos resultados a nivel Estatal para cualquier estado 
                                      AGS<- ggplot(data =subset(ENOE, Estado =="AGS"), aes(CS_P13_1, log(ING_X_HRS), fill = factor(SEX)))+ geom_boxplot(aes(weight = FAC))
                                      AGS <-AGS + labs(title = "Ingreso por hora (Log), a nivel Nacional" , x = "Escolaridad" , y = "Ingreso (log)")
                                      AGS
                                      
                                      #para sacar resultados a nivel estatal
                                      ZAC<- ggplot(data =subset(ENOE, Estado =="AGS"), aes(CS_P13_1, log(ING_X_HRS), fill = factor(SEX)))
                                      ZAC<- ZAC+ geom_boxplot(aes(weight = FAC))
                                      ZAC <-ZAC + labs(title = "Ingreso por hora (Log), en Aguascalientes" , x = "Escolaridad" , y = "Ingreso (log)")
                                      ZAC
                                      
                                      
                                      #Del distrito Federal
                                      ```{r, echo = FALSE, message = FALSE, warning = F}
                                      DF <- ggplot(data =subset(ENOE, Estado =="DF"), aes(CS_P13_1, log(ING_X_HRS), fill = factor(SEX)))
                                      DF <- DF + geom_boxplot(aes(weight = FAC))
                                      DF <-DF + labs(title = "Ingreso por hora (Log), en el Distrito Federal" , x = "Escolaridad" , y = "Ingreso (log)")
                                      DF
                                      ```
                                      
                                      #De interpretaci?n m?s directa se puede obtener un boxplot con los ingresos por hora en pesos por escolaridad. Estos son los resultados a nivel nacional. Esta forma de presentar los resultados incluye a aquellas personas que declaran tener ingresos de cero que son una basta mayoria, por ello es frecuente que la mediana se grafique en cero. Se recorta la gr?fica para no mostrar a todos los ouliers y poder apreciar las barras. 
                                      
                                      ```{r, echo = FALSE, message = FALSE, warning = F}
                                      Nac <- ggplot(ENOE, aes(CS_P13_1, ING_X_HRS, fill = factor(SEX)))
                                      Nac <- Nac +geom_boxplot(aes(weight = FAC)) #El factor de expansi?n lo calcula ggplot 
                                      Nac <- Nac + labs(title = "Ingreso por hora, a nivel Nacional" , x = "Escolaridad" , y = "Ingreso")
                                      ylim1 = boxplot.stats(ENOE$ING_X_HRS)$stats[c(1,5)]
                                      Nac<- Nac + coord_cartesian(ylim = ylim1 * 10)
                                      Nac
                                      ```
                                      
                                      Una forma de solucionar las medianas en cero es el s?lo considerar a aquellas personas que tienen ingresos positivos. Probablemente esta sea la gr?fica m?s adecuada, se podr?a subsetear por alguna otra variable como condici?n de ocupaci?n. Se reduce sustancialmente el tama?o de la muestra. 
                                      
                                      ```{r, echo = FALSE, message = FALSE, warning = F}
                                      Nac <- ggplot(data = subset(ENOE, ING_X_HRS>0),aes(CS_P13_1, ING_X_HRS, fill = factor(SEX)))
                                      Nac <- Nac +geom_boxplot(aes(weight = FAC)) #El factor de expansi?n lo calcula ggplot 
                                      Nac <- Nac + labs(title = "Ingreso por hora, a nivel Nacional" , x = "Escolaridad" , y = "Ingreso")
                                      ylim1 = boxplot.stats(ENOE$ING_X_HRS)$stats[c(1,5)]
                                      Nac<- Nac + coord_cartesian(ylim = ylim1 * 10)
                                      Nac
                                      ```
                                      
                                      Podemos repetir esto para cualquier estado.
                                      ```{r, echo = FALSE, message = FALSE, warning = F}
                                      
                                      BC<- ggplot(data =subset(ENOE, Estado =="BC" & ING_X_HRS > 0) , aes(CS_P13_1, ING_X_HRS, fill = factor(SEX)))
                                      BC<- BC+ geom_boxplot(aes(weight = FAC))
                                      BC <-BC + labs(title = "Ingreso por hora en Baja California por nivel de escolaridad y Sexo" , x = "Escolaridad" , y = "Ingreso")
                                      ylim1 = boxplot.stats(subset(ENOE, Estado == "BC")$ING_X_HRS)$stats[c(1,5)]
                                      BC<- BC + coord_cartesian(ylim = ylim1 * 5)
                                      BC #Poner en algun lugar el numero de observaciones
                                      ```
                                      
                                      --Vamos a ver que pasa con la informalidad por Estado
                                      Definimos como fuerza laboral a todos aquellos que aparecen en la ENOE como pertenecientes a alguna clasificaci?n de la poblaci?n ocupada (TUE2 1-7).
                                      
                                      --Ingreso de la poblaci?n por Clasificaci?n de la poblaci?n ocupada por tipo de unidad econ?mica y sexo (Ingresos reportados mayores  a cerro )
                                      
                                      
                                      
                                      ```{r, echo = FALSE, message = FALSE, warning = F}
                                      Nac <- ggplot(data = subset(ENOE, ING_X_HRS >0 ),aes(TUE2, ING_X_HRS, fill = factor(SEX)))
                                      Nac <- Nac +geom_boxplot(aes(weight = FAC)) #El factor de expansi?n lo calcula ggplot 
                                      Nac <- Nac + labs(title = "Ingreso por hora, a nivel Nacional, por tipo de unidad econ?mica" , x = "Escolaridad" , y = "Ingreso")
                                      ylim1 = boxplot.stats(ENOE$ING_X_HRS)$stats[c(1,5)]
                                      Nac<- Nac + coord_cartesian(ylim = ylim1 * 3.5)
                                      Nac
                                      ```
                                      Ingreso por hora de la poblaci?n a nivel nacional por clasificaci?n de la poblaci?n seg?n sector de actividad 
                                      
                                      ```{r, echo = FALSE, message = FALSE, warning = F}
                                      Nac <- ggplot(data = subset(ENOE, ING_X_HRS >0 ),aes(RAMA_EST2, ING_X_HRS, fill = factor(SEX)))
                                      Nac <- Nac +geom_boxplot(aes(weight = FAC)) #El factor de expansi?n lo calcula ggplot 
                                      Nac <- Nac + labs(title = "Ingreso por hora, a nivel Nacional, por Clasificaci?n de la poblaci?n seg?n sector de actividad" , x = "Escolaridad" , y = "Ingreso")
                                      ylim1 = boxplot.stats(ENOE$ING_X_HRS)$stats[c(1,5)]
                                      Nac<- Nac + coord_cartesian(ylim = ylim1 * 3.5)
                                      Nac
                                      ```
                                      
                                      
                                      
                                      
                                      table(ENOE$ING_X_HRS)
                                      Cosas por hacer
                                      
                                      
                                      --clasificaci?n SCIAN
                                      
                                      --% de informalidad por Estado 
                                      
                                      --% de informalidad por grupo de edad
                                      
                                      --% de informalidad por sector
                                      
                                      --% de informalidad por nivel educativo 
                                      
                                      
                                      