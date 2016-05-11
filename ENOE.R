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
library("foreign")
library("RColorBrewer")
library("survey")
#library(choroplethr)

getwd()
# Bajar 10 años de enoe
#Sociodemográficos

for(i in 2006: 2015)
{
      for (j in 1:4) 
      {
            #print(str_c("Bajando año-trimestre:  ",(str_c(i,j))))
            #download(str_c(str_c(str_c(str_c(URL,i),"/trim"),str_c(j,"/sdemt")), str_pad(str_c(j,substring (i,3,4)), 2, "left", "0"), ".zip&ht=02"), str_c("data/", str_c(i,j)), mode = "wb")
            unzip(str_c("data/", str_c(i,j)), exdir = "data")
            #file.remove(str_c("data/"), str_c(i,j))#esto no funciona por ahora 
      }
}

########################################### 
setwd("E:/Proyectos R/ENOE/data")
ENOE <- read.dbf("sdemt415.dbf")
setwd("E:/Proyectos R/ENOE")

#UNIVERSO para el uso de las variables precodificadas se deben de utilizar 
#los siguientes criterios R_DEF=00 Y C_RES=1 ó 3 y EDA >= 14 y <= 98 

ENOE <- filter(ENOE, R_DEF == "00") #00 Resultado definitivo de la entrevista, entrevista completa
ENOE$EDA<-as.numeric(ENOE$EDA)
ENOE <- filter(ENOE, EDA >= 14)
ENOE <- filter(ENOE, EDA <=98)
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
ENOE<- mutate(ENOE, Mujer = ifelse(SEX == "MUJER",1,0)) #Mujeres
ENOE<- mutate(ENOE, Mujer_Pob = FAC * Mujer) #Mujeres multiplicadas por el factor de poblacion
ENOE<- mutate(ENOE,Hombre = ifelse(SEX == "HOMBRE",1,0)) #Hombres
ENOE<- mutate(ENOE, Hombre_Pob = FAC * Hombre) #Hombres multiplicados por el factor de poblacion
ENOE<- mutate(ENOE, ING_Pob_HR = ING_X_HRS * FAC)
ENOE<- mutate(ENOE, OCUPADA = ifelse(CLASE2 ==1,1,0)) #Poblacion ocupada, para sacar ingresos promedio
ENOE<- mutate(ENOE, OCUPADA_Pob = OCUPADA * FAC) #Poblacion ocupada, para sacar ingresos promedio
ENOE<- mutate(ENOE, HRSOCUP_Pob = HRSOCUP * FAC)
ENOE<- mutate(ENOE, ANIOS_ESC_Pob = ANIOS_ESC * FAC)
ENOE<- mutate(ENOE, SUB_O_POB = SUB_O * FAC)
#Codificación de las variables de escolaridad
ENOE$CS_P13_1<- factor(ENOE$CS_P13_1, labels = c("Ninguna", "Preescolar", "Primaria", "Secundaria","Preparatoria o bach", "Normal", "Carrera técnica", "Profesional", "Maestría", "Doctorado", "No sabe"))
#Para cada nivel de escolaridad
ENOE$Ninguna<-ifelse(ENOE$CS_P13_1 == "Ninguna",1,0) #Ninguna
ENOE$Ninguna<-ENOE$Ninguna * ENOE$FAC
ENOE$Preescolar<-ifelse(ENOE$CS_P13_1 == "Preescolar",1,0)
ENOE$Preescolar<-ENOE$Preescolar * ENOE$FAC
ENOE$Primaria<-ifelse(ENOE$CS_P13_1 == "Primaria",1,0)
ENOE$Primaria<-ENOE$Primaria * ENOE$FAC
ENOE$Secundaria<-ifelse(ENOE$CS_P13_1 == "Secundaria",1,0)
ENOE$Secundaria<-ENOE$Secundaria * ENOE$FAC
ENOE$Preparatoria<-ifelse(ENOE$CS_P13_1 == "Preparatoria o bach",1,0)
ENOE$Preparatoria<-ENOE$Preparatoria * ENOE$FAC
ENOE$Normal<-ifelse(ENOE$CS_P13_1 == "Normal",1,0)
ENOE$Normal<-ENOE$Normal * ENOE$FAC
ENOE$Tecnica<-ifelse(ENOE$CS_P13_1 == "Carrera técnica",1,0)
ENOE$Tecnica<-ENOE$Tecnica * ENOE$FAC
ENOE$Profesional<-ifelse(ENOE$CS_P13_1 == "Profesional",1,0)
ENOE$Profesional<-ENOE$Profesional * ENOE$FAC
ENOE$Maestria<-ifelse(ENOE$CS_P13_1 == "Maestría",1,0)
ENOE$Maestria<-ENOE$Maestria * ENOE$FAC
ENOE$Doctorado<-ifelse(ENOE$CS_P13_1 == "Doctorado",1,0)
ENOE$Doctorado<-ENOE$Doctorado * ENOE$FAC
ENOE$NS<-ifelse(ENOE$CS_P13_1 == "No sabe",1,0)
ENOE$NS<-ENOE$NS * ENOE$FAC
#hombres y mujeres con maestría
table(ENOE$CS_P13_1, exclude = NULL) #tabla con los datos sin el factor de expansión

ENOEB <- ENOE[ENOE$EDA <=75,] #ENOEB restringe la edad de 14 a 75 años para comparación de ingreso salarial 




###Ninguna
Ninguna <- ggplot(ENOEB[ENOEB$CS_P13_1=="Ninguna",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Ninguna <- Ninguna + geom_smooth(aes(weight = FAC))
Ninguna <- Ninguna + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual") + ggtitle("Ingreso Mensual declarado por Escolaridad (Ninguna) y Sexo(ENOE, IV 2015) ")
ggsave("graphs/Ing_sexo_ninguna.png", plot = Primaria, dpi = 500, width = 14, height = 11)

###
Primaria <- ggplot(ENOEB[ENOEB$CS_P13_1=="Primaria",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Primaria <- Primaria + geom_smooth(aes(weight = FAC))
Primaria <- Primaria + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual") + ggtitle("Ingreso Mensual declarado por Escolaridad (Primaria) y Sexo(ENOE, IV 2015) ")
ggsave("graphs/Ing_sexo_primaria.png", plot = Primaria, dpi = 500, width = 14, height = 11)

###
Secundaria <- ggplot(ENOEB[ENOEB$CS_P13_1=="Secundaria",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Secundaria <- Secundaria + geom_smooth(aes(weight = FAC))
Secundaria <- Secundaria + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Secundaria) y Sexo(ENOE, IV 2015) ")
ggsave("graphs/Ing_sexo_secundaria.png", plot = Primaria, dpi = 500, width = 14, height = 11)

###
Tecnica <- ggplot(ENOEB[ENOEB$CS_P13_1=="Tecnica",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Tecnica <- Tecnica + geom_smooth(aes(weight = FAC))
Tecnica <- Tecnica + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Carrera Técnica) y Sexo(ENOE, IV 2015) ")
ggsave("graphs/Ing_sexo_secundaria.png", plot = Primaria, dpi = 500, width = 14, height = 11)

####
Preparatoria <- ggplot(ENOEB[ENOEB$CS_P13_1=="Preparatoria o bach",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Preparatoria <- Preparatoria + geom_smooth(aes(weight = FAC))
Preparatoria <- Preparatoria + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Preparatoria) y Sexo(ENOE, IV 2015) ")
Preparatoria
ggsave("graphs/Ing_sexo_preparatoria.png", plot = Primaria, dpi = 500, width = 14, height = 11)

#####
Profesional <- ggplot(ENOEB[ENOEB$CS_P13_1=="Profesional",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Profesional <- Profesional + geom_smooth(aes(weight = FAC))
Profesional <- Profesional + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Profesional) y Sexo(ENOE, IV 2015) ")
ggsave("graphs/Ing_sexo_Profesional.png", plot = Primaria, dpi = 500, width = 14, height = 11)

Maestria <- ggplot(ENOEB[ENOEB$CS_P13_1=="Maestria",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Maestria <- Maestria + geom_smooth(aes(weight = FAC))
Maestria <- Maestria + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Maestría) y Sexo(ENOE, IV 2015) ")
ggsave("graphs/Ing_sexo_Maestria.png", plot = Primaria, dpi = 500, width = 14, height = 11)

Doctorado <- ggplot(ENOEB[ENOEB$CS_P13_1=="Doctorado",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Doctorado <- Doctorado + geom_boxplot(aes(weight = FAC))
Doctorado <- Doctorado+ xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Doctorado) y Sexo(ENOE, IV 2015) ")
Doctorado

ggsave("graphs/Ing_sexo_Doctorado.png", plot = Doctorado, dpi = 500, width = 14, height = 11)




sum(table(ENOE$CS_P13_1))





for(i in 2006: 2015)
{} 

Todo <- ggplot(ENOEB, aes(EDA, INGOCUP, color = CS_P13_1)) #edad, Ingreso mensual, sexo, para personas con preescolar
Todo <- Todo + geom_smooth(aes(weight = FAC))
Todo <- Todo + xlab("Edad (mayores de 14 años)") +ylab("Ingreso Mensual")
Todo
export.png


#personas que ganan 0, hombres mujeres
#Grado de escolaridad por población y sexo
table(ENOE$CS_P13_1)
#tabla con los datos (número de personas en cada categoria )
#por ocupaci[on]
#quitando a los que ganan 0
#hist(ENOE$EDA)


#########################Tabla nivel educativo
ENOEMUJ <- ENOE[ENOE$SEX == "MUJER",]
ENOEHOM <- ENOE[ENOE$SEX == "HOMBRE",]

a1<-sum(ENOEMUJ[ENOEMUJ$CS_P13_1=="Ninguna",]$FAC)
b1<-sum(ENOEHOM[ENOEHOM$CS_P13_1=="Ninguna",]$FAC)
c1<-sum(ENOE[ENOE$CS_P13_1=="Ninguna",]$FAC)

a2<-sum(ENOEMUJ[ENOEMUJ$CS_P13_1=="Preescolar",]$FAC)
b2<-sum(ENOEHOM[ENOEHOM$CS_P13_1=="Preescolar",]$FAC)
c2<-sum(ENOE[ENOE$CS_P13_1=="Preescolar",]$FAC)

a3<-sum(ENOEMUJ[ENOEMUJ$CS_P13_1=="Primaria",]$FAC)
b3<-sum(ENOEHOM[ENOEHOM$CS_P13_1=="Primaria",]$FAC)
c3<-sum(ENOE[ENOE$CS_P13_1=="Primaria",]$FAC)

a4<-sum(ENOEMUJ[ENOEMUJ$CS_P13_1=="Secundaria",]$FAC)
b4<-sum(ENOEHOM[ENOEHOM$CS_P13_1=="Secundaria",]$FAC)
c4<-sum(ENOE[ENOE$CS_P13_1=="Secundaria",]$FAC)

a5<-sum(ENOEMUJ[ENOEMUJ$CS_P13_1=="Preparatoria o bach",]$FAC)
b5<-sum(ENOEHOM[ENOEHOM$CS_P13_1=="Preparatoria o bach",]$FAC)
c5<-sum(ENOE[ENOE$CS_P13_1=="Preparatoria o bach",]$FAC)

a6<-sum(ENOEMUJ[ENOEMUJ$CS_P13_1=="Normal",]$FAC)
b6<-sum(ENOEHOM[ENOEHOM$CS_P13_1=="Normal",]$FAC)
c6<-sum(ENOE[ENOE$CS_P13_1=="Normal",]$FAC)

a7<-sum(ENOEMUJ[ENOEMUJ$CS_P13_1=="Carrera técnica",]$FAC)
b7<-sum(ENOEHOM[ENOEHOM$CS_P13_1=="Carrera técnica",]$FAC)
c7<-sum(ENOE[ENOE$CS_P13_1=="Carrera técnica",]$FAC)

a8<-sum(ENOEMUJ[ENOEMUJ$CS_P13_1=="Profesional",]$FAC)
b8<-sum(ENOEHOM[ENOEHOM$CS_P13_1=="Profesional",]$FAC)
c8<-sum(ENOE[ENOE$CS_P13_1=="Profesional",]$FAC)

a9<-sum(ENOEMUJ[ENOEMUJ$CS_P13_1=="Maestría",]$FAC)
b9<-sum(ENOEHOM[ENOEHOM$CS_P13_1=="Maestría",]$FAC)
c9<-sum(ENOE[ENOE$CS_P13_1=="Maestría",]$FAC)

a10<-sum(ENOEMUJ[ENOEMUJ$CS_P13_1=="Doctorado",]$FAC)
b10<-sum(ENOEHOM[ENOEHOM$CS_P13_1=="Doctorado",]$FAC)
c10<-sum(ENOE[ENOE$CS_P13_1=="Doctorado",]$FAC)

a11<-sum(ENOEMUJ[ENOEMUJ$CS_P13_1=="No sabe",]$FAC)
b11<-sum(ENOEHOM[ENOEHOM$CS_P13_1=="No sabe",]$FAC)
c11<-sum(ENOE[ENOE$CS_P13_1=="No sabe",]$FAC)

A<-as.matrix(c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11), nrow =11 , ncol = 1)
B<-as.matrix(c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11), nrow =11 , ncol = 1)
C<-as.matrix(c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11), nrow =11 , ncol = 1)
Edu<-cbind(A,cbind(B,C))

X<- colSums(Edu)
Edu<-rbind(Edu,X)
colnames(Edu) <- c("Hombres", "Mujeres", "Total")
rownames(Edu) <- c("Ninguna", "Preescolar", "Primaria", "Secundaria" , "Preparatoria o Bach", "Normal" , "Técnica", "Profesional", "Maestría", "Doctorado", "No sabe", "Total" )
write.csv(Edu,file="./tablas/Educacion.csv" )

#población que dice ganar 0 



#Personas que ganan 0 por nivel educativo 


as.vector(c(a1,a2,a3))

?sapply()

?list(for (i))

str_c()



letters[1:32]

?letters

a+b


table(ENOEHOM$SEX)




table(ENOE$CS_P13_1)
#Boxplot de ingreso por hora a nivel nacional utilizando logaritmos, el logaritmo muestra valores menores a uno como negativos y no muestra a los que reportan ingreso de cero.   El boxplot muestra la mediana y los cuartiles, cada cuartil contiene alrededor del 25% de la poblaci?n. 
NacLog <- ggplot(ENOE, aes(CS_P13_1, log(ING_X_HRS)))
NacLog <- NacLog +geom_boxplot(aes(weight = FAC)) #El factor de expansi?n lo calcula ggplot 
NacLog <- NacLog + labs(title = "Ingreso por hora (Log), a nivel Nacional" , x = "Escolaridad" , y = "Ingreso (log)")
NacLog
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
                                      
#Una forma de solucionar las medianas en cero es el s?lo considerar a aquellas personas que tienen ingresos positivos. Probablemente esta sea la gr?fica m?s adecuada, se podr?a subsetear por alguna otra variable como condici?n de ocupaci?n. Se reduce sustancialmente el tama?o de la muestra. 
 ```{r, echo = FALSE, message = FALSE, warning = F}
 Nac <- ggplot(data = subset(ENOE, ING_X_HRS>0),aes(CS_P13_1, ING_X_HRS, fill = factor(SEX)))
 Nac <- Nac +geom_boxplot(aes(weight = FAC)) #El factor de expansi?n lo calcula ggplot 
 Nac <- Nac + labs(title = "Ingreso por hora, a nivel Nacional" , x = "Escolaridad" , y = "Ingreso")
 ylim1 = boxplot.stats(ENOE$ING_X_HRS)$stats[c(1,5)]
 Nac<- Nac + coord_cartesian(ylim = ylim1 * 10)
 Nac
 ```
 #Podemos repetir esto para cualquier estado.
 ```{r, echo = FALSE, message = FALSE, warning = F}
 
 BC<- ggplot(data =subset(ENOE, Estado =="BC" & ING_X_HRS > 0) , aes(CS_P13_1, ING_X_HRS, fill = factor(SEX)))
 BC<- BC+ geom_boxplot(aes(weight = FAC))
 BC <-BC + labs(title = "Ingreso por hora en Baja California por nivel de escolaridad y Sexo" , x = "Escolaridad" , y = "Ingreso")
 ylim1 = boxplot.stats(subset(ENOE, Estado == "BC")$ING_X_HRS)$stats[c(1,5)]
 BC<- BC + coord_cartesian(ylim = ylim1 * 5)
 BC #Poner en algun lugar el numero de observaciones
 ```
                                      
 #--Vamos a ver que pasa con la informalidad por Estado
 #Definimos como fuerza laboral a todos aquellos que aparecen en la ENOE como pertenecientes a alguna clasificaci?n de la poblaci?n ocupada (TUE2 1-7).
 #--Ingreso de la poblaci?n por Clasificaci?n de la poblaci?n ocupada por tipo de unidad econ?mica y sexo (Ingresos reportados mayores  a cerro )
 ```{r, echo = FALSE, message = FALSE, warning = F}
 Nac <- ggplot(data = subset(ENOE, ING_X_HRS >0 ),aes(TUE2, ING_X_HRS, fill = factor(SEX)))
 Nac <- Nac +geom_boxplot(aes(weight = FAC)) #El factor de expansi?n lo calcula ggplot 
 Nac <- Nac + labs(title = "Ingreso por hora, a nivel Nacional, por tipo de unidad econ?mica" , x = "Escolaridad" , y = "Ingreso")
 ylim1 = boxplot.stats(ENOE$ING_X_HRS)$stats[c(1,5)]
 Nac<- Nac + coord_cartesian(ylim = ylim1 * 3.5)
 Nac
 ```
 #Ingreso por hora de la poblaci?n a nivel nacional por clasificaci?n de la poblaci?n seg?n sector de actividad 
                                      
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
                                      
                                      
                                      