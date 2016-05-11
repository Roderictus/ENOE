#Gráficas de ocupación por sexo ingreso mensual, mayores de 14 menores de 75 años

library("ggplot2")
library("viridis")
library("scales")
library("foreign")
library("RColorBrewer")
library("survey")
library("tidyr")


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
ENOE$CLASE1 <- factor(ENOE$CLASE1, labels = c( "PEA", "No PEA" ))
ENOE$ING7C<- factor(ENOE$ING7C, labels =c("No Clas","Hasta 1 SM", "1-2 SM", "2-3 SM", "3-5 SM", "Más de 5 SM", "No recibe ingresos", "No especificado"))  #clasificación de la población ocupada por nivel de ingreso 
ENOE$MEDICA5C <- factor(ENOE$MEDICA5C, labels = c("No Clas", "Sin prestaciones", "Solo acceso a instituciones de salud", "Acceso a inst. de salud y otras prest.", "no tiene acceso, pero si otras prestaciones", "No especificado" ))
ENOE$AMBITO2 <- factor (ENOE$AMBITO2, labels = c("no clas", "Sin Establecimiento" , "Con establecimiento", "Pequeños establecimientos" , "Medianos Establecimientos", "Grandes Establecimientos", "Gobierno", "Otros") )
ENOE<- mutate(ENOE, Mujer = ifelse(SEX == "MUJER",1,0)) #Mujeres
ENOE<- mutate(ENOE, Mujer_Pob = FAC * Mujer) #Mujeres multiplicadas por el factor de poblacion
ENOE<- mutate(ENOE, Hombre = ifelse(SEX == "HOMBRE",1,0)) #Hombres
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
#table(ENOE$CS_P13_1, exclude = NULL) #tabla con los datos sin el factor de expansión

#Con ingresos de ocupación mayores a cero
ENOEB <- ENOE[ENOE$EDA <=75,] #ENOEB restringe la edad de 14 a 75 años para comparación de ingreso salarial 

ENOEB<-ENOEB[ENOEB$INGOCUP >0,]#ojo

###Ninguna
Ninguna <- ggplot(ENOEB[ENOEB$CS_P13_1=="Ninguna",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Ninguna <- Ninguna + geom_smooth(aes(weight = FAC))
Ninguna <- Ninguna + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual") + ggtitle("Ingreso Mensual declarado por Escolaridad (Ninguna) y Sexo(ENOE, IV 2015) ")
Ninguna
ggsave("graphs/Ing_sexo_ninguna.png", plot = Ninguna, dpi = 500, width = 14, height = 11)

###
Primaria <- ggplot(ENOEB[ENOEB$CS_P13_1=="Primaria",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Primaria <- Primaria + geom_smooth(aes(weight = FAC))
Primaria <- Primaria + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual") + ggtitle("Ingreso Mensual declarado por Escolaridad (Primaria) y Sexo(ENOE, IV 2015) ")
Primaria
ggsave("graphs/Ing_sexo_primaria.png", plot = Primaria, dpi = 500, width = 14, height = 11)

###
Secundaria <- ggplot(ENOEB[ENOEB$CS_P13_1=="Secundaria",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Secundaria <- Secundaria + geom_smooth(aes(weight = FAC))
Secundaria <- Secundaria + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Secundaria) y Sexo(ENOE, IV 2015) ")
Secundaria
ggsave("graphs/Ing_sexo_secundaria.png", plot = Secundaria, dpi = 500, width = 14, height = 11)

###
Tecnica <- ggplot(ENOEB[ENOEB$CS_P13_1=="Carrera técnica",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Tecnica <- Tecnica + geom_smooth(aes(weight = FAC))
Tecnica <- Tecnica + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Carrera Técnica) y Sexo(ENOE, IV 2015) ")
Tecnica
ggsave("graphs/Ing_sexo_tecnica.png", plot = Preparatoria, dpi = 500, width = 14, height = 11)

####
Preparatoria <- ggplot(ENOEB[ENOEB$CS_P13_1=="Preparatoria o bach",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Preparatoria <- Preparatoria + geom_smooth(aes(weight = FAC))
Preparatoria <- Preparatoria + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Preparatoria) y Sexo(ENOE, IV 2015) ")
Preparatoria
ggsave("graphs/Ing_sexo_preparatoria.png", plot = Preparatoria, dpi = 500, width = 14, height = 11)

#####
Profesional <- ggplot(ENOEB[ENOEB$CS_P13_1=="Profesional",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Profesional <- Profesional + geom_smooth(aes(weight = FAC))
Profesional <- Profesional + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Profesional) y Sexo(ENOE, IV 2015) ")
Profesional
ggsave("graphs/Ing_sexo_Profesional.png", plot = Profesional, dpi = 500, width = 14, height = 11)

Maestria <- ggplot(ENOEB[ENOEB$CS_P13_1=="Maestría",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Maestria <- Maestria + stat_smooth(aes(weight = FAC))
#Maestria <- Maestria + geom_jitter(aes(weight = FAC))
Maestria <- Maestria + xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Maestría) y Sexo(ENOE, IV 2015) ")
Maestria
ggsave("graphs/Ing_sexo_Maestria.png", plot = Maestria, dpi = 500, width = 14, height = 11)

Doctorado <- ggplot(ENOEB[ENOEB$CS_P13_1=="Doctorado",], aes(EDA, INGOCUP, color = SEX)) #edad, Ingreso mensual, sexo, para personas con preescolar
Doctorado <- Doctorado + stat_smooth(aes(weight = FAC))
Doctorado <- Doctorado + geom_jitter(aes(weight = FAC))
Doctorado <- Doctorado+ xlab("Edad (14 a 75 años)") +ylab("Ingreso Mensual")  + ggtitle("Ingreso Mensual declarado por Escolaridad (Doctorado) y Sexo(ENOE, IV 2015) ")
Doctorado
ggsave("graphs/Ing_sexo_Doctorado.png", plot = Doctorado, dpi = 500, width = 14, height = 11)
