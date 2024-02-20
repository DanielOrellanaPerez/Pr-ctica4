nuevo_dir <- "C/practica4"
setwd(nuevo_dir)

#Ejercicio 1
library(readxl)
spear <- read_excel("C:/practica4/spearheads (2).xlsx")
str(spear)
class(spear)

spear <- as.data.frame(spear)
class (spear)
View(spear)
#Ejercicio 2

names(spear)[names(spear)== "Mat"]<- "Materiales" #Seleccionamos el operador y los corchetes, el doble igual nos compara los valores y los vamos cambiando a los nombres en español
names(spear)[names(spear)== "Con"]<- "Contexto"
names(spear)[names(spear)== "Cond"]<- "Conservacion"
names(spear)[names(spear)== "Loo"]<- "Loop"
names(spear)[names(spear)== "Peg"]<- "Remache"
names(spear)[names(spear)== "Date"]<- "Fecha"
names(spear)[names(spear)== "Maxwi"]<- "Ancho_max"
names(spear)[names(spear)== "Losoc"]<- "Longitud_encaje"
names(spear)[names(spear)== "Mawit"]<- "Ancho_Max_Encaje"
names(spear)[names(spear)== "Weight"]<- "Peso"
names(spear)[names(spear)== "Upsoc"]<- "Ancho_Encaje"
View(spear)

#Ejercicio 3 asigna_las_etiquetas
spear$Contexto=factor(spear$Contexto, levels=c('1','2','3'), labels=c("s/c", "Habitacional", "Funerario"))
spear$Conservacion=factor(spear$Conservacion, levels=c(1,2,3,4), 
                          labels=c("Excelente", "Bueno", "Regular", "Malo"))
spear$Remache=factor(spear$Remache, levels=c(1,2), labels=c('Si', 'No'))
spear$Materiales=factor(spear$Materiales, levels=c(1,2), labels=c('Bronce', 'Hierro'))
View(spear)


#Ejercicio 4
freq.mat=table(spear$Materiales)
View(freq.mat)
freq.con=table(spear$Contexto)
View(freq.con)
freq.cond=table(spear$Conservacion)
View(freq.cond)


#Ejercicio 5
cross.conmat=table(spear$Contexto,spear$Materiales)
View(cross.conmat)
cross.condmat=table(spear$Conservacion,spear$Materiales)
View(cross.condmat)



#Ejercicio 6
prop.mat=prop.table(freq.mat)
View(prop.mat)
prop.mat <- as.data.frame(prop.mat)

prop.mat$porcentaje <-prop.mat$Freq*100
prop.mat

prop.mat=prop.table(freq.mat)
View(prop.mat)
prop.mat <- as.data.frame(prop.mat)
prop.mat$Porcentaje <- prop.mat$Freq*100
prop.mat

prop.con=prop.table(freq.con)
View(prop.con)
prop.con <- as.data.frame(prop.con)
prop.con$Porcentaje <- prop.con$Freq*100
prop.con

prop.cond=prop.table(freq.cond)
View(prop.cond)
prop.cond <- as.data.frame(prop.cond)
prop.cond$Porcentaje <- prop.cond$Freq*100
prop.cond

#Ejercicio 7
prop.cross.condmat=round(prop.table(cross.condmat)*100)
View(prop.cross.condmat)
prop.cross.conmat=round(prop.table(cross.conmat) * 100)
View(prop.cross.conmat)


#Ejercicio 8

graf_con=barplot(table(spear$Conservacion), horiz = F)
graf_cont=barplot(table(spear$Contexto), horiz = F)

#Ejercicio 9

graf_rem=barplot(table(spear$Remache), horiz = T)
graf_mat=barplot(table(spear$Materiales), horiz = T)

#Ejercicio 10

bar.agrup = barplot(cross.condmat, widht = 0.9, main = "Estado de conservacion", xlab = "Material", ylab = "Conservacion")

#Creamos un porcenteje para cada factor y creamos un data frame para el gráfico de sectores y por último, creamos el gráfico de sectores para la variable conservación.

#Ejercicio 11

graf_set=pie(freq.cond)

#Ejercicio 12
graf_hist <- ggplot(melt(spear),aes(x=value,fill=variable))+geom_histogram(binwidth=10,color="red",alpha=0.7,position="identity")+
  labs(title="Histograma Conjunto de Variables Continuas", x="Valor",y="Frecuencia")+
  facet_wrap(~variable, scales = "free")
print(graf_hist)

