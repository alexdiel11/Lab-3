#Laboratorio Relacion entre Variables

x <- matrix(c(70,130,90, 95,450,30, 35,30,70),nrow=3)
x
rownames(x) <- c("Below Average","Average","Above Average")
x
colnames(x) <- c("Poor","Adequate","Excellent")
x
barplot(x, ylim=c(0,600), main = "Figueroa")
barplot(x,ylim=c(0,600), beside=TRUE, main="Figueroa")
barplot(x,beside=TRUE,legend.text=TRUE, ylim=c(0,500), main="Figueroa")
barplot(x,beside=TRUE, legend.text=TRUE, xlab="Nutritional Status", ylab="Frequency", main="Nutritional Status vs Academic Performance-Figueroa", col=rainbow(3))
barplot(x,beside=TRUE, legend.text=TRUE, xlab="Nutritional Status", ylab="Frequency", main="Nutritional Status vs Academic Performance-Figueroa", col=c("tomato","turquoise","violetred"))

x.margins <- addmargins(x)
print(x.margins)

x.cond.prob.fila <- prop.table(x,2)
round(print(x.cond.prob.fila,4))
x.cond.prob.col<-prop.table(x,2)
round(print(x.cond.prob.col), 4)

### 1 es fila, 2 es columna

barplot(x.cond.prob.fila,beside=TRUE,legend.text=TRUE, main="Figueroa")
barplot(x.cond.prob.col,beside=TRUE,legend.text=TRUE,ylim=c(0,0.8), main="Figueroa")

xr.cond.prob <- round(100*x.cond.prob.fila,1)
print(xr.cond.prob)
barplot(xr.cond.prob,beside=TRUE, legend.text=TRUE, xlab="Nutritional Status", ylab="Percent", main="Nutritional Status vs Academic Performance-Figueroa", col=rainbow(3))
barplot(xr.cond.prob,beside=TRUE,ylim=c(0,80), main="Figueroa")
abline(h=c(20,40,60,80))
box()
barplot(xr.cond.prob,beside=TRUE, legend.text=TRUE, xlab="Nutritional Status", ylab="Percent", main="Nutritional Status vs Academic Performance-Figueroa", col=rainbow(3), add=TRUE)
mosaicplot(xr.cond.prob,main=" Nutritional Status vs Academic Performance-Figueroa ",col=c("Sky Blue","Royal Blue"))

#Ejemplo Pesticidas
pesticidas=matrix(c(29,19485,98,7098),ncol=2)
rownames(pesticidas)=c("Organico","Convencional")
colnames(pesticidas)=c("Presente","Ausente")
pesticidas
apply(pesticidas,2,sum)/sum(pesticidas)
pesticidas.rel=pesticidas/apply(pesticidas,1,sum)
pesticidas.rel
barplot(t(pesticidas.rel),col=c("navy","lightblue"), ylim=c(0,1.55),legend=T, main="Figueroa")
barplot(t(pesticidas.rel),col=c("navy","lightblue"), ylim=c(0,1.25),legend=T,beside=T, main="Figueroa")
mosaicplot(pesticidas,col=c("navy","lightblue"), main="Frecuencias de presencia de pesticidas-Figueroa")
mosaicplot(pesticidas.rel,col=c("navy","lightblue"), main="Frecuencias relativas de presencia de pesticidas-Figueroa")

#Ejemplo Ingreso a Ciencias Naturales
naturales06.frm=read.csv("naturales2006.csv")
naturales06.frm
apply(naturales06.frm,2,sum)
round(100*apply(naturales06.frm,2,sum)/sum(naturales06.frm,2))
round(100*naturales06.frm/apply(naturales06.frm,1,sum),2)
barplot(t(as.matrix(naturales06.frm)),legend=T,col=c("Blue","Red"),beside=T,cex.names=0.75,main="Figueroa")
mosaicplot(naturales06.frm,main="Figueroa",col=c("Blue","Red"))

#Ejemplo 3.4 Libro Texto

Religious=matrix(c(229,276,297,243,88,59,103,40,49,16),nrow = 2)
colnames(Religious)=c(" 0 "," 1-9 "," 10-19 "," 20-39"," 40 or more")
rownames(Religious) =c("Female","Male")
Religious
Religious.rel= Religious /apply(Religious,1,sum)
round(Religious.rel,4)
barplot(Religious.rel,beside=TRUE,legend.text=TRUE, ylim=c(0,0.5), main="Figueroa")



#Relacion entre variables Cuantitativas

testdata=read.csv("testscores.csv",header=TRUE)
str(testdata)
head(testdata)
plot(testdata$MIDTERM,testdata$FINAL, ylim=c(0,90), main="Figueroa")

plot(testdata$MIDTERM,testdata$FINAL, pch=2, main="Figueroa")

plot(testdata$MIDTERM,testdata$FINAL, pch=13, main="Figueroa")

plot(testdata$MIDTERM,testdata$FINAL, pch=13, cex=2,ylim=c(0,90), main="Figueroa")
plot(testdata$MIDTERM,testdata$FINAL, pch=13, cex=0.5,ylim=c(0,90), main="Figueroa")
plot(testdata$MIDTERM,testdata$FINAL, pch=16, ylim=c(0,90),main="Figueroa")
plot(testdata$MIDTERM,testdata$FINAL, pch=5, cex=1.5, ylim=c(0,90), col="red", main="Figueroa")
plot(testdata$MIDTERM,testdata$FINAL, pch=22, cex=1.5,ylim=c(0,90), bg="lightblue", col="red", main="Figueroa")  
plot(testdata$MIDTERM,testdata$FINAL, pch=24, cex=1.5,ylim=c(0,90), bg="orange", col="black", xlab="Midterm Exam Scores", ylab="Final Exam Scores", main="Correlating Final Exam and Miterm Exam Scores-Figueroa")

pairs(testdata, main="Figueroa")

#######Ejemplo
GPA <- c(2.8, 3.6, 3.4, 3, 3.1, 3.3, 2.7, 3.8)
Time <- c(14, 25, 15, 5, 10, 12, 5, 21)
GPA
Time
cor(Time,GPA)
reg_lin <- lm(GPA ~ Time)
reg_lin
plot(Time, GPA, main="Figueroa")
plot(Time, GPA, ylim=c(0,4),main="Figueroa")
abline(reg_lin) 

valor.predecido <- data.frame(Time = c(5, 25))
predict(reg_lin, valor.predecido)


###########Ejemplo

humandev.frm=read.csv("human_development.csv")
attach(humandev.frm)
names(humandev.frm)
plot(GDP,INTERNET,ylab="Usuarios de Internet (%)",  xlab="PIB (miles de dolares per capita)",  main="Uso de Internet vs PIB-Figueroa",pch=16)

pairs(humandev.frm)

cor(GDP,INTERNET)
cor(humandev.frm)

internet.reg=lm(INTERNET ~ GDP)
coef(internet.reg)
plot(GDP,INTERNET,xlab="Producto Interno Bruto (per capita)",  ylab="Uso de Internet (%)",pch=16, main="Figueroa")
abline(internet.reg,col=2)

predict(internet.reg,newdata=data.frame(GDP=c(10,15,20,25,30)))

#Ejemplo Asnos (se pichea)

asnos.frm=read.csv("asnos.csv")
attach(asnos.frm)
plot(x,y,xlab="Inversion en Educacion (% del presupuesto)",  ylab="Numero de asnos",pch=16, main="Figueroa")
cor(x,y)
asnos.reg=lm(y~x)
abline(asnos.reg,col=2)
coef(asnos.reg)

#Missing values in R
x<-c(24, 14, 17, 25, 12, NA, 11, NA)
mean(x)
mean(na.omit(x))
mean(x, na.rm = TRUE)
sort(x)


data_na.frm=read.csv("DatosPerdidos.csv")
attach(data_na.frm)
data_na.frm
cor(Edad, Peso)
cor(Edad, Peso, use='complete.obs')
mean(Edad)
mean(Edad, na.rm = TRUE)
quantile(Peso)
quantile(Peso, na.rm = TRUE)

####Ejemplo
admision.frm=read.csv("admitidos.csv",header=TRUE)
attach(admision.frm)
table(GENERO)
table(TIPO)
table(GPA)
boxplot(GPA~GENERO,main="Figueroa")
boxplot(IGS~TIPO, main="Figueroa")
boxplot(GPA~GENERO:TIPO, main="Figueroa")
sapply(split(GPA,GENERO),mean)
sapply(split(IGS,TIPO),quantile)
table(GENERO,TIPO)
xy <- matrix(c(181,132,84,103),nrow=2)
rownames(xy) <- c("Femenino","Masculino")
colnames(xy) <- c("Privada","Publica")
xy.margins <- addmargins(xy)
print(xy.margins)
xy.cond.prob <- prop.table(xy,2)
print(xy.cond.prob)
barplot(xy.cond.prob,beside=TRUE, xlab=" ", ylab="Percent", main="Comparacion Tipo de Escuela - Genero-Figueroa", col=rainbow(2))
mosaicplot(xy.cond.prob,main=" Comparacion Tipo de escuela - Genero-Figueroa ",col=c("Blue","Red"))
plot(GPA,IGS,ylab="IGS", xlab="GPA", main="Comparacion GPA - IGS-Figueroa",pch=16)
pairs(admision.frm)
cor(GPA,IGS)
lm.result=lm(IGS~GPA)
lm(formula = IGS ~ GPA)

#Asignación
Happines = matrix(c(26,117,172,233,473,383,164,293,132), nrow=3)
colnames(Happines)=c("Not Too Happy","Pretty Happy","Very Happy")
rownames(Happines)=c("Above Avg.","Average","Below Avg.")
Happines
Happines.H=Happines/apply(Happines,1,sum)
round(Happines.H,4)


GDP=c(3,8,9,10,11,20,29,30,31,31,31,34,41)
OIL=c(1,2,4,7,8,18,12,13,11,12,16,26,26)

GDP.OIL= read.csv("GDP.OIL1.csv")
attach(GDP.OIL)
names(GDP.OIL)
plot(GDP.OIL,ylab=("Consumo de petroleo"),xlab=("Ingreso per capita"),main="Figueroa",pch=16)
x<-GDP
y<-OIL
cor(y,x)
p=c(x,y)
gdp.oil=lm(y~x)
abline(gdp.oil,col=2)
reg_lin <- lm(y~x)
reg_lin
valor.predecido <- data.frame(x = c(5, 25))
predict(reg_lin, valor.predecido)
