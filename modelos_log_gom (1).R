
#Proyecciones con modelo Logistico y Gompertz
#Elaborado por: NAVA GONZALEZ MARIA NAYELI, VILCHIS SOTELO MARIA MIEL

fecha <- seq (1950, 2000, by=10) #Vector fechas
Y <- c(25.8,34.9, 48.2, 66.8, 81.2, 97.5) # datos poblacionales!!!! En millones.

x <- (fecha - 1950) / 10 #X

log_y <- log10(Y) # #Para gompertz

n <- (length(log_y)/3) ## Para gompertz

y_logis <- 1 / Y #Para modelo logistico

S1_l <- sum(y_logis[1:2]) #M logistico
S2_l <- sum(y_logis[3:4]) #M logistico
S3_l <- sum(y_logis[5:6]) #M logistico

bcuad_l <- (S3_l - S2_l) / (S2_l - S1_l) # b cuadrada logistico
raizb_l <- sqrt(bcuad_l) # para la ecuación logistica

a_log <- (S2_l - S1_l) * (raizb_l - 1) / (bcuad_l - 1)^2 # a para la ecuación logistica
k_log <- (S1_l - (bcuad_l - 1) * a_log/ (raizb_l - 1)) / 2 #k para la logistica


S1_g <- sum(log_y[1:2]) #M gompertz
S2_g <- sum(log_y[3:4]) #M gompertz
S3_g <- sum(log_y[5:6]) #M gompertz

bcuad_g <- (S3_g - S2_g) / (S2_g - S1_g) # b cuadrada gompertz
raizb_g <- sqrt(bcuad_g)   # b para compertz
a_gom <- (S2_g - S1_g) * (raizb_g - 1) / (bcuad_g - 1)^2 #a para gompertz
k_gom <- (S1_g - (bcuad_g - 1) * a_gom/ (raizb_g - 1)) / 2 #k para gompertz


i <- NULL
y1_logisfinal <-NULL 
y1_gomfinal <- NULL
for ( i in 1:6){ #For para agregar los elementos de un vector para gompertz y logistico
  
  y1_logisfinal[i] <- c(k_log + a_log * (raizb_l ^ x[i]))
  y1_gomfinal[i] <- c(k_gom + a_gom*(raizb_g^x[i]))
}

print(y1_logisfinal)
print(y1_gomfinal)

#Y estimados para ambos modelos
ajuste_logisfinal <- 1 / y1_logisfinal #Ajuste del mod logistico
ajuste_gomfinal <- 10^(y1_gomfinal) #Ajuste de modelo gompertz, el 10 es por el log base 10

data.frame(Fecha = fecha, Poblacion = Y,
           x, Y_logis = y_logis,  Y_logis_estim = y1_logisfinal, Ajust_Logistico = ajuste_logisfinal,Log_y = log_y,
           Y_gompertz = y1_gomfinal, Ajust_gompertz = ajuste_gomfinal ) # muestra los datos en forma de tablita

#Proyeccion para los años 2010-2070

fecha2 <- seq (1950, 2070, by=10) #Vector fechas
x1 <-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)

i <- NULL
logi_pro <-NULL 
gom_pro <- NULL
for ( i in 1:13){ #For para agregar los elementos de un vector para gompertz y logistico
  
  logi_pro[i] <- c(1/(k_log + a_log * (raizb_l ^ x1[i])))
  gom_pro[i] <- c(10^(k_gom + a_gom*(raizb_g^x1[i])))
}

print(logi_pro)
print(gom_pro)

f<- seq (1950, 2070, by=10) #Vector fechas
#Grafica Logistico
plot(f,logi_pro, main = "Proyecciones" , xlab = "Año (xi)", ylab = "Población en millones",font.sub  = 2, type="l",col = "red",lwd = 3)

#Grafica Gompertz
lines(f,gom_pro, type="l", col= "green", lwd = 2.5 )

f1 <- seq(1950,2000, by = 10)
#Grafica de datos reales
lines(f1,Y, type = "l", col= "blue", lwd = 4)

#Puntos de 2010,2020
x <- c(2010,2020)
y <- c(112.33,126.014)

lines(x,y, type = "p", col= "black", pch = 16)

legend("bottomright", legend = c("G.Logístico", "G.Gompertz", "G.datos reales", "Población real"),
       lwd = 3, col = c("red", "green","blue", "black"))








