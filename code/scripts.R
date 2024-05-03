library(dplyr)
library(ggplot2) 
library(scales)

#1. Carga de datos
setwd("C:/Data/hotel_bookings")
hotel<-read.csv("hotel_bookings.csv", header= TRUE,sep= ",")


#INSPECCION DE DATOS
head(hotel)
names(hotel)
str(hotel)
View(hotel)
dim(hotel)

#PRE-PROCESAMIENTO DE DATOS

#IDENTIFICACION DE DATOS FALTANTES
sum(is.na(hotel))

new_hotel <- hotel[complete.cases(hotel), ]

#DATOS DUPLICADOS
new_hotel <- unique(new_hotel)

#OUTLIERS 

#DATOS QUE USAREMOS (CHILDREN, BABIES, REQUIRED_CAR_PARKING_SPACES)
boxplot(new_hotel$children, main="Cantidad de niños según las reservas de hotel")
boxplot(new_hotel$babies, main="Cantidad de bebés según las reservas de hotel")
boxplot(new_hotel$required_car_parking_spaces, main="Personas que requieren parqueo")

#Visualizacion de datos

#PRIMERA PREGUNTA

#¿Cuántas reservas se realizan por tipo de hotel? o ¿Qué tipo de hotel prefiere 
#la gente?

freq_table <- table(new_hotel$hotel)
pie(freq_table, 
    main = "Reservas por tipo de hotel", 
    col = rainbow(length(freq_table)),
    labels = paste(names(freq_table), ": ", freq_table))


#SEGUNDA PREGUNTA

#¿Está aumentando la demanda con el tiempo?

freq_table <- table(new_hotel$arrival_date_year)
plot(names(freq_table), freq_table, 
     type = "o", 
     main = "Cantidad de reservas según el año de reservación.",
     xlab = "Año",
     ylab = "Cantidad de reservas",
     col = "blue",
     lwd = 2)


#TERCERA PREGUNTA Y CUARTA PREGUNTA
#¿Cuándo se producen las temporadas de reservas: alta, media y baja? 
#¿Cuándo es menor la demanda de reservas?

#Hallamos la cantidad de reservas por mes
table(new_hotel$arrival_date_month)

#Creamos los vectores de meses y la cantidad de reservas
meses <- c("April", "August", "December", "February", "January", "July", "June", "March", "May", "November", "October", "September")
reservas <- c(11089, 13877, 6780, 8068, 5929, 12661, 10939, 9794, 11791, 6794, 11160, 10508)

meses_reservas <- data.frame(Mes=meses, Reservas=reservas)

meses_reservas

# Calculamos la media y la desviación estándar de las ventas
media <- mean(meses_reservas$Reservas)
desviacion <- sd(meses_reservas$Reservas) 

media
desviacion

# Definimos los límites para cada nivel de demanda
limite_bajo <- media - desviacion
limite_alto <- media + desviacion

limite_bajo
limite_alto

# Función para clasificar la demanda en niveles de demanda bajo, medio y alto
clasificar_demanda <- function(reservas) {
  if (reservas < limite_bajo) {
    return("Bajo")
  } else if (reservas < limite_alto) {
    return("Medio")
  } else {
    return("Alto")
  }
}

# Aplicamos la función a nuestros datos y agregamos la clasificación al dataframe
meses_reservas$Nivel_Demanda <- sapply(meses_reservas$Reservas, clasificar_demanda)


#GRAFICO PARA HALLAR LOS NIVELES DE DEMANDA 
#Ordenamos por mes
meses_reservas <- meses_reservas[order(match(meses_reservas$Mes, month.name)), ]

colores <- c("Bajo" = "blue", "Medio" = "green", "Alto" = "red")
barplot(height = meses_reservas$Reservas,
        names.arg = meses_reservas$Mes,
        col = colores[meses_reservas$Nivel_Demanda],
        main = "Reservas por Mes según el Nivel de Demanda",
        xlab = "Mes",
        ylab = "Reservas")


#QUINTA PREGUNTA
# Contamos las reservas que incluyen niños y/o bebés
reservas_con_niños <- sum(new_hotel$children > 0)
reservas_con_bebes <- sum(new_hotel$babies > 0)
reservas_con_niños_y_bebes <- sum(new_hotel$children > 0 | new_hotel$babies > 0)

# Resultados
cat("Reservas que incluyen niños:", reservas_con_niños, "\n")
cat("Reservas que incluyen bebés:", reservas_con_bebes, "\n")
cat("Reservas que incluyen niños y/o bebés:", reservas_con_niños_y_bebes, "\n")

# Vector con los recuentos
counts <- c(reservas_con_niños, reservas_con_bebes, reservas_con_niños_y_bebes)

# Vector con las etiquetas
labels <- c("Niños", "Bebés", "Niños y/o Bebés")

# Graficar el gráfico de barras
barplot(counts, 
        names.arg = labels,
        main = "Reservas que incluyen niños y/o bebés",
        xlab = "Categoría",
        ylab = "Cantidad de reservas",
        col = rainbow(length(counts)))

#SEXTA PREGUNTA
# Calcular la cantidad de hoteles con y sin espacios de estacionamiento para cada tipo de hotel
con_estacionamiento <- tapply(new_hotel$required_car_parking_spaces > 0, new_hotel$hotel, sum)
sin_estacionamiento <- tapply(new_hotel$required_car_parking_spaces == 0, new_hotel$hotel, sum)

con_estacionamiento
sin_estacionamiento


# Creamos una matriz de datos para el gráfico de barras apiladas
data_matrix <- rbind(con_estacionamiento, sin_estacionamiento)
data_matrix


# Etiquetas de leyenda y colores
legend_labels <- c("Con estacionamiento", "Sin estacionamiento")
colors <- c("blue", "red")

# Graficar el gráfico de barras apiladas
barplot(data_matrix, 
        main = "Cantidad de personas que necesitan estacionamiento por tipo de hotel",
        xlab = "Tipo de hotel",
        ylab = "Cantidad de hoteles",
        col = colors,
        legend.text = legend_labels,
        args.legend = list(x = "topright"),
        beside = TRUE)


#SEPTIMA PREGUNTA

# Calcular la cantidad de cancelaciones de reservas por mes
cancelaciones_por_mes <- table(new_hotel$arrival_date_month[new_hotel$is_canceled == 1])
cancelaciones_por_mes

# Ordenar los meses de enero a diciembre
cancelaciones_por_mes <- cancelaciones_por_mes[match(month.name, names(cancelaciones_por_mes))]
# Crear 
plot(cancelaciones_por_mes, type = "o", col = "blue", main = "Cancelaciones por Mes", xlab = "Mes", ylab = "Cantidad de Cancelaciones")




#PREGUNTA 8

# Agrupamos los datos por tipo de cliente y sumamos el número total de noches de fin de semana
resumen <- new_hotel %>%
  group_by(customer_type) %>%
  summarize(total_noches_finde = sum(stays_in_weekend_nights))

resumen

# Crea el gráfico de línea con área sombreada
ggplot(resumen, aes(x = customer_type, y = total_noches_finde, group = 1)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = 0, ymax = total_noches_finde), alpha = 0.3, fill = "blue") +
  labs(title = "Total de Noches de Fin de Semana por Tipo de Cliente",
       x = "Tipo de Cliente",
       y = "Total de Noches de Fin de Semana") +
  theme_minimal()






