
edades <- c(20, 30, 40, 46, 34, 36, 25, 75, 51)

edades

mean(edades)

median(edades)

median(edades)

sum(edades)

sum(c(1, 2, 3, 4, 5))

1+2+3+4+5+6+7

sum(5:55)

min(edades)
max(edades)

typeof(edades)
class(edades)
# integer (enteros)
# double (decimales)
class(6)
class("gallina")


paste("perro", "gato")

promedio <- mean(edades)

paste("el promedio es", promedio)

toupper("hola")
tolower("HOLA")


años <- c(1994, 1996, 2010, 2001)

class(años)

años <- c("1994", "1996", "2010", "2001")
class(años)

mean(años)

# NA: missing

class(as.numeric(años))
mean(as.numeric(años))

años_2 <- as.numeric(años)
años_2

años <- as.numeric(años)

años




sample(x = edades, size = 5)

sample(edades, 3)

sample(edades, 1)

sample(x = edades, 
       size = 5)

seq(from = 1960, to = 2030, by = 10)

seq(0, 1, 0.1)

rep("holi", 100)

promedio

round(promedio, 0)

paste("el promedio es", promedio)

paste("el promedio es", 
      # acá va el valor
      round(promedio, 1) # redondeado a 1 decimal
      
)
cifra <- 59345793

options(scipen = 99999) # desactovar notación científica

signif(cifra, digits = 1)


multiplicar <- function(numero) {
  numero * 100
}

multiplicar(numero = 0.5)

multiplicar(450)

# crear función
saludar <- function(nombre) {
  paste("hola", nombre, "¿cómo estás?")
}

saludar("Matías")

persona <- function(nombre, edad) {
  
  paste("la persona", nombre, "tiene una edad de", edad, "años") 
}

persona("Yael", 25)
persona(nombre = "Yael", edad = 25)
persona(25, "Yael") # mal


# multiplicar("gato")


precio <- 5990

numero <- 5990

# sacar decimales
numero2 <- round(numero, 0)
# poner separador de miles
numero3 <- format(numero2, big.mark = ".", decimal.mark = ",")
# poner signo peso
paste0("$", numero3)


# crear una función

pesos <- function(numero) {
  # sacar decimales
  numero <- round(numero, 0)
  
  numero <- signif(numero, 3)
  
  # poner separador de miles
  numero <- format(numero, 
                   big.mark = ".", decimal.mark = ",", trim = T)
  # poner signo peso
  paste0("$", numero)
}

pesos(precio)

pesos(34554)

sueldo <- 670000
pesos(sueldo)

precios <- c(1990, 10000, 40000, 103334, 4454645, 43454, 465656)

pesos(precios)

# format(signif(round(precios, 0), 3), big.mark = ".", decimal.mark = ",", trim = T)



ifelse(1 == 2,
       yes = "sipi",
       no = "nopi")

# conozco
animales <- c("serpiente", "perro", "gato", "rata", "gallina", "pez")

ifelse("sapo" %in% animales,
       yes = "conocido",
       no = "desconocido")

cifra <- 600

ifelse(cifra > 500,
       "alta",
       "baja")


precios

ifelse(precios > 50000,
       "caro",
       "barato")

edades > 35

ifelse(edades > 40, 
       "adulto", 
       "joven")


## control de flujo ----

precio <- 2000

if (precio > 10000) {
  # si es que se cumple
  precio_t <- pesos(precio)
  
  resultado <- paste("vale", precio_t, "así que es caro")
  
} else {
  # si es que NO se cumple
  
  resultado <- paste("si vale", precio, "es barato")
}

resultado

presupuesto <- 5000000000

cifra <- 2990

if (cifra > 1000000) {
  cifra <- signif(cifra, 2)
}

presupuesto - cifra



presupuesto <- 5000000000

cifra <- 2990

if (cifra > 10000) {
  cifra <- cifra/1000
}

presupuesto - cifra


cifra <- 33333333

cifra_2 <- ifelse(cifra > 10000,
                  yes = cifra/1000,
                  no = cifra)

cifra_2




# iteraciones


for (i in 1:10) {
  print(i)
}

for (i in 1:10) {
  if (i > 5) {
    numero <- i * 10
    print(numero)
  } else {
    print(i)
  }
}

for (edad in edades) {
  reporte <- paste("la persona tiene", edad, "años")
  
  print(reporte)
  
  write(reporte, 
        file = paste("reporte", edad, ".txt"))
}



for (edad in edades) {
  
  edad_sobre_promedio <- ifelse(edad > mean(edades),
                                "mayor al promedio",
                                "menor al promedio")
  
  reporte <- paste("la persona tiene", edad, "años,",
                   "entre un grupo donde el promedio es", mean(edades),
                   "la mínima es", min(edades), "y la máxima es", max(edades),
                   "la persona tiene una edad", edad_sobre_promedio)
  
  write(reporte, 
        file = paste("reporte", edad, ".txt"))
}





# crear un data frame

data.frame("primera" = c(1, 2, 3, 4, 5),
           "segunda" = c(10, 20, 30, 40, 50),
           "tercera" = c("a", "b", "c", "d", "e")
)

edades <- c(25, 30, 36, 42)
nombres <- c("juan", "maría", "pedrito", "jacinta")
color <- c("morado", "rosado", "verde", "naranja")

tabla <- data.frame(edades,
                    nombres,
                    color)

tabla


tabla$edades
tabla$nombres

tabla[, 1]

mean(tabla$edades)
mean(tabla[, 2])

tabla[1, ]

tabla[3, ]

tabla[1, 3]


clasificacion <- ifelse(tabla$edades > 18 & tabla$edades <= 30,
       "18 a 30",
       "+31")

clasificacion

tabla$grupo <- clasificacion

tabla

# tidyverse
library(dplyr)

select(tabla, color)

filter(tabla, edades > 30)

slice(tabla, 4)

mutate(tabla, 
       grupo = ifelse(edades > 18 & edades <= 30,
              "18 a 30",
              "+31"))
