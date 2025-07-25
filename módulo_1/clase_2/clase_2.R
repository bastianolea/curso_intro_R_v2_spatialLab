
# funciones ----

cifra <- 54314

# las funciones que empiezan con "is" nos permiten consultar si un objeto "es" de cierto tipo
is.numeric(cifra)
is.character(cifra)
# la respuesta llega como un dato tipo lógico: TRUE o FALSE


# para obtener el tipo de un objeto:
cifra2 <- "8575495"

typeof(cifra2)
class(cifra2)

is.numeric(cifra2)

# las funciones que empiezan con "as" convierten el tipo de los objetos
cifra3 <- as.numeric(cifra2) # convertir a numérico

typeof(cifra3)
class(cifra3)

# podemos convertir a formato numérico un vector de datos en caracter
cifras <- c("3442", "09486085", "6734637", "cuatromil", "3434")

class(cifras)
as.numeric(cifras)
# lo que ocurrió fue que el elemento numero 4 no pudo convertirse a numérico,
# por lo tanto, queda expresado como NA

# a este suceso se le llama "coerción"
prueba <- c(1, 2, 3, 4, 5, 6, "perro")

prueba
# en este caso, creamos un vector con elementos de dos tipos distintos, y como
# sabemos que los vectores sólo pueden ser de un tipo, R convierte todos los elementos
# al tipo de datos más "general", en este caso caracter


edades <- c(30, 40, 50, 55, 45, 46, 47, 23, 25, 74, 63)

mean(edades)

median(edades)

sum(edades)

min(edades)

max(edades)

# obtener cantidad de elementos del vector
length(edades)

# extraer una muestra aleatoria desde el vector
sample(edades, 1) # un elemento al azar
sample(edades, 5) # cinco elementos al azar

# crear una secuencia entre dos números
seq(from = 10, to = 100, by = 5)
# del 10 al 100, de 5 en 5

# del 1 al 2, avanzando en 0.1
seq(from = 1, to = 2, by = 0.1)

# podemos usar funciones para definir cualquier argumento de cualquier otra función
seq(from = min(edades), to = max(edades), by = 10)

# repetir un objeto
rep("hola", 10)

# crear una secuencia de años
años <- seq(1900, 2020, 10)

# crear una secuencia de números decimales
numeros <- seq(from = 0, to = 2, by = 0.132)

# redondear números decimales
round(numeros, 2)

# crear un vector de varios millones de números
numeros <- 500000:4000000

# ver cuántos números tiene el vector
length(numeros)

# extraer una muestra de este vector
ingresos <- sample(numeros, 20)

ingresos

# desactivar la notación científica
options(scipen = 99999)

ingresos

# redondear números enteros en sus cifras significativas
ingresos2 <- signif(ingresos, 3)


## crear funciones ----

# definimos los argumentos posibles de la función, y dentro de ella, usamos los argumentos
multiplicar <- function(numero) {
  numero * 100
}

# luego aplicamos la función y reemplazamos el argumento por el valor que queramos
multiplicar(numero = 0.5)

multiplicar(450)


# crear otra función
saludar <- function(nombre) {
  paste("hola", nombre, "¿cómo estás?")
}

saludar("Matías")

# crear una función de dos argumentos
persona <- function(nombre, edad) {
  
  paste("la persona", nombre, "tiene una edad de", edad, "años") 
}
persona("Yael", 25)
persona(nombre = "Yael", edad = 25)
persona(25, "Yael") # mal

# ejemplo de una función más compleja, que busca hacer varios pasos en uno solo

# primero haamos los pasos por separado:
numero <- 5990

# sacar decimales
numero2 <- round(numero, 0)
# poner separador de miles
numero3 <- format(numero2, big.mark = ".", decimal.mark = ",")
# poner signo peso
paste0("$", numero3)


# ahora creemos una función que englobe todos esos pasos

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

pesos(6990)

sueldo <- 670000
pesos(sueldo)

# la gracia es que podemos usar la función sobre un vector:
precios <- c(1990, 10000, 40000, 103334, 4454645, 43454, 465656)

pesos(precios)


# repaso de comparaciones
4 == 4
4 == 30
4 != 2


# ifelse ----
# función que a partir de una comparación, retorna un elementos si la
# comparación es TRUE, u otro si es FALSE
año <- 2025
ifelse(año == 2025, yes = "presente", no = "pasado")

año <- 2020
ifelse(año == 2025, "presente", "pasado")

ifelse(1 == 2,
       yes = "sipi",
       no = "nopi")


cifra <- 600

ifelse(cifra > 500,
       "alta",
       "baja")


edades > 35

ifelse(edades > 40, 
       "adulto", 
       "joven")

# en este ejemplo, la misma expresión de ifelse retorna distintos valores dependiendo de
# el dato que le entregamos (el objeto "año")

# apliquemos ifelse() al vector de ingresos anteriormente creado
ifelse(ingresos2 > 1000000, yes = "alto", no = "bajo")
# de este modo estamos convirtiendo una variable numérica a una variable categórica


# pegar texto ----
casos <- c(2, 4, 3, 1)

paste(casos, "casos")

# combinar paste() con ifelse() para escribir una palabra en plural dependiendo del dato
paste(casos, ifelse(casos > 1, "casos", "caso"))

# otra forma de hacer lo mismo, pero sólo cambiando la letra "s"
paste0(casos, " ", "caso", ifelse(casos > 1, "s", ""))




# flujo de control con if else ----
# esta técnica permite crear condicionalidad en la ejecución del código
# usando una comparación, se decide si el código se ejecutará o no
# el código sólo se ejecuta si la comparación retorna TRUE

basura <- 7000

basura

if (basura > 900) {
  # limpiar el dato porque viene en kilos en vez de toneladas
  basura <- basura /1000
  message("el valor es mayor a 900, por lo que será dividido en 1000")
} 

basura
# en este ejemplo, creamos un flujo de control donde un dato se divide sólo
# si se cumple un cierto criterio

# probemos cambiando el dato, esta vez agregando un mensaje que también confirma
# si la condición no se cumple
basura <-  6

if (basura > 900) {
  # limpiar el dato porque viene en kilos en vez de toneladas
  basura <- basura /1000
  message("valor anómalo: el valor es mayor a 900, por lo que será dividido en 1000")
} else {
  message("valor normal")
}

# en el apartado "else" podemos especificar código que se ejecutará si la
# condición es FALSE, o bien, podemos omitir el apartado "else" para que sólo
# se ejecute el código si la condición es TRUE


# otro ejemplo:
precio <- 2000

if (precio > 10000) {
  # si es que se cumple
  precio_t <- pesos(precio)
  
  resultado <- paste("vale", precio_t, "así que es caro")
  
} else {
  # si es que NO se cumple
  
  resultado <- paste("si vale", precio, "es barato")
}


# ejemplo en el que sólo se hace una operación si es que se cumple cierta condición
cifra <- 50000000
# cifra <- 2990

if (cifra > 1000000) {
  cifra <- signif(cifra, 2)
}
# sólo se aplica la corrección si corresponde en base al criterio que definimos



# funciones + ifelse ----


personas <- c("paula", "catherine", "luis", "basti", "natalia", "raul", "susana")

# crear otra función para saludar
saludar <- function(persona) {
  paste("¡hola ", persona, "!", sep = "")
}

saludar(personas)

# agreguémosle un nuevo argumento a la función, y dependiendo del argumento,
# la función hará algo distinto:
saludar <- function(persona, alegria) {
  
  exclamacion <- ifelse(alegria == TRUE, "!!!", "!")
  
  paste("¡hola ", persona, exclamacion, sep = "")
}

saludar(personas, alegria = FALSE)

saludar(personas, alegria = TRUE)



# en este caso, la función es igual a la anterior, pero al definir el segundo
# argumento de la función, le proveemos un valor por defecto (FALSE), de modo
# que la función puede usarse sin especificar el argumento, en cuyo caso el 
# argumento asumirá el valor por defecto
saludar <- function(persona, alegria = FALSE) {
  
  exclamacion <- ifelse(alegria == TRUE, "!!!", "!")
  
  paste("¡hola ", persona, exclamacion, sep = "")
}

# sin el argumento (asume el valor por defecto)
saludar(personas)

# con el argumento
saludar(personas, alegria = TRUE)


# en vez de un ifelse, podemos usar un rep(), para que la cantidad de exclamaciones
# se defina en un argumento
saludar2 <- function(persona, alegria = 3) {
  
  exclamacion <- rep("!", alegria)
  
  exclamacion2 <- paste(exclamacion, collapse = "")
  
  paste("¡hola ", persona, exclamacion2, sep = "")
}

saludar2(personas)

saludar2(personas, 10)




# creamos un argumento, el cual se usará para modificar el flujo de la función
saludar3 <- function(persona, alegria = 3, omitir = "no") {
  # if (persona %in% c("susana", "basti")) {
  #   paste("HOLA", "nombre", "QUE BUENO QUE VINISTEEE")
  # }
  
  if (omitir == "sí") {
    message("omitiendo")
    paste0(persona, ".")
    
  } else {
  
  exclamacion <- rep("!", alegria)
  
  exclamacion2 <- paste(exclamacion, collapse = "")
  
  persona <- ifelse(persona == "susana", toupper(persona), persona)
  
  paste("¡hola ", persona, exclamacion2, sep = "")
  }
}
# si se pone "no", la función funcionará normalmente, pero si se pone "sí", 
# la función se comportará distinto

saludar3(personas, alegria = 4)

saludar3(personas, alegria = 4, omitir = "sí")



# iteraciones ----
# en una iteración, se realiza una operación multiples veces en base al vector que entregues
pasos <- 10:20

for (i in pasos) {
 texto <- paste("este es el paso:", i)
 
 print(texto)
}
# en este caso tenemos un vector de 10 números, por lo que el código especificado
# se aplica a cada uno de los números, usando el objeto "i" como si fuera el objeto 
# que contiene el valor de cada paso (10, 11, 12, etc.)
 



# en una iteración, podemos controlar el flujo del código con if else
for (persona in personas) {

  saludo <- paste("hola", persona)
  
  if (persona == "susana") {
    saludo <- paste("chao", persona)
  }
  
  print(saludo)
}
# en este caso, ponemos una excepción para que, en un paso específico, 
# el comportamiento sea distinto

# ejemplo: sólo en los números mayores a 5 se va a aplicar cierta operación
for (i in 1:10) {
  if (i > 5) {
    numero <- i * 10
    print(numero)
  } else {
    print(i)
  }
}

# en este ejemplo, controlamos el flujo del código para que hayan múltiples condiciones
# y para cada condición se haga algo distinto
for (persona in personas) {
  
  if (persona == "susana") {
    saludo <- paste("holaaaa susanaaa!")
    
  } else if (persona == "catherine") {  
    saludo <- paste("excelente pregunta, catherine")
    
  } else if (persona == "basti") {
    saludo <- sample(c("serpiente", "perro", "gato", "rata", "gallina", "pez"), 1)
    
  } else {
    saludo <- paste("hola", persona)
  }
  
  print(saludo)
}
# por cada paso, el objeto "persona" asume el valor del elemento correspondiente del vector "personas",
# y avanza por el código probando si "persona" coincide con alguna de las condiciones dadas,
# y al final, si no cayó en ninguna de las comparaciones específicas, realiza una operación general para
# todos los demás casos


animo <- "feliz"
# animo <- "enojado"
# animo <- "triste"

for (persona in personas) {
  
  if (persona == "susana") {
    
    if (animo == "feliz") {
    saludo <- paste("holaaaa susanaaa!")
    
    } else if (animo == "enojado") {
      saludo <- paste("hola po susana")
    }
    
  } else if (persona == "catherine") {  
    saludo <- paste("excelente pregunta catherine")
    
  } else if (persona == "basti") {
    saludo <- sample(animales, 1)
    
  } else {
    saludo <- paste("hola", persona)
  }
  
  print(saludo)
}


# ejemplo de un loop que guarda archivos en el computador en base a los datos que se le entregan
for (edad in edades) {
  reporte <- paste("la persona tiene", edad, "años")
  
  print(reporte)
  
  write(reporte, 
        file = paste("reporte", edad, ".txt"))
}
# por cada cifra en el vector edades, genera un archivo de texto
# el archivo quedará guardado en la carpeta de inicio de tu disco duro (/)

# versión más compleja de lo anterior:
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



# data frames ----
# los data frames son tablas de datos construidas con vectores
edades

edades <- c(30, 43, 50, 28, 23)
ingresos <- c(400, 345, 200, 700, 1000)
pais <- c("chile", "perú", "colombia", "brasil", "argentina")

length(pais)

datos <- data.frame(edades,
           ingresos,
           pais)

datos

# ver características del dataframe
dim(datos) # filas y columnas
length(datos) # cantidad de columnas

# extraer una columna como vector
datos$ingresos
datos$pais

# selección de elementos en vectores
personas[5]

# en un dataframe, lo mismo extraería la columna en esa posición
datos[3]

# extraer una fila
datos[3, ]
# [x, y]

# extraer dos columnas en base a su posición
datos[ , 1:2]

# extraer dos columnas en base a su nombre
datos[ , c("ingresos", "pais")]

# realizar una operación en base a una columna, y guardarla como vector
año <- datos$edades - 2025

año

# luego, usamos ese nuevo vector para introducirlo al dataframe como
# una nueva columna
datos$año <- año

datos

# lo mismo se podría hacer de una forma más directa: creando la columna
# de inmediato a partir de la operación directa sobre una columna existente
datos$año <- datos$edades - 2025
datos

# aplicar ifelse para crear nueva columna en base a columna existente
datos$ingresos
datos$nivel <- ifelse(datos$ingresos >= 700, "alto", "bajo")
datos

# modificar una columna existente aplicándole una función
datos$año
datos$año <- abs(datos$año)
datos

# modificar una columna existente aplicándole una operación matemática
datos$ingresos
datos$ingresos <- datos$ingresos * 1000
datos
