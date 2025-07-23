
# clase 1

# operaciones matemáticas ----

# recordar que podemos ejecutar cualquier línea de código poniendo 
# el cursor sobre la línea, en cualquier posición, y presionar el 
# botón "Run" que está arriba a la derecha del panel de scripts, o
# presionando la combinación de teclas control + enter

4 + 4
4 - 2
2 * 10
70 / 10
654^3

1+2+3+4+5+6+7+8+9+10
# esta es una suma del 1 al 10

# asignación ----
# Usamos el operador de asignación (<-) para asignar un valor a un objeto.
# option + guion (Mac) o alt + guion (Windows) para escribir el operador
# de asignación 

edad <- 32

# si ejecutamos un objeto, obtenemos su contenido
edad

año <- 2025
año

# los objetos pueden contener datos de cualquier tipo
animal <- "gato"

# podemos utilizar los objetos para realizar operaciones matemáticas
edad - año
año - edad


# ejemplo de presupuesto ----
# Si ejecutamos las siguientes líneas en orden, estaremos calculando un 
# presupuesto a partir de la creación de objetos y el cálculo basado en
# los objetos que vamos creando a partir de valores que podemos cambiar

presupuesto <- 30000

pizza <- 3000

gasto <- pizza * 3

restante <- presupuesto - gasto

restante
# obtenemos el presupuesto restante luego de simular una compra
# ejercicio: cambiar los valores que se asignan a los objetos 
# para obtener un resultado distinto


# ejemplo de presupuesto más complejo ----

presupuesto <- 30000

bebida <- 1500

gasto <- bebida * 9

papas <- 2500

# simulamos que las papas tienen un 20% de descuento
# Tomamos el objeto gasto, y volvemos a aumentar su valor, 
# sobreescribiendo el objeto con su nuevo resultado
gasto <- gasto + (papas*0.8) * 3

restante <- presupuesto - gasto

restante



# comparaciones ----

50 >= 50
40 < 50

# ejemplo de una comparación usando un objeto
limite <- 18
edad > limite

mayor_de_edad <- edad > 18

mayor_de_edad
# las comparaciones entregan como resultado valores de verdadero o falso (TRUE/FALSE)


# vectores ----

# podemos crear un vector, que es una secuencia de datos, con la función c()
edades <- c(31, 34, 56, 76, 38, 26, 42, 48)

# operaciones sobre vectores
edades + 100

edades - año

# crear un vector nuevos a partir de operaciones con objetos y vectores
años_nacimiento <- año - edades

años_nacimiento

# comparación sobre un vector
edades >= 18

edades > 40

# números decimales
3.2 + 4.1


# operadores ----

# el operador "&" (y)significa que ambas condiciones deben cumplirse
edades > 40 & edades < 60

# el operador "|" (o) significa que cualquiera de las condiciones debe cumplirse
edades > 40 | edades < 60

# crear una secuencia de números
1:10

# vectores de datos de texto
animales <- c("serpiente", "perro", "gato", "rata", "gallina", "pez")
mamíferos <- c("vaca", "perro", "caballo", "gato", "humano", "rata")


# el operador "%in%" significa que los valores del vector que estén en el 
# segundo vector serán verdaderos; puede leerse como "estos elementos que 
# estén en este conjunto"
animales %in% mamíferos

animales %in% c("gato", "gallina")

# obtener subconjuntos de vectores
animales
animales[1] # el primer elemento
animales[4] # el cuarto elemento
animales[90] # el elemento número 90 (no existe)

# seleccionar uno por uno todos los elementos del vector
animales[c(1, 2, 3, 4, 5, 6)]

# seleccionar todos los elementos del vector usando valores lógicos
animales[c(T, T, T, T, T, T)]
# equivale a decir, por cada valor, "sí", "sí", "sí", etc.

# filtrar un vector con valores lógicos: el elemento de la posición donde
# ponermos el FALSE será omitido
animales[c(T, FALSE, T, T, T, T)]



animales %in% favoritos

favoritos <- c("gato", "gallina")

# Usando el mismo principio, podemos filtrar los vectores a partir de una 
# comparación que retorne un vector de verdadero o falso


animales[animales %in% favoritos]


# funciones ----
# las funciones son programas que permiten aplicar ciertas cálculos sobre los datos 

# promedio de un vector
mean( c(1, 2, 3, 4, 5) )

# promerdio de un vector anteriormente creado
mean(años_nacimiento)

# mediana
median(años_nacimiento)

# mínimo y máximo
min(años_nacimiento)
max(años_nacimiento)
