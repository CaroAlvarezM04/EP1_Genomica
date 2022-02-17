#EXAMEN PARCIAL 1
#Carolina Alvarez

#Librerias necesarias
library(igraph)
library(igraphdata)

#####
#PARTE 2

#objeto de tipo igraph precargado en igraphdata
data("karate")

#1.Encuentre las tres personas más conectadas.
sort(degree(karate), decreasing = TRUE)
#Las personas con mayor numero de conexiones son:
#John A
#Mr. Hi
#Actor 33

#2. La gráfica de la distribución de conectividades.
#calculo de la distribucion del numero de conexiones
dd <- degree.distribution(karate)
#histograma de la distribucion
hist(dd)
#distribución de cola larga mejor representada
plot(dd, main="Degree distribution", xlab="Degree", ylab="Frequency")

#3. El diámetro de la red.
diameter(karate)
#diametro = 13

#4. El coeficiente de clusterización cada una de las 3 personas más conectadas
transitivity(karate, v="Jhon_A")
#0.2556818
transitivity(karate, v="Mr. Hi")
#0.2556818
transitivity(karate, v="Actor 33")
#0.2556818

#5. Encuentre si los hay, a los nodos con coeficiente de clusterización de 1. Discute su significado.
#calcular el coeficiente local de clusterizacion
cc <- transitivity(karate, type = "local")
#convertir a vector
as.vector(cc)
#poner nombres de identificacion
names(cc) <- c(1:34)
cc
#Los nodos con coeficiente de clusterización 1 son:
#Actor 8, Actor 13, Actor 15, Actor 16, Actor 17, Actor 18, Actor 19, Actor 21, 
  #Actor 22, Actor 23, Actor 27
#Significa que todos los vecinos de estos nodos estan conectados entre si

#6. El porcentaje de conexiones respecto al total.
#calcular el numero total de conexiones
pd <- degree(karate)
#convertirlo a vector
pdv <- as.vector(pd)
#una tabla de resumen donde te dice cuantas veces se repiten los numeros de conexiones
pdtab <- table(pdv)
as.matrix(pdtab)
#la suma total de conexiones
sumD <- sum(pdtab)
#proporcion de una conexion = 32.35294
prop_uno <- 11*100/sumD
#proporcion de 2 conexiones = 17.64706
prop_dos <- 6*100/sumD
#proporcion de tres conexiones = 17.64706
prop_tres <- 6*100/sumD
#proporcion de cuatro conexiones = 8.823529
prop_cuatro <- 3*100/sumD
#proporcion de cinco conexiones = 5.882353
prop_cin <- 2*100/sumD
#proporcion de seis conexiones = 2.941176
prop_seis <- 1*100/sumD
#proporcion de 9 conexiones = 2.941176
prop_nie <- 1*100/sumD
#proporcion de 10 conexiones = 2.941176
prop_diez <- 1*100/sumD
#proporcion de 12 conexiones = 2.941176
prop_doce <- 1*100/sumD
#proporcion de 16 conexiones = 2.941176
prop_deis <- 1*100/sumD
#proporcion de 17 conexiones = 2.941176
prop_diet <- 1*100/sumD
#MI PLAN ERA EXTRAER ELEMENTOS DE LA MATRIZ PERO NO ME DEJO CONVERTIRLO A CARACTER NUMERICO

#7. El promedio de conectividades.
#calcular el numero de conexiones de la red
pd <- degree(karate)
#sacar el promedio con la funcion mean
mean(pd)
#4.588235

#8. Encuentre QUIÉNES son las 3 personas más importantes con al menos 3 distintos métodos
#Centralize a graph according to the betweenness of vertices
betw <- centr_betw(karate) #calcular la medida
betw <- as.vector(betw$res) #convertir a vector
names(betw) <- c(1:34) #identificar cada nodo por numero
sort(betw, decreasing = TRUE) #ordenar por medida
#Centralize a graph according to the degrees of vertices
de <- centr_degree(karate)
de <- as.vector(de$res)
names(de) <- c(1:34)
sort(de, decreasing = TRUE)
#Centralize a graph according to the closeness of vertices
clo <- centr_clo(karate)
clo <- as.vector(clo$res)
names(clo) <- c(1:34)
sort(clo, decreasing = TRUE)
#LAS PERSONAS MAS IMPORTANTES SON Mr. Hi, Actor 33 y John A

#9.Encuentre la trayectoria entre las personas más alejadas. 
farthest_vertices(karate)
#la distancia mas larga es de 13 pasos desde el actor 16 al actor 17

#10. Clusteriza la red con al menos 4 métodos distintos y discute tu resultado sabiendo que ese grupo de personas se separo en dos clubes distintos con el tiempo.
info <- cluster_infomap(karate)
membership(info)
plot(info, karate)

lp <- cluster_label_prop(karate)
membership(lp)
plot(lp, karate)

le <- cluster_leading_eigen(karate)
membership(le)
plot(le, karate)

op <- cluster_optimal(karate)
membership(op)
plot(op, karate)

#Con todos lo metodos se foman dos clusters son muchos miembros de forma notoria
#Y se forman pequeños clusters, podria explicar cierta interacción entre los grupos que se separaron
#Si alguien del grupo 1 tenia relacion con el grupo 2 pero tenia mas interaccion con otro pequeño grupo,
#es suficiente para que se forme un cluster pequeño que este conectado con los dos grandes.

##########
###17/02/2022
##########