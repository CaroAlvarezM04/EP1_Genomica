#EXAMEN PARCIAL 1
#Carolina Alvarez

#Librerias necesarias
library(igraph)
library(igraphdata)

#####
#PARTE 1

#Cargar la matriz de amigues
amikos <- read.csv("amikos.csv")
#Ponemos nombres a los renglones
row.names(amikos)<-amikos$...1
amikos<-amikos[,-1]
#Quitamos los datos con NA
amikos<-amikos[-2,]
amikos<-amikos[,-2]
#convertir la base a matriz
amikos<-as.matrix(amikos)
#Cargar la matriz de adyacencia
red_amikos<-graph_from_adjacency_matrix(amikos,mode="directed")

#1. Grafique la red
plot(red_amikos)

#2. Determine a las tres personas con mas amigues
#la funcion degree calcula el numero de conexiones por nodo
#Esta calculando el degree de entrada
amikos_in<-degree(red_amikos,mode="in")
#por orden decreciente el numero de nodos
sort(amikos_in, decreasing = TRUE)
#los tres nodos con mas amigues son Carolina, Mayela y Mafer con 12 nodos cada una

#3. Determine a las tres personas que considera que tiene mas amigues
#la funcion degree calcula el numero de conexiones por nodo
#Esta calculando el degree de salida
amikos_out<-degree(red_amikos,mode="out")
#por orden decreciente el numero de nodos
sort(amikos_out, decreasing = TRUE)
#los tres nodos que consideran a mas personas sus amigues son Enrique con 14, Julian y Mafer con 12

#4. Las tres personas más importantes por tres medidas de centralidad
#Centralizar un grafo según la cercanía de los vértices
centr_clo(red_amikos)
#Centralizar un grafo según los grados de los vértices
centr_degree(red_amikos)
#Centralizar un gráfico de acuerdo con la centralidad del vector propio de los vértices
centr_eigen(red_amikos)
#LAS PERSONAS MAS IMPORTANTES SON: Ernesto, Carolina, Mafer

#5. Clusteriza la red con al menos dos metodos y determine cuales son los clusters
#Estructura de la comunidad a través de caminatas aleatorias cortas
wc <- cluster_walktrap(red_amikos)
#que te diga a que cluster pertenece cada uno
members <- membership(wc)
#ordenar por orden decreciente 
sort(members, decreasing = TRUE)
##SE FORMAN 3 CLUSTERS: 
#1. Adrian, Fernanda, Ximena, Paloma, Enrique, Mayela, Julian, Erick
#2. Andrea, Rebeca, Ceci, Ernesto, Mafer, Maria, Nat, Mitzi, Karen, Carolina
#3. Sara, Isabel

#Optimal community structure
wc2 <- cluster_optimal(red_amikos)
#que te diga a que cluster pertenece cada uno
members2 <- membership(wc2)
#ordenar por orden decreciente 
sort(members2, decreasing = TRUE)
##SE FORMAN 3 CLUSTERS: 
#1. Rebe, Mafer, Enrique, Julian, Erick
#2. Andrea, Ceci, Ernesto, Maria, Nat, Mitzi, Karen, Carolina
#3. Adrian, Sara, Isabel, Fernanda, Xime, Paloma, Maye

#6. Calcule el diametro
diameter(red_amikos)
#Diametro = 3

#7. La matriz de distancias y dibuje un heatmap
mds <- distances(red_amikos)
heatmap(mds)

##########
###17/02/2022
##########