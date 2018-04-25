
# ----- Ejercicio 2 + preproceso datos Seguros.csv (no obligatorio) -------
df <- read.csv2("seguros2.csv")
head(df)
head(df)
summary(df)
nrow(df)
ncol(df)
colnames(df)
res <- sapply(df, class)
kable(data.frame(variables=names(res),clase=as.vector(res)))


set.seed(20)
k <- kmeans(df[, 2:3], 2, nstart = 20)
k$cluster <- as.factor(k$cluster)

set.seed(10)
k2 <- kmeans(df[, 2:4], 3, nstart = 1)
k2$cluster <- as.factor(k2$cluster)
k2
clusplot(df, k2$cluster, main = 'Clusplot')

#APARTADO NO NECESARIO - PARA QUE FUERA, TENDRIAMOS QUE HABER LIDO DE SEGUROS.CSV
df_clean <- df[rowSums(is.na(df)) == 0,]
colnames(df_clean)
colnames(df_clean)[which(names(df_clean) == "Fecha.adquisicin")] <- "FA"
colnames(df_clean)[which(names(df_clean) == "A.o.compra.Coche")] <- "ACC"
colnames(df_clean)[which(names(df_clean) == "A.os.Coche")] <- "AC"
# Se podria remplazar por un for
df_clean$Todo.Riesgo <- as.ordered(gsub("S\355", "Si", df_clean$Todo.Riesgo))
df_clean$Obligatorio <- as.ordered(gsub("S\355", "Si", df_clean$Obligatorio))
df_clean$Cristales <- as.ordered(gsub("S\355", "Si", df_clean$Cristales))
df_clean$Incendio <- as.ordered(gsub("S\355", "Si", df_clean$Incendio))

sapply(df_clean$Numero.incidente, class)
sapply(df_clean$Edad, class)
sapply(df_clean$AC, class)
sapply(df_clean$Cubicaje, class)
sapply(df_clean$Costos, class)
as.numeric(gsub("\\.", "",as.character(df_clean$Numero.incidente)))
as.numeric(gsub("\\.", "",as.character(df_clean$ID.Asegurado)))
as.double(as.character(df_clean$Costos))

df_clean$Numero.incidente <- as.numeric(gsub("\\.", "",as.character(df_clean$Numero.incidente)))
df_clean$ID.Asegurado <- as.numeric(gsub("\\.", "",as.character(df_clean$ID.Asegurado)))
df_clean$Numero.incidente <- as.numeric(gsub("\\.", "",as.character(df_clean$Numero.incidente)))
df_clean$Costos <- as.double(as.character(df_clean$Costos))

str(df_clean)
summary(df_clean)

# Comparando 3 dimensiones Sexo, Edad, años del coche
k <- kmeans(na.omit(df_clean), center=4)
df_b <- read.csv2("seguros2.csv", dec = ",")
df_c <- read.csv2("seguros2.csv")
k <- kmeans(na.exclude(df_b), center=4)
k <- kmeans(na.exclude(df), center=4)
insuranceCluster <- kmeans(df_clean[, c(2,4,11)], 5, nstart = 20)


# ------ Ejercicio 3 -------------------------

dfi <- read.csv("iris.csv")
head(dfi)
summary(dfi)
nrow(dfi)
ncol(dfi)
colnames(dfi)
res <- sapply(dfi, class)
kable(data.frame(variables=names(res),clase=as.vector(res)))

set.seed(20)
irisCluster <- kmeans(dfi[, 3:4], 3, nstart = 20)
irisCluster

table(irisCluster$cluster, dfi$Species)


#Sepal.Lenght and Sepal.Width en vez de Petal (Pasamos un grupo a más que existe a ver que ocurre)
irisCluster2 <- kmeans(dfi[, 1:2], 4, nstart = 30)
irisCluster2

table(irisCluster2$cluster, dfi$Species)

library(cluster)
clusplot(dfi, irisCluster$cluster, color=TRUE, shade = TRUE, lines=0)
