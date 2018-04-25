Sys.setlocale('LC_ALL','C')
my.readCSV <- function(f) {
  L <- readLines(f, n = 1)
  if (grepl(";", L)) {
    print("Not english format - reading with csv2")
    df <- read.csv2(f, dec = ",")
  } else {
    if(grepl(",", L)) {
      print("English format")
      df <- read.csv(archivo, dec = ",")
    }
  }
  invisible(df)
}

archivo <- "seguros2.csv"
df <- my.readCSV(archivo)
head(df)
summary(df)
nrow(df)
ncol(df)
colnames(df)
res <- sapply(df, class)
kable(data.frame(variables=names(res),clase=as.vector(res)))


colnames(df_clean)
df_clean <- df[rowSums(is.na(df)) == 0,]
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

as.double(as.character(df_clean$Costos))

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

nrow(df_clean[rowSums(is.na(df_clean)) != 0,])
str(df_clean)

library(ggplot2)
set.seed(20)
k <- kmeans(df[, 2:3], 2, nstart = 20)
k$cluster <- as.factor(k$cluster)

set.seed(10)
k2 <- kmeans(df[, 2:4], 3, nstart = 1)
k2$cluster <- as.factor(k2$cluster)
k2
clusplot(df, k2$cluster, main = 'Cusplot')

centers <- 4
k2$cluster <- kmeans(df, centers, iter.max = 20, 
       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                     "MacQueen"), trace=FALSE)
k2
clusplot(df, k2, main = 'Cusplot')


library(cluster)
clusplot(df, k2$cluster, color=TRUE, shade = TRUE, lines=0)
clusplot(df, k$cluster, main = 'Cusplot')
k

?kmeans
