library(cclust)
library(ggplot2)


source("preprocessing.R")

# --------------
# ---- PCA -----
# --------------

comp <- princomp(scale(data.m))
screeplot(comp)


kmeans.3 <- cclust(data.m[,c(1,2,3,4)], centers = 3, method = "kmeans")


par(mfrow = c(1, 2))
plot(comp$scores[,1], comp$scores[,2], col = kmeans.3$cluster, 
     main = "K-means", xlab = "Comp1", ylab = "Comp2")
points(comp$scores[1:10,1], comp$scores[1:10,2], pch = 8, col = 'Yellow')
text(comp$scores[1:3,1], comp$scores[1:3,2], labels = data$Name[1:3])
text(comp$scores[16120:16122,1], comp$scores[16120:16122,2], labels = data$Name[16120:16122], col = "Blue")
#text(comp$scores[c(234,237),1], comp$scores[c(234,237),2], labels = data$Name[c(234,237)], col = "Blue")
plot(comp$scores[,1], comp$scores[,2], col = cut(data$Overall, breaks = 3), main = "Overall",
     xlab = "Comp1", ylab = "Comp2")
points(comp$scores[1:10,1], comp$scores[1:10,2], pch = 8, col = 'Yellow')
text(comp$scores[1:3,1], comp$scores[1:3,2], labels = data$Name[1:3])
text(comp$scores[16120:16122,1], comp$scores[16120:16122,2], labels = data$Name[16120:16122], col = "Blue")
#text(comp$scores[c(234,237),1], comp$scores[c(234,237),2], labels = data$Name[c(234,237)], col = "Blue")
par(mfrow = c(1, 1))


##CALINSKI - HARABASZ

data.k <- data.m[1:2500,]

do.kmeans <- function (whatK)
{
  r <- cclust (data.k,whatK,iter.max=100,method="kmeans",dist="euclidean")
  (clustIndex(r,data.k, index="calinski"))
}

max (replicate (100, do.kmeans(5)))

# So it is not a matter of wrong initialization, this is really the best 5-means can do here

res <- rep(NA,10)
for (K in 2:10){
  res[K] <- max (replicate (100, do.kmeans(K)))
}
plot(res, type="l")


#Boxplots
par(mfrow=c(2,3))
vars <- c(18,19,20,21,22,23)
for (i in vars) {
  boxplot(data[,i]~data$Role, main = colnames(data)[i], col = c(2,3,4))
}

#transformation for the Value?
par(mfrow=c(1,2))

eps <- 100
boxplot(data$Value, main = "Value")
boxplot(log(data$Value+eps), main = "Log-Value")

boxplot(data$Value~data$Role, main = "Value")
boxplot(log(data$Value)~data$Role, main = "Log-Value", col = c(2,3,4))

par(mfrow=c(1,1))


##Value transformated
eps <- 10
Value.trans <- log(data$Value+eps)

plot(data$Nationality)
plot(data$Club)

plot(data$Preferred.Foot~data$Role, main = "Preferred Foot vS Role", xlab = "Role", ylab = "Preferred Foot")

par(mfrow=c(1,3))
boxplot(data$Overall, main = "Overall")
boxplot(data$Potential, main = "Potential")
boxplot(data$Value, main = "Value")
#-----------------
#----DATA VISUALIZATION II--#
#-----------------

#library(graphics)
#Star.var <- data.m[,11:16]
#Star.Best <- data.m[1:5,11:16]

#stars(Star.Best, scale = T)
