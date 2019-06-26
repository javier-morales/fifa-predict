library(cclust)
library(ggplot2)
theme_set(theme_light())


source("preprocessing.R")


# Some plots

ggplot(data, aes(x = Role, y = Value)) +
  geom_boxplot(fill = c("red", "green", "blue")) +
  scale_y_log10()

ggplot(data, aes(x = Role, y = Physicality)) +
  geom_boxplot(fill = c("red", "green", "blue")) +
  scale_y_log10()


# Histogram Value
ggplot(data, aes(x = Value)) +
  geom_histogram(color = "black", fill = "white") +
  scale_x_log10() +
  ylab("Counts")

# --------------
# ---- PCA -----
# --------------

comp <- princomp(data.m.s)
screeplot(comp)

set.seed(1)
kmeans.3 <- cclust(data.m[,c(2,3,4,12,13)], centers = 3, method = "kmeans")


ggplot() +
  geom_point(aes(x = comp$scores[,1], y = comp$scores[,2], colour = rainbow(3)[data$Role]), data) +
  geom_text(aes(x = comp$scores[1:6,1], y = comp$scores[1:6,2], label = data.players[1:6,1]),
            head(data), size = 2, nudge_y = 0.5, check_overlap = T) +
  theme(legend.position="none") +
  xlab("Comp. 1") + ylab("Comp. 2")

# K-means
ggplot() +
  geom_point(aes(x = comp$scores[,1], y = comp$scores[,2], col = rainbow(3)[kmeans.3$cluster])) +
  theme(legend.position="none") +
  xlab("Comp. 1") + ylab("Comp. 2")

# Value
ggplot() +
  geom_point(aes(x = comp$scores[,1], y = comp$scores[,2], col = cut(data.m.s[,2], breaks = 3))) +
  theme(legend.position="none") +
  xlab("Comp. 1") + ylab("Comp. 2")


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


#transformation for the Value?
par(mfrow=c(1,2))

eps <- 100
boxplot(data$Value, main = "Value")
boxplot(log(data$Value+eps), main = "Log-Value")

boxplot(data$Value~data$Role, main = "Value")
boxplot(log(data$Value)~data$Role, main = "Log-Value", col = c(2,3,4))

par(mfrow=c(1,1))

