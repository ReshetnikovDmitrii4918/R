mo <- 22
d <- 4.69
sigma <-sqrt(d)

norm1 <- rnorm(500, mo, sigma)
norm2 <- rnorm(500, mo, sigma)

t.test(norm1, norm2)

df <- data.frame(iris)

shapiro.test(df$Sepal.Length)
shapiro.test(df$Sepal.Width)
shapiro.test(df$Petal.Length)
shapiro.test(df$Petal.Width)

N <- 200
k10norm <- rnorm(N, 10, sqrt(sqrt(10)))
k10hi <- rchisq(N, 10)

k15norm <- rnorm(N, 15, sqrt(sqrt(15)))
k15hi <- rchisq(N, 15)

k20norm <- rnorm(N, 20, sqrt(sqrt(20)))
k20hi <- rchisq(N, 20)

k25norm <- rnorm(N, 25, sqrt(sqrt(25)))
k25hi <- rchisq(N, 25)

k30norm <- rnorm(N, 30, sqrt(sqrt(30)))
k30hi <- rchisq(N, 30)

ks.test(k10norm, k10hi)
ks.test(k15norm, k15hi)
ks.test(k20norm, k20hi)
ks.test(k25norm, k25hi)
ks.test(k30norm, k30hi)


df3 = force(HairEyeColor)
head(df3)
df3 = df3[,,2]
chisq.test(df3)


library(dplyr)
library(ggplot2)
df <- force(iris)
df <- df[,c(3,5)]
df <- df %>% filter (df$Species != "virginica")
ggplot (df, aes(x = Petal.Length)) + geom_bar(aes(fill=Species)) + 
      facet_grid(. ~ Species)

first = df %>% filter(df$Species == "setosa")
second = df %>% filter(df$Species == "versicolor")

shapiro.test(first$Petal.Length)
shapiro.test(second$Petal.Length)

bartlett.test(list(first$Petal.Length, second$Petal.Length))

t.test(first$Petal.Length, second$Petal.Length)
