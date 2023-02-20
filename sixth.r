dataframe <- datasets::swiss

install.packages('datasets')
library(datasets)
df <- data.frame(swiss)


install.packages('psych')
library(psych)
pairs.panels(df[c(2,3,4)])

plot(df$Examination, df$Education, main="График зависимой и независимой переменных")

summary(lm(df[[2]] ~ df[[3]]))
summary(lm(df[[2]] ~ df[[4]]))
summary(lm(df[[3]] ~ df[[4]]))

df2 <- data.frame(x = df[[4]])
pred <- predict(lm(df[[3]] ~ df[[4]]), data=df2)
lines(df2$x, pred, col="red")

pred2 <- predict(lm(df[[3]] ~ poly(df[[4]], 5)), newdata=df2)
lines(df2$x, pred2, col="black")

library(dplyr)
library(ggplot2)
ggplot(df, aes(x=df[[4]], y=df[[3]], color=df[[2]])
)+
  geom_point(alpha=1)+
  geom_smooth()+
  theme_classic()

install.packages('stargazer')
library(stargazer)
stargazer(lm(df[[3]] ~ df[[4]]), out='regression.html')
