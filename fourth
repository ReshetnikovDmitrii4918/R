#встроенный набор данных
data(InsectSprays)
force(InsectSprays)

#построение графика
boxplot(count~spray, xlab="Инсектициды", ylab="Кол-во выживших насекомых",
        main="Эффективность инсектицидов", col=rgb(sample(0:255/255), 
        sample(0:255/255), sample(0:255/255)), data=InsectSprays)

#рассчет средних значений
medianValues<-lapply(unstack(InsectSprays), mean)

#добавление средних значений
points(c(1:length(medianValues)), medianValues)

#встроенный набор данных и подключение пакета
data(ToothGrowth)
install.packages(ggplot2)
library(ggplot2)

#перевод в тип factor
ToothGrowth$dose<-as.factor(ToothGrowth$dose)

#нахождение среднего значения и отклонения
dev<-aggregate(len~dose, ToothGrowth, sd)
medium<-aggregate(len~dose, ToothGrowth, mean)$len

#построение графика
ggplot(ToothGrowth, aes(x=dose, y=len, color=dose))+
  geom_violin()+
  theme(legend.position="bottom")+
  geom_point(data=dev, aes(dose, medium))+
  geom_point(data=dev, aes(dose, medium + len))+
  geom_point(data=dev, aes(dose, medium - len))

#эскпоненциальное распределение
exp<-rexp(500)
hist(exp, main="Экспоненциальное распределение", xlab="x",
     ylab="Количесство", col="red", labels=TRUE)
curve(dexp(x)*250, add=T)

#нормальное распределение
norm<-rnorm(500)
hist(norm, main="Нормальное распределение", xlab="x",
     ylab="Количество", col="blue", labels=TRUE)
curve(dnorm(x)*250, add=T)

#3d pie
install.packages("plotrix")
library(plotrix)
data<-c(sum(ToothGrowth$len[ToothGrowth$dose==0.5]), 
        sum(ToothGrowth$len[ToothGrowth$dose==1]),
        sum(ToothGrowth$len[ToothGrowth$dose==2]))
pie3D(data, labels=c("0.5", "1", "2"), main="3D pie")
