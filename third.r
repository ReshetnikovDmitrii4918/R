#мат ожидание
lm=list()
lmerr=list()
top=c()
bottom=c()
lambda=5
mo=0.2
D=0.04
sig=0.2
beta4=0.0144

#зависимость матожидания
y_rexp<-rexp(1000, lambda)
a=c()
for(i in c(2:length(y_rexp))) {
  a=c(a, mean(y_rexp[1:i]))
}
lm=append(lm, list(a))
matplot(a, type="l", xlab = "N", ylab = "mu",
        ylim = c(-0.5, 0.9), xlim=c(0,1000), 
        main = "Зависимость мат ожидания от N")

for(j in c(2:100)) {
  y_rexp<-rexp(1000, lambda)
  a=c()
  for(i in c(2:length(y_rexp))) {
    a=c(a, mean(y_rexp[1:i]))
  }
  lm=append(lm, list(a))
  lines(a)
}
for(j in c(1:1000)) {
  top=c(top, mo+(3*sig)/j**(1/2))
  bottom=c(bottom, mo-(3*sig)/j**(1/2))
}

abline(a=mo, b=0, col="green")
lines(top, col="red")
lines(bottom, col="red")

#зависимость квадрата ошибки
a=c()
a=(lm[[1]]-mo)**2
plot(log2(c(1:999)), log2(a), type="l",
     xlab="N", ylab="Квадрат ошибки",
     ylim=c(-50, 0),
     main="Зависимость квадрата ошибки и 
     усредненного значения от N")
lmerr=append(lmerr, list(a))

for(j in c(2:100)) {
  a=c()
  a=(lm[[j]]-mo)**2
  lmerr=append(lmerr, list(a))
  lines(log2(c(1:999)), log2(a))
}
df=as.data.frame(lmerr)
lmerr_mid=rowMeans(df)
lines(log2(c(1:999)), log2(lmerr_mid),col="red")

#теор дисперсия
disp=vector()
for(i in c(1:1000)) {
  disp=append(disp, D/i)
}
plot(log2(c(1:1000)), log2(disp), type="l",
     ylim=c(-50, 0), xlab="N", ylab="",
     main="Теоретическая дисперсия и усредненная ошибка от N")
lines(log2(c(1:999)), log2(lmerr_mid),col="red")

#второй центральный момент
ldisp=list()
lderr=list()
top=c()
bottom=c()
#оценка дисперсии
y_rexp<-rexp(1000, 5)
a=c()
disp=c()
for(i in c(1:length(y_rexp))) {
  a=c(a, mean(y_rexp[1:i]))
}
for(i in c(2:length(y_rexp))) {
  disp=c(disp, mean((y_rexp[1:i]-a[1:i])**2))
}
ldisp=append(ldisp, list(disp))
matplot(disp, type="l", xlab="N", ylab="D",
        xlim=c(0, 1000),
        main="Оценка дисперсии",
        ylim=c(-0.6, 0.7))

for(j in c(2:100)) {
  y_rexp<-rexp(1000, 5)
  a=c()
  disp=c()
  for(i in c(1:length(y_rexp))) {
    a=c(a, mean(y_rexp[1:i]))
  }
  for(i in c(2:length(y_rexp))) {
    disp=c(disp, mean((y_rexp[1:i]-a[1:i])**2))
  }
  ldisp=append(ldisp, list(disp))
  lines(disp)
}
for(j in c(1:1000)) {
  top=c(top, D+3*((beta4-D**2)/j)**(1/2))
  bottom=c(bottom, D-3*((beta4-D**2)/j)**(1/2))
}
abline(a=D, b=0, col="green")
lines(top, col="red")
lines(bottom, col="red")

#квадрат ошибки
lderr=list()
a=c()
a=(ldisp[[1]]-D)**2

plot(log2(c(1:999)), log2(a), type="l",
     xlab="N", ylab="Квадрат ошибки",
     ylim=c(-50, 0),
     main="Зависимость квадрата ошибки дисперсии и
     усредненного значения от N")
lderr=append(lderr, list(a))
for(j in c(2:100)) {
  a=c()
  a=(ldisp[[j]]-0.04)**2
  lderr=append(lderr, list(a))
  lines(log2(c(1:999)), log2(a))
}

df=as.data.frame((lderr))
lderr_mid=rowMeans(df)
lines(log2(c(1:999)), log2(lderr_mid), col="red")

#теор и среднее значения
teor=vector()
for(i in c(1:1000)) {
  teor=append(teor, (beta4-D**2)/i)
}
plot(log2(c(1:1000)), log2(teor), type="l",
     ylim=c(-50, 0), xlab="N", ylab="",
     main="Зависимость теоретического и среднего 
     значений от N")
lines(log2(c(1:999)), log2(lderr_mid), col="red")
