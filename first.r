# создание переменных
a<-8
b<-12
c<-9
d<-(a+b>c)

# создание векторов
vec1<-c()
vec2<-1:3
vec3<-seq(5, 9, 3)
vec4<-c(vec1, vec2, vec3)

vec5<-1:15
var_2<-vec5[vec5%%2==0]
vec6<-vec5[c(2,3,5)]

sum(vec5[vec5>5])

# работа со списком
vec7<-seq(1, 10, 2)
vec8<-c("q", "w","e", "r", "t", "y")
vec9<-c(FALSE, TRUE, TRUE, TRUE, FALSE)
mylist<-list(int=vec7, char=vec8, bool=vec9)
line1<-mylist[[1]]
line2<-mylist[[2]][3]

# создание таблицы
df<-read.csv("c:/users/asus/desktop/book1.csv", sep=";", dec=",")
df$fact<-as.factor(df$fact)
df$date1<-as.Date(df$date1, "%d.%m.%y")
df$date2<-as.Date(df$date2, "%d.%m.%y")
str(df)

# создание df2
valueX<-ifelse((df$a > df$b), x<-c(df$a), 
               ifelse((df$a < df$b), x<-c(df$b), x<-0))
df2 <- data.frame(days = difftime(df$date1, df$date2), 
                  root = (-df$b)/df$k, valueX = c(valueX))

# подсчет строк
summary(df$fact)

# сохранение df2
write.csv(df2, "c:/users/asus/desktop/df2.csv")
