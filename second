a<-17
b<-31
c<--2
d<-13
k<-7

#векторы вещественных чисел
vec1<-runif(5, a, b)
vec2<-runif(5, a, b)

#векторы целых чисел
vec3<-sample(c:d, 5)
vec4<-sample(c:d, 5)

#вектор положительных чисел
vec5<-sample(0:k, 5)

#объединение в датафреймы
df1<-data.frame(RealVec1 = vec1, RealVec2 = vec2, PosVec = vec5)
df2<-data.frame(IntVec1 = vec3, IntVec2 = vec4)

#Нахождение максимального значения
p_max<-c(apply(df1, 1, max), apply(df2, 1, max))
p_max

colunm_sum<-list(apply(df1, 2, function(x) sum(x[x>0])),
                 apply(df2, 2, function(x) sum(x[x>0])))
colunm_sum<-function(y) {
  result<-list(apply(y, 2, function(x) sum(x[x>0])))
  return(result)
}
mylist<-list(colunm_sum(df1), colunm_sum(df2))
mylist

#объединение датафреймов
my_df<-cbind(df1, df2)

negative_values<-list(apply(my_df, 2, function(test) test[test<0]))
negative_values<-function(z) {
  res<-list(apply(z, 2, function(test) test[test<0]))
  return(res)
}
negativelist<-list(negative_values(my_df))

my_df1<-as.data.frame(lapply(my_df,
    function(ran) ran[sample(c(TRUE, NA),
        prob = c(0.8, 0.2), size = length(ran),
        replace = TRUE)]))

#замена значений
na_values<-function(rep) {
  sapply(rep, function(nan) ifelse(is.na(nan),
      mean(nan, na.rm = TRUE), nan))
}
na_values(my_df1)

random<-data.frame(seq(length=50,from= -7, by=2))
y<-lapply(random, function(x) x^2+3*x-19)

#случайный датафрейм
data()
my_df2<-women

install.packages('dplyr')
library('dplyr')
#нечетные строки
A<-filter(my_df2, (row_number() %% 2 == 1))
#четные строки
B<-filter(my_df2, (row_number() %% 2 == 0))
my_df3<-rbind(A, B)

#выборка значений
fil<-select(my_df2, 1, function(x) x[1] > mean(x, na.rm = TRUE)
            & x[2] < mean(x, na.rm = TRUE))
arrange(fil, desc(fil)) %>%
slice(1:10)  

#график 
negative_values_plot<-function(val) {
  negative<-list()
  positive<-list() 
  for(i in my_df) {
    negative<-append(negative, length(which(i<0)))
    positive<-append(positive, length(which(i>0)))
  }
  
  matplot(c(1:length(my_df)), cbind(positive, negative),
          xlab='Столбцы',
          ylab='Кол-во значений',
          pch=3)
  lines(c(1:length(my_df)), positive, col="blue")
  lines(c(1:length(my_df)), negative, col="green")
}

#гистограмма
pic<-rnorm(50:100, mean=3, sd=0.3)
hist(pic, col="red", xlab='Значения',
     ylab='Количество', main="Гистограмма", freq=F)
lines(density(pic, bw=0.09), col = "black")
pic
