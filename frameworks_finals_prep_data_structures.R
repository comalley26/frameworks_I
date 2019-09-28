
a <- c(1, 45, "Nikhil") 
b <- c(2, 45, TRUE) 
c <- c(2, TRUE, "Nikhil")

class(a)
class(b)
class(c)

m1 = matrix(1:8,nrow=2,ncol=4) 
m2 = matrix(8:1,nrow=2,ncol=4)

m1*m2

set.seed(10) 
df = data.frame(id = 1:10, 
                gender=sample(c('Male','Female'),
                              size = 10,
                              replace = T), 
                attended=sample(c(T,F),
                                size = 10,
                                replace=T), 
                score=sample(x = 1:100,
                             size = 10,
                             replace = T), 
                stringsAsFactors = T) 
df

income = round(runif(n = 10,min = 50000,max = 80000)/1000)*1000
income

c(0, 1, 0, 1) == T 
c('Vishal','Rohan','Nikhil') == 'Nikhil'

str(diamonds)

diamonds[diamonds$price<1000,] %>% nrow()

diamonds[diamonds$x == 0,] %>% nrow()

diamonds[diamonds$price == 0,]

diamonds[diamonds$carat > 4, ] %>% nrow()

# Most common ways to read files into R:
# read.table() 
# read.csv() 
# read.delim()






