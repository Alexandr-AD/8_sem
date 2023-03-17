Variant<-1
set.seed(Variant) 
k<-sample(c(10:25),1)
m<-sample(c(3:6),1)
t1<-sample(c(14:20),1)
t2<-sample(c(2:5),1)
# View(data.frame(k,m,t1,t2))

lambda<-k/t1
u<-1/t2
y<-lambda/u
sum0<-0

for (i in 0:m) {
  sum0<-sum0+(y^i/factorial(i))
}
P0=(sum0+(y^(m+1)/factorial(m)*(m-y)))^-1
1-P0
