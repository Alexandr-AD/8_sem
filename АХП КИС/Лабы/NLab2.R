install.packages("PolynomF")
library(PolynomF)

Variant<-1
set.seed(Variant) 
X1<-sample(c(1:20),5)
X2<-sample(c(3:100),5)
X3<-sample(c(0:40),5)
pp1<-runif(5)
p1<-pp1/sum(pp1)
pp2<-runif(5)
p2<-pp2/sum(pp2)
pp3<-runif(5)
p3<-pp3/sum(pp3)
View(data.frame(X1, X2, X3, p1, p2, p3))

# Мат. ожидание
M1<-sum(X1*p1)
M1
M2<-sum(X2*p2)
M2
M3<-sum(X3*p3)
M3

M<-M1+M2+M3

#дисперсия
D1<-sum(p1*(X1-M1)^2)
D2<-sum(p2*(X2-M2)^2)
D3<-sum(p3*(X3-M3)^2)

D<-D1+D2+D3

#ср. кв. отклонение
SCO<-sqrt(D)

#коэффицент вариации
K<-(SCO/M)*100

poly<-function(X, p){
  y<-polynom()
  mnogo<-p[1]*y^X[1]+p[2]*y^X[2]+p[3]*y^X[3]+p[4]*y^X[4]+p[5]*y^X[5]
  return(mnogo)
}

poly1<-poly(X1, p1)
poly2<-poly(X2, p2)
poly3<-poly(X3, p3)

M11<-deriv(poly1)(1)
M22<-deriv(poly2)(1)
M33<-deriv(poly3)(1)

MM<-deriv(poly1*poly2*poly3)(1)

DD<-deriv(deriv(poly1*poly2*poly3))(1)+MM-MM^2

X123<-matrix(ncol=3, nrow=100000)

for (i in c(1:100000)) {
  possibillity<-runif(1)
  if(possibillity <= p1[1])
    X123[i,1]<-X1[1]
  else if(possibillity>p1[1] & possibillity<=p1[1]+p1[2])
    X123[i,1]<-X1[2]
  else if(possibillity>p1[1]+p1[2] & possibillity<=p1[1]+p1[2]+p1[3])
    X123[i,1]<-X1[3]
  else if(possibillity>p1[1]+p1[2]+p1[3] & possibillity<=p1[1]+p1[2]+p1[3]+p1[4])
    X123[i,1]<-X1[4]
  else
    X123[i,1]<-X1[5]
  
  if(possibillity <= p2[1])
    X123[i,1]<-X2[1]
  else if(possibillity>p2[1] & possibillity<=p2[1]+p2[2])
    X123[i,1]<-X2[2]
  else if(possibillity>p2[1]+p2[2] & possibillity<=p2[1]+p2[2]+p2[3])
    X123[i,1]<-X2[3]
  else if(possibillity>p2[1]+p2[2]+p2[3] & possibillity<=p2[1]+p2[2]+p2[3]+p2[4])
    X123[i,1]<-X2[4]
  else
    X123[i,1]<-X2[5]
}


View(X123)