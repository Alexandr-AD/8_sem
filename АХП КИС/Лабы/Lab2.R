install.packages("PolynomF")
library(PolynomF)

Variant<-1
# Задание 1 _____________________________________________________________________________________________________________________________
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

# дисперсия
D1<-sum(p1*(X1-M1)^2)
D2<-sum(p2*(X2-M2)^2)
D3<-sum(p3*(X3-M3)^2)

D<-D1+D2+D3

# ср. кв. отклонение
SCO<-sqrt(D)

# коэффицент вариации
K<-(SCO/M)*100

poly<-function(X, p){
  y<-polynom()
  mnogo<-p[1]*y^X[1]+p[2]*y^X[2]+p[3]*y^X[3]+p[4]*y^X[4]+p[5]*y^X[5]
  return(mnogo)
}

poly1<-poly(X1, p1)
poly2<-poly(X2, p2)
poly3<-poly(X3, p3)

# Мат. ожидание через производящую функцию
M11<-deriv(poly1)(1)
M22<-deriv(poly2)(1)
M33<-deriv(poly3)(1)

MM<-deriv(poly1*poly2*poly3)(1)

DD<-deriv(deriv(poly1*poly2*poly3))(1)+MM-MM^2

polySCO<- sqrt(DD)

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
    X123[i,2]<-X2[1]
  else if(possibillity>p2[1] & possibillity<=p2[1]+p2[2])
    X123[i,2]<-X2[2]
  else if(possibillity>p2[1]+p2[2] & possibillity<=p2[1]+p2[2]+p2[3])
    X123[i,2]<-X2[3]
  else if(possibillity>p2[1]+p2[2]+p2[3] & possibillity<=p2[1]+p2[2]+p2[3]+p2[4])
    X123[i,2]<-X2[4]
  else
    X123[i,2]<-X2[5]
  
  if(possibillity <= p3[1])
    X123[i,3]<-X3[1]
  else if(possibillity>p3[1] & possibillity<=p3[1]+p3[2])
    X123[i,3]<-X3[2]
  else if(possibillity>p3[1]+p2[2] & possibillity<=p3[1]+p3[2]+p3[3])
    X123[i,3]<-X3[3]
  else if(possibillity>p3[1]+p2[2]+p2[3] & possibillity<=p3[1]+p3[2]+p3[3]+p3[4])
    X123[i,3]<-X3[4]
  else
    X123[i,3]<-X3[5]
}

Pmean1<-mean(X123[,1])
Pmean2<-mean(X123[,2])
Pmean3<-mean(X123[,3])
Pmean<-Pmean1+Pmean2+Pmean3

PSCO1<-sd(X123[,1])
PSCO2<-sd(X123[,2])
PSCO3<-sd(X123[,3])

PD1<-PSCO1^2
PD2<-PSCO2^2
PD3<-PSCO3^2
PD<-PD1+PD2+PD3

PSCO<-sqrt(PD)

PK<-(PSCO/Pmean)*100

print("Мат. ожидание")
print("теория:")
print(paste("M1 =", M1 ))
print(paste("M2 =", M2 ))
print(paste("M3 =", M3 ))
print(paste("M =", M ))
print("Производящая функция:")
print(paste("M1 =", M11 ))
print(paste("M2 =", M22 ))
print(paste("M3 =", M33 ))
print(paste("M =", MM ))
print("Практика:")
print(paste("M1 =", Pmean1 ))
print(paste("M2 =", Pmean2 ))
print(paste("M3 =", Pmean3 ))
print(paste("M =", Pmean ))

print("Дисперсия")
print("теория:")
print(paste("D1 =", D1 ))
print(paste("D2 =", D2 ))
print(paste("D3 =", D3 ))
print(paste("D =", D ))
print("Производящая функция:")
print(paste("D =", DD ))
print("Практика:")
print(paste("D1 =", PD1 ))
print(paste("D2 =", PD2 ))
print(paste("D3 =", PD3 ))
print(paste("D =", PD ))

print("Ср. кв. отклонение")
print("теория:")
print(paste("sd =", SCO ))
print("Производящая функция:")
print(paste("sd =", polySCO ))
print("Практика:")
print(paste("sd1 =", PSCO1 ))
print(paste("sd2 =", PSCO2 ))
print(paste("sd3 =", PSCO3 ))
print(paste("sd =", PSCO ))

View(X123)

# Задание 2 _____________________________________________________________________________________________________________________________
install.packages(stats)
library(stats)

set.seed(Variant) 
lambda<-runif(3)
lambda

n<-300000
# lambda=1.2
tau<-2
a1<-lambda[1]*tau
a2<-lambda[2]*tau
a3<-lambda[3]*tau
#Функция rpois из пакета stats генерит случайные числа, распределенные по закону Пуассона с параметром a=lambda*tau
X1<-rpois(n,a1)
X2<-rpois(n,a2)
X3<-rpois(n,a3)

# p21<-c()
# p22<-c()
# p23<-c()

# print(paste("Мат. ожидание, дисперсия, ср. кв. отклонение", mean(X1), var(X1), sd(X1)))
# 
# print(paste("Мат. ожидание, дисперсия, ср. кв. отклонение", mean(X2), var(X2), sd(X2)))
# 
# print(paste("Мат. ожидание, дисперсия, ср. кв. отклонение", mean(X3), var(X3), sd(X3)))

M123<-a1+a2+a3
D123<-M123
sd123<-sqrt(D123)
K123<-sd123/D123*100

print(paste("Теор. Мат. ожидание, дисперсия, ср. кв. отклонение, к. вариации", M123, D123, sd123, K123))
print(paste("Мат. ожидание, дисперсия, ср. кв. отклонение, к. вариации", 
            mean(X1+X2+X3), var(X1+X2+X3), sd(X1+X2+X3), sd(X1+X2+X3)/var(X1+X2+X3)*100 ))

# Задание 2 _____________________________________________________________________________________________________________________________
Variant<-1
set.seed(Variant) 
alpha<-runif(7)[7]
alpha

# Задание 3 _____________________________________________________________________________________________________________________________
library(stats)
n<-30000
t <-c()
for (i in c(1:n)){
  i<-sum(rexp(5, alpha))
  t <- c(t, i)
}
t

mean(t)
5/alpha
var(t)
5/(alpha^2)
# Задание 5 _____________________________________________________________________________________________________________________________
Variant<-1 
set.seed(Variant) 
alpha<-runif(100)[77]
alpha
M_5<-2/alpha
M_5
D_5<-2/alpha^2
D_5

n<-100000
z1<-runif(n,0,2)
z2<-runif(n,0,10)
s<-sd(z1+z2)
s<-s^2
s