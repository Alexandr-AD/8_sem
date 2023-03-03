Data<-read.csv("https://raw.githubusercontent.com/junaart/ForStudents/master/KIS/Lab_1/Computers.csv",sep=",")
View(Data)
install.packages("lpSolve")
library(lpSolve)

W<-Data
cf<-lm(price~speed+ram+hd, Data)
# cf2<-lm(price~ram+screen+multi, Data)
# cf3<-lm(price~speed+hd+screen, Data)
# cf4<-lm(price~hd+screen+multi, Data)

cf$coefficients

# cf1$coefficients
# cf2$coefficients
# cf3$coefficients
# cf4$coefficients

# H1
objective.in<-c(1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 
                1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 
                1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 
                1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4])
max(Data$speed)
min(Data$speed)
max(Data$ram)
min(Data$ram)
max(Data$hd)
min(Data$hd)

min(Data$price)
max(Data$price)

# Ограничения:
# 6<=C1<=10
# 8<=C2<=14
# 5<=C3<=9
# 6<=C4<=15
limc1<-c(6, 10)
limc2<-c(8, 14)
limc3<-c(5, 9)
limc4<-c(6, 15)
speed<-mean(Data$speed)
speed
ram<-mean(Data$ram)
ram
hd<-mean(Data$hd)
hd

const.mat<-matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, # ограничение на x1
                    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, # ограничение на x5
                    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, # ограничение на x9
                    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, # ограничение на x13
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                    1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4],
                    0, 0.25*0.6, 0.25*0.3, 0.25*0.1, 0, 0.25*0.3, 0.25*0.3, 0.25*0.4, 0, 0.25*0.4, 0.25*0.5, 0.25*0.1, 0, 0.25*0.3, 0.25*0.3, 0.25*0.4),
                  nrow=37, byrow=TRUE)

const.dir<-c(">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
       ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
       ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
       ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
       "<=", "<=", "<=", "<=", ">=")
const.rhs<-c(limc1[1]*cf$coefficients[1], limc1[2]*cf$coefficients[1], limc1[1]*speed, limc1[2]*speed, 
       limc1[1]*ram, limc1[2]*ram, limc1[1]*hd, limc1[2]*hd,
       limc2[1]*cf$coefficients[1], limc2[2]*cf$coefficients[1], limc2[1]*speed, limc2[2]*speed, 
       limc2[1]*ram, limc2[2]*ram, limc2[1]*hd, limc2[2]*hd,
       limc3[1]*cf$coefficients[1], limc3[2]*cf$coefficients[1], limc3[1]*speed, limc3[2]*speed, 
       limc3[1]*ram, limc3[2]*ram, limc3[1]*hd, limc3[2]*hd,
       limc4[1]*cf$coefficients[1], limc4[2]*cf$coefficients[1], limc4[1]*speed, limc4[2]*speed, 
       limc4[1]*ram, limc4[2]*ram, limc4[1]*hd, limc4[2]*hd,
       20000, 20000, 20000, 20000, 1500)
ResH1<-lp("min", objective.in, const.mat, const.dir, const.rhs)
ResH1$solution

# H2
objective.in<-c(0, 0.25*0.6, 0.25*0.3, 0.25*0.1, 
                0, 0.25*0.3, 0.25*0.3, 0.25*0.4, 
                0, 0.25*0.4, 0.25*0.5, 0.25*0.1, 
                0, 0.25*0.3, 0.25*0.3, 0.25*0.4)

const.mat<-matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, # ограничение на x1
                    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, # ограничение на x5
                    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, # ограничение на x9
                    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, # ограничение на x13
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                    1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4],
                    1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4]),
                  nrow=37, byrow=TRUE)

const.dir<-c(">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
             ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
             ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
             ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
             "<=", "<=", "<=", "<=", "<=")
const.rhs<-c(limc1[1]*cf$coefficients[1], limc1[2]*cf$coefficients[1], limc1[1]*speed, limc1[2]*speed, 
             limc1[1]*ram, limc1[2]*ram, limc1[1]*hd, limc1[2]*hd,
             limc2[1]*cf$coefficients[1], limc2[2]*cf$coefficients[1], limc2[1]*speed, limc2[2]*speed, 
             limc2[1]*ram, limc2[2]*ram, limc2[1]*hd, limc2[2]*hd,
             limc3[1]*cf$coefficients[1], limc3[2]*cf$coefficients[1], limc3[1]*speed, limc3[2]*speed, 
             limc3[1]*ram, limc3[2]*ram, limc3[1]*hd, limc3[2]*hd,
             limc4[1]*cf$coefficients[1], limc4[2]*cf$coefficients[1], limc4[1]*speed, limc4[2]*speed, 
             limc4[1]*ram, limc4[2]*ram, limc4[1]*hd, limc4[2]*hd,
             20000, 20000, 20000, 20000, 80000)
ResH2<-lp("max", objective.in, const.mat, const.dir, const.rhs)
ResH2$solution

HH1<-function(p){return(sum(c(1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 
                              1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 
                              1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 
                              1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4])*p))}
HH2<-function(p){return(sum(c(0, 0.25*0.6, 0.25*0.3, 0.25*0.1, 
                                 0, 0.25*0.3, 0.25*0.3, 0.25*0.4, 
                                 0, 0.25*0.4, 0.25*0.5, 0.25*0.1, 
                                 0, 0.25*0.3, 0.25*0.3, 0.25*0.4)*p))}
HH1(ResH1$solution)
optProiz<-HH2(ResH1$solution)
optProiz

const.mat<-matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, # ограничение на x1
                    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, # ограничение на x5
                    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, # ограничение на x9
                    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, # ограничение на x13
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                    1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4], 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, cf$coefficients[2], cf$coefficients[3], cf$coefficients[4],
                    0, 0.25*0.6, 0.25*0.3, 0.25*0.1, 0, 0.25*0.3, 0.25*0.3, 0.25*0.4, 0, 0.25*0.4, 0.25*0.5, 0.25*0.1, 0, 0.25*0.3, 0.25*0.3, 0.25*0.4),
                  nrow=37, byrow=TRUE)

const.dir<-c(">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
             ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
             ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
             ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
             "<=", "<=", "<=", "<=", ">=")
const.rhs<-c(limc1[1]*cf$coefficients[1], limc1[2]*cf$coefficients[1], limc1[1]*speed, limc1[2]*speed, 
             limc1[1]*ram, limc1[2]*ram, limc1[1]*hd, limc1[2]*hd,
             limc2[1]*cf$coefficients[1], limc2[2]*cf$coefficients[1], limc2[1]*speed, limc2[2]*speed, 
             limc2[1]*ram, limc2[2]*ram, limc2[1]*hd, limc2[2]*hd,
             limc3[1]*cf$coefficients[1], limc3[2]*cf$coefficients[1], limc3[1]*speed, limc3[2]*speed, 
             limc3[1]*ram, limc3[2]*ram, limc3[1]*hd, limc3[2]*hd,
             limc4[1]*cf$coefficients[1], limc4[2]*cf$coefficients[1], limc4[1]*speed, limc4[2]*speed, 
             limc4[1]*ram, limc4[2]*ram, limc4[1]*hd, limc4[2]*hd,
             20000, 20000, 20000, 20000, optProiz)
ResHH1<-lp("min", objective.in, const.mat, const.dir, const.rhs)
ResHH1$solution

HH1(ResHH1$solution)
HH2(ResHH1$solution)


N1<-ResHH1$solution[1]/cf$coefficients[1]
N2<-ResHH1$solution[5]/cf$coefficients[1]
N3<-ResHH1$solution[9]/cf$coefficients[1]
N4<-ResHH1$solution[13]/cf$coefficients[1]

ideal_c1 <- c(ResHH1$solution[2]/N1, ResHH1$solution[3]/N1, ResHH1$solution[4]/N1)
ideal_c2 <- c(ResHH1$solution[6]/N2, ResHH1$solution[7]/N2, ResHH1$solution[8]/N2)
ideal_c3 <- c(ResHH1$solution[10]/N3, ResHH1$solution[11]/N3, ResHH1$solution[12]/N3)
ideal_c4 <- c(ResHH1$solution[14]/N4, ResHH1$solution[15]/N4, ResHH1$solution[16]/N4)

NewW1<-data.frame()
# W<-data.frame()
for (i in c(1:length(rownames(Data))))
{
  if ((Data[i,3]>60) & (Data[i,4]>1000) & (Data[i,5]>20))
    
    NewW1<-rbind(NewW1,W[i,])
}
View(NewW1)

NewW2<-data.frame()
for (i in c(1:length(rownames(Data))))
{
  if ((Data[i,3]<40) & (Data[i,4]<500) & (Data[i,5]>5))
    
    NewW2<-rbind(NewW2,W[i,])
}
View(NewW2)

NewW3<-data.frame()
for (i in c(1:length(rownames(Data))))
{
  if ((Data[i,3]>30) & (Data[i,4]>700) & (Data[i,5]>10))
    
    NewW3<-rbind(NewW3,W[i,])
}
View(NewW3)

NewW4<-data.frame()
for (i in c(1:length(rownames(Data))))
{
  if ((Data[i,3]>40) & (Data[i,4]>400) & (Data[i,5]>20))
    
    NewW4<-rbind(NewW4,W[i,])
}
View(NewW4)

normalize<-function(K)
{
  new_data<-K
  mmax<-max(K$price) 
  mmin<-min(K$price)
  price<-(mmax-new_data$price)*100/(mmax-mmin)
  mmax<-max(K$speed) 
  mmin<-min(K$speed)
  speed<-(new_data$speed-mmin)*100/(mmax-mmin)
  mmax<-max(K$hd)
  mmin<-min(K$hd)
  hd<-(new_data$hd-mmin)*100/(mmax-mmin)
  mmax<-max(K$ram)
  mmin<-min(K$ram)
  ram<-(new_data$ram-mmin)*100/(mmax-mmin)
  result<-data.frame(price,speed,hd,ram)
  rownames(result)<-rownames(new_data)
  return(result)
}

norm_W1=normalize(NewW1)
norm_W2=normalize(NewW2)
norm_W3=normalize(NewW3)
norm_W4=normalize(NewW4)

norm_W1
norm_W2
norm_W3
norm_W4

DPareto<-function(X,Y){
  p<-TRUE; l<-FALSE; i<-1;
  while (p & (i<=length(X))){
    if (X[i]<Y[i]) p<-FALSE
    if (X[i]>Y[i]) l<-TRUE
    i<-i+1
  }
  if (!p | !l) return(FALSE)
  else return(TRUE)
}

result1<-c()
for (i in c(1:length(rownames(norm_W1)))){
  p<-TRUE
  for (j in c(1:length(rownames(norm_W1)))){
    if (DPareto(norm_W1[j,],norm_W1[i,]))
      p<-FALSE}
  if (p) result1<-c(result1, rownames(norm_W1)[i])
}
result1

result2<-c()
for (i in c(1:length(rownames(norm_W2)))){
  p<-TRUE
  for (j in c(1:length(rownames(norm_W2)))){
    if (DPareto(norm_W2[j,],norm_W2[i,]))
      p<-FALSE}
  if (p) result2<-c(result2, rownames(norm_W2)[i])
}
result2

result3<-c()
for (i in c(1:length(rownames(norm_W3)))){
  p<-TRUE
  for (j in c(1:length(rownames(norm_W3)))){
    if (DPareto(norm_W3[j,],norm_W3[i,]))
      p<-FALSE}
  if (p) result3<-c(result3, rownames(norm_W3)[i])
}
result3

result4<-c()
for (i in c(1:length(rownames(norm_W4)))){
  p<-TRUE
  for (j in c(1:length(rownames(norm_W4)))){
    if (DPareto(norm_W4[j,],norm_W4[i,]))
      p<-FALSE}
  if (p) result4<-c(result4, rownames(norm_W4)[i])
}
result4

#функция определения расстояния между точками
distance<-function(A, B) {return(sqrt(sum((A-B)^2)))}

for (i in c(1,2,3,4))
{
  if (i==1)
  {
    k<-result1[1]
    k_min<-distance(ideal_c1, norm_W1[k,c(1:4)])
    for (j in result1)
    {
      if (distance(ideal_c1, norm_W1[j,c(1:4)])<k_min)
      {
        k_min<-distance(ideal_c1, norm_W1[j,c(1:4)])
        k<-j
      }     
    }
    cat("\n")
    print(paste("Лучший для C1: ",k,":", k_min))
    cat("\n")
  }
  else
    if (i==2)
    {
      k<-result2[1]
      k_min<-distance(ideal_c2, norm_W2[k,c(1:4)])
      for (j in result2)
      {
        if (distance(ideal_c2, norm_W2[j,c(1:4)])<k_min)
        {
          k_min<-distance(ideal_c2, norm_W2[j,c(1:4)])
          k<-j
        }  
      }
      cat("\n")
      print(paste("Лучший для C2: ",k,":", k_min))
      cat("\n")
    }
  else
    if (i==3)
    {
      k<-result3[1]
      k_min<-distance(ideal_c3, norm_W3[k,c(1:4)])
      for (j in result3)
      {
        if (distance(ideal_c3, norm_W3[j,c(1:4)])<k_min)
        {
          k_min<-distance(ideal_c3, norm_W3[j,c(1:4)])
          k<-j
        }  
      }
      cat("\n")
      print(paste("Лучший для C3: ",k,":", k_min))
      cat("\n")
    }
  else
    if (i==4)
    {
      k<-result4[1]
      k_min<-distance(ideal_c4, norm_W4[k,c(1:4)])
      for (j in result4)
      {
        if (distance(ideal_c4, norm_W4[j,c(1:4)])<k_min)
        {
          k_min<-distance(ideal_c4, norm_W4[j,c(1:4)])
          k<-j
        }  
      }
      cat("\n")
      print(paste("Лучший для C4: ",k,":", k_min))
      cat("\n")
    }
}

