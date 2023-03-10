# 1 задание ------------------------------------------------------------------
install.packages("matrixcalc")
library(matrixcalc)

Variant<-1
set.seed(Variant) 
k<-sample(c(4:9),1)
pp1<-runif(4)
pp2<-runif(3)
pp3<-runif(2)
p1<-pp1/sum(pp1)
p2<-c(c(0),pp2/sum(pp2))
p3<-c(c(0,0),pp3/sum(pp3))
p4<-c(0,0,0,1)
P<-data.frame()
P<-rbind(P,p1)
P<-rbind(P,p2)
P<-rbind(P,p3)
P<-rbind(P,p4)
rownames(P)<-c("p1","p2","p3","p4")
colnames(P)<-c("","","","")
# View(P)
print(paste("k=",as.character(k)))

# теория
p0<-matrix(c(1, 0, 0, 0), ncol=4, nrow=1, byrow=TRUE)
print(p0)
print(P)

p0 %*% matrix.power(data.matrix(P),k)
p0 %*% matrix.power(data.matrix(P),k-1)
p0 %*% matrix.power(data.matrix(P),k-2)

findState<-function(state){
  rnum<-runif(1)
  if(state == 1)
  {
    if(rnum <= P[1, 1]){
      # sk[1] = sk[1]+1
      result = 1
    }
    else 
      if(rnum > P[1, 1] & rnum <= P[1,1]+P[1, 2]){
        # sk[2] = sk[2]+1
        result=2
      }
      else 
        if(rnum > P[1,1]+P[1, 2] & rnum <= P[1,1]+P[1, 2]+P[1,3]){
          # sk[3] = sk[3]+1
          result=3
        }
        else 
          if(rnum > P[1,1]+P[1, 2]+P[1,3] & rnum <= P[1,1]+P[1, 2]+P[1,3]+P[1,4]){
            # sk[4] = sk[4]+1
            result=4
          }
          
  }
  else
    if(state == 2)
    {
      if(rnum <= P[2, 2]){
        # sk[2] = sk[2]+1
        result = 2
      }
      else 
        if(rnum > P[2, 2] & rnum <= P[2,2]+P[2, 3]){
          # sk[3] = sk[3]+1
          result=3
        }
      else 
        if(rnum > P[2,2]+P[2, 3] & rnum <= P[2,2]+P[2, 3]+P[2,4]){
          # sk[4] = sk[4]+1
          result=4
        }
    }
  else
    if(state == 3)
    {
      if(rnum <= P[3, 3]){
        # sk[3] = sk[3]+1
        result = 3
      }
      else 
        if(rnum > P[3, 3] & rnum <= P[3,3]+P[3, 4]){
          # sk[4] = sk[4]+1
          result=4
        }
    }
  else
    if(state == 4)
    {
          # sk[4] = sk[4]+1
          result=4
    }
    return(result)
}
# практика
sk<-c(0,0,0,0)
sk1<-c(0,0,0,0)
sk2<-c(0,0,0,0)

lstate = 1

for (j in c(1:100000)) {
  for (i in c(1:4)) {
    lstate=findState(lstate)
  }  
  if(lstate==1)
    sk[1] = sk[1]+1
  else
    if(lstate==2)
      sk[2] = sk[2]+1
  else
    if(lstate==3)
      sk[3] = sk[3]+1
  else
    if(lstate==4)
      sk[4] = sk[4]+1
  lstate = 1
  for (i in c(1:3)) {
    lstate=findState(lstate)
  }  
  if(lstate==1)
    sk1[1] = sk1[1]+1
  else
    if(lstate==2)
      sk1[2] = sk1[2]+1
  else
    if(lstate==3)
      sk1[3] = sk1[3]+1
  else
    if(lstate==4)
      sk1[4] = sk1[4]+1
  lstate = 1
  for (i in c(1:2)) {
    lstate=findState(lstate)
  }  
  if(lstate==1)
    sk2[1] = sk2[1]+1
  else
    if(lstate==2)
      sk2[2] = sk2[2]+1
  else
    if(lstate==3)
      sk2[3] = sk2[3]+1
  else
    if(lstate==4)
      sk2[4] = sk2[4]+1
  lstate = 1
}
sk
sk1
sk2

spos<-c(0,0,0,0)
spos1<-c(0,0,0,0)
spos2<-c(0,0,0,0)

for (i in c(1:4)) {
  spos[i]<-sk[i]/100000
  
}
for (i in c(1:4)) {
  spos1[i]<-sk1[i]/100000
  
}
for (i in c(1:4)) {
  spos2[i]<-sk2[i]/100000
  
}
spos
p0 %*% matrix.power(data.matrix(P),k)
spos1
p0 %*% matrix.power(data.matrix(P),k-1)
spos2
p0 %*% matrix.power(data.matrix(P),k-2)
# 2 задание ------------------------------------------------------------------

Variant<-1
set.seed(Variant) 
k<-sample(c(10:25),1)
t1<-sample(c(14:20),1)
t2<-sample(c(2:5),1)
View(data.frame(k,t1,t2))

F1 <- function(lk, lt1, lt2, N) {
  lambda1 = 1/lt1
  lambda2 = 1/lt2
  
  set.seed(Variant)
  
  getTask <- vector(mode='double', length=N*lk)
  getTask[1:lk] <- rexp(lk, lambda1)
  for (i in 1:(N-1)) {
    getTask[(i*lk+1):(i*lk+lk)] <- getTask[((i-1)*lk+1):(i*lk)] + rexp(lk, lambda1)
  }

  getTask <- sort(getTask)
  
  freeServ <- vector(mode='double', length=length(getTask))
  qSize <- vector(mode='integer', length=length(getTask))
  waiTime <- vector(mode='double', length=length(getTask))
  decrQ <- vector(mode='double', length=length(getTask))
  
  time_free <- 0 
  
  for (i in 1:length(getTask)) {
    recTime <- getTask[i]
    requTime <- rexp(1, lambda2)
    
    if (time_free < recTime) {
      time_free <- recTime + requTime
      waiTime[i] <- requTime 
    } else {
      time_free <- time_free + requTime
      waiTime[i] <- time_free - recTime 
      decrQ <- append(decrQ, time_free)
    }
    decrQ <- decrQ[decrQ > recTime]
    qSize[i] <- length(decrQ)
    freeServ[i] <- time_free
  }
  
  return(list(
    getTask = getTask,
    freeServ = freeServ,
    qSize = qSize,
    waiTime = waiTime
  ))
}

F1_res_2 = F1(k, 80, t2, 10000)

# вероятность того, что программа не будет выполнена сразу же 
length(
  F1_res_2$qSize[F1_res_2$qSize > 0]
) / length(F1_res_2$qSize)

# среднее время до получения результатов
mean(F1_res_2$waiTime)

# среднее количество ожидающих программ
mean(F1_res_2$qSize)