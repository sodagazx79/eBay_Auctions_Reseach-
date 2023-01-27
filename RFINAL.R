data<-read.csv(file.choose(),header=TRUE)
data

data_ebay<-read.csv(file.choose(),header=TRUE)
data_ebay


#reading numerical variables

watchers<-data[,2]
watchers

bidders<-data[,3]
bidders

bids<-data[,7]
bids

rating<-data[,8]
rating

Nrating<-data[,9]
Nrating

startp<-data[,10]
startp

shipping<-data[,12]
shipping

pics<-data[,15]
pics

SPrating<-data[,16]
SPrating

SNrating<-data[,17]
SNrating

SNArating<-data[,18]
SNArating

notprating<-cbind(SNrating,SNArating)

fprice<-data[,1]
fprice

daataaa<-data.frame(cbind(fprice,capacity,network,color,conditions,model,watchers,bidders,bids,SNrating,SPrating,SNArating,pics,startp,,rating,Nrating,shipping))


data1<-data.frame(cbind(fprice,watchers,bidders,bids,startp,shipping,pics,SPrating,SNrating,n.locked,c128,c256nhigher,c.fponw,c.white,c.black,c.silver,c.gold,c.blue,c.others,d.1day,d.3days,d.5days,c.eight,c.SE,c.12nhigher,c.eleven))

#reading categorical variables



network<-data$network
network
n.locked<-rep(-99,length(network))
for(i in 1:length(network)){
if (network[i]=="unlocked") {
n.locked[i]=0
} else {
n.locked[i]=1
}
}
cbind(network,n.locked)

capacity<-data$capacity
capacity

c128<-rep(-99,length(capacity))
c256nhigher<-rep(-99,length(capacity))

for(i in 1:length(capacity)){
if (capacity[i]=="128") {
c128[i]=1
} else {
c128[i]=0
}
if (capacity[i]=="256"|capacity[i]=="512") {
c256nhigher[i]=1
} else {
c256nhigher[i]=0
}
}
cbind(capacity,c128,c256nhigher)

color<-data$color
color
c.white<-rep(-99,length(color))
c.black<-rep(-99,length(color))
c.silver<-rep(-99,length(color))
c.gold<-rep(-99,length(color))
c.blue<-rep(-99,length(color))
c.others<-rep(-99,length(color))

for(i in 1:length(color)){
if (color[i]=="white") {
c.white[i]=1
} else {
c.white[i]=0
}
if (color[i]=="black") {
c.black[i]=1
} else {
c.black[i]=0
}
if (color[i]=="silver") {
c.silver[i]=1
} else {
c.silver[i]=0
}
if (color[i]=="gold") {
c.gold[i]=1
} else {
c.gold[i]=0
}
if (color[i]=="blue") {
c.blue[i]=1
} else {
c.blue[i]=0
}
if (color[i]=="green"|color[i]=="rose"|color[i]=="sierra blue"|color[i]=="red"|color[i]=="graphite"|color[i]=="purple") {
c.others[i]=1
} else {
c.others[i]=0
}
}

cbind(color,c.white,c.black,c.silver,c.gold,c.blue,c.others)


sum(as.numeric(c.white))
sum(as.numeric(c.black))
sum(as.numeric(c.silver))
sum(as.numeric(c.gold))
sum(as.numeric(c.blue))
sum(as.numeric(c.others))



conditions<-data$conditions
conditions
c.new<-rep(-99,length(conditions))
c.fponw<-rep(-99,length(conditions))

for(i in 1:length(conditions)){
if (conditions[i]=="fponw") {
c.fponw[i]=1
} else {
c.fponw[i]=0
}
}

cbind(conditions,c.fponw)

sum(as.numeric(c.new))
sum(as.numeric(c.fponw))


duration<-data$duration
duration
d.1day<-rep(-99,length(duration))
d.3days<-rep(-99,length(duration))
d.5days<-rep(-99,length(duration))
for(i in 1:length(conditions)){
if (duration[i]=="1day") {
d.1day[i]=1
} else {
d.1day[i]=0
}
if (duration[i]=="3days") {
d.3days[i]=1
} else {
d.3days[i]=0
}
if (duration[i]=="5days") {
d.5days[i]=1
} else {
d.5days[i]=0
}
}

cbind(duration,d.1day,d.3days,d.5days)

sum(as.numeric(d.1day))
sum(as.numeric(d.3days))
sum(as.numeric(d.5days))
sum(as.numeric(d.10days))



model<-data$model
model


c.eight<-rep(-99,length(model))
c.SE<-rep(-99,length(model))
c.12nhigher<-rep(-99,length(model))
c.eleven<-rep(-99,length(model))


for(i in 1:length(model)){
if (model[i]=="8"|model[i]=="8+") {
c.eight[i]=1
} else {
c.eight[i]=0
}
if (model[i]=="SE"|model[i]=="SE2nd") {
c.SE[i]=1
} else {
c.SE[i]=0
}
if (model[i]=="12"|model[i]=="12mini"|model[i]=="12pro"|model[i]=="12promax"|model[i]==13|model[i]=="13promax") {
c.12nhigher[i]=1
} else {
c.12nhigher[i]=0
}
if (model[i]=="11"|model[i]=="11pro"|model[i]=="11promax") {
c.eleven[i]=1
} else {
c.eleven[i]=0
}
}


cbind(model,c.eight,c.SE,c.12nhigher,c.eleven)

sum(as.numeric(c.eight))
sum(as.numeric(c.SE))
sum(as.numeric(c.thirteen))
sum(as.numeric(c.twelve))
sum(as.numeric(c.eleven))



data1<-data.frame(cbind(fprice,watchers,bidders,bids,startp,shipping,pics,SPrating,SNrating,n.locked,c128,c256nhigher,c.fponw,c.white,c.black,c.silver,c.gold,c.blue,c.others,d.1day,d.3days,d.5days,c.eight,c.SE,c.12nhigher,c.eleven))

data2<-as.matrix(data1)
x<-cbind(rep(1,dim(data2)[1]),data2[,2:26])  # create the design matrix
Y<-data2[,1]
Y<-sqrt(Y)
x<-x[,-1]
x1<-x[,1:25]
P<-dim(x1)[2]

id<-1:P
alpha.in<-0.1
alpha.out<-0.15
in.m<-NULL


p.val<-rep(-99,P)
for(i in 1:P){
  p.val[i]<-summary(lm(Y~x1[,i]))$coefficients[2,4]
}

c.enter<-id[order(p.val)[1]]
check<-p.val[c.enter] < alpha.in
if(check==1){in.m<-c(in.m,c.enter)}

while(check==1){
  
  cand<-id[-in.m]
  P.cand<-length(cand)
  P.in<-length(in.m)
  p.val1<-rep(-99,P.cand)
  
  for(i in 1:P.cand){
    p.val1[i]<-summary(lm(Y~x1[,c(in.m,cand[i])]))$coefficients[(P.in+2),4]
  }
  
  c.enter<-cand[order(p.val1)[1]]
  check1<-sort(p.val1)[1] < alpha.in
  
  if(check1==1){in.m<-c(in.m,c.enter)}
  P.in<-length(in.m)
  p.val2<-summary(lm(Y~x1[,in.m]))$coefficients[2:(P.in+1),4]
  
  c.exit<-in.m[order(p.val2,decreasing=TRUE)[1]]
  check2<-sort(p.val2,decreasing=TRUE)[1] > alpha.out
  if(check2==1){in.m<-in.m[-c.exit]}
  
  check<-check1==1 || check2==1
}

##################################################################
# These are the predictors that FSR have chosen to be in the model
in.m

#24=c.12nhigher,25=c.eleven,9=n.locked,11=c256nhigher,12=c.fponw,
#17=c.blue,1=watchers,4=startp,2=bidders,22=c.eight

fit.best<-lm(fprice~c.12nhigher+c.eleven+n.locked+c256nhigher+c.fponw
+c.blue+watchers+startp+bidders+c.eight,data=data1)
data3<-data.frame(cbind(c.12nhigher,c.eleven,n.locked,c256nhigher,c.fponw
,c.blue,watchers,startp,bidders,c.eight))

summary(fit.best)
cor(data3)
vif(fit.best)

Yhat=predict(fit.best)
ei=fprice-Yhat
qqnorm(ei)
qqline(ei)



plot(Yhat,ei,ylab="Residuals",xlab="Fitted values",xaxt="n", main="Residuals vs. Fitted values")
abline(0,0)

library(MASS)

boxcox(fit.best)

fit.trans<-lm(sqrt(fprice)~c.12nhigher+c.eleven+n.locked+c256nhigher+c.fponw
+c.blue+watchers+startp+bidders+c.eight,data=data1)
summary(fit.trans)

Yhat=predict(fit.trans)
ei=sqrt(fprice)-Yhat
qqnorm(ei)
qqline(ei)

plot(Yhat,ei,ylab="Residuals",xlab="Fitted values",xaxt="n", main="Residuals vs. Fitted values")
abline(0,0)

#####after box cox we did the transformation and too the square root of y now
##need to do that again

#24=c.12nhigher,25=c.eleven,9=n.locked,11=c256nhigher,12=c.fponw,
#10=c128,4=startp,2=bidders,22=c.eight

data4<-data.frame(cbind(c.12nhigher,c.eleven,n.locked,c256nhigher,c.fponw
,c128,startp,startp,bidders,c.eight))

fit.best1<-lm(fprice~c.12nhigher+c.eleven+n.locked+c256nhigher+c.fponw
+c128+startp+bidders+c.eight,data=data1)

summary(fit.best1)
Yhat=predict(fit.best1)
ei=fprice-Yhat
qqnorm(ei)
qqline(ei)

plot(Yhat,ei,ylab="Residuals",xlab="Fitted values",xaxt="n", main="Residuals vs. Fitted values")
abline(0,0)

boxcox(fit.best1)

fit.transe1<-lm(sqrt(fprice)~c.12nhigher+c.eleven+n.locked+c256nhigher+c.fponw
+c128+startp+bidders+c.eight,data=data1)

Yhat=predict(fit.transe1)
ei=sqrt(fprice)-Yhat
qqnorm(ei)
qqline(ei)

plot(Yhat,ei,ylab="Residuals",xlab="Fitted values",xaxt="n", main="Residuals vs. Fitted values")
abline(0,0)

data5<-as.matrix(data4)
data5new<-data5[-21,]

##############checking assumption one more time

data1<-data.frame(cbind(fprice,watchers,bidders,bids,startp,shipping,pics,SPrating,SNrating,n.locked,c128,c256nhigher,c.fponw,c.white,c.black,c.silver,c.gold,c.blue,c.others,d.1day,d.3days,d.5days,c.eight,c.SE,c.12nhigher,c.eleven))

data2<-as.matrix(data1)
dataa2<-data2[-21,]
#x<-cbind(rep(1,dim(dataa2)[1]),dataa2[,2:26])  # create the design matrix
Y<-dataa2[,1]
Y<-sqrt(Y)
x<-dataa2[,2:26]

P<-dim(x)[2]

id<-1:P
alpha.in<-0.1
alpha.out<-0.15
in.m<-NULL


p.val<-rep(-99,P)
for(i in 1:P){
  p.val[i]<-summary(lm(Y~x[,i]))$coefficients[2,4]
}

c.enter<-id[order(p.val)[1]]
check<-p.val[c.enter] < alpha.in
if(check==1){in.m<-c(in.m,c.enter)}

while(check==1){
  
  cand<-id[-in.m]
  P.cand<-length(cand)
  P.in<-length(in.m)
  p.val1<-rep(-99,P.cand)
  
  for(i in 1:P.cand){
    p.val1[i]<-summary(lm(Y~x[,c(in.m,cand[i])]))$coefficients[(P.in+2),4]
  }
  
  c.enter<-cand[order(p.val1)[1]]
  check1<-sort(p.val1)[1] < alpha.in
  
  if(check1==1){in.m<-c(in.m,c.enter)}
  P.in<-length(in.m)
  p.val2<-summary(lm(Y~x[,in.m]))$coefficients[2:(P.in+1),4]
  
  c.exit<-in.m[order(p.val2,decreasing=TRUE)[1]]
  check2<-sort(p.val2,decreasing=TRUE)[1] > alpha.out
  if(check2==1){in.m<-in.m[-c.exit]}
  
  check<-check1==1 || check2==1
}

##################################################################
# These are the predictors that FSR have chosen to be in the model
in.m



data6<-data.frame(cbind(fprice[-21],c.12nhigher[-21],c.eleven[-21],n.locked[-21],c256nhigher[-21],c.fponw[-21]
,c.blue[-21],watchers[-21],startp[-21],bidders[-21],c.eight[-21]))

fprice1<-data6[,1]
c.12nhigher1<-data6[,2]
c.eleven1<-data6[,3]
n.locked1<-data6[,4]
c256nhigher1<-data6[,5]
c.fponw1<-data6[,6]
c.blue1<-data6[,7]
watchers1<-data6[,8]
startp1<-data6[,9]
bidders1<-data6[,10]
c.eight1<-data6[,11]




fit.best3<-lm(fprice1~c.12nhigher1+c.eleven1+n.locked1+c256nhigher1+c.fponw1
+c.blue1+watchers1+startp1+bidders1+c.eight1,data=data6)



Yhat=predict(fit.best3)
#get thelength fprice
ei=fprice1-Yhat
qqnorm(ei)
qqline(ei)



plot(Yhat,ei,ylab="Residuals",xlab="Fitted values",xaxt="n", main="Residuals vs. Fitted values")
abline(0,0)

library(MASS)

boxcox(fit.best3)

fit.trans3<-lm(sqrt(fprice1)~c.12nhigher1+c.eleven1+n.locked1+c256nhigher1+c.fponw1
+c.blue1+watchers1+startp1+bidders1+c.eight1,data=data6)


Yhat=predict(fit.trans3)
ei=sqrt(fprice1)-Yhat
qqnorm(ei)
qqline(ei)

plot(Yhat,ei,ylab="Residuals",xlab="Fitted values",xaxt="n", main="Residuals vs. Fitted values")
abline(0,0)

boxcox(fit.trans3)


######extra
data7<-data.frame(cbind(c.12nhigher,c.eleven,c.fponw,n.locked,c.eight,c256nhigher,bidders
,startp,c128,c.blue,pics))

data8<-as.matrix(data7)
data9<-data8[-21,]

fit.best4<-lm(fprice[-21]~c.12nhigher[-21]+c.eleven[-21]+c.fponw[-21]+n.locked[-21]+c256nhigher[-21]+bidders[-21]
+startp[-21]+c128[-21]+c.blue[-21]+pics[-21],data=data7)



Yhat=predict(fit.best4)
#get thelength fprice
ei=fprice-Yhat
qqnorm(ei)
qqline(ei)



plot(Yhat,ei,ylab="Residuals",xlab="Fitted values",xaxt="n", main="Residuals vs. Fitted values")
abline(0,0)

library(MASS)

boxcox(fit.best4)

fit.trans4<-lm(sqrt(fprice[-21])~c.12nhigher[-21]+c.eleven[-21]+c.fponw[-21]+n.locked[-21]+c256nhigher[-21]+bidders[-21]
+startp[-21]+c128[-21]+c.blue[-21]+pics[-21],data=data7)



Yhat=predict(fit.trans4)
ei=sqrt(fprice)-Yhat
qqnorm(ei)
qqline(ei)

plot(Yhat,ei,ylab="Residuals",xlab="Fitted values",xaxt="n", main="Residuals vs. Fitted values")


boxcox(fit.trans4)

sort(hatvalues(fit.trans3),decreasing = T)[1:8]
sort(cooks.distance(model1), decreasing= T)[1:8]


