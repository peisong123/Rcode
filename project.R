data <- read.csv(file="age.surgery.project.csv",header=TRUE,sep=",")
summary(data)
names(data)
head(data)

DeathAt30=data$DeathAt30
Age=data$Age
Female=data$Female
SurgType=data$SurgType
Height=data$Height
BMI=data$BMI
PrevMI=data$PrevMI

DeathAt30=as.factor(DeathAt30)
Female=as.factor(Female)
SurgType=factor(SurgType,c("Isolated CABG","Isolated Valve","CABG+Valve"))
PrevMI=as.factor(PrevMI)


myTable= xtabs(~Female+SurgType+PrevMI, data=data)
ftable(myTable)

data1=data.frame(Sex=c(rep("Yes",6),rep("No",6)),
                 PrevMI=c("No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes"),
                 SurgeryType=c("CABG+Valve","CABG+Valve","Isolated CABG","Isolated CABG","Isolated Valve","Isolated Valve","CABG+Valve","CABG+Valve","Isolated CABG","Isolated CABG","Isolated Valve","Isolated Valve"),
                 Count=c(86,37,92,82,245,29,188,86,470,301,343,41))
xtabs(Count~.,data=data1)
ftable(xtabs(Count~.,data=data1))

######## M independence
model.x.y.z=glm(Count~Sex+SurgryType+PrevMI,data=data1,family="poisson")
summary(model.x.y.z)

sum(fitted(model.x.y.z))

#####jointly independence
model.xy.z=glm(Count~Sex*SurgeryType+PrevMI,data=data1,family="poisson")
model.yz.x=glm(Count~Sex+SurgeryType*PrevMI,data=data1,family="poisson")
model.xz.y=glm(Count~Sex*PrevMI+SurgeryType,data=data1,family="poisson")

summary(model.xy.z)
summary(model.yz.x)
summary(model.xz.y)

##Conditional independence
model.xy.yz=glm(Count~Sex*SurgeryType+SurgeryType*PrevMI,data=data1,family="poisson")
model.xy.xz=glm(Count~Sex*SurgeryType+Sex*PrevMI,data=data1,family="poisson")
model.xz.yz=glm(Count~SurgeryType*PrevMI+Sex*PrevMI,data=data1,family="poisson")

summary(model.xy.yz)
summary(model.xy.xz)
summary(model.xz.yz)

model.xy.yz.xz = glm(Count~Sex*SurgeryType + SurgeryType*PrevMI+Sex*PrevMI,  data=data1, family="poisson") 
summary(model.xy.yz.xz)

data.frame(data1, X.Y.Z. = fitted(model.x.y.z),
           XY.Z = fitted(model.xy.z), YZ.X = fitted(model.yz.x),XZ.Y = fitted(model.xz.y),
           XY.YZ = fitted(model.xy.yz), XZ.YZ = fitted(model.xz.yz), XY.XZ = fitted(model.xy.xz),
           XY.YZ.XZ=fitted(model.xy.yz.xz))

modellist = list(model.x.y.z, 
                 model.xy.z,model.yz.x,model.xz.y, 
                 model.xy.yz,model.xz.yz, model.xy.xz,
                 model.xy.yz.xz)                   
                 
data.frame(Model=c("X,Y,Z","XY.Z", "YZ.X","XZ,Y","XY,YZ","XZ,YZ", "XY,XZ","XY,YZ,XZ"),
           Dev=round(unlist(lapply(modellist, deviance)),4), 
           X2=round(unlist(lapply(modellist, function(x){ sum( residuals(x,type="pearson")^2)} )),4), 
           AIC=round(unlist(lapply(modellist, AIC)),4),
           BIC=round(unlist(lapply(modellist, AIC,k=log(2000))),4),
           Df = unlist(lapply(modellist,function(x){ x$df.residual})))
                 
              
#####Part 2

linear.model=glm(DeathAt30 ~ Age+Female+ SurgType+ Height + BMI + PrevMI, data = data, family = "binomial")

summary(linear.model)

full.model = glm(DeathAt30 ~ Age*Female* SurgType* Height * BMI * PrevMI , data = data, family = "binomial")
summary(full.model)

library(MASS)
step=stepAIC(full.model,direction="both")
step$anova

model0=glm( DeathAt30 ~ Age + Female + SurgType + Height + BMI + PrevMI + 
             Female:SurgType + Female:Height + SurgType:Height + Female:BMI + 
             SurgType:BMI + Height:BMI + Female:PrevMI + SurgType:PrevMI + 
             Height:PrevMI + BMI:PrevMI + Female:Height:BMI + Female:SurgType:PrevMI + 
             Female:Height:PrevMI + SurgType:Height:PrevMI + Female:BMI:PrevMI + 
             SurgType:BMI:PrevMI, data = data, family = "binomial")
summary(model0)  

model1<-update(model0, ~.
                 -Female:SurgType:PrevMI -
                 Female:Height:PrevMI - SurgType:Height:PrevMI - Female:BMI:PrevMI -
                 SurgType:BMI:PrevMI
                -Female:PrevMI - SurgType:PrevMI - Height:PrevMI - BMI:PrevMI 
               )
summary(model1)


model2<-update(model1,~.-SurgType-SurgType:Height-SurgType:BMI)
summary(model2)


final.model<-update(model2,~.-PrevMI)
summary(final.model)

confint(final.model)
predict(final.model, data, type="response")
plot(residuals(final.model,type="pearson"))

plot(residuals(final.model,type="pearson")~Age,data=data,pch=16)

par(mfrow=c(2,2))
boxplot(Age~DeathAt30,data=data,ylab="DeathAt30",xlab="Age",horizontal=TRUE);
boxplot(Height~DeathAt30,data=data,ylab="DeathAt30",xlab="Height",horizontal=TRUE);
boxplot(BMI~DeathAt30,data=data,ylab="DeathAt30",xlab="BMI",horizontal=TRUE);

par(mfrow=c(2,2))
plot(final.model,which=c(1,3,4))



