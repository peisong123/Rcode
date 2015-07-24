install.packages('ggplot2')
library(ggplot2)
str(diamonds)
head(diamonds)

g=ggplot(diamonds,aes(cut))
g+geom_histogram()
g+geom_bar(fill=c("red","yellow","blue","green","pink"))## color=c(......)

g1=ggplot(diamonds,aes(y=carat,x=color))
g1+geom_boxplot()
g1+geom_violin()

g=ggplot(diamonds,aes(x=clarity,fill=cut))
g+geom_histogram()


## +geom_smooth()

data()
USArrests
head(USArrests)

ggplot(USArrests,aes(x=UrbanPop,y=Murder))+geom_point(size=2,shape=3,color="red",alpha=.8)+
  theme_classic()+
  labs(x="% Urban Population", y="Murder arrests per 1000000 people", 
       title="Title")+
       scale_x_continuous(limits=c(1,100))


