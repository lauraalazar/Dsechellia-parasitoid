library(reshape)
library(ggplot2)

df.lab<-read.table("data/survival_parasitisation_lab.csv",header=T)

# HOST SURVIVAL
## Does the noni affect survival of mel or sim without parasitization?
unpar<-subset(df.lab,df.lab$treatment=="control")
anova(glm(cbind(survival,mortality)~species+medium,data=unpar,quasibinomial),test="F")
summary(glm(cbind(survival,mortality)~species+medium,data=unpar,quasibinomial),test="F")
# the medium does not affect survival of mel nor sim, but 
# sim does have higher survival rates

# WASPS EMERGENCE
## Does the medium affect the emergence of wasps?
par<-subset(df.lab,df.lab$treatment=="wasp")
anova(glm(cbind(wasps, survival-wasps)~species+medium, data=par, quasibinomial), test="F")
summary(glm(cbind(wasps, survival-wasps)~species+medium, data=par, quasibinomial), test="F")
# The number of wasps that emerged (among the survivors) was significantly lower on noni than on yeast

#PARASITIZATION RATE
## Is parasitization affected by the medium?
anova(glm(cbind(not_parasitized, survival-not_parasitized)~species+medium, data=par, quasibinomial), test="F")
summary(glm(cbind(not_parasitized, survival-not_parasitized)~species+medium, data=par, quasibinomial), test="F")
## significantly more larvae escaped parasitism on noni medium than on yeast 


#FIELD DATA
field.dat <- read.table(text="
noni    D.sechellia  D.malerkotliana Wasp
early 27 73 0
ripe 46 0 0
rotting 3 30 9", header=T,as.is=T)

mdat<-melt(dat,id.vars="noni")

##Bars
ggplot(mdat, aes(noni,value,fill=variable)) +
  geom_bar(stat="identity", position="dodge")

##Pie Charts
ggplot(mdat, aes(noni,value,fill=variable)) + coord_polar("y", start=0)


