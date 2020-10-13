#Load librariies
library(reshape)
library(ggplot2)
library(tibble)
library(ggpubr)
library(grid)
library(png)
library(mdthemes)
library(cowplot)

#Load the data
df.lab<-read.table("survival_parasitisation_lab.csv",header=T)
field.data <- read.table("field_notes.txt",header=T)


# HOST SURVIVAL
## Does the noni affect survival of mel or sim without parasitization?
unpar<-subset(df.lab,df.lab$treatment=="control")
anova(glm(cbind(survival,mortality)~species+medium,data=unpar,quasibinomial),test="F")
summary(glm(cbind(survival,mortality)~species+medium,data=unpar,quasibinomial),test="F")
# the medium does not affect survival of mel nor sim, but 
# sim does have higher survival rates


#Reorganized data frame to plot mean sd of survival  
#df2 <- data_summary(unpar, varname="survival", groupnames=c("species", "medium"))

# re-define sp. for proper scientific names 
variable_names <- list(
  "melanogaster" =  expression(paste(italic("D. melanogaster"))),
  "simulans" = expression(paste(italic("D. simulans")))
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}


# Plot survival
unpar.colors = c( "#A6CEE3","#1F78B4") 
unpar.plot<- ggplot(unpar, aes(x=medium, y=survival, fill=species)) +
  geom_boxplot() +
  ylim(0, 30) +
  theme(legend.position = "none", strip.text = element_text(size=12, face="italic")) +
  geom_signif(aes(annotations =c("NS"), xmin = c(1), xmax = c(2),y_position = c(28)), textsize=3, 
              data=unpar, manual =T) +
  scale_fill_manual(values=c("#56B4E9","#56B4E9"))+
  ggtitle("Survival of unparasitized host flies") +
  facet_grid(~ species, labeller=variable_labeller)+
  theme_bw()


# WASPS EMERGENCE
## Does the medium affect the emergence of wasps?
par<-subset(df.lab,df.lab$treatment=="wasp")
par.aov <- anova(glm(cbind(wasps, survival-wasps)~species+medium, data=par, quasibinomial), test="F")
summary(glm(cbind(wasps, survival-wasps)~species+medium, data=par, quasibinomial), test="F")
# The number of wasps that emerged (among the survivors) was significantly lower on noni than on yeast

#Plot wasp-fly survival
par.colors = c("#FFED6F","#56B4E9","#1A1A1A")  
emerged.df <- data.frame(emerged = c(rep("unparasitized fly",length(par$not_parasitized)),
                                     rep("resistant",length(par$capsule)),
                                     rep("wasps", length(par$wasps))),
                         host_sp =   rep(rep(c("D. melanogaster","D. simulans"),each=4), 2),
                         medium  =   rep(c("noni","yeast"),each=8),
                         value   = c(par$not_parasitized,par$capsule,par$wasps))


par.plot <- ggplot(emerged.df, aes(x=medium,y=value,fill=emerged )) +
  geom_boxplot() +
  #geom_bar(stat="identity", position="stack") +
  geom_signif(y_position = c(30), xmin = c(1),xmax = c(2), annotation = c("**"))+
  ylab("Number  of emerged insects") +
  #ylim(0, 95) +
  ggtitle("Success and rate of parasitization") +  
  scale_fill_manual("emerged type", values=par.colors, labels=expression("fly with capsule","unparasitized fly",paste("wasp", italic( "(A. citri)"))))+
  theme(strip.text = element_text(size=12, face="italic")) +
  facet_grid(~ host_sp, labeller=variable_labeller)+
  theme_bw()


#PARASITIZATION RATE
## Is parasitization affected by the medium?
anova(glm(cbind(not_parasitized, survival-not_parasitized)~species+medium, data=par, quasibinomial), test="F")
summary(glm(cbind(not_parasitized, survival-not_parasitized)~species+medium, data=par, quasibinomial), test="F")
## significantly more larvae escaped parasitism on noni medium than on yeast 


#Publication Plot
legend <- get_legend(par.plot)
par.plot <-par.plot + theme(legend.position="none")
unpar.plot <-unpar.plot + theme(legend.position="none")
fig1_grid <- plot_grid(unpar.plot, par.plot, legend, ncol=3, nrow=1, labels = c("A)","B)"), rel_widths = c(1, 1, 0.5))

png("fig1.png",fig1_grid,width=8.5,height=6,units="in",res=600)
fig1_grid
dev.off()


#FIELD DATA
#Reorganize data
field.data.melted<-melt(field.data[,c(1,4,5,6,7)],id.vars=c("days_open","sample"))
field.data.melted$noni <- rep((c(rep("unripe",3),rep("ripe",8),rep("overripe",8))),3)

#colors for plot
field.colors = c("greenyellow","yellow","pink","#A6CEE3","#1F78B4","#1A1A1A") #negro: "#1A1A1A" #naranja,amarillo: "#FFED6F", "#FF7F00"

#include scheme figures of noni stages
field.data.melted$variable <- factor(field.data.melted$variable, levels=c("wasps","dmalerkotliana","dsechellia"))
unripe_img <- png::readPNG("unripe_nonis.png")
unripe_pic <- rasterGrob(unripe_img, interpolate=TRUE)
ripe_img <- png::readPNG("ripe.png")
ripe_pic <- rasterGrob(ripe_img, interpolate=TRUE)
overripe_img <- png::readPNG("overripe.png")
overripe_pic <- rasterGrob(overripe_img, interpolate=TRUE)

# Plot
noni <-data.frame(x = factor(c(1:19)),  colour = factor(c(rep(1,3),rep(2,8),rep(3,8))))

field.plot <- ggplot(field.data.melted,aes(x=factor(sample),y=value,fill=factor(variable)))+
  geom_bar(stat = "identity",color="white") + 
  labs(title ="Emerged flies from each field-collected noni samples",
       x="noni sample",y="Number  of emerged insects") + 
  ylim(-10, 50) +
  scale_fill_manual("colour code",values=field.colors,labels=expression("unripe noni","ripe noni","overripe noni", italic("D. malerkotliana"), italic("D. sechellia"),italic("L. boulardi"))) + #,labels=cecal.grupo.lab
  #scale_x_continuous(name="sample", limits=c(1, 19))+ 
  theme( axis.text.x = element_blank(),axis.ticks = element_blank())+
  annotation_custom(unripe_pic, xmin=0, xmax=4.5, ymin=-10, ymax=-1) +
  annotation_custom(ripe_pic, xmin=5, xmax=11, ymin=-10, ymax=-2) +
  annotation_custom(overripe_pic, xmin=12.5, xmax=17.5, ymin=-10, ymax=-1) +
  geom_tile(data=noni, aes(x = x, y = -1, fill = colour)) 


