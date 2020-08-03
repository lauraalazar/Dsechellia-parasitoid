library(reshape)
library(ggplot2)
library(tibble)
library(ggpubr)
library(grid)
library(png)  
library(mdthemes)

df.lab<-read.table("survival_parasitisation_lab.csv",header=T)


unpar.colors = c( "#A6CEE3","#1F78B4") 

# HOST SURVIVAL
## Does the noni affect survival of mel or sim without parasitization?
unpar<-subset(df.lab,df.lab$treatment=="control")
anova(glm(cbind(survival,mortality)~species+medium,data=unpar,quasibinomial),test="F")
summary(glm(cbind(survival,mortality)~species+medium,data=unpar,quasibinomial),test="F")
# the medium does not affect survival of mel nor sim, but 
# sim does have higher survival rates


data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(unpar, varname="survival", groupnames=c("species", "medium"))


variable_names <- list(
  "melanogaster" =  "D. melanogaster",
  "simulans" = "D. simulans"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}


unpar.plot<- ggplot(df2, aes(x=medium, y=survival, fill=species)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  ylim(0, 30) +
  geom_errorbar(aes(ymin=survival-sd, ymax=survival+sd), width=.2,
                position=position_dodge(.9)) +
  theme(legend.position = "none", strip.text = element_text(size=12, face="italic")) +
  geom_signif(aes(annotations =c("NS"), textsize=c(3),xmin = c(1), xmax = c(2),y_position = c(28)), 
              data=df2, manual =T) +
  scale_fill_manual(values=c("#56B4E9","#56B4E9"))+
  ggtitle("Survival of unparasitized host flies") +
  facet_grid(~ species, labeller=variable_labeller)


# WASPS EMERGENCE
## Does the medium affect the emergence of wasps?
par<-subset(df.lab,df.lab$treatment=="wasp")
par.aov <- anova(glm(cbind(wasps, survival-wasps)~species+medium, data=par, quasibinomial), test="F")
summary(glm(cbind(wasps, survival-wasps)~species+medium, data=par, quasibinomial), test="F")
# The number of wasps that emerged (among the survivors) was significantly lower on noni than on yeast

#Plot wasp-fly survival
par.colors = c("#FFED6F","#56B4E9","#1A1A1A")  #c("#BC80BD","#FFED6F","#666666") gray: #999999"
emerged.df <- data.frame(emerged = c(rep("unparasitized",length(par$not_parasitized)),
                                     rep("resistant",length(par$capsule)),
                                     rep("wasps", length(par$wasps))),
                         host_sp =   rep(rep(c("D. melanogaster","D. simulans"),each=4), 2),
                         medium  =   rep(c("noni","yeast"),each=8),
                         value   = c(par$not_parasitized,par$capsule,par$wasps))
variable_names <- list(
  "melanogaster" = "D. melanogaster" ,
  "simulans" = "D. simulans"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

par.plot <- ggplot(emerged.df, aes(x=medium,y=value,fill=emerged )) +
  geom_bar(stat="identity", position="stack") +
  geom_signif(y_position = c(92), xmin = c(1),xmax = c(2), annotation = c("**"))+
  ylab("Number  of emerged insects") +
  ylim(0, 95) +
  ggtitle("Parazitization and parasitization success ") +  
  scale_fill_manual("emerged type", values=par.colors, labels=expression("fly with capsule","unparasitized",paste("wasp", italic( "(A. citri)"))))+
  theme(strip.text = element_text(size=12, face="italic")) +
  facet_grid(~ host_sp, labeller=variable_labeller)

par.plot
#PARASITIZATION RATE
## Is parasitization affected by the medium?
anova(glm(cbind(not_parasitized, survival-not_parasitized)~species+medium, data=par, quasibinomial), test="F")
summary(glm(cbind(not_parasitized, survival-not_parasitized)~species+medium, data=par, quasibinomial), test="F")
## significantly more larvae escaped parasitism on noni medium than on yeast 

pdf("/lsalazar/publicaciones/dsechellia/peerJ/PeerJ_revision/figures/infection_rate.pdf",width=5,height=5)
par.plot
dev.off()


pdf("/lsalazar/publicaciones/dsechellia/peerJ/PeerJ_revision/figures/survival_hosts.pdf", width=4, height=4)
p
dev.off()


pdf("/lsalazar/publicaciones/dsechellia/peerJ/PeerJ_revision/figures/lab_exp.pdf")
grid.arrange(unpar.plot, par.plot, ncol=2, top=textGrob("Effect of noni on host flies and wasps", gp=gpar(fontsize=15)))
dev.off()

#FIELD DATA

field.data <- read.table("field_notes.txt",header=T)

field.data.melted<-melt(field.data[,c(1,4,5,6,7)],id.vars=c("days_open","sample"))
field.data.melted$noni <- rep((c(rep("unripe",3),rep("ripe",8),rep("overripe",8))),3)

field.colors = c("greenyellow","yellow","pink","#A6CEE3","#1F78B4","#1A1A1A") #negro: "#1A1A1A" #naranja,amarillo: "#FFED6F", "#FF7F00"


#define the order
field.data.melted$variable <- factor(field.data.melted$variable, levels=c("wasps","dmalerkotliana","dsechellia"))
unripe_img <- png::readPNG("figures/unripe_nonis.png")
unripe_pic <- rasterGrob(unripe_img, interpolate=TRUE)
ripe_img <- png::readPNG("figures/ripe_noni.png")
ripe_pic <- rasterGrob(ripe_img, interpolate=TRUE)
overripe_img <- png::readPNG("figures/overripe.png")
overripe_pic <- rasterGrob(overripe_img, interpolate=TRUE)


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


