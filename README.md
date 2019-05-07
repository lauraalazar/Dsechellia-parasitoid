# _D. sechellia_-parasitoid
Collection of data showing evidence of D.sechellia loss of immune response against parasitoid wasps

# Genomic Data
Extracted from Salazar-Jaramillo et al., 2017 Evolution of a cellular immune response in Drosophila: a phenotypic and
249 genomic comparative analysis. Genome Biology and Evolution 6:273–289

# Transcriptomic Data
Extracted from Salazar-Jaramillo et al., 2017. Inter- and intra-species variation in genome-wide gene expression of _Drosophila_ in response to 246 parasitoid wasp attack. BMC Genomics 18:331. Repository https://github.com/lauraalazar/RNAseq-Drosophila-parasitoids

# Field Data

The number of adult drosophilidaes and wasps that emerged from noni fruit was counted. The stage of the fruit was divided into three categories: early, ripe and rotting. The following script creates a table with the found numbers and describes the fraction of insect in each fruit stage

```
dat <- read.table(text="
noni    D.sechellia  D.malerkotliana Wasp
early 27 73 0
ripe 46 0 0
rotting 3 30 9", header=T,as.is=T)

library(reshape)
mdat<-melt(dat,id.vars="noni")

library(ggplot2)
##Bars
ggplot(mdat, aes(noni,value,fill=variable)) +
geom_bar(stat="identity", position="dodge")

##Pie Charts
ggplot(mdat, aes(noni,value,fill=variable)) + coord_polar("y", start=0)
```
