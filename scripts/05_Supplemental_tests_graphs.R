### Load pacakages
library(here)
library(R2jags)
library(dplyr)
library (ggplot2)

# -------- Load datas -----------
data<-read.csv(file =  here("data","sampaio.data.csv"), sep = ",", fileEncoding  = "UTF-8")
head(data)
hd<-read.csv(file =  here("data","human_density.csv"), sep = ";", fileEncoding  = "UTF-8")
hd<-hd[order(hd$camera_id),]

# -------- add human density 1km scales in data.frame -----------
data$hd<-scale(hd$Housholds)

## -------- Testing hidrography effects over river banks -----------
shapiro.test(data[data$river.bank=="Dogs",]$Hyd)
shapiro.test(data[data$river.bank=="No dogs",]$Hyd)
wilcox.test(Hyd ~ river.bank, data = data,
            exact = FALSE)
### -------- Graph -----------
(g.Hyd<-ggplot(data, aes(x = river.bank, y = Hyd)) +
    geom_violin(fill = "grey",alpha=0.5)+
    geom_jitter(width =0.1, size=1.5, color = "black")+
    xlab("")+
    ylab("Hydrography")+
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size=11),
          axis.title = element_text(size=12, face="bold")))

## -------- Testing deforestation effects over river banks -----------
shapiro.test(data[data$river.bank=="Dogs",]$Def)
shapiro.test(data[data$river.bank=="No dogs",]$Def)
wilcox.test(Def ~ river.bank, data = data, exact = F)
### -------- Graph -----------
(g.Def<-ggplot(data, aes(x = river.bank, y = Def)) +
    geom_violin(fill = "grey",alpha=0.5)+
    geom_jitter(width =0.1, size=1.5, color = "black")+
    xlab("")+
    ylab("Deforestation")+
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size=11),
          axis.title = element_text(size=12, face="bold")))

## -------- Testing flooding effects over river banks -----------
shapiro.test(data[data$river.bank=="Dogs",]$Flo)
shapiro.test(data[data$river.bank=="No dogs",]$Flo)
wilcox.test(Flo ~ river.bank, data = data, exact=F)
### -------- Graph -----------
(g.Flo<-ggplot(data, aes(x = river.bank, y = Flo)) +
    geom_violin(fill = "grey",alpha=0.5)+
    geom_jitter(width =0.1, size=1.5, color = "black")+
    xlab("")+
    ylab("Flooded area")+
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size=11),
          axis.title = element_text(size=12, face="bold")))

## -------- Testing VDND effects over river banks -----------
shapiro.test(data[data$river.bank=="Dogs",]$VDND)
shapiro.test(data[data$river.bank=="No dogs",]$VDND)
wilcox.test(VDND ~ river.bank, data = data, exact=F)
### -------- Graph -----------
(g.VDND<-ggplot(data, aes(x = river.bank, y = VDND)) +
    geom_violin(fill = "grey",alpha=0.5)+
    geom_jitter(width =0.1, size=1.5, color = "black")+
    xlab("")+
    ylab("VDND")+
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size=11),
          axis.title = element_text(size=12, face="bold"))+
    annotate("text", x= 1, y=.6, label = "W=11; p<0.01", size = 3, fontface = "bold"))

## -------- Testing human density effects over river banks -----------
shapiro.test(data[data$river.bank=="Dogs",]$hd)
shapiro.test(data[data$river.bank=="No dogs",]$hd)
wilcox.test(hd ~ river.bank, data = data, exact=F)
### -------- Graph -----------
(g.HH<-ggplot(data, aes(x = river.bank, y = hd)) +
    geom_violin(fill = "grey",alpha=0.5)+
    geom_jitter(width =0.1, size=1.5, color = "black")+
    xlab("")+
    ylab("Human density")+
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size=11),
          axis.title = element_text(size=12, face="bold")))

# -------- All graphs -----------
library(grid)
gridExtra::grid.arrange(g.Hyd,
                        g.Def, g.Flo,
                        g.VDND, g.HH,
                        ncol = 4, nrow=4,
                        bottom = textGrob("", vjust =-1, gp = gpar(fontface = "bold", cex = 1)),
                        left = textGrob("", rot = 90, hjust =0, vjust=1.5, gp = gpar(fontface = "bold", cex = 1)),
                        layout_matrix = rbind(c(NA,1,1,NA),
                                              c(NA,1,1,NA),
                                              c(2,2,3,3),
                                              c(2,2,3,3),
                                              c(4,4,5,5),
                                              c(4,4,5,5)))
