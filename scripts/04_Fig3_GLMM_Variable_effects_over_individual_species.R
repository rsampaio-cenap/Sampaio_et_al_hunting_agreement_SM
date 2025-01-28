### Load pacakages
library(here)
library(dplyr)
library(ggplot2)

# -------- Script to visualize the variable effects over individual species relative abundance --------
# -------- Load MSOM models --------
out_AB_int<-readRDS(here("data", "RN_multitaxa_AB_int.rds"))
out_R_int<-readRDS(here("data", "RN_multitaxa_r_int.rds"))
# -------- Load functions
source(file = here("data","graph.function.r"))

# -------- Load spp traits -----------
spp<- read.csv(file=here("data", "Tabela_1.csv"), header=T, dec=".", sep = ";", na.strings = "", encoding = "UTF-8")
spp$G<-ifelse(spp$Guild=="He.Fr",1,
              ifelse(spp$Guild=="Car",2,3))
# -------- Load data -----------
data<-read.csv(file =  here("data","sampaio.data.csv"), sep = ",", fileEncoding  = "UTF-8")
y<-data[2:22]
species<- colnames(y)


#------- ABUNDANCE GRAPHS ------
(DF<-co(out_AB_int,"a1|mu.a1",species,spp))
(g.AB.Dist.Int<-g(DF,"","","blue","black","red","(A)",-.5))

(DF<-co(out_AB_int,"a2|mu.a2",species,spp))
(g.AB.C.Int<-g(DF,"","","black","red","blue","(B)",-2.5))

(DF<-co(out_AB_int,"a3|mu.a3",species,spp))
(g.AB.Int<-g(DF,"","","black","blue","red","(C)",-1.8))


#------- DETECTION GRAPHS ------
(DF<-co(out_R_int,"r1|mu.r1",species,spp))
(g.R.Dist.Int<-g(DF,"","Bayesian Credible Intervals (± 95% BCIs)","blue","black","red","(D)",-.7))

(DF<-co(out_R_int,"r2|mu.r2",species,spp))
(g.R.C.Int<-g(DF,"","Bayesian Credible Intervals (± 95% BCIs)","black","red","blue","(E)",-3.2))

(DF<-co(out_R_int,"r3|mu.r3",species,spp))
(g.R.Int<-g(DF,"","Bayesian Credible Intervals (± 95% BCIs)","black","blue","red","(F)",-2.7))


#------- ALL GRAPHS --------
library(gridExtra)
library(grid)
(global.s<-gridExtra::grid.arrange(arrangeGrob(g.AB.Dist.Int, 
                                               top = textGrob("Community distance", vjust =0.5, hjust = 0.3, gp = gpar(fontface = "bold", cex = 1)),
                                               left = textGrob("Relative abundance",rot = 90, vjust =1.1, gp = gpar(fontface = "bold", cex = 1))),
                                   arrangeGrob(g.AB.C.Int,
                                               top = textGrob("Hunting strategy", vjust =0.5, hjust = 0.3, gp = gpar(fontface = "bold", cex = 1))),
                                   arrangeGrob(g.AB.Int,
                                               top = textGrob("Interaction", vjust =0.5, hjust = 0.1, gp = gpar(fontface = "bold", cex = 1))),
                                   arrangeGrob(g.R.Dist.Int, 
                                               top = textGrob("Community distance", vjust =0.5, hjust = 0.3, gp = gpar(fontface = "bold", cex = 1)),
                                               left = textGrob("Individual detection",rot = 90, vjust =1.1, gp = gpar(fontface = "bold", cex = 1))),
                                   arrangeGrob(g.R.C.Int,
                                               top = textGrob("Hunting strategy", vjust =0.5, hjust = 0.3,gp = gpar(fontface = "bold", cex = 1))),
                                   arrangeGrob(g.R.Int,
                                               top = textGrob("Interaction", vjust =0.5, hjust = 0.1,gp = gpar(fontface = "bold", cex = 1))),
                                   ncol = 3, nrow=2))
