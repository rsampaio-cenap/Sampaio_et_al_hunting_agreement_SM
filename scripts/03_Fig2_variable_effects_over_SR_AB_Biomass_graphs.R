### Load pacakages
library(here)
library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)

# -------- Script to test the variable effects over estimated species richness, aggrageted abundance and biomass --------
# -------- Load MSOM models --------
out_AB_int<-readRDS(here("data", "RN_multitaxa_AB_int.rds"))

# -------- Load data -----------
data<-read.csv(file =  here("data","sampaio.data.csv"), sep = ",", fileEncoding  = "UTF-8")

# -------- Creating an object with aggregated abundance, richness, biomass and site characteristics -------- 
AB<-as.data.frame(out_AB_int$BUGSoutput$sims.list$AB)
colnames(AB)<-row.names(data)
AB<-AB |>pivot_longer(cols = c(colnames(AB)),
                      names_to = "Sites",
                      values_to = "AB") |>
  as.data.frame()

SR<-as.data.frame(out_AB_int$BUGSoutput$sims.list$SR)
colnames(SR)<-row.names(data)
SR<-SR |>pivot_longer(cols = c(colnames(SR)),
                      names_to = "Sites",
                      values_to = "SR") |>
  as.data.frame()

BI<-as.data.frame(out_AB_int$BUGSoutput$sims.list$BI)
colnames(BI)<-row.names(data)
BI<-BI |>pivot_longer(cols = c(colnames(BI)),
                      names_to = "Sites",
                      values_to = "BI") |>
  as.data.frame()

derived<-as.data.frame(c(AB,SR[2],BI[2]))
derived$Com.dist<-rep(data$Com.D,1500)
derived$Hunting<-rep(data$margem,1500)

# -------- Save derived object -----------
write.csv(derived, here("data", "derived.csv"), fileEncoding  = "UTF-8")

# -------- Bayesian GLMM -----------
## -------- Species Richness --------
m.normal.SR  <- brm(SR ~ Com.dist + Hunting + (1|Sites), 
                    data=derived,
                    chains = 3, # nb of chains
                    iter = 100000, # nb of iterations, including burnin
                    warmup = 50000, # burnin
                    thin = 100)
### -------- Save results --------  
saveRDS(m.normal.SR, here("data", "m.normal.SR.rds"))

### -------- Load model --------
m.normal.SR<-readRDS(here("data", "m.normal.SR.rds"))

### -------- Coefs --------
summary(m.normal.SR)$fixed

### -------- see convergence --------
plot(m.normal.SR)


## -------- Abundance --------
m.normal.l.AB <-  brm(log(AB) ~ Com.dist + Hunting + (1|Sites), 
                      data=derived,
                      chains = 3, # nb of chains
                      iter = 100000, # nb of iterations, including burnin
                      warmup = 50000, # burnin
                      thin = 100)
## -------- Save results -------- 
saveRDS(m.normal.l.AB, here("data", "m.normal.l.AB.rds"))

## -------- Coefs --------
summary(m.normal.l.AB)$fixed

## -------- see convergence --------
plot(m.normal.l.AB)


## -------- Biomass --------
m.normal.l.BI <-  brm(log(BI) ~ Com.dist + Hunting + (1|Sites), 
                      data=derived,
                      chains = 3, # nb of chains
                      iter = 100000, # nb of iterations, including burnin
                      warmup = 50000, # burnin
                      thin = 100)

## -------- Save results --------
saveRDS(m.normal.l.BI, here("data", "m.normal.l.BI.rds"))

## -------- Coefs --------
summary(m.normal.l.BI)$fixed

## -------- see convergence --------
plot(m.normal.SR)


# -------- Script to visualize the variable effects over species richness, aggregated abundance and biomass --------
# -------- Load models --------
m.normal.SR<-readRDS(here("data", "m.normal.SR.rds"))
m.normal.l.AB<-readRDS(here("data", "m.normal.l.AB.rds"))
m.normal.l.BI<-readRDS(here("data", "m.normal.l.BI.rds"))

# -------- Load data --------
derived<-read.csv(file =  here("data","derived.csv"), sep = ",", fileEncoding  = "UTF-8")
names(derived)
## -------- Species richness --------
### -------- Checking coefs--------
summary(m.normal.SR)$fixed
#### -------- Graph --------
(g.SR_com <- ggplot(derived, aes(x = Com.dist, y = SR)) +
   geom_jitter( colour = "darkgrey")+
   ylab("Species richness")+
   xlab("Community distance (scaled)")+
   geom_smooth(method='glm', se=T,colour="black")+
   theme(panel.background = element_rect(fill = "transparent", colour = NA),
         axis.line = element_line(colour = "black"),
         axis.text = element_text(size=12),
         axis.title = element_text(size=13, face="bold"))+
   annotate("text", x=-1.5, y=28, label = "(A)", size = 5)+
   annotate("text", x=-0.8, y=28, label = "B = 2.6 ± 1.2; Rhat = 1.01", size = 4, fontface = "bold"))

(g.SR_hunt <- ggplot(derived, aes(x = Hunting, y = SR)) +
    ylab("Species richness")+
    xlab("Hunting strategy")+
    geom_jitter(width=0.01, colour = "darkgrey")+
    geom_violin(fill="transparent")+
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size=12),
          axis.title = element_text(size=13, face="bold"))+
    annotate("text", x=.6, y=28, label = "(D)", size = 5)+
    annotate("text", x=1.2, y=28, label = "B = 1.6 ± 1.5; Rhat = 1.01", size = 4, fontface = "bold"))

## -------- Relative abundance --------
### -------- Checking coefs--------
summary(m.normal.l.AB)$fixed
#### -------- Graph --------
(g.AB_com <- ggplot(derived, aes(x = Com.dist, y = log(AB))) +
   geom_jitter(colour = "darkgrey")+
   ylab("Aggregated relative abundance (log)")+
   xlab("Community distance (scaled)")+
   geom_smooth(method='lm', colour="black")+
   theme(panel.background = element_rect(fill = "transparent", colour = NA),
         axis.line = element_line(colour = "black"),
         axis.text = element_text(size=12),
         axis.title = element_text(size=13, face="bold"))+
   annotate("text", x=-1.5, y=5, label = "(B)", size = 5)+
   annotate("text", x=-0.8, y=5, label = "B = 0.4 ± 0.2; Rhat = 1.01", size = 4, fontface = "bold"))

(g.AB_hunt <- ggplot(derived, aes(x = Hunting, y = log(AB))) +
    ylab("Aggregated relative abundance (log)")+
    xlab("Hunting strategy")+
    geom_jitter(width=0.01,colour = "darkgrey")+
    geom_violin(fill="transparent")+
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size=12),
          axis.title = element_text(size=13, face="bold"))+
    annotate("text", x=.6, y=5, label = "(E)", size = 5)+
    annotate("text", x= 1.2, y=5, label = "B = 0.3 ± 0.2; Rhat = 1.01", size = 4, fontface = "bold"))

## -------- Biomass --------
### -------- Checking coefs--------
summary(m.normal.l.BI)$fixed
#### -------- Graph --------
(g.BI_com <- ggplot(derived, aes(x = Com.dist, y = log(BI))) +
   geom_jitter(colour = "darkgrey")+
   ylab("Aggregated biomass (log)")+
   xlab("Community distance (scaled)")+
   geom_smooth(method='lm', colour="black")+
   theme(panel.background = element_rect(fill = "transparent", colour = NA),
         axis.line = element_line(colour = "black"),
         axis.text = element_text(size=12),
         axis.title = element_text(size=13, face="bold"))+
   annotate("text", x=-1.5, y=9, label = "(C)", size = 5)+
   annotate("text", x=-0.8, y=9, label = "B = 0.7 ± 0.2; Rhat = 1.01", size = 4, fontface = "bold"))

(g.BI_hunt <- ggplot(derived, aes(x = Hunting, y = log(BI))) +
    ylab("Aggregated biomass (log)")+
    xlab("Hunting strategy")+
    geom_jitter(width = 0.01,colour = "darkgrey")+
    geom_violin(fill="transparent")+
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size=12),
          axis.title = element_text(size=13, face="bold"))+
    annotate("text", x=1.5, y=9, label = "(F)", size = 5)+
    annotate("text", x= 2.1, y=9, label = "B = 0.5 ± 0.3; Rhat = 1.01", size = 4, fontface = "bold"))

# -------- All graphs --------
library(gridExtra)
library(grid)
(global<-gridExtra::grid.arrange(g.SR_com, g.AB_com, g.BI_com,
                                 g.SR_hunt,g.AB_hunt, g.BI_hunt,
                                 ncol = 3,nrow=2,
                                 bottom = textGrob("", vjust =-1, gp = gpar(fontface = "bold", cex = 1)),
                                 left = textGrob("", rot = 90, hjust =0, vjust=1.5, gp = gpar(fontface = "bold", cex = 1))))
