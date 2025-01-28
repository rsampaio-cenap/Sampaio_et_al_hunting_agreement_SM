co<- function (out,x,species,spp) { 
  DF<-as.data.frame(out$BUGSoutput$summary)
  DF<-DF[c(1,3,7)]
  DF.1<-DF|>
    filter(grepl(paste0(x), row.names(DF)))
  DF<-DF.1[c(1:21,32),]
  colnames(DF) <- c("mean", "lowerCI", "upperCI")
  DF$sp<-c(species,"All.sp")
  #(DF <- round(DF[order(DF$mean, decreasing=TRUE),][1:3],2)) # reorder based on species names
  ### Plot for visual checking 
  DF$cor<-ifelse(DF$lowerCI < 0 &          DF$upperCI < 0, "vermelho",
                 ifelse(DF$lowerCI > 0 & DF$upperCI > 0, "azul", "preto"))
  ### Inserting spp data
  DF.1<-merge(DF, spp, by  = "sp")
  DF.1<-DF.1 |> arrange(desc(Order), Category, desc(Kg1))
  return(DF.1)}


g<- function (DF,eixo_y,eixo_x,cor1,cor2,cor3,texto,x){
  beta<-ggplot2::ggplot(DF, aes(y=mean, x=reorder(reorder(sp, Kg1), 
                                                  #-as.numeric(as.factor(Category))), 
                                                  as.numeric(as.factor(Order))), ymin=lowerCI, ymax=upperCI, colour = cor)) +  
    geom_point(size=1.5)+
    geom_linerange(size=0.5)+
    coord_flip() +
    xlab(eixo_y) + 
    ylab(eixo_x)+
    #ylab("Regression coefficients (± 95% CI)") +
    geom_hline(yintercept=0, linetype = "dashed", col="darkgray")+
    scale_color_manual(values = c(cor1,cor2,cor3))+
    theme (panel.background = element_rect(fill = "transparent", colour = NA),
           axis.line = element_line(colour = "black"),
           axis.text.x = element_text(size=10),
           axis.text.y = element_text(size=10),
           axis.title = element_text(size=10, face = "bold"),
           plot.margin=unit(c(0,0.25,0,0),"cm"),
           plot.title = element_text(face = "bold", hjust = 0.5, size = 10))+
    guides(color = "none")+
    #ggtitle(y)+
    annotate("text", y=x, x=22, label = texto, size = 4.5)
  return (beta) }