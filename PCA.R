



# ---------------------------------------------
# Plot PCA
# 20 Feb 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



library(factoextra)
library(ggplot2)

FQ.frm=read.csv("data/PCAPhysico.csv",header=T, row.names=1)
attach(FQ.frm)
FQ.frm

Urbanpools <- prcomp(FQ.frm, scale = TRUE, center=T)
summary(Urbanpools)

res.var <- get_pca_var(Urbanpools)
res.var$coord          # Coordinates

res.ind <- get_pca_ind(Urbanpools)
res.ind$coord          # Coordinates

fviz_pca_biplot(Urbanpools,
                repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# Plot Function  ----------------------------------------------------------

physico.names <- c("TempAgua" = "Temperature", "O2" = "pH", 
                "pH" = "Dis.Oxyg.","Conduct"="Conductivity") # New names physicochemical var.

PCbiplot <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x)
# Site names
  plot <- ggplot(data, aes_string(x=x, y=y)) + 
                 geom_text(aes(label=obsnames), size=3.5, color='gray20',
                           vjust = -1.5) +
    geom_point(aes(colour = obsnames),size=5) +
    scale_color_manual(values=c("#4575b4", "#4575b4", #Catie
                                "#313695","#313695", # DA
                                "#d73027","#d73027", #ER
                                "#003c30","#003c30", # LAN
                                "#9970ab","#9970ab", # LP
                                "#7fbc41","#7fbc41" # LS
                                )) +
    
    labs(x= "PC1 (40.1%)", y = "PC2 (28.9%)") # Modifica con tus datos
# Intercepts  
  plot <- plot + geom_hline(yintercept=0, size=.2,linetype="dashed") + 
                 geom_vline(xintercept=0, size=.2,linetype="dashed")
# Loading table  
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min((max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
              (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x]))))
  datapc <- transform(datapc,v1 = .7 * mult * (get(x)),
                             v2 = .7 * mult * (get(y)))
# Coordinates & loading names 
  plot <- plot + coord_equal() + ylim(-2,2) + xlim(-2.3,2) +
    geom_text(data=datapc, aes(x=v1, y=v2, label=physico.names), #varnames
              size = 6, vjust=-0.5, color="black")
# Arrows  
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                              arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="black")
  plot <- plot + theme_bw() +
    theme(plot.margin = margin(1.2,1.2,1.2,1.2, "cm"))+ 
    theme(legend.position = "none") +
    theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
    theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y
    theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
    theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black"))  #subaxis y
  
  plot
}

Fig <- PCbiplot(Urbanpools)
Fig

ggsave("Figure 1.jpeg",Fig, width = 200, height = 220, units = "mm")


References

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# https://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
