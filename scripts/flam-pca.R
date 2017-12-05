#flam_PCA.R
##Analyze flammability dimension

#dataset
source("./final_summary_dataset.R")

#apply pca to selected flammability measurements
flamabove.PCA <- temp.above %>%
  select ( dur, lossrate, massloss,
           max.fh) %>% pca(nPcs=3, method="ppca",
                           center=TRUE,scale="uv")
summary(flamabove.PCA)

#extract pca loading and score for biplot
flamabove.loads <- as.data.frame(loadings(flamabove.PCA))
flamabove.scores <- as.data.frame(scores(flamabove.PCA))

#assign grouping label (species) for pca scores and variable names for loading 
#loading names split into two sets because pisition of text 
flamabove.scores$sp.cd <- temp.above$sp.cd
varnames1 <- c("mass loss rate", "mass loss","max. flame height")
varnames2 <- c("duration of heating")

#adjust relative length of loading segments on biplot 
mult <- min(
  (max(flamabove.scores$PC2) - min(flamabove.scores$PC2)/(max(flamabove.loads$PC2)-min(flamabove.loads$PC2))),
  (max(flamabove.scores$PC1) - min(flamabove.scores$PC1)/(max(flamabove.loads$PC1)-min(flamabove.loads$PC1)))
)

flamabove.loads <- transform(flamabove.loads,
                             v1 = 0.6 * mult * PC1,
                             v2 = 0.6 * mult * PC2
)

#final biplot 

flamabove.loads1 <- flamabove.loads[c(2:4),]
flamabove.loads2 <- flamabove.loads[1,]


ggplot()+
  geom_point(data = flamabove.scores, aes(x=PC1, y=PC2, color=sp.cd, alpha=0.001)) +
  scale_color_manual(values=color)+
  labs(x="Principle component 1 (58.78%)", y="Principle component 2 (21.32%)") + 
  geom_segment(data = flamabove.loads, aes(x=0,y=0,xend=v1,yend=v2),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color = "black")+
  geom_text(data = flamabove.loads1, aes(x=v1, y=v2, label=varnames1),
            size = 5, vjust=0, color="black") + pubtheme.nogridlines +
  theme(legend.position = "none", axis.text=element_text(size=18),
        axis.title=element_text(size=20)) + 
  geom_text(data = flamabove.loads2, aes(x=v1, y=v2-0.4, label=varnames2),
            size = 5, vjust=0, color="black")
#clean up env
rm("flamabove.loads")
