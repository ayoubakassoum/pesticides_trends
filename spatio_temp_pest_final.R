
################################################################
########   Spatio-temporal dynamics of pesticide use    ########
############       January 13 2023                ##############
########           Revised July 2023                   #########
################################################################

# packages
library(sf)
library(rnaturalearth)
library(dplyr)
library(ggplot2)
library(tidyr)
library(geosphere)
library(reshape2)
library(estdaR)
library(RColorBrewer)
library(magrittr)
library(xtable)
library(ggthemes)
library(gridExtra)
library(moments)


###############################################
####     data for pesticide use per ha     ####
###############################################

## pesticide use

  Africa  <- read.csv("Inputs_Pesticides_Use_E_Africa.csv")
  America <- read.csv("Inputs_Pesticides_Use_E_Americas.csv")
  Europe  <- read.csv("Inputs_Pesticides_Use_E_Europe.csv")
  Asia    <- read.csv("Inputs_Pesticides_Use_E_Asia.csv")
  Oceania <- read.csv("Inputs_Pesticides_Use_E_Oceania.csv")

# select only the total pesticide use
  full   <- rbind(Africa, America, Asia, Oceania, Europe)
  full_p <- full[full$Item == "Pesticides (total)",]
  full_a <- full_p[, c(3, seq(9, ncol(full_p)-1, 2))]

# discard URSS (from 1992, it disappears) + remove 1992 (many missing values)
  full_a <- full_a[, !(names(full_a) %in% c("Y1990", "Y1991", "Y1992"))] 
  full_a <- full_a[order(full_a$Area),]

# consider only countries with observations for all years
  full_b <- na.omit(full_a)

# extrapolations
  full_b[full_b$Area == "Antigua and Barbuda",]$Y2015 <- mean(c(full_b[full_b$Area == "Antigua and Barbuda",]$Y2014, full_b[full_b$Area == "Antigua and Barbuda",]$Y2016))

  full_b[full_b$Area == "Guinea",]$Y2009 <- mean(c(full_b[full_b$Area == "Guinea",]$Y2008, full_b[full_b$Area == "Guinea",]$Y2010))

  full_b[full_b$Area == "Brunei Darussalam",]$Y2013 <- mean(c(full_b[full_b$Area == "Brunei Darussalam",]$Y2012, full_b[full_b$Area == "Brunei Darussalam",]$Y2014))

  full_b[full_b$Area == "Congo",]$Y2009 <- mean(c(full_b[full_b$Area == "Congo",]$Y2008, full_b[full_b$Area == "Congo",]$Y2011))

  full_b[full_b$Area == "Congo",]$Y2010 <- mean(c(full_b[full_b$Area == "Congo",]$Y2008, full_b[full_b$Area == "Congo",]$Y2011))

# discard some countries because of inconsistent/partial data
  discard <- c("Mauritania", "United Republic of Tanzania", "Paraguay")
  full_c <- full_b[!(full_b$Area %in% discard),]

# discard some countries with non constant reporting scheme
  discard_n <- c("Bulgaria", "Cameroon", "Colombia", "Costa Rica", "Cyprus", "Ecuador", "Estonia", "Iceland", "Latvia", "Mauritius", "Spain")
  full_c <- full_c[!(full_c$Area %in% discard_n),]

# extrapolations again
  full_c[full_c$Area == "Armenia",]$Y2018 <- mean(c(full_c[full_c$Area == "Armenia",]$Y2017, full_c[full_c$Area == "Armenia",]$Y2019))

  full_c[full_c$Area == "Tunisia",]$Y2009 <- mean(c(full_c[full_c$Area == "Tunisia",]$Y2008, full_c[full_c$Area == "Tunisia",]$Y2010))

  full_c[full_c$Area == "Portugal",]$Y2018 <- full_c[full_c$Area == "Portugal",]$Y2017
  full_c[full_c$Area == "Portugal",]$Y2019 <- full_c[full_c$Area == "Portugal",]$Y2017
  full_c[full_c$Area == "Portugal",]$Y2020 <- full_c[full_c$Area == "Portugal",]$Y2017

# some other manipulations
  full_c$Area[full_c$Area == "C\xf4te d'Ivoire"] <- "Côte d'Ivoire"
  full_c$Area[full_c$Area == "T\xfcrkiye"] <- "Turkey"

  full_c <- full_c[order(full_c$Area),]

  rownames(full_c) <- full_c$Area
  full_c <- full_c[, -1]
  colnames(full_c) <- 1993:2020




## crop land uses

  Africa_land  <- read.csv("Inputs_LandUse_E_Africa.csv")
  America_land <- read.csv("Inputs_LandUse_E_Americas.csv")
  Europe_land  <- read.csv("Inputs_LandUse_E_Europe.csv")
  Asia_land    <- read.csv("Inputs_LandUse_E_Asia.csv")
  Oceania_land <- read.csv("Inputs_LandUse_E_Oceania.csv")

# select only cropland and years 1993:2020 + order
  full_land     <- rbind(Africa_land, America_land, Asia_land, Oceania_land, Europe_land)
  full_cropland <- full_land[full_land$Item == "Cropland",]
  cropland <- full_cropland[, c(3, seq(73, ncol(full_cropland)-1, 2))]

# fix encoding problems + order
  cropland$Area[cropland$Area == "C\xf4te d'Ivoire"] <- "Côte d'Ivoire"
  cropland$Area[cropland$Area == "T\xfcrkiye"] <- "Turkey"
  cropland <- cropland[order(cropland$Area),]

# match pesticide data
  cropland_match <- cropland[which(cropland$Area %in% rownames(full_c)),]

  t4 <- cropland_match[match(rownames(full_c),cropland_match$Area),1:29] 
  t4[which(!(rownames(full_c) %in% t4$Area)),"Area"] <- rownames(full_c)[which(!(rownames(full_c) %in% t4$Area))] 

  rownames(t4) <- t4[,1]
  t4[,1]<-NULL
  colnames(t4) <- 1993:2020


# fix for France
france_extra <- cropland[cropland$Area %in% c("R\xe9union", "Guadeloupe", "Martinique", "French Guyana"),]
france_extra[, 2:19] <- 0
t4[rownames(t4)=="France",] <- t4[rownames(t4)=="France",] + apply(france_extra[,-1], 2, sum)


# fix for slovakia
t4[rownames(t4)=="Slovakia",] <- 0.7*t4[rownames(t4)=="Slovakia",]


# Pesticides per crop land use ha
t5 <- full_c/t4
t5$name_long <- rownames(full_c)
t5 <- na.omit(t5)   #remove  China- Macao as it present NA as cropland value


## make a map of pesticide use by hectares
world <- ne_countries(scale = "medium", returnclass = "sp")
area_km2 <- 1/1000000*geosphere::areaPolygon(world)
world <- st_as_sf(world)
world <- world[,c("name_long","continent","economy")]
world["area_km2"] <- area_km2
world[50,"name_long"] <- "Cabo Verde"
world[82,"name_long"] <- "Gambia"
world[223,"name_long"] <- "United Republic of Tanzania"
world[32,"name_long"] <- "Bolivia (Plurinational State of)" 
world[91,"name_long"] <- "China, Hong Kong SAR"
world[133,"name_long"] <- "China, Macao SAR"
world[222,"name_long"] <- "China, Taiwan Province of" 
world[177,"name_long"] <- "Democratic People's Republic of Korea" 
world[103,"name_long"] <- "Iran (Islamic Republic of)" 
world[122,"name_long"] <- "Lao People's Democratic Republic"
world[210,"name_long"] <- "Syrian Arab Republic"
world[234,"name_long"] <- "Viet Nam"
world[57,"name_long"] <- "Czechia"
world[142,"name_long"] <- "North Macedonia"
world[137,"name_long"] <- "Republic of Moldova"
world[77,"name_long"] <- "United Kingdom of Great Britain and Northern Ireland"
world[231,"name_long"] <- "Venezuela (Bolivarian Republic of)"
world[42,"name_long"] <- "China, mainland"
world[227,"name_long"] <- "United States of America"

world[45,"name_long"] <- "Congo"
world[74,"name_long"] <- "Faroe Islands"


pest.per.h.map <- merge(world, t5, by = "name_long", all.x = T)
final_map <- pest.per.h.map %>% drop_na

data <- st_drop_geometry(final_map)


# 2020 pesticide per ha map
aux  <- quantile(data$"2020",seq(0,1,1/5))
aux  <- c(-Inf,aux[2:(length(aux)-1)],Inf)
cols <- c(brewer.pal(n = 5, name = 'YlOrRd'),"grey")
lab  <- c(as.character(unique(cut(data$"2020",aux))),"No Data")


png("Pest_global_map_2020.png",

    units="in",
    width = 12,
    height =10,
    res = 300,
    pointsize=13)

ggplot()+ geom_sf(data=world,aes(fill="grey")) +
	 geom_sf(data=final_map, aes(fill=cut(data$"2020",aux)))+ 
	 scale_fill_manual(name= "Pesticide use per ha",values=cols, labels=lab) +
	 ggthemes::theme_map()

dev.off()



################################
####   Temporal Analysis    ####
################################

# descriptive statistics
t6 <- t5[,-29]
colnames(t6) <- paste(rep("pest", 28), seq(1993, 2020, 1), sep = "_")

stat_des <- function(x) c(Min = min(x),  Q = quantile(x, 0.25),  Q = quantile(x, 0.50),  Q = quantile(x, 0.75),  Max = max(x), SD = sd(x), Skewness = skewness(x), Kurtosis = kurtosis(x))

stat <- apply(t6, 2, stat_des)
colnames(stat) <- seq(1993, 2020, 1)
write.csv(round(t(stat), 4), "stat_pest.csv")

xtable(t(stat), caption = 'Descriptive statistics', label = 't1', booktabs = TRUE, digits = 4)


# pesticide use in 1993 and 2020
png("Log_Distribution_1993_2020.png",
    units="in",
    width = 12, 
    height =10,
    res = 300,
    pointsize=13)

d1 <- log(t6$pest_1993) 
d2 <- log(t6$pest_2020) 

hist(d1,  breaks=seq(-10,10,0.5), col=rgb(1,0,0,.2), border="white", probability = TRUE, xlab = "Pesticide use in logarithm", ylab =  "Density", main = "", cex.lab=0.8)
hist(d2,  breaks=seq(-10,10,0.5), col=rgb(0,0,1,.2), border="white",probability = TRUE, add=TRUE)

lines(density(d1, kernel = "epanechnikov"), col = "red", lwd=1, lty = 1)
lines(density(d2, kernel = "epanechnikov"), col = "blue", lwd=1, lty = 2)

legend("topright", legend=c("Pesticide use in logarithm (1993)", "Pesticide use in logarithm (2020)"), col=c("red", "blue"), lty= c(1, 2), cex=1)

dev.off()



# markov chain
p <- mkv(log(data[,5:32]),fixed=FALSE)$Probabilities
p <- rbind(p,st.st(p))
rownames(p) <- c(paste("Q ",1:5,sep=""), "\u03C0")
colnames(p) <- paste("Q",1:5,sep=" ")
col <- c("white","orange")
p <- round(p,3)
data.plot <- reshape2::melt(p)
names(data.plot) <- c("y","x","Probabilities")
s <- rep(.2,150)
s[seq(6,150,6)]<-.7
g <- ggplot(data.plot,aes(x,y,Probabilities,label=sprintf("%0.3f", round(Probabilities, digits = 3)))) +
 		 geom_tile(aes(fill=Probabilities),color="black")+theme_bw() +
         scale_fill_gradient(low=col[1], high=col[2]) +
        labs(x=NULL, y=NULL,title=NULL) +
         theme(axis.title=element_text(size=16,face="bold"),
         		panel.border = element_rect(colour="white"),
               axis.text = element_text(size=14),
               axis.line = element_line(),
               legend.text =element_text(size=14),
               legend.title=element_text(size=.8*16,face="bold"),
               plot.title = element_text(size=2*16,hjust=0.5))+
         geom_text(size=6)+scale_x_discrete(position = "top")+
         	scale_y_discrete(limits = rev(levels(data.plot$y)))




png("Markov_prob_matrix.png",

    units="in",
    width = 12, 
    height =10,
    res = 300,
    pointsize=13)
g

dev.off()


###############################################
####        Spatio-Temporal Analysis       ####
###############################################

## W matrix: geography
knn1 <- knearneigh(coordinates(as_Spatial(final_map))) %>% knn2nb
c.dist <- max(unlist(nbdists(knn1,coordinates(as_Spatial(final_map)))))
k1 <- dnearneigh(coordinates(as_Spatial(final_map)), 0, c.dist)
w.dist <- nb2listw(k1)
sf_use_s2(FALSE) #fix the map's spherical problems

queen <- poly2nb(final_map)
queen[[20]]<-0L  # checked
queen[[44]]<-0L  # checked
queen[[124]]<-0L # checked

no.link <- subset(queen, subset=card(queen)==0)
no.link <- as.integer(attributes(no.link)$region.id)

queen_1 <- queen
queen_2 <- queen

queen_1[no.link] <- k1[no.link] 
queen_2[no.link] <- knn1[no.link]

n.queen <- card(queen_2)
w.dist <- nb2listw(queen_2)




## W matrix: crop
  Africa_crop  <- read.csv("Production_Crops_Livestock_E_Africa.csv")
  America_crop <- read.csv("Production_Crops_Livestock_E_Americas.csv")
  Europe_crop  <- read.csv("Production_Crops_Livestock_E_Europe.csv")
  Asia_crop    <- read.csv("Production_Crops_Livestock_E_Asia.csv")
  Oceania_crop <- read.csv("Production_Crops_Livestock_E_Oceania.csv")


  crop     <- rbind(Africa_crop, America_crop, Asia_crop, Oceania_crop, Europe_crop)
  crop     <- crop[crop$Element == "Area harvested",]
  crop     <- crop[, c(3, 6, seq(74, ncol(crop)-1, 2))]


# fix encoding problems
  crop$Area[crop$Area == "C\xf4te d'Ivoire"] <- "Côte d'Ivoire"
  crop$Area[crop$Area == "T\xfcrkiye"] <- "Turkey"


aux1  <- crop[which(crop$Area %in% rownames(t5)),]
types <- unique(aux1$Item)


h_c <- matrix(0,nrow(t5),ncol=length(types))

for(j in 1:length(types)){
	aux <- crop %>% filter(Item == types[j]) 
	aux <- aux[order(aux$Area),]
        aux <- aux[,-2]

	aux1 <- aux[which(aux$Area %in% rownames(t5)),]
        aux1 <- aux1[match(rownames(t5),aux1$Area),]

        aux1[which(!(rownames(t5) %in% aux1$Area)),"Area"] <- rownames(t5)[which(!(rownames(t5) %in% aux1$Area))]

        rownames(aux1) <- aux1[,1]
	aux1[,1] <- NULL


        dem <- 28 - apply(aux1,2,is.na) %>% rowSums
	dem <- ifelse(dem > 0, dem, 1)
	h_c[,j] <- rowSums(aux1, na.rm=TRUE)/dem
       
}



h_c_ha <- h_c/rowSums(h_c) 
d2     <- matrix(0, nrow(h_c_ha), nrow(h_c_ha))

for(k in 1:nrow(d2)){
	for(j in 1:nrow(d2)){
		cat(k,"fila ",j,"\n")
		d2[j,k] <- sum(abs(h_c_ha[k,] - h_c_ha[j,]))
	}
}




# gap = 0.6 (used value)
thresh <- apply(d2,1,function(x) min(x[x>0],na.rm=TRUE)) #threshold to ensure at least 1 neighbor
d.aux  <- matrix(0, nrow(h_c_ha), nrow(h_c_ha))

for (u in 1:length(thresh)){
	tt <- max(thresh[u],0.6) #switches between 0.6 or the threshold
	cat(tt,"\n")
	d.aux[u,] <- ifelse(d2[u,] <= tt,1,0)
}
d.aux[16,] <- d.aux[,16] <- 0
d.aux[16,40] <- d.aux[40,16] <- 1 # Use Dominican Republic as Bermuda`s neighbour
diag(d.aux) <- 0 
d.aux <- ifelse(d.aux + t(d.aux)>0,1,0)
rownames(d.aux) <- colnames(d.aux) <- data$name_long

w.d.p <- mat2listw(d.aux)




## W matrix: gdp

  Africa_gdp  <- read.csv("Macro-Statistics_Key_Indicators_E_Africa.csv")
  America_gdp <- read.csv("Macro-Statistics_Key_Indicators_E_Americas.csv")
  Europe_gdp  <- read.csv("Macro-Statistics_Key_Indicators_E_Europe.csv")
  Asia_gdp    <- read.csv("Macro-Statistics_Key_Indicators_E_Asia.csv")
  Oceania_gdp <- read.csv("Macro-Statistics_Key_Indicators_E_Oceania.csv")

# select only gdp per capita 
  full_gdp   <- rbind(Africa_gdp, America_gdp, Asia_gdp, Oceania_gdp, Europe_gdp)
  full_p_gdp <- full_gdp[full_gdp$Element == "Value US$ per capita, 2015 prices",]
  full_a_gdp <- full_p_gdp[, c(3, seq(78, ncol(full_p_gdp)-6, 3))]

# fix encoding problems + order
  full_a_gdp$Area[full_a_gdp$Area == "C\xf4te d'Ivoire"] <- "Côte d'Ivoire"
  full_a_gdp$Area[full_a_gdp$Area == "T\xfcrkiye"] <- "Turkey"
  full_a_gdp$Area[full_a_gdp$Area == "Netherlands (Kingdom of the)"] <- "Netherlands"
  full_a_gdp <- full_a_gdp[order(full_a_gdp$Area),]

# match pesticide data
  gdp_match <- full_a_gdp[which(full_a_gdp$Area %in% rownames(t5)),]

  tx <- gdp_match[match(rownames(t5), gdp_match$Area),]
  tx[which(!(rownames(t5) %in% tx$Area)),"Area"] <- rownames(t5)[which(!(rownames(t5) %in% tx$Area))]

  tx$mean <- apply(tx[,-1],1, mean)
  tx$mean[tx$Area == "Faroe Islands"] <- 56689.3 #from wordbank
  tx$mean[tx$Area == "China, Taiwan Province of"] <- tx$mean[tx$Area == "China, mainland"]


# creation of w based on gdp
  w_new      <- abs(outer(tx$mean, tx$mean, '-'))



# threashold = 300 (used value)
thresh <- apply(w_new,1,function(x) min(x[x>0],na.rm=TRUE)) #threshold to ensure at least 1 neighbor


w_test <- matrix(0, ncol = ncol(w_new), nrow = nrow(w_new))

for (u in 1:length(thresh)){
	tt <- max(thresh[u], 300) #switches between 300 or the threshold
	cat(tt,"\n")
	w_test[u,] <- ifelse(w_new[u,] <= tt,1,0)
}

rownames(w_test) <- colnames(w_test) <- rownames(t5)
diag(w_test) <- 0 
w.d.p_gdp <- mat2listw(w_test)
n.prod_gdp <- card(w.d.p_gdp$neighbours)



### analyses

## Moran I
data1 <- data
data[,5:32] <- apply(data1[,5:32],2,log) #using log

g.M.I <- lapply(5:32,function(x) moran.test(data[,x],listw=w.dist,alternative="two.sided"))
p.M.I <- lapply(5:32,function(x) moran.test(data[,x],listw= w.d.p,alternative="two.sided"))
d.M.I <- lapply(5:32,function(x) moran.test(data[,x],listw= w.d.p_gdp,alternative="two.sided"))


table.moran <- data.frame("Year"=names(data)[5:32],
                          
                          t(sapply(1:length(g.M.I), function(x) c("statistic"=round(unname(g.M.I[[x]]$estimate[1]),4), "z-value"=round(unname(g.M.I[[x]]$statistic[1]),4),"p-value"=g.M.I[[x]]$p.value))),

                          t(sapply(1:length(p.M.I), function(x)    c("statistic"=round(unname(p.M.I[[x]]$estimate[1]),4),"z-value"=round(unname(p.M.I[[x]]$statistic[1]),4), "p-value"=p.M.I[[x]]$p.value))),

                          t(sapply(1:length(d.M.I), function(x)    c("statistic"=round(unname(d.M.I[[x]]$estimate[1]),4),"z-value"=round(unname(d.M.I[[x]]$statistic[1]),4), "p-value"=d.M.I[[x]]$p.value))))

colnames(table.moran) <- c("Year", "statistic", "z-value", "p-value","statistic", "z-value","p-value", "statistic", "z-value", "p-value")



print(xtable(table.moran, caption = 'Moran I', label = 't3', booktabs = TRUE, digits=c(4,4,4,4,4,4,4,4,4,4,4)), include.rownames=FALSE)





## Spatial Markov
# geo
p1 <- sp.mkv(data[,5:32],w.dist,fixed=FALSE)$Probabilities
St <- do.call(rbind, lapply(1:5,function(x)st.st(p1[,,x])))

m<-rbind(p1[,,1],St[1,],p1[,,2],St[2,],p1[,,3],St[3,],p1[,,4],St[4,],p1[,,5],St[5,])
l.l <- c(rep("l(1) ",6),rep("l(2) ",6),rep("l(3) ",6),rep("l(4) ",6),rep("l(5) ",6))
rownames(m) <- paste(l.l, rep(c(paste("Q ",1:5,sep=""), "\u03C0"),5),sep=" ")
colnames(m) <- paste("Q",1:5,sep=" ")
  col<-c("white","orange")
  m<-round(m,3)
  data.plot <- reshape2::melt(m)
  names(data.plot) <- c("y","x","Probabilities")
  s <- rep(.2,150)
  s[seq(6,150,6)]<-.7
 g1 <- ggplot(data.plot,aes(x,y,Probabilities,label=sprintf("%0.3f", round(Probabilities, digits = 3)))) +
 		 geom_tile(aes(fill=Probabilities),color="black",size=s)+theme_bw() +
         scale_fill_gradient(low=col[1], high=col[2]) +
        labs(x=NULL, y=NULL,title=NULL) +
         theme(axis.title=element_text(size=16,face="bold"),
         		panel.border = element_rect(colour="white"),
               axis.text = element_text(size=14),
               axis.line = element_line(),
               legend.text =element_text(size=14),
               legend.title=element_text(size=.8*16,face="bold"),
               plot.title = element_text(size=2*16,hjust=0.5))+
         geom_text(size=6)+scale_x_discrete(position = "top")+
         	scale_y_discrete(limits = rev(levels(data.plot$y)))


png("Spatial_Markov_prob_matrix_geographical.png",

    units="in",
    width = 12, 
    height =10,
    res = 300,
    pointsize=13)
g1

dev.off()


sp.homo.test(data[,5:32],w.dist)
      

#crop
p1 <- sp.mkv(data[,5:32],w.d.p,fixed=FALSE)$Probabilities
St <- do.call(rbind, lapply(1:5,function(x)st.st(p1[,,x])))

m<-rbind(p1[,,1],St[1,],p1[,,2],St[2,],p1[,,3],St[3,],p1[,,4],St[4,],p1[,,5],St[5,])
l.l <- c(rep("l(1) ",6),rep("l(2) ",6),rep("l(3) ",6),rep("l(4) ",6),rep("l(5) ",6))
rownames(m) <- paste(l.l, rep(c(paste("Q ",1:5,sep=""), "\u03C0"),5),sep=" ")
colnames(m) <- paste("Q",1:5,sep=" ")
  col<-c("white","cyan4")
  m<-round(m,3)
  data.plot <- reshape2::melt(m)
  names(data.plot) <- c("y","x","Probabilities")
  s <- rep(.2,150)
  s[seq(6,150,6)]<-.7
 g2 <- ggplot(data.plot,aes(x,y,Probabilities,label=sprintf("%0.3f", round(Probabilities, digits = 3)))) +
 		 geom_tile(aes(fill=Probabilities),color="black",size=s)+theme_bw() +
         scale_fill_gradient(low=col[1], high=col[2]) +
        labs(x=NULL, y=NULL,title=NULL) +
         theme(axis.title=element_text(size=16,face="bold"),
         		panel.border = element_rect(colour="white"),
               axis.text = element_text(size=14),
               axis.line = element_line(),
               legend.text =element_text(size=14),
               legend.title=element_text(size=.8*16,face="bold"),
               plot.title = element_text(size=2*16,hjust=0.5))+
         geom_text(size=6)+scale_x_discrete(position = "top")+
         	scale_y_discrete(limits = rev(levels(data.plot$y)))

      
png("Spatial_Markov_prob_matrix_prod_dist.png",

    units="in",
    width = 12, 
    height =10,
    res = 300,
    pointsize=13)
g2

dev.off()



sp.homo.test(data[,5:32],w.d.p)


#gdp
p1 <- sp.mkv(data[,5:32],w.d.p_gdp,fixed=FALSE)$Probabilities
St <- do.call(rbind, lapply(1:5,function(x)st.st(p1[,,x])))

m<-rbind(p1[,,1],St[1,],p1[,,2],St[2,],p1[,,3],St[3,],p1[,,4],abs(St[4,]),p1[,,5],St[5,])
l.l <- c(rep("l(1) ",6),rep("l(2) ",6),rep("l(3) ",6),rep("l(4) ",6),rep("l(5) ",6))
rownames(m) <- paste(l.l, rep(c(paste("Q ",1:5,sep=""), "\u03C0"),5),sep=" ")
colnames(m) <- paste("Q",1:5,sep=" ")
  col<-c("white","deepskyblue2")
  m<-round(m,3)
  data.plot <- reshape2::melt(m)
  names(data.plot) <- c("y","x","Probabilities")
  s <- rep(.2,150)
  s[seq(6,150,6)]<-.7
 g3 <- ggplot(data.plot,aes(x,y,Probabilities,label=sprintf("%0.3f", round(Probabilities, digits = 3)))) +
 		 geom_tile(aes(fill=Probabilities),color="black",size=s)+theme_bw() +
         scale_fill_gradient(low=col[1], high=col[2]) +
     labs(x=NULL, y=NULL,title=NULL)+
         theme(axis.title=element_text(size=16,face="bold"),
         		panel.border = element_rect(colour="white"),
               axis.text = element_text(size=14),
               axis.line = element_line(),
               legend.text =element_text(size=14),
               legend.title=element_text(size=.8*16,face="bold"),
               plot.title = element_text(size=2*16,hjust=0.5))+
         geom_text(size=6)+scale_x_discrete(position = "top")+
         	scale_y_discrete(limits = rev(levels(data.plot$y)))

      
png("Spatial_Markov_prob_matrix_gdp_dist.png",

    units="in",
    width = 12, 
    height =10,
    res = 300,
    pointsize=13)
g3

dev.off()



sp.homo.test(data[,5:32],w.d.p_gdp)






## rose diagrams (all years) k = 4
dlisas <- lapply(5:31,function(x) d.LISA(data[,x],data[,x+1],w.dist,k =4, nsim=999,mean.rel=FALSE,Regime=data$continent))


p1 <- list()
for(i in 1:length(dlisas)){p1[[i]]<-dlisas[[i]]$data} 
p1 <- do.call(rbind.data.frame,p1)


step <- (2*pi)/4
breaks <- seq(0,2*pi,step)
lmt <- (breaks * 180)/pi # rad to deg
symb<-c()
for (i in seq_len(length(lmt)-1)){
	symb <- c(symb, paste(lmt[i],lmt[i+1],sep="-"))
}
lim=c(symb[1],symb[length(symb):2])

rose.p1<-ggplot(p1,aes(angle,fill=Regime),xlab=" ",ylab=" ") +geom_bar(width=1,colour="black",size=0.1)+scale_x_discrete(name="",limit=lim)+scale_y_continuous(name="",breaks=seq(0,max(table(p1$angle)),100),labels=seq(0,max(table(p1$angle)),100))+coord_polar(start=pi*0)+theme(legend.position="bottom",panel.grid.major = element_line( color="gray50",linetype="solid"),panel.background = element_blank(),axis.text = element_text(size = 10, colour="black"),axis.title = element_text(size = 10),legend.text=element_text(size=16),legend.title=element_text(size=18))

rc1 <- rose.p1 + scale_fill_brewer(palette = "Set2") + labs(fill = "Decomposition of movements across continents")


t1 <- p1 %$% table(Regime,angle)
t1 <- t1[,c(1, 4, 2, 3)]


xtable(round(t1,4))

print(xtable(t1,type ="latex") ,file="Cumulative_DLISA_geographical.tex")


png("DLISA_Cumulative_1993_2020_continent_geo.png",

    units="in",
    width = 15, 
    height =10,
    res = 300,
    pointsize=13)

rc1

dev.off()



# crop
dlisas <- lapply(5:31,function(x) d.LISA(data[,x],data[,x+1],w.d.p,nsim=999,k=4, mean.rel=FALSE,Regime=data$continent))


p1 <- list()
for(i in 1:length(dlisas)){p1[[i]]<-dlisas[[i]]$data} 
p1 <- do.call(rbind.data.frame,p1)


rose.p1<-ggplot(p1,aes(angle,fill=Regime),xlab=" ",ylab=" ") +geom_bar(width=1,colour="black",size=0.1)+scale_x_discrete(name="",limit=lim)+scale_y_continuous(name="",breaks=seq(0,max(table(p1$angle)),100),labels=seq(0,max(table(p1$angle)),100))+coord_polar(start=pi*0)+theme(legend.position="bottom",panel.grid.major = element_line( color="gray50",linetype="solid"),panel.background = element_blank(),axis.text = element_text(size = 10, colour="black"),axis.title = element_text(size = 10),legend.text=element_text(size=16),legend.title=element_text(size=18))

rg1 <- rose.p1 + scale_fill_brewer(palette = "Set2")  +  ggtitle("CROP") + labs(fill = "Decomposition of movements across continents")



t1 <- p1 %$% table(Regime,angle)
t1 <- t1[,c(1, 4, 2, 3)]


xtable(round(t1,4))

print(xtable(t1,type ="latex") ,file="Cumulative_DLISA_prod_dist.tex")


png("DLISA_Cumulative_1993_2020_continent_crop.png",

    units="in",
    width = 15, 
    height =10,
    res = 300,
    pointsize=13)

rg1

dev.off()


# gdp
dlisas <- lapply(5:31,function(x) d.LISA(data[,x],data[,x+1],w.d.p_gdp,nsim=999,k=4, mean.rel=FALSE,Regime=data$continent))


p1 <- list()
for(i in 1:length(dlisas)){p1[[i]]<-dlisas[[i]]$data} 
p1 <- do.call(rbind.data.frame,p1)


rose.p1<-ggplot(p1,aes(angle,fill=Regime),xlab=" ",ylab=" ") +geom_bar(width=1,colour="black",size=0.1)+scale_x_discrete(name="",limit=lim)+scale_y_continuous(name="",breaks=seq(0,max(table(p1$angle)),100),labels=seq(0,max(table(p1$angle)),100))+coord_polar(start=pi*0)+theme(legend.position="bottom",panel.grid.major = element_line( color="gray50",linetype="solid"),panel.background = element_blank(),axis.text = element_text(size = 10, colour="black"),axis.title = element_text(size = 10),legend.text=element_text(size=16),legend.title=element_text(size=18))

rp1 <- rose.p1 + scale_fill_brewer(palette = "Set2") +  ggtitle("GDP") + labs(fill = "Decomposition of movements across continents")



t1 <- p1 %$% table(Regime,angle)
t1 <- t1[,c(1, 4, 2, 3)]


xtable(round(t1,4))

print(xtable(t1,type ="latex") ,file="Cumulative_DLISA_gdp_dist.tex")


png("DLISA_Cumulative_1993_2020_continent_gdp.png",

    units="in",
    width = 15, 
    height =10,
    res = 300,
    pointsize=13)

rp1

dev.off()





png("DLISA_Cumulative_1993_2020_continent_gdp_crop.png",

    units="in",
    width = 20, 
    height =10,
    res = 300,
    pointsize=13)

ggpubr::ggarrange(rg1,rp1, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

dev.off()
###################                    the end                  #####################################
