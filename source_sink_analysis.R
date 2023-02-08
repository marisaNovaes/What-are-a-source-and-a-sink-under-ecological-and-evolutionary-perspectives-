#==================== Packages ===============

install.packages("openxlsx")
install.packages("tmap", dependencies = T)
install.packages("summarytools")

library(raster)
library(openxlsx)
library(tmap)
library(summarytools)
library(tidyverse)
library(hrbrthemes)
library(MASS) 
library(scales) 
library(ggplot2)

#=================== Standards ================
cor=c("green","orange","blue")

projwgs <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
projalbers.sirgas <-"+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000 +y_0=10000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
newproj="+proj=utm +datum=WGS84 +no_defs"

#================== Folder data ================

setwd("D:\\PPGBAN\\Doutorado")

#=================== Data reading ==============

dados= read.xlsx("dados_analise.xlsx") #data
View(dados)

mundo = shapefile("mundo_santartida.shp") #shapefile
plot(mundo)

#================ Study map ====================

#Create a new field to put the information you want to appear on the map
mundo$novo=0

#Modify the new variable to the country you want using the correct country name 
mundo$novo[mundo$NAME=="Antigua and Barbuda"]=1
mundo$novo[mundo$NAME=="Argentina"]=3
mundo$novo[mundo$NAME=="Australia"]=3
mundo$novo[mundo$NAME=="Barbados"]=1
mundo$novo[mundo$NAME=="Belize"]=1
mundo$novo[mundo$NAME=="Bosnia and Herzegovina"]=1
mundo$novo[mundo$NAME=="Brazil"]=4
mundo$novo[mundo$NAME=="Canada"]=8
mundo$novo[mundo$NAME=="China"]=6
mundo$novo[mundo$NAME=="Congo"]=1
mundo$novo[mundo$NAME=="Costa Rica"]=1
mundo$novo[mundo$NAME=="Croatia"]=1
mundo$novo[mundo$NAME=="Dominica"]=1
mundo$novo[mundo$NAME=="Estonia"]=1
mundo$novo[mundo$NAME=="Finland"]=1
mundo$novo[mundo$NAME=="France"]=3
mundo$novo[mundo$NAME=="Germany"]=1
mundo$novo[mundo$NAME=="Greenland"]=1
mundo$novo[mundo$NAME=="India"]=4
mundo$novo[mundo$NAME=="Japan"]=1
mundo$novo[mundo$NAME=="Maldives"]=1
mundo$novo[mundo$NAME=="Mexico"]=1
mundo$novo[mundo$NAME=="Netherlands"]=3
mundo$novo[mundo$NAME=="Peru"]=1
mundo$novo[mundo$NAME=="Poland"]=1
mundo$novo[mundo$NAME=="Portugal"]=1
mundo$novo[mundo$NAME=="Romania"]=1
mundo$novo[mundo$NAME=="Slovenia"]=1
mundo$novo[mundo$NAME=="Spain"]=4
mundo$novo[mundo$NAME=="St. Christopher-Nevis"]=1
mundo$novo[mundo$NAME=="St. Lucia"]=1
mundo$novo[mundo$NAME=="St. Vincent and the Grenadines"]=1
mundo$novo[mundo$NAME=="Sweden"]=4
mundo$novo[mundo$NAME=="Switzerland"]=2
mundo$novo[mundo$NAME=="Trinidad and Tobago"]=1
mundo$novo[mundo$NAME=="United Kingdom"]=3
mundo$novo[mundo$NAME=="United States"]=28
mundo$novo[mundo$NAME=="Vietnam"]=2
mundo$novo[mundo$NAME=="Zimbabwe"]=2

#Plotting the map
x11()
tmap_style("white")
mapa <- tm_shape(mundo)+
  tm_fill("novo", title = "Number of Publications",
          style="fixed", breaks=c(-Inf, 1, 15, 30),
            palette = c('#fee0d2','#fc9272','#de2d26'))+
  tm_borders(alpha=.1)+
  tm_legend(legend.format = list(text.separator= "to",text.less.than = "Less than"), 
            legend.position=c("left","bottom"), 
            legend.title.size = 1.5,
            legend.text.size = 1.2) +
  tm_scale_bar(width = 0.15, text.size = 0.7, text.color = NA, 
               color.dark = "black", color.light = "white", lwd = 1, position = NA)+
  tm_layout(attr.outside = TRUE, legend.outside = FALSE, frame=NA)

mapa

#breaks=c(-Inf, 1, 15, 30)

tmap_save (mapa, filename = "mapa1.png")
#width = 1000, height = 1400


#================ Descriptive Analysis =============
table(dados$REINO[dados$INC=="S"], useNA="no") 

table(dados$CLASSE[dados$INC=="S"], useNA="no")

table(dados$ESCALA[dados$INC=="S"], useNA="no")

table(dados$TIPO[dados$INC=="S"], useNA="no")

table(dados$DEF[dados$INC=="S"], useNA="no")

table(dados$PAIS[dados$INC=="S"], useNA="no") #Frequency of each country

ctable(dados$CLASSE[dados$INC=="S"], dados$ESCALA[dados$INC=="S"], useNA="no") #Comparing class with scale 

#Removing the scales that do not have enough n for the analysis
escala_nova = dados$ESCALA[dados$INC=="S"]
escala_nova[escala_nova==1]=NA; escala_nova[escala_nova==3]=NA; escala_nova[escala_nova==5]=NA
table(escala_nova)

#Removing the classes that do not have enough n for the analysis
classe_nova = dados$CLASSE[dados$INC=="S"]
classe_nova[classe_nova=="Actinopterygii"]=NA; classe_nova[classe_nova=="Amphibia"]=NA; classe_nova[classe_nova=="Arachnida"]=NA; 
classe_nova[classe_nova=="Bivalvia"]=NA; classe_nova[classe_nova=="Reptilia"]=NA

#Table with only the classes and scales with enough n
ctable(classe_nova[dados$INC=="S"], escala_nova[dados$INC=="S"], useNA="no")

#Chi square test
chisq.test(classe_nova[dados$INC=="S"], escala_nova[dados$INC=="S"])

#Comparing scale with study types
ctable(dados$ESCALA[dados$INC=="S"], dados$ESTUDO[dados$INC=="S"], useNA="no")
ctable(escala_nova[dados$INC=="S"], dados$ESTUDO[dados$INC=="S"], useNA="no") 

#Chi square test
chisq.test(escala_nova[dados$INC=="S"], dados$ESTUDO[dados$INC=="S"])

ctable(dados$PAIS[dados$INC=="S"], dados$SELECAO[dados$INC=="S"], useNA="no")

#================ Quantitative Analysis =============

#Distribution of body mass data
x11()

p <- ggplot(dados, aes(x=MASSA)) + 
  geom_histogram(binwidth = 1.5, color="white", fill="gray62") +
  labs(x="Body Mass", y = "Count")

p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
  theme_classic()

ggsave("hist_bodymass.png", width=9, height=8, unit="cm", dpi=300)
dev.off()

#Correlation between body mass and biological scale using ggplot
library(ggplot2)

#Without log
ggplot(dados, aes(y = MASSA, x = ESCALA)) +
  geom_boxplot()

#With log
ggplot(dados, aes(y = log10(MASSA), x = ESCALA)) +
  geom_boxplot()

#Correlation between body mass and biological scale using boxplot

#Save image
#png(file="saving_plot1.png", width = 900, height = 800)

#Rename the labels
label1=c("Individual","Population","Community","Landscape","Biogeographical")

# Labels...
aty <- axTicks(2)
labels <- sapply(aty,function(i)
  as.expression(bquote(10^.(i) ))
)
axis(2,at=aty,labels=labels)

#Boxplot
 b <- boxplot(log10(dados$MASSA)~ dados$ESCALA, names=label1, xlab = "Research scale", 
        ylab= expression("Body Mass Log"[10]))

#Plotting the points
stripchart(log10(dados$MASSA)~ dados$ESCALA,
           data = dados,
           method = "jitter",
           pch = 19,
           col = 2:6,
           vertical = TRUE,
           add = TRUE)
par(cex.lab=1) # is for y-axis
par(cex.axis=1) # is for x-axis

#Relationship between size (of study area) and body mass
x11()
#VD: Dimensao; VI: Massa
plot(log10(dados$DIMENSAO),log10(dados$MASSA))

#Linear regression
pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)

#Model construction
mod <- lm(log10(DIMENSAO) ~ log10(MASSA), dados)

#Graphical Analysis
x11()
par(mfrow=c(2,2))
plot(mod)

png("regression_assumptions.png")
png(file="regression_assumptions.png")
dev.off()

par(mfrow=c(1,1))

#Residuals Normality
shapiro.test(mod$residuals)

#Outliers in residuals
summary(rstandard(mod))

#Residue Independence (Durbin-Watson)
durbinWatsonTest(mod)

#Homoscedasticity (Breusch-Pagan):
bptest(mod)

# Model analyses
summary(mod)

ggplot(data = dados, mapping = aes(x = log10(MASSA), y=log10(DIMENSAO))) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") + 
  theme_classic()

X11()
pacman::p_load(ggpmisc)
ggplot(data = dados, mapping = aes(x = log10(MASSA), y=log10(DIMENSAO))) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label=paste(..eq.label.., ..adj.rr.label..,
                                        sep = "*plain(\",\")~~")),
  label.x = -5, label.y = 6) +
  labs(x=expression("Body Mass Log"[10]), y = expression("Dimension research Log"[10])) +
  theme_classic()

ggsave("regressao.png", width=14, height=14, unit="cm", dpi=300)
dev.off()

