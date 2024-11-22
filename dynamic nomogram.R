
install.packages(c("DynNom","shiny","Hmisc","devtools"))

install.packages(c("DynNom","regplot"))

library(DynNom)
library(rms)
library(Hmisc)
library(devtools)
library(htmltools)
library(magrittr)
library(shiny)
library(regplot)

library(readxl)
mydata <- read_excel("XXXXXXXXX.xlsx")
View(mydata)

dev <- train
vad <- test

ddist <- datadist(dev)
options(datadist="ddist")

dev<- na.omit(dev)
dev <- as.data.frame(dev)
dim(dev)

##Set the variables
dev$diagnosis <- factor(dev$diagnosis,labels = c("alive","dead"))
dev$age. <- factor(dev$age.,labels = c("<56",">=56"))
dev$Lym. <- factor(dev$Lym.,labels = c("<0.60",">=0.60"))
dev$CPB. <- factor(dev$CPB.,labels = c("<230",">=230"))
dev$Venti. <- factor(dev$Venti.,labels = c("<62",">=62"))
dev$CRRT <- factor(dev$CRRT, labels = c("normal","need CRRT"))


##Develope the model
modelD <-glm(diagnosis ~ age.+Lym.+CPB.+CRRT+Venti.+IL10, family = binomial,
             data = dev)

##remove.packages("fastmap")
##install.packages("fastmap", version = "1.2.0")
##install.packages("fastmap")
library(fastmap)
##Draw a dynamic nomogram
DynNom(modelD,
       DNtitle = "Nomogram",
       DNxlab = "probability",
       data = dev)

mydata <- read_excel(XXX.xlsx")

mydata$status <- factor(mydata$status,labels = c("alive","dead"))
mydata$age <- factor(mydata$age,labels = c("<56","≥56"))
mydata$Lym <- factor(mydata$Lym,labels = c("≥0.60","<0.60"))###change
mydata$CPB <- factor(mydata$CPB,labels = c("<230","≥230"))
mydata$Ventilation <- factor(mydata$Ventilation,labels = c("<62","≥62"))
mydata$CRRT <- factor(mydata$CRRT, labels = c("normal","need CRRT"))

dd=datadist(mydata)
option <- options(datadist = "dd")

colnames(mydata)
formula <- as.formula(status ~ age + Lym + CPB + CRRT + Ventilation+ IL10)
##formula <- as.formula(status ~ age + Lym + CPB + CRRT + Ventilation)
model <- lrm(formula,
             data = mydata,
             x=TRUE,
             y=TRUE)

Nomogram_1 <- nomogram(model,
                       fun = function(x)1/(1+exp(-x)),
                       lp=F,
                       fun.at = c(.001,.01,.05,.5, .95, .99),
                       funlabel = "Risk")

plot(Nomogram_1,
     xfarc= 0.30,
     cex.var = 1.6,
     cex.axis = 1.4,
     tcl = -0.5,
     lmgp = 0.3,
     label.every = 1)

## 添加个案的列线图
Nomogram_2 <- glm(formula,
                  family = binomial(link = logit),
                  data = mydata)
regplot(Nomogram_2,
        plots = c("density","boxes"),
        observation = mydata[15,],#指定样本 
        center = T,
        points = TRUE,
        droplines = F,
        title = "individual nomogram",
        odds = F,
        interval = "confidence",
        rank = NULL,
        clickable = F,##是否启用交互模式
        dencol = "skyblue",
        boxcol = "skyblue"
        )




####Shinyapps*


setwd("D:/R work/DynNomapp")
data <- read_excel("XXXXX.xlsx")

data$BMI <- factor(data$BMI,
                    levels =c(0,1),
                    labels =c("＜26.3","≥26.3"))

data$Monocyte <- factor(data$Momocyte,
                   levels =c(0,1),
                   labels =c("＜0.64","≥0.64"))

data$LAC <- factor(data$LAC,levels =c(0,1),labels =c("＜1.75","≥1.75"))
data$pCrCl <- factor(data$pCrCl,levels =c(0,1),labels =c("＜80.8","≥80.8"))
data$IL6 <- factor(data$IL6,levels =c(0,1),labels =c("＜71","≥71"))
data$IL10 <- factor(data$IL10,levels =c(0,1),labels =c("＜22","≥22"))

dat <- data
fit.glm <- glm(status ~ BMI+Monocyte+LAC+CPB+Ttrans+pCrCl+IL6+IL10,
               data = dat,
               family = binomial()) 
library(DynNom)
DNbuilder(fit.glm,
          covariate = "numeric")
## www.shinyapps.io
##install.packages("rsconnect")
library(rsconnect)

rsconnect::setAccountInfo(name='XX', token='XX', secret='XX')
accounts(server = NULL)

dir <- getwd()
path <- paste0(dir,"/DynNomapp",collapse = "")
path

rsconnect::deployApp("XXXX")  ##
