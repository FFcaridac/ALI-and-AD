

setwd("xxx")

library(corrplot) 
library(glmnet) 
library(caret) 
library(CBCgrps)
library(nortest)
library(tidyverse)
library(ggpubr)
library(rms)
library(pROC)
library(viridis)
library(ggplot2)
library(ggvenn)
library(RColorBrewer)
library(xgboost)
library(randomForest)



rm(list=ls())

library(readxl)
data <- read_excel("xxxx")
View(data)


###随机森林
data%>%
  mutate(Grade=sample(LETTERS[1:6],252,replace=T))->data
view(data)

##变量转化因子型
data$Event <- as.factor(data$Event)
data$Grade <- as.factor(data$Grade)
str(data)
##独热编码
encoded_vars<- model.matrix(~Grade-1,data=data)
encoded_df <-cbind(data[,!names(data)%in% c("Grade")],encoded_vars)
View(encoded_df)
str(encoded_df)

##拆分测试集&训练集
set.seed(123)
trainIndex <- createDataPartition(encoded_df$Event,p=.7,
                                  list = FALSE,
                                  times = 1)
trainData <-encoded_df[trainIndex,]
testData <- encoded_df[-trainIndex,]

###随机森林二分类变量筛选
fit.rf <- randomForest(
  Event ~ .,
  data= trainData,
  importance= TRUE,
  ntree =500
)

fit.rf

##10-fold CV
set.seed(123)
ctrl <- trainControl(
  method = "CV",
  number = 10
)
rf_model_cv <- train(
  Event ~.,
  data = trainData,
  method ="rf",
  trControl = ctrl
)

##模型评价
rf_pred <- predict(
  rf_model_cv,
  newdata = testData,
  type = "prob"
)[,2]

rf_roc <-roc(
  testData$Event,
  rf_pred
)

auc_value <-auc(rf_roc)

##绘制ROC曲线
p1 <-ggroc(rf_roc,legacy.axes = TRUE,size=1,color="#69b3a2")+
  ggtitle("ROC curve of Randomforest")+
  geom_abline(intercept = 0,slope = 1,linetype="dashed",color="grey")+
  theme_bw()+
  annotate(geom = "text",
           label=paste0("AUC in the test set: ",round(auc_value,3)),
           x=0.7,
           y=0.05)

p1

ggsave("RF_ROC_0.804.png",
       plot = p1,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

ggsave("RF_ROC_0.804.pdf",
       plot = p1,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)


###随机森林 变量重要性评价
varImpPlot(fit.rf)

rf.imp <- importance(fit.rf)%>%as.data.frame()
rf.imp%>%
  mutate(Variable=rownames(rf.imp))%>%
  arrange(desc(MeanDecreaseAccuracy))->rf.imp

top_vars<-rf.imp[1:15,]

###创建变量重要性绘图

p2<-ggplot(top_vars,
           aes(y=reorder(Variable,-MeanDecreaseAccuracy),
               x=MeanDecreaseAccuracy))+
  geom_bar(stat = "identity",
           fill = "#69b3a2",
           alpha = 0.8,
           width = 0.6)+
  scale_fill_viridis()+
  labs(title = "Variable Importance Plot of RF",
       x= "Important Score",
       y= "")+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30,hjust = 0.9))
p2

ggsave("RF_VIP_15.png",
       plot = p2,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

ggsave("RF_VIP_15.pdf",
       plot = p2,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

  
##看不同树木和OOB之间的关系
plot(fit.rf)

err_rates <-fit.rf[["err.rate"]]

err_rates%>%
  as.data.frame()%>%
  mutate(Tree=1:500) ->err_rates
view(err_rates)
###数据转化
err_rates%>%
  pivot_longer(cols = 1:3,
               names_to = "OOB",
               values_to = "value") ->err_rates
view(err_rates)

##创建OOB与树数量关系的折线图
p3<-ggplot(err_rates,
           aes(x=Tree,
               y=value,
               color=OOB))+
  geom_line(size=1) +
  labs(title = "The relationship between tree number and OOB",
       x="Number of Trees",
       y="Error Rate")+
  scale_color_brewer(palette = "Set2",
                     name = "Error rate",
                     label=c("Normal","ALI","OOB"))+
  theme_bw()

p3

ggsave("RF_OOB_Reverse.png",
       plot = p3,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

ggsave("RF_OOB_Reverse.pdf",
       plot = p3,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)


####
####LASSO
set.seed(1234)
x <- model.matrix(Event~.,trainData)[,-1]

y <- as.numeric(trainData$Event)-1

fit.lasso<-glmnet(x,
                  y,
                  alpha = 1,
                  family = "binomial",
                  type.measure = "class")
cv_lasso <- cv.glmnet(x,y,alpha=1,
                      family ="binomial",
                      type.measure = "class",
                      nfolds = 10)
plot(fit.lasso,xvar = "lambda")

plot.data <- data.frame(as.matrix(fit.lasso$lambda),
                        as.matrix(t(fit.lasso$beta)))
plot.data%>%
  rename(lambda="as.matrix.fit.lasso.lambda.")%>%
  pivot_longer(cols = 2:ncol(plot.data),
               names_to = "variable",
               values_to = "coef")->plot.data

view(plot.data)

##绘制图形
colors <- viridis(73)

scientific_10 <-function(x){
  parse(text = gsub("e","%*% 10^",scales::scientific_format()(x)))
}

p4 <-ggplot(plot.data,
            aes(x=lambda,
                y=coef,
                color=variable))+
  geom_line(size=1,alpha=0.8)+
  scale_color_manual(values = colors)+
  scale_x_log10(label=scientific_10)+
  labs(title = "LASSO Regression Path",
       x= "Log lambda",
       y= "Coefficient")+
  theme_bw()+
  theme(legend.text = element_text(size = 6))
p4

plot(cv_lasso)

###ROC
x_test <- model.matrix(Event~., testData)[,-1]
y_test<-as.numeric(testData$Event)-1

y_pred <-predict(cv_lasso,
                 s= cv_lasso$lambda.min,
                 newx = x_test,
                 type ="class")
y_prob <-predict(cv_lasso,
                 s= cv_lasso$lambda.min,
                 newx = x_test,
                 type= "response")

roc_lasso <-roc(y_test,
             y_prob[,1])
auc_value <- auc(roc_lasso)

p5 <- ggroc(roc_lasso,
            legacy.axes =TRUE,
            size=1,
            color="#69b3a2")+
  ggtitle("ROC curve of LASSO")+
  geom_abline(intercept = 0,
              slope = 1,
              linetype= "dashed",
              color="grey")+
  theme_bw()+
  annotate(geom = "text",
           label= paste0("AUC in the test set: ",round(auc_value,3)),
           x=0.7,
           y=0.05)
p5

cv_lasso

ggsave("LASSO_ROC_0.712.png",
       plot = p5,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

ggsave("LASSO_ROC_0.712.pdf",
       plot = p5,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

se_lambda <- cv_lasso$lambda.min
se_lambda

se_coef <-coef(cv_lasso,s="lambda.min")
se_coef

##非零系数
index <- which(se_coef!=0)
index
coef <- se_coef[index][-1] #对应回归系数
coef

diffvariables=row.names(se_coef)[index][-1]
diffvariables

lasso.result.se <-cbind(diffvariables,coef)%>%
  as.data.frame()
lasso.result.se

lasso.result.se%>%
  mutate(direction=ifelse(coef>0,"up","Down"))->lasso.result.se
lasso.result.se

lasso.result.se$coef <-as.numeric(lasso.result.se$coef)

p6 <-ggplot(lasso.result.se,
            aes(y=reorder(diffvariables,coef),
                x=coef,
                fill=direction))+
  geom_col(alpha=0.8,width = 0.6)+
  labs(title = "Important variables in LASSO",
       x="Coef",
       y="")+
  coord_flip()+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  theme(axis.title.x = element_text(angle = 30,hjust = 0.9),
        legend.position = "")
p6

ggsave("LASSO_VIP22.png",
       plot = p6,
       width = 12,
       height = 8,
       units = "in",
       dpi = 600)
ggsave("LASSO_VIP22.pdf",
       plot = p6,
       width = 12,
       height = 8,
       units = "in",
       dpi = 600)

#####Xgboost
trainlabel <- as.numeric(trainData$Event)-1
testlabel <- as.numeric(testData$Event)-1

train_matrix <- xgb.DMatrix(data=as.matrix(trainData[,-1]),
                            label=trainlabel)
test_matrix <- xgb.DMatrix(data = as.matrix(testData[,-1]),
                           label=testlabel)

##定义训练模型的所需的参数
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.01,
  max_depth = 4,
  subsample = 0.8,
  colsample_bytree = 0.8
)

##10-fold CV
set.seed(123)
cv <- xgb.cv(params = params,
             data = train_matrix,
             nrounds = 1000,
             nfold = 10,
             early_stopping_rounds = 10)


##找到最佳迭代函数
best_iter <-which.max(cv$evaluation_log$test_auc_mean)
best_iter

##训练模型
model <-xgb.train(params = params,
                  data = train_matrix,
                  nrounds = best_iter)

##测试机预测

pred <- predict(model,test_matrix)

##ROC曲线和AUC的值
roc_xgb <-roc(testData$Event,pred)
auc <- auc(roc_xgb)

p7 <- ggroc(roc_xgb,
            legacy.axes = TRUE,
            size = 1,
            color = "#69b3a2")+
  ggtitle("ROC curve of XGBoost")+
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              color = "grey")+
  theme_bw()+
  annotate(geom = "text",
           label = paste0("AUC in the test set: ",round(auc,3)),
           x = 0.7,
           y = 0.05)
p7

ggsave("XGBoost_ROC_0.788.png",
       plot = p7,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

ggsave("XGBoost_ROC_0.788.pdf",
       plot = p7,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)



##变量重要性评价
importance <- xgb.importance(model = model)

importance <- importance[order(importance$Gain,
                               decreasing = TRUE), ][1:15,1:2]
p8 <-ggplot(importance,
            aes(y = reorder(Feature,-Gain),
                x = Gain))+
  geom_bar(stat =  "identity",
            fill = "#69b3a2",
            alpha = 0.8,
            width = 0.6)+
  scale_fill_viridis()+
  labs(title = "Variable Importance plot of XGBoost",
       x = "Importance Score",
       y = "")+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30,hjust = 0.9))
p8

ggsave("XGBoost_VIP_15.png",
       plot = p8,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

ggsave("XGBoost_VIP_15.pdf",
       plot = p8,
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

###BiocManager::install("shap")
##source("shap.R") 
library(dplyr)       # 数据处理
library(mlbench)     # 加载数据集
library(shapviz)     # SHAP值计算与可视化
library(tidyverse)   # 数据操作
library(tidymodels)  # 包含了rsample等相关包

##shap_result = shap.score.rank(xgb_model = model,
                              X_train = train_matrix,
                              shap_approx = F)


hubgene <- list(
  RF=rownames(top_vars),
  LASSO=lasso.result.se$diffvariables,
  XGBoost=importance$Feature)
hubgene

ggvenn(hubgene,
       show_percentage = F,
       stroke_color = "black",
       stroke_alpha =  0.3,
       stroke_linetype = "solid",
       stroke_size =  0.8,
       set_name_color = "red",
       set_name_size = 6,
       text_color = "white",
       text_size = 6)+
  scale_fill_brewer(palette = "Set1")

hubgenename <- intersect(intersect(rownames(top_vars),
                                   lasso.result.se$diffvariables),
                         importance$Feature)
hubgenename
