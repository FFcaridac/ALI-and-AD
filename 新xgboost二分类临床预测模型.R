
install.packages(c("ggpubr","corrplot",
                   "glmnet","caret","CBCgrps",
                   "tidyverse"))
install.packages("data.table","xgboost","Matrix","caTools")
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

library(readxl)
data <- read_excel("C:/Users/79871/Desktop/ALI/DATA/Routcome.xlsx")

##data <- read_excel("C:/Users/79871/Desktop/0814IL10/data1011.xlsx")

View(data)

setwd("C:/Users/79871/Desktop/ALI/R1016")

##setwd("C:/Users/79871/Desktop/0814IL10/R1011")

#数据整理：主要靠Excel，导入后做轻微设置改动####

head(data)
#设置因子型
ordata <- data
colnames(data)
data[,1:5] <- lapply(data[,1:5],as.factor)

###基线表制作####
data <- as.data.frame(data)#有时候需要
tab1 <-twogrps(data, gvar = "diagnosis",ShowStatistic = T)
write.csv(tab1$Table,file="tab1outcome.csv")
###如果导出不了tab1先运行以下这句代码：tab1<-as.data.frame(tab1$Table)
###基线表的另外一个做法，备用于CBCgrps做不出来的情况
#先安装这个包
install.packages("tableone")
library(tableone) # 加载包，这个包要自己指定非参数检验的变量，具体看帮助文档
tab3 <- CreateTableOne(strata = "diagnosis" , data = data)
tab3
tab3Mat <- print(tab3, exact = "extent", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## 保存为 CSV 格式文件
write.csv(tab3Mat, file = "myTable1.csv")

###ROC####
rocdata <- ordata
rocdata$diagnosis <- as.factor(rocdata$diagnosis)
###rocdata$diagnosis <- as.numeric(rocdata$diagnosis)
reformulate(colnames(data))
roc.list <- roc(diagnosis ~ sex + TotalArch + Bentall + Age. + BMI. + CrCl. + 
                  pCrCl. + WBC. + Lym. + Mono. + Neu. + HBg. + LAC. + RBCper. + 
                  Plasmaper. + PLTper. + Cryoper. + Ttrans. + pWBC. + IL6. + 
                  IL10. + TNF. + HAC. + ACCT. + CPB. + age + BMI + CrCI + pCrCI + 
                  WBC + Lymphocyte + Monocyte + Neutrophil + Hemoglobin + MCV + 
                  RDW + PLT + PDW + sCr + BUN + AST + ALT + PT + INR + FIB + 
                  TT + Ddimer + CRP + cTnI + cTnT + CKMB + MYO + PH + PCO2 + 
                  GLU + HCO3 + BE + LAC + RBCper + Plasmaper + PLTper + Cryoper + 
                  Ttransfusion + pWBC + pLym + pMono + pNeu + pHb + pPLT + 
                  psCr + pBUN + pAST + pALT + IL6 + IL10 + TNF + IFN + HCA + 
                  ACCT + CPB, data = rocdata)

g.list <- ggroc(roc.list,alpha = 0.8, linetype = 1, size = 1)
g.list+theme_test()
#提取AUC####
auc <- sapply(roc.list,"[",9)
auc <- as.data.frame(auc)
auc <- t(auc)
auc <- as.data.frame(auc)
AUC <- arrange(auc,desc(V1),by_group=FALSE)
write.csv(AUC,"aucall.csv")

signif(AUC$V1,digit=3) 
AUC$X <- rownames(AUC)
AUC$X1 <- gsub('.{4}$', '', AUC$X)

ggplot(AUC, aes(x = V1, y = reorder(X1, V1),fill=V1)) +
  geom_col(width = 0.7) +
  scale_fill_viridis(begin = 0, end = 0.85, option = "D") +
  labs(x = "AUC") +
  ylab("")+
  geom_text(aes(label = signif(AUC$V1,digit=3) ),
            size=2.5,vjust=0.3,hjust=-0.2,color="#2878B5")+
   scale_x_continuous(limits = c(0, 1))+
  theme_classic()+theme(legend.position = 'none')
  

###相关性共线性分析：####
numdata = data%>%
  select(where(is.numeric))

##或者粗糙点
numdata = ordata[,-1]

#计算相关性矩阵
M<-cor(numdata,method = "spearman")
M
#相关性的显著性检验
testRes<-cor.mtest(numdata,conf.level = 0.95)
testRes
par(mfrow=c(2,3))
#相关性热图####
corrplot(M,method ='circle',tl.col = "black")
corrplot(
  M,
  method='color',
  type = 'upper',
  add = T ,
  tl.pos = "n",
  cl.pos = "n",
  diag = F,
  p.mat = testRes$p,
  sig.level = c(0.001,0.01,0.05),
  pch.cex = 1.5,
  insig = 'label_sig'
)

##数据集划分####

set.seed(3464)
trainIndex <- createDataPartition(data$diagnosis, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train <- data[trainIndex,]
test  <- data[-trainIndex,]
write.csv(train,file="train.csv")
write.csv(test,file="test.csv")

#train 基线表####
train <- as.data.frame(train)#有时候需要
tab2 <-twogrps(train, gvar = "diagnosis")
write.csv(tab2$Table,file="tab1train.csv")
##test 基线表###
test <- as.data.frame(test)#有时候需要
tab2222 <-twogrps(test, gvar = "diagnosis")
write.csv(tab2222$Table,file="tab1test.csv")


#minmax标准化转换####
min_max_scale = function(x){
  (x-min(x))/(max(x)-min(x))
}

data2 = train%>%
  mutate_if(.predicate = is.numeric,
            .funs = min_max_scale)%>%
  as.data.frame()

set.seed(123) #random number generator
x <- data.matrix(data2[, -1])
y <- data2[, 1]
y<-as.numeric(unlist(y))
#lasso
lasso <- glmnet(x, y, family = "binomial",nlambda = 1000, alpha = 1)
print(lasso)
plot(lasso, xvar = "lambda", label = TRUE)

tmp <- as_tibble(as.matrix(coef(lasso)), rownames = "coef") %>% 
  pivot_longer(cols = -coef, 
               names_to = "variable", 
               names_transform = list(variable = parse_number), 
               values_to = "value") %>% 
  group_by(variable) %>% 
  mutate(lambda = lasso$lambda[variable + 1], 
         norm = sum(if_else(coef == "(Intercept)", 0, abs(value))))

ggplot(tmp[-c(1:906),], aes(log(lambda),value,color=coef,acute=coef))+
  geom_line(linewidth=1.2)+
  labs(x="Log Lambda",y="Coefficients")+
  theme_bw()

#交叉验证
lasso.cv = cv.glmnet(x, y,alpha = 1,nfolds =20,family="binomial")
plot(lasso.cv)
lasso.cv$lambda.min #minimum
lasso.cv$lambda.1se #one standard error away
coef(lasso.cv, s = "lambda.1se")
coef(lasso.cv, s = "lambda.min")


lassov <- c("diagnosis","BMI.","Mono.","pCrCl.","LAC.","Ttransfusion","CPB","IL6.","IL10.")


###提取lasso筛选变量后对数据进行标准化
dat.train <- train[,lassov]
dat.test <- test[,lassov]
dat.train = dat.train%>%
  mutate_if(.predicate = is.numeric,
            .funs = min_max_scale)%>%
  as.data.frame()
dat.test = dat.test%>%
  mutate_if(.predicate = is.numeric,
            .funs = min_max_scale)%>%
  as.data.frame()
write.csv(dat.train,file="dat.train.csv")
write.csv(dat.test,file="dat.test.csv")

#train-test 基线表####
train$group <- 1
test$group <- 0
all <- rbind(train,test)
tab2 <- twogrps(all[,-2], gvar = "group")
write.csv(tab2$Table,file="tab4.csv")


###Xgboost####


library(data.table) 
library(xgboost) 
library(Matrix) 
library(caTools)

##在工作路径里重新找到dat.train&test 两个CSV文件，打开，删除第一列

library(readr)
dat_test <- read_csv("C:/Users/79871/Desktop/ALI/R1016/dat.test.csv") 
View(dat_test)

library(readr)
dat_train <- read_csv("C:/Users/79871/Desktop/ALI/R1016/dat.train.csv")
View(dat_train)



# 定义训练集特征和目标变量
X_train <- dat_train[, -1]
y_train <- dat_train[, 1]

# 将特征和目标变量转换为DMatrix格式
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = dat_train$diagnosis)
# 设置XGBoost参数
params <- list(objective = "binary:logistic", eval_metric = "logloss", eta = 0.1, max_depth = 3)
# 设置迭代轮数（树的数量）
nrounds <- 100
# 训练XGBoost模型
xgb_model_final <- xgboost(params = params, data = dtrain, nrounds = nrounds)

# 在训练集上进行预测
train_predictions <- predict(xgb_model_final, newdata = dtrain)
train_predictions1 <- ifelse(train_predictions > 0.5,1,0)

# 计算准确率
accuracy <- mean(train_predictions1 == y_train)
print(paste("训练集准确率:", accuracy))


# 在测试集上进行预测
X_test <- dat_test[, -1]
y_test <- as.factor(dat_test$diagnosis)

dtest <- xgb.DMatrix(data = as.matrix(X_test))
test_predictions <- predict(xgb_model_final, newdata = dtest)
test_predictions1 <- ifelse(test_predictions > 0.5,1,0)

# 计算准确率
accuracy <- mean(test_predictions1 == y_test)
print(paste("测试集准确率:", accuracy))



##调参
# 将数据集转换为trainControl对象
ctrl <- trainControl(
  method = "cv",   # 交叉验证
  number = 10,     # 5折交叉验证
  verboseIter = FALSE)

# 设置参数网格
param_grid <- expand.grid(
  nrounds = c(100, 200), # 迭代轮数（nrounds）
  max_depth = c(3, 6), # 最大树深度（max_depth）
  eta = c(0.1), # 学习率（eta）
  gamma = c(0, 0.1), # 树分裂所需的最小损失减少值
  colsample_bytree = c(0.8), # 特征子采样比例（colsample_bytree）
  min_child_weight = c(1, 3), # 叶子节点的最小权重和（min_child_weight）
  subsample = c(0.8)) # 和样本子采样比例（subsample）



# 使用train()函数进行参数调优
xgb_model <- train(
  x = X_train,
  y = dat_train$diagnosis,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = param_grid)

# 输出最佳参数配置
print(xgb_model$bestTune)


# 设置最优参数
params <- xgb_model$bestTune

# 设置最佳XGBoost参数
params_1 <- list(objective = "binary:logistic", eval_metric = "logloss", 
               eta = 0.1, max_depth = 3, gamma = 0.1,
               colsample_bytree = 0.8,
               min_child_weight = 1,
               subsample = 0.8)


# 训练模型
xgb_model_final <- xgb.train(params = params_1, data = dtrain, nrounds = 100)

# 在训练集上进行预测
train_predictions <- predict(xgb_model_final, newdata = dtrain)
train_predictions1 <- ifelse(train_predictions > 0.5,1,0)

# 计算准确率
accuracy <- mean(train_predictions1 == y_train)
print(paste("训练集准确率:", accuracy))


# 在测试集上进行预测
X_test <- dat_test[, -1]
y_test <- as.factor(dat_test$diagnosis)

dtest <- xgb.DMatrix(data = as.matrix(X_test))
test_predictions <- predict(xgb_model_final, newdata = dtest)
test_predictions1 <- ifelse(test_predictions > 0.5,1,0)

# 计算准确率
accuracy <- mean(test_predictions1 == y_test)
print(paste("测试集准确率:", accuracy))



#混淆矩阵####
library(caret)
train_predictions1 <- as.factor(train_predictions1)
conf_matrix <- confusionMatrix(train_predictions1, train$diagnosis)
conf_matrix

test_predictions1 <- as.factor(test_predictions1)
conf_matrix <- confusionMatrix(test_predictions1, test$diagnosis)
conf_matrix

###ROCtrain####
library(pROC)
train_predictions <- predict(xgb_model_final, newdata = dtrain)

roc.list1 <- roc(train$diagnosis, train_predictions)
roc.list1
ci.auc(roc.list1)
g.list1 <- ggroc(roc.list1, alpha = 1 ,size = 1,
                 legacy.axes = TRUE,color="#1B90CF")
g.list1+theme_update() + 
  annotate(geom = "segment", x = 0, y = 0, xend =1, yend = 1,linetype=2)+
  annotate("text", x = 0.7 , y = 0.20,
           label = "AUC=0.997",colour="#5d6174",size=8)+
  ggtitle("ROC-train")
###ROCtest####
test_predictions <- predict(xgb_model_final, newdata = dtest)
roc.list2 <- roc(test$diagnosis, test_predictions)
roc.list2
ci.auc(roc.list2)
g.list2 <- ggroc(roc.list2, alpha = 1 ,size = 1,
                 legacy.axes = TRUE,color="#1B90CF")
g.list2+theme_update() + #设置主题
  annotate(geom = "segment", x = 0, y = 0, xend =1, yend = 1,linetype=2)+
  annotate("text", x = 0.7 , y = 0.20,
           label = "AUC=0.705",colour="#5d6174",size=8)+
  ggtitle("ROC-test")

###train校准曲线####          
val.prob(train_predictions,          
         as.numeric(train$diagnosis)-1,     
         logistic.cal = FALSE,          
         statloc = F,          
         riskdist = "calibrated",          
         legendloc = c(0.8,0.25))

d <- as.data.frame(train_predictions)
d$dia <-  as.numeric(train$diagnosis)-1
         
d %>%           
  mutate(bins = cut(train_predictions, breaks = seq(0,1,1/5))) %>%           
  group_by(bins) %>%           
  summarise(p_pred = mean(train_predictions), p_true = mean(dia == 1)) %>%           
  ggplot(aes(p_pred, p_true)) +          
  geom_point(color = "red", size = 3) +          
  geom_line(color = "steelblue", linewidth = 1) +          
  geom_abline(intercept = 0, slope = 1, linetype = 2) +          
  xlab("Predicted Probability") + 
  ylab("Actual Probability")

###test校准曲线####
        
val.prob(test_predictions,          
         as.numeric(test$diagnosis)-1,          
         logistic.cal = FALSE,          
         statloc = F,          
         riskdist = "calibrated",          
         legendloc = c(0.8,0.25))

d <- as.data.frame(test_predictions)
d$dia <-  as.numeric(test$diagnosis)-1
        
d %>%           
  mutate(bins = cut(test_predictions, breaks = seq(0,1,1/5))) %>%           
  group_by(bins) %>%           
  summarise(p_pred = mean(test_predictions), p_true = mean(dia == 1)) %>%           
  ggplot(aes(p_pred, p_true)) +          
  geom_point(color = "red", size = 3) +          
  geom_line(color = "steelblue", linewidth = 1) +          
  geom_abline(intercept = 0, slope = 1, linetype = 2) +          
  xlab("Predicted Probability") + 
  ylab("Actual Probability")



###决策曲线####
t1 <- train
t2 <- test

t1$prob <- train_predictions
t2$prob <- test_predictions
#install.packages("dcurves")
library(dcurves)
colnames(t1)
dca(diagnosis~prob, data = t1,
    as_probability = "prob") %>%
  plot(smooth = TRUE) +
  ggplot2::labs(x = "Treatment Threshold Probability")

dca(diagnosis~prob, data = t2,
    as_probability = "prob") %>%
  plot(smooth = TRUE) +
  ggplot2::labs(x = "Treatment Threshold Probability")

## SHAP####
##install.packages("shapviz")
library(shapviz)
shap <- shapviz(xgb_model_final,X_pred =data.matrix(dat_train[,-1]))
sv_waterfall(shap, 
             row_id = 4,
             fill_colors=c("#E31F1CD7", "#246EE3"))

sv_force(shap,
         row_id = 45,
         max_display = 10,
         fill_colors=c("#E31F1CD7", "#246EE3"))

sv_waterfall(shap, 
             row_id = 2,
             fill_colors=c("#E31F1CD7", "#246EE3"))

sv_force(shap,
         row_id = 101,
         max_display = 10,
         fill_colors=c("#E31F1CD7", "#246EE3"))

sv_importance(shap, kind = "beeswarm")

sv_dependence(shap, 
              v = c("BMI.","Mono.","pCrCl.","LAC.","Ttransfusion","CPB","IL6.","IL10."))

sv_dependence(shap, 
              v = "IL6.")
sv_dependence(shap, 
              v = "IL10.")







