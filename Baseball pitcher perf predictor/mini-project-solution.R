

library(plotly)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(Metrics)
library(glmnetnet)

setwd("C:/Users/mmssw/Documents/C5555_ML")
pitching = read.csv("2022 MLB Player Stats - Pitching.csv", encoding = "UTF-8")
pitching$Name <- gsub("\xa0", " ", pitching$Name)
summary(pitching)

#AVG
new_pitching=pitching[,-c(4,5)]
new_pitching %>% group_by(Name) %>% 
  mutate_at(colnames(new_pitching)[1], min) %>%
  mutate_at(colnames(new_pitching)[-c(1,2)], list(mean)) %>%
  group_by(Name) %>% 
  slice(1) %>% 
  arrange(Rk) -> avg_pitching

avg_pitching = as.data.frame(avg_pitching)

#select any one row from each group by name
onename=pitching %>% group_by(Name) %>% slice(1)  %>% arrange(Rk)
ggplot(onename, aes(SO, W.L.)) + geom_point() + geom_smooth(method = "lm")



#-----------data visualization-----------------
ggplot(pitching, aes(Age)) + geom_density()
plot_ly(pitching, x = ~Age, type="box", name="Age") %>%
add_trace(pitching, x = ~SO, name = 'SO') %>%
add_trace(pitching, x = ~ER, name = 'ER') %>%
add_trace(pitching, x = ~IP, name = 'IP')



#eda
boxplot(fivenum(pitching$Age))
boxplot(pitching$Age, horizontal = TRUE, xaxt = "n")
axis(side = 1, at = fivenum(pitching$Age), labels = TRUE)
plot(pitching)

#eda on age
hist(pitching$Age, breaks=21)

freq = table(pitching$Age)
freq = as.data.frame(freq)

x= as.numeric(as.vector(freq[,1]))
y= as.vector(freq[,2])
x
y
plot(x,y)
lines(x,y, type = "b")

#ERA vs SO
plot(pitching$ER, pitching$SO)
plot_ly(x=onename$ER, y=onename$SO, type="scatter") %>% layout(yaxis = list(title = 'SO'))
ggplot(onename, aes(ER, SO))+geom_point() + geom_smooth()

plot(pitching$IP, pitching$SO)
plot(pitching$BK, pitching$SO)
plot(pitching$FIP, pitching$SO, xlim = c(0,10))

#SO VS WL 
p=plot_ly(x=pitching$SO, y=pitching$W.L., type="scatter")
p <- layout(p, yaxis = list(title = 'W-L%'))
p


#Rk vs each attribute
plot_ly(x=pitching$Rk, y=pitching$SO, xlim = c(0,10), type="scatter")
lp = c()
for(col in colnames(pitching[ , 9:35])){
  print(col)
  lp[[col]] = ggplot(pitching, aes(Rk, col)) + geom_point()
  
}


grid.arrange(grobs = lp, ncol=5)


#more than one team
pitching %>% group_by(Name) %>% 
  summarise(rank = min(Rk), count=n()) %>% arrange(rank) -> player.teams

count_more_1_ply= player.teams %>% filter(count>1)
barplot(count_more_1_ply$count, width = 8, space = 3)
count_more_1_ply

plot_ly(y=count_more_1_ply$count, type = "bar",  text = count_more_1_ply$Name)
p=plot_ly(x=pitching$ERA, y=pitching$SO, type="scatter")
p <- layout(p, yaxis = list(title = 'SO'))
p

# Load the caret package
install.packages("caret")
install.packages("rtools")
require(caret)
install.packages("vctrs")


#---------------------target columns creation --------------

avg_pitching = filter(avg_pitching, IP > 9)
avg_pitching$score = (9*(avg_pitching$SO/avg_pitching$IP)) /(avg_pitching$FIP + avg_pitching$ERA)
avg_pitching %>% select(Rk, SO, IP, FIP, ERA) %>% filter(Rk %in% c(250,6))


#feature_cleaning
#ATTRIBUTES IN CONTROL OF THE PITCHER(RESPONSE VARIABLES) VS NOT IN CONTROL(EXPLANATORY VARIABLES)
avg_pitching_cleaned = avg_pitching[, c("Age", "G","GS","GF","CG","IBB","IP","BF","SHO","SV","H","R","ER","BK","WP","score")]

#train test split
#shuffling the data
set.seed(233)
avg_pitching_cleaned = avg_pitching_cleaned[sample(nrow(avg_pitching_cleaned)),]
trainIndex <- createDataPartition(avg_pitching_cleaned$score, p = 0.66, list = FALSE)
avg_pitching_cleaned_train <- as.data.frame(avg_pitching_cleaned[trainIndex,])
avg_pitching_cleaned_test <- as.data.frame(avg_pitching_cleaned[-trainIndex,])


#data_scaling
standardize <- function(x, mean, sd) {
  return((x - mean)/sd)
}

means <- colMeans(avg_pitching_cleaned_train)
sds <- apply(avg_pitching_cleaned_train, 2, sd)
#norm_avg_pitching_cleaned_train = as.data.frame(avg_pitching_cleaned_train %>% 
 # mutate_at(colnames(avg_pitching_cleaned_train), list(scale)))


norm_avg_pitching_cleaned_train = as.data.frame(Map(standardize, avg_pitching_cleaned_train, mean=means, sd=sds))
#

#norm_avg_pitching_cleaned_test = avg_pitching_cleaned_test %>% 
  #mutate_at(colnames(avg_pitching_cleaned_test), ~standardize(.,mean=means,sd=sds))

norm_avg_pitching_cleaned_test = as.data.frame(Map(standardize, avg_pitching_cleaned_test, mean=means, sd=sds))
(25 - means[7])/sds[7]

X_train = norm_avg_pitching_cleaned_train[,-which(names(norm_avg_pitching_cleaned_train) == "score")]
Y_train = norm_avg_pitching_cleaned_train$score
boxplot(X_train)
X_test = norm_avg_pitching_cleaned_test[,-which(names(norm_avg_pitching_cleaned_test) == "score")]
Y_test = norm_avg_pitching_cleaned_test$score

Y_plot_train = data.frame(x = 1:nrow(avg_pitching_cleaned_train), y = Y_train)
Y_plot_test = data.frame(x = 1:nrow(avg_pitching_cleaned_test), y = Y_test)

ggplot(Y_plot_train, aes(x,y)) + geom_point() + geom_smooth()
ggplot(Y_plot_test, aes(x,y)) + geom_point() + geom_smooth()

ggplot(norm_avg_pitching_cleaned_train, aes(Age)) + geom_density()
ggplot(avg_pitching_cleaned_train, aes(Age)) + geom_density()
ggplot(norm_avg_pitching_cleaned_test, aes(score)) + geom_density()
ggplot(avg_pitching_cleaned_test, aes(score)) + geom_density()

#distributions
ggplot(avg_pitching_cleaned_train, aes(BF)) + geom_density()
ggplot(norm_avg_pitching_cleaned_train, aes(BF)) + geom_density()

plot_ly(x = norm_avg_pitching_cleaned_train$score, type="box", name="train") %>%
  add_trace(x = norm_avg_pitching_cleaned_test$score, name = 'test')

summary(avg_pitching_cleaned_train$score)
summary(avg_pitching_cleaned_test$score)

#------feature selection------
library(stats)
library(utils)
pairs(norm_avg_pitching_cleaned_train)
m3 = lm(score ~Age+G+GS+GF+CG+IBB+IP+BF+SHO+SV+H+R+ER+BK+WP, data = norm_avg_pitching_cleaned_train)

predict(m3 , newdata = X_test)
rmse(Y_test, predict(m3 , newdata = X_test))
summary(m3)
anova(m3)$`F value`


#f-test
#global

totalss <- sum((norm_avg_pitching_cleaned_train$score - mean(norm_avg_pitching_cleaned_train$score))^2)

regss <- sum((fitted(m3) - mean(norm_avg_pitching_cleaned_train$score))^2)
resiss <- sum((norm_avg_pitching_cleaned_train$score-fitted(m3))^2)

fstatistic <- (regss/15)/(resiss/413)

pvalue <- 1-pf(fstatistic, df1=15, df2=413)

R2 <- regss/totalss
#passed

#local
summary(m3)$coefficients
selected_var_ttest = which(abs(summary(m3)$coefficients[,3])>=qt(1-0.025,413))
var_ttest=colnames(norm_avg_pitching_cleaned_train)[selected_var_ttest]
selected_var_ftest = which(anova(m3)$`F value`>=qf(0.95,15,413))
var_ftest=colnames(norm_avg_pitching_cleaned_train)[selected_var_ftest]
var_ftest
var_ttest


#--------------------------------model----------------------------

cor(avg_pitching_cleaned_train)
ggplot(norm_avg_pitching_cleaned_train, aes(R, score))+geom_point() + geom_smooth(method="lm")


m_linear_all = lm(score ~., data = norm_avg_pitching_cleaned_train)

#f-test_features
#with interaction
aR_int_ftest = c()
models_int_ftest = list()
rm_int_ftest = list()
for(i in seq(1:3)){
  m = lm(score ~ polym(Age,G,GF,IBB,BF,SHO,SV,H,R,degree = i, raw = T),data = norm_avg_pitching_cleaned_train)
  models_int_ftest = append(models_int_ftest,m)
  aR_int_ftest = append(aR_int_ftest, summary(m)$adj.r.squared)
  rm_int_ftest = append(rm_int_ftest,rmse(Y_test, predict(m , newdata = X_test[,var_ftest])))
  print(paste("Order",i,":",summary(m)$adj.r.squared))
}
plot_ly(x = (1:length(aR_int_ftest)), y = aR_int_ftest, type = "scatter", mode ="lines") %>% 
  add_trace(y = rm_int_ftest, type="scatter", mode="lines")

#without interaction
aR_nint_ftest = c()
models_nint_ftest = list()
rm_nint_ftest = list()

for(i in seq(1:5)){
  m = lm(as.formula(
    paste('score ~',paste('poly(',var_ftest,',',i,', raw = T)',collapse = ' + ')))
    ,data = norm_avg_pitching_cleaned_train)
  models_nint_ftest = append(models_nint_ftest,m)
  rm_nint_ftest = append(rm_nint_ftest, rmse(Y_test, predict(m, newdata = X_test[,var_ftest])))
  aR_nint_ftest = append(aR_nint_ftest, summary(m)$adj.r.squared)
  print(paste("Order",i,":",summary(m)$adj.r.squared))
}

plot_ly(x = (1:length(aR_int_ftest)), y = aR_int_ftest, type = "scatter", mode ="lines",name = "aR_int") %>% 
  add_trace(y = rm_int_ftest, type="scatter", mode="lines", name="rmse_int") %>% 
  add_trace(x = (1:length(aR_nint_ftest)) ,y = aR_nint_ftest, type = "scatter", mode ="lines",name = "aR_nint") %>% 
  add_trace(x = (1:length(aR_nint_ftest)),y = rm_nint_ftest, type="scatter", mode="lines", name="rmse_nint")

#best f_test model is degree2 without interaction

#t-test_features
#with interaction
aR_int_ttest = c()
models_int_ttest = list()
rm_int_ttest = list()
for(i in seq(1:3)){
  m = lm(score ~ polym(GS,CG,IP,H,degree = i, raw = T),data = norm_avg_pitching_cleaned_train)
  models_int_ttest = append(models_int_ttest,m)
  aR_int_ttest = append(aR_int_ttest, summary(m)$adj.r.squared)
  rm_int_ttest = append(rm_int_ttest,rmse(Y_test, predict(m , newdata = X_test[,var_ttest])))
  print(paste("Order",i,":",summary(m)$adj.r.squared))
}
plot_ly(x = (1:length(aR_int_ttest)), y = aR_int_ttest, type = "scatter", mode ="lines") %>% 
  add_trace(y = rm_int_ttest, type="scatter", mode="lines")

#without interaction
aR_nint_ttest = c()
models_nint_ttest = list()
rm_nint_ttest = list()

for(i in seq(1:5)){
  m = lm(as.formula(
    paste('score ~',paste('poly(',var_ttest,',',i,', raw = T)',collapse = ' + ')))
    ,data = norm_avg_pitching_cleaned_train)
  models_nint_ttest = append(models_nint_ttest,m)
  rm_nint_ttest = append(rm_nint_ttest, rmse(Y_test, predict(m, newdata = X_test[,var_ttest])))
  aR_nint_ttest = append(aR_nint_ttest, summary(m)$adj.r.squared)
  print(paste("Order",i,":",summary(m)$adj.r.squared))
}

plot_ly(x = (1:length(aR_int_ttest)), y = aR_int_ttest, type = "scatter", mode ="lines",name = "aR_int_ttest") %>% 
  add_trace(y = rm_int_ttest, type="scatter", mode="lines", name="rmse_nint_ttest") %>% 
  add_trace(x = (1:length(aR_nint_ttest)) ,y = aR_nint_ttest, type = "scatter", mode ="lines",name = "aR_nint_ttest") %>% 
  add_trace(x = (1:length(aR_nint_ttest)),y = rm_nint_ttest, type="scatter", mode="lines", name="rmse_nint_ttest")

#best t_test model is degree2 without interaction

#allfeatures
#with interaction

aR_int_all = c()
models_int_all = list()
rm_int_all = list()
for(i in seq(1:2)){
  m = lm(score ~ polym(Age,G,GS,GF,CG,IBB,IP,BF,SHO,SV,H,R,ER,BK,WP,degree = i, raw = T),data = norm_avg_pitching_cleaned_train)
  models_int_all = append(models_int_all,m)
  aR_int_all = append(aR_int_all, summary(m)$adj.r.squared)
  rm_int_all = append(rm_int_all,rmse(Y_test, predict(m , newdata = X_test)))
  print(paste("Order",i,":",summary(m)$adj.r.squared))
}
plot_ly(x = (1:length(aR_int_all)), y = aR_int_all, type = "scatter", mode ="lines") %>% 
  add_trace(y = rm_int_all, type="scatter", mode="lines")

#without interaction
aR_nint_all = c()
models_nint_all = list()
rm_nint_all = list()

for(i in seq(1:5)){
  m = lm(as.formula(
    paste('score ~',paste('poly(',colnames(norm_avg_pitching_cleaned_train[-16]),',',i,', raw = T)',collapse = ' + ')))
    ,data = norm_avg_pitching_cleaned_train)
  models_nint_all = append(models_nint_all,m)
  rm_nint_all = append(rm_nint_all, rmse(Y_test, predict(m, newdata = X_test)))
  aR_nint_all = append(aR_nint_all, summary(m)$adj.r.squared)
  print(paste("Order",i,":",summary(m)$adj.r.squared))
}
plot_ly(x = (1:length(aR_int_all)), y = aR_int_all, type = "scatter", mode ="lines",name = "aR_int_all") %>% 
  add_trace(y = rm_int_all, type="scatter", mode="lines", name="rmse_int_all") %>% 
  add_trace(x = (1:length(aR_nint_all)) ,y = aR_nint_all, type = "scatter", mode ="lines",name = "aR_nint_all") %>% 
  add_trace(x = (1:length(rm_nint_all)),y = rm_nint_all, type="scatter", mode="lines", name="rm_nint_all")


#rmse_all
#plot_ly(x = (1:length(rm_int_all)), y = rm_int_all, type = "scatter", mode ="lines",name = "rm_int_all") %>% 
  #add_trace(x = (1:length(rm_nint_all)) ,y = rm_nint_all, type="scatter", mode="lines", name="rm_nint_all") %>% 
  #add_trace(x = (1:length(rm_int_ftest)) ,y = rm_int_ftest, type = "scatter", mode ="lines",name = "rm_int_ftest") %>% 
  #add_trace(x = (1:length(rm_nint_ftest)),y = rm_nint_ftest, type="scatter", mode="lines", name="rm_nint_ftest") %>% 
  #add_trace(x = (1:length(rm_int_ttest)),y = rm_int_ttest, type="scatter", mode="lines", name="rm_int_ttest") %>% 
  #add_trace(x = (1:length(rm_nint_ttest)),y = rm_nint_ttest, type="scatter", mode="lines", name="rm_nint_ttest")

rmse_all=c(rm_int_all,rm_nint_all,rm_int_ftest,rm_nint_ftest,rm_int_ttest,rm_nint_ttest)
rmse_all = as.data.frame(unlist(rmse_all))


plot_ly(x = (1:nrow(rmse_all)), y = rmse_all$`unlist(rmse_all)`,type = "scatter", mode ="lines")
#------------------regularization-----------------


m_reg_ftest_ridge = cv.glmnet(as.matrix(X_train[,var_ftest]),as.matrix(Y_train),alpha=0)
m_reg_ftest_lasso = cv.glmnet(as.matrix(X_train[,var_ftest]),as.matrix(Y_train),alpha=1)
rmse(Y_test, predict(m_reg_ftest_ridge ,as.matrix(X_test[,var_ftest]), s = "lambda.min"))
rmse(Y_test, predict(m_reg_ftest_lasso ,as.matrix(X_test[,var_ftest]), s = "lambda.min"))

m_reg_ttest_ridge = cv.glmnet(as.matrix(X_train[,var_ttest]),as.matrix(Y_train),alpha=0)
m_reg_ttest_lasso = cv.glmnet(as.matrix(X_train[,var_ttest]),as.matrix(Y_train),alpha=1)
rmse(Y_test, predict(m_reg_ttest_ridge ,as.matrix(X_test[,var_ttest]), s = "lambda.min"))
rmse(Y_test, predict(m_reg_ttest_lasso ,as.matrix(X_test[,var_ttest]), s = "lambda.min"))

m_reg_all_ridge = cv.glmnet(as.matrix(X_train),as.matrix(Y_train),alpha=0)
m_reg_all_lasso = cv.glmnet(as.matrix(X_train),as.matrix(Y_train),alpha=1)
rmse(Y_test, predict(m_reg_all_ridge ,as.matrix(X_test), s = "lambda.min"))
rmse(Y_test, predict(m_reg_all_lasso ,as.matrix(X_test), s = "lambda.min"))

#-----best_model--------

bm = lm(as.formula(
  paste('score ~',paste('poly(',var_ftest,',',2,', raw = T)',collapse = ' + ')))
  ,data = norm_avg_pitching_cleaned_train)
summary(bm)
paste("RMSE of the best model:",rmse(Y_test, predict(bm, newdata = X_test[,var_ftest])))
predict(bm, newdata = X_test[,var_ftest])
rmse_all
summary(bm)
ggplot(rmse_all, aes((1:nrow(rmse_all)),rmse_all$`unlist(rmse_all)`)) + geom_point()

#f-test
#global

totalss <- sum((norm_avg_pitching_cleaned_train$score - mean(norm_avg_pitching_cleaned_train$score))^2)

regss <- sum((fitted(bm) - mean(norm_avg_pitching_cleaned_train$score))^2)
resiss <- sum((norm_avg_pitching_cleaned_train$score-fitted(bm))^2)

fstatistic <- (regss/17)/(resiss/411)

pvalue <- 1-pf(fstatistic, df1=17, df2=411)

R2 <- regss/totalss

if(qf(0.95,17,411)<fstatistic){
  print("Model is significant")
}
#passed

write.csv(norm_avg_pitching_cleaned_train,"train_set.csv",row.names = FALSE)
write.csv(norm_avg_pitching_cleaned_test,"test_set.csv", row.names = FALSE)
