library('dplyr')
library('gridExtra')
library('factoextra')
library('ggplot2')
library('knitr')
library('wesanderson')
#install.packages('xgboost')
library('reshape')
library(caTools)
library('cluster')
library(InformationValue)
library('devtools')
#install.packages("randomForest")
library(randomForest)
library('lubridate')
library('xgboost')
library('Matrix')
library('data.table')

df_rf= read.csv("C:/Users/Sameer Kumar/Documents/Experential Learning/Week 2/atomic_alerts_2.csv")
View(df_rf)

###################################################################
# RANDOM FOREST #

items <- c("TP/HIGH")
df_rf = df_rf %>% mutate(df_rf$classification = ifelse(df_rf$classification %in%  items, 1, 0))

set.seed(100)
train <- sample(nrow(df_rf), 0.7*nrow(df_rf), replace = FALSE)
TrainSet <- df_rf[train,]
ValidSet <- df_rf[-train,]
summary(TrainSet)
summary(ValidSet)

model1 <- randomForest(classification ~ ., data = TrainSet, importance = TRUE, na.action = na.exclude)
model1

# Predicting on train set
predTrain <- predict(model1, TrainSet, type = "class")

# Checking classification accuracy
table(predTrain, TrainSet$classification)  


# Predicting on Validation set
predValid <- predict(model1, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$classification)                    
table(predValid,ValidSet$classification)
summary(model1)

##############################################
## XGBOOST-----TP/HIGH##

levels(df_rf[,"classification"])

sparse_matrix <- sparse.model.matrix(classification~.-1, data = df_rf)
head(sparse_matrix)

output_vector = df_rf[, "classification"] == "TP/HIGH"
                    

xg = xgboost(data = data.matrix(sparse_matrix), 
             label = output_vector, max.depth = 4,
             eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = xg)
head(importance)

importanceRaw <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = xg, data = sparse_matrix, label = output_vector)

importanceRaw
# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceClean)

xgb.plot.importance(importance_matrix = importanceRaw)

#-----------------------------------------------------------#
#                       TP/LOW                              #
#-----------------------------------------------------------#

levels(df_rf[,"classification"])

sparse_matrix <- sparse.model.matrix(classification~.-1, data = df_rf)
head(sparse_matrix)

output_vector_low = df_rf[, "classification"] == "TP/LOW"


xg_low = xgboost(data = data.matrix(sparse_matrix), 
                 label = output_vector_low, max.depth = 4,
                 eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

importance_low <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = xg_low)
head(importance_low)

importanceRaw_low <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = xg_low, data = sparse_matrix, label = output_vector_low)


# Cleaning for better display
importanceClean_low <- importanceRaw_low[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceClean_low)

xgb.plot.importance(importance_matrix = importanceRaw_low)



##############################################################
#k-means clustering

set.seed(1001)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_rf$owner_name = as.integer(df_rf$owner_name)
df_rf$classification = as.integer(df_rf$classification)
df_rf$hru = as.integer(df_rf$hru)
df_rf$indicator_pairs = as.integer(df_rf$indicator_pairs)
df_rf$person_type = as.integer(df_rf$person_type)
df_rf$person_status = as.integer(df_rf$person_status)
df_rf$function_group = as.integer(df_rf$function_group)
df_rf$job_function = as.integer(df_rf$job_function)
df_rf$career_band = as.integer(df_rf$career_band)
df_rf$industry_focus_name = as.integer(df_rf$industry_focus_name)
df_rf$Tenure = as.integer(df_rf$Tenure)





#keep <- c("classification", 'score', 'risk_factor')

#df_kc = df[keep]

View(df_rf)

df_normal = as.data.frame(lapply(df_rf, normalize))

df_wss_normal <- (nrow(df_normal)-1)*sum(apply(df_normal,2,var))
for (i in 2:15) 
  df_wss_normal[i] <- sum(kmeans(df_normal, centers=i)$withinss)

plot(1:15, df_wss_normal[1:15], type="b", xlab="Number of Clusters",
     ylab="Within sum of squares",
     main="Elbow plot to find the optimal number of clusters (k) ",
     pch=20, cex=2)



df_cluster_normal = kmeans(scale(df_normal), 6, nstart = 100)  

df_cluster_normal 

cluster_plot = fviz_cluster(df_cluster_normal, data = df_normal)
cluster_plot 


