# HarvardX: PH125.9x Data Science Capstone: Predicting Liver Disease - Final

#load necessary packages for analysis/modeling
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(BiocManager)) install.packages("BiocManager", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(WVPlots)) install.packages("WVPlots", repos = "http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(gridExtra)
library(knitr)
library(reshape2)
library(forecast)
library(janitor)
library(matrixStats)
library(gam)
library(randomForest)
library(ggpubr)
library(WVPlots)
library(MLmetrics)
library(BiocManager)


#download and read in indian_liver_patient from GitHub

url <- "https://raw.githubusercontent.com/tfitzg/CYO-Capstone-Indian-Liver-Disease/master/indian_liver_patient.csv"
liver_whole <- read_csv(url)
download.file(url, "indian_liver_patient.csv")

tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
liver_whole <- read_csv(tmp_filename)
file.remove(tmp_filename)

#review structure
str(liver_whole)



#covert to data frame from tibble for creating data partition
liver_whole <- data.frame(liver_whole)

#for consistency in results, we set the seed.  We then partion the data such that 20% is in "test set" and 80% remains for model building.
set.seed(1, sample.kind = "Rounding") 
test_index <- createDataPartition(y = liver_whole$Dataset, times = 1,
                                  p = 0.2, list = FALSE) 
liver <- liver_whole[-test_index,]
test_set <- liver_whole[test_index,]


#Visualization and Preprocessing

head(liver)

#check for missing
any(is.na(liver))

#locate missing
liver[!complete.cases(liver),] 

#review Alb_Glob Ratio
summary(liver$Albumin_and_Globulin_Ratio)

#replace missing with mean Albumin and Globulin Ratio
liver[is.na(liver)] <- mean(liver$Albumin_and_Globulin_Ratio, na.rm = TRUE) 

#add factor Disease for clarity, remove "Dataset and change Gender to factor"
liver <- liver %>% 
  mutate(Disease = as.factor(ifelse(Dataset == "1", "Disease", "NoDisease")), 
         Gender = as.factor(Gender)) %>% 
  select(-Dataset)

#histograms
TB <- liver %>% ggplot(aes(Total_Bilirubin)) +
  geom_histogram(bins = 30, color = "green") + 
  ggtitle("Total Bilirubin")

DB <- liver %>% ggplot(aes(Direct_Bilirubin)) +
  geom_histogram(bins = 30, color = "blue") + 
  ggtitle("Direct Bilirubin")

AP <- liver %>% ggplot(aes(Alkaline_Phosphotase)) +
  geom_histogram(bins = 30, color = "yellow") + 
  ggtitle("Alkaline Phosphotase")

Al_A <- liver %>% ggplot(aes(Alamine_Aminotransferase)) +
  geom_histogram(bins = 30, color = "orange") + 
  ggtitle("Alamine Aminotransferase")

Asp_A <- liver %>% ggplot(aes(Aspartate_Aminotransferase)) +
  geom_histogram(bins = 30, color = "red") + 
  ggtitle("Aspartate Aminotransferase")

TP <- liver %>% ggplot(aes(Total_Protiens)) +
  geom_histogram(bins = 30, color = "darkblue") + 
  ggtitle("Total_Protiens")

Alb <- liver %>% ggplot(aes(Albumin)) +
  geom_histogram(bins = 30, color = "purple") + 
  ggtitle("Albumin")

A_G_R <- liver %>% ggplot(aes(Albumin_and_Globulin_Ratio)) +
  geom_histogram(bins = 30, color = "pink") + 
  ggtitle("Albumin and Globulin Ratio")

#grid plot of all distributions
grid.arrange(TB, DB, AP, Al_A, Asp_A, TP, Alb, A_G_R, ncol = 2)

#age
liver %>% ggplot(aes(Age)) +
  geom_histogram(bins = 30) + 
  ggtitle("Age")
shapiro.test(liver$Age)

#formula loop for tranformations
columns <- liver[, 3:7]
trans_cols <- sapply(columns, function(t) {
  transf <- log10(t)
})

trans_cols <- as.data.frame(trans_cols)

#histograms of the transformed variables

TB_t <- trans_cols %>% ggplot(aes(Total_Bilirubin)) +
  geom_histogram(bins = 30, color = "green") + 
  ggtitle("Total Bilirubin")

DB_t <- trans_cols %>% ggplot(aes(Direct_Bilirubin)) +
  geom_histogram(bins = 30, color = "blue") + 
  ggtitle("Direct Bilirubin")

AP_t <- trans_cols %>% ggplot(aes(Alkaline_Phosphotase)) +
  geom_histogram(bins = 30, color = "yellow") + 
  ggtitle("Alkaline Phosphotase")

Al_A_t <- trans_cols %>% ggplot(aes(Alamine_Aminotransferase)) +
  geom_histogram(bins = 30, color = "orange") + 
  ggtitle("Alamine Aminotransferase")

Asp_A_t <- trans_cols %>% ggplot(aes(Aspartate_Aminotransferase)) +
  geom_histogram(bins = 30, color = "red") + 
  ggtitle("Aspartate Aminotransferase")

grid.arrange(TB_t, DB_t, AP_t, Al_A_t, Asp_A_t, ncol = 2)

#qqplots do not show normality even when transformed (but still better)
q1 <- ggqqplot(trans_cols$Aspartate_Aminotransferase, title = "Transformed Asp_Aminotransferase")
qt1 <- ggqqplot(liver$Aspartate_Aminotransferase, title = "Original Asp_Aminotransferase")

q2 <- ggqqplot(trans_cols$Total_Bilirubin, title = "Transformed Total_Bilirubin")
qt2 <- ggqqplot(liver$Total_Bilirubin, title = "Original Total_Bilirubin")

grid.arrange(q2, qt2, ncol = 2)
grid.arrange(q1, qt1, ncol = 2)


#new_liver is the liver dataframe updated with transformed columns
new_liver <- liver %>% mutate(Total_Bilirubin = trans_cols$Total_Bilirubin,
                              Direct_Bilirubin = trans_cols$Direct_Bilirubin,
                              Alkaline_Phosphotase = trans_cols$Alkaline_Phosphotase,
                              Alamine_Aminotransferase = trans_cols$Alamine_Aminotransferase,
                              Aspartate_Aminotransferase = trans_cols$Aspartate_Aminotransferase)



#count by disease status
p1 <- new_liver %>%
  group_by(Disease) %>%
  summarize(count = n()) %>%
  ggplot(aes(Disease, count)) +
  geom_bar(stat = "identity") +
  ggtitle("Count by Disease Status")
#note need to deal with prevalence

#Count of records by gender
p2 <- new_liver %>%
  group_by(Gender) %>%
  summarize(count = n()) %>%
  ggplot(aes(Gender, count)) +
  geom_bar(stat = "identity") +
  ggtitle("Count by Gender")

grid.arrange(p1, p2, ncol = 2)


#Disease/Sex
new_liver %>%
  group_by(Gender, Disease) %>%
  summarize(count = n()) %>%
  ggplot(aes(Disease, count)) +
  geom_bar(stat = "identity", aes(fill = Gender))

#table for disease records only, breakdown by sex
has_disease_female <- round(mean((new_liver$Gender == "Female" & new_liver$Disease == "Disease")/(new_liver$Disease == "Disease"), na.rm = TRUE), 3)
has_disease_male <- round(mean((new_liver$Gender == "Male" & new_liver$Disease == "Disease")/(new_liver$Disease == "Disease"), na.rm = TRUE), 3)
data.table("Disease" = "Has Disease", "Female" = has_disease_female, "Male" = has_disease_male) %>% knitr:: kable()

#show that both Genders more likely to have disease the not in this dataset
new_liver %>%
  group_by(Gender, Disease) %>%
  summarize(count = n()) %>%
  ggplot(aes(Gender, count)) +
  geom_bar(stat = "identity", aes(fill = Disease))

#Disease count by Gender
new_liver %>%
  group_by(Gender, Disease) %>%
  summarize(Frequency = n()) %>%
  knitr::kable()

#table with proportions
prop_female_wdisease <- round(mean((new_liver$Gender == "Female" & new_liver$Disease == "Disease")/((new_liver$Gender == "Female" & new_liver$Disease == "Disease")+ (new_liver$Gender == "Female" & new_liver$Disease == "NoDisease")), na.rm = TRUE), 3)
prop_male_wdisease <- round(mean((new_liver$Gender == "Male" & new_liver$Disease == "Disease")/((new_liver$Gender == "Male" & new_liver$Disease == "Disease")+ (new_liver$Gender == "Male" & new_liver$Disease == "NoDisease")), na.rm = TRUE), 3)

data.table("Gender" = c("Female", "Male"), "Proportion_with_Disease" = c(prop_female_wdisease, prop_male_wdisease)) %>% knitr:: kable()


#Age/Sex and Disease status
new_liver %>%
  group_by(Gender, Disease) %>%
  mutate(Gender_status = paste(Gender, Disease, sep = "-")) %>%
  ggplot(aes(Gender_status, Age)) +
  geom_boxplot(aes(fill = Disease)) +
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90))

#stratify by Age
new_liver <- new_liver %>% mutate(age_bin = as.factor(case_when(Age < 20 ~ '1',
                                                                Age >= 20 & Age <= 40 ~ '2',
                                                                Age >= 40 & Age <= 60 ~ '3',
                                                                Age >= 60 ~ '4')))

new_liver %>%
  group_by(age_bin, Disease) %>%
  summarize(count = n()) %>%
  ggplot(aes(age_bin, count)) +
  geom_bar(stat = "identity", aes(fill = Disease))

new_liver %>% 
  group_by(age_bin, Disease) %>%
  summarize(count = n()) %>%
  spread(Disease, count) %>%
  mutate(Disease_proportion = Disease/(Disease + NoDisease)) %>%
  knitr::kable()

#correlations
my_data <- new_liver[, c(3:10)]
correlations <- round(cor(my_data, use = "pairwise"), 3) #use pairwise bc of 4 missing values
print(correlations) #consider 0.7 to be strong correlation

melted_cor <- melt(correlations)
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "black", high = "red", mid = "white") +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  ggtitle("Correlation Matrix of Chemical Compounds")

#Example of linear relationships stratified by Age bin or Gender
#Bilirubin
new_liver %>%
  ggplot(aes(Total_Bilirubin, Direct_Bilirubin)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ age_bin) +
  ggtitle("Relationship Across Age Bin")

#Aminotransferases
new_liver %>%
  ggplot(aes(Alamine_Aminotransferase, Aspartate_Aminotransferase)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ Gender) +
  ggtitle("Relationship Across Gender")


#boxplots of Disease status vs features 
b_1 <- new_liver %>%
  ggplot(aes(Disease, Total_Bilirubin)) +
  geom_boxplot(aes(fill = Disease)) 

b_1_2 <- new_liver %>%
  ggplot(aes(Disease, Direct_Bilirubin)) +
  geom_boxplot(aes(fill = Disease)) 

b_2 <- new_liver %>%
  ggplot(aes(Disease, Alkaline_Phosphotase)) +
  geom_boxplot(aes(fill = Disease)) 

b_3_1 <- new_liver %>%
  ggplot(aes(Disease, Alamine_Aminotransferase)) +
  geom_boxplot(aes(fill = Disease)) 

b_3 <- new_liver %>%
  ggplot(aes(Disease, Aspartate_Aminotransferase)) +
  geom_boxplot(aes(fill = Disease)) 

b_4 <- new_liver %>%
  ggplot(aes(Disease, Albumin)) +
  geom_boxplot(aes(fill = Disease)) 

b_5 <- new_liver %>%
  ggplot(aes(Disease, Total_Protiens)) +
  geom_boxplot(aes(fill = Disease)) 

b_6 <- new_liver %>%
  ggplot(aes(Disease, Albumin_and_Globulin_Ratio)) +
  geom_boxplot(aes(fill = Disease)) 

b_7 <- new_liver %>%
  ggplot(aes(Disease, Age)) +
  geom_boxplot(aes(fill = Disease))

grid.arrange(b_1, b_1_2, b_2, b_3_1, b_3, b_4, b_5, b_6, b_7, ncol = 3)

#Gender - Chi-sq
freq <- new_liver %>%
  group_by(Gender, Disease) %>%
  summarize(Frequency = n())

two_by_two <- data.frame(Disease = c("Disease", "NoDisease"),
                         female = c(freq$Frequency[1], freq$Frequency[2]),
                         male = c(freq$Frequency[3], freq$Frequency[4]))
chisq_test <- two_by_two %>% select(-Disease) %>% chisq.test()
knitr::kable(two_by_two)
cat("p value =", chisq_test$p.value) 

#age_bin - Chi-sq
age_bin <- new_liver %>%
  group_by(age_bin, Disease) %>%
  summarize(count = n())

age_table <- data.frame(Disease = c("Disease", "NoDisease"),
                        age_1 = c(age_bin$count[1], age_bin$count[1]),
                        age_2 = c(age_bin$count[3], age_bin$count[4]),
                        age_3 = c(age_bin$count[5], age_bin$count[6]),
                        age_4 = c(age_bin$count[7], age_bin$count[8]))
chisq_test_age <- age_table %>% select(-Disease) %>% chisq.test()
knitr::kable(age_table)
cat("p value =", chisq_test_age$p.value)



#train set and test set preprocessing/scaling and modeling using train set

#check test set for NAs
any(is.na(test_set)) 

#choose features and assign to train_set
train_set <- new_liver %>%
  select(Total_Bilirubin, Alkaline_Phosphotase, Aspartate_Aminotransferase, Albumin, 
         Gender, Age, Disease)

#select same features for test_set
test_set <- test_set %>%
  mutate(Disease = as.factor(ifelse(Dataset == "1", "Disease", "NoDisease")), 
         Gender = as.factor(Gender)) %>%
  select(Total_Bilirubin, Alkaline_Phosphotase, Aspartate_Aminotransferase, Albumin, 
         Gender, Age, Disease, -Dataset)


#we need to apply log10 transformation to the highly skewed features in test_set which we did for train_set previously
test_set <- test_set %>% mutate(Total_Bilirubin = log10(Total_Bilirubin), 
                                Alkaline_Phosphotase = log10(Alkaline_Phosphotase), 
                                Aspartate_Aminotransferase = log10(Aspartate_Aminotransferase))

#convert features to matrix and Disease response (y) to factor vector
train_x <- train_set %>% 
  select(-Disease) %>% 
  data.matrix()
train_y <- train_set$Disease

#center and scale the matrix train_x
train_x_centered <- sweep(train_x, 2, colMeans(train_x))
train_x_scaled <- sweep(train_x_centered, 2, colSds(train_x), FUN = "/") 


#apply scaling to test_set, using data from train_set since test is "unknown"
#first convert test set to matrix and response vector
test_x <- test_set %>% 
  select(-Disease) %>% 
  data.matrix()
test_y <- test_set$Disease

#obtain train set column means and standard deviations
means <- colMeans(train_x)
std_devs <- colSds(train_x)

test_x_centered <- sweep(test_x, 2, means)
test_x_scaled <- sweep(test_x_centered, 2, std_devs, FUN = "/")

#Our final training and test feature matrices
train_x <- train_x_scaled
test_x <- test_x_scaled
#train_y and test_y are just the Disease response vectors previously defined


# Modeling

#logistic regression
#set seed for conistency in results
set.seed(1, sample.kind = "Rounding")

#train logistic regression model
train_glm <- train(train_x, train_y, method = "glm", family = "binomial")

#confusion matrix for train cross validation
cm_glm <- confusionMatrix(train_glm, "none")
print(cm_glm)
#precision of the glm model
Precision_glm <- cm_glm$table[1,1]/(cm_glm$table[1,1] + cm_glm$table[1,2])
cat("Precision =", Precision_glm)

#model summary - could simplify model
summary(train_glm$finalModel)

varImp(train_glm, scale = FALSE)


#Random Forest
trctrl    = trainControl(summaryFunction = prSummary, classProbs = TRUE)

set.seed(1, sample.kind = "Rounding")
train_rf_1 <- train(train_x, train_y, 
                    method = "rf",
                    trControl=trctrl,
                    metric = "Precision",
                    tuneGrid = data.frame(mtry = seq(1, 5, 1)))
print(train_rf_1)

#now manually select nodesize, setting mtry to 4
set.seed(1, sample.kind = "Rounding")
nodesize <- seq(2, 20, 2)
Precision <- sapply(nodesize, function(ns){
  train(train_x, train_y, method = "rf", 
        trControl=trctrl,
        metric = "Precision", 
        tuneGrid = data.frame(mtry = 4),
        nodesize = ns)$results$Precision
})
qplot(nodesize, Precision)


#build model (mtry = 4, nodesize = 12)
set.seed(1, sample.kind = "Rounding")
train_rf <- train(train_x, train_y, 
                  method = "rf",
                  trControl=trctrl,
                  metric = "Precision",
                  tuneGrid = data.frame(mtry = 4),
                  nodesize = 12)

#confusion matrix for train cross validation
cm_rf <- confusionMatrix(train_rf, "none")
print(cm_rf)
#calculate precision
Precision_rf <- cm_rf$table[1,1]/(cm_rf$table[1,1] + cm_rf$table[1,2])
cat("Precision =", Precision_rf)


#KNN
set.seed(1, sample.kind = "Rounding")
train_knn_1 <- train(train_x, train_y,
                   method = "knn",
                   trControl = trctrl,
                   metric = "Precision",
                   tuneGrid = data.frame(k = seq(3, 100, 5)))
ggplot(train_knn_1, highlight = TRUE)
train_knn_1$bestTune

#visualization of tradeoff between precision and recall
train_knn_1$results %>% 
  ggplot(aes(Recall, Precision)) +
  geom_line() +
  geom_label(aes(label = k)) +
  xlim(0.73, 0.95) +
  ylim(0.68, 0.78)

#KNN, k = 3
set.seed(1, sample.kind = "Rounding")
train_knn <- train(train_x, train_y,
                     method = "knn",
                     tuneGrid = data.frame(k = 3))  

#confusion matrix for knn
cm_knn <- confusionMatrix(train_knn, "none")
print(cm_knn)
#precision for knn
Precision_knn <- cm_knn$table[1,1]/(cm_knn$table[1,1] + cm_knn$table[1,2])
cat("Precision =", Precision_knn)


#Loess
set.seed(1, sample.kind = "Rounding")
grid <- expand.grid(span = seq(0.05, 0.35, len = 10), degree = 1) 
train_loess_1 <- train(train_x, train_y,
                     method = "gamLoess",
                     tuneGrid = grid,
                     trControl = trctrl,
                     metric = "Precision")
ggplot(train_loess_1, highlight = TRUE)

#span = 0.05 had highest precision
train_loess <- train(train_x, train_y,
                    method = "gamLoess",
                    span = 0.05)

#confusion matrix
cm_loess <- confusionMatrix(train_loess, "none")
print(cm_loess)
#Precision
Precision_loess <- cm_loess$table[1,1]/(cm_loess$table[1,1] + cm_loess$table[1,2])
cat("Precision =", Precision_loess)


#K-means clustering
set.seed(1, sample.kind = "Rounding")
k <- kmeans(train_x, centers = 2, n = 25)
print(k)

#create data table to analyze results of clustering on train set
train_x_df <- data.frame(cbind(k$cluster, train_x))
cluster_table <- cbind(Disease = train_y, train_x_df)
cluster_table$cluster <- as.character(k$cluster)

cluster_table <- cluster_table %>% 
  mutate(cluster_result = case_when(Disease == "Disease" & cluster == 2 ~ "True Positive", 
                                    Disease == "Disease" & cluster == 1 ~ "False Negative",
                                    Disease == "NoDisease" & cluster == 1 ~ "True Negative",
                                    Disease == "NoDisease" & cluster == 2 ~ "False Positive"))
cluster_precision <- (sum(cluster_table$cluster_result == "True Positive"))/
  (sum((cluster_table$cluster_result == "True Positive")
       + (cluster_table$cluster_result == "False Positive")))

cluster_accuracy <- (sum((cluster_table$cluster_result == "True Positive") + 
                           (cluster_table$cluster_result == "True Negative")))/
  (sum((cluster_table$cluster_result == "True Positive") + 
         (cluster_table$cluster_result == "False Positive")
       + (cluster_table$cluster_result == "True Negative") + 
         (cluster_table$cluster_result == "False Negative")))

cat("Precision =", cluster_precision)
cat("Accuracy =", cluster_accuracy)

#clustering visualizations
cluster_table %>%
  ggplot(aes(Aspartate_Aminotransferase, Albumin, colour = cluster)) +
  geom_point(aes(shape = Disease), size = 2)

cluster_table %>%
  ggplot(aes(Total_Bilirubin, Alkaline_Phosphotase, colour = cluster)) +
  geom_point(aes(shape = Disease), size = 2)


#Final model and evaluation against test set

#predict test set results using k-means centers from train set
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  #calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 2, "Disease", "NoDisease") 
confusionMatrix(data = factor(kmeans_preds), reference = test_y, mode = "everything")

