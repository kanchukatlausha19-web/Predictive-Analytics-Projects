install.packages(c(
  "ggplot2",
  "dplyr",
  "reshape2",
  "gridExtra",
  "caret",
  "randomForest",
  "rpart",
  "rpart.plot",
  "corrplot",
  "pROC"
))

library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(corrplot)
library(pROC)


library(dplyr)


library(ggplot2)
library(reshape2)
library(gridExtra)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(corrplot)
library(pROC)

data <- read.csv("/Users/keerunaidu/Desktop/PA Notes/UCI_Credit_Card.csv")

dim(data)
head(data)
str(data)

colnames(data)[colnames(data) == "default.payment.next.month"] <- "default"

data <- data[, !names(data) %in% "ID"]



data$SEX <- factor(data$SEX,
                   levels = c(1,2),
                   labels = c("Male","Female"))

data$EDUCATION <- factor(data$EDUCATION,
                         levels = c(1,2,3,4,5,6),
                         labels = c("Graduate","University","High School","Others","Unknown1","Unknown2"))

data$MARRIAGE <- factor(data$MARRIAGE,
                        levels = c(0,1,2,3),
                        labels = c("Others","Married","Single","Others2"))

data$default <- factor(data$default,
                       levels = c(0,1),
                       labels = c("No Default","Default"))


colSums(is.na(data))


default_counts <- data.frame(table(data$default))

colnames(default_counts) <- c("default","n")

default_counts$pct <- round(default_counts$n / sum(default_counts$n) * 100,1)

ggplot(default_counts,
       aes(x = default, y = n, fill = default)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(n,"\n(",pct,"%)")),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("steelblue","tomato")) +
  labs(title = "Distribution of Default vs Non-Default Clients",
       x = "Default Status",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")



ggplot(data,
       aes(x = LIMIT_BAL,
           fill = default,
           color = default)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("steelblue","tomato")) +
  scale_color_manual(values = c("steelblue","tomato")) +
  labs(title = "Credit Limit Distribution by Default",
       x = "Credit Limit",
       y = "Density") +
  theme_minimal() 



edu_default <- aggregate(default ~ EDUCATION,
                         data = data,
                         FUN = function(x) mean(x == "Default") * 100)

colnames(edu_default) <- c("EDUCATION","pct")

ggplot(edu_default,
       aes(x = reorder(EDUCATION,-pct),
           y = pct,
           fill = EDUCATION)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(pct,1),"%")),
            vjust = -0.5) +
  labs(title = "Default Rate by Education",
       x = "Education Level",
       y = "Default Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none") 




ggplot(data,
       aes(x = AGE,
           fill = default)) +
  geom_histogram(binwidth = 2,
                 position = "dodge",
                 alpha = 0.8) +
  scale_fill_manual(values = c("steelblue","tomato")) +
  labs(title = "Age Distribution by Default",
       x = "Age",
       y = "Count") +
  theme_minimal() 




set.seed(2025)

train_index <- createDataPartition(data$default,
                                   p = 0.70,
                                   list = FALSE)

train_data <- data[train_index, ]

test_data <- data[-train_index, ]

nrow(train_data)
nrow(test_data) 




log_model <- glm(default ~ .,
                 data = train_data,
                 family = binomial)

log_probs <- predict(log_model,
                     newdata = test_data,
                     type = "response")

log_preds <- factor(ifelse(log_probs > 0.5,
                           "Default",
                           "No Default"),
                    levels = levels(test_data$default))

cm_log <- confusionMatrix(log_preds,
                          test_data$default,
                          positive="Default")

print(cm_log)



tree_model <- rpart(default ~ .,
                    data = train_data,
                    method = "class")

rpart.plot(tree_model)

tree_preds <- predict(tree_model,
                      newdata = test_data,
                      type = "class")

cm_tree <- confusionMatrix(tree_preds,
                           test_data$default,
                           positive="Default")

print(cm_tree) 




rf_model <- randomForest(default ~ .,
                         data = train_data,
                         ntree = 200,
                         importance = TRUE)

rf_preds <- predict(rf_model,
                    newdata = test_data)

cm_rf <- confusionMatrix(rf_preds,
                         test_data$default,
                         positive="Default")

print(cm_rf) 


data <- na.omit(data)

colSums(is.na(data))




set.seed(2025)

train_index <- createDataPartition(data$default,
                                   p = 0.70,
                                   list = FALSE)

train_data <- data[train_index, ]
test_data <- data[-train_index, ]


rf_model <- randomForest(default ~ .,
                         data = train_data,
                         ntree = 200,
                         importance = TRUE)

rf_preds <- predict(rf_model,
                    newdata = test_data)

cm_rf <- confusionMatrix(rf_preds,
                         test_data$default,
                         positive = "Default")

print(cm_rf) 



importance_df <- as.data.frame(importance(rf_model))

importance_df$Variable <- rownames(importance_df)

importance_df <- importance_df[order(-importance_df$MeanDecreaseGini),]

ggplot(importance_df[1:15,],
       aes(x = reorder(Variable,MeanDecreaseGini),
           y = MeanDecreaseGini)) +
  geom_bar(stat="identity",
           fill="darkgreen") +
  coord_flip() +
  labs(title="Top 15 Important Variables",
       x="Variables",
       y="Importance") +
  theme_minimal() 


model_comparison <- data.frame(
  Model = c("Logistic Regression","Decision Tree","Random Forest"),
  Accuracy = c(cm_log$overall["Accuracy"],
               cm_tree$overall["Accuracy"],
               cm_rf$overall["Accuracy"])
)

print(model_comparison)



cat("PROJECT EXECUTED SUCCESSFULLY")