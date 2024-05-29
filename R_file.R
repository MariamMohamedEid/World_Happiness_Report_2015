#install.packages("readxl")
#install.packages("readr")
#install.packages("corrplot")
#install.packages("lattice")
#install.packages("Metrics")
#install.packages("caret")

library(readxl)

file_path <- "2015.csv"

data <- read.csv(file_path)

# Data Exploration
#----------------------------------------------------------------------------------------------------------------------------------------
dim(data) #print dimensions n.of rows and columns
#----------------------------------------------------------------------------------------------------------------------------------------
names(data) #print columns names
#----------------------------------------------------------------------------------------------------------------------------------------
str(data) #summerize data structure
#----------------------------------------------------------------------------------------------------------------------------------------
summary(data) #summerize data
#----------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(data))#check if there is null values
#----------------------------------------------------------------------------------------------------------------------------------------
head(data, n = 10)  # Display the first 10 rows
#----------------------------------------------------------------------------------------------------------------------------------------
# checking for duplicates 

duplicates <- data[duplicated(data), ]
if (nrow(duplicates) > 0) {
  print("Duplicate rows found:")
  print(duplicates)
} else {
  print("No duplicates found.")
}

#----------------------------------------------------------------------------------------------------------------------------------------
#correlation heatmap
library(corrplot)
library(readr)
datacorr <- data
numeric_data <- datacorr[, sapply(datacorr, is.numeric)]
correlation_matrix <- cor(numeric_data)
pdf("correlation_heatmap.pdf", width = 10, height = 8)  # Adjust dimensions as needed
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
dev.off()


#----------------------------------------------------------------------------------------------------------------------------------------
# Visualization

#----------------------------------------------------------------------------------------------------------------------------------------
#pairwise scatterplots
plot(data[-c(1,2,3)])
#pairs(data[-c(1,2,3)]) another way
#----------------------------------------------------------------------------------------------------------------------------------------
# Create a bar plot of the frequency of regions
barplot(table(data$Region), col = "skyblue", main = "Frequency of regions", xlab = "Region", ylab = "Frequency", las=2)

#----------------------------------------------------------------------------------------------------------------------------------------
#bar plot showing the average happiness per region
AvgHappinessPerRegion <- aggregate(data[,4],list(data$Region), mean)
barplot(AvgHappinessPerRegion$x, names.arg= AvgHappinessPerRegion$Group.1, main= "Average Happiness Per Region", las=2, col = "skyblue")

#----------------------------------------------------------------------------------------------------------------------------------------
# Top 10 Happiest Countries
Top10Countries = head(data$Country,10)
Top10Happiest = head(data$Happiness.Score,10)
barplot(Top10Happiest, names.arg = Top10Countries, main="Top 10 Happiest Countries", xlab="Countries", ylab="Ranks", las=2, col = "skyblue")

#----------------------------------------------------------------------------------------------------------------------------------------
#Last 10 Happiest Countries
Last10Countries = tail(data$Country,10)
Last10Happiest = tail(data$Happiness.Score,10)
barplot(Last10Happiest, names.arg = Last10Countries, main="Last 10 Happiest Countries", xlab="Countries", ylab="Ranks", las=2, col = "skyblue")


#----------------------------------------------------------------------------------------------------------------------------------------

countries <- c("Libya", "Palestinian Territories", "Egypt", "Yemen", "Syria")

# Find the indices of the rows where the Country column matches any of the countries
indices <- which(data$Country == countries[1] |
                   data$Country == countries[2] |
                   data$Country == countries[3] |
                   data$Country == countries[4] |
                   data$Country == countries[5])

# Subset the data using the indices
df_subset <- data[indices, ]

# Select the columns for plotting
cols_to_plot <- c("Happiness.Score", "Economy..GDP.per.Capita.", 
                  "Family", "Health..Life.Expectancy.", 
                  "Freedom", "Generosity", "Trust..Government.Corruption.")

# Create a grouped bar plot
barplot(t(df_subset[cols_to_plot]), 
        beside = TRUE, 
        col = rainbow(length(cols_to_plot)), 
        legend.text = TRUE, 
        args.legend = list(x = "topright"), 
        names.arg = countries,
        las=2,
        main = "Happiness Report for Selected Countries", 
        xlab = "Countries", 
        ylab = "Values")


#----------------------------------------------------------------------------------------------------------------------------------------
library(lattice) 
max_density <- max(density(data$Happiness.Score)$y)
densityplot(~ data$Happiness.Score | data$Region, data=data,auto.key=T,ylim = c(0, max_density*2), xlab="Regions")

#----------------------------------------------------------------------------------------------------------------------------------------
#Scatter plot of Happiness Score and Freedom
plot(data$Freedom, data$Happiness.Score, 
     xlab = "Freedom",
     ylab = "Happiness Score",
     main = "Scatter Plot of Happiness Score and Freedom",
     )
#abline(lm(data$Happiness.Score~data$Freedom,data=data),col='red')

#----------------------------------------------------------------------------------------------------------------------------------------

#Scatter plot of Happiness Score and GDP per Capita
plot(data$Economy, data$Happiness.Score, 
     xlab = "GDP per Capita",
     ylab = "Happiness Score",
     main = "Scatter Plot of Happiness Score and GDP per Capita")
#abline(lm(data$Happiness.Score~data$Economy..GDP.per.Capita.,data=data),col='red')

#----------------------------------------------------------------------------------------------------------------------------------------

#Scatter plot of Happiness Score and Family 
     plot(data$Family, data$Happiness.Score, 
     xlab = "Family",
     ylab = "Happiness Score",
     main = "Scatter Plot of Happiness Score and Family")
#abline(lm(data$Happiness.Score~data$Family,data=data),col='red')

#----------------------------------------------------------------------------------------------------------------------------------------

#Scatter plot of Happiness Score and Generosity
plot(data$Generosity, data$Happiness.Score, 
     xlab = "Generosity",
     ylab = "Happiness Score",
     main = "Scatter Plot of Happiness Score and Generosity")
#abline(lm(data$Happiness.Score~data$Generosity,data=data),col='red')

#----------------------------------------------------------------------------------------------------------------------------------------
#Scatter plot of Happiness Score and Health 
plot(data$Health, data$Happiness.Score, 
     xlab = "Health",
     ylab = "Happiness Score",
     main = "Scatter Plot of Happiness Score and Health")
#abline(lm(data$Happiness.Score~data$Health..Life.Expectancy.,data=data),col='red')

#----------------------------------------------------------------------------------------------------------------------------------------
#Scatter plot of Happiness Score and Corruption
plot(data$Trust..Government.Corruption., data$Happiness.Score, 
     xlab = "Trust..Governement.Corruption",
     ylab = "Happiness Score",
     main = "Scatter Plot of Happiness Score and Corruption")
#abline(lm(data$Happiness.Score~data$Trust..Government.Corruption.,data=data),col='red')

#----------------------------------------------------------------------------------------------------------------------------------------
# Hypothesis Testing (ANOVA)
#----------------------------------------------------------------------------------------------------------------------------------------
happiness = data
happiness <- happiness[happiness$Region %in% c("Western Europe", "Latin America and Caribbean", "Sub-Saharan Africa"),]
regions <- happiness$Region
regions
score <- happiness$Trust..Government.Corruption.*100
frame <- data.frame(regions, score)
frame

aggregate(x=score, by=list(regions), FUN="mean")

boxplot(score ~ as.factor(regions), data=happiness,xlab="Regions", ylab="Trust Score", las=2)

model <- aov((score ~ regions), data=happiness)
summary(model)
# since in the Pr(>F) field the value is 0.000461 which is <0.05,
# then the null hypothesis is rejected

# Shows the pairwise relationship between the samples
TukeyHSD(model)

library(lattice) 
densityplot(~ score, group=regions, data=happiness, auto.key=T)
densityplot(~ score | regions, data=happiness)

# Hypothesis (H0): As a region, "Western Europe", "Sub-Saharan Africa"
#                 and "Latin America and Caribbean" are all the same in terms of
#                 Trust(Government Corruption)

# Alternative Hypothesis: There is a significant difference between the three regions
#                        when it comes to the value of Corruption

# Conclusion:
#For "Sub-Saharan Africa-Latin America and Caribbean", the p-value(0.9691793) > 0.05 so we can't reject
#the null hypothesis which means that there's no significant difference between them in terms of trust

#For "Western Europe-Latin America and Caribbean", the p-value(0.0019540) < 0.05, so we reject the null
#hypothesis which means that there's a significant difference between them in terms of trust 

#For "Western Europe-Sub-Saharan Africa ", the p-value(0.0009103) < 0.05, so we reject the null
#hypothesis which means that there's a significant difference between them in terms of trust 
#----------------------------------------------------------------------------------------------------------------------------------------

freedoms <- happiness$Freedom
score <- happiness$Happiness.Score
frame <- data.frame(freedoms, score)
frame

model <- aov((score ~ freedoms), data=happiness)
summary(model)

# Null Hypothesis (H0): The value of the Freedom variable doesn't impact the 
#                       overall happiness score.

# Alternative Hypothesis: The value of the happiness variable has a significant impact 
#                         on the overall happiness score

# Conclusion: Reject the Null Hypothesis. P-value of freedom variable equals 6.88e-15
#             (Less than 0.005)
#----------------------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------
# Selecting and Training the Model (Using the Linear Regression Algorithm)
#----------------------------------------------------------------------------------------------------------------------------------------
# Why Linear Regression?
# Answer: we needed to estimate the relationship between two quantitative variables 
#         "Economy" and "Happiness Score". Using Linear Regression we were able to 
#         conclude there's a strong direct relationship between the two variables.

# Performance Measures:
#       Standard Error (SE): the approximate standard deviation of a statistical sample population
#                     that  describes the variation between the calculated mean of the population 
#                     and one which is considered known, or accepted as accurate. 
#                     The lower the RSE, the better the model.
#
#       P-value: if <0.05, value is significant in identifying relationship
#----------------------------------------------------------------------------------------------------------------------------------------

# Reading the Shuffled Dataset
happiness <- read.csv("newHappiness4.csv")

#random_indices <- sample(nrow(happiness))  # Generate random row indices
#shuffled_happiness <- happiness[random_indices, ]  # Reorder rows based on random indices

# Splitting the Data into Training and Testing Datasets
train <- happiness[0:79,]
test <- happiness[80:158,]

economies <- train$Economy..GDP.per.Capita.
score <- train$Happiness.Score
plot(happiness$Economy..GDP.per.Capita., happiness$Happiness.Score, xlab="Economy (GDP per Capita)", ylab="Happiness Score")

fit <- lm(train$Happiness.Score ~ train$Economy..GDP.per.Capita.)
fit

summary(fit)

abline(fit, col="blue")

# Predict happiness scores for test data
test_pred <- predict(fit, newdata = test)
test_pred

# Define threshold (e.g., mean happiness score in training data)
#threshold <- mean(train$Happiness.Score) another way to define the threshold
threshold <- 4

# Convert predictions to binary classes
test_pred_class <- ifelse(test_pred > threshold, 1, 0)

# Convert actual happiness scores to binary classes
test_actual_class <- ifelse(test$Happiness.Score > threshold, 1, 0)

# Calculate true positives, false positives, true negatives, false negatives
TP <- sum(test_pred_class == 1 & test_actual_class == 1)
FP <- sum(test_pred_class == 1 & test_actual_class == 0)
TN <- sum(test_pred_class == 0 & test_actual_class == 0)
FN <- sum(test_pred_class == 0 & test_actual_class == 1)

print(paste("TP: ",TP))
print(paste("FP: ",FP))
print(paste("TN: ",FP))
print(paste("FN: ",FN))

# Calculate precision
precision <- TP / (TP + FP)

# Calculate recall
recall <- TP / (TP + FN)

# Print precision and recall
print(paste(" precision: ",precision))
print(paste(" Recall: ",recall))

# Manually Calculating the Accuracy
accuracy <- mean(test_actual_class == test_pred_class) * 100
paste(" Accuracy: ",accuracy)

#----------------------------------------------------------------------------------------------------------------------------------------
# Calculating different Error Rates
#----------------------------------------------------------------------------------------------------------------------------------------
library(Metrics)
rmse_value <- rmse(test_actual_class, test_pred_class)
paste(" Root Mean Square Error: ",rmse_value)

# Extract residuals from the model
residuals <- residuals(fit)

# Calculate residual sum of squares (RSS)
rss <- sum(residuals^2)

# Calculate degrees of freedom (df)
df <- df.residual(fit)

# Calculate Residual Standard Error (RSE)
rse <- sqrt(rss / df)
paste(" Residual Standard Error: ",rse)

# Printing the Confusion Matrix and the Accuracy
xtab <- table(test_pred_class, test_actual_class)

library(caret)
cm <- caret::confusionMatrix(xtab)

# Confusion matrix data
confusion_matrix_data <- matrix(c(2, 5, 6, 66), nrow=2, byrow=TRUE)
colnames(confusion_matrix_data) <- c("Actual Class 0", "Actual Class 1")
rownames(confusion_matrix_data) <- c("Predicted Class 0", "Predicted Class 1")

# Print confusion matrix
print("Confusion Matrix:")
print(confusion_matrix_data)

cat("\nAccuracy:", cm$overall['Accuracy'])

