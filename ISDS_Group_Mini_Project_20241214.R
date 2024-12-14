#### ISDS Group Mini Project

library(ggplot2)
library(car)
library(corrplot)
library(GGally)
library(DescTools)

#### [1.1] Plot correlation heatmap
cor_matrix <- cor(UKContentment[, c("Influence_Decisions", "Get_On_Well", "Belong", "Drug_Use_And_Selling", "Overall")])
corrplot(cor_matrix, method = "circle", type = "lower", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", 
         col = colorRampPalette(c("blue", "white", "red"))(200))


#### [1.2] Plot scatter matrix
variables <- UKContentment[, c("Influence_Decisions", "Get_On_Well", "Belong", "Drug_Use_And_Selling", "Overall")]
ggpairs(
  variables,
  title = "Relationships Between Variables",
  upper = list(continuous = wrap("cor", stars = FALSE)),
  diag = list(continuous = wrap("densityDiag", fill = "orange")),
  lower = list(continuous = wrap("points", alpha = 0.6, color = "orange"))
)


#### [1.3] Box plot 
par(mfrow = c(1, 5))
boxplot(UKContentment$Influence_Decisions, main = "Influence_Decisions")
boxplot(UKContentment$Get_On_Well, main = "Get_On_Well")
boxplot(UKContentment$Belong, main = "Belong")
boxplot(UKContentment$Drug_Use_And_Selling, main = "Drug_Use_And_Selling")
boxplot(UKContentment$Overall, main = "Overall")
par(mfrow = c(1, 1))


#### [1.4] histograms
hist(UKContentment$Influence_Decisions, 
     main = "Histogram of Proportion of Residents Unable to Influence Decisions", 
     xlab = "Proportion of 'Yes' Responses", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")

hist(UKContentment$Get_On_Well, 
     main = "Histogram of Proportion of Residents Who Believe People Don't Get Along", 
     xlab = "Proportion of 'Yes' Responses", 
     ylab = "Frequency", 
     col = "lightgreen", 
     border = "black")

hist(UKContentment$Belong, 
     main = "Histogram of Proportion of Residents Who Feel They Don't Belong", 
     xlab = "Proportion of 'Yes' Responses", 
     ylab = "Frequency", 
     col = "lightcoral", 
     border = "black")

hist(UKContentment$Drug_Use_And_Selling, 
     main = "Histogram of Proportion of Residents Considering Drugs a Problem", 
     xlab = "Proportion of 'Yes' Responses", 
     ylab = "Frequency", 
     col = "lightgoldenrod", 
     border = "black")

hist(UKContentment$Overall, 
     main = "Histogram of Proportion of Residents Dissatisfied with Living in Their Area", 
     xlab = "Proportion of 'Yes' Responses", 
     ylab = "Frequency", 
     col = "lightpink", 
     border = "black")


#### [1.5] Checking for multicollinearity
vif_ID <- lm(Influence_Decisions ~ Get_On_Well+Belong+Drug_Use_And_Selling, data=UKContentment)
vif(vif_ID)
vif_GOW <- lm(Get_On_Well ~ Influence_Decisions+Belong+Drug_Use_And_Selling, data=UKContentment)
vif(vif_GOW)
vif_B <- lm(Belong ~ Get_On_Well+Influence_Decisions+Drug_Use_And_Selling, data=UKContentment)
vif(vif_B)
vif_DUAS <- lm(Drug_Use_And_Selling ~ Get_On_Well+Belong+Influence_Decisions, data=UKContentment)
vif(vif_DUAS)
#There is no significant multicollinearity among the four variables.


#### [2] Clean data 

#result explanation of "boxplot.stat" function：
#stats：the minimum whisker value, the minimum box value, the median, the maximum box value, the maximum whisker value
#n: the number of non-empty values
#conf：the 95% confidence interval of the median
#out：outliers
ID_status = boxplot.stats(UKContentment$Influence_Decisions, coef=1.5, do.out=TRUE)
GOW_status = boxplot.stats(UKContentment$Get_On_Well, coef=1.5, do.out=TRUE)
B_status = boxplot.stats(UKContentment$Belong, coef=1.5, do.out=TRUE)
DUAS_status = boxplot.stats(UKContentment$Drug_Use_And_Selling, coef=1.5, do.out=TRUE)
O_status = boxplot.stats(UKContentment$Overall, coef=1.5, do.out=TRUE)
cat("Influence_Decisions has",length(ID_status$out), "outliers:", ID_status$out,'\n')
cat("Get_On_Well has",length(GOW_status$out), "outliers:", GOW_status$out,'\n')
cat("Belong has",length(B_status$out), "outliers:", B_status$out,'\n')
cat("Drug_Use_And_Selling has",length(DUAS_status$out), "outliers:", DUAS_status$out,'\n')
cat("Overall has",length(O_status$out), "outliers:", O_status$out,'\n')


#### [2.1] Data cleaning method 1: Exclude Outliers
exclude_method = function(dataframe,targetList,outlier){
  excluded_list = dataframe[!(dataframe[[targetList]] %in% outlier), ]
  return(excluded_list)
}
output1 = exclude_method(UKContentment,'Influence_Decisions',ID_status$out)
output2 = exclude_method(output1,'Get_On_Well',GOW_status$out)
output3 = exclude_method(output2,'Belong',B_status$out)
output4 = exclude_method(output3,'Drug_Use_And_Selling',DUAS_status$out)
cleaned_UK1 = exclude_method(output4,'Overall',O_status$out)
nrow(cleaned_UK1)

exclude_model <- lm(Overall~. - Authority - Area, data=cleaned_UK1)
summary(exclude_model) #AdjR2:0.8228


#### [2.2] Data cleaning method 2: Winsorization (replace outliers with maximum or minimum values)
cleaned_UK2 = UKContentment
cleaned_UK2$Influence_Decisions <- Winsorize(cleaned_UK2$Influence_Decisions,val=c(ID_status$stats[1],ID_status$stats[5]))
cleaned_UK2$Get_On_Well <- Winsorize(cleaned_UK2$Get_On_Well,val=c(GOW_status$stats[1],GOW_status$stats[5]))
cleaned_UK2$Belong <- Winsorize(cleaned_UK2$Belong,val=c(B_status$stats[1],B_status$stats[5]))
cleaned_UK2$Drug_Use_And_Selling <- Winsorize(cleaned_UK2$Drug_Use_And_Selling,val=c(DUAS_status$stats[1],DUAS_status$stats[5]))
cleaned_UK2$Overall <- Winsorize(cleaned_UK2$Overall,val=c(O_status$stats[1],O_status$stats[5]))

winsorize_model_1 <- lm(Overall~. - Authority - Area, data=cleaned_UK2)
summary(winsorize_model_1) #AdjR2:0.844


#### [2.3] Data cleaning method 3: Winsorize by area
Area_List <- split(UKContentment, UKContentment$Area)
cleaned_UK3 = data.frame()
for (area_data in Area_List){
  ID_status1 = boxplot.stats(area_data$Influence_Decisions, coef=1.5, do.out=TRUE)
  GOW_status1 = boxplot.stats(area_data$Get_On_Well, coef=1.5, do.out=TRUE)
  B_status1 = boxplot.stats(area_data$Belong, coef=1.5, do.out=TRUE)
  DUAS_status1 = boxplot.stats(area_data$Drug_Use_And_Selling, coef=1.5, do.out=TRUE)
  O_status1 = boxplot.stats(area_data$Overall, coef=1.5, do.out=TRUE)
  area_data$Influence_Decisions <- Winsorize(area_data$Influence_Decisions,val=c(ID_status1$stats[1],ID_status1$stats[5]))
  area_data$Get_On_Well <- Winsorize(area_data$Get_On_Well,val=c(GOW_status1$stats[1],GOW_status1$stats[5]))
  area_data$Drug_Use_And_Selling <- Winsorize(area_data$Drug_Use_And_Selling,val=c(DUAS_status1$stats[1],DUAS_status1$stats[5]))
  area_data$Overall <- Winsorize(area_data$Overall,val=c(O_status1$stats[1],O_status1$stats[5]))
  cleaned_UK3 = rbind(cleaned_UK3,area_data)
}
winsorize_model_2 <- lm(Overall~. - Authority - Area, data=cleaned_UK3)
summary(winsorize_model_2) #AdjR2:0.8429

#### [2.4] Data cleaning method 4: DO NOTHING!!!
donothing_model <- lm(Overall~. - Authority - Area, data=UKContentment)
summary(donothing_model) #AdjR2:0.8475

# !!!We finally chose to do nothing with the data
# Reason1: The models obtained by the three processing methods are not as good as those without processing data.
# Reason2: Any data is real and valuable data, even if they are outliers, we cannot change them easily.
# Although there is a risk of overfitting if the data is not processed


#### [3.1] The impact of a single variable on the overall
simplemod_ID <- lm(Overall~Influence_Decisions, data=UKContentment)
summary(simplemod_ID)
plot(UKContentment$Influence_Decisions, UKContentment$Overall, xlab="Influence_Decisions", ylab="Overall", main=" Influence_Decisions vs  Overall")
abline(simplemod_ID)

simplemod_GOW <- lm(Overall~Get_On_Well, data=UKContentment)
summary(simplemod_GOW)
plot(UKContentment$Get_On_Well,UKContentment$Overall, xlab="Get_On_Well", ylab="Overall", main=" Get_On_Well vs  Overall")
abline(simplemod_GOW)

simplemod_Belong <- lm(Overall~Belong, data=UKContentment)
summary(simplemod_Belong)
plot(UKContentment$Belong,UKContentment$Overall, xlab="Belong", ylab="Overall", main=" Belong vs  Overall")
abline(simplemod_Belong)

simplemod_Drug <- lm(Overall~Drug_Use_And_Selling, data=UKContentment)
summary(simplemod_Drug)
plot(UKContentment$Drug_Use_And_Selling, UKContentment$Overall, xlab="Drug_Use_And_Selling", ylab="Overall", main=" Drug_Use_And_Selling vs  Overall")
abline(simplemod_Drug)

# simplemod_Area <- lm(Overall~Area, data=UKContentment)
# summary(simplemod_Area)
# We found that all four questions have a significant impact on "Overall"


#### [3.2] Simple Mltiple linear regression model
multiplemod <- lm(Overall~Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=UKContentment)
summary(multiplemod) #AdjR2:0.8475


#### [3.3] Finding interaction terms
# Consider two interactions
interaction_test0 <- lm(Overall ~ (Influence_Decisions + Get_On_Well + Belong + Drug_Use_And_Selling)^2, data=UKContentment)
summary(interaction_test0) #AdjR2:0.8511 
interaction_test1 <- lm(Overall~Influence_Decisions*Get_On_Well+Belong+Drug_Use_And_Selling, data=UKContentment)
summary(interaction_test1) #AdjR2:0.8478 
interaction_test2 <- lm(Overall~Influence_Decisions*Belong+Get_On_Well+Drug_Use_And_Selling, data=UKContentment)
summary(interaction_test2) #AdjR2:0.847 
interaction_test3 <- lm(Overall~Influence_Decisions*Drug_Use_And_Selling+Get_On_Well+Belong, data=UKContentment)
summary(interaction_test3) #AdjR2:0.849
interaction_test4 <- lm(Overall~Influence_Decisions+Get_On_Well*Belong+Drug_Use_And_Selling, data=UKContentment)
summary(interaction_test4) #AdjR2:0.8507 
interaction_test5 <- lm(Overall~Influence_Decisions+Get_On_Well*Drug_Use_And_Selling+Belong, data=UKContentment)
summary(interaction_test5) #AdjR2:0.8472 
interaction_test6 <- lm(Overall~Influence_Decisions+Get_On_Well+Belong*Drug_Use_And_Selling, data=UKContentment)
summary(interaction_test6) #AdjR2:0.8481 
# Consider three interactions
interaction_test7 <- lm(Overall~Influence_Decisions:Get_On_Well:Belong+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=UKContentment)
summary(interaction_test7) #AdjR2:0.8471 
interaction_test8 <- lm(Overall~Influence_Decisions:Get_On_Well:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=UKContentment)
summary(interaction_test8) #AdjR2:0.8478 
interaction_test9 <- lm(Overall~Influence_Decisions:Belong:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=UKContentment)
summary(interaction_test9) #AdjR2:0.8476 
interaction_test10 <- lm(Overall~Get_On_Well:Belong:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=UKContentment)
summary(interaction_test10) #AdjR2:0.8474 
# In the above models, according to R-squared, the interaction term selected is "Get_On_Well:Belong".

#################################

# So the chosen model is as follows:
interaction_mod <- lm(Overall~Influence_Decisions+Get_On_Well*Belong+Drug_Use_And_Selling, data=UKContentment)
summary(interaction_mod) #AdjR2:0.8507
# After introducing the interaction term, the r-squared value increased from 0.8457 to 0.8507,
#and the residual standard error decreased from 2.754 to 2.707,
#indicating that the interaction term improved the model.
# The p-value of F-statistic is 2.2e-16, indicating that the model is significant and reasonable.


#### [3.4] Check the residual distribution of the model
par(mfrow = c(2, 2))
plot(interaction_mod)
par(mfrow = c(1, 1))


#### [3.4.1] We found that the data numbers 25, 30 and 33 are obviously abnormal
# Calculate Cook's distances
cooks_dist <- cooks.distance(interaction_mod)
cooks_dist[c(25, 30, 33)]
# winsorized 25 and 30.
UK = UKContentment
UK[25,2] = ID_status$stats[5]
UK[30,5] = DUAS_status$stats[5]
# UK[33,3] = GOW_status$stats[5]


#### [3.4.2] Simple Mltiple linear regression model
multiplemod_UK <- lm(Overall~Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=UK)
summary(multiplemod_UK) #AdjR2:0.8515


#### [3.4.3] Finding interaction terms
# Consider two interactions
interaction_test1_UK <- lm(Overall~Influence_Decisions*Get_On_Well+Belong+Drug_Use_And_Selling, data=UK)
summary(interaction_test1_UK) #AdjR2:0.8517 
interaction_test2_UK <- lm(Overall~Influence_Decisions*Belong+Get_On_Well+Drug_Use_And_Selling, data=UK)
summary(interaction_test2_UK) #AdjR2:0.8511 
interaction_test3_UK <- lm(Overall~Influence_Decisions*Drug_Use_And_Selling+Get_On_Well+Belong, data=UK)
summary(interaction_test3_UK) #AdjR2:0.8527 
interaction_test4_UK <- lm(Overall~Influence_Decisions+Get_On_Well*Belong+Drug_Use_And_Selling, data=UK)
summary(interaction_test4_UK) #AdjR2:0.8559 
interaction_test5_UK <- lm(Overall~Influence_Decisions+Get_On_Well*Drug_Use_And_Selling+Belong, data=UK)
summary(interaction_test5_UK) #AdjR2:0.8518 
interaction_test6_UK <- lm(Overall~Influence_Decisions+Get_On_Well+Belong*Drug_Use_And_Selling, data=UK)
summary(interaction_test6_UK) #AdjR2:0.8541 
# # Consider three interactions
# interaction_test7_UK <- lm(Overall~Influence_Decisions:Get_On_Well:Belong+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=UK)
# summary(interaction_test7_UK)
# interaction_test8_UK <- lm(Overall~Influence_Decisions:Get_On_Well:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=UK)
# summary(interaction_test8_UK)
# interaction_test9_UK <- lm(Overall~Influence_Decisions:Belong:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=UK)
# summary(interaction_test9_UK)
# interaction_test10_UK <- lm(Overall~Get_On_Well:Belong:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=UK)
# summary(interaction_test10_UK)
# Considering the r-squared of the model and the significance of interaction term, 
#and considering that introducing interaction terms would make the model complex and difficult to interpret, 
#we finally decided to introduce one interaction term, which is "Get_On_Well*Belong".

# !!!So the NEW final model is as follows: 
# (The model does not changed)
interaction_mod_UK <- lm(Overall~Influence_Decisions+Get_On_Well*Belong+Drug_Use_And_Selling, data=UK)
summary(interaction_mod_UK) #AdjR2:0.8559

#### [3.4.4] Check the residual distribution of the new model
par(mfrow = c(2, 2))
plot(interaction_mod_UK)
par(mfrow = c(1, 1))
# Looks better now!

residuals_UK <- residuals(interaction_mod_UK)
hist(residuals_UK, breaks=20, main="Residuals Histogram", xlab="Residuals", col="lightblue", border="white")
qqnorm(residuals_UK, main="Q-Q Plot of Residuals")
qqline(residuals_UK, col="red", lwd=2)
shapiro.test(residuals_UK)
# The p-value is 0.1759 > 0.05, indicates that the residuals are normally distributed.


#### [3.4.5] AIC (Check model performance) (the lower the better)
aic_value1 <- AIC(interaction_mod_UK)
aic_value2 <- AIC(multiplemod_UK)
aic_value3 <- AIC(interaction_test1_UK)
aic_value4 <- AIC(interaction_test2_UK)
aic_value5 <- AIC(interaction_test3_UK)
# aic_value? <- AIC(interaction_test4_UK)
aic_value6 <- AIC(interaction_test5_UK)
aic_value7 <- AIC(interaction_test6_UK)
cat(" The AIC value for Model 1 is:", AIC(interaction_mod_UK), "\n",
  "The AIC value for Model 2 is:", AIC(multiplemod_UK), "\n",
  "The AIC value for Model 3 is:", AIC(interaction_test1_UK), "\n",
  "The AIC value for Model 4 is:", AIC(interaction_test2_UK), "\n",
  "The AIC value for Model 5 is:", AIC(interaction_test3_UK), "\n",
  "The AIC value for Model 6 is:", AIC(interaction_test5_UK), "\n",
  "The AIC value for Model 7 is:", AIC(interaction_test6_UK), "\n")


#### [3.4.6] BIC (Check model performance) (the lower the better)
bic_value1 <- BIC(interaction_mod_UK)
bic_value2 <- BIC(multiplemod_UK)
bic_value3 <- BIC(interaction_test1_UK)
bic_value4 <- BIC(interaction_test2_UK)
bic_value5 <- BIC(interaction_test3_UK)
# bic_value? <- BIC(interaction_test4_UK)
bic_value6 <- BIC(interaction_test5_UK)
bic_value7 <- BIC(interaction_test6_UK)
cat(" The BIC value for Model 1 is:", BIC(interaction_mod_UK), "\n",
    "The BIC value for Model 2 is:", BIC(multiplemod_UK), "\n",
    "The BIC value for Model 3 is:", BIC(interaction_test1_UK), "\n",
    "The BIC value for Model 4 is:", BIC(interaction_test2_UK), "\n",
    "The BIC value for Model 5 is:", BIC(interaction_test3_UK), "\n",
    "The BIC value for Model 6 is:", BIC(interaction_test5_UK), "\n",
    "The BIC value for Model 7 is:", BIC(interaction_test6_UK), "\n")


#### [3.4.7] Root mean square error (Check model performance) (the lower the better)
models <- list(
   interaction_mod_UK, multiplemod_UK, interaction_test1_UK, interaction_test2_UK,
   interaction_test3_UK, interaction_test5_UK, interaction_test6_UK
)
rmse_values <- c()

for (i in 1:length(models)) {
  predicted <- predict(models[[i]], newdata = UK)
  actual <- UK$Overall
  rmse <- sqrt(mean((actual - predicted)^2))
  rmse_values <- c(rmse_values, rmse)
  cat("RMSE of model", i, "is: ", rmse, "\n")
}
# Calculate the average RMSE
average_rmse <- mean(rmse_values)
cat("The average RMSE of all models is: ", average_rmse, "\n")


#### [3.4.8] K-fold cross validation (Check model performance) (the lower the better)
library(caret)
set.seed(123)
models <- list(
Overall ~ Influence_Decisions + Get_On_Well * Belong + Drug_Use_And_Selling,
Overall ~ Influence_Decisions + Get_On_Well + Belong + Drug_Use_And_Selling,
Overall ~ Influence_Decisions * Get_On_Well + Belong + Drug_Use_And_Selling,
Overall ~ Influence_Decisions * Belong + Get_On_Well + Drug_Use_And_Selling,
Overall ~ Influence_Decisions * Drug_Use_And_Selling + Belong + Get_On_Well,
Overall ~ Influence_Decisions + Get_On_Well * Drug_Use_And_Selling + Belong,
Overall ~ Influence_Decisions + Get_On_Well + Belong * Drug_Use_And_Selling
)
model_rmse <- c()

for (i in 1:length(models)) {
  train_control <- trainControl(method = "cv",  # K-fold cross validation
                                number = 10,  # K = 10
                                verboseIter = FALSE)
  cv_model <- train(models[[i]], data = UK,
                    method = "lm",
                    trControl = train_control)
  rmse <- cv_model$results$RMSE
  model_rmse <- c(model_rmse, rmse)
  cat("The average RMSE of model", i, "is: ", rmse, "\n")
}
# Find the model with the smallest RMSE
best_model_index <- which.min(model_rmse)
cat("\n The best performing model is: ", best_model_index, ", RMSE is: ", model_rmse[best_model_index], "\n")

# !!! Our model is the best in all five test methods (Adjusted R-squared, RMSE, AIC, BIC, and RMSE of 10-fold cross validation).


#### [3.5] Standardizing Data (To find out which question has the greatest impact)
# By standardizing the data, we can directly see which one has the largest influence by looking at the coefficient size.
standardized_UK <- as.data.frame(scale(UK[, c("Influence_Decisions", "Get_On_Well", "Belong", "Drug_Use_And_Selling")]))
# Keep the dependent variable and other non-standardized variables
standardized_UK$Overall <- UK$Overall
standardized_UK$Area <- UK$Area
# Model after standardizing data
standardized_model <- lm(Overall ~ Influence_Decisions+Get_On_Well*Belong+Drug_Use_And_Selling, data = standardized_UK)
summary(standardized_model)  #AdjR2:0.8559
# Conclusion: By comparing the coefficients, we know that "Drug_Use_And_Selling" has the greatest impact (-3.3834),
#followed by "Get_On_Well" (2.3355) and "Belong" (1.9945),
#then the one with the least impact is "Influence_Decisions" (0.4454).


#### [4.1] Test model performance in each area. Then draw a table to summarize.
Areas_List <- split(standardized_UK, standardized_UK$Area)

model_performance <- data.frame(
  Area = character(),
  R_squared = numeric(),
  Adj_R_squared = numeric(),
  Residual_SE = numeric(),
  F_statistic = numeric(),
  P_value = numeric(),
  Influence_Decisions_Coeff = numeric(),
  Get_On_Well_Coeff = numeric(),
  Belong_Coeff = numeric(),
  Drug_Use_And_Selling_Coeff = numeric(),
  stringsAsFactors = FALSE
)

for (area in names(Areas_List)) {
  print(area)
  mod <- lm(Overall ~ Influence_Decisions + Get_On_Well * Belong + Drug_Use_And_Selling, data = Areas_List[[area]])
  print(summary(mod))
  
  summary_mod <- summary(mod)
  r_squared <- summary_mod$r.squared
  adj_r_squared <- summary_mod$adj.r.squared
  residual_se <- summary_mod$sigma
  f_statistic <- summary_mod$fstatistic[1]
  p_value <- pf(f_statistic, summary_mod$fstatistic[2], summary_mod$fstatistic[3], lower.tail = FALSE)
  
  coeffs <- summary_mod$coefficients
  ID_coeff <- ifelse("Influence_Decisions" %in% rownames(coeffs), coeffs["Influence_Decisions", "Estimate"], NA)
  GOW_coeff <- ifelse("Get_On_Well" %in% rownames(coeffs), coeffs["Get_On_Well", "Estimate"], NA)
  B_coeff <- ifelse("Belong" %in% rownames(coeffs), coeffs["Belong", "Estimate"], NA)
  DUAS_coeff <- ifelse("Drug_Use_And_Selling" %in% rownames(coeffs), coeffs["Drug_Use_And_Selling", "Estimate"], NA)
  
  model_performance <- rbind(model_performance, data.frame(
    Area = area,
    R_squared = r_squared,
    Adj_R_squared = adj_r_squared,
    Residual_SE = residual_se,
    F_statistic = f_statistic,
    P_value = p_value,
    Influence_Decisions_Coeff = ID_coeff,
    Get_On_Well_Coeff = GOW_coeff,
    Belong_Coeff = B_coeff,
    Drug_Use_And_Selling_Coeff = DUAS_coeff
  ))
}

print(model_performance)
# It is recommended to find the average or weighted average of the four coefficients here.(See Excel attachment)
# The model performs well in most of the areas, except "North East". 
# We will continue to model the "North East" separately.


#### [4.2] Modeling "North East"
# Reason: The model does not perform well in the Northeast.
# Note: The original data is used here, not the cleaned data above.
#### [4.2.1]
northeast <- subset(UKContentment, Area == "North East")
summary(northeast)

par(mfrow = c(1, 5))
boxplot(northeast$Influence_Decisions, main = "Influence_Decisions")
boxplot(northeast$Get_On_Well, main = "Get_On_Well")
boxplot(northeast$Belong, main = "Belong")
boxplot(northeast$Drug_Use_And_Selling, main = "Drug_Use_And_Selling")
boxplot(northeast$Overall, main = "Overall")
par(mfrow = c(1, 1))

#### [4.2.2] Clean data "North East" method 1: Exclude Outliers
northeast <- subset(UKContentment, Area == "North East")
ID_status_NE = boxplot.stats(northeast$Influence_Decisions, coef=1.5, do.out=TRUE)
GOW_status_NE = boxplot.stats(northeast$Get_On_Well, coef=1.5, do.out=TRUE)
B_status_NE = boxplot.stats(northeast$Belong, coef=1.5, do.out=TRUE)
DUAS_status_NE = boxplot.stats(northeast$Drug_Use_And_Selling, coef=1.5, do.out=TRUE)
O_status_NE = boxplot.stats(northeast$Overall, coef=1.5, do.out=TRUE)
cat("Influence_Decisions has",length(ID_status_NE$out), "outliers:", ID_status_NE$out,'\n')
cat("Get_On_Well has",length(GOW_status_NE$out), "outliers:", GOW_status_NE$out,'\n')
cat("Belong has",length(B_status_NE$out), "outliers:", B_status_NE$out,'\n')
cat("Drug_Use_And_Selling has",length(DUAS_status_NE$out), "outliers:", DUAS_status_NE$out,'\n')
cat("Overall has",length(O_status_NE$out), "outliers:", O_status_NE$out,'\n')

output5 = exclude_method(northeast,'Influence_Decisions',ID_status_NE$out)
output6 = exclude_method(output5,'Get_On_Well',GOW_status_NE$out)
output7 = exclude_method(output6,'Belong',B_status_NE$out)
output8 = exclude_method(output7,'Drug_Use_And_Selling',DUAS_status_NE$out)
cleaned_NE1 = exclude_method(output8,'Overall',O_status_NE$out)
nrow(cleaned_NE1)

#### [4.2.3] Clean data "North East" method 2: Winsorization (replace outliers with maximum or minimum values)
cleaned_NE2 = northeast
cleaned_NE2$Influence_Decisions <- Winsorize(cleaned_NE2$Influence_Decisions,val=c(ID_status_NE$stats[1],ID_status_NE$stats[5]))
cleaned_NE2$Get_On_Well <- Winsorize(cleaned_NE2$Get_On_Well,val=c(GOW_status_NE$stats[1],GOW_status_NE$stats[5]))
cleaned_NE2$Belong <- Winsorize(cleaned_NE2$Belong,val=c(B_status_NE$stats[1],B_status_NE$stats[5]))
cleaned_NE2$Drug_Use_And_Selling <- Winsorize(cleaned_NE2$Drug_Use_And_Selling,val=c(DUAS_status_NE$stats[1],DUAS_status_NE$stats[5]))
cleaned_NE2$Overall <- Winsorize(cleaned_NE2$Overall,val=c(O_status_NE$stats[1],O_status_NE$stats[5]))

#### [4.2.4]
# "North East" Model before cleaning data
model_NE <- lm(Overall ~ Influence_Decisions + Get_On_Well + Belong + Drug_Use_And_Selling, data = northeast)
summary(model_NE) #AdjR2:0.5123 
# "North East" Model after excluding outliers
model_exclude_NE <- lm(Overall ~ Influence_Decisions + Get_On_Well + Belong + Drug_Use_And_Selling, data = cleaned_NE1)
summary(model_exclude_NE) #AdjR2:0.3803 
# "North East" Model after winsorizing outliers
model_winsorize_NE <- lm(Overall ~ Influence_Decisions + Get_On_Well + Belong + Drug_Use_And_Selling, data = cleaned_NE2)
summary(model_winsorize_NE) #AdjR2:0.5228 
# First, considering the small amount of data (only eight groups),and there are many independent variables (four), try not to delete any data.
# Second, "model_winsorize_NE" performs the best.
# Conclusion: winsorizing outliers is the best way.

#### [4.2.5] Finding interaction terms in "North East"
# Consider two interactions (The number of samples is too small to use the square method above)
interaction_test1_NE <- lm(Overall~Influence_Decisions*Get_On_Well+Belong+Drug_Use_And_Selling, data=cleaned_NE2)
summary(interaction_test1_NE) #AdjR2:0.2855 
interaction_test2_NE <- lm(Overall~Influence_Decisions*Belong+Get_On_Well+Drug_Use_And_Selling, data=cleaned_NE2)
summary(interaction_test2_NE) #AdjR2:0.3489 
interaction_test3_NE <- lm(Overall~Influence_Decisions*Drug_Use_And_Selling+Get_On_Well+Belong, data=cleaned_NE2)
summary(interaction_test3_NE) #AdjR2:0.3403 
interaction_test4_NE <- lm(Overall~Influence_Decisions+Get_On_Well*Belong+Drug_Use_And_Selling, data=cleaned_NE2)
summary(interaction_test4_NE) #AdjR2:0.2954 
interaction_test5_NE <- lm(Overall~Influence_Decisions+Get_On_Well*Drug_Use_And_Selling+Belong, data=cleaned_NE2)
summary(interaction_test5_NE) #AdjR2:0.3872 
interaction_test6_NE <- lm(Overall~Influence_Decisions+Get_On_Well+Belong*Drug_Use_And_Selling, data=cleaned_NE2)
summary(interaction_test6_NE) #AdjR2:0.4578 
# # Consider three interactions
# interaction_test7_NE <- lm(Overall~Influence_Decisions:Get_On_Well:Belong+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=cleaned_NE2)
# summary(interaction_test7_NE)
# interaction_test8_NE <- lm(Overall~Influence_Decisions:Get_On_Well:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=cleaned_NE2)
# summary(interaction_test8_NE)
# interaction_test9_NE <- lm(Overall~Influence_Decisions:Belong:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=cleaned_NE2)
# summary(interaction_test9_NE)
# interaction_test10_NE <- lm(Overall~Get_On_Well:Belong:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=cleaned_NE2)
# summary(interaction_test10_NE)
# No significant interaction terms were found.
# Conclusion: The best model is "model_winsorize_NE", which is the model without any interaction terms.
# BUT!!! Even so, "model_winsorize_NE" may also be a meaningless model because the model performs poorly (p-value is 0.2029).
# Consider small sample sizes and other factors that affect "Overall".

#### [4.2.6] Check the residual distribution of the model
par(mfrow = c(2, 2))
plot(model_winsorize_NE)
par(mfrow = c(1, 1))

#### [4.2.7] Standardizing Data of "North East"
# By standardizing the data, we can directly see which one has the largest influence by looking at the coefficient size.
standardized_NE <- as.data.frame(scale(cleaned_NE2[, c("Influence_Decisions", "Get_On_Well", "Belong", "Drug_Use_And_Selling")]))
# Keep the dependent variable
standardized_NE$Overall <- cleaned_NE2$Overall
# Model after standardizing data
standardized_model_NE <- lm(Overall ~ Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data = standardized_NE)
summary(standardized_model_NE) #AdjR2:0.5228 
# Conclusion: By comparing the coefficients, we know that "Influence_Decisions" has the greatest impact in four questions.


#### [4.3] Modeling "London"
# Reason: London is the most developed area in England. Consider whether London will be different.
#### [4.3.1]
london <- subset(UKContentment, Area == "London")
summary(london)

par(mfrow = c(1, 5))
boxplot(london$Influence_Decisions, main = "Influence_Decisions")
boxplot(london$Get_On_Well, main = "Get_On_Well")
boxplot(london$Belong, main = "Belong")
boxplot(london$Drug_Use_And_Selling, main = "Drug_Use_And_Selling")
boxplot(london$Overall, main = "Overall")
par(mfrow = c(1, 1))

#### [4.3.2] Clean data "London" method 1: Exclude Outliers
ID_status_LON = boxplot.stats(london$Influence_Decisions, coef=1.5, do.out=TRUE)
GOW_status_LON = boxplot.stats(london$Get_On_Well, coef=1.5, do.out=TRUE)
B_status_LON = boxplot.stats(london$Belong, coef=1.5, do.out=TRUE)
DUAS_status_LON = boxplot.stats(london$Drug_Use_And_Selling, coef=1.5, do.out=TRUE)
O_status_LON = boxplot.stats(london$Overall, coef=1.5, do.out=TRUE)
cat("Influence_Decisions has",length(ID_status_LON$out), "outliers:", ID_status_LON$out,'\n')
cat("Get_On_Well has",length(GOW_status_LON$out), "outliers:", GOW_status_LON$out,'\n')
cat("Belong has",length(B_status_LON$out), "outliers:", B_status_LON$out,'\n')
cat("Drug_Use_And_Selling has",length(DUAS_status_LON$out), "outliers:", DUAS_status_LON$out,'\n')
cat("Overall has",length(O_status_LON$out), "outliers:", O_status_LON$out,'\n')

output9 = exclude_method(london,'Influence_Decisions',ID_status_LON$out)
output10 = exclude_method(output9,'Get_On_Well',GOW_status_LON$out)
output11 = exclude_method(output10,'Belong',B_status_LON$out)
output12 = exclude_method(output11,'Drug_Use_And_Selling',DUAS_status_LON$out)
cleaned_LON1 = exclude_method(output12,'Overall',O_status_LON$out)
nrow(cleaned_LON1)

#### [4.3.3] Clean data "London" method 2: Winsorization (replace outliers with maximum or minimum values)
cleaned_LON2 = london
cleaned_LON2$Influence_Decisions <- Winsorize(cleaned_LON2$Influence_Decisions,val=c(ID_status_LON$stats[1],ID_status_LON$stats[5]))
cleaned_LON2$Get_On_Well <- Winsorize(cleaned_LON2$Get_On_Well,val=c(GOW_status_LON$stats[1],GOW_status_LON$stats[5]))
cleaned_LON2$Belong <- Winsorize(cleaned_LON2$Belong,val=c(B_status_LON$stats[1],B_status_LON$stats[5]))
cleaned_LON2$Drug_Use_And_Selling <- Winsorize(cleaned_LON2$Drug_Use_And_Selling,val=c(DUAS_status_LON$stats[1],DUAS_status_LON$stats[5]))
cleaned_LON2$Overall <- Winsorize(cleaned_LON2$Overall,val=c(O_status_LON$stats[1],O_status_LON$stats[5]))

#### [4.3.4]
# "London" Model before cleaning data
model_LON <- lm(Overall ~ Influence_Decisions + Get_On_Well + Belong + Drug_Use_And_Selling, data = london)
summary(model_LON) #AdjR2:0.7311 
# "London" Model after excluding outliers
model_exclude_LON <- lm(Overall ~ Influence_Decisions + Get_On_Well + Belong + Drug_Use_And_Selling, data = cleaned_LON1)
summary(model_exclude_LON) #AdjR2:0.6968 
# "London" Model after winsorizing outliers
model_winsorize_LON <- lm(Overall ~ Influence_Decisions + Get_On_Well + Belong + Drug_Use_And_Selling, data = cleaned_LON2)
summary(model_winsorize_LON) #AdjR2:0.7816 
# Conclusion: winsorizing outliers is the best way.

#### [4.3.5] Finding interaction terms in "London"
# Consider two interactions
interaction_test1_LON <- lm(Overall~Influence_Decisions*Get_On_Well+Belong+Drug_Use_And_Selling, data=cleaned_LON2)
summary(interaction_test1_LON) #AdjR2:0.7973 
interaction_test2_LON <- lm(Overall~Influence_Decisions*Belong+Get_On_Well+Drug_Use_And_Selling, data=cleaned_LON2)
summary(interaction_test2_LON) #AdjR2:0.7745 
interaction_test3_LON <- lm(Overall~Influence_Decisions*Drug_Use_And_Selling+Get_On_Well+Belong, data=cleaned_LON2)
summary(interaction_test3_LON) #AdjR2:0.7757 
interaction_test4_LON <- lm(Overall~Influence_Decisions+Get_On_Well*Belong+Drug_Use_And_Selling, data=cleaned_LON2)
summary(interaction_test4_LON) #AdjR2:0.7745 
interaction_test5_LON <- lm(Overall~Influence_Decisions+Get_On_Well*Drug_Use_And_Selling+Belong, data=cleaned_LON2)
summary(interaction_test5_LON) #AdjR2:0.7736 
interaction_test6_LON <- lm(Overall~Influence_Decisions+Get_On_Well+Belong*Drug_Use_And_Selling, data=cleaned_LON2)
summary(interaction_test6_LON) #AdjR2:0.7772 
# # Consider three interactions
# interaction_test7_LON <- lm(Overall~Influence_Decisions:Get_On_Well:Belong+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=cleaned_LON2)
# summary(interaction_test7_LON)
# interaction_test8_LON <- lm(Overall~Influence_Decisions:Get_On_Well:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=cleaned_LON2)
# summary(interaction_test8_LON)
# interaction_test9_LON <- lm(Overall~Influence_Decisions:Belong:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=cleaned_LON2)
# summary(interaction_test9_LON)
# interaction_test10_LON <- lm(Overall~Get_On_Well:Belong:Drug_Use_And_Selling+Influence_Decisions+Get_On_Well+Belong+Drug_Use_And_Selling, data=cleaned_LON2)
# summary(interaction_test10_LON)
# The interaction term "Influence_Decisions:Get_On_Well" in the model "interaction_test1_LON" has a significant effect (p=0.0862), 
#so model 1 is the final model.
interaction_test1_LON <- lm(Overall~Influence_Decisions*Get_On_Well+Belong+Drug_Use_And_Selling, data=cleaned_LON2)
summary(interaction_test1_LON) #AdjR2:0.7973 

#### [4.3.6] Check the residual distribution of the model
par(mfrow = c(2, 2))
plot(interaction_test1_LON)
par(mfrow = c(1, 1))

#### [4.3.7] Standardizing Data of "London"
# By standardizing the data, we can directly see which one has the largest influence by looking at the coefficient size.
standardized_LON <- as.data.frame(scale(cleaned_LON2[, c("Influence_Decisions", "Get_On_Well", "Belong", "Drug_Use_And_Selling")]))
# Keep the dependent variable
standardized_LON$Overall <- cleaned_LON2$Overall
# Model after standardizing data
standardized_model_LON <- lm(Overall~Influence_Decisions*Get_On_Well+Belong+Drug_Use_And_Selling, data=standardized_LON)
summary(standardized_model_LON) #AdjR2:0.7973 
# Conclusion: By comparing the coefficients, we know that "Get_On_Well" has the greatest impact (6.5119), followed by "Drug_Use_And_Selling" (-2.5265).
# In London area, while "Drug" play a role, "Get_On_Well" seems to be more important.

