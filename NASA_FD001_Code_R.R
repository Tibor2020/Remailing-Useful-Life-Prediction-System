# List of packages for session
.packages = c("tidyverse", "knitr", "caret", "randomForest", "rpart", "ggpubr", "survival", "survminer", 
              "kernlab", "ggridges")

# Install missing packages
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages
lapply(.packages, require, character.only=TRUE)

#Downloading the data
dl <- tempfile()
download.file("https://drive.google.com/uc?export=download&id=1OZ7oS6CO6TSvUNRfjX70ApqBEtcBf2Um", dl)
train_FD001 <- read.delim(dl, header = FALSE, sep = "", dec = ".")

download.file("https://drive.google.com/uc?export=download&id=1ki9J33OA8-_E-hA_YPmmoS2joYH094NB", dl)
test_FD001 <- read.delim(dl, header = FALSE, sep = "", dec = ".")

download.file("https://drive.google.com/uc?export=download&id=1lip3QctzuadXp4-rk8IE3rpu4DTEyVbY", dl)
RUL_FD001 <- read.delim(dl, header = FALSE, sep = "", dec = ".")


names(train_FD001) <- c("unit_no", "time_cycles", "op_1", "op_2", "op_3", "s_01", "s_02", "s_03", "s_04", "s_05", "s_06",
                        "s_07", "s_08", "s_09", "s_10", "s_11", "s_12", "s_13", "s_14", "s_15", "s_16", "s_17",
                        "s_18", "s_19", "s_20", "s_21")

names(test_FD001) <- c("unit_no", "time_cycles", "op_1", "op_2", "op_3", "s_01", "s_02", "s_03", "s_04", "s_05", "s_06",
                        "s_07", "s_08", "s_09", "s_10", "s_11", "s_12", "s_13", "s_14", "s_15", "s_16", "s_17",
                        "s_18", "s_19", "s_20", "s_21")

names(RUL_FD001) <- "RUL"

###########################
#Exploratory Data Analysis#
###########################

#Table 1: Dataset Dimensions
Table_1 <- data.frame(
  Dataset = c("train_FD001", "test_FD001"),
  No_of_Rows = c(nrow(train_FD001), nrow(test_FD001)),
  No_of_Cols = c(ncol(train_FD001), ncol(test_FD001)))

knitr::kable(
  Table_1,
  format = "pipe",
  caption = "Dataset Dimensions",
  col.names = c("Dataset", "No. of Rows", "No. of Columns"))


#Plot1: Distribution of Maximum Time Cycles
train_FD001 %>%
  group_by(unit_no) %>%
  summarise(n=n(), time_cycles = max(time_cycles)) %>%
  ggplot(aes(time_cycles)) +
  geom_histogram(binwidth = 10, color = "black") +
  labs(title="Maximum Time Cycles", x = "Time Cycles", y = "Count")

#Table 2: Preview of the dataset
train_FD001 %>%
  select(1:11) %>%
  head() %>% 
  knitr::kable(format = "pipe", caption = "Preview of the dataset")

#Table 3: Preview of the dataset2
train_FD001 %>%
  select(12:23) %>%
  head() %>% 
  knitr::kable(format = "pipe", caption = "Preview of the dataset")

#Table 4: Preview of the dataset3
train_FD001 %>%
  select(24:26) %>%
  head() %>% 
  knitr::kable(format = "pipe", caption = "Preview of the dataset")

#Adding a RUL column to the train set
train_FD001 <- train_FD001 %>% group_by(unit_no) %>% mutate(max_tc = max(time_cycles)) %>% 
  mutate(RUL = max_tc - time_cycles) %>% select(-max_tc)

# Distribution of RUL
train_FD001 %>%
  group_by(unit_no) %>%
  summarise(n=n(), RUL = max(RUL)) %>%
  ggplot(aes(RUL)) +
  geom_histogram(binwidth = 10, color = "black") +
  labs(title="Remaining Useful Life", x = "RUL", y = "Count")

train_FD001 %>%
  group_by(unit_no) %>%
  summarise(n=n(), RUL = max(RUL)) %>%
  ggplot(aes(RUL)) +
  geom_density(fill = "gray2", alpha = 0.5) +
  labs(title="Remaining Useful Life", x = "RUL", y = "Density")

#Sensor reading plots
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
sample_unit <- sample(seq(1:100), 10, replace = FALSE)

plot_df <- train_FD001 %>% filter(unit_no %in% sample_unit)

plot1 <- plot_df %>% ggplot(aes(x = RUL, y = s_03, group = unit_no)) +
  geom_line(color = plot_df$unit_no) +
  labs(title="s_03 sensor readings") +
  scale_x_reverse()+
  theme(legend.position = "none")

plot2 <- plot_df %>% ggplot(aes(x = RUL, y = s_07, group = unit_no)) +
  geom_line(color = plot_df$unit_no) +
  labs(title="s_07 sensor readings") +
  scale_x_reverse()+
  theme(legend.position = "none")

plot3 <- plot_df %>% ggplot(aes(x = RUL, y = s_05, group = unit_no)) +
  geom_line(color = plot_df$unit_no) +
  labs(title="s_05 sensor readings") +
  scale_x_reverse()+
  theme(legend.position = "none")

plot4 <- plot_df %>% ggplot(aes(x = RUL, y = s_06, group = unit_no)) +
  geom_line(color = plot_df$unit_no) +
  labs(title="s_06 sensor readings") +
  scale_x_reverse()

plot5 <- plot_df %>% ggplot(aes(x = RUL, y = s_14, group = unit_no)) +
  geom_line(color = plot_df$unit_no) +
  labs(title="s_14 sensor readings") +
  scale_x_reverse()+
  theme(legend.position = "none")

ggarrange(plot1, plot2, plot3, plot4, plot5, ncol = 2, nrow = 3)

#Removing features with no useful information
train_FD001 <- subset(train_FD001, select = - c(op_1, op_2, op_3, s_01, s_05, s_06, s_09, s_10, s_14, s_16, s_18, s_19))
test_FD001 <- subset(test_FD001, select = - c(op_1, op_2, op_3, s_01, s_05, s_06, s_09, s_10, s_14, s_16, s_18, s_19))


#Decomposing the sensor readings
remaining_sens <- c("s_02", "s_03", "s_04", "s_07", "s_08", "s_11", 
                    "s_12", "s_13", "s_15", "s_17", "s_20", "s_21")

#Smoothing the train set
for (i in remaining_sens) {x <- paste(i,"trend", sep = "_")
train_FD001 <- train_FD001 %>%
  group_by(unit_no) %>%
  mutate(!!x := fitted(loess(get(i) ~ time_cycles, span = 0.5)))%>%
  ungroup()}

#Smoothing the test set
for (i in remaining_sens) {x <- paste(i,"trend", sep = "_")
test_FD001 <- test_FD001 %>%
  group_by(unit_no) %>%
  mutate(!!x := fitted(loess(get(i) ~ time_cycles, span = 0.5)))%>%
  ungroup()}

#train_FD001 <- train_FD001 %>%
#group_by(unit_no) %>%
#mutate(s_02_trend = lm(s_02 ~ poly(time_cycles, 3))$fitted.values) %>%
#ungroup()

#Plot3: Sensor Trend Curves Sample
train_FD001 %>% filter(unit_no == 1) %>% ggplot(aes(x = time_cycles, y = s_07_trend)) +
  geom_line(aes(color = "s_07_trend"), col = "blue") +
  geom_line(aes(x = time_cycles, y = s_07, color = "s_07"), col = "darkgrey") +
  labs(title="unit_no 1, s_07 sensor readings")

#NASA Score function for train() function
NASA_Score <- function(data, lev = NULL, model = NULL){
  d <- data[, "pred"]- data[, "obs"]
  out <- ifelse(d >= 0, exp(d/10)-1, exp(-d/13)-1)
  out_sum <- sum(out)
  names(out_sum) <- "NASA_Score"
  return(out_sum)}

#Plot 4: Nasa score vs error plot
error <- seq(from = -50, to= 50 )

Score <- sapply(error, function(d){out <- ifelse(d >= 0, exp(d/10)-1, exp(-d/13)-1) 
                return(out)})

Score <- cbind(error, Score)
Score <- as.data.frame(Score) 

Score %>% ggplot(aes(x = error, y = Score)) +
  geom_line() +
  labs(title="NASA Score", x = "Error", y = "NASA Score") 


#################
#Building models#
#################

#Survival Analysis
#Adding an event column
train_FD001 <- train_FD001 %>% group_by(unit_no) %>% mutate(event = ifelse(RUL == 0, 1, 0))

#Kaplan-Meier Model
KM_df <- train_FD001 %>% filter(event==1)
survival_model <- Surv(time = KM_df$time_cycles, event = KM_df$event)
kaplan_fit <- survfit(survival_model ~ 1, data = KM_df)
summary(kaplan_fit)

##Plot 5:KM plot
ggsurvplot(kaplan_fit, data = KM_df,
           xlab = "Cycles", 
           ylab = "Overall survival probability")


#Baseline Linear Regression Model################################################################
##Divide train_FD001 to a test and a train set	
set.seed(1, sample.kind="Rounding") 
test_units <- sample(seq(1:100), 20, replace =FALSE)
train_set <- train_FD001 %>% filter(!unit_no %in% test_units)
test_set <- train_FD001 %>% filter(unit_no %in% test_units)
test_set <- test_set %>% filter(time_cycles >= 100) %>% group_by(unit_no) %>% slice_sample(n=1)

##Trying of the linear regression model with different cutoffs
cutoffs <- seq(from = 100, to = 150, by =5)
fitControl <- trainControl(summaryFunction = NASA_Score)

##Concatenate strings to create a formula
remaining_sens_trend <- paste(remaining_sens, "trend", sep = "_")
f <- paste(remaining_sens_trend,collapse=' + ')
f <- paste('RUL ~',f)

##Convert to formula
f <- as.formula(f)

##Scoring function for evaluation of the results
Scoring_func <- function(pred, obs){
  d <- pred-obs
  out <- ifelse(d >= 0, exp(d/10)-1, exp(-d/13)-1)
  out_sum <- sum(out)
  return(out_sum)}

##RMSE function for evaluation of the results
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))}

##The lm model
lm_result <- sapply(cutoffs, function(x){
  df <- train_set %>% filter(time_cycles >= x)
  lm_fit <- train(f, data = df, method = "lm",  maximize=FALSE, trControl = fitControl)
  ft <- predict(lm_fit, newdata = test_set)
  Sc <- Scoring_func(ft, test_set$RUL)
  return(Sc)})
lm_result <- as.data.frame(cbind(cutoffs, lm_result))

best_cutoff <- lm_result$cutoffs[which.min(lm_result$lm_result)]

##Plot 5:Searching for the best cutoff
lm_result %>%
  ggplot(aes(x = cutoffs, y = lm_result)) +
  geom_line() +
  labs(title="NASA Score vs. Cutoffs", x = "Cutoff", y = "NASA Score")


##Divide train_FD001 to a test and a train set again	
set.seed(1, sample.kind="Rounding") 
test_units <- sample(seq(1:100), 20, replace =FALSE)
train_set <- train_FD001 %>% filter(!unit_no %in% test_units)
test_set <- train_FD001 %>% filter(unit_no %in% test_units)
train_set <- train_set %>% filter(time_cycles >= 140) #best_cutoff

last_row <- test_set %>% filter(time_cycles >= 100) %>% group_by(unit_no) %>% 
  slice_sample(n = 1) %>% select(unit_no, RUL)

test_set2 <- test_set[0,] #Create an empty data frame based on the test_set

for (i in test_units) {x <- test_set %>% filter(unit_no == i & RUL >= last_row$RUL[which(last_row$unit_no == i)])
  test_set2 <- rbind(test_set2, x)} #Fill the empty data frame = test set

lm_fit <- train(f, data = train_set, method = "lm",  maximize=FALSE, trControl = fitControl) 

xs <- seq(1:10) #No. of rows for prediction

##Computing the scores based different no. of rows (xs)
lm_result <- sapply(xs, function(x){df <-  test_set2 %>% group_by(unit_no) %>% slice_tail(n = x)
  df["pred"] <- predict(lm_fit, newdata = df)
  df <- df %>% group_by(unit_no) %>% summarise(mean_pred = mean(pred), min_RUL = min(RUL))
  res <- Scoring_func(df$mean_pred, df$min_RUL)
  return(res)})

lm_result <- as.data.frame(cbind(xs, lm_result))

##Plot 6:Searching for the best x
lm_result %>%
  ggplot(aes(x = xs, y = lm_result)) +
  geom_line() +
  labs(title="NASA Score vs. x", x = "x", y = "NASA Score")


##Final lm model
##Selecting the last row for each unit in test_FD001
test_FD001 <- test_FD001 %>% group_by(unit_no) %>% slice_tail(n=1)
##Using the best cutoff on the train set
train_FD001 <- train_FD001 %>% filter(time_cycles >= best_cutoff)
##lm model
lm_fit <- train(f, data = train_FD001, method = "lm",  maximize=FALSE, trControl = fitControl)
##Prediction
lm_pred <- predict(lm_fit, newdata = test_FD001)
lm_pred <- ifelse(lm_pred < 0, 0, floor(lm_pred))
lm_result_Score <- Scoring_func(lm_pred, RUL_FD001$RUL)
lm_result_RMSE <- RMSE(lm_pred, RUL_FD001$RUL)
lm_result <- RUL_FD001$RUL - lm_pred

##Table 6:Final lm results
Summary_Table <- data.frame(Method = "lm",
                            Score = round(lm_result_Score),
                            RMSE = round(lm_result_RMSE, digits = 4),
                            Error = paste0("[", min(lm_result), ",", max(lm_result), "]"))

Result_Summary <- function(){knitr::kable(Summary_Table, 
                                          format = "pipe", 
                                          caption = "Performance Metrics")}

Result_Summary()

#kNN Model##########################################################
fitControl <- trainControl(summaryFunction = NASA_Score, method = "cv", number=5)
set.seed(1, sample.kind="Rounding")
knn_fit <- train(f, data = train_FD001, method = "knn",  maximize=FALSE, trControl = fitControl, 
                 tuneGrid = data.frame(k = c(70, 80, 90, 100))) 

saveRDS(knn_fit, "knn_fit.rds")
knn_fit <- readRDS("knn_fit.rds")

##Prediction
knn_pred <- predict(knn_fit, newdata = test_FD001)
knn_pred <- ifelse(knn_pred < 0, 0, floor(knn_pred))
knn_result_Score <- Scoring_func(knn_pred, RUL_FD001$RUL)
knn_result_RMSE <- RMSE(knn_pred, RUL_FD001$RUL)
knn_result <- RUL_FD001$RUL - knn_pred

ggplot(knn_fit, highlight = TRUE) +
  ggtitle("k ~ NASA Score")

Summary_Table <- Summary_Table %>% add_row(Method = "kNN", 
                                           Score = round(knn_result_Score),
                                           RMSE = round(knn_result_RMSE, digits = 4),
                                           Error = paste0("[", min(knn_result), ",", max(knn_result), "]"))
                                           
Result_Summary()


#randomForest Model##########################################################
fitControl <- trainControl(summaryFunction = NASA_Score)
rf_fit <- train(f, data = train_FD001, method = "rf",  maximize=FALSE, trControl = fitControl, 
                 tuneGrid = data.frame(mtry = c(3, 5, 7, 10, 12))) 

saveRDS(rf_fit, "rf_fit.rds")
rf_fit <- readRDS("rf_fit.rds")

##Prediction
rf_pred <- predict(rf_fit, newdata = test_FD001)
rf_pred <- ifelse(rf_pred < 0, 0, floor(rf_pred))
rf_result_Score <- Scoring_func(rf_pred, RUL_FD001$RUL)
rf_result_RMSE <- RMSE(rf_pred, RUL_FD001$RUL)
rf_result <- RUL_FD001$RUL - rf_pred

ggplot(rf_fit, highlight = TRUE) +
  ggtitle("mtry ~ NASA Score")

Summary_Table <- Summary_Table %>% add_row(Method = "RF", 
                                           Score = round(rf_result_Score),
                                           RMSE = round(rf_result_RMSE, digits = 4),
                                           Error = paste0("[", min(rf_result), ",", max(rf_result), "]"))

Result_Summary()


#SVR Model##########################################################
fitControl <- trainControl(summaryFunction = NASA_Score, method = "cv", number=5)
set.seed(1, sample.kind="Rounding")
svr_fit <- train(f, data = train_FD001, method = "svmLinear",  preProc = c("center","scale"),
                 maximize=FALSE, trControl = fitControl, tuneGrid = expand.grid(C = c(1, 10, 20, 50, 75, 100))) 

saveRDS(svr_fit, "svr_fit.rds")
svr_fit <- readRDS("svr_fit.rds")

##Prediction
svr_pred <- predict(svr_fit, newdata = test_FD001)
svr_pred <- ifelse(svr_pred < 0, 0, floor(svr_pred))
svr_result_Score <- Scoring_func(svr_pred, RUL_FD001$RUL)
svr_result_RMSE <- RMSE(svr_pred, RUL_FD001$RUL)
svr_result <- RUL_FD001$RUL - svr_pred

ggplot(svr_fit, highlight = TRUE) + 
  ggtitle("Cost parameter ~ NASA Score")

Summary_Table <- Summary_Table %>% add_row(Method = "SVR", 
                                           Score = round(svr_result_Score),
                                           RMSE = round(svr_result_RMSE, digits = 4),
                                           Error = paste0("[", min(svr_result), ",", max(svr_result), "]"))

Result_Summary()


#############
#Conclusions#
#############

#Prediction Error ~ Method Plot##########################################################
##Building the prediction error dataset
lm_result <- cbind(lm_result, c("lm"))
knn_result <- cbind(knn_result, c("knn"))
rf_result <- cbind(rf_result, c("RF"))
svr_result <- cbind(svr_result, c("SVR"))

all_results <- as.data.frame(rbind(lm_result, knn_result, rf_result, svr_result))

all_results$error <- as.numeric(all_results$error)

names(all_results) <- c("error", "method")

##Prediction Error ~ Method Plot
all_results %>%
  ggplot(aes(error, method, group = method, fill = method)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, scale = 1, alpha = 0.2) +
  scale_x_continuous(breaks = seq(-100, 100, by = 10)) +
  scale_y_discrete(labels = c("SVR", "RF", "kNN", "lm")) +
  labs(title="Prediction Error Density ~ Method", x = "Prediction Error", y = "Method")
  



