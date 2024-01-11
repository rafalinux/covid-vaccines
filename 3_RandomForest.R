#!/usr/bin/Rscript

# 31 October 2023
# Time Series with RandomForest
#

######################################
#                                    #
# LOADING LIBRARIES                  #
#                                    #
######################################
library(ggplot2)
library(forecast)
library(timetk)
library(recipes)
library(glmnet)
library(parsnip)
library(workflows)
library(tidyverse)
library(tidyquant)
library(modeltime)
library(lubridate)
library(randomForest)

set.seed(12345)

##############################################################
#                                                            #
# DATA                                                       #
#                                                            #
##############################################################

ingresos <- read.table("ingresos.csv", header=TRUE, sep=",")
ingresos$Var1 <- as.Date(ingresos$Var1, "%Y-%m-%d")

#######################################
#                                     #
# ANALYSIS of the TIME SERIES         #
#                                     #
#######################################

# The common, simple command to convert our dataset into a time series would be:
# serie_temporal <- ts(ingresos$Freq1, frequency = 365.25, start = c(2020, 01, 01) )
# HOWEVER, we cannot use it "as is", because there are GAPS in the date that
# need to be fullfilled.
######################################

alldates <- data.frame(seq(as.Date('2020-01-01'), as.Date('2021-12-31'), by = "days"))
colnames(alldates) <- c('Var1')
ingresos.alldates <- merge(alldates, ingresos, by="Var1", all=TRUE)
ingresos.alldates[is.na(ingresos.alldates)] <- 0
rm(alldates)

# Now we can haver our time series
ingresos.timeseries <- ts(ingresos.alldates[,2], start = c(2020, 01, 01), frequency = 365.25)

# We can check the time series in a plot
plot.ts(ingresos.timeseries)

# We chose a window, removing the first wave, as it is an outlier
# that can interfere with the general trend of the pandemic
ingresos.ventana <- window(ingresos.timeseries, start = 2020.5, end = 2021.2)
plot.ts(ingresos.ventana)

#######################################
#                                     #
# TIME SERIES con RANDOM FOREST       #
#                                     #
#######################################
# Para que las fechas salgan en inglés
Sys.setlocale("LC_TIME", "en_GB.UTF-8") 

#######################################
#                                     #
# TIME SERIES con RANDOM FOREST (I)   #
#                                     #
#######################################
# https://www.pluralsight.com/guides/machine-learning-for-time-series-data-in-r

#######################################
#                                     #
# HOSPITALIZATIONS                    #
#                                     #
#######################################

# 1. TRAINING AND TEST SETS

train_tbl <- as_tibble( ingresos[ingresos$Var1 >= '2020-07-01' & ingresos$Var1 <= '2021-03-01',]  ) #second and third waves
test_tbl <- as_tibble( ingresos[ingresos$Var1 >= '2021-03-02' & ingresos$Var1 <= '2021-12-31',]  )#fourth and fifth waves

#Change the column name - Var1 to date
train_tbl <- train_tbl %>% 
  rename("date" = "Var1", "value" = "Freq")
test_tbl <- test_tbl %>% 
  rename("date" = "Var1", "value" = "Freq")
total_tbl <- bind_rows(train_tbl, test_tbl)

# 2. DATA FEATURES

recipe_spec_timeseries <- recipe(value ~ ., data = train_tbl) %>%
  step_timeseries_signature(date) 
bake(prep(recipe_spec_timeseries), new_data = train_tbl)

recipe_spec_final <- recipe_spec_timeseries %>%
  step_rm(date) %>%
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts")) %>%
  step_normalize(contains("index.num"), date_year) %>%
  step_interact(~ date_month.lbl * date_day) %>%
  step_interact(~ date_month.lbl * date_mweek) %>%
  step_interact(~ date_month.lbl * date_wday.lbl * date_yday) %>%
  step_dummy(contains("lbl"), one_hot = TRUE) 

recipe_spec_final <- recipe_spec_timeseries %>%
  step_fourier(date, period = 365, K = 5) %>%
  step_rm(date) %>%
  step_rm(contains("iso"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts")) %>%
  step_normalize(contains("index.num"), date_year) %>%
  step_dummy(contains("lbl"), one_hot = TRUE) 
juice(prep(recipe_spec_final))

# https://www.tidyverse.org/blog/2018/11/parsnip-0-0-1/
# https://cran.r-project.org/web/packages/parsnip/parsnip.pdf
# https://parsnip.tidymodels.org/articles/Examples.html   <- EXAMPLE OF RANDOM FOREST

rf_reg_spec <- 
  rand_forest(trees = 1000, min_n = 5) %>% 
  # This model can be used for classification or regression, so set mode
  set_mode("regression") %>% 
  set_engine("randomForest")
rf_reg_spec

workflow_rf <- workflow() %>%
  add_recipe(recipe_spec_final) %>%
  add_model(rf_reg_spec)

workflow_rf

workflow_trained <- workflow_rf %>% fit(data = train_tbl)

#predicción con los datos de entrenamiento
prediction_tbl_train <- workflow_trained %>% 
  predict(train_tbl) %>%
  bind_cols(train_tbl) 

prediction_tbl_train

#predicción/forecasting con los datos de testeo
prediction_tbl <- workflow_trained %>% 
  predict(test_tbl) %>%
  bind_cols(test_tbl) 

prediction_tbl

# Model Evaluation Metrics 
#https://www.pluralsight.com/guides/machine-learning-for-time-series-data-in-r
# After creating the training and test set, build the machine learning model. 
# Before doing that, it's important to decide on the evaluation metric. The evaluation metric to be used is 
# Mean Absolute Percentage Error (or MAPE). The lower the MAPE value, the better the forecasting model. 
# The code below creates a utility function for calculating MAPE, which will be used to evaluate 
# the performance of the model.
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

mape(prediction_tbl_train$value, prediction_tbl_train$.pred)
#[1] 5.899117   <- Can RandomForest be better than ElasticNet? Maybe.

# 5. PLOTTING FITTED DATA

titulo <- expression(paste("Observed ", italic("versus"), " estimated hospitalizations due to COVID-19"))

ggplot(aes(x = date), data = total_tbl) +
  geom_vline(xintercept = as.Date("2021-03-02"), linetype="dashed", color = "grey", size=.8) +
  annotate("text", x = ymd("2020-11-01"), y = 3000,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2021-07-01"), y = 3000,
           color = palette_light()[[1]], label = "Forecast Region") + 
  geom_point(aes(x = date, y = value),  
             alpha = 0.5, color = "#00AFBB") +
  # Add predictions
  geom_point(aes(x = date, y = .pred*1), data = prediction_tbl, 
             alpha = 0.5, color = "red") +
  ylim(limits=c(0,3500)) +
  scale_x_date(date_labels = "%B-%Y", date_breaks = '2 months') +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black"),
        axis.text=element_text(size=8),  #tamaño de las fechas
        axis.title.x = element_text(vjust=-0.2),
        axis.title.y = element_text(vjust=+0.6),
        axis.title=element_text(size=10,face="bold"), #tamaño de los títulos de los ejes
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
        plot.caption = element_text(size = 7.5, color = "grey40")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank() ) +
  #labs(title = "GLM: Out-Of-Sample Forecast - Admissions")
  labs(title = titulo,
       subtitle="Forecast using Random Forest regression")+  
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(aes(x = date), data = total_tbl) +
  geom_vline(xintercept = as.Date("2021-03-02"), linetype="dashed", color = "grey", size=.8) +
  annotate("text", x = ymd("2020-11-01"), y = 3000,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2021-07-01"), y = 3000,
           color = palette_light()[[1]], label = "Forecast Region") + 
  geom_point(aes(x = date, y = value),  
             alpha = 0.5, color = "#00AFBB") +
  # Add predictions
  geom_point(aes(x = date, y = .pred*1), data = prediction_tbl_train, 
             alpha = 0.5, color = "red") +
  ylim(limits=c(0,3500)) +
  scale_x_date(date_labels = "%B-%Y", date_breaks = '2 months') +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black"),
        axis.text=element_text(size=8),  
        axis.title.x = element_text(vjust=-0.2),
        axis.title.y = element_text(vjust=+0.6),
        axis.title=element_text(size=10,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
        plot.caption = element_text(size = 7.5, color = "grey40")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank() ) +
  #labs(title = "GLM: Out-Of-Sample Forecast - Admissions")
  labs(title = titulo,
       subtitle="Fitted predictions using Random Forest regression")+  
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(aes(x = date), data = total_tbl) +
  geom_vline(xintercept = as.Date("2021-03-02"), linetype="dashed", color = "grey", size=.8) +
  annotate("text", x = ymd("2020-11-01"), y = 3000,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2021-07-01"), y = 3000,
           color = palette_light()[[1]], label = "Forecast Region") + 
  geom_point(aes(x = date, y = value),  
             alpha = 0.5, color = "#00AFBB") +
  # Add predictions TESTING
  geom_point(aes(x = date, y = .pred*1), data = prediction_tbl, 
             alpha = 0.5, color = "red") +
  # Add predictions TRAINING
  geom_point(aes(x = date, y = .pred*1), data = prediction_tbl_train, 
             alpha = 0.5, color = "red") +
  ylim(limits=c(0,3500)) +
  scale_x_date(date_labels = "%B-%Y", date_breaks = '2 months') +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black"),
        axis.text=element_text(size=8),  
        axis.title.x = element_text(vjust=-0.2),
        axis.title.y = element_text(vjust=+0.6),
        axis.title=element_text(size=10,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
        plot.caption = element_text(size = 7.5, color = "grey40")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank() ) +
  labs(title = titulo,
       subtitle="Fitted predictions using Random Forest regression")+  
  theme(plot.title = element_text(hjust = 0.5)) 


# ADDING MOVING AVERAGE AND CONFIDENCE INTERVAL

# Calculate standard deviation of residuals (with the training data, not with the testing data)
test_resid_sd <- prediction_tbl_train %>%
  summarize(stdev = sd(value - .pred))

prediction_tbl <- prediction_tbl %>%
  mutate(
    lo.95 = .pred - 1.96 * test_resid_sd$stdev,
    lo.80 = .pred - 1.28 * test_resid_sd$stdev,
    hi.80 = .pred + 1.28 * test_resid_sd$stdev,
    hi.95 = .pred + 1.96 * test_resid_sd$stdev
  )

# MOVING AVERAGE with "zoo" library
rolling_average <- rollmean(prediction_tbl$.pred, 7) #moving average a 7 días
rolling_average <- append(c(NA,NA,NA,NA,NA,NA), rolling_average)
prediction_tbl <- cbind(prediction_tbl, moving_average=rolling_average)
cum_prediction_tbl <- cumsum(prediction_tbl$.pred)
cum_prediction_observada <- cumsum(prediction_tbl$value)
cum_prediction_pred_hi.95 <- cumsum(prediction_tbl$hi.95)
cum_prediction_pred_lo.95 <- cumsum(prediction_tbl$lo.95)
prediction_tbl <- cbind(prediction_tbl, 
                        acumulados.pred=cum_prediction_tbl,
                        acumulados.observados=cum_prediction_observada,
                        acumulados.pred.hi.95=cum_prediction_pred_hi.95,
                        acumulados.pred.lo.95=cum_prediction_pred_lo.95)

ggplot() + 
  geom_vline(xintercept = as.Date("2021-03-02"), linetype="dashed", color = "grey", size=.8) +
  annotate("text", x = ymd("2020-11-01"), y = 3000,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2021-07-01"), y = 3000,
           color = palette_light()[[1]], label = "Forecast Region") + 
  geom_ribbon(aes(x =date, ymin = lo.95, ymax = hi.95), alpha=0.8, data = prediction_tbl, fill = "#D5DBFF", color = NA, size = 0)+
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, x = date), data = prediction_tbl, fill = "#596DD5", color = NA, size = 0, alpha = 0.6) +
  geom_point(data=total_tbl[total_tbl$date <= "2021-03-02", ], aes(x = date, y = value),  alpha = 0.5, color = "#00AFBB") +
  #geom_point(data=total_tbl, aes(x = date, y = value),  alpha = 0.5, color = "#00AFBB") +
  geom_point(aes(x = date, y = .pred*1), data = prediction_tbl, alpha = 0.5, color = "red") +
  geom_line(aes(x = date, y = moving_average), data = prediction_tbl, color = "white", size = .75) + 
  scale_x_date(date_labels = "%B-%Y", date_breaks = '2 months') +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black"),
        axis.text=element_text(size=8),  
        axis.title.x = element_text(vjust=-0.2),
        axis.title.y = element_text(vjust=+0.6),
        axis.title=element_text(size=10,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
        plot.caption = element_text(size = 7.5, color = "grey40")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank() ) +
  labs(title = titulo,
       subtitle="Forecast using Random Forest regression")+  
  theme(plot.title = element_text(hjust = 0.5)) 

##########################################
#                                        #
# CUMULATIVE HOSPITALIZATIONS            #
#                                        #
##########################################
bp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

colors <- c("Estimated" = "#0072B2", "Observed" = "#009E73")
ggplot(data = predicciones_ingresos, aes(x=date) ) +
  geom_line(aes(y = acumulados.pred, color="Estimated")) +
  geom_ribbon(aes(ymin = acumulados.pred.lo.95, ymax = acumulados.pred.hi.95), alpha=0.8, fill = "#D5DBFF", color = NA, size = 0)+
  #geom_point(color = "#00AFBB", size = 1) +
  geom_line(aes(y = acumulados.observados, color = "Observed"), group=1) +
  scale_x_date(date_labels = "%B-%Y", date_breaks = '2 months') +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black"),
        axis.text=element_text(size=8),  #tamaño de las fechas
        axis.title.x = element_text(vjust=-0.2),
        axis.title.y = element_text(vjust=+0.6),
        axis.title=element_text(size=10,face="bold"), #tamaño de los títulos de los ejes
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
        plot.caption = element_text(size = 7.5, color = "grey40")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank() ) +
  theme(legend.title = element_blank()) +
  labs(title = "Hospitalizations averted by COVID-19 vaccinations using the RandomForest model",
       subtitle="Cumulative number of hospitalizations (observed versus estimated)",
       color="Legend" ) +  
  theme(legend.position = c(.25, .75) ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = colors)
