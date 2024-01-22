# covid-vaccines
Mathematical modeling to estimate hospitalizations and death averted by COVID-19 vaccines

## Introduction
The code here is tries to explain what we have done to analyze our data. First, we transformed our data set into a time series. The not-included file "**pacientes.csv**" was given to us by the Spanish Ministry of Health. Those data are open, and can be requested from [https://www.sanidad.gob.es/estadEstudios/estadisticas/estadisticas/estMinisterio/SolicitudCMBD.htm](https://www.sanidad.gob.es/estadEstudios/estadisticas/estadisticas/estMinisterio/SolicitudCMBD.htm). Due to our contract with the Spanish Ministry of Health, we cannot share these data, but we can share the code to analyze the file. So, we have uploaded the code in R. 

Once historical data on the number of hospitalizations from COVID-19 were collected (the dataset **pacientes.csv**), we prepared the data for analysis by checking for missing values, outliers, and inconsistencies. Then, we aggregated data on daily hospitalizations into appropriate time intervals to have a time series (**ingresos.csv**). Exploratory analyses were used to understand the trends and patterns in the historical data. This involved creating visualizations and summary statistics to identify any seasonality or trends in the occurrence of COVID-19.

## List fo files

- **README.md**. This file.
- **ingresos.csv**. The only dataset we can share. It is a time series of hospitalizations in Spain between 2020 and 2021.
- **comorbidities.R**. This file includes the categorization of diabetes, chronic liver disease, hypertension, chronic kidney disease, an so on. It is used in **0_Data_Processing.R**.
- **0_Data_Processing.R**. This file includes the processing of "**pacientes.csv**", that is, the file provided by the Spanish Ministry of Health. Should a researcher achived this dataset, it could be processed with this code.
- **1_Baseline_Time_Series**. With the dataset **ingresos.csv**, a nice figure can be plotted. If researchers cannot access to "**pacientes.csv**", BEGIN HERE.
- **2_ElasticNet.R**. It runs the ElasticNet algorithm using our time series.
- **3_RandomForest.R**. It runs the RandomForest algorithm using our time series.

## Forecasting with machine learning algorithms
With a clean data set, the next stage was to select a forecasting method. Estimating population data, specifically the number of events due to COVID-19, is a common but challenging task in epidemiology and clinical research. To make such predictions, statistical methods and models to forecast future trends can be used, but choosing an appropriate forecasting method depends on the characteristics of the data. More specifically, the choice of method depends on the complexity of the data and the availability of relevant predictor variables. As noted, common methods include time series analysis, regression analysis, and machine learning techniques. We discarded time series analysis methods such as Auto-Regressive Integrated Moving Average (ARIMA) because we were unable to capture and project time-dependent patterns in the data, given their nature. Specifically, ARIMA estimations did not converge in our data set, likely because the developed model was not a good fit for the data. Likewise, Markov Chains with Monte Carlo simulations (Markov chain Monte Carlo or MCMC) required some assumptions that could not be fulfilled. The characteristics of our data and the underlying dynamics of COVID-19 hospitalizations did not justify the choice of the normal distribution and the assumption of independence in time steps. In addition, adjustments and the fine-tuning of some parameters based on the likelihood of our data and the prior distributions were either too complex or unavailable.

We used two machine learning algorithms (mentioned above) because these can capture non-obvious (both linear and nonlinear) patterns in data. We evaluated each model’s performance using appropriate metrics such as mean absolute percentage error through cross-validation to ensure reliability for the training period (July 2020 to February 2021). Then we used each model to forecast future population hospitalizations for the desired time period (March to December 2021). We created point forecasts (single estimates) and prediction intervals (confidence intervals) to quantify the uncertainty in our predictions.
