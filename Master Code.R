#### Code for Master Thesis - "The Optimal Time-Varying Factor Portfolio" Jakob Skriver Matthisson - S127909. Copenhagen Business School ###

# We clear the environment to start fresh
rm(list = ls())
# Load in the needed packages
library(dplyr)
library(moments)
library(tidyr)
library(lubridate)
library(tidyverse)
library(scales)
library(ggplot2)
library(tidyquant)
library(zoo)
library(reshape2)
library(corrplot)
library(lmtest)
library(sandwich)
library(fastDummies)
library(gtable)
library(gt)
library(timetk)
library(rsample)
library(tsibble)
library(matlib)
library(data.table)
library(CVXR)
library(stargazer)
library(IntroCompFinR)
library(readxl)
library(googledrive)
library(rmote)
library(sandwich)
library(car)
options(scipen = 999)
library(rugarch)
library(rmgarch)
library(roll)
library(xtable)

# All Data is read in from the GitHub-locations

## Analysis for 4.1

# Loading in Monthly FF data and getting all data in percentage terms.
monthly = read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/MonthlyFF.csv'))
monthly$month = floor_date(ymd(str_c(monthly$month,'01')),'month')
monthly = monthly%>%
  mutate(across(where(is.numeric),~ . / 100))
# Unlinke all other factor long-shorts BAB is not constructed as an excess return due to its leverage.
# Therefore, as we want to have everything in excess return terms, we need to subtract the risk-free rate.
monthly$BAB=monthly$BAB-monthly$RF

# Loading in Daily FF and rate data modelling it to percentages and adjusting BAB
daily = read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/FactorsDaily.csv'))
daily = drop_na(daily)
daily<-transform(daily,date=as.Date(as.character(date),"%Y%m%d"))
daily = daily%>%
  mutate(across(where(is.numeric),~ . / 100))

# Load in the AQR Data
Quality = read.csv('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/QualityFactor.csv')
Quality<-transform(Quality,date = as.Date(as.character(date), '%m/%d/%Y'))
BAB = read.csv('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/BAB.csv')
BAB<-transform(BAB,date = as.Date(as.character(date), '%m/%d/%Y'))
R_free = read.csv('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/Risk_Free_Daily.csv')
R_free<-transform(R_free,date=as.Date(as.character(date),"%Y%m%d"))
R_free$RF = R_free$RF/100
daily = merge(daily, Quality, by = 'date')
daily = merge(daily,BAB, by = 'date')
daily = merge(daily,R_free, by = 'date')
daily$BAB=daily$BAB-daily$RF

# Later in the analysis we need to know which quarter and month we are in. Hence, we add these details

daily$Quarter = as.yearqtr(daily$date)
daily$month = as.yearmon(daily$date)

# Create Figure 1.1:
plot(daily$date[252:NROW(daily)],rollmean(daily$HML, 252), type = 'l', ylab = 'Daily Excess Return', xlab = 'Date', main = 'Rolling 252-day Excess Return on the Value Factor')

# Throughout this analysis we will use both total and excess return. Hence, we create a dataframe called "daily_full" for the total return
# including the risk-free rate. This will be used extensively throughout the code.

# Generate Basic summary statistics for daily and monthly excess returns

sumstats = matrix(nrow = 8, ncol = 8)

for (i in 2:9){
  sumstats[1,i-1]=mean(daily[,i])*100
  sumstats[2,i-1]=sd(daily[,i])*100
  sumstats[3,i-1]=skewness(daily[,i])
  sumstats[4,i-1] = kurtosis(daily[,i])
  sumstats[5,i-1] = mean(daily[,i])/sd(daily[,i])
  sumstats[6,i-1] = lm(data = daily, daily[,i] ~ Mkt.RF)[[1]][1]*100
  sumstats[7,i-1] = lm(data = daily, daily[,i] ~ Mkt.RF)[[1]][2]
  sumstats[8,i-1] = t.test(daily[,i])[[1]]
  sumstats = round(sumstats, 4)
}
col_names = list('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
rownames(sumstats) = c('Ex. Ret (%)', 'Std. Dev. (%)', 'Skewness', 'Kurtosis', 'Sharpe', 'CAPM Alpha (%)', 'CAPM Beta', 'T-test')
colnames(sumstats) = col_names
# Throughout, I use the xtable function to export the results directly to LaTeX.
print(xtable(sumstats, type = 'latex', caption = 'Summary Statistics of Daily Returns on Factors, Jul 1st, 1963 - Nov 30, 2022', digits = 3))
## Correlation Analysis of Excess Returns
# Constant correlation
correlation_matrix = cor(daily[,2:9], use = 'complete.obs')

# Plot of the correlations

corrplot(correlation_matrix, method = 'number', type = 'lower', main = 'Factor Correlations on Daily Data', mar=c(0,0,1,0))

# Rolling Correlation of Excess Returns
# Calculate rolling correlation and create figure 3.
rolling_correlation <- roll_cor(daily$HML, daily$SMB, width = 252)
rolling_correlation_mom_val <- roll_cor(daily$HML, daily$Momentum, width = 252)
timeseriesindex = daily[1:length(rolling_correlation_mom_val), 'date']
rolling_correlation_qual_val <- roll_cor(daily$HML, daily$Quality, width = 252)
plot(x = timeseriesindex, y = rolling_correlation, type = "l", xlab = "Time", ylab = "Rolling Correlation", main = 'Rolling Correlation for Selected Factors', ylim = c(-1,1))
lines(x = timeseriesindex, y = rolling_correlation_mom_val, col = 'green')
lines(x = timeseriesindex, y  = rolling_correlation_qual_val, col = 'red')
legend('topleft',legend = c('Size-Value', 'Value-Momentum', 'Quality-Value'), col = c('black', 'green', 'red'), lty = 1)


## Analysis 4.1.1 Performance Across Business Cycles
# Read in OECD data, clean it, and create the indicator.
OECD = read.csv('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/OECD.csv')
OECD$month = as.Date(paste(OECD$TIME, "-01", sep=""))
OECD$Delta = (OECD$OECD_Indicator - dplyr::lag(OECD$OECD_Indicator))/OECD$OECD_Indicator
OECD$OECD_econstage = ifelse(OECD$OECD_Indicator<100 & OECD$Delta<0, 'Downturn', ifelse(OECD$OECD_Indicator>100 & OECD$Delta<0, "Slowdown", ifelse(OECD$OECD_Indicator>100 & OECD$Delta>0, "Expansion", "Recovery")))
OECD = subset(OECD, select = -c(TIME, OECD_Indicator, Delta))
# Load in NBER Data, with already classified busines-cycle-stages.

NBER = read.csv('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/NBER%20Recessions.csv')
NBER$month = as.Date(NBER$DATE, "%d/%m/%Y")
NBER = subset(NBER, select = -c(USREC, DATE))

# Loading in GDP data (quarterly)

GDP = read.csv('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/GDP.csv')
GDP$DATE = as.Date(GDP$DATE, "%d/%m/%Y")
# To construct the GDP variable indicator per Ilmanen et al (2021) I calculate the yearOverYear change as well as change in growth
GDP = GDP %>% group_by(month=month(DATE)) %>%
  arrange(DATE) %>%
  mutate(yearOverYear=GDP/lag(GDP,1)-1) %>%
  ungroup() %>% arrange(DATE)

GDP$GrowthChange = GDP$yearOverYear-dplyr::lag(GDP$yearOverYear)
# I now follow Ilmanen et al (2021) and over a rolling 40 quarters (10 years) calculate the z-score of growth and change in growth
# based on this score, I allocate the economic period.
rollz<-function(x, width){
avg=rollapply(x, width, mean, na.rm = T, fill=NA, partial = T)
  std=rollapply(x, width, sd, na.rm = T, fill = NA, partial = T)
  z=(x-avg)/std
  return(z)
}

GDP$zgrowth = abs(rollz(GDP$yearOverYear, 40))
GDP$zgrowth_change = abs(rollz(GDP$GrowthChange, 40))

GDP$GDP_econstage = "Expansion" # Prefilling
GDP$GDP_econstage = ifelse(GDP$zgrowth<1 & GDP$zgrowth_change<1, dplyr::lag(GDP$GDP_econstage),
                                                                            ifelse(GDP$yearOverYear>0 & GDP$GrowthChange>0, 'Expansion', ifelse(GDP$yearOverYear<0 & GDP$GrowthChange>0, 'Recovery',
                                                                                                                                          ifelse(GDP$yearOverYear>0 & GDP$GrowthChange<0, 'Slowdown', 'Contraction'))))
# Merge the monthly data and the Econ data
monthly$Quarter = as.yearqtr(monthly$month)
GDP$Quarter = as.yearqtr(GDP$DATE)
GDP = subset(GDP, select = c(Quarter, GDP_econstage))
NBER$month = as.yearmon(NBER$month)
OECD$month = as.yearmon(OECD$month)
monthly$month = as.yearmon(monthly$month)

monthlyMerged = merge(monthly, GDP, by='Quarter')
monthlyMerged = merge(monthlyMerged, NBER, by = 'month')
monthlyMerged = merge(monthlyMerged, OECD, by = 'month' )

# Merge the daily data and the Econ data
dailyMerged = merge(daily, GDP, by = 'Quarter')
dailyMerged = merge(dailyMerged, NBER, by = 'month')
dailyMerged = merge(dailyMerged, OECD, by = 'month' )
# Calculate excess returns in each of the economic cycles by the three classification metrics. 
col_names = list('EconStage', 'Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
# Define a function to do the summary statistics.

perf_econ_stages = function(classification, title){
  PerfEconStages = dailyMerged%>%
    group_by(.data[[classification]])%>%
    summarise_at(4:11,funs(mean = mean))
  PerfEconStages = mutate_if(PerfEconStages, is.numeric,~ . * 100)
  PerfEconStages = mutate_if(PerfEconStages, is.numeric, round, digits = 4)
  Count = as.data.frame(dailyMerged%>%
                          group_by(.data[[classification]])%>%
                          summarise(n()))
  PerfEconStages$Count = Count[,2]
  names(PerfEconStages) = c(append(col_names,'Count'))
  print(xtable(PerfEconStages, type = 'latex', caption = title, digits = 3))
  
}

# Calculate the statistics
perf_econ_stages('GDP_econstage', 'Table 5: Daily Excess Returns (%) of Factors in Economic Stages, Classified by GDP')
perf_econ_stages('NBER_econstage', 'Table 6: Excess Monthly Returns of Factors in Economic Stages, Classified by NBER')
perf_econ_stages('OECD_econstage', 'Table 7 Excess Monthly Returns (%) of Factors in Economic Stages, Classified by OECD')

## Correlation across economic stages

col_names = list('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
row_names = list('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
# Define a function to do the calculations and 
corr_matrix_function = function(classification, stage, title){
  corr_matrix = (round(as.data.frame(cor(dailyMerged[monthlyMerged[classification] == stage, ][, 4:11])),4))
  names(corr_matrix) = c(col_names)
  rownames(corr_matrix)= row_names
  print(xtable(corr_matrix, caption = title, digits = 3))
}
# Do correlation matrices across the expansion and downturn of each classification method
corr_matrix_function('GDP_econstage', 'Expansion', 'Correlation Matrix - GDP Expansion')
corr_matrix_function('GDP_econstage', 'Contraction', ' Correlation Matrix - GDP Contraction')
corr_matrix_function('GDP_econstage', 'Recovery', 'Correlation Matrix - GDP Recovery')
corr_matrix_function('NBER_econstage', 'Pure-Expansion', 'Correlation Matrix - NBER Pure-Expansion')
corr_matrix_function('NBER_econstage', 'Recession', 'Correlation Matrix - NBER Recession')
corr_matrix_function('NBER_econstage', 'Post-Recession', 'Correlation Matrix - NBER Post-Recession')
corr_matrix_function('OECD_econstage', 'Expansion', 'Correlation Matrix - OECD Expansion')
corr_matrix_function('OECD_econstage', 'Downturn', 'Correlation Matrix - OECD Downturn')
corr_matrix_function('OECD_econstage', 'Recovery', 'Correlation Matrix - OECD Recovery')

## Analysis 4.2 Predictive Regressions:

# Read in the macroeconomic predictors according to Welch & Goyal (2008)
drive_deauth()

macro_predictors_link <-
  "https://docs.google.com/spreadsheets/d/1OArfD2Wv9IvGoLkJ8JyoXS0YMQLDZfY2"

drive_download(
  macro_predictors_link,
)

start_date=ymd('1920-01-01')
end_date = ymd('2022-12-31')

macro_predictors <- read_xlsx(
  "PredictorData2021.xlsx",
  sheet = "Monthly"
) |>
  mutate(month = ym(yyyymm)) |>
  mutate(across(where(is.character), as.numeric)) |>
  mutate(
    IndexDiv = Index + D12,
    logret = log(IndexDiv) - log(lag(IndexDiv)),
    Rfree = log(Rfree + 1),
    rp_div = lead(logret - Rfree, 1), # Future excess market return
    dp = log(D12) - log(Index), # Dividend Price ratio
    dy = log(D12) - log(lag(Index)), # Dividend yield
    ep = log(E12) - log(Index), # Earnings price ratio
    de = log(D12) - log(E12), # Dividend payout ratio
    tms = lty - tbl, # Term spread
    dfy = BAA - AAA # Default yield spread
  ) |>
  select(month, rp_div, dp, dy, ep, de, svar,
         bm = `b/m`, ntis, tbl, lty, ltr,
         tms, dfy
  ) |>
  filter(month >= start_date & month <= end_date)

# Read in the CPI data from FRED (2023a)
cpi_monthly <- tq_get("CPIAUCSL",
                      get = "economic.data",
                      from = start_date,
                      to = end_date
) |>
  transmute(
    month = floor_date(date, "month"),
    cpi = price / price[month == max(month)]
  )

# I calculate the inflation

cpi_monthly$cpi_growth = (cpi_monthly$cpi - dplyr::lag(cpi_monthly$cpi))/dplyr::lag(cpi_monthly$cpi)
macro_predictors = merge(macro_predictors, cpi_monthly, by = 'month')

names(macro_predictors)[names(macro_predictors) == "cpi_growth"] <- "infl"

# Read in the sentiment index (Wurgler, 2023) and merge it onto the predictor data from before

Sentiment = read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/Sentiment.csv'))
Sentiment$month = as.Date(as.character(paste(Sentiment$Date, "01", sep="")), format = '%Y%m%d')
Sentiment = subset(Sentiment,select = -c(Date))
macro_predictors = merge(macro_predictors, Sentiment, by = 'month')
macro_predictors$month = as.yearmon(macro_predictors$month)
# Merge the predictor data onto the data frame with monthly data and econ-stage data.
monthlyMerged = merge(monthlyMerged, macro_predictors, by = 'month')
monthlyMerged = dummy_cols(monthlyMerged, select_columns = c('OECD_econstage', 'NBER_econstage', 'GDP_econstage'))

# Now we can do the regressions. All are done with Newey West (1986) standard errors.
# The implementation of Newey-West standard errors is done following Hanck et al (2023)
# We start with the lagged regressions based on OECD.
m<-floor(0.75*length(monthlyMerged$SMB)^(1/3)) # Settings for the Newey-West Errors
reg_fit_mom = lm(monthlyMerged$mom ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
             dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
             dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+
               dplyr::lag(monthlyMerged$OECD_econstage_Expansion)+dplyr::lag(monthlyMerged$OECD_econstage_Recovery)+dplyr::lag(monthlyMerged$OECD_econstage_Slowdown), na.action=na.exclude)
NW_VCOV<-NeweyWest(reg_fit_mom,prewhite = F,adjust = T)
reg_data_mom = coeftest(reg_fit_mom,vcov=NW_VCOV)

reg_fit_val = lm(monthlyMerged$HML ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$OECD_econstage_Slowdown)+
                   dplyr::lag(monthlyMerged$OECD_econstage_Expansion)+dplyr::lag(monthlyMerged$OECD_econstage_Recovery), na.action=na.exclude)


NW_VCOV<-NeweyWest(reg_fit_val,lag=m-1,prewhite = F,adjust = T)
reg_data_val = coeftest(reg_fit_val,vcov=NW_VCOV)

reg_fit_size = lm(monthlyMerged$SMB ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+
                    dplyr::lag(monthlyMerged$OECD_econstage_Slowdown)+
                    dplyr::lag(monthlyMerged$OECD_econstage_Expansion)+dplyr::lag(monthlyMerged$OECD_econstage_Recovery), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_size,lag=m-1,prewhite = F,adjust = T)
reg_data_size = coeftest(reg_fit_size,vcov=NW_VCOV)

reg_fit_inv = lm(monthlyMerged$CMA ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                    dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                    dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$OECD_econstage_Slowdown)+
                   dplyr::lag(monthlyMerged$OECD_econstage_Expansion)+dplyr::lag(monthlyMerged$OECD_econstage_Recovery), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_inv,lag=m-1,prewhite = F,adjust = T)
reg_data_inv = coeftest(reg_fit_inv,vcov=NW_VCOV)

reg_fit_earn = lm(monthlyMerged$RMW ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$OECD_econstage_Slowdown)+
                    dplyr::lag(monthlyMerged$OECD_econstage_Expansion)+dplyr::lag(monthlyMerged$OECD_econstage_Recovery), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_earn,lag=m-1,prewhite = F,adjust = T)
reg_data_earn = coeftest(reg_fit_earn,vcov=NW_VCOV)

reg_fit_qual = lm(monthlyMerged$Quality ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                    dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                    dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$OECD_econstage_Slowdown)+
                    dplyr::lag(monthlyMerged$OECD_econstage_Expansion)+dplyr::lag(monthlyMerged$OECD_econstage_Recovery), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_qual,lag=m-1,prewhite = F,adjust = T)
reg_data_qual = coeftest(reg_fit_qual,vcov=NW_VCOV)

reg_fit_bab = lm(monthlyMerged$BAB ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                    dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                    dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$OECD_econstage_Slowdown)+
                   dplyr::lag(monthlyMerged$OECD_econstage_Expansion)+dplyr::lag(monthlyMerged$OECD_econstage_Recovery), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_bab,lag=m-1,prewhite = F,adjust = T)
reg_data_bab = coeftest(reg_fit_bab,vcov=NW_VCOV)

# Now we can collect the regressions and collect them in a table using LaTeX

my_models = list(reg_data_val, reg_data_size, reg_data_inv, reg_data_earn, reg_data_mom, reg_data_qual, reg_data_bab)
stargazer(my_models, title = 'Regressions', type = 'html', out = 'training output', keep.stat = 'rsq')

mom2 =  round(summary(reg_fit_mom)$adj.r.squared, 4)
value_r2 = round(summary(reg_fit_val)$adj.r.squared, 4)
size_r2 = round(summary(reg_fit_size)$adj.r.squared, 4)
inv_r2 = round(summary(reg_fit_inv)$adj.r.squared, 4)
earn_r2 = round(summary(reg_fit_earn)$adj.r.squared, 4)
qual_r2 = round(summary(reg_fit_qual)$adj.r.squared, 4)
bab_r2 = round(summary(reg_fit_bab)$adj.r.squared, 4)

star = stargazer(my_models,
                 add.lines = list(c("Adjusted $R^2$", value_r2, size_r2, inv_r2, earn_r2, mom2, qual_r2, bab_r2))
)
cat(star, sep = '\n', file = 'OECD.tex')

# Now lagged NBER regressions

reg_fit_mom = lm(monthlyMerged$mom ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$`NBER_econstage_Post-Recession`)+
                   dplyr::lag(monthlyMerged$`NBER_econstage_Pre-Recession`)+dplyr::lag(monthlyMerged$`NBER_econstage_Pure-Expansion`), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_mom,lag=m-1,prewhite = F,adjust = T)
reg_data_mom = coeftest(reg_fit_mom,vcov=NW_VCOV)

reg_fit_val = lm(monthlyMerged$HML ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$`NBER_econstage_Post-Recession`)+
                   dplyr::lag(monthlyMerged$`NBER_econstage_Pre-Recession`)+dplyr::lag(monthlyMerged$`NBER_econstage_Pure-Expansion`), na.action=na.exclude)


NW_VCOV<-NeweyWest(reg_fit_val,lag=m-1,prewhite = F,adjust = T)
reg_data_val = coeftest(reg_fit_val,vcov=NW_VCOV)

reg_fit_size = lm(monthlyMerged$SMB ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$`NBER_econstage_Post-Recession`)+
                   dplyr::lag(monthlyMerged$`NBER_econstage_Pre-Recession`)+dplyr::lag(monthlyMerged$`NBER_econstage_Pure-Expansion`), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_size,lag=m-1,prewhite = F,adjust = T)
reg_data_size = coeftest(reg_fit_size,vcov=NW_VCOV)

reg_fit_inv = lm(monthlyMerged$CMA ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                    dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                    dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$`NBER_econstage_Post-Recession`)+
                    dplyr::lag(monthlyMerged$`NBER_econstage_Pre-Recession`)+dplyr::lag(monthlyMerged$`NBER_econstage_Pure-Expansion`), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_inv,lag=m-1,prewhite = F,adjust = T)
reg_data_inv = coeftest(reg_fit_inv,vcov=NW_VCOV)

reg_fit_earn = lm(monthlyMerged$RMW ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                    dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                    dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$`NBER_econstage_Post-Recession`)+
                    dplyr::lag(monthlyMerged$`NBER_econstage_Pre-Recession`)+dplyr::lag(monthlyMerged$`NBER_econstage_Pure-Expansion`), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_earn,lag=m-1,prewhite = F,adjust = T)
reg_data_earn = coeftest(reg_fit_earn,vcov=NW_VCOV)

reg_fit_qual = lm(monthlyMerged$Quality ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                    dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                    dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$`NBER_econstage_Post-Recession`)+
                    dplyr::lag(monthlyMerged$`NBER_econstage_Pre-Recession`)+dplyr::lag(monthlyMerged$`NBER_econstage_Pure-Expansion`), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_qual,lag=m-1,prewhite = F,adjust = T)
reg_data_qual = coeftest(reg_fit_qual,vcov=NW_VCOV)

reg_fit_bab = lm(monthlyMerged$BAB ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                    dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                    dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$`NBER_econstage_Post-Recession`)+
                    dplyr::lag(monthlyMerged$`NBER_econstage_Pre-Recession`)+dplyr::lag(monthlyMerged$`NBER_econstage_Pure-Expansion`), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_bab,lag=m-1,prewhite = F,adjust = T)
reg_data_bab = coeftest(reg_fit_bab,vcov=NW_VCOV)


my_models = list(reg_data_val, reg_data_size, reg_data_inv, reg_data_earn, reg_data_mom, reg_data_qual, reg_data_bab)
stargazer(my_models, title = 'Regressions', type = 'html', out = 'training output', keep.stat = 'rsq')

mom_r2 =  round(summary(reg_fit_mom)$adj.r.squared, 4)
value_r2 = round(summary(reg_fit_val)$adj.r.squared, 4)
size_r2 = round(summary(reg_fit_size)$adj.r.squared, 4)
inv_r2 = round(summary(reg_fit_inv)$adj.r.squared, 4)
earn_r2 = round(summary(reg_fit_earn)$adj.r.squared, 4)
qual_r2 = round(summary(reg_fit_qual)$adj.r.squared, 4)
bab_r2 = round(summary(reg_fit_bab)$adj.r.squared, 4)

star = stargazer(my_models,
                 add.lines = list(c("Adjusted $R^2$", value_r2, size_r2, inv_r2, earn_r2, mom_r2, qual_r2, bab_r2))
)
cat(star, sep = '\n', file = 'NBER.tex')


## Lagged GDP regressions

reg_fit_mom = lm(monthlyMerged$mom ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$GDP_econstage_Expansion)+
                   dplyr::lag(monthlyMerged$GDP_econstage_Recovery)+dplyr::lag(monthlyMerged$GDP_econstage_Slowdown), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_mom,lag=m-1,prewhite = F,adjust = T)
reg_data_mom = coeftest(reg_fit_mom,vcov=NW_VCOV)

reg_fit_val = lm(monthlyMerged$HML ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$GDP_econstage_Expansion)+
                   dplyr::lag(monthlyMerged$GDP_econstage_Recovery)+dplyr::lag(monthlyMerged$GDP_econstage_Slowdown), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_val,lag=m-1,prewhite = F,adjust = T)
reg_data_val = coeftest(reg_fit_val,vcov=NW_VCOV)

reg_fit_size = lm(monthlyMerged$SMB ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$GDP_econstage_Expansion)+
                   dplyr::lag(monthlyMerged$GDP_econstage_Recovery)+dplyr::lag(monthlyMerged$GDP_econstage_Slowdown), na.action=na.exclude)


NW_VCOV<-NeweyWest(reg_fit_size,lag=m-1,prewhite = F,adjust = T)
reg_data_size = coeftest(reg_fit_size,vcov=NW_VCOV)

reg_fit_inv = lm(monthlyMerged$CMA ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$GDP_econstage_Expansion)+
                   dplyr::lag(monthlyMerged$GDP_econstage_Recovery)+dplyr::lag(monthlyMerged$GDP_econstage_Slowdown), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_inv,lag=m-1,prewhite = F,adjust = T)
reg_data_inv = coeftest(reg_fit_inv,vcov=NW_VCOV)

reg_fit_earn = lm(monthlyMerged$RMW ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$GDP_econstage_Expansion)+
                   dplyr::lag(monthlyMerged$GDP_econstage_Recovery)+dplyr::lag(monthlyMerged$GDP_econstage_Slowdown), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_earn,lag=m-1,prewhite = F,adjust = T)
reg_data_earn = coeftest(reg_fit_earn,vcov=NW_VCOV)

reg_fit_qual = lm(monthlyMerged$Quality ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$GDP_econstage_Expansion)+
                   dplyr::lag(monthlyMerged$GDP_econstage_Recovery)+dplyr::lag(monthlyMerged$GDP_econstage_Slowdown), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_qual,lag=m-1,prewhite = F,adjust = T)
reg_data_qual = coeftest(reg_fit_qual,vcov=NW_VCOV)

reg_fit_bab = lm(monthlyMerged$BAB ~ dplyr::lag(monthlyMerged$Sentiment_Index)+dplyr::lag(monthlyMerged$dp)+dplyr::lag(monthlyMerged$ep)+
                   dplyr::lag(monthlyMerged$svar)+dplyr::lag(monthlyMerged$lty)+
                   dplyr::lag(monthlyMerged$dfy)+dplyr::lag(monthlyMerged$infl)+dplyr::lag(monthlyMerged$tms)+dplyr::lag(monthlyMerged$GDP_econstage_Expansion)+
                   dplyr::lag(monthlyMerged$GDP_econstage_Recovery)+dplyr::lag(monthlyMerged$GDP_econstage_Slowdown), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_bab,lag=m-1,prewhite = F,adjust = T)
reg_data_bab = coeftest(reg_fit_bab,vcov=NW_VCOV)


library(stargazer)
my_models = list(reg_data_val, reg_data_size, reg_data_inv, reg_data_earn, reg_data_mom, reg_data_qual, reg_data_bab)
stargazer(my_models, title = 'Regressions', type = 'html', out = 'training output', keep.stat = 'rsq')

mom2 =  round(summary(reg_fit_mom)$adj.r.squared, 4)
value_r2 = round(summary(reg_fit_val)$adj.r.squared, 4)
size_r2 = round(summary(reg_fit_size)$adj.r.squared, 4)
inv_r2 = round(summary(reg_fit_inv)$adj.r.squared, 4)
earn_r2 = round(summary(reg_fit_earn)$adj.r.squared, 4)
qual_r2 = round(summary(reg_fit_qual)$adj.r.squared, 4)
bab_r2 = round(summary(reg_fit_bab)$adj.r.squared, 4)

star = stargazer(my_models,
                 add.lines = list(c("Adjusted $R^2$", value_r2, size_r2, inv_r2, earn_r2, mom2, qual_r2, bab_r2))
)
cat(star, sep = '\n', file = 'GDP.tex')


# Then the contemporaneous regressions
## NBER regressions, contemporaneous

reg_fit_mom = lm(monthlyMerged$mom ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                   monthlyMerged$svar+monthlyMerged$lty+
                   monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$`NBER_econstage_Post-Recession`+
                   monthlyMerged$`NBER_econstage_Pre-Recession`+monthlyMerged$`NBER_econstage_Pure-Expansion`, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_mom,lag=m-1,prewhite = F,adjust = T)
reg_data_mom = coeftest(reg_fit_mom,vcov=NW_VCOV)

reg_fit_val = lm(monthlyMerged$HML ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                   monthlyMerged$svar+monthlyMerged$lty+
                   monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$`NBER_econstage_Post-Recession`+
                   monthlyMerged$`NBER_econstage_Pre-Recession`+monthlyMerged$`NBER_econstage_Pure-Expansion`, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_val,lag=m-1,prewhite = F,adjust = T)
reg_data_val = coeftest(reg_fit_val,vcov=NW_VCOV)

reg_fit_size = lm(monthlyMerged$SMB ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                    monthlyMerged$svar+monthlyMerged$lty+
                    monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$`NBER_econstage_Post-Recession`+
                    monthlyMerged$`NBER_econstage_Pre-Recession`+monthlyMerged$`NBER_econstage_Pure-Expansion`, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_size,lag=m-1,prewhite = F,adjust = T)
reg_data_size = coeftest(reg_fit_size,vcov=NW_VCOV)

reg_fit_inv = lm(monthlyMerged$CMA ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                   monthlyMerged$svar+monthlyMerged$lty+
                   monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$`NBER_econstage_Post-Recession`+
                   monthlyMerged$`NBER_econstage_Pre-Recession`+monthlyMerged$`NBER_econstage_Pure-Expansion`, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_inv,lag=m-1,prewhite = F,adjust = T)
reg_data_inv = coeftest(reg_fit_inv,vcov=NW_VCOV)

reg_fit_earn = lm(monthlyMerged$RMW ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                    monthlyMerged$svar+monthlyMerged$lty+
                    monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$`NBER_econstage_Post-Recession`+
                    monthlyMerged$`NBER_econstage_Pre-Recession`+monthlyMerged$`NBER_econstage_Pure-Expansion`, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_earn,lag=m-1,prewhite = F,adjust = T)
reg_data_earn = coeftest(reg_fit_earn,vcov=NW_VCOV)

reg_fit_qual = lm(monthlyMerged$Quality ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                    monthlyMerged$svar+monthlyMerged$lty+
                    monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$`NBER_econstage_Post-Recession`+
                    monthlyMerged$`NBER_econstage_Pre-Recession`+monthlyMerged$`NBER_econstage_Pure-Expansion`, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_qual,lag=m-1,prewhite = F,adjust = T)
reg_data_qual = coeftest(reg_fit_qual,vcov=NW_VCOV)

reg_fit_bab = lm(monthlyMerged$BAB ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                   monthlyMerged$svar+monthlyMerged$lty+
                   monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$`NBER_econstage_Post-Recession`+
                   monthlyMerged$`NBER_econstage_Pre-Recession`+monthlyMerged$`NBER_econstage_Pure-Expansion`, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_bab,lag=m-1,prewhite = F,adjust = T)
reg_data_bab = coeftest(reg_fit_bab,vcov=NW_VCOV)


library(stargazer)
my_models = list(reg_data_val, reg_data_size, reg_data_inv, reg_data_earn, reg_data_mom, reg_data_qual, reg_data_bab)
stargazer(my_models, title = 'Regressions', type = 'html', out = 'training output', keep.stat = 'rsq')

mom_r2 =  round(summary(reg_fit_mom)$adj.r.squared, 4)
value_r2 = round(summary(reg_fit_val)$adj.r.squared, 4)
size_r2 = round(summary(reg_fit_size)$adj.r.squared, 4)
inv_r2 = round(summary(reg_fit_inv)$adj.r.squared, 4)
earn_r2 = round(summary(reg_fit_earn)$adj.r.squared, 4)
qual_r2 = round(summary(reg_fit_qual)$adj.r.squared, 4)
bab_r2 = round(summary(reg_fit_bab)$adj.r.squared, 4)

star = stargazer(my_models,
                 add.lines = list(c("Adjusted $R^2$", value_r2, size_r2, inv_r2, earn_r2, mom_r2, qual_r2, bab_r2))
)
cat(star, sep = '\n', file = 'NBER_Contemp.tex')


## OECD regressions, comtemporary.

reg_fit_mom = lm(monthlyMerged$mom ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                   monthlyMerged$svar+monthlyMerged$lty+
                   monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$OECD_econstage_Slowdown+
                   monthlyMerged$OECD_econstage_Expansion+monthlyMerged$OECD_econstage_Recovery, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_mom,lag=m-1,prewhite = F,adjust = T)
reg_data_mom = coeftest(reg_fit_mom,vcov=NW_VCOV)



reg_fit_val = lm(monthlyMerged$HML ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                   monthlyMerged$svar+monthlyMerged$lty+
                   monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$OECD_econstage_Slowdown+
                   monthlyMerged$OECD_econstage_Expansion+monthlyMerged$OECD_econstage_Recovery, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_val,lag=m-1,prewhite = F,adjust = T)
reg_data_val = coeftest(reg_fit_val,vcov=NW_VCOV)

reg_fit_size = lm(monthlyMerged$SMB ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                    monthlyMerged$svar+monthlyMerged$lty+
                    monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$OECD_econstage_Slowdown+
                    monthlyMerged$OECD_econstage_Expansion+monthlyMerged$OECD_econstage_Recovery, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_size,lag=m-1,prewhite = F,adjust = T)
reg_data_size = coeftest(reg_fit_size,vcov=NW_VCOV)

reg_fit_inv = lm(monthlyMerged$CMA ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                   monthlyMerged$svar+monthlyMerged$lty+
                   monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$OECD_econstage_Slowdown+
                   monthlyMerged$OECD_econstage_Expansion+monthlyMerged$OECD_econstage_Recovery, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_inv,lag=m-1,prewhite = F,adjust = T)
reg_data_inv = coeftest(reg_fit_inv,vcov=NW_VCOV)

reg_fit_earn = lm(monthlyMerged$RMW ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                    monthlyMerged$svar+monthlyMerged$lty+
                    monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$OECD_econstage_Slowdown+
                    monthlyMerged$OECD_econstage_Expansion+monthlyMerged$OECD_econstage_Recovery, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_earn,lag=m-1,prewhite = F,adjust = T)
reg_data_earn = coeftest(reg_fit_earn,vcov=NW_VCOV)

reg_fit_qual = lm(monthlyMerged$Quality ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                    monthlyMerged$svar+monthlyMerged$lty+
                    monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$OECD_econstage_Slowdown+
                    monthlyMerged$OECD_econstage_Expansion+monthlyMerged$OECD_econstage_Recovery, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_qual,lag=m-1,prewhite = F,adjust = T)
reg_data_qual = coeftest(reg_fit_qual,vcov=NW_VCOV)

reg_fit_bab = lm(monthlyMerged$BAB ~ monthlyMerged$Sentiment_Index+monthlyMerged$dp+monthlyMerged$ep+
                   monthlyMerged$svar+monthlyMerged$lty+
                   monthlyMerged$dfy+monthlyMerged$infl+monthlyMerged$tms+monthlyMerged$OECD_econstage_Slowdown+
                   monthlyMerged$OECD_econstage_Expansion+monthlyMerged$OECD_econstage_Recovery, na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_bab,lag=m-1,prewhite = F,adjust = T)
reg_data_bab = coeftest(reg_fit_bab,vcov=NW_VCOV)

library(stargazer)
my_models = list(reg_data_val, reg_data_size, reg_data_inv, reg_data_earn, reg_data_mom, reg_data_qual, reg_data_bab)
stargazer(my_models, title = 'Regressions', type = 'html', out = 'training output', keep.stat = 'rsq')

mom_r2 =  round(summary(reg_fit_mom)$adj.r.squared, 4)
value_r2 = round(summary(reg_fit_val)$adj.r.squared, 4)
size_r2 = round(summary(reg_fit_size)$adj.r.squared, 4)
inv_r2 = round(summary(reg_fit_inv)$adj.r.squared, 4)
earn_r2 = round(summary(reg_fit_earn)$adj.r.squared, 4)
qual_r2 = round(summary(reg_fit_qual)$adj.r.squared, 4)
bab_r2 = round(summary(reg_fit_bab)$adj.r.squared, 4)

star = stargazer(my_models,
                 add.lines = list(c("Adjusted $R^2$", value_r2, size_r2, inv_r2, earn_r2, mom_r2, qual_r2, bab_r2))
)
cat(star, sep = '\n', file = 'OECD_Contemp.tex')


# Test for OLS-assumptions

## Test for OLS
#  2) Linearity
reg_matrix = matrix(nrow = 7, ncol = 2)
k=0
for (i in list(reg_fit_size, reg_fit_val, reg_fit_earn, reg_fit_inv, reg_fit_mom, reg_fit_qual, reg_fit_bab)){
  k=k+1
  reg_matrix[k,1] = round((residualPlots(model = i)[12,1]),4)
  reg_matrix[k,2] = round((residualPlots(model = i)[12,2]),4)
}
rownames(reg_matrix) = c('Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
colnames(reg_matrix) = c('T-stat', 'P-value')

print(xtable(reg_matrix, caption = 'Results of Tukey Test for Additivity - Factors', digits = 3))

## Plot of the residuals vs. fitted plot

plot(reg_fit_mom, which = 1, main = 'Residuals vs. Fitted for Momentum')

## Plots of the residuals in each of the independent variables to assess from where the non-linearity arises
residualPlots(reg_fit_mom, main = 'Residual Plots for the Contemp. OECD Regression on Momentum',
              terms = ~ .-monthlyMerged$OECD_econstage_Recovery-monthlyMerged$OECD_econstage_Expansion-monthlyMerged$OECD_econstage_Slowdown)


residualPlots(reg_fit_val, main = 'Residual Plots for the Contemp. OECD Regression on Value',
              terms = ~ .-monthlyMerged$OECD_econstage_Recovery-monthlyMerged$OECD_econstage_Expansion-monthlyMerged$OECD_econstage_Slowdown)

#  3) No autocorrelation
## Durbin Watson for autocorrelation
dw_matrix = matrix(nrow = 7, ncol = 2)
k=0
for (i in list(reg_fit_size, reg_fit_val, reg_fit_earn, reg_fit_inv, reg_fit_mom, reg_fit_qual, reg_fit_bab)){
  k=k+1
  dw_matrix[k,1] = round(durbinWatsonTest(i)[[1]],4)
  dw_matrix[k,2] = durbinWatsonTest(i)[[3]]
}

rownames(dw_matrix) = c('Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
colnames(dw_matrix) = c('Autocor.', 'P-value')

print(xtable(dw_matrix, caption = 'Durbin Watson Test', digits = 3))

## To visually assess the autocorrelation, plot the errors against the lagged errors

temp=reg_fit_val$residuals
tempLag=dplyr::lag(temp,1)
plot(temp, tempLag, main =  'Errors vs. Lagged Errors, Value Factor', xlab = 'Lagged Errors', ylab = 'Errors')


# 4) The variance is finite
## Breusch-Pagan Test for constant variance
var_table = matrix(nrow = 7, ncol = 2)
k=0
for (i in list(reg_fit_size, reg_fit_val, reg_fit_earn, reg_fit_inv, reg_fit_mom, reg_fit_qual, reg_fit_bab)){
  k=k+1
  var_table[k,1] = round(lmtest::bptest(i)[[1]],4)
  var_table[k,2] = round(lmtest::bptest(i)[[4]],4)
}

rownames(var_table) = c('Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
colnames(var_table) = c('Breusch-Pagan Stat', 'P-value')
print(xtable(var_table, caption = ' Breusch-Pagan Stat', digits = 3))

# 5) The residuals are normally distributed (not really needed, as assessed in the autocorrelation)
## Quantile-Quantile plot for the value and momentum factor assessng normality. 
qqPlot(temp, main = 'QQ Plot - Value Factor', ylab = 'Value Factor Monthly Return')
qqPlot(temp, main = 'QQ Plot - Momentum Factor', ylab = 'Momentum Factor Monthly Return')
# Test for multicolinearity
## Creating the VIF-factor
vif_table = as.data.frame(round(vif(reg_fit_val),4))
rownames(vif_table) = c('Sentiment', 'DivPrice', 'E/P', 'Market Variance', 'Yield', 'Def. Spread', 'Inflation', 'Term Spread', 'Slowdown', 'Expansion', 'Recovery')
colnames(vif_table) = 'VIF-Score'
print(xtable(vif_table, caption ='VIF Score - Contemp. OECD Reg', digits = 3))

# Test for persistance in regressors
ar_table = matrix(nrow = 8, ncol = 1)

k=0
for (i in c('Sentiment_Index', 'dp', 'ep', 'svar', 'lty', 'dfy', 'infl', 'tms')){
  k=k+1
  ar_table[k] = round(arima(monthlyMerged[i], order = c(1,0,0))[[1]][1],4)
}


rownames(ar_table) = c('Sentiment', 'DivPrice', 'E/P', 'Market Variance', 'Yield', 'Def. Spread', 'Inflation', 'Term Spread')
colnames(ar_table) = 'Rho'
print(xtable(ar_table, caption = 'Rho Coefficient in AR(1) Model for Dependent Variables', digits = 3))

## Analysis for section 4.3; Mean-Variance optimization ##

# Firstly, I construct a naive, all across all periods and stages, portfolio.
# Remembering the mean-variance efficient portfolio relies on the assumption of constant risk-free rate, I estimate the covariance on excess returns rather than full returns as included would else also be the variance of the risk-free rate.

means_excess = colMeans(daily[,2:9]) # Generate the means of the excess returns
cov_basic_excess = cov(daily[,2:9], use = 'complete.obs') # Generate the covariances of the excess returns
vec = c(rep(1,length(means_excess))) # Generate a vector of ones needed to calculate the tangency portfolio

## Calc Tangency Pf and max_slope Pf

tangency = (inv(cov_basic_excess)%*%(means_excess))/(vec%*%inv(cov_basic_excess)%*%(means_excess))[1][[1]]

equal_weight = c(rep(1/length(means_excess), length(means_excess))) # Define benchmark of equal weights

## Multiply the weights to get the returns of the portfolio

equal_rets = rowSums(sweep(daily[,2:9], MARGIN = 2, equal_weight, '*'))
tangency_rets = rowSums(sweep(daily[,2:9], MARGIN = 2, tangency, '*'))
tangency_rets_full = tangency_rets + daily$RF
equal_rets_full = equal_rets + daily$RF

## Calc the out of sample portfolio

cutoff = as.Date('2013-06-30') # Set cutoff date for in-sample. All after this date will be out-of-sample.
# Define function to calculate the out-of-sample performance and portfolio
oos_function = function(cutoff){
  in_sample = daily[daily$date<=cutoff,]
  out_of_sample = daily[daily$date>cutoff,]
  
  
  means_in_sample = colMeans(in_sample[,2:9]) # Generate the means of the excess returns
  cov_in_sample = cov(in_sample[,2:9], use = 'complete.obs') # Generate the covariances of the excess returns
  vec = c(rep(1,length(means_in_sample))) # Vector of ones
  
  tangency_in_sample = (inv(cov_in_sample)%*%(means_in_sample))/(vec%*%inv(cov_in_sample)%*%(means_in_sample))[1][[1]]
  
  equal_weight = c(rep(1/length(means_in_sample), length(means_in_sample))) # Define benchmark of equal weights
  
  out_of_sample = cbind(out_of_sample, t(tangency_in_sample))
  
  out_of_sample$pfret_tangency = rowSums(out_of_sample[2:9]*out_of_sample[13:20])
  
  out_of_sample$pfret_full_tangency = out_of_sample$pfret_tangency + out_of_sample$RF
  
  out_of_sample$equal_rets = rowSums(sweep(out_of_sample[,2:9], MARGIN = 2, equal_weight, '*'))
  
  out_of_sample$equal_rets_full = out_of_sample$equal_rets +out_of_sample$RF
  
  return(out_of_sample)
}

# Get the out-of-sample portfolio

out_of_sample = oos_function(cutoff)

# Show Weights Table
dates = daily$date
mv_weights = cbind(tangency, t(out_of_sample[1,13:20]))
rownames(mv_weights) = c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
colnames(mv_weights) = c('Naive Tangency', 'In Sample Tangency')
print(xtable(mv_weights, caption = 'Naive Tangency Weights', digits = 3))


# Show statistics of performance of the naive tangency portfolio and out-of-sample tangency portfolio

ind_matrix = matrix(nrow = 8, ncol = 4)
ind_matrix[1,1] = round((tail(cumprod(1+equal_rets_full), 1)^(1/n_distinct(daily$month))-1)*100,4)
ind_matrix[1,2] = round((tail(cumprod(1+tangency_rets_full),1)^(1/n_distinct(daily$month))-1)*100,4)
ind_matrix[2,1] = round(mean(equal_rets)*100,4)
ind_matrix[2,2] = round(mean(tangency_rets)*100,4)
ind_matrix[3,1] = round(sd(equal_rets)*100,4)
ind_matrix[3,2] = round(sd(tangency_rets)*100,4)
ind_matrix[4,1] = round(mean(equal_rets)/sd(equal_rets),4)
ind_matrix[4,2] = round(mean(tangency_rets)/sd(tangency_rets),4)
ind_matrix[5,1] = round(min((cumprod(1+equal_rets_full)-cummax(cumprod(1+equal_rets_full)))/cummax(cumprod(1+equal_rets_full)))*100,4)
ind_matrix[5,2] = round(min((cumprod(1+tangency_rets_full)-cummax(cumprod(1+tangency_rets_full)))/cummax(cumprod(1+tangency_rets_full)))*100,4)
ind_matrix[6,1] = round(lm(equal_rets ~ daily$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(equal_rets ~ daily$Mkt.RF)[[1]][2],4)
ind_matrix[8,1] = round(lm(equal_rets ~ daily$Mkt.RF)[[1]][1]/(sd(equal_rets)-sd(daily$Mkt.RF)*lm(equal_rets ~ daily$Mkt.RF)[[1]][2]),4)
ind_matrix[6,2] = round(lm(tangency_rets ~ daily$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(tangency_rets ~ daily$Mkt.RF)[[1]][2],4)
ind_matrix[8,2] = round(lm(tangency_rets ~ daily$Mkt.RF)[[1]][1]/(sd(tangency_rets)-sd(daily$Mkt.RF)*lm(tangency_rets ~ daily$Mkt.RF)[[1]][2]),4)
ind_matrix[1,3] = round((tail(cumprod(1+out_of_sample$equal_rets_full), 1)^(1/n_distinct(out_of_sample$month))-1)*100,4)
ind_matrix[2,3] = round(mean(out_of_sample$equal_rets)*100,4)
ind_matrix[3,3] = round(sd(out_of_sample$equal_rets)*100,4)
ind_matrix[4,3] = round(mean(out_of_sample$equal_rets)/sd(out_of_sample$equal_rets),4)
ind_matrix[5,3] = round(min((cumprod(1+out_of_sample$equal_rets_full)-cummax(cumprod(1+out_of_sample$equal_rets_full)))/cummax(cumprod(1+out_of_sample$equal_rets_full)))*100,4)
ind_matrix[6,3] = round(lm(out_of_sample$equal_rets ~ out_of_sample$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,3] = round(lm(out_of_sample$equal_rets ~ out_of_sample$Mkt.RF)[[1]][2],4)
ind_matrix[8,3] = round(lm(out_of_sample$equal_rets ~ out_of_sample$Mkt.RF)[[1]][1]/(sd(out_of_sample$equal_rets)-sd(out_of_sample$Mkt.RF)*lm(out_of_sample$equal_rets ~ out_of_sample$Mkt.RF)[[1]][2]),4)
ind_matrix[1,4] = round((tail(cumprod(1+out_of_sample$pfret_full_tangency), 1)^(1/n_distinct(out_of_sample$month))-1)*100,4)
ind_matrix[2,4] = round(mean(out_of_sample$pfret_tangency)*100,4)
ind_matrix[3,4] = round(sd(out_of_sample$pfret_tangency)*100,4)
ind_matrix[4,4] = round(mean(out_of_sample$pfret_tangency)/sd(out_of_sample$pfret_tangency),4)
ind_matrix[5,4] = round(min((cumprod(1+out_of_sample$pfret_full_tangency)-cummax(cumprod(1+out_of_sample$pfret_full_tangency)))/cummax(cumprod(1+out_of_sample$pfret_full_tangency)))*100,4)
ind_matrix[6,4] = round(lm(out_of_sample$pfret_tangency ~ out_of_sample$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,4] = round(lm(out_of_sample$pfret_tangency ~ out_of_sample$Mkt.RF)[[1]][2],4)
ind_matrix[8,4] = round(lm(out_of_sample$pfret_tangency ~ out_of_sample$Mkt.RF)[[1]][1]/(sd(out_of_sample$pfret_tangency)-sd(out_of_sample$Mkt.RF)*lm(out_of_sample$pfret_tangency ~ out_of_sample$Mkt.RF)[[1]][2]),4)


stat_names = c('Mean Monthly Ret (%)', 'Excess Daily Ret (%)', 'Excess Std. Dev (%)', 'Sharpe Ratio', 'Drawdown (%)', 'CAPM Alpha (%)', 'CAPM Beta', 'Information Ratio')
rownames(ind_matrix) = stat_names
colnames(ind_matrix) = c('Equal Full Period', 'Naive Tangency Full Period', 'OOS Equal', 'OOS Tangency')
print(xtable(ind_matrix, caption = 'Performance Metrics of the Equal-Weighted and Naive Tangency Portfolio', digits = c(3)))


# Plot the Efficient Frontier of the assets


## Amending the function of Zivot (2019) in the IntroCompFinR package.

plot.Markowitz <-
  function(x, plot.assets=FALSE, ...)
  {
    if (!plot.assets) {
      y.lim=c(0,max(x$er))
      x.lim=c(0,max(x$sd))
      plot(x$sd,x$er,type="b",xlim=x.lim, ylim=y.lim,
           xlab="Portfolio SD", ylab="Portfolio ER",...)
    }
    else {
      call = x$call
      mu.vals = eval(call$er)
      sd.vals = sqrt( diag( eval(call$cov.mat) ) )
      y.lim = range(c(0,mu.vals,x$er))
      x.lim = range(c(0,sd.vals,x$sd))
      plot(x$sd,x$er,type="b", xlim=x.lim, ylim=y.lim,
           xlab="Portfolio SD", ylab="Portfolio ER",...)
      text(sd.vals, mu.vals, labels=names(mu.vals))
    }
    invisible()
  }


ef <- efficient.frontier(means_excess, cov_basic_excess, alpha.min=-2, alpha.max = 2, nport = 20)
plot.Markowitz(ef, plot.assets = TRUE, col = 'blue', pch=16, main = 'Efficient Frontier of Factors')
tan.port <- tangency.portfolio(means_excess, cov_basic_excess, 0, shorts = TRUE)
gmin.port = globalMin.portfolio(means_excess, cov_basic_excess)
points(gmin.port$sd, gmin.port$er, col="green", pch=16, cex=1.3)
points(tan.port$sd, tan.port$er, col="red", pch=16, cex=1.3)
text(gmin.port$sd, gmin.port$er, labels="Min.Var", pos=2.5)
text(tan.port$sd, tan.port$er, labels="Tangency", pos=3)
sr.tan = (tan.port$er - 0)/tan.port$sd
abline(a=0, b=sr.tan, col="red", lwd=2)

# 4.3.1 optimal portfolios across stages of the business cycle

## The Naive approach of using all observations as in table 12 is used

## Starting with the OECD-measure

daily_expansion_oecd = dailyMerged[dailyMerged$OECD_econstage=='Expansion',]
daily_downturn_oecd = dailyMerged[dailyMerged$OECD_econstage=='Downturn',]
daily_recovery_oecd = dailyMerged[dailyMerged$OECD_econstage=='Recovery',]
daily_slowdown_oecd = dailyMerged[dailyMerged$OECD_econstage=='Slowdown',]

## Calculating means and covariances
means_expansion_oecd = colMeans(daily_expansion_oecd[,4:11])
cov_expansion_oecd = cov(daily_expansion_oecd[,4:11])
means_downturn_oecd = colMeans(daily_downturn_oecd[,4:11])
cov_downturn_oecd = cov(daily_downturn_oecd[,4:11])
means_recovery_oecd = colMeans(daily_recovery_oecd[,4:11])
cov_recovery_oecd = cov(daily_recovery_oecd[,4:11])
means_slowdown_oecd = colMeans(daily_slowdown_oecd[,4:11])
cov_slowdown_oecd = cov(daily_slowdown_oecd[,4:11])

## Estimting the tangency portfolio in each OECD-state
tangency_expansion_oecd = (inv(cov_expansion_oecd)%*%(means_expansion_oecd))/(vec%*%inv(cov_expansion_oecd)%*%(means_expansion_oecd))[1]
tangency_downturn_oecd = (inv(cov_downturn_oecd)%*%(means_downturn_oecd))/(vec%*%inv(cov_downturn_oecd)%*%(means_downturn_oecd))[1]
tangency_recovery_oecd = (inv(cov_recovery_oecd)%*%(means_recovery_oecd))/(vec%*%inv(cov_recovery_oecd)%*%(means_recovery_oecd))[1]
tangency_slowdown_oecd = (inv(cov_slowdown_oecd)%*%(means_slowdown_oecd))/(vec%*%inv(cov_slowdown_oecd)%*%(means_slowdown_oecd))[1]

oecd_weights = round(bind_cols(tangency_expansion_oecd, tangency_slowdown_oecd, tangency_recovery_oecd, tangency_downturn_oecd),4)
oecd_weights = as.data.frame(oecd_weights)
rownames(oecd_weights) =  c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
colnames(oecd_weights) = c('Expansion', 'Slowdown', 'Recovery', 'Downturn')

print(xtable(oecd_weights, caption = 'Optimal Weights Across Business Cycles, OECD', digits = 3))

# Calculating performance of the business-cycle-fitted portfolio
daily_expansion_oecd  = cbind(daily_expansion_oecd, t(tangency_expansion_oecd))
daily_downturn_oecd = cbind(daily_downturn_oecd, t(tangency_downturn_oecd))
daily_recovery_oecd = cbind(daily_recovery_oecd, t(tangency_recovery_oecd))
daily_slowdown_oecd = cbind(daily_slowdown_oecd, t(tangency_slowdown_oecd))
daily_OECD = bind_rows(daily_expansion_oecd, daily_downturn_oecd, daily_recovery_oecd, daily_slowdown_oecd)
daily_OECD$pfret = rowSums(daily_OECD[4:11]*daily_OECD[16:23])
daily_OECD$pfret_full = daily_OECD$pfret+daily$RF
daily_OECD=daily_OECD[order(daily_OECD$date),]

# OECD Portfolio Lagged One Period, realistically implementable portfolio
OECD$lagged = dplyr::lag(OECD$OECD_econstage)
dailyMerged_OECD_lagged = merge(daily, OECD, by = 'month')
daily_expansion_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Expansion',]
daily_downturn_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Downturn',]
daily_recovery_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Recovery',]
daily_slowdown_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Slowdown',]

daily_expansion_oecd_lagged  = cbind(daily_expansion_oecd_lagged, t(tangency_expansion_oecd))
daily_downturn_oecd_lagged = cbind(daily_downturn_oecd_lagged, t(tangency_downturn_oecd))
daily_recovery_oecd_lagged = cbind(daily_recovery_oecd_lagged, t(tangency_recovery_oecd))
daily_slowdown_oecd_lagged = cbind(daily_slowdown_oecd_lagged, t(tangency_slowdown_oecd))
daily_OECD_lagged = bind_rows(daily_expansion_oecd_lagged, daily_downturn_oecd_lagged, daily_recovery_oecd_lagged, daily_slowdown_oecd_lagged)
daily_OECD_lagged$pfret = rowSums(daily_OECD_lagged[3:10]*daily_OECD_lagged[15:22])
daily_OECD_lagged$pfret_full = daily_OECD_lagged$pfret+daily$RF
daily_OECD_lagged=daily_OECD_lagged[order(daily_OECD_lagged$date),]

## TAA Weight adjusted Busness-Cycle-Portfolio
### Define the weights
expansion_TAA_weights = c(0.25,0.15,0.15,0,0.10,0,0.10,0.25)
slowdown_TAA_weights = c(0.10,0.05,0.15,0.10,0.10,0.15,0.20,0.15)
recovery_TAA_weights = c(0.25,0.20,0.10,0.05,0.10,0.05,0,0.25)
downturn_TAA_weights = c(0.1,0,0.05,0.10,0.25,0.25,0.25,0)
daily_expansion_oecd = dailyMerged[dailyMerged$OECD_econstage=='Expansion',]
daily_downturn_oecd = dailyMerged[dailyMerged$OECD_econstage=='Downturn',]
daily_recovery_oecd = dailyMerged[dailyMerged$OECD_econstage=='Recovery',]
daily_slowdown_oecd = dailyMerged[dailyMerged$OECD_econstage=='Slowdown',]

### Contemporary
daily_expansion_oecd_TAA  = cbind(daily_expansion_oecd, t(expansion_TAA_weights))
daily_downturn_oecd_TAA = cbind(daily_downturn_oecd, t(downturn_TAA_weights))
daily_recovery_oecd_TAA = cbind(daily_recovery_oecd, t(recovery_TAA_weights))
daily_slowdown_oecd_TAA = cbind(daily_slowdown_oecd, t(downturn_TAA_weights))
daily_OECD_TAA = bind_rows(daily_expansion_oecd_TAA, daily_downturn_oecd_TAA, daily_recovery_oecd_TAA, daily_slowdown_oecd_TAA)
daily_OECD_TAA$pfret = rowSums(daily_OECD_TAA[4:11]*daily_OECD_TAA[16:23])
daily_OECD_TAA$pfret_full = daily_OECD_TAA$pfret+daily_OECD_TAA$RF
daily_OECD_TAA=daily_OECD_TAA[order(daily_OECD_TAA$date),]

### Lagged
daily_expansion_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Expansion',]
daily_downturn_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Downturn',]
daily_recovery_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Recovery',]
daily_slowdown_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Slowdown',]

daily_expansion_oecd_TAA_lagged  = cbind(daily_expansion_oecd_lagged, t(expansion_TAA_weights))
daily_downturn_oecd_TAA_lagged = cbind(daily_downturn_oecd_lagged, t(downturn_TAA_weights))
daily_recovery_oecd_TAA_lagged = cbind(daily_recovery_oecd_lagged, t(recovery_TAA_weights))
daily_slowdown_oecd_TAA_lagged = cbind(daily_slowdown_oecd_lagged, t(downturn_TAA_weights))
daily_OECD_TAA_lagged = bind_rows(daily_expansion_oecd_TAA_lagged, daily_downturn_oecd_TAA_lagged, daily_recovery_oecd_TAA_lagged, daily_slowdown_oecd_TAA_lagged)
daily_OECD_TAA_lagged$pfret = rowSums(daily_OECD_TAA_lagged[3:10]*daily_OECD_TAA_lagged[15:22])
daily_OECD_TAA_lagged$pfret_full = daily_OECD_TAA_lagged$pfret+daily_OECD_TAA_lagged$RF
daily_OECD_TAA_lagged=daily_OECD_TAA_lagged[order(daily_OECD_TAA_lagged$date),]

# Calculate the performance
ind_matrix = matrix(nrow = 8, ncol = 4)
rownames(ind_matrix) = stat_names
ind_matrix[1,1] = round((tail(cumprod(1+daily_OECD$pfret_full), 1)^(1/n_distinct(daily_OECD$month))-1)*100,4)
ind_matrix[2,1] = round(mean(daily_OECD$pfret)*100,4)
ind_matrix[3,1] = round(sd(daily_OECD$pfret)*100,4)
ind_matrix[4,1] = round(mean(daily_OECD$pfret)/sd(daily_OECD$pfret),4)
ind_matrix[5,1] = round(min((cumprod(1+daily_OECD$pfret_full)-cummax(cumprod(1+daily_OECD$pfret_full)))/cummax(cumprod(1+daily_OECD$pfret_full)))*100,4)
ind_matrix[6,1] = round(lm(daily_OECD$pfret ~ daily$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(daily_OECD$pfret ~ daily$Mkt.RF)[[1]][2],4)
ind_matrix[8,1] = round(lm(daily_OECD$pfret ~ daily$Mkt.RF)[[1]][1]/(sd(daily_OECD$pfret)-sd(daily$Mkt.RF)*lm(daily_OECD$pfret ~ daily$Mkt.RF)[[1]][2]),4)
ind_matrix[1,2] = round((tail(cumprod(1+daily_OECD_lagged$pfret_full), 1)^(1/n_distinct(daily_OECD_lagged$month))-1)*100,4)
ind_matrix[2,2] = round(mean(daily_OECD_lagged$pfret)*100,4)
ind_matrix[3,2] = round(sd(daily_OECD_lagged$pfret)*100,4)
ind_matrix[4,2] = round(mean(daily_OECD_lagged$pfret)/sd(daily_OECD_lagged$pfret),4)
ind_matrix[5,2] = round(min((cumprod(1+daily_OECD_lagged$pfret_full)-cummax(cumprod(1+daily_OECD_lagged$pfret_full)))/cummax(cumprod(1+daily_OECD_lagged$pfret_full)))*100,4)
ind_matrix[6,2] = round(lm(daily_OECD_lagged$pfret ~ daily_OECD_lagged$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(daily_OECD_lagged$pfret ~ daily_OECD_lagged$Mkt.RF)[[1]][2],4)
ind_matrix[8,2] = round(lm(daily_OECD_lagged$pfret ~ daily_OECD_lagged$Mkt.RF)[[1]][1]/(sd(daily_OECD_lagged$pfret)-sd(daily_OECD_lagged$Mkt.RF)*lm(daily_OECD_lagged$pfret ~ daily_OECD_lagged$Mkt.RF)[[1]][2]),4)
ind_matrix[1,3] = round((tail(cumprod(1+daily_OECD_TAA$pfret_full), 1)^(1/n_distinct(daily_OECD_TAA$month))-1)*100,4)
ind_matrix[2,3] = round(mean(daily_OECD_TAA$pfret)*100,4)
ind_matrix[3,3] = round(sd(daily_OECD_TAA$pfret)*100,4)
ind_matrix[4,3] = round(mean(daily_OECD_TAA$pfret)/sd(daily_OECD_TAA$pfret),4)
ind_matrix[5,3] = round(min((cumprod(1+daily_OECD_TAA$pfret_full)-cummax(cumprod(1+daily_OECD_TAA$pfret_full)))/cummax(cumprod(1+daily_OECD_TAA$pfret_full)))*100,4)
ind_matrix[6,3] = round(lm(daily_OECD_TAA$pfret ~ daily_OECD_TAA$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,3] = round(lm(daily_OECD_TAA$pfret ~ daily_OECD_TAA$Mkt.RF)[[1]][2],4)
ind_matrix[8,3] = round(lm(daily_OECD_TAA$pfret ~ daily_OECD_TAA$Mkt.RF)[[1]][1]/(sd(daily_OECD_TAA$pfret)-sd(daily_OECD_TAA$Mkt.RF)*lm(daily_OECD_TAA$pfret ~ daily_OECD_TAA$Mkt.RF)[[1]][2]),4)
ind_matrix[1,4] = round((tail(cumprod(1+daily_OECD_TAA_lagged$pfret_full), 1)^(1/n_distinct(daily_OECD_TAA_lagged$month))-1)*100,4)
ind_matrix[2,4] = round(mean(daily_OECD_TAA_lagged$pfret)*100,4)
ind_matrix[3,4] = round(sd(daily_OECD_TAA_lagged$pfret)*100,4)
ind_matrix[4,4] = round(mean(daily_OECD_TAA_lagged$pfret)/sd(daily_OECD_TAA_lagged$pfret),4)
ind_matrix[5,4] = round(min((cumprod(1+daily_OECD_TAA_lagged$pfret_full)-cummax(cumprod(1+daily_OECD_TAA_lagged$pfret_full)))/cummax(cumprod(1+daily_OECD_TAA_lagged$pfret_full)))*100,4)
ind_matrix[6,4] = round(lm(daily_OECD_TAA_lagged$pfret ~ daily_OECD_TAA_lagged$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,4] = round(lm(daily_OECD_TAA_lagged$pfret ~ daily_OECD_TAA_lagged$Mkt.RF)[[1]][2],4)
ind_matrix[8,4] = round(lm(daily_OECD_TAA_lagged$pfret ~ daily_OECD_TAA_lagged$Mkt.RF)[[1]][1]/(sd(daily_OECD_TAA_lagged$pfret)-sd(daily_OECD_TAA_lagged$Mkt.RF)*lm(daily_OECD_TAA_lagged$pfret ~ daily_OECD_TAA_lagged$Mkt.RF)[[1]][2]),4)

colnames(ind_matrix) = c('Contemp. OECD Adjusted', 'Lagged OECD. Adjusted', 'Contemp. TAA', 'Lagged TAA')

print(xtable(ind_matrix, caption = 'Performance Metrics of the Business Cycle OECD-Portfolio', digits = 3))


### 4.3.2 Rolling Mean-Variance Optimization ###
# Calcuating the rolling tangency portfolios. Technically, I construct a list of lists in R of the means and covariances at the beginning of each month, looking back 252 trading days.

## Construct Rolling Portfolios
# First we use rolling portfolios backwards looking 12 months (on average 252 trading days)
# Code follows (Shenanigans, 2019)
daily_lists = daily[1:9] %>%
  mutate(year_month = yearmonth(date)) %>%
  nest(-year_month) %>%
  rolling_origin(
    initial = 12,
    assess = 1,
    skip =0,
    cumulative = F
  )

ListNamesDates <- map(daily_lists$splits, ~assessment(.x) %>%
                        select(year_month)) %>%
  plyr::ldply(., data.frame) %>% 
  pull(year_month)

mus <- map(daily_lists$splits, ~ analysis(.x) %>% 
                       unnest(data) %>%
                       select(-year_month, -date) %>%
                       colMeans) %>%
  setNames(c(ListNamesDates))


SigmasInv<- map(daily_lists$splits, ~ analysis(.x) %>%
                             unnest() %>%
                             tk_xts(., date_var = date) %>%
                             cov(.) %>%
                             inv(.))%>%
  setNames(c(ListNamesDates))

Sigmas = map(daily_lists$splits, ~ analysis(.x) %>%
                         unnest() %>%
                         tk_xts(., date_var = date) %>%
                         cov(.)) %>%
  setNames(c(ListNamesDates))

ones = rep(list(vec),NROW(Sigmas))
# I construct an expanding window of 12 months.
# The code again follows (Shenanigans, 2019)

daily_lists_expanding = daily[1:9] %>%
  mutate(year_month = yearmonth(date)) %>%
  nest(-year_month) %>%
  rolling_origin(
    initial = 12,
    assess = 1,
    skip =0,
    cumulative = T
  )

ListNamesDates <- map(daily_lists_expanding$splits, ~assessment(.x) %>%
                        select(year_month)) %>%
  plyr::ldply(., data.frame) %>% 
  pull(year_month)

mus_expanding <- map(daily_lists_expanding$splits, ~ analysis(.x) %>% 
             unnest(data) %>%
             select(-year_month, -date) %>%
             colMeans) %>%
  setNames(c(ListNamesDates))


SigmasInv_expanding <- map(daily_lists_expanding$splits, ~ analysis(.x) %>%
                   unnest() %>%
                   tk_xts(., date_var = date) %>%
                   cov(.) %>%
                   inv(.))%>%
  setNames(c(ListNamesDates))

Sigmas_expanding = map(daily_lists_expanding$splits, ~ analysis(.x) %>%
               unnest() %>%
               tk_xts(., date_var = date) %>%
               cov(.)) %>%
  setNames(c(ListNamesDates))

ones = rep(list(vec),NROW(Sigmas))

### 4.3.2.1 One in Market and Scaled in factors ###
# I first compute the equal-weighted counterpart to benchmark against. 1 in the market and 1/7 in the 7 other factors

ew_mkt_factor_weights = c(1,rep(1/7,7))
ew_mkt_factor_rets = rowSums(sweep(daily[,2:9],MARGIN = 2, ew_mkt_factor_weights, FUN = '*')) 
ew_mkt_factor_rets_full = ew_mkt_factor_rets+daily$RF
ew_mkt_factor_rets = ew_mkt_factor_rets[253:NROW(daily)] # As the rolling portfolio does not start before 252 days, the equal-weight portfolio is set to the same length
ew_mkt_factor_rets_full = ew_mkt_factor_rets_full[253:NROW(daily)]
# I then do the same for the benchmark of 1 in the market and 0 factor bets.
mkt_inv_weights = c(1,rep(0,7))
mkt_inv_rets = rowSums(sweep(daily[,2:9],MARGIN = 2, mkt_inv_weights, FUN = '*')) 
mkt_inv_rets_full = mkt_inv_rets+daily$RF
mkt_inv_rets = mkt_inv_rets[253:NROW(daily)]
mkt_inv_rets_full = mkt_inv_rets_full[253:NROW(daily)]
# Now that I have the benchmarks construct the market-weights via rolling and expanding mechanisms.
#At all times the weights are capped at various caps from abs(100%) to abs(500%)
market_weight_capped1 = matrix(nrow = NROW(mus), ncol = 8)
market_weight_capped1[,1] = rep(1,NROW(mus))
market_weight_expanding_capped1 = matrix(nrow = NROW(mus), ncol = 8)
market_weight_expanding_capped1[,1] = rep(1,NROW(mus))
market_weight_capped2 = matrix(nrow = NROW(mus), ncol = 8)
market_weight_capped2[,1] = rep(1,NROW(mus))
market_weight_expanding_capped2 = matrix(nrow = NROW(mus), ncol = 8)
market_weight_expanding_capped2[,1] = rep(1,NROW(mus))
market_weight_capped5 = matrix(nrow = NROW(mus), ncol = 8)
market_weight_capped5[,1] = rep(1,NROW(mus))
market_weight_expanding_capped5 = matrix(nrow = NROW(mus), ncol = 8)
market_weight_expanding_capped5[,1] = rep(1,NROW(mus))
var_weights = rep(1/7,7)

SharpeFunction = function(var_weights, Sigmas, mus){
  weight = append(1,var_weights)
  Mu = mus%*%weight
  Var = weight%*%Sigmas%*%weight
  Sharpe = Mu/sqrt(Var)
  return(Sharpe)
}
# Rolling weights Cap 1
for (i in 1:NROW(Sigmas)){
  market_weight_capped1[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = Sigmas[[i]], mus = mus[[i]], control = list(fnscale=-1), lower = rep(-1,7), upper = rep(1,7), method = 'L-BFGS-B')$par
}
# Expanding Weights Cap 1
for (i in 1:NROW(Sigmas)){
  market_weight_expanding_capped1[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = Sigmas_expanding[[i]], mus = mus_expanding[[i]], control = list(fnscale=-1), lower = rep(-1,7), upper = rep(1, 7), method = 'L-BFGS-B')$par
}


# Rolling weights Cap 2
for (i in 1:NROW(Sigmas)){
  market_weight_capped2[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = Sigmas[[i]], mus = mus[[i]], control = list(fnscale=-1), lower = rep(-2,7), upper = rep(2, 7), method = 'L-BFGS-B')$par
}
# Expanding Weights Cap 2
for (i in 1:NROW(Sigmas)){
  market_weight_expanding_capped2[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = Sigmas_expanding[[i]], mus = mus_expanding[[i]], control = list(fnscale=-1), lower = rep(-2,7), upper = rep(2, 7), method = 'L-BFGS-B')$par
}

# Rolling weights Cap 5
for (i in 1:NROW(Sigmas)){
  market_weight_capped5[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = Sigmas[[i]], mus = mus[[i]], control = list(fnscale=-1), lower = rep(-5,7), upper = rep(5, 7), method = 'L-BFGS-B')$par
}
# Expanding Weights Cap 5
for (i in 1:NROW(Sigmas)){
  market_weight_expanding_capped5[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = Sigmas_expanding[[i]], mus = mus_expanding[[i]], control = list(fnscale=-1), lower = rep(-5,7), upper = rep(5, 7), method = 'L-BFGS-B')$par
}

# Merge the weights with the daily returns to calculate performance metrics
months = monthly$month[13:713]
matrix_to_cleaned_df = function(matrix){
  dataframe = as.data.frame(matrix)
  colnames(dataframe) = c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
  dataframe$month = months
  df = merge(dataframe, daily, by = 'month')
  df$pfret =rowSums(df[2:9]*df[11:18])
  df$pfret_full = df$pfret + daily$RF[253:NROW(daily)]
  return(df)
}

mkt_factor_rolling_capped1 = matrix_to_cleaned_df(market_weight_capped1)
mkt_factor_expanding_capped1 = matrix_to_cleaned_df(market_weight_expanding_capped1)
mkt_factor_rolling_capped2 = matrix_to_cleaned_df(market_weight_capped2)
mkt_factor_expanding_capped2 = matrix_to_cleaned_df(market_weight_expanding_capped2)
mkt_factor_rolling_capped5 = matrix_to_cleaned_df(market_weight_capped5)
mkt_factor_expanding_capped5 = matrix_to_cleaned_df(market_weight_expanding_capped5)

# Calculate performance metrics

ind_matrix = matrix(nrow = 8, ncol = 8)
ind_matrix[1,1] = round((tail(cumprod(1+ew_mkt_factor_rets_full), 1)^(1/NROW(Sigmas))-1)*100,4)
ind_matrix[1,2] = round((tail(cumprod(1+mkt_factor_expanding_capped1$pfret_full),1)^(1/NROW(Sigmas))-1)*100,4)
ind_matrix[2,1] = round(mean(ew_mkt_factor_rets*100),4)
ind_matrix[2,2] = round(mean(mkt_factor_expanding_capped1$pfret*100),4)
ind_matrix[3,1] = round(sd(ew_mkt_factor_rets*100),4)
ind_matrix[3,2] = round(sd(mkt_factor_expanding_capped1$pfret*100),4)
ind_matrix[4,1] = round(mean(ew_mkt_factor_rets)/sd(ew_mkt_factor_rets),4)
ind_matrix[4,2] = round(mean(mkt_factor_expanding_capped1$pfret)/sd(mkt_factor_expanding_capped1$pfret),4)
ind_matrix[5,1] = round(min((cumprod(1+ew_mkt_factor_rets_full)-cummax(cumprod(1+ew_mkt_factor_rets_full)))/cummax(cumprod(1+ew_mkt_factor_rets_full)))*100,4)
ind_matrix[5,2] = round(min((cumprod(1+mkt_factor_expanding_capped1$pfret_full)-cummax(cumprod(1+mkt_factor_expanding_capped1$pfret_full)))/cummax(cumprod(1+mkt_factor_expanding_capped1$pfret_full)))*100,2)
ind_matrix[1,3] = round((tail(cumprod(1+mkt_factor_rolling_capped1$pfret_full), 1)^(1/NROW(Sigmas))-1)*100,4)
ind_matrix[2,3] = round(mean(mkt_factor_rolling_capped1$pfret*100),4)
ind_matrix[3,3] = round(sd(mkt_factor_rolling_capped1$pfret*100),4)
ind_matrix[4,3] = round(mean(mkt_factor_rolling_capped1$pfret)/sd(mkt_factor_rolling_capped1$pfret),4)
ind_matrix[5,3] = round(min((cumprod(1+mkt_factor_rolling_capped1$pfret_full)-cummax(cumprod(1+mkt_factor_rolling_capped1$pfret_full)))/cummax(cumprod(1+mkt_factor_rolling_capped1$pfret_full)))*100,4)
ind_matrix[1,4] = round((tail(cumprod(1+mkt_factor_expanding_capped2$pfret_full), 1)^(1/NROW(Sigmas))-1)*100,4)
ind_matrix[2,4] = round(mean(mkt_factor_expanding_capped2$pfret*100),4)
ind_matrix[3,4] = round(sd(mkt_factor_expanding_capped2$pfret*100),4)
ind_matrix[4,4] = round(mean(mkt_factor_expanding_capped2$pfret)/sd(mkt_factor_expanding_capped2$pfret),4)
ind_matrix[5,4] = round(min((cumprod(1+mkt_factor_expanding_capped2$pfret_full)-cummax(cumprod(1+mkt_factor_expanding_capped2$pfret_full)))/cummax(cumprod(1+mkt_factor_expanding_capped2$pfret_full)))*100,2)
ind_matrix[1,5] = round((tail(cumprod(1+mkt_factor_rolling_capped2$pfret_full), 1)^(1/NROW(Sigmas))-1)*100,4)
ind_matrix[2,5] = round(mean(mkt_factor_rolling_capped2$pfret*100),4)
ind_matrix[3,5] = round(sd(mkt_factor_rolling_capped2$pfret*100),4)
ind_matrix[4,5] = round(mean(mkt_factor_rolling_capped2$pfret)/sd(mkt_factor_rolling_capped2$pfret),4)
ind_matrix[5,5] = round(min((cumprod(1+mkt_factor_rolling_capped2$pfret_full)-cummax(cumprod(1+mkt_factor_rolling_capped2$pfret_full)))/cummax(cumprod(1+mkt_factor_rolling_capped2$pfret_full)))*100,4)
ind_matrix[1,6] = round(tail(cumprod(1+mkt_factor_expanding_capped5$pfret_full), 1)^(1/NROW(Sigmas))-1,4)*100
ind_matrix[2,6] = round(mean(mkt_factor_expanding_capped5$pfret*100),4)
ind_matrix[3,6] = round(sd(mkt_factor_expanding_capped5$pfret*100),4)
ind_matrix[4,6] = round(mean(mkt_factor_expanding_capped5$pfret)/sd(mkt_factor_expanding_capped5$pfret),4)
ind_matrix[5,6] = round(min((cumprod(1+mkt_factor_expanding_capped5$pfret_full)-cummax(cumprod(1+mkt_factor_expanding_capped5$pfret_full)))/cummax(cumprod(1+mkt_factor_expanding_capped5$pfret_full)))*100,2)
ind_matrix[1,7] = round(tail(cumprod(1+mkt_factor_rolling_capped5$pfret_full), 1)^(1/NROW(Sigmas))-1,4)*100
ind_matrix[2,7] = round(mean(mkt_factor_rolling_capped5$pfret*100),4)
ind_matrix[3,7] = round(sd(mkt_factor_rolling_capped5$pfret*100),4)
ind_matrix[4,7] = round(mean(mkt_factor_rolling_capped5$pfret)/sd(mkt_factor_rolling_capped5$pfret),4)
ind_matrix[5,7] = round(min((cumprod(1+mkt_factor_rolling_capped5$pfret_full)-cummax(cumprod(1+mkt_factor_rolling_capped5$pfret_full)))/cummax(cumprod(1+mkt_factor_rolling_capped5$pfret_full)))*100,2)
ind_matrix[1,8] = round(tail(cumprod(1+mkt_inv_rets_full), 1)^(1/NROW(Sigmas))-1,4)*100
ind_matrix[2,8] = round(mean(mkt_inv_rets*100),4)
ind_matrix[3,8] = round(sd(mkt_inv_rets*100),4)
ind_matrix[4,8] = round(mean(mkt_inv_rets)/sd(mkt_inv_rets),4)
ind_matrix[5,8] = round(min((cumprod(1+mkt_inv_rets_full)-cummax(cumprod(1+mkt_inv_rets_full)))/cummax(cumprod(1+mkt_inv_rets_full)))*100,2)
ind_matrix[6,1] = round(lm(ew_mkt_factor_rets ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(ew_mkt_factor_rets ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,1] = round(lm(ew_mkt_factor_rets ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(ew_mkt_factor_rets)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(ew_mkt_factor_rets ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,2] = round(lm(mkt_factor_expanding_capped1$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(mkt_factor_expanding_capped1$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,2] = round(lm(mkt_factor_expanding_capped1$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(mkt_factor_expanding_capped1$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(mkt_factor_expanding_capped1$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,3] = round(lm(mkt_factor_rolling_capped1$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,3] = round(lm(mkt_factor_rolling_capped1$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,3] = round(lm(mkt_factor_rolling_capped1$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(mkt_factor_rolling_capped1$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(mkt_factor_rolling_capped1$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,4] = round(lm(mkt_factor_expanding_capped2$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,4] = round(lm(mkt_factor_expanding_capped2$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,4] = round(lm(mkt_factor_expanding_capped2$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(mkt_factor_expanding_capped2$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(mkt_factor_expanding_capped2$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,5] = round(lm(mkt_factor_rolling_capped2$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,5] = round(lm(mkt_factor_rolling_capped2$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,5] = round(lm(mkt_factor_rolling_capped2$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(mkt_factor_rolling_capped2$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(mkt_factor_rolling_capped2$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,6] = round(lm(mkt_factor_expanding_capped5$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,6] = round(lm(mkt_factor_expanding_capped5$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,6] = round(lm(mkt_factor_expanding_capped5$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(mkt_factor_expanding_capped5$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(mkt_factor_expanding_capped5$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,7] = round(lm(mkt_factor_rolling_capped5$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,7] = round(lm(mkt_factor_rolling_capped5$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,7] = round(lm(mkt_factor_rolling_capped5$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(mkt_factor_rolling_capped5$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(mkt_factor_rolling_capped5$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,8] = round(lm(mkt_inv_rets ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,8] = round(lm(mkt_inv_rets~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,8] = round(round(lm(mkt_inv_rets ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1],4)/(sd(mkt_inv_rets)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(mkt_inv_rets ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)

rownames(ind_matrix) = stat_names
colnames(ind_matrix) = c('Equal', 'Roll 1', 'Exp 1', 'Exp 2', 'Roll 2', 'Exp 5', 'Roll 5', 'Market')

print(xtable(ind_matrix, caption = 'Performance Metrics of the Time-adjusted Mkt-Factor Portfolios', digits = 3))

## Conservative TAA Strategy
### Abs(30%) Strategy, Rolling Weights

market_weight_capped30 = matrix(nrow = NROW(mus), ncol = 8)
market_weight_capped30[,1] = rep(1,NROW(mus))

for (i in 1:NROW(Sigmas)){
  market_weight_capped30[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = Sigmas[[i]], mus = mus[[i]], control = list(fnscale=-1), lower = rep(-0.3,7), upper = rep(0.3,7), method = 'L-BFGS-B')$par
}

mkt_factor_rolling_capped30 = matrix_to_cleaned_df(market_weight_capped30)

### Lagged Business-Cycle Adjusted

expansion_TAA_weights = c(1,0.15,0.15,0,0.10,0,0.10,0.25)
slowdown_TAA_weights = c(1,0.05,0.15,0.10,0.10,0.15,0.20,0.15)
recovery_TAA_weights = c(1,0.20,0.10,0.05,0.10,0.05,0,0.25)
downturn_TAA_weights = c(1,0,0.05,0.10,0.25,0.25,0.25,0)

daily_expansion_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Expansion',]
daily_downturn_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Downturn',]
daily_recovery_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Recovery',]
daily_slowdown_oecd_lagged = dailyMerged_OECD_lagged[dailyMerged_OECD_lagged$lagged=='Slowdown',]


daily_expansion_oecd_TAA_lagged  = cbind(daily_expansion_oecd_lagged, t(expansion_TAA_weights))
daily_downturn_oecd_TAA_lagged = cbind(daily_downturn_oecd_lagged, t(downturn_TAA_weights))
daily_recovery_oecd_TAA_lagged = cbind(daily_recovery_oecd_lagged, t(recovery_TAA_weights))
daily_slowdown_oecd_TAA_lagged = cbind(daily_slowdown_oecd_lagged, t(downturn_TAA_weights))
daily_OECD_TAA_lagged_ft = bind_rows(daily_expansion_oecd_TAA_lagged, daily_downturn_oecd_TAA_lagged, daily_recovery_oecd_TAA_lagged, daily_slowdown_oecd_TAA_lagged)
daily_OECD_TAA_lagged_ft$pfret = rowSums(daily_OECD_TAA_lagged_ft[3:10]*daily_OECD_TAA_lagged_ft[15:22])
daily_OECD_TAA_lagged_ft$pfret_full = daily_OECD_TAA_lagged_ft$pfret+daily_OECD_TAA_lagged_ft$RF
daily_OECD_TAA_lagged_ft=daily_OECD_TAA_lagged_ft[order(daily_OECD_TAA_lagged_ft$date),]

### Compare the performance

ind_matrix = matrix(nrow = 8, ncol = 2)
rownames(ind_matrix) = stat_names
ind_matrix[1,1] = round((tail(cumprod(1+daily_OECD_TAA_lagged_ft$pfret_full), 1)^(1/n_distinct(daily_OECD_TAA_lagged_ft$month))-1)*100,4)
ind_matrix[2,1] = round(mean(daily_OECD_TAA_lagged_ft$pfret)*100,4)
ind_matrix[3,1] = round(sd(daily_OECD_TAA_lagged_ft$pfret)*100,4)
ind_matrix[4,1] = round(mean(daily_OECD_TAA_lagged_ft$pfret)/sd(daily_OECD_TAA_lagged_ft$pfret),4)
ind_matrix[5,1] = round(min((cumprod(1+daily_OECD_TAA_lagged_ft$pfret_full)-cummax(cumprod(1+daily_OECD_TAA_lagged_ft$pfret_full)))/cummax(cumprod(1+daily_OECD_TAA_lagged_ft$pfret_full)))*100,4)
ind_matrix[6,1] = round(lm(daily_OECD_TAA_lagged_ft$pfret ~ daily_OECD_TAA_lagged_ft$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(daily_OECD_TAA_lagged_ft$pfret ~ daily_OECD_TAA_lagged_ft$Mkt.RF)[[1]][2],4)
ind_matrix[8,1] = round(lm(daily_OECD_TAA_lagged_ft$pfret ~ daily_OECD_TAA_lagged_ft$Mkt.RF)[[1]][1]/(sd(daily_OECD_TAA_lagged_ft$pfret)-sd(daily_OECD_TAA_lagged_ft$Mkt.RF)*lm(daily_OECD_TAA_lagged_ft$pfret ~ daily_OECD_TAA_lagged_ft$Mkt.RF)[[1]][2]),4)
ind_matrix[1,2] = round((tail(cumprod(1+mkt_factor_rolling_capped30$pfret_full), 1)^(1/n_distinct(mkt_factor_rolling_capped30$month))-1)*100,4)
ind_matrix[2,2] = round(mean(mkt_factor_rolling_capped30$pfret)*100,4)
ind_matrix[3,2] = round(sd(mkt_factor_rolling_capped30$pfret)*100,4)
ind_matrix[4,2] = round(mean(mkt_factor_rolling_capped30$pfret)/sd(mkt_factor_rolling_capped30$pfret),4)
ind_matrix[5,2] = round(min((cumprod(1+mkt_factor_rolling_capped30$pfret_full)-cummax(cumprod(1+mkt_factor_rolling_capped30$pfret_full)))/cummax(cumprod(1+mkt_factor_rolling_capped30$pfret_full)))*100,4)
ind_matrix[6,2] = round(lm(mkt_factor_rolling_capped30$pfret ~ mkt_factor_rolling_capped30$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(mkt_factor_rolling_capped30$pfret ~ mkt_factor_rolling_capped30$Mkt.RF)[[1]][2],4)
ind_matrix[8,2] = round(lm(mkt_factor_rolling_capped30$pfret ~ mkt_factor_rolling_capped30$Mkt.RF)[[1]][1]/(sd(mkt_factor_rolling_capped30$pfret)-sd(mkt_factor_rolling_capped30$Mkt.RF)*lm(mkt_factor_rolling_capped30$pfret ~ mkt_factor_rolling_capped30$Mkt.RF)[[1]][2]),4)

rownames(ind_matrix) = stat_names
colnames(ind_matrix) = c('Lag. TAA Business Cycle Factor-tilt', 'Roll. Cap 0.3')

print(xtable(ind_matrix, caption = 'Performance Metrics of the Time-adjusted Mkt-Factor Portfolios', digits = 3))

## Plot weights over time, no shorts vs. two market portfolios
dates = mkt_factor_expanding_capped2$date
plot(dates, mkt_factor_rolling_capped2$Value, type = 'l', col = 'red', ylab = 'Weight on Value Factor', main = 'Time-varying Weight on the Value Factor in Portfolios')
lines(dates, mkt_factor_expanding_capped2$Value, type = 'l')
legend('topright', c('Expanding', 'Rolling'), lty = 1, col = c('black', 'red'))

# Plot the earnings factor

plot(dates, mkt_factor_rolling_capped2$Earnings, type = 'l', col = 'red', ylab = 'Weight on Earnings Factor', main = 'Time-Varying Factor-Tilt Weight on the Earnings Factor')
lines(dates, mkt_factor_expanding_capped2$Earnings, type = 'l')
legend('bottomleft', c('Expanding', 'Rolling'), lty = 1, col = c('black', 'red'))

# Plot the investment factor

plot(dates, mkt_factor_rolling_capped2$Investment, type = 'l', col = 'red', ylab = 'Weight on Investment Factor', main = ' Time-Varying Factor-Tilt Weight on the Investment Factor')
lines(dates, mkt_factor_expanding_capped2$Investment, type = 'l')
legend('bottomleft', c('Expanding', 'Rolling'), lty = 1, col = c('black', 'red'))

### Regressions of the Factor Weights on the Lagged Economic Variables ###
# Regression of the weights of the factor-tilt portfolios - Rolling Portfolio
colnames(market_weight_capped2) = c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
market_weight_capped2 = as.data.frame(market_weight_capped2)
market_weight_capped2$month = months
constrained_market_merged_rolling = merge(monthlyMerged, market_weight_capped2, by = 'month')

reg_fit_size = lm(constrained_market_merged_rolling$Size ~ dplyr::lag(constrained_market_merged_rolling$Sentiment_Index)+dplyr::lag(constrained_market_merged_rolling$svar)+dplyr::lag(constrained_market_merged_rolling$lty)+
                    dplyr::lag(constrained_market_merged_rolling$dfy)+dplyr::lag(constrained_market_merged_rolling$tms)+dplyr::lag(constrained_market_merged_rolling$infl)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Expansion)+
                    dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_rolling$Mkt.RF)+dplyr::lag(constrained_market_merged_rolling$SMB), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_size,lag=m-1,prewhite = F,adjust = T)
reg_data_size = coeftest(reg_fit_size,vcov=NW_VCOV)

reg_fit_val = lm(constrained_market_merged_rolling$Value ~ dplyr::lag(constrained_market_merged_rolling$Sentiment_Index)+dplyr::lag(constrained_market_merged_rolling$svar)+dplyr::lag(constrained_market_merged_rolling$lty)+
                   dplyr::lag(constrained_market_merged_rolling$dfy)++dplyr::lag(constrained_market_merged_rolling$tms)+dplyr::lag(constrained_market_merged_rolling$infl)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Expansion)+
                   dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_rolling$Mkt.RF)+dplyr::lag(constrained_market_merged_rolling$HML), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_val,lag=m-1,prewhite = F,adjust = T)
reg_data_val = coeftest(reg_fit_val,vcov=NW_VCOV)


reg_fit_mom = lm(constrained_market_merged_rolling$Momentum ~ dplyr::lag(constrained_market_merged_rolling$Sentiment_Index)+dplyr::lag(constrained_market_merged_rolling$svar)+dplyr::lag(constrained_market_merged_rolling$lty)+
                   dplyr::lag(constrained_market_merged_rolling$dfy)++dplyr::lag(constrained_market_merged_rolling$tms)+dplyr::lag(constrained_market_merged_rolling$infl)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Expansion)+
                   dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_rolling$Mkt.RF)+dplyr::lag(constrained_market_merged_rolling$mom), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_mom,lag=m-1,prewhite = F,adjust = T)
reg_data_mom = coeftest(reg_fit_mom,vcov=NW_VCOV)

reg_fit_inv = lm(constrained_market_merged_rolling$Investment ~ dplyr::lag(constrained_market_merged_rolling$Sentiment_Index)+dplyr::lag(constrained_market_merged_rolling$svar)+dplyr::lag(constrained_market_merged_rolling$lty)+
                   dplyr::lag(constrained_market_merged_rolling$dfy)++dplyr::lag(constrained_market_merged_rolling$tms)+dplyr::lag(constrained_market_merged_rolling$infl)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Expansion)+
                   dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_rolling$Mkt.RF)+dplyr::lag(constrained_market_merged_rolling$CMA), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_inv,lag=m-1,prewhite = F,adjust = T)
reg_data_inv = coeftest(reg_fit_inv,vcov=NW_VCOV)

reg_fit_earn = lm(constrained_market_merged_rolling$Earnings ~ dplyr::lag(constrained_market_merged_rolling$Sentiment_Index)+dplyr::lag(constrained_market_merged_rolling$svar)+dplyr::lag(constrained_market_merged_rolling$lty)+
                    dplyr::lag(constrained_market_merged_rolling$dfy)++dplyr::lag(constrained_market_merged_rolling$tms)+dplyr::lag(constrained_market_merged_rolling$infl)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Expansion)+
                    dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_rolling$Mkt.RF)+dplyr::lag(constrained_market_merged_rolling$RMW), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_earn,lag=m-1,prewhite = F,adjust = T)
reg_data_earn = coeftest(reg_fit_earn,vcov=NW_VCOV)

reg_fit_qual = lm(constrained_market_merged_rolling$Quality.y ~ dplyr::lag(constrained_market_merged_rolling$Sentiment_Index)+dplyr::lag(constrained_market_merged_rolling$svar)+dplyr::lag(constrained_market_merged_rolling$lty)+
                    dplyr::lag(constrained_market_merged_rolling$dfy)+dplyr::lag(constrained_market_merged_rolling$tms)+dplyr::lag(constrained_market_merged_rolling$infl)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Expansion)+
                    dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_rolling$Mkt.RF)+dplyr::lag(constrained_market_merged_rolling$Quality.x), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_qual,lag=m-1,prewhite = F,adjust = T)
reg_data_qual = coeftest(reg_fit_qual,vcov=NW_VCOV)

reg_fit_bab = lm(constrained_market_merged_rolling$BAB.y ~ dplyr::lag(constrained_market_merged_rolling$Sentiment_Index)+dplyr::lag(constrained_market_merged_rolling$svar)+dplyr::lag(constrained_market_merged_rolling$lty)+
                   dplyr::lag(constrained_market_merged_rolling$dfy)+dplyr::lag(constrained_market_merged_rolling$tms)+dplyr::lag(constrained_market_merged_rolling$infl)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Expansion)+
                   dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_rolling$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_rolling$Mkt.RF)+dplyr::lag(constrained_market_merged_rolling$BAB.x), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_bab,lag=m-1,prewhite = F,adjust = T)
reg_data_bab = coeftest(reg_fit_bab,vcov=NW_VCOV)

my_models = list(reg_data_val, reg_data_size, reg_data_inv, reg_data_earn, reg_data_mom, reg_data_qual, reg_data_bab)

stargazer(my_models, title = 'Regressions', type = 'html', out = 'training output', keep.stat = 'rsq')

mom_r2 =  round(summary(reg_fit_mom)$adj.r.squared, 4)
value_r2 = round(summary(reg_fit_val)$adj.r.squared, 4)
size_r2 = round(summary(reg_fit_size)$adj.r.squared, 4)
inv_r2 = round(summary(reg_fit_inv)$adj.r.squared, 4)
earn_r2 = round(summary(reg_fit_earn)$adj.r.squared, 4)
qual_r2 = round(summary(reg_fit_qual)$adj.r.squared, 4)
bab_r2 = round(summary(reg_fit_bab)$adj.r.squared, 4)

star = stargazer(my_models,
                 add.lines = list(c("Adjusted $R^2$", value_r2, size_r2, inv_r2, earn_r2, mom_r2, qual_r2, bab_r2))
)

cat(star, sep = '\n', file = 'OECD_Weights_Mkt_Factor_Unconstrained.tex')

# Regression of the weights of the factor-tilt portfolios - Expanding Portfolio
colnames(market_weight_expanding_capped2) = c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
market_weight_expanding_capped2 = as.data.frame(market_weight_expanding_capped2)
market_weight_expanding_capped2$month = months
constrained_market_merged_expanding = merge(monthlyMerged, market_weight_expanding_capped2, by = 'month')

reg_fit_size = lm(constrained_market_merged_expanding$Size ~ dplyr::lag(constrained_market_merged_expanding$Sentiment_Index)+dplyr::lag(constrained_market_merged_expanding$svar)+dplyr::lag(constrained_market_merged_expanding$lty)+
                    dplyr::lag(constrained_market_merged_expanding$dfy)+dplyr::lag(constrained_market_merged_expanding$tms)+dplyr::lag(constrained_market_merged_expanding$infl)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Expansion)+
                    dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_expanding$Mkt.RF)+dplyr::lag(constrained_market_merged_expanding$SMB), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_size,lag=m-1,prewhite = F,adjust = T)
reg_data_size = coeftest(reg_fit_size,vcov=NW_VCOV)

reg_fit_val = lm(constrained_market_merged_expanding$Value ~ dplyr::lag(constrained_market_merged_expanding$Sentiment_Index)+dplyr::lag(constrained_market_merged_expanding$svar)+dplyr::lag(constrained_market_merged_expanding$lty)+
                   dplyr::lag(constrained_market_merged_expanding$dfy)+dplyr::lag(constrained_market_merged_expanding$tms)+dplyr::lag(constrained_market_merged_expanding$infl)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Expansion)+
                   dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_expanding$Mkt.RF)+dplyr::lag(constrained_market_merged_expanding$HML), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_val,lag=m-1,prewhite = F,adjust = T)
reg_data_val = coeftest(reg_fit_val,vcov=NW_VCOV)


reg_fit_mom = lm(constrained_market_merged_expanding$Momentum ~ dplyr::lag(constrained_market_merged_expanding$Sentiment_Index)+dplyr::lag(constrained_market_merged_expanding$svar)+dplyr::lag(constrained_market_merged_expanding$lty)+
                   dplyr::lag(constrained_market_merged_expanding$dfy)+dplyr::lag(constrained_market_merged_expanding$tms)+dplyr::lag(constrained_market_merged_expanding$infl)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Expansion)+
                   dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_expanding$Mkt.RF)+dplyr::lag(constrained_market_merged_expanding$mom), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_mom,lag=m-1,prewhite = F,adjust = T)
reg_data_mom = coeftest(reg_fit_mom,vcov=NW_VCOV)

reg_fit_inv = lm(constrained_market_merged_expanding$Investment ~ dplyr::lag(constrained_market_merged_expanding$Sentiment_Index)+dplyr::lag(constrained_market_merged_expanding$svar)+dplyr::lag(constrained_market_merged_expanding$lty)+
                   dplyr::lag(constrained_market_merged_expanding$dfy)+dplyr::lag(constrained_market_merged_expanding$tms)+dplyr::lag(constrained_market_merged_expanding$infl)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Expansion)+
                   dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_expanding$Mkt.RF)+dplyr::lag(constrained_market_merged_expanding$CMA), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_inv,lag=m-1,prewhite = F,adjust = T)
reg_data_inv = coeftest(reg_fit_inv,vcov=NW_VCOV)

reg_fit_earn = lm(constrained_market_merged_expanding$Earnings ~ dplyr::lag(constrained_market_merged_expanding$Sentiment_Index)+dplyr::lag(constrained_market_merged_expanding$svar)+dplyr::lag(constrained_market_merged_expanding$lty)+
                    dplyr::lag(constrained_market_merged_expanding$dfy)+dplyr::lag(constrained_market_merged_expanding$tms)+dplyr::lag(constrained_market_merged_expanding$infl)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Expansion)+
                    dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_expanding$Mkt.RF)+dplyr::lag(constrained_market_merged_expanding$RMW), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_earn,lag=m-1,prewhite = F,adjust = T)
reg_data_earn = coeftest(reg_fit_earn,vcov=NW_VCOV)

reg_fit_qual = lm(constrained_market_merged_expanding$Quality.y ~ dplyr::lag(constrained_market_merged_expanding$Sentiment_Index)+dplyr::lag(constrained_market_merged_expanding$svar)+dplyr::lag(constrained_market_merged_expanding$lty)+
                    dplyr::lag(constrained_market_merged_expanding$dfy)+dplyr::lag(constrained_market_merged_expanding$tms)+dplyr::lag(constrained_market_merged_expanding$infl)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Expansion)+
                    dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_expanding$Mkt.RF)+dplyr::lag(constrained_market_merged_expanding$Quality.x), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_qual,lag=m-1,prewhite = F,adjust = T)
reg_data_qual = coeftest(reg_fit_qual,vcov=NW_VCOV)

reg_fit_bab = lm(constrained_market_merged_expanding$BAB.y ~ dplyr::lag(constrained_market_merged_expanding$Sentiment_Index)+dplyr::lag(constrained_market_merged_expanding$svar)+dplyr::lag(constrained_market_merged_expanding$lty)+
                   dplyr::lag(constrained_market_merged_expanding$dfy)+dplyr::lag(constrained_market_merged_expanding$tms)+dplyr::lag(constrained_market_merged_expanding$infl)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Expansion)+
                   dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Slowdown)+dplyr::lag(constrained_market_merged_expanding$OECD_econstage_Recovery)+dplyr::lag(constrained_market_merged_expanding$Mkt.RF)+dplyr::lag(constrained_market_merged_expanding$BAB.x), na.action=na.exclude)

NW_VCOV<-NeweyWest(reg_fit_bab,lag=m-1,prewhite = F,adjust = T)
reg_data_bab = coeftest(reg_fit_bab,vcov=NW_VCOV)

my_models = list(reg_data_val, reg_data_size, reg_data_inv, reg_data_earn, reg_data_mom, reg_data_qual, reg_data_bab)

stargazer(my_models, title = 'Regressions', type = 'html', out = 'training output', keep.stat = 'rsq')

mom2 =  round(summary(reg_fit_mom)$adj.r.squared, 4)
value_r2 = round(summary(reg_fit_val)$adj.r.squared, 4)
size_r2 = round(summary(reg_fit_size)$adj.r.squared, 4)
inv_r2 = round(summary(reg_fit_inv)$adj.r.squared, 4)
earn_r2 = round(summary(reg_fit_earn)$adj.r.squared, 4)
qual_r2 = round(summary(reg_fit_qual)$adj.r.squared, 4)
bab_r2 = round(summary(reg_fit_bab)$adj.r.squared, 4)

star = stargazer(my_models,
                 add.lines = list(c("Adjusted $R^2$", value_r2, size_r2, inv_r2, earn_r2, mom2, qual_r2, bab_r2))
)

cat(star, sep = '\n', file = 'OECD_Weights_Mkt_Factor_Expanding.tex')



## Appendix XIV Predicting Weights Based on Regressions ##
# Require 7 years or 84 monthly observations to make the prediction
# Expanding Regression on the Expanding Weights
weight_table = matrix(nrow = 677, ncol = 8)
weight_table[85:677,1] = 1
for (i in 1:677){
  weight_table[i,2]=rollRegres::roll_regres(formula = Size[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(SMB)), data = constrained_market_merged_expanding, min_obs = 84, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,3]=rollRegres::roll_regres(formula = Value[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(HML)), data = constrained_market_merged_expanding, min_obs = 84, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,4]=rollRegres::roll_regres(formula = Momentum[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(mom)), data = constrained_market_merged_expanding, min_obs = 84, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,5]=rollRegres::roll_regres(formula = Investment[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(CMA)), data = constrained_market_merged_expanding, min_obs = 84, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,6]=rollRegres::roll_regres(formula = Earnings[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(RMW)), data = constrained_market_merged_expanding, min_obs = 84, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,7]=rollRegres::roll_regres(formula = Quality.y[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(Quality.x)), data = constrained_market_merged_expanding, min_obs = 84, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,8]=rollRegres::roll_regres(formula = BAB.y[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(BAB.x)), data = constrained_market_merged_expanding, min_obs = 84, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
}

weight_table = as.data.frame(weight_table[!rowSums(!is.finite(weight_table)),])
colnames(weight_table)= c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
weight_table$month = constrained_market_merged_expanding$month[86:NROW(constrained_market_merged_expanding)]
reg_exp_exp = merge(daily, weight_table, by = 'month')
reg_exp_exp$pfret = rowSums(reg_exp_exp[3:10]*reg_exp_exp[13:20])
reg_exp_exp$pfret_full = reg_exp_exp$pfret+reg_exp_exp$RF

## Rolling regression on Expanding Weights
weight_table = matrix(nrow = 677, ncol = 8)
weight_table[85:677,1] = 1
for (i in 1:677){
  weight_table[i,2]=rollRegres::roll_regres(formula = Size[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(SMB)), data = constrained_market_merged_expanding, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,3]=rollRegres::roll_regres(formula = Value[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(HML)), data = constrained_market_merged_expanding, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,4]=rollRegres::roll_regres(formula = Momentum[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(mom)), data = constrained_market_merged_expanding, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,5]=rollRegres::roll_regres(formula = Investment[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(CMA)), data = constrained_market_merged_expanding, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,6]=rollRegres::roll_regres(formula = Earnings[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(RMW)), data = constrained_market_merged_expanding, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,7]=rollRegres::roll_regres(formula = Quality.y[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(Quality.x)), data = constrained_market_merged_expanding, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,8]=rollRegres::roll_regres(formula = BAB.y[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(BAB.x)), data = constrained_market_merged_expanding, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
}

weight_table = as.data.frame(weight_table[!rowSums(!is.finite(weight_table)),])
colnames(weight_table)= c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
weight_table$month = constrained_market_merged_expanding$month[86:NROW(constrained_market_merged_expanding)]
reg_exp_roll = merge(daily, weight_table, by = 'month')
reg_exp_roll$pfret = rowSums(reg_exp_roll[3:10]*reg_exp_roll[13:20])
reg_exp_roll$pfret_full = reg_exp_roll$pfret+reg_exp_roll$RF
## Rolling Regression on Rolling Weights

weight_table = matrix(nrow = 677, ncol = 8)
weight_table[85:677,1] = 1
for (i in 1:677){
  weight_table[i,2]=rollRegres::roll_regres(formula = Size[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(SMB)), data = constrained_market_merged_rolling, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,3]=rollRegres::roll_regres(formula = Value[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(HML)), data = constrained_market_merged_rolling, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,4]=rollRegres::roll_regres(formula = Momentum[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(mom)), data = constrained_market_merged_rolling, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,5]=rollRegres::roll_regres(formula = Investment[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(CMA)), data = constrained_market_merged_rolling, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,6]=rollRegres::roll_regres(formula = Earnings[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(RMW)), data = constrained_market_merged_rolling, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,7]=rollRegres::roll_regres(formula = Quality.y[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(Quality.x)), data = constrained_market_merged_rolling, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,8]=rollRegres::roll_regres(formula = BAB.y[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(BAB.x)), data = constrained_market_merged_rolling, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
}

weight_table = as.data.frame(weight_table[!rowSums(!is.finite(weight_table)),])
colnames(weight_table)= c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')

weight_table$month = constrained_market_merged_expanding$month[86:NROW(constrained_market_merged_expanding)]
reg_roll_roll = merge(daily, weight_table, by = 'month')
reg_roll_roll$pfret = rowSums(reg_roll_roll[3:10]*reg_roll_roll[13:20])
reg_roll_roll$pfret_full = reg_roll_roll$pfret+reg_roll_roll$RF


## Expanding Regression on Rolling Weights

weight_table = matrix(nrow = 677, ncol = 8)
weight_table[85:677,1] = 1
for (i in 1:677){
  weight_table[i,2]=rollRegres::roll_regres(formula = Size[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(SMB)), data = constrained_market_merged_rolling, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,3]=rollRegres::roll_regres(formula = Value[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(HML)), data = constrained_market_merged_rolling, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,4]=rollRegres::roll_regres(formula = Momentum[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(mom)), data = constrained_market_merged_rolling, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,5]=rollRegres::roll_regres(formula = Investment[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(CMA)), data = constrained_market_merged_rolling, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,6]=rollRegres::roll_regres(formula = Earnings[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(RMW)), data = constrained_market_merged_rolling, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,7]=rollRegres::roll_regres(formula = Quality.y[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(Quality.x)), data = constrained_market_merged_rolling, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  weight_table[i,8]=rollRegres::roll_regres(formula = BAB.y[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(BAB.x)), data = constrained_market_merged_rolling, width = 84, do_downdates = F, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
}

weight_table = as.data.frame(weight_table[!rowSums(!is.finite(weight_table)),])
colnames(weight_table)= c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')

weight_table$month = constrained_market_merged_expanding$month[86:NROW(constrained_market_merged_expanding)]
reg_roll_exp = merge(daily, weight_table, by = 'month')
reg_roll_exp$pfret = rowSums(reg_roll_exp[3:10]*reg_roll_exp[13:20])
reg_roll_exp$pfret_full = reg_roll_exp$pfret+reg_roll_exp$RF

# Do the well-known comparison table
## As a benchmark, consider the backwards looking mean-variance during the same period.
mean_var_expanding = mkt_factor_expanding_capped2[mkt_factor_expanding_capped2$date>as.Date('1972-07-31'),]
mean_var_rolling = mkt_factor_rolling_capped2[mkt_factor_rolling_capped2$date>as.Date('1972-07-31'),]

ind_matrix = matrix(nrow = 8, ncol = 6)
rownames(ind_matrix) = stat_names
ind_matrix[1,1] = round((tail(cumprod(1+reg_exp_exp$pfret_full), 1)^(1/n_distinct(reg_exp_exp$month))-1)*100,4)
ind_matrix[1,2] = round((tail(cumprod(1+reg_exp_roll$pfret_full),1)^(1/n_distinct(reg_exp_roll$month))-1)*100,4)
ind_matrix[1,3] = round((tail(cumprod(1+reg_roll_roll$pfret_full),1)^(1/n_distinct(reg_roll_roll$month))-1)*100,4)
ind_matrix[1,4] = round((tail(cumprod(1+reg_roll_exp$pfret_full),1)^(1/n_distinct(reg_roll_exp$month))-1)*100,4)
ind_matrix[1,5] = round((tail(cumprod(1+mean_var_rolling$pfret_full),1)^(1/n_distinct(mean_var_rolling$month))-1)*100,4)
ind_matrix[1,6] = round((tail(cumprod(1+mean_var_expanding$pfret_full),1)^(1/n_distinct(mean_var_expanding$month))-1)*100,4)
ind_matrix[2,1] = round(mean(reg_exp_exp$pfret*100),4)
ind_matrix[2,2] = round(mean(reg_exp_roll$pfret*100),4)
ind_matrix[2,3] = round(mean(reg_roll_roll$pfret*100),4)
ind_matrix[2,4] = round(mean(reg_roll_exp$pfret*100),4)
ind_matrix[2,5] = round(mean(mean_var_rolling$pfret*100),4)
ind_matrix[2,6] = round(mean(mean_var_expanding$pfret*100),4)
ind_matrix[3,1] = round(sd(reg_exp_exp$pfret*100),4)
ind_matrix[3,2] = round(sd(reg_exp_roll$pfret*100),4)
ind_matrix[3,3] = round(sd(reg_roll_roll$pfret*100),4)
ind_matrix[3,4] = round(sd(reg_roll_exp$pfret*100),4)
ind_matrix[3,5] = round(sd(mean_var_rolling$pfret*100),4)
ind_matrix[3,6] = round(sd(mean_var_expanding$pfret*100),4)
ind_matrix[4,1] = round(mean(reg_exp_exp$pfret)/sd(reg_exp_exp$pfret),4)
ind_matrix[4,2] = round(mean(reg_exp_roll$pfret)/sd(reg_exp_roll$pfret),4)
ind_matrix[4,3] = round(mean(reg_roll_roll$pfret)/sd(reg_roll_roll$pfret),4)
ind_matrix[4,4] = round(mean(reg_roll_exp$pfret)/sd(reg_roll_exp$pfret),4)
ind_matrix[4,5] = round(mean(mean_var_rolling$pfret)/sd(mean_var_rolling$pfret),4)
ind_matrix[4,6] = round(mean(mean_var_expanding$pfret)/sd(mean_var_expanding$pfret),4)
ind_matrix[5,1] = round(min((cumprod(1+reg_exp_exp$pfret_full)-cummax(cumprod(1+reg_exp_exp$pfret_full)))/cummax(cumprod(1+reg_exp_exp$pfret_full)))*100,4)
ind_matrix[5,2] = round(min((cumprod(1+reg_exp_roll$pfret_full)-cummax(cumprod(1+reg_exp_roll$pfret_full)))/cummax(cumprod(1+reg_exp_roll$pfret_full)))*100,4)
ind_matrix[5,3] = round(min((cumprod(1+reg_roll_roll$pfret_full)-cummax(cumprod(1+reg_roll_roll$pfret_full)))/cummax(cumprod(1+reg_roll_roll$pfret_full)))*100,4)
ind_matrix[5,4] = round(min((cumprod(1+reg_roll_exp$pfret_full)-cummax(cumprod(1+reg_roll_exp$pfret_full)))/cummax(cumprod(1+reg_roll_exp$pfret_full)))*100,4)
ind_matrix[5,5] = round(min((cumprod(1+mean_var_rolling$pfret_full)-cummax(cumprod(1+mean_var_rolling$pfret_full)))/cummax(cumprod(1+mean_var_rolling$pfret_full)))*100,4)
ind_matrix[5,6] = round(min((cumprod(1+mean_var_expanding$pfret_full)-cummax(cumprod(1+mean_var_expanding$pfret_full)))/cummax(cumprod(1+mean_var_expanding$pfret_full)))*100,4)
ind_matrix[6,1] = round(lm(reg_exp_exp$pfret ~ reg_exp_exp$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(reg_exp_exp$pfret~ reg_exp_exp$Mkt.RF)[[1]][2],4)
ind_matrix[8,1] = round(lm(reg_exp_exp$pfret ~ reg_exp_exp$Mkt.RF)[[1]][1]/(sd(reg_exp_exp$pfret)-sd(reg_exp_exp$Mkt.RF)*lm(reg_exp_exp$pfret ~ reg_exp_exp$Mkt.RF)[[1]][2]),4)
ind_matrix[6,2] = round(lm(reg_exp_roll$pfret ~ reg_exp_roll$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(reg_exp_roll$pfret~ reg_exp_roll$Mkt.RF)[[1]][2],4)
ind_matrix[8,2] = round(lm(reg_exp_roll$pfret ~ reg_exp_roll$Mkt.RF)[[1]][1]/(sd(reg_exp_roll$pfret)-sd(reg_exp_roll$Mkt.RF)*lm(reg_exp_roll$pfret ~ reg_exp_roll$Mkt.RF)[[1]][2]),4)
ind_matrix[6,3] = round(lm(reg_roll_roll$pfret ~reg_roll_roll$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,3] = round(lm(reg_roll_roll$pfret~ reg_roll_roll$Mkt.RF)[[1]][2],4)
ind_matrix[8,3] = round(lm(reg_roll_roll$pfret ~ reg_roll_roll$Mkt.RF)[[1]][1]/(sd(reg_roll_roll$pfret)-sd(reg_roll_roll$Mkt.RF)*lm(reg_roll_roll$pfret ~ reg_roll_roll$Mkt.RF)[[1]][2]),4)
ind_matrix[6,4] = round(lm(reg_roll_exp$pfret ~ reg_roll_exp$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,4] = round(lm(reg_roll_exp$pfret~ reg_roll_exp$Mkt.RF)[[1]][2],4)
ind_matrix[8,4] = round(lm(reg_roll_exp$pfret ~ reg_roll_exp$Mkt.RF)[[1]][1]/(sd(reg_roll_exp$pfret)-sd(reg_roll_exp$Mkt.RF)*lm(reg_roll_exp$pfret ~ reg_roll_exp$Mkt.RF)[[1]][2]),4)
ind_matrix[6,5] = round(lm(mean_var_rolling$pfret ~ mean_var_rolling$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,5] = round(lm(mean_var_rolling$pfret~ mean_var_rolling$Mkt.RF)[[1]][2],4)
ind_matrix[8,5] = round(lm(mean_var_rolling$pfret ~ mean_var_rolling$Mkt.RF)[[1]][1]/(sd(mean_var_rolling$pfret)-sd(mean_var_rolling$Mkt.RF)*lm(mean_var_rolling$pfret ~ mean_var_rolling$Mkt.RF)[[1]][2]),4)
ind_matrix[6,6] = round(lm(mean_var_expanding$pfret ~ mean_var_expanding$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,6] = round(lm(mean_var_expanding$pfret~ mean_var_expanding$Mkt.RF)[[1]][2],4)
ind_matrix[8,6] = round(lm(mean_var_expanding$pfret ~ mean_var_expanding$Mkt.RF)[[1]][1]/(sd(mean_var_expanding$pfret)-sd(mean_var_expanding$Mkt.RF)*lm(mean_var_expanding$pfret ~ mean_var_expanding$Mkt.RF)[[1]][2]),4)


ind_matrix=as.data.frame(ind_matrix)
colnames(ind_matrix) = c('Exp. Weights, Exp. Reg', 'Exp. Weights, Roll. Reg', 'Roll. Weights, Roll. Reg', 'Roll. Weights, Exp. Reg', 'Mean Var Roll', 'Mean Var Exp')

print(xtable(ind_matrix, caption = 'Performance of Predicted Weights Portfolios', digits = 3))

# Plot fitted weights to actual weights
plot(reg_roll_roll$date, reg_roll_roll$Value, type = 'l', main =  'Fitted Regression Weights vs. Mean-Variance Weights in the Value Factor', xlab = 'Date', ylab = 'Weight in the Value Factor')
lines(mean_var_rolling$date, mean_var_rolling$Value, type = 'l', col = 'red')
lines(reg_exp_roll$date, reg_exp_roll$Value, lty = 2, type = 'l', col = 'green')
lines(mean_var_rolling$date, mean_var_expanding$Value, lty = 2, type = 'l', col = 'grey')
legend('bottomleft', c('Rolling Reg. Rolling Weight', 'Mean Variance Rolling Weight', 'Rolling Reg. Expanding Weight', 'Mean Variance Expanding Weight'), lty = c(1,1,2,2), col = c('black', 'red', 'green', 'grey') )


# Regression of returns and predictions on macroeconomic factors. Start with 84 months to match the regression and prediction of weights

return_predictions = matrix(nrow = 677, ncol = 8)
for (i in 1:677){
  
  return_predictions[i,1]=rollRegres::roll_regres(formula = Mkt.RF[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)), data = monthlyMerged, min_obs = 84, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  
  return_predictions[i,2]=rollRegres::roll_regres(formula = SMB[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                            +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                            +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(SMB)), data = monthlyMerged, min_obs = 84, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions[i,3]=rollRegres::roll_regres(formula = HML[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(HML)), data = monthlyMerged, min_obs = 84, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions[i,4]=rollRegres::roll_regres(formula = mom[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(mom)), data = monthlyMerged, min_obs = 84, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions[i,5]=rollRegres::roll_regres(formula = CMA[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(CMA)), data = monthlyMerged, min_obs = 84, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions[i,6]=rollRegres::roll_regres(formula = RMW[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(RMW)), data = monthlyMerged, min_obs = 84, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions[i,7]=rollRegres::roll_regres(formula = Quality[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(Quality)), data = monthlyMerged, min_obs = 84, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions[i,8]=rollRegres::roll_regres(formula = BAB[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(BAB)), data = monthlyMerged, min_obs = 84, width = 84, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
}

return_predictions = as.data.frame(return_predictions[!rowSums(!is.finite(return_predictions)),])
colnames(return_predictions)= c('Mkt.RF', 'SMB', 'HML', 'RMW', 'CMA', 'mom', 'Quality', 'BAB')

return_predictions$month = monthlyMerged$month[86:NROW(monthlyMerged)]
return_predictions = return_predictions[192:NROW(return_predictions),]


## Regression with 275 prediction months to fit the monthly GARCH models.
return_predictions_275 = matrix(nrow = 677, ncol = 8)
for (i in 1:677){
  return_predictions_275[i,1]=rollRegres::roll_regres(formula = Mkt.RF[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                      +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                      +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)), data = monthlyMerged, min_obs = 275, width = 275, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions_275[i,2]=rollRegres::roll_regres(formula = SMB[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(SMB)), data = monthlyMerged, min_obs = 275, width = 275, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions_275[i,3]=rollRegres::roll_regres(formula = HML[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(HML)), data = monthlyMerged, min_obs = 275, width = 275, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions_275[i,4]=rollRegres::roll_regres(formula = mom[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(mom)), data = monthlyMerged, min_obs = 275, width = 275, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions_275[i,5]=rollRegres::roll_regres(formula = CMA[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(CMA)), data = monthlyMerged, min_obs = 275, width = 275, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions_275[i,6]=rollRegres::roll_regres(formula = RMW[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(RMW)), data = monthlyMerged, min_obs = 275, width = 275, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions_275[i,7]=rollRegres::roll_regres(formula = Quality[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(Quality)), data = monthlyMerged, min_obs = 275, width = 275, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
  return_predictions_275[i,8]=rollRegres::roll_regres(formula = BAB[2:678] ~ na.omit(dplyr::lag(Sentiment_Index))+na.omit(dplyr::lag(svar))+na.omit(dplyr::lag(lty))+na.omit(dplyr::lag(dfy))
                                                  +na.omit(dplyr::lag(infl))+na.omit(dplyr::lag(OECD_econstage_Expansion))+na.omit(dplyr::lag(OECD_econstage_Slowdown))
                                                  +na.omit(dplyr::lag(OECD_econstage_Recovery))+na.omit(dplyr::lag(Mkt.RF)+dplyr::lag(BAB)), data = monthlyMerged, min_obs = 275, width = 275, do_downdates = T, do_compute = '1_step_forecasts')$one_step_forecasts[[i]]
  
}

return_predictions_275 = as.data.frame(return_predictions_275[!rowSums(!is.finite(return_predictions_275)),])
colnames(return_predictions_275)= c('Mkt.RF', 'SMB', 'HML', 'RMW', 'CMA', 'mom', 'Quality', 'BAB')

return_predictions_275$month = monthlyMerged$month[277:NROW(monthlyMerged)]
plot(return_predictions$month, monthlyMerged$HML[277:NROW(monthlyMerged)], type = 'l', xlab = 'Date', ylab = 'Return on the Value Factor', main = 'Return on Value Factor, Actual vs. Reg. Forecasts')
lines(return_predictions$month,return_predictions$HML, type = 'l', col = 'red' )
lines(return_predictions_275$month, return_predictions_275$HML, type = 'l', col = 'blue')
legend('bottomleft', c('Actual', 'Prediction w. 84 obs', 'Prediction w. 275 obs'), lty = 1, col = c('black', 'red', 'blue'))

## Calculate the correlations of the predicted returns with the actual returns

ret_corr = matrix(nrow = 8, ncol = 2)
k=1

for (i in c('Mkt.RF', 'SMB', 'HML', 'RMW', 'CMA', 'mom', 'Quality', 'BAB')){
  ret_corr[k,1]=cor(monthlyMerged[[i]][277:NROW(monthlyMerged)], return_predictions[[i]])
  ret_corr[k,2]=cor(monthlyMerged[[i]][277:NROW(monthlyMerged)], return_predictions_275[[i]])
  k=k+1
}

rownames(ret_corr) = c('Market', 'Size', 'Value', 'Investment', 'Earnings', 'Momentum', 'Quality', 'BAB')
colnames(ret_corr) = c('Prediction w. 84 obs', 'Prediction w. 275 obs')

print(xtable(ret_corr, caption = 'Correlation Reg. Pred Rets vs. Actual Rets', digits = 3))

### Test of autocorrelation in the returns, also during business cycles ###

# Box-Pierce Test adjusted with White Standard Errors (Varneskov, 2023)
## Define the function (Varneskov, 2023)

fct_box_test <- function(retm, lags){
  # step 1: calculate the acf 
  acf_retM <- acf(retm, lag.max = lags, plot = FALSE)
  acf_retM <- acf_retM$acf[-1]  # delete the first element since this is acf(0)=1.
  
  # step 2: calculate V_2(100) on pages 107-108
  retm_hat   <- retm - mean(retm)
  gamma_0    <- mean(retm^2)
  V_vec      <- c() # initializing the covariance matrix as as vector
  
  for(j in 1:lags){
    retm_hat_j <- lag(retm_hat,n=j)[-(1:j)] # the component [-(1:j)] deletes the missing observations
    retm_hat_0 <- retm_hat[-(1:j)]
    
    lambda_jj  <- mean(retm_hat_j^2 * retm_hat_0^2)
    V_vec      <- c(V_vec, lambda_jj / gamma_0^2)    # sequentially stores the variance elements.
  }
  
  V2 <- diag(V_vec)  # construct the diagonal covariance matrix
  
  # step 3: calculate the test statistics
  BP <- NROW(retm) * ( t(acf_retM) %*% solve(V2) %*% acf_retM ) %>% as.vector
  
  BL <- 0
  for(j in 1:lags){
    BL <- BL + NROW(retm)*(NROW(retm) + 2) * (acf_retM[j]^2/V_vec[j])/(NROW(retm)-j)
  }
  
  # step 4: return the test statistic
  # we return the output as a list, since this is a need way to store it.
  Q <- list(
    BP = BP,
    BL = BL,
    LS = (sqrt(NROW(retm))*acf_retM)/sqrt(V_vec)
  )
  
  Q
}
## Then loop through the factors to get the Box-Pierce statistics.


k=0

BP_matrix=matrix(nrow=8, ncol = 2)
rownames(BP_matrix) =  c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')

for (i in c('Mkt.RF', 'SMB', 'HML', 'RMW','CMA', 'Momentum', 'Quality', 'BAB')){
  
  k=k+1
  # We then call the box-test function.
  box_tests <- fct_box_test(retm = daily[[i]], lags = 100) 
  
  # We can then construct the Box-Pierce test
  box_pierce_test_stat    <- box_tests$BP
  box_pierce_test_pvalue  <- 1-pchisq(q = box_pierce_test_stat, df = 100)
  
  BP_matrix[k,1] = round(box_pierce_test_stat,4)
  BP_matrix[k,2] = round(box_pierce_test_pvalue,4)
  
  
}

BP_matrix = as.data.frame(BP_matrix)
colnames(BP_matrix) = c('Test Stat', 'P-value')

print(xtable(BP_matrix, caption = 'Box Pierce Test for Factors - 100 lags with White Standard Errors', digits = 3 ))


# Then do plots of the autocorrelation itself.


for (i in c('Mkt.RF', 'SMB', 'HML', 'RMW','CMA', 'Momentum', 'Quality', 'BAB')){
  jpeg(file = i)
  
  plot(seq(1,100),fct_box_test(daily[[i]], 100)$LS, main = paste0("Autocorrelation of the first 100 lags on ", i), xlab = 'Lag', ylab = 'T-stat')
  abline(h=1.96)
  abline(h=-1.96)
  dev.off()

}


# Plots of Business-Cycle Specific Autocorrelations

## Momentum

k=0
counter = 26

BP_mom_OECD=matrix(nrow=4, ncol = 2)
rownames(BP_mom_OECD) =  unique(dailyMerged$OECD_econstage)
for (s in unique(dailyMerged$OECD_econstage)){
  temp = dailyMerged[dailyMerged$OECD_econstage==s,]
  k=k+1
  # I call the Box-Pierce Test function defined above.
  box_tests <- fct_box_test(retm = temp$Momentum, lags = 100) 
  
  # We can then construct the Box-Pierce test
  box_pierce_test_stat    <- box_tests$BP
  box_pierce_test_pvalue  <- 1-pchisq(q = box_pierce_test_stat, df = 100)
  
  BP_mom_OECD[k,1] = round(box_pierce_test_stat,4)
  BP_mom_OECD[k,2] = round(box_pierce_test_pvalue,4)
  
  # Plot
  
  jpeg(file = s)
  
  plot(seq(1,100),fct_box_test(temp$Momentum, 100)$LS, main = paste0(" Momentum Autocorrelation During ", s), xlab = 'Lag', ylab = 'T-stat')
  abline(h=1.96)
  abline(h=-1.96)
  dev.off()
  counter = counter +1
  
  
}

BP_mom_OECD = as.data.frame(BP_mom_OECD)
colnames(BP_mom_OECD) = c('Test Stat', 'P-value')

print(xtable(BP_mom_OECD, caption = 'Box Pierce Test for Momentum in different economic stages - 100 lags with White Standard Errors', digits = 3))

# Market

k=0

BP_market_OECD=matrix(nrow=4, ncol = 2)
rownames(BP_market_OECD) =  unique(dailyMerged$OECD_econstage)
for (s in unique(dailyMerged$OECD_econstage)){
  temp = dailyMerged[dailyMerged$OECD_econstage==s,]
  k=k+1
    # We then call the box-test function.
    box_tests <- fct_box_test(retm = temp$Mkt.RF, lags = 100) 
    
    # We can then construct the Box-Pierce test
    box_pierce_test_stat    <- box_tests$BP
    box_pierce_test_pvalue  <- 1-pchisq(q = box_pierce_test_stat, df = 100)
    
    BP_market_OECD[k,1] = round(box_pierce_test_stat,4)
    BP_market_OECD[k,2] = round(box_pierce_test_pvalue,4)
    
    # Plot
    
    jpeg(file = s)
    
    plot(seq(1,100),fct_box_test(temp$Mkt.RF, 100)$LS, main = s, xlab = 'Lag', ylab = 'T-stat')
    abline(h=1.96)
    abline(h=-1.96)
    dev.off()
    
    
    
}
  
  BP_market_OECD = as.data.frame(BP_market_OECD)
  colnames(BP_market_OECD) = c('Test Stat', 'P-value')
  
  print(xtable(BP_market_OECD, caption = 'BP Test for the Market in Econ. Stages - 100 lags White SE'))


## 4.3.2.4 Individual Long-Short Factor Portfolios ##
# Read in the return data for the Individual Factor Portfolios

individual_factors = read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/FactorsIndividual.csv'))
univariate_factors = read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/FactorsUnivariate.csv'))

# Clean the returns so I can use them
## Further, to make the time-series comparable to the 4.3.2.2 and 4.3.2.1, the data is capped at Nov. 30th, 2022. 

individual_factors<-transform(individual_factors,date=as.Date(as.character(X),"%Y%m%d"))
individual_factors = subset(individual_factors, select = -c(X))
individual_factors = individual_factors%>%
  mutate(across(where(is.numeric),~ . / 100))

univariate_factors = transform(univariate_factors, date =as.Date(as.character(X), '%Y%m%d'))
univariate_factors = subset(univariate_factors, select = -c(X))
univariate_factors = univariate_factors%>%
  mutate(across(where(is.numeric),~ . / 100))

individual_factors = subset(individual_factors, date < as.Date("2022-12-01"))
univariate_factors = subset(univariate_factors, date < as.Date("2022-12-01"))

## Construct the rolling portfolios

individual_factors_nested = individual_factors %>%
  mutate(year_month = yearmonth(date)) %>%
  nest(-year_month) %>%
  rolling_origin(
    initial = 12,
    assess = 1,
    skip =0,
    cumulative = F
  )


univariate_factors_nested = univariate_factors %>%
  mutate(year_month = yearmonth(date)) %>%
  nest(-year_month) %>%
  rolling_origin(
    initial = 12,
    assess = 1,
    skip =0,
    cumulative = F
  )


mus_individual <- map(individual_factors_nested$splits, ~ analysis(.x) %>% 
             unnest(data) %>%
             select(-year_month, -date) %>%
             colMeans) %>%
  setNames(c(ListNamesDates))


Sigmas_individual = map(individual_factors_nested$splits, ~ analysis(.x) %>%
               unnest() %>%
               tk_xts(., date_var = date) %>%
               cov(.)) %>%
  setNames(c(ListNamesDates))


mus_univariate <- map(univariate_factors_nested$splits, ~ analysis(.x) %>% 
                        unnest(data) %>%
                        select(-year_month, -date) %>%
                        colMeans) %>%
  setNames(c(ListNamesDates))

Sigmas_univariate = map(univariate_factors_nested$splits, ~ analysis(.x) %>%
                          unnest() %>%
                          tk_xts(., date_var = date) %>%
                          cov(.)) %>%
  setNames(c(ListNamesDates))
# Construct the expanding portfolios

individual_factors_expanding = individual_factors %>%
  mutate(year_month = yearmonth(date)) %>%
  nest(-year_month) %>%
  rolling_origin(
    initial = 12,
    assess = 1,
    skip =0,
    cumulative = T
  )


univariate_factors_expanding = univariate_factors %>%
  mutate(year_month = yearmonth(date)) %>%
  nest(-year_month) %>%
  rolling_origin(
    initial = 12,
    assess = 1,
    skip =0,
    cumulative = T
  )


mus_individual_expanding <- map(individual_factors_expanding$splits, ~ analysis(.x) %>% 
                        unnest(data) %>%
                        select(-year_month, -date) %>%
                        colMeans) %>%
  setNames(c(ListNamesDates))


Sigmas_individual_expanding = map(individual_factors_expanding$splits, ~ analysis(.x) %>%
                          unnest() %>%
                          tk_xts(., date_var = date) %>%
                          cov(.)) %>%
  setNames(c(ListNamesDates))


mus_univariate_expanding <- map(univariate_factors_expanding$splits, ~ analysis(.x) %>% 
                        unnest(data) %>%
                        select(-year_month, -date) %>%
                        colMeans) %>%
  setNames(c(ListNamesDates))

Sigmas_univariate_expanding = map(univariate_factors_expanding$splits, ~ analysis(.x) %>%
                          unnest() %>%
                          tk_xts(., date_var = date) %>%
                          cov(.)) %>%
  setNames(c(ListNamesDates))



# Create an equal-weight benchmark, which invests (1/11) in each of the 11 portfolios.

ew = rep(1/11,11)
ew_ret_univariate = rowSums(sweep(univariate_factors[,1:11],MARGIN = 2, ew, FUN = '*'))
ew_ret_full_univariate = ew_ret_univariate+daily$RF
ew_ret_univariate = ew_ret_univariate[253:NROW(daily)] # Make the benchmark start at day 253, when the rolling portfolios start
ew_ret_full_univariate = ew_ret_full_univariate[253:NROW(daily)]

ew_ret_individual = rowSums(sweep(individual_factors[,1:11],MARGIN = 2, ew, FUN = '*'))
ew_ret_full_individual = ew_ret_individual+daily$RF
ew_ret_individual = ew_ret_individual[253:NROW(daily)] # Make the benchmark start at day 253, when the rolling portfolios start
ew_ret_full_individual = ew_ret_full_individual[253:NROW(daily)]

# Now find the optimal weights via numeric optimization maximizing the Sharpe ratio

individual_factors_weight_rolling = matrix(nrow = NROW(Sigmas_univariate), ncol = 11)
univariate_factors_weight_rolling = matrix(nrow = NROW(Sigmas_univariate), ncol = 11)
individual_factors_weight_expanding = matrix(nrow = NROW(Sigmas_univariate), ncol = 11)
univariate_factors_weight_expanding = matrix(nrow = NROW(Sigmas_univariate), ncol = 11)


SharpeFunctionIndividual = function(weight, Sigmas, mus){
  Mu = mus%*%weight
  Var = weight%*%Sigmas%*%weight
  Sharpe = Mu/sqrt(Var)
  sumweights = sum(weight)
  Objective = Sharpe - ((sumweights-1)^2)*1000
  return(Objective)
}

weights = c(1/11,1/11,1/11,1/11,1/11,1/11,1/11,1/11,1/11,1/11,1/11)
for (i in 1:NROW(Sigmas_univariate)){
  individual_factors_weight_rolling[i,1:11]=round(optim(par = weights, fn = SharpeFunctionIndividual, Sigmas = Sigmas_individual[[i]], mus = mus_individual[[i]], control = list(fnscale=-1), lower = rep(-2,11), upper = rep(2, 11), method = 'L-BFGS-B')$par,4)
}

for (i in 1:NROW(Sigmas_univariate)){
  univariate_factors_weight_rolling[i,1:11] = round(optim(par = weights, fn = SharpeFunctionIndividual, Sigmas = Sigmas_univariate[[i]], mus = mus_univariate[[i]], control = list(fnscale=-1), lower = rep(-2,11), upper = rep(2, 11), method = 'L-BFGS-B')$par,4)
}

for (i in 1:NROW(Sigmas_univariate)){
  individual_factors_weight_expanding[i,1:11]=round(optim(par = weights, fn = SharpeFunctionIndividual, Sigmas = Sigmas_individual_expanding[[i]], mus = mus_individual_expanding[[i]], control = list(fnscale=-1), lower = rep(-2,11), upper = rep(2, 11), method = 'L-BFGS-B')$par,4)
}

for (i in 1:NROW(Sigmas_univariate)){
  univariate_factors_weight_expanding[i,1:11] = round(optim(par = weights, fn = SharpeFunctionIndividual, Sigmas = Sigmas_univariate_expanding[[i]], mus = mus_univariate_expanding[[i]], control = list(fnscale=-1), lower = rep(-2,11), upper = rep(2, 11), method = 'L-BFGS-B')$par,4)
}

# Then create a function to merge the optimal weights with the daily returns
matrix_to_cleaned_df_individual = function(matrix, df2){
  dataframe = as.data.frame(matrix)
  colnames(dataframe) = c('Market', 'Value', 'Growth', 'High_Earnings', 'Low_Earnings', 'High_Investment', 'Low_Investment', 'Small',
                          'Big', 'Winners', 'Loses')
  dataframe$month = months
  df = merge(dataframe, df2, by = 'month')
  df$pfret =rowSums(df[2:12]*df[13:23])
  df$pfret_full = df$pfret + daily$RF[253:length(daily$RF)]
  return(df)
}

individual_factors$month = daily$month
univariate_factors$month = daily$month
individual_rolling = matrix_to_cleaned_df_individual(individual_factors_weight_rolling, individual_factors)
univariate_rolling = matrix_to_cleaned_df_individual(univariate_factors_weight_rolling, univariate_factors)
individual_expanding = matrix_to_cleaned_df_individual(individual_factors_weight_expanding, individual_factors)
univariate_expanding = matrix_to_cleaned_df_individual(univariate_factors_weight_expanding, univariate_factors)

# Plot weights of the value factor over time for the different portfolios
## Rolling and expanding methodologies for the value portfolios

plot(dates, individual_rolling$Value.x, type = 'l', ylab = 'Weight on Factors', main = 'Weight in the Individual Factors 1964-2022')
lines(dates, individual_rolling$Growth.x, type = 'l', col = 'red')
lines(dates, individual_expanding$Value.x, type = 'l', col = 'grey')
lines(dates, individual_expanding$Growth.x, type = 'l', col = 'blue')
legend('topleft', c('Value Rolling', 'Growth Rolling', 'Value Expanding', 'Growth Expanding'), col = c('black',  'red', 'grey', 'blue'), lty = 1)

# The rolling portfolios for the value factor for all the portfolios considered
plot(dates, individual_rolling$Value.x, type = 'l', main = 'Value Factor Related Bets - Different Mean-Variance Approaches', xlab = 'Dates', ylab = 'Weight')
lines(dates, univariate_rolling$Value.x, type = 'l', col = 'grey', lty = 2)
lines(dates, individual_rolling$Growth.x, type = 'l', col = 'red')
lines(dates, univariate_rolling$Growth.x, type = 'l', col = 'blue', lty = 2)
lines(dates, mkt_factor_rolling_capped2$Value, type = 'l', col = 'green')
legend('bottomleft', c('Value Ind.', 'Growth Ind.', 'Value Univ.', 'Growth Univ.', 'Value Factor-Tilt'), col = c('black','grey', 'red', 'blue', 'green'), lty = c(1,2,1,2,1))


# Calculate the well-known performance matrix
ind_matrix = matrix(nrow = 8, ncol = 6)
ind_matrix[1,1] = round((tail(cumprod(1+individual_rolling$pfret_full), 1)^(1/NROW(Sigmas_univariate))-1)*100,4)
ind_matrix[1,2] = round((tail(cumprod(1+individual_expanding$pfret_full),1)^(1/NROW(Sigmas_univariate))-1)*100,4)
ind_matrix[2,1] = round(mean(individual_rolling$pfret)*100,4)
ind_matrix[2,2] = round(mean(individual_expanding$pfret)*100,4)
ind_matrix[3,1] = round(sd(individual_rolling$pfret)*100,4)
ind_matrix[3,2] = round(sd(individual_expanding$pfret)*100,4)
ind_matrix[4,1] = round(mean(individual_rolling$pfret)/sd(individual_rolling$pfret),4)
ind_matrix[4,2] = round(mean(individual_expanding$pfret)/sd(individual_expanding$pfret),4)
ind_matrix[5,1] = round(min((cumprod(1+individual_rolling$pfret_full)-cummax(cumprod(1+individual_rolling$pfret_full)))/cummax(cumprod(1+individual_rolling$pfret_full))),4)*100
ind_matrix[5,2] = round(min((cumprod(1+individual_expanding$pfret_full)-cummax(cumprod(1+individual_expanding$pfret_full)))/cummax(cumprod(1+individual_expanding$pfret_full))),4)*100
ind_matrix[1,3] = round((tail(cumprod(1+univariate_rolling$pfret_full), 1)^(1/NROW(Sigmas_univariate))-1)*100,4)
ind_matrix[2,3] = round(mean(univariate_rolling$pfret)*100,4)
ind_matrix[3,3] = round(sd(univariate_rolling$pfret)*100,4)
ind_matrix[4,3] = round(mean(univariate_rolling$pfret)/sd(univariate_rolling$pfret),4)
ind_matrix[5,3] = round(min((cumprod(1+univariate_rolling$pfret_full)-cummax(cumprod(1+univariate_rolling$pfret_full)))/cummax(cumprod(1+univariate_rolling$pfret_full))),4)*100
ind_matrix[1,4] = round((tail(cumprod(1+univariate_expanding$pfret_full), 1)^(1/NROW(Sigmas_univariate))-1)*100,4)
ind_matrix[2,4] = round(mean(univariate_expanding$pfret)*100,4)
ind_matrix[3,4] = round(sd(univariate_expanding$pfret)*100,4)
ind_matrix[4,4] = round(mean(univariate_expanding$pfret)/sd(univariate_expanding$pfret),4)
ind_matrix[5,4] = round(min((cumprod(1+univariate_expanding$pfret_full)-cummax(cumprod(1+univariate_expanding$pfret_full)))/cummax(cumprod(1+univariate_expanding$pfret_full))),4)*100
ind_matrix[6,1] = round(lm(individual_rolling$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(individual_rolling$pfret~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,1] = round(lm(individual_rolling$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(individual_rolling$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(individual_rolling$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,2] = round(lm(individual_expanding$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(individual_expanding$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,2] = round(lm(individual_expanding$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(individual_expanding$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(individual_expanding$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,3] = round(lm(univariate_rolling$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,3] = round(lm(univariate_rolling$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,3] = round(lm(univariate_rolling$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(univariate_rolling$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(univariate_rolling$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,4] = round(lm(univariate_expanding$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,4] = round(lm(univariate_expanding$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,4] = round(lm(univariate_expanding$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(univariate_expanding$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(univariate_expanding$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[1,5] = round((tail(cumprod(1+ew_ret_full_univariate), 1)^(1/NROW(Sigmas_univariate))-1)*100,4)
ind_matrix[2,5] = round(mean(ew_ret_univariate)*100,4)
ind_matrix[3,5] = round(sd(ew_ret_univariate)*100,4)
ind_matrix[4,5] = round(mean(ew_ret_univariate)/sd(ew_ret_univariate),4)
ind_matrix[5,5] = round(min((cumprod(1+ew_ret_univariate)-cummax(cumprod(1+ew_ret_univariate)))/cummax(cumprod(1+ew_ret_univariate))),4)*100
ind_matrix[6,5] = round(lm(ew_ret_univariate ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,5] = round(lm(ew_ret_univariate ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,5] = round(lm(ew_ret_univariate ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(ew_ret_univariate)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(ew_ret_univariate ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[1,6] = round((tail(cumprod(1+ew_ret_full_individual), 1)^(1/NROW(Sigmas_univariate))-1)*100,4)
ind_matrix[2,6] = round(mean(ew_ret_individual)*100,4)
ind_matrix[3,6] = round(sd(ew_ret_individual)*100,4)
ind_matrix[4,6] = round(mean(ew_ret_individual)/sd(ew_ret_individual),4)
ind_matrix[5,6] = round(min((cumprod(1+ew_ret_individual)-cummax(cumprod(1+ew_ret_individual)))/cummax(cumprod(1+ew_ret_individual))),4)*100
ind_matrix[6,6] = round(lm(ew_ret_individual ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,6] = round(lm(ew_ret_individual ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,6] = round(lm(ew_ret_individual ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(ew_ret_individual)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(ew_ret_individual ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)

rownames(ind_matrix) = stat_names
colnames(ind_matrix) = c('Individual Rolling', 'Individual Expanding', 'Univariate Rolling', 'Univariate Expanding', 'Equal Univariate', 'Equal Individual')

print(xtable(ind_matrix, caption = ' Performance of Individual Factor Portfolios', digits = 3 ))


### 4.4 Including Transaction Costs ###
# Loading the transaction costs from Novy-Marx & Velikov (2016)
first_dates = daily[!duplicated(daily$month),][1] # Generating a list of the first trading days in each month. This is needed for merging the transaction costs. 
TransCosts = read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/TransCosts.csv')) # Read in the data
TransCosts$date = first_dates$date # Assuming that each day where transaction costs are paid is the first trading day of the month.
TransCosts = TransCosts%>%
  mutate(across(where(is.numeric),~ . / 100))

daily_trans = daily
Trans_Merged = merge(daily, TransCosts, by = 'date', all.x = T)
Trans_Merged[is.na(Trans_Merged)] = 0
daily_trans[3:9] = Trans_Merged[13:19]+ Trans_Merged[3:9] # Define returns as the returns with transaction costs
# Now it's just constructing the portfolios.
# Firstly we construct the naive portfolio that "cheats" by looking at the full period.

# Get the performance of the factors after considering the Novy-Marx (2016) transaction costs
trans_matrix = matrix(nrow = 8, ncol = 7)
for (i in 3:9){
  trans_matrix[1,i-2] = round(mean(TransCosts[[i-1]]*100),4)
  trans_matrix[2,i-2] = round(mean(daily[[i]])*100,4)
  trans_matrix[3,i-2] = round(mean(daily[[i]])/sd(daily[[i]]),4)
  trans_matrix[4,i-2] = round(mean(daily_trans[[i]])*100,4)
  trans_matrix[5,i-2] = round(mean(daily_trans[[i]])/sd(daily_trans[[i]]),4)
  trans_matrix[6,i-2] = round(lm(daily_trans[[i]] ~ daily$Mkt.RF)[[1]][1]*100,4)
  trans_matrix[7,i-2] = round(lm(daily_trans[[i]] ~ daily$Mkt.RF)[[1]][2],4)
  trans_matrix[8,i-2] = round(lm(daily_trans[[i]] ~ daily$Mkt.RF)[[1]][1]/(sd(daily_trans[[i]])-sd(daily$Mkt.RF)*lm(daily_trans[[i]] ~ daily$Mkt.RF)[[1]][2]),4)

}

rownames(trans_matrix) = c('TC Monthly (%)','Excess Ret wo TC (%)', 'Sharpe wo TC', 'Excess Ret w. TC (%)', 'Sharpe w TC', 'Alpha w TC', 'Beta w TC', 'Information Ratio w TC')
colnames(trans_matrix) = c('Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')

print(xtable(trans_matrix, caption = 'Factors after TC, Daily', digits = 3))

# Now begins the mean-variance optimization with the transaction costs
# It is assumed, that the investor knows the previous transaction costs and optimizes taking these into consideration
# First, we consider the Naive tangency portfolio with transaction costs
## Construct new means and variances for the tangency portfolio

means_trans = colMeans(daily_trans[2:9])
cov_trans = cov(daily_trans[2:9])

tangency_trans = round((inv(cov_trans)%*%(means_trans))/(vec%*%inv(cov_trans)%*%(means_trans))[1],4)
colnames(tangency_trans) = c('Weight')
rownames(tangency_trans) = c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
print(xtable(tangency_trans, caption = 'Naive Tangency Weights with TC', digits = 3))

## Consider rolling tangency restricted portfolio

daily_trans_rolling = daily_trans[1:9] %>%
  mutate(year_month = yearmonth(date)) %>%
  nest(-year_month) %>%
  rolling_origin(
    initial = 12,
    assess = 1,
    skip =0,
    cumulative = F
  )

daily_trans_expanding = daily_trans[1:9] %>%
  mutate(year_month = yearmonth(date)) %>%
  nest(-year_month) %>%
  rolling_origin(
    initial = 12,
    assess = 1,
    skip =0,
    cumulative = T
  )

mus_trans_rolling <- map(daily_trans_rolling$splits, ~ analysis(.x) %>% 
             unnest(data) %>%
             select(-year_month, -date) %>%
             colMeans) %>%
  setNames(c(ListNamesDates))

mus_trans_expanding <- map(daily_trans_expanding$splits, ~ analysis(.x) %>% 
                           unnest(data) %>%
                             select(-year_month, -date) %>%
                           colMeans) %>%
  setNames(c(ListNamesDates))


Sigmas_trans_rolling = map(daily_trans_rolling$splits, ~ analysis(.x) %>%
               unnest() %>%
               tk_xts(., date_var = date) %>%
               cov(.)) %>%
  setNames(c(ListNamesDates))

Sigmas_trans_expanding = map(daily_trans_expanding$splits, ~ analysis(.x) %>%
                             unnest() %>%
                             tk_xts(., date_var = date) %>%
                             cov(.)) %>%
  setNames(c(ListNamesDates))

ones = rep(list(vec),NROW(Sigmas))
# Calculate the optimal weights, explicitly considering transaction costs
market_weight_trans_expanding = matrix(nrow = NROW(mus), ncol = 8)
market_weight_trans_expanding[,1] = rep(1,NROW(mus))
market_weight_trans_rolling = matrix(nrow = NROW(mus), ncol = 8)
market_weight_trans_rolling[,1] = rep(1,NROW(mus))
for (i in 1:NROW(Sigmas_trans_expanding)){
  market_weight_trans_expanding[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = Sigmas_trans_expanding[[i]], mus = mus_trans_expanding[[i]], control = list(fnscale=-1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}
for (i in 1:NROW(Sigmas_trans_expanding)){
  market_weight_trans_rolling[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = Sigmas_trans_rolling[[i]], mus = mus_trans_rolling[[i]], control = list(fnscale=-1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}

matrix_to_cleaned_df = function(matrix){
  dataframe = as.data.frame(matrix)
  colnames(dataframe) = c('Market', 'Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
  dataframe$month = months
  df = merge(dataframe, daily_trans, by = 'month')
  df$pfret =rowSums(df[2:9]*df[11:18])
  df$pfret_full = df$pfret +daily$RF[253:length(daily$RF)]
  return(df)
}

trans_expanding = matrix_to_cleaned_df(market_weight_trans_expanding)
trans_rolling = matrix_to_cleaned_df(market_weight_trans_rolling)

# Create the equal-returns benchmark
ew_trans = as.data.frame(rowSums(sweep(daily_trans[,2:9], MARGIN = 2, ew_mkt_factor_weights, FUN = '*')))
colnames(ew_trans) = 'pfret'
ew_trans$date = daily$date
ew_trans$pfret_full = ew_trans$pfret+ daily$RF
ew_trans = ew_trans[ew_trans$date>as.Date('1964-06-30'),]# Make the benchmark of equal length as the rolling portfolio

# Plot weights

plot(dates, trans_rolling$Value, type = 'l', ylab = 'Weight', main = 'Weight in the Momentum and Value Factor - With and Without TC')
lines(dates, mkt_factor_rolling_capped2$Value, type = 'l', col ='red')
lines(dates, trans_rolling$Momentum.x, col = 'blue', type = 'l')
lines(dates, mkt_factor_rolling_capped2$Momentum.x, col = 'grey', type = 'l')
grid()
legend('topright', c('Value TC', 'Value wo TC', 'Momentum TC', 'Momentum wo TC'), lty = 1, col = c('black', 'red', 'blue', 'grey'))

# Now, I consider the double transaction costs. I.e. I also include the transacion costs for rebalancing the portfolio and as well as a short-sell cost.
## Set the return data with Novy-Marx & Velikov (2016) as a time-series to calculate the monthly return
## The monthly return is then used to find the needed rebalancing at the beginning of each month.
daily_trans = daily_trans %>% column_to_rownames(., var = "date")
daily_trans = as.xts(daily_trans[1:8])
monthly_rets_trans = apply.monthly(daily_trans, Return.cumulative)
## Rolling Portfolio
weight_chgs_rolling = market_weight_trans_rolling*(1+monthly_rets_trans[13:length(monthly_rets_trans$Mkt.RF)]) - dplyr::lag(market_weight_trans_rolling)
weight_chgs_rolling[1,] = market_weight_trans_rolling[1,] # The first month, I assume that the investor creates positions from 0
## The same for the expanding portfolio
weight_chgs_expanding = market_weight_trans_expanding*(1+monthly_rets_trans[13:length(monthly_rets_trans$Mkt.RF)])-dplyr::lag(market_weight_trans_expanding)
weight_chgs_expanding[1,] = market_weight_trans_rolling[1,]
## Define transaction costs of flipping (20 bps per 100%) for the equal-weighted, rolling and expanding portfolio
trans_cost_equal = as.data.frame(rowSums(abs(ew_mkt_factor_weights*(1+monthly_rets_trans[13:length(monthly_rets_trans$Mkt.RF)])-ew_mkt_factor_weights)*0.002))
trans_cost_rolling = as.data.frame(rowSums(abs(weight_chgs_rolling)*0.002))
trans_cost_expanding = as.data.frame(rowSums(abs(weight_chgs_expanding)*0.002))
## Define the shorting costs, 7 bps per month shorted.
shorting_costs_rolling = rowSums(ifelse(market_weight_trans_rolling<0, abs(market_weight_trans_rolling)*0.0007,0))
shorting_costs_expanding = rowSums(ifelse(market_weight_trans_expanding<0, abs(market_weight_trans_expanding)*0.0007,0))
## Now subtract the transaction costs and shorting costs from the returns after the Novy-Marx & Velikov (2016) transaction costs
## This is done both for the rolling, expanding and equal-weight portfolio.
trans_rolling[trans_rolling$date %in% first_dates$date,][20] = trans_rolling[trans_rolling$date %in% first_dates$date,][20]-trans_cost_rolling-shorting_costs_rolling
trans_rolling[trans_rolling$date %in% first_dates$date,][21] = trans_rolling[trans_rolling$date %in% first_dates$date,][21]-trans_cost_rolling-shorting_costs_rolling
trans_expanding[trans_expanding$date %in% first_dates$date,][20] = trans_expanding[trans_expanding$date %in% first_dates$date,][20]-trans_cost_expanding-shorting_costs_expanding
trans_expanding[trans_expanding$date %in% first_dates$date,][21] = trans_expanding[trans_expanding$date %in% first_dates$date,][21]-trans_cost_expanding-shorting_costs_expanding
ew_trans[ew_trans$date %in% first_dates$date,][1] = ew_trans[ew_trans$date %in% first_dates$date,][1]-trans_cost_equal
ew_trans[ew_trans$date %in% first_dates$date,][2] = ew_trans[ew_trans$date %in% first_dates$date,][2]-trans_cost_equal

# Calculate the performance of the portfolios after considering the double transaction costs 
ind_matrix = matrix(nrow = 8, ncol = 3)
ind_matrix[1,1] = round((tail(cumprod(1+trans_rolling$pfret_full), 1)^(1/NROW(Sigmas_trans_rolling))-1)*100,4)
ind_matrix[1,2] = round((tail(cumprod(1+trans_expanding$pfret_full),1)^(1/NROW(Sigmas_trans_rolling))-1)*100,4)
ind_matrix[2,1] = round(mean(trans_rolling$pfret)*100,4)
ind_matrix[2,2] = round(mean(trans_expanding$pfret)*100,4)
ind_matrix[3,1] = round(sd(trans_rolling$pfret)*100,4)
ind_matrix[3,2] = round(sd(trans_expanding$pfret)*100,4)
ind_matrix[4,1] = round(mean(trans_rolling$pfret)/sd(trans_rolling$pfret),4)
ind_matrix[4,2] = round(mean(trans_expanding$pfret)/sd(trans_expanding$pfret),4)
ind_matrix[5,1] = round(min((cumprod(1+trans_rolling$pfret_full)-cummax(cumprod(1+trans_rolling$pfret_full)))/cummax(cumprod(1+trans_rolling$pfret_full)))*100,4)
ind_matrix[5,2] = round(min((cumprod(1+trans_expanding$pfret_full)-cummax(cumprod(1+trans_expanding$pfret_full)))/cummax(cumprod(1+trans_expanding$pfret_full)))*100,4)
ind_matrix[1,3] = round((tail(cumprod(1+ew_trans$pfret_full), 1)^(1/NROW(Sigmas_trans_rolling))-1)*100,4)
ind_matrix[2,3] = round(mean(ew_trans$pfret)*100,4)
ind_matrix[3,3] = round(sd(ew_trans$pfret)*100,4)
ind_matrix[4,3] = round(mean(ew_trans$pfret)/sd(ew_trans$pfret),4)
ind_matrix[5,3] = round(min((cumprod(1+ew_trans$pfret_full)-cummax(cumprod(1+ew_trans$pfret_full)))/cummax(cumprod(1+ew_trans$pfret_full)))*100,4)
ind_matrix[6,1] = round(lm(trans_rolling$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(trans_rolling$pfret~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,1] = round(lm(trans_rolling$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(trans_rolling$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(trans_rolling$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,2] = round(lm(trans_expanding$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(trans_expanding$pfret~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,2] = round(lm(trans_expanding$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(trans_expanding$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(trans_expanding$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)
ind_matrix[6,3] = round(lm(ew_trans$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]*100,4)
ind_matrix[7,3] = round(lm(ew_trans$pfret~ daily$Mkt.RF[253:NROW(daily)])[[1]][2],4)
ind_matrix[8,3] = round(lm(ew_trans$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][1]/(sd(ew_trans$pfret)-sd(daily$Mkt.RF[253:NROW(daily)])*lm(ew_trans$pfret ~ daily$Mkt.RF[253:NROW(daily)])[[1]][2]),4)

rownames(ind_matrix)= stat_names
colnames(ind_matrix) = c('Rolling', 'Expanding', 'Equal-Weighted')

print(xtable(ind_matrix, caption = 'Performance of Market-Factor Portfolios after Double TC', digits = 3))


### 4.5 Analysis of Available Factor ETFs and their replication ###
# Read in the price data of the ETFs
SizeETF = read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/SizeFactorETF.csv'))
ValueETF = read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/ValueFactorETF.csv'))
MomentumETF = read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/MomentumFactorETF.csv'))
QualityETF=read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/QualityFactorETF.csv'))
LowBetaETF = read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/LowBetaETF.csv'))
MarketETF = read.csv(url('https://raw.githubusercontent.com/jskriverm/The-Optimal-Factor-Timing-Portfolio---Master-Thesis/master/MarketETF.csv'))
# Analyzing average return and correlation with the factors considered
## Calculate returns from the prices and merge it onto the dataframe of the daily returns.
calculate_return <- function(df) {
  df$Return <- df$Adj.Close / lag(df$Adj.Close) - 1
  df$date = as.Date(df$date, "%d/%m/%Y")
  df = merge(subset(daily, select = -c(Quarter, month)), df, by = 'date')
  df=drop_na(df)
  df$ExcessRet = df$Return - df$RF
  return(df)
}
ETF_List = list(SizeETF, ValueETF, MomentumETF, QualityETF, LowBetaETF, MarketETF)
list_of_dfs <- lapply(ETF_List, calculate_return)
names(list_of_dfs) = c("SizeETF", "ValueETF", "MomentumETF", "QualityETF", "LowBetaETF", 'MarketETF')
list2env(list_of_dfs, envir = globalenv())

# Generate a table to compare the performance of the ETFs vs. that of the factors.
comp_matrix = matrix(ncol = 6, nrow = 9)
comp_matrix[1,1] = round(mean(SizeETF$ExcessRet)*100,4)
comp_matrix[2,1] = round(mean(SizeETF$SMB)*100,4)
comp_matrix[3,1] = round(sd(SizeETF$ExcessRet)*100,4)
comp_matrix[4,1] = round(sd(SizeETF$SMB)*100,4)
comp_matrix[5,1]=round(cor(SizeETF$SMB,SizeETF$ExcessRet),4)
comp_matrix[6,1] = round(lm(SizeETF$ExcessRet ~ SizeETF$Mkt.RF)[[1]][1]*100,4)
comp_matrix[7,1] = round(lm(SizeETF$SMB ~ SizeETF$Mkt.RF)[[1]][1]*100,4)
comp_matrix[8,1] = round(lm(SizeETF$ExcessRet ~ SizeETF$Mkt.RF)[[1]][2],4)
comp_matrix[9,1] = round(lm(SizeETF$SMB ~ SizeETF$Mkt.RF)[[1]][2],4)
comp_matrix[1,2] = round(mean(ValueETF$ExcessRet)*100,4)
comp_matrix[2,2] = round(mean(ValueETF$HML)*100,4)
comp_matrix[3,2] = round(sd(ValueETF$ExcessRet)*100,4)
comp_matrix[4,2] = round(sd(ValueETF$HML)*100,4)
comp_matrix[5,2]=round(cor(ValueETF$HML,ValueETF$ExcessRet),4)
comp_matrix[6,2] = round(lm(ValueETF$ExcessRet ~ ValueETF$Mkt.RF)[[1]][1]*100,4)
comp_matrix[7,2] = round(lm(ValueETF$HML ~ ValueETF$Mkt.RF)[[1]][1]*100,4)
comp_matrix[8,2] = round(lm(ValueETF$ExcessRet ~ ValueETF$Mkt.RF)[[1]][2],4)
comp_matrix[9,2] = round(lm(ValueETF$HML ~ ValueETF$Mkt.RF)[[1]][2],4)
comp_matrix[1,3] = round(mean(MomentumETF$ExcessRet)*100,4)
comp_matrix[2,3] = round(mean(MomentumETF$Momentum)*100,4)
comp_matrix[3,3] = round(sd(MomentumETF$ExcessRet)*100,4)
comp_matrix[4,3] = round(sd(MomentumETF$Momentum)*100,4)
comp_matrix[5,3]=round(cor(MomentumETF$Momentum,MomentumETF$ExcessRet),4)
comp_matrix[6,3] = round(lm(MomentumETF$ExcessRet ~ MomentumETF$Mkt.RF)[[1]][1]*100,4)
comp_matrix[7,3] = round(lm(MomentumETF$Momentum ~ MomentumETF$Mkt.RF)[[1]][1]*100,4)
comp_matrix[8,3] = round(lm(MomentumETF$ExcessRet ~ MomentumETF$Mkt.RF)[[1]][2],4)
comp_matrix[9,3] = round(lm(MomentumETF$Momentum ~ MomentumETF$Mkt.RF)[[1]][2],4)
comp_matrix[1,4] = round(mean(QualityETF$ExcessRet)*100,4)
comp_matrix[2,4] = round(mean(QualityETF$Quality)*100,4)
comp_matrix[3,4] = round(sd(QualityETF$ExcessRet)*100,4)
comp_matrix[4,4] = round(sd(QualityETF$Quality)*100,4)
comp_matrix[5,4]=round(cor(QualityETF$Quality,QualityETF$ExcessRet),4)
comp_matrix[6,4] = round(lm(QualityETF$ExcessRet ~ QualityETF$Mkt.RF)[[1]][1]*100,4)
comp_matrix[7,4] = round(lm(QualityETF$Quality ~ QualityETF$Mkt.RF)[[1]][1]*100,4)
comp_matrix[8,4] = round(lm(QualityETF$ExcessRet ~ QualityETF$Mkt.RF)[[1]][2],4)
comp_matrix[9,4] = round(lm(QualityETF$Quality ~ QualityETF$Mkt.RF)[[1]][2],4)
comp_matrix[1,5] = round(mean(LowBetaETF$ExcessRet)*100,4)
comp_matrix[2,5] = round(mean(LowBetaETF$BAB)*100,4)
comp_matrix[3,5] = round(sd(LowBetaETF$ExcessRet)*100,4)
comp_matrix[4,5] = round(sd(LowBetaETF$BAB)*100,4)
comp_matrix[5,5]=round(cor(LowBetaETF$BAB,LowBetaETF$ExcessRet),4)
comp_matrix[6,5] = round(lm(LowBetaETF$ExcessRet ~ LowBetaETF$Mkt.RF)[[1]][1]*100,4)
comp_matrix[7,5] = round(lm(LowBetaETF$BAB ~ LowBetaETF$Mkt.RF)[[1]][1]*100,4)
comp_matrix[8,5] = round(lm(LowBetaETF$ExcessRet ~ LowBetaETF$Mkt.RF)[[1]][2],4)
comp_matrix[9,5] = round(lm(LowBetaETF$BAB ~ LowBetaETF$Mkt.RF)[[1]][2],4)
comp_matrix[1,6] = round(mean(MarketETF$ExcessRet)*100,4)
comp_matrix[2,6] = round(mean(MarketETF$Mkt.RF)*100,4)
comp_matrix[3,6] = round(sd(MarketETF$ExcessRet)*100,4)
comp_matrix[4,6] = round(sd(MarketETF$Mkt.RF)*100,4)
comp_matrix[5,6]=round(cor(MarketETF$Mkt.RF,MarketETF$ExcessRet),4)
comp_matrix[6,6] = round(lm(MarketETF$ExcessRet ~ MarketETF$Mkt.RF)[[1]][1]*100,4)
comp_matrix[7,6] = 0
comp_matrix[8,6] = round(lm(MarketETF$ExcessRet ~ MarketETF$Mkt.RF)[[1]][2],4)
comp_matrix[9,6] = 1
rownames(comp_matrix) = c('Mean Daily ETF Ret (%)', 'Mean Daily Factor Ret (%)', 'Daily Std. Dev ETF (%)', 'Daily Std. Dev Factor (%)', 'Correlation - Factor/ETF', 'CAPM Daily Alpha ETF (%)', 'CAPM Daily Alpha Factor (%)', 'CAPM Beta ETF', 'CAPM Beta Factor')
colnames(comp_matrix) = c('Size', 'Value', 'Momentum', 'Quality', 'BAB', 'Market')
print(xtable(comp_matrix, caption = 'Comparison of ETFs with Factors', digits = 3))

### 4.6 Predicting Returns and Volatility via GARCH ###

# Check for stationarity
df_table = matrix(nrow=8, ncol = 1)
library(tseries)
df_table[1,1] = adf.test(daily$Mkt.RF)[[1]]
df_table[2,1]=adf.test(daily$SMB)[[1]]
df_table[3,1]=adf.test(daily$HML)[[1]]
df_table[4,1]=adf.test(daily$RMW)[[1]]
df_table[5,1]=adf.test(daily$CMA)[[1]]
df_table[6,1]=adf.test(daily$Momentum)[[1]]
df_table[7,1]=adf.test(daily$Quality)[[1]]
df_table[8,1]=adf.test(daily$BAB)[[1]]
df_table = round(df_table,4)
colnames(df_table) = 'Dickey-Fuller Statistic'
rownames(df_table) = c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')

print(xtable(df_table, caption = 'Dickey-Fuller Tests for Stationarity', digits = 3))
# Specify the models

model1=ugarchspec(mean.model = list(armaOrder=c(2,0)), variance.model = list(garchOrder=c(1,1), model = 'gjrGARCH'), distribution.model = 'norm') # Univariate GARCH specification for the DCC-GARCH

gspec = gogarchspec(mean.model = list(model = c('AR'), lag = 1), variance.model = list(model='gjrGARCH', garchOrder = c(1,1)),
                    distribution.model = c('mvnorm')) # Specification for the GOGARCH-fit
multispec = dccspec(uspec = multispec(replicate(8, model1)), dccOrder = c(1,1), distribution = 'mvnorm') # DCC Specification

# Daily GO-GARCH

gogarch = gogarchroll(spec = gspec, data = daily[2:9]%>%
                          as.matrix,
                              n.ahead = 1,
                              forecast.length = 13703, 
                              refit.every = 1, 
                              refit.window = 'moving', 
                              solver = "hybrid") # Estimation of the rolling GOGARCH(1,1) 
# This estimation takes the better part of 17hrs(on my ThinkPad with 16gb ram), wherefore running it and coming to the GARCH-portfolios requires a bit of patience :)

# Calculate the daily estimates of the covariance and means for the daily GO-GARCH
cov_gogarch = rcov(gogarch) # This takes the better part of an hour to run.
mu_gogarch = fitted(gogarch)


# Calculate optimal weights for the daily GO-GARCH
SharpeFunction = function(var_weights, Sigmas, mus){
  weight = append(1,var_weights)
  Mu = mus%*%weight
  Var = weight%*%Sigmas%*%weight
  Sharpe = Mu/sqrt(Var)
  return(Sharpe)
}

garch_weights = matrix(nrow=NROW(mu_gogarch), ncol = 8)
garch_weights[,1] = 1
for (i in 1:NROW(mu_gogarch)){
  garch_weights[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = as.matrix(cov_gogarch[,,i]), mus =as.matrix(mu_gogarch[i,]), control = list(fnscale=-1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}

# Create a dataframe for the daily GARCH optimal weights
garch_weights = as.data.frame(garch_weights)
colnames(garch_weights) = c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
garch_weights$date = daily$date[1256:NROW(daily)]
garch_table = merge(daily, garch_weights, by = 'date')
garch_table$pfret = rowSums(garch_table[2:9]*garch_table[13:20])
garch_table$pfret_full = garch_table$pfret+garch_table$RF

# Compare the estimated vol, corr and mean on the daily GARCH with the actual estimates.
## Volatilities
vol_table = matrix(nrow = NROW(mu_gogarch), ncol = 8)
for (i in 1: NROW(mu_gogarch)){
  vol_table[i,] = diag(cov_gogarch[,,i])
}

colnames(vol_table)= c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
vol_table = as.data.frame(vol_table)
dates = daily$date[1256:NROW(daily)]

plot(dates,abs(garch_table$Mkt.RF), type ='l', col = 'red', xlab = 'Date', ylab = 'Daily SD/Abs(ret)', main = 'Abs(Ret) vs. GARCH Estimated Daily SD of the Market')
lines(dates, sqrt(vol_table$Market), type = 'l')
legend('topright', c('Abs Return', 'GARCH Fit'), col = c('red', 'black'), lty = 1)
## Means

plot(dates, garch_table$Mkt.RF, col = 'red', type = 'l', main = 'GO-GARCH(1,1) Means og Daily Returns vs. Actual Daily Returns for the Market Return', ylab = 'Daily Returns', xlab = 'Date')
lines(dates, mu_gogarch$Mkt.RF, col = 'black', type = 'l')
legend('topright', c('Actual Return', 'GARCH Mean'), lty = 1, col = c('red', 'black'))

## Correlations
cor_gogarch = rcor(gogarch)
cor_table = matrix(nrow = NROW(mu_gogarch), ncol = 1)
for (i in 1:NROW(mu_gogarch)){
  cor_table[i]=cor_gogarch[1,3,i]
}
cor_table = as.data.frame(cor_table)
rownames(cor_table) = dates
### Estimate a rolling correlation on the actual returns, backwards looking 200 days
roll_correlation <- function(x) cor(x[, 1], x[, 3])
rolling_correlation <- rollapply(garch_table[,2:9], 200, roll_correlation, by.column = FALSE)


plot(dates, cor_table[,1], type = 'l', col = 'black', main = 'Rolling 200 day Corr. on Market/Value vs. GARCH Corr. on Market/Value', ylab = 'Correlation')
lines(dates[200:NROW(dates)], rolling_correlation, col = 'red')
legend('topleft', c('GO-GARCH(1,1) Correlation', 'Actual 200-day Correlation'), col = c('Black', 'Red'), lty = 1)

# Get the overall parameters for the GO-GARCH on daily data.

multifit = gogarchfit(daily[2:9]%>%
                        as.matrix, spec = gspec, solver = 'hybrid')

coef_matrix = round(coef(multifit),4)
colnames(coef_matrix)=c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
rownames(coef_matrix) = c('Omega', 'Alpha', 'Beta', 'Gamma')

print(xtable(coef_matrix, caption = 'Estimation Parameters, Daily GO-GARCH(1,1)', digits = 3))

# Estimate monthly rolling and expanding GO-GARCH and the monthly DCC-model:
garchmonthly = gogarchroll(spec = gspec, data = monthly[2:9]%>%
                        as.matrix,
                        n.ahead = 1,
                        forecast.length = 415, 
                        refit.every = 1, 
                        refit.window = 'moving', 
                        solver = "hybrid") # This takes 2 hrs to run


garchmonthly_expanding = gogarchroll(spec = gspec, data = monthly[2:9]%>%
                             as.matrix,
                           n.ahead = 1,
                           forecast.length = 415, 
                           refit.every = 1, 
                           refit.window = 'recursive', 
                           solver = "hybrid") # This takes another 2 hrs

## DCC Monthly Test

dccmonthly = dccroll(spec = multispec, data = monthly[2:9],
                     n.ahead = 1,
                     forecast.length = 415,
                     refit.every = 1,
                     refit.window = 'moving') # And this takes another 2 hrs



# Get the means and covariances from the models
## DCC.model
cov_monthly_DCC = rcov(dccmonthly, output = 'array')
mu_monthly_DCC = as.data.frame(fitted(dccmonthly))
rownames(mu_monthly_DCC)=monthly$month[301:NROW(monthly)]
## Monthly rolling GO-GARCH
cov_monthly = rcov(garchmonthly, output = 'array')
cov_monthly[,,14]=cov_monthly[,,13] # The estimate for the 14th month does not converge. Hence, to avoid missing values, I assume that it is equal to the 13th month.
mu_monthly = as.data.frame(fitted(garchmonthly))
rownames(mu_monthly)=monthly$month[301:NROW(monthly)]
## Monthly Expanding GO-GARCH
cov_monthly_expanding = rcov(garchmonthly_expanding, output = 'array')
cov_monthly_expanding[,,5]=cov_monthly[,,4] # The estimate for the 5th month does not converge. Hence, to avoid missing values, I assume that it is equal to the 4th month.
mu_monthly_expanding = as.data.frame(fitted(garchmonthly_expanding))
rownames(mu_monthly_expanding) = monthly$month[301:NROW(monthly)]

# Optimize the weights of the models based on the predictions of means and covariances
## Monthly Rolling GO-GARCH
garch_weights_monthly = matrix(nrow=NROW(mu_monthly), ncol = 8)
garch_weights_monthly[,1] = 1
for (i in 1:NROW(mu_monthly)){
  garch_weights_monthly[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = as.matrix(cov_monthly[,,i]), mus =as.matrix(mu_monthly[i,]), control = list(fnscale=-1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}

## Monthly Expanding GO-GARCH
garch_weights_monthly_expanding = matrix(nrow=NROW(mu_monthly_expanding), ncol = 8)
garch_weights_monthly_expanding[,1] = 1
for (i in 1:NROW(mu_monthly_expanding)){
  garch_weights_monthly_expanding[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = as.matrix(cov_monthly_expanding[,,i]), mus =as.matrix(mu_monthly_expanding[i,]), control = list(fnscale=-1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}


## Estimate means via an AR(2) model with rolling 30-day window
ar_frame = matrix(ncol = 8, nrow = NROW(mu_monthly))
for (i in 280:694){
  end = i+20
  for (k in 2:9){
    ar_frame[i-279,k-1]=predict(arima(monthly[i:end,k], order = c(2,0,0), method = 'ML'), n.ahead = 1)[[1]][1]
    
  }
}

## Get optimal weights for the monthly rolling GO-GARCH with AR(2) 30-day rolling mean
garch_weights_monthly_ar = matrix(nrow=NROW(ar_frame), ncol = 8)
garch_weights_monthly_ar[,1] = 1
for (i in 1:NROW(ar_frame)){
  garch_weights_monthly_ar[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = as.matrix(cov_monthly[,,i]), mus =ar_frame[i,], control = list(fnscale=-1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}


## Monthly DCC
garch_weights_monthly_DCC = matrix(nrow=NROW(mu_monthly_DCC), ncol = 8)
garch_weights_monthly_DCC[,1] = 1
for (i in 1:NROW(mu_monthly_DCC)){
  garch_weights_monthly_DCC[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = as.matrix(cov_monthly_DCC[,,i]), mus =as.matrix(mu_monthly_DCC[i,]), control = list(fnscale=-1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}

## Get optimal weights for the regression-estimated means and monthly DCC as variance-covariance estimate.
garch_weights_monthly_DCC_reg = matrix(nrow=NROW(return_predictions), ncol = 8)
garch_weights_monthly_DCC_reg[,1] = 1
for (i in 1:402){
  garch_weights_monthly_DCC_reg[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = as.matrix(cov_monthly_DCC[,,i]), mus =t(as.matrix(as.numeric(return_predictions[i,1:8]))), control = list(fnscale=-1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}

garch_weights_monthly_DCC_reg_275 = matrix(nrow=NROW(return_predictions_275), ncol = 8)
garch_weights_monthly_DCC_reg_275[,1] = 1
for (i in 1:402){
  garch_weights_monthly_DCC_reg_275[i,2:8]=optim(par = var_weights, fn = SharpeFunction, Sigmas = as.matrix(cov_monthly_DCC[,,i]), mus =t(as.matrix(as.numeric(return_predictions_275[i,1:8]))), control = list(fnscale=-1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}

# Throw the weights into a dataframe with the daily returns to calculate performance of the portfolio
# Further, throw the volatilities into a dataframe.
## DCC GARCH
garch_weights_monthly_DCC = as.data.frame(garch_weights_monthly_DCC)
colnames(garch_weights_monthly_DCC) = c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
garch_weights_monthly_DCC$month = as.yearmon(monthly$month[301:NROW(monthly)])
garch_table_monthly_DCC = merge(daily, garch_weights_monthly_DCC, by = 'month')
garch_table_monthly_DCC$pfret = rowSums(garch_table_monthly_DCC[3:10]*garch_table_monthly_DCC[13:20])
garch_table_monthly_DCC$pfret_full = garch_table_monthly_DCC$pfret+garch_table_monthly_DCC$RF
garch_table_monthly_DCC = garch_table_monthly_DCC[garch_table_monthly_DCC$date<as.Date('2022-12-01'),]

vol_table_monthly_DCC = matrix(nrow = NROW(mu_monthly_DCC), ncol = 8)
for (i in 1: NROW(mu_monthly_DCC)){
  vol_table_monthly_DCC[i,] = diag(cov_monthly_DCC[,,i])
}

colnames(vol_table_monthly_DCC)= c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
vol_table_monthly_DCC = as.data.frame(vol_table_monthly_DCC)
vol_table_monthly_DCC = vol_table_monthly_DCC[1:413,]

cor_monthly_dcc = rcor(dccmonthly)
cor_table_monthly_dcc = matrix(nrow = NROW(mu_monthly), ncol = 1)
for (i in 1:NROW(mu_monthly)){
  cor_table_monthly_dcc[i]=cor_monthly_dcc[1,3,i]
}

cor_table_monthly_dcc = cor_table_monthly_dcc[1:413,]

## DCC with regression means

garch_weights_monthly_DCC_reg = as.data.frame(garch_weights_monthly_DCC_reg)
colnames(garch_weights_monthly_DCC_reg) = c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
garch_weights_monthly_DCC_reg$month = as.yearmon(return_predictions$month)
garch_table_monthly_DCC_reg = merge(daily, garch_weights_monthly_DCC_reg, by = 'month')
garch_table_monthly_DCC_reg$pfret = rowSums(garch_table_monthly_DCC_reg[3:10]*garch_table_monthly_DCC_reg[13:20])
garch_table_monthly_DCC_reg$pfret_full = garch_table_monthly_DCC_reg$pfret+garch_table_monthly_DCC_reg$RF
garch_table_monthly_DCC_reg = garch_table_monthly_DCC_reg[garch_table_monthly_DCC_reg$date<as.Date('2022-12-01'),]


garch_weights_monthly_DCC_reg_275 = as.data.frame(garch_weights_monthly_DCC_reg_275)
colnames(garch_weights_monthly_DCC_reg_275) = c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
garch_weights_monthly_DCC_reg_275$month = as.yearmon(return_predictions$month)
garch_table_monthly_DCC_reg_275 = merge(daily, garch_weights_monthly_DCC_reg_275, by = 'month')
garch_table_monthly_DCC_reg_275$pfret = rowSums(garch_table_monthly_DCC_reg_275[3:10]*garch_table_monthly_DCC_reg_275[13:20])
garch_table_monthly_DCC_reg_275$pfret_full = garch_table_monthly_DCC_reg_275$pfret+garch_table_monthly_DCC_reg_275$RF
garch_table_monthly_DCC_reg_275 = garch_table_monthly_DCC_reg_275[garch_table_monthly_DCC_reg_275$date<as.Date('2022-12-01'),]

## ROlling GO-GARCH with AR specified mean

garch_weights_monthly_ar = as.data.frame(garch_weights_monthly_ar )
colnames(garch_weights_monthly_ar) = c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
garch_weights_monthly_ar $month = as.yearmon(monthly$month[301:NROW(monthly)])
garch_table_monthly_ar = merge(daily, garch_weights_monthly_ar , by = 'month')
garch_table_monthly_ar$pfret = rowSums(garch_table_monthly_ar[3:10]*garch_table_monthly_ar[13:20])
garch_table_monthly_ar$pfret_full = garch_table_monthly_ar$pfret+garch_table_monthly_ar$RF
garch_table_monthly_ar = garch_table_monthly_ar[garch_table_monthly_ar$date<as.Date('2022-12-01'),]
## Rolling GO-GARCH with GARCH-specified mean returns

garch_weights_monthly = as.data.frame(garch_weights_monthly)
colnames(garch_weights_monthly) = c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
garch_weights_monthly$month = as.yearmon(monthly$month[301:NROW(monthly)])
garch_table_monthly = merge(daily, garch_weights_monthly, by = 'month')
garch_table_monthly$pfret = rowSums(garch_table_monthly[3:10]*garch_table_monthly[13:20])
garch_table_monthly$pfret_full = garch_table_monthly$pfret+garch_table_monthly$RF
garch_table_monthly = garch_table_monthly[garch_table_monthly$date<as.Date('2022-12-01'),]

vol_table_monthly = matrix(nrow = NROW(mu_monthly), ncol = 8)
for (i in 1: NROW(mu_monthly)){
  vol_table_monthly[i,] = diag(cov_monthly[,,i])
}

colnames(vol_table_monthly)= c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
vol_table_monthly = as.data.frame(vol_table_monthly)
vol_table_monthly = vol_table_monthly[1:413,]

cor_monthly_rolling = rcor(garchmonthly)
cor_table_monthly = matrix(nrow = NROW(mu_monthly), ncol = 1)
for (i in 1:NROW(mu_monthly)){
  cor_table_monthly[i]=cor_monthly_rolling[1,3,i]
}
cor_table_monthly = cor_table_monthly[1:413,]

## Expanding GO-GARCH with GARCH-specified mean returns

garch_weights_monthly_expanding = as.data.frame(garch_weights_monthly_expanding)
colnames(garch_weights_monthly_expanding) = c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
garch_weights_monthly_expanding$month = as.yearmon(monthly$month[301:NROW(monthly)])
garch_table_monthly_expanding = merge(daily, garch_weights_monthly_expanding, by = 'month')
garch_table_monthly_expanding$pfret = rowSums(garch_table_monthly_expanding[3:10]*garch_table_monthly_expanding[13:20])
garch_table_monthly_expanding$pfret_full = garch_table_monthly_expanding$pfret+garch_table_monthly_expanding$RF
garch_table_monthly_expanding = garch_table_monthly_expanding[garch_table_monthly_expanding$date<as.Date('2022-12-01'),]

vol_table_monthly_expanding = matrix(nrow = NROW(mu_monthly_expanding), ncol = 8)
for (i in 1: NROW(mu_monthly_expanding)){
  vol_table_monthly_expanding[i,] = diag(cov_monthly_expanding[,,i])
}

colnames(vol_table_monthly_expanding)= c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
vol_table_monthly_expanding = as.data.frame(vol_table_monthly_expanding)
vol_table_monthly_expanding = vol_table_monthly_expanding[1:413,]
months = monthly$month[301:713]

cor_monthly_expanding = rcor(garchmonthly_expanding)
cor_table_monthly_expanding = matrix(nrow = NROW(mu_monthly), ncol = 1)
for (i in 1:NROW(mu_monthly)){
  cor_table_monthly_expanding[i]=cor_monthly_expanding[1,3,i]
}
cor_table_monthly_expanding = cor_table_monthly_expanding[1:413]


# Compare the fit of the estimated monthly models in plots

## Variances

plot(months,abs(monthly$Mkt.RF)[301:713], type ='l', col = 'red', xlab = 'Date', ylab = 'Monthly SD/Abs(ret)', main = 'Abs(Ret) of the Market vs. GARCH Volatilities')
lines(months, sqrt(vol_table_monthly$Market), type = 'l', col = 'black')
lines(months, sqrt(vol_table_monthly_DCC$Market), type = 'l', col = 'blue')
lines(months, sqrt(vol_table_monthly_expanding$Market), type = 'l', col = 'grey')
legend('topright', c('Abs Return', 'GO-GARCH Rolling Fit', 'DCC GARCH', 'GO-GARCH Expanding Fit'), col = c('red', 'black', 'blue', 'grey'), lty = 1)

##Means

plot(months, monthly$Mkt.RF[301:713], col = 'red', type = 'l', main = 'GARCH Means vs. Actual Means for the Market Return', ylab = 'Monthly Return')
lines(months, mu_monthly$Mkt.RF[1:413], col = 'black', type = 'l')
lines(months, ar_frame[1:413,1], col = 'green', type = 'l')
lines(months, mu_monthly_DCC$Mkt.RF[1:413], col = 'blue', type = 'l')
lines(months, mu_monthly_expanding$Mkt.RF[1:413], col = 'grey', type = 'l')
lines(return_predictions_300$month, return_predictions_300$Mkt.RF, col = 'plum', type = 'l')
legend('bottomleft', c('Actual Return', 'GO-GARCH Rolling Mean', 'GO-GARCH Expanding Mean', 'AR2 Mean Rolling', 'DCC Mean', 'Reg. Estimates'), lty = 1, col = c('red', 'black', 'grey', 'green', 'blue', 'plum'))

## Correlations

roll_correlation <- function(x) cor(x[, 1], x[, 3])
rolling_correlation <- rollapply(monthly[,2:9], 60, roll_correlation, by.column = FALSE)
rolling_correlation = as.data.frame(rolling_correlation)
rolling_correlation$month = monthly$month[60:NROW(monthly)]
rolling_correlation = rolling_correlation[242:654,]

plot(months, cor_table_monthly, col = 'red', type = 'l', main = 'Rolling 60 month Corr. on Market/Value vs. GARCH Corr. on Market/Value', ylab = 'Correlation', xlab = 'Month')
lines(months, cor_table_monthly_expanding, col = 'grey')
lines(months, cor_table_monthly_dcc, col = 'blue')
lines(months, rolling_correlation$rolling_correlation, col = 'black')
legend('topleft', c('Rolling GO-GARCH', 'Actual 60-month Correlation', 'Expanding GO-GARCH', 'DCC GARCH'), col = c('Red', 'Black', 'grey', 'blue'), lty = 1)

cor_table_monthly_expanding
# Calculate the performance excluding transaction costs

ind_matrix = matrix(nrow = 8, ncol = 6)
ind_matrix[1,1] = round((tail(cumprod(1+garch_table$pfret_full), 1)^(1/n_distinct(garch_table$month))-1)*100,4)
ind_matrix[1,2] = round((tail(cumprod(1+garch_table_monthly$pfret_full),1)^(1/NROW(garch_table_monthly))-1)*100,4)
ind_matrix[1,3] = round((tail(cumprod(1+garch_table_monthly_ar$pfret_full),1)^(1/NROW(garch_table_monthly))-1)*100,4)
ind_matrix[1,4] = round((tail(cumprod(1+garch_table_monthly_expanding$pfret_full),1)^(1/NROW(garch_table_monthly))-1)*100,4)
ind_matrix[1,5] = round((tail(cumprod(1+garch_table_monthly_DCC$pfret_full),1)^(1/NROW(garch_table_monthly))-1)*100,4)
ind_matrix[1,6] = round((tail(cumprod(1+garch_table_monthly_DCC_reg_275$pfret_full),1)^(1/NROW(garch_table_monthly_DCC_reg_275))-1)*100,4)
ind_matrix[2,1] = round(mean(garch_table$pfret)*100,4)
ind_matrix[2,2] = round(mean(garch_table_monthly$pfret)*100,4)
ind_matrix[2,3] = round(mean(garch_table_monthly_ar$pfret)*100,4)
ind_matrix[2,4] = round(mean(garch_table_monthly_expanding$pfret)*100,4)
ind_matrix[2,5] = round(mean(garch_table_monthly_DCC$pfret)*100,4)
ind_matrix[2,6] = round(mean(garch_table_monthly_DCC_reg_275$pfret)*100,4)
ind_matrix[3,1] = round(sd(garch_table$pfret)*100,4)
ind_matrix[3,2] = round(sd(garch_table_monthly$pfret)*100,4)
ind_matrix[3,3] = round(sd(garch_table_monthly_ar$pfret)*100,4)
ind_matrix[3,4] = round(sd(garch_table_monthly_expanding$pfret)*100,4)
ind_matrix[3,5] = round(sd(garch_table_monthly_DCC$pfret)*100,4)
ind_matrix[3,6] = round(sd(garch_table_monthly_DCC_reg_275$pfret)*100,4)
ind_matrix[4,1] = round(mean(garch_table$pfret)/sd(garch_table$pfret),4)
ind_matrix[4,2] = round(mean(garch_table_monthly$pfret)/sd(garch_table_monthly$pfret),4)
ind_matrix[4,3] = round(mean(garch_table_monthly_ar$pfret)/sd(garch_table_monthly_ar$pfret),4)
ind_matrix[4,4] = round(mean(garch_table_monthly_expanding$pfret)/sd(garch_table_monthly_expanding$pfret),4)
ind_matrix[4,5] = round(mean(garch_table_monthly_DCC$pfret)/sd(garch_table_monthly_DCC$pfret),4)
ind_matrix[4,6] = round(mean(garch_table_monthly_DCC_reg_275$pfret)/sd(garch_table_monthly_DCC_reg_275$pfret),4)
ind_matrix[5,1] = round(min((cumprod(1+garch_table$pfret)-cummax(cumprod(1+garch_table$pfret)))/cummax(cumprod(1+garch_table$pfret)))*100,4)
ind_matrix[5,2] = round(min((cumprod(1+garch_table_monthly$pfret_full)-cummax(cumprod(1+garch_table_monthly$pfret_full)))/cummax(cumprod(1+garch_table_monthly$pfret_full)))*100,4)
ind_matrix[5,3] = round(min((cumprod(1+garch_table_monthly_ar$pfret_full)-cummax(cumprod(1+garch_table_monthly_ar$pfret_full)))/cummax(cumprod(1+garch_table_monthly_ar$pfret_full)))*100,4)
ind_matrix[5,4] = round(min((cumprod(1+garch_table_monthly_expanding$pfret_full)-cummax(cumprod(1+garch_table_monthly_expanding$pfret_full)))/cummax(cumprod(1+garch_table_monthly_expanding$pfret_full)))*100,4)
ind_matrix[5,5] = round(min((cumprod(1+garch_table_monthly_DCC$pfret_full)-cummax(cumprod(1+garch_table_monthly_DCC$pfret_full)))/cummax(cumprod(1+garch_table_monthly_DCC$pfret_full)))*100,4)
ind_matrix[5,6] = round(min((cumprod(1+garch_table_monthly_DCC_reg_275$pfret_full)-cummax(cumprod(1+garch_table_monthly_DCC_reg_275$pfret_full)))/cummax(cumprod(1+garch_table_monthly_DCC_reg_275$pfret_full)))*100,4)
ind_matrix[6,1] = round(lm(garch_table$pfret ~ garch_table$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(garch_table$pfret~ garch_table$Mkt.RF)[[1]][2],4)
ind_matrix[8,1] = round(lm(garch_table$pfret ~ garch_table$Mkt.RF)[[1]][1]/(sd(garch_table$pfret)-sd(garch_table$Mkt.RF)*lm(garch_table$pfret ~ garch_table$Mkt.RF)[[1]][2]),4)
ind_matrix[6,2] = round(lm(garch_table_monthly$pfret ~ garch_table_monthly$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(garch_table_monthly$pfret~ garch_table_monthly$Mkt.RF)[[1]][2],4)
ind_matrix[8,2] = round(lm(garch_table_monthly$pfret ~ garch_table_monthly$Mkt.RF)[[1]][1]/(sd(garch_table_monthly$pfret)-sd(garch_table_monthly$Mkt.RF)*lm(garch_table_monthly$pfret ~ garch_table_monthly$Mkt.RF)[[1]][2]),4)
ind_matrix[6,3] = round(lm(garch_table_monthly_ar$pfret ~ garch_table_monthly$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,3] = round(lm(garch_table_monthly_ar$pfret~ garch_table_monthly$Mkt.RF)[[1]][2],4)
ind_matrix[8,3] = round(lm(garch_table_monthly_ar$pfret ~ garch_table_monthly$Mkt.RF)[[1]][1]/(sd(garch_table_monthly_ar$pfret)-sd(garch_table_monthly$Mkt.RF)*lm(garch_table_monthly_ar$pfret ~ garch_table_monthly$Mkt.RF)[[1]][2]),4)
ind_matrix[6,4] = round(lm(garch_table_monthly_expanding$pfret ~ garch_table_monthly$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,4] = round(lm(garch_table_monthly_expanding$pfret~ garch_table_monthly$Mkt.RF)[[1]][2],4)
ind_matrix[8,4] = round(lm(garch_table_monthly_expanding$pfret ~ garch_table_monthly$Mkt.RF)[[1]][1]/(sd(garch_table_monthly_expanding$pfret)-sd(garch_table_monthly$Mkt.RF)*lm(garch_table_monthly_expanding$pfret ~ garch_table_monthly$Mkt.RF)[[1]][2]),4)
ind_matrix[6,5] = round(lm(garch_table_monthly_DCC$pfret ~ garch_table_monthly$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,5] = round(lm(garch_table_monthly_DCC$pfret~ garch_table_monthly$Mkt.RF)[[1]][2],4)
ind_matrix[8,5] = round(lm(garch_table_monthly_DCC$pfret ~ garch_table_monthly$Mkt.RF)[[1]][1]/(sd(garch_table_monthly_DCC$pfret)-sd(garch_table_monthly$Mkt.RF)*lm(garch_table_monthly_DCC$pfret ~ garch_table_monthly$Mkt.RF)[[1]][2]),4)
ind_matrix[6,6] = round(lm(garch_table_monthly_DCC_reg_275$pfret ~ garch_table_monthly_DCC_reg_275$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,6] = round(lm(garch_table_monthly_DCC_reg_275$pfret~ garch_table_monthly_DCC_reg_275$Mkt.RF)[[1]][2],4)
ind_matrix[8,6] = round(lm(garch_table_monthly_DCC_reg_275$pfret ~ garch_table_monthly_DCC_reg_275$Mkt.RF)[[1]][1]/(sd(garch_table_monthly_DCC_reg_275$pfret)-sd(garch_table_monthly_DCC_reg_275$Mkt.RF)*lm(garch_table_monthly_DCC_reg_275$pfret ~ garch_table_monthly_DCC_reg_275$Mkt.RF)[[1]][2]),4)


ind_matrix=as.data.frame(ind_matrix)
rownames(ind_matrix) = c('Mean Monthly Ret (%)', 'Excess Ret (%)', 'Excess Std. Dev (%)', 'Sharpe', 'Drawdown (%)', 'CAPM Alpha (%)', 'CAPM Beta', 'Information Ratio')
colnames(ind_matrix) = c('Daily Rol.', 'Mon. Roll. GO-GARCH', 'Mon. Roll. AR2', 'Mon. Exp. GO-GARCH', 'Mon. Roll. DCC', 'Reg. Est. DCC')

print(xtable(ind_matrix, caption = 'Performance of Factor-tilt Portfolios estimated by GARCH', digits = 3))


# Calculate the performance when considering transaction costs
# Here, I only consider the second-pass transaction costs of 0.35bps daily/7 bps monthly shorting costs and a transaction cost of 20 bps per 100% the factor is flipped.
## Daily GO-GARCH
weight_chgs_daily = garch_table[13:20]*(1+garch_table[2:9])-dplyr::lag(garch_table[13:20])
weight_chgs_daily[1,] = garch_table[1,13:20]
trans_costs_daily = weight_chgs_daily*0.002
shorting_costs_daily <- rowSums(apply(garch_table[13:20], 2, function(x) ifelse(x < 0, abs(x)*0.000035, 0)))
garch_table_trans = garch_table[2:9]-trans_costs_daily-shorting_costs_daily
garch_table_trans$pfret = rowSums(garch_table_trans*garch_table[13:20])
garch_table_trans$pfret_full = garch_table_trans$pfret+garch_table$RF
## Monthly Rolling GO-GARCH
monthly_rets = apply.monthly(as.xts(garch_table_monthly[3:10], order.by = garch_table_monthly$date), Return.cumulative)
weight_chgs_monthly = as.data.frame(1+monthly_rets)*garch_weights_monthly[1:413,1:8]-dplyr::lag(garch_weights_monthly[1:413,1:8])
weight_chgs_monthly[1,] = garch_weights_monthly[1,1:8]
trans_costs_monthly = rowSums(abs(weight_chgs_monthly)*0.002)
shorting_costs_monthly = rowSums(apply(garch_weights_monthly[1:8], 2, function(x) ifelse(x < 0, abs(x)*0.0007, 0)))

first_dates_monthly_garch = garch_table_monthly[!duplicated(garch_table_monthly$month),][2]
garch_table_monthly_trans = garch_table_monthly
garch_table_monthly_trans[garch_table_monthly_trans$date %in% first_dates_monthly_garch$date,][21] = garch_table_monthly_trans[garch_table_monthly_trans$date %in% first_dates_monthly_garch$date,][21]-trans_costs_monthly-shorting_costs_monthly
garch_table_monthly_trans[garch_table_monthly_trans$date %in% first_dates_monthly_garch$date,][22] = garch_table_monthly_trans[garch_table_monthly_trans$date %in% first_dates_monthly_garch$date,][22]-trans_costs_monthly-shorting_costs_monthly

## Monthly Expanding GO-GARCH
weight_chgs_monthly_expanding = as.data.frame(1+monthly_rets)*garch_weights_monthly_expanding[1:413,1:8]-dplyr::lag(garch_weights_monthly_expanding[1:413,1:8])
weight_chgs_monthly_expanding[1,] = garch_weights_monthly_expanding[1,1:8]
trans_costs_monthly_expanding = rowSums(abs(weight_chgs_monthly_expanding)*0.002)
shorting_costs_monthly_expanding = rowSums(apply(garch_weights_monthly_expanding[1:8], 2, function(x) ifelse(x < 0, abs(x)*0.0007, 0)))

garch_table_monthly_trans_expanding = garch_table_monthly_expanding
garch_table_monthly_trans_expanding[garch_table_monthly_trans_expanding$date %in% first_dates_monthly_garch$date,][21] = garch_table_monthly_trans_expanding[garch_table_monthly_trans_expanding$date %in% first_dates_monthly_garch$date,][21]-trans_costs_monthly_expanding-shorting_costs_monthly_expanding
garch_table_monthly_trans_expanding[garch_table_monthly_trans_expanding$date %in% first_dates_monthly_garch$date,][22] = garch_table_monthly_trans_expanding[garch_table_monthly_trans_expanding$date %in% first_dates_monthly_garch$date,][22]-trans_costs_monthly_expanding-shorting_costs_monthly_expanding

## Monthly DCC
weight_chgs_monthly_DCC = as.data.frame(1+monthly_rets)*garch_weights_monthly_DCC[1:413,1:8]-dplyr::lag(garch_weights_monthly_DCC[1:413,1:8])
weight_chgs_monthly_DCC[1,] = garch_weights_monthly_DCC[1,1:8]
trans_costs_monthly_DCC = rowSums(abs(weight_chgs_monthly_DCC)*0.002)
shorting_costs_monthly_DCC = rowSums(apply(garch_weights_monthly_DCC[1:8], 2, function(x) ifelse(x < 0, abs(x)*0.0007, 0)))

garch_table_monthly_trans_DCC = garch_table_monthly_DCC
garch_table_monthly_trans_DCC[garch_table_monthly_trans_DCC$date %in% first_dates_monthly_garch$date,][21] = garch_table_monthly_trans_DCC[garch_table_monthly_trans_DCC$date %in% first_dates_monthly_garch$date,][21]-trans_costs_monthly_DCC-shorting_costs_monthly_DCC
garch_table_monthly_trans_DCC[garch_table_monthly_trans_DCC$date %in% first_dates_monthly_garch$date,][22] = garch_table_monthly_trans_DCC[garch_table_monthly_trans_DCC$date %in% first_dates_monthly_garch$date,][22]-trans_costs_monthly_DCC-shorting_costs_monthly_DCC

## Monthly DCC w. reg.
weight_chgs_monthly_DCC_reg_275 = as.data.frame(1+monthly_rets)*garch_weights_monthly_DCC_reg_275[1:413,1:8]-dplyr::lag(garch_weights_monthly_DCC_reg_275[1:413,1:8])
weight_chgs_monthly_DCC_reg_275[1,] = garch_weights_monthly_DCC_reg_275[1,1:8]
trans_costs_monthly_DCC_reg_275 = rowSums(abs(weight_chgs_monthly_DCC_reg_275)*0.002)
shorting_costs_monthly_DCC_reg_275 = rowSums(apply(garch_weights_monthly_DCC_reg_275[1:8], 2, function(x) ifelse(x < 0, abs(x)*0.0007, 0)))

garch_table_monthly_trans_DCC_reg_275 = garch_table_monthly_DCC_reg_275
garch_table_monthly_trans_DCC_reg_275[garch_table_monthly_trans_DCC_reg_275$date %in% first_dates_monthly_garch$date,][21] = garch_table_monthly_trans_DCC_reg_275[garch_table_monthly_trans_DCC_reg_275$date %in% first_dates_monthly_garch$date,][21]-trans_costs_monthly_DCC_reg_275-shorting_costs_monthly_DCC_reg_275
garch_table_monthly_trans_DCC_reg_275[garch_table_monthly_trans_DCC_reg_275$date %in% first_dates_monthly_garch$date,][22] = garch_table_monthly_trans_DCC_reg_275[garch_table_monthly_trans_DCC_reg_275$date %in% first_dates_monthly_garch$date,][22]-trans_costs_monthly_DCC_reg_275-shorting_costs_monthly_DCC_reg_275

## Monthly DCC w. reg.
weight_chgs_monthly_DCC_reg_84 = as.data.frame(1+monthly_rets)*garch_weights_monthly_DCC_reg[1:413,1:8]-dplyr::lag(garch_weights_monthly_DCC_reg[1:413,1:8])
weight_chgs_monthly_DCC_reg_84[1,] = garch_weights_monthly_DCC_reg[1,1:8]
trans_costs_monthly_DCC_reg_84 = rowSums(abs(weight_chgs_monthly_DCC_reg_84)*0.002)
shorting_costs_monthly_DCC_reg_84 = rowSums(apply(garch_weights_monthly_DCC_reg[1:8], 2, function(x) ifelse(x < 0, abs(x)*0.0007, 0)))

garch_table_monthly_trans_DCC_reg_84 = garch_table_monthly_DCC_reg
garch_table_monthly_trans_DCC_reg_84[garch_table_monthly_trans_DCC_reg_84$date %in% first_dates_monthly_garch$date,][21] = garch_table_monthly_trans_DCC_reg_84[garch_table_monthly_trans_DCC_reg_84$date %in% first_dates_monthly_garch$date,][21]-trans_costs_monthly_DCC_reg_84-shorting_costs_monthly_DCC_reg_84
garch_table_monthly_trans_DCC_reg_84[garch_table_monthly_trans_DCC_reg_84$date %in% first_dates_monthly_garch$date,][22] = garch_table_monthly_trans_DCC_reg_84[garch_table_monthly_trans_DCC_reg_84$date %in% first_dates_monthly_garch$date,][22]-trans_costs_monthly_DCC_reg_84-shorting_costs_monthly_DCC_reg_84

## Standard Monthly Backwards Looking Mean Variance
backwards_est = mkt_factor_rolling_capped2[mkt_factor_rolling_capped2$date>as.Date('1988-06-30'),]
weight_chgs_monthly = as.data.frame(1+monthly_rets)*market_weight_capped2[289:NROW(market_weight_capped2),1:8]-dplyr::lag(market_weight_capped2[289:NROW(market_weight_capped2),1:8])
weight_chgs_monthly[1,] = market_weight_capped2[289,1:8]
trans_costs_monthly = rowSums(abs(weight_chgs_monthly)*0.002)
shorting_costs_monthly = rowSums(apply(market_weight_capped2[289:690,1:8], 2, function(x) ifelse(x < 0, abs(x)*0.0007, 0)))
backwards_est[backwards_est$date %in% first_dates_monthly_garch$date,][21] = backwards_est[backwards_est$date %in% first_dates_monthly_garch$date,][21]-trans_costs_monthly-shorting_costs_monthly
backwards_est[backwards_est$date %in% first_dates_monthly_garch$date,][22] = backwards_est[backwards_est$date %in% first_dates_monthly_garch$date,][22]-trans_costs_monthly-shorting_costs_monthly

# Consider the performance in the well-known performance metric
ind_matrix = matrix(nrow = 8, ncol = 6)
rownames(ind_matrix) = stat_names
ind_matrix[1,1] = round((tail(cumprod(1+garch_table_trans$pfret_full), 1)^(1/n_distinct(garch_table_trans$month))-1)*100,4)
ind_matrix[1,2] = round((tail(cumprod(1+garch_table_monthly_trans$pfret_full),1)^(1/n_distinct(garch_table_monthly_trans$month))-1)*100,4)
ind_matrix[2,1] = round(mean(garch_table_trans$pfret)*100,4)
ind_matrix[2,2] = round(mean(garch_table_monthly_trans$pfret)*100,4)
ind_matrix[3,1] = round(sd(garch_table_trans$pfret)*100,4)
ind_matrix[3,2] = round(sd(garch_table_monthly_trans$pfret)*100,4)
ind_matrix[4,1] = round(mean(garch_table_trans$pfret)/sd(garch_table_trans$pfret),4)
ind_matrix[4,2] = round(mean(garch_table_monthly_trans$pfret)/sd(garch_table_monthly_trans$pfret),4)
ind_matrix[5,1] = round(min((cumprod(1+garch_table_trans$pfret_full)-cummax(cumprod(1+garch_table_trans$pfret_full)))/cummax(cumprod(1+garch_table_trans$pfret_full))),4)*100
ind_matrix[5,2] = round(min((cumprod(1+garch_table_monthly_trans$pfret)-cummax(cumprod(1+garch_table_monthly_trans$pfret)))/cummax(cumprod(1+garch_table_monthly_trans$pfret)))*100,4)
ind_matrix[1,6] = round((tail(cumprod(1+backwards_est$pfret_full), 1)^(1/n_distinct(garch_table_monthly_trans$month))-1)*100,4)
ind_matrix[2,6] = round(mean(backwards_est$pfret)*100,4)
ind_matrix[3,6] = round(sd(backwards_est$pfret)*100,4)
ind_matrix[4,6] = round(mean(backwards_est$pfret)/sd(backwards_est$pfret),4)
ind_matrix[5,6] = round(min((cumprod(1+backwards_est$pfret_full)-cummax(cumprod(1+backwards_est$pfret_full)))/cummax(cumprod(1+backwards_est$pfret_full)))*100,4)
ind_matrix[1,5] = round((tail(cumprod(1+garch_table_monthly_trans_DCC_reg$pfret_full), 1)^(1/n_distinct(garch_table_monthly_trans_DCC_reg$month))-1)*100,4)
ind_matrix[2,5] = round(mean(garch_table_monthly_trans_DCC_reg$pfret)*100,4)
ind_matrix[3,5] = round(sd(garch_table_monthly_trans_DCC_reg$pfret)*100,4)
ind_matrix[4,5] = round(mean(garch_table_monthly_trans_DCC_reg$pfret)/sd(garch_table_monthly_trans_DCC_reg$pfret),4)
ind_matrix[5,5] = round(min((cumprod(1+garch_table_monthly_trans_DCC_reg$pfret_full)-cummax(cumprod(1+garch_table_monthly_trans_DCC_reg$pfret_full)))/cummax(cumprod(1+garch_table_monthly_trans_DCC_reg$pfret_full)))*100,4)
ind_matrix[1,3] = round((tail(cumprod(1+garch_table_monthly_trans_expanding$pfret_full), 1)^(1/n_distinct(garch_table_monthly_trans$month))-1)*100,4)
ind_matrix[2,3] = round(mean(garch_table_monthly_trans_expanding$pfret)*100,4)
ind_matrix[3,3] = round(sd(garch_table_monthly_trans_expanding$pfret)*100,4)
ind_matrix[4,3] = round(mean(garch_table_monthly_trans_expanding$pfret)/sd(garch_table_monthly_trans_expanding$pfret),4)
ind_matrix[5,3] = round(min((cumprod(1+garch_table_monthly_trans_expanding$pfret_full)-cummax(cumprod(1+garch_table_monthly_trans_expanding$pfret_full)))/cummax(cumprod(1+garch_table_monthly_trans_expanding$pfret_full))),4)*100
ind_matrix[1,4] = round((tail(cumprod(1+garch_table_monthly_trans_DCC$pfret_full), 1)^(1/n_distinct(garch_table_monthly_trans$month))-1)*100,4)
ind_matrix[2,4] = round(mean(garch_table_monthly_trans_DCC$pfret)*100,4)
ind_matrix[3,4] = round(sd(garch_table_monthly_trans_DCC$pfret)*100,4)
ind_matrix[4,4] = round(mean(garch_table_monthly_trans_DCC$pfret)/sd(garch_table_monthly_trans_DCC$pfret),4)
ind_matrix[5,4] = round(min((cumprod(1+garch_table_monthly_trans_DCC$pfret_full)-cummax(cumprod(1+garch_table_monthly_trans_DCC$pfret_full)))/cummax(cumprod(1+garch_table_monthly_trans_DCC$pfret_full))),4)*100
ind_matrix[6,1] = round(lm(garch_table_trans$pfret ~ garch_table_trans$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(garch_table_trans$pfret~ garch_table_trans$Mkt.RF)[[1]][2],4)
ind_matrix[8,1] = round(lm(garch_table_trans$pfret ~ garch_table_trans$Mkt.RF)[[1]][1]/(sd(garch_table_trans$pfret)-sd(garch_table_trans$Mkt.RF)*lm(garch_table_trans$pfret ~ garch_table_trans$Mkt.RF)[[1]][2]),4)
ind_matrix[6,2] = round(lm(garch_table_monthly_trans$pfret ~ garch_table_monthly_trans$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(garch_table_monthly_trans$pfret~ garch_table_monthly_trans$Mkt.RF)[[1]][2],4)
ind_matrix[8,2] = round(lm(garch_table_monthly_trans$pfret ~ garch_table_monthly_trans$Mkt.RF)[[1]][1]/(sd(garch_table_monthly$pfret)-sd(garch_table_monthly_trans$Mkt.RF)*lm(garch_table_monthly_trans$pfret ~ garch_table_monthly_trans$Mkt.RF)[[1]][2]),4)
ind_matrix[6,3] = round(lm(garch_table_monthly_trans_expanding$pfret ~ garch_table_monthly_trans$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,3] = round(lm(garch_table_monthly_trans_expanding$pfret~ garch_table_monthly_trans$Mkt.RF)[[1]][2],4)
ind_matrix[8,3] = round(lm(garch_table_monthly_trans_expanding$pfret ~garch_table_monthly_trans$Mkt.RF)[[1]][1]/(sd(garch_table_monthly_trans_expanding$pfret)-sd(garch_table_monthly_trans$Mkt.RF)*lm(garch_table_monthly_trans_expanding$pfret ~ garch_table_monthly_trans$Mkt.RF)[[1]][2]),4)
ind_matrix[6,4] = round(lm(garch_table_monthly_trans_DCC$pfret ~ garch_table_monthly_trans$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,4] = round(lm(garch_table_monthly_trans_DCC$pfret~ garch_table_monthly_trans$Mkt.RF)[[1]][2],4)
ind_matrix[8,4] = round(lm(garch_table_monthly_trans_DCC$pfret ~ garch_table_monthly_trans$Mkt.RF)[[1]][1]/(sd(garch_table_monthly_trans_DCC$pfret)-sd(garch_table_monthly_trans$Mkt.RF)*lm(garch_table_monthly_trans_DCC$pfret ~ garch_table_monthly_trans$Mkt.RF)[[1]][2]),4)
ind_matrix[6,6] = round(lm(backwards_est$pfret ~ garch_table_monthly_trans$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,6] = round(lm(backwards_est$pfret~ garch_table_monthly_trans$Mkt.RF)[[1]][2],4)
ind_matrix[8,6] = round(lm(backwards_est$pfret ~ garch_table_monthly_trans$Mkt.RF)[[1]][1]/(sd(backwards_est$pfret)-sd(garch_table_monthly_trans_DCC_reg$Mkt.RF)*lm(garch_table_monthly_trans_DCC_reg$pfret ~ garch_table_monthly_trans_DCC_reg$Mkt.RF)[[1]][2]),4)
ind_matrix[6,5] = round(lm(garch_table_monthly_trans_DCC_reg$pfret ~ garch_table_monthly_trans_DCC_reg$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,5] = round(lm(garch_table_monthly_trans_DCC_reg$pfret~ garch_table_monthly_trans_DCC_reg$Mkt.RF)[[1]][2],4)
ind_matrix[8,5] = round(lm(garch_table_monthly_trans_DCC_reg$pfret ~ garch_table_monthly_trans_DCC_reg$Mkt.RF)[[1]][1]/(sd(garch_table_monthly_trans_DCC_reg$pfret)-sd(garch_table_monthly_trans_DCC_reg$Mkt.RF)*lm(garch_table_monthly_trans_DCC_reg$pfret ~ garch_table_monthly_trans_DCC_reg$Mkt.RF)[[1]][2]),4)


ind_matrix=as.data.frame(ind_matrix)
colnames(ind_matrix) = c('Daily GARCH', 'Mon. Roll. GO-GARCH', 'Mon. Exp. GO-GARCH', 'DCC GARCH', 'DCC Reg.', 'Backw. Mean-Var')

print(xtable(ind_matrix, caption = 'Performance of GARCH Portfolios after Transaction Costs', digits = 3 ))

## Table for comparing the different regressions
ind_matrix = matrix(nrow = 8, ncol = 4)
ind_matrix[1,1] = round((tail(cumprod(1+garch_table_monthly_DCC_reg$pfret_full), 1)^(1/n_distinct(garch_table_monthly_DCC_reg$month))-1)*100,4)
ind_matrix[2,1] = round(mean(garch_table_monthly_DCC_reg$pfret)*100,4)
ind_matrix[3,1] = round(sd(garch_table_monthly_DCC_reg$pfret)*100,4)
ind_matrix[4,1] = round(mean(garch_table_monthly_DCC_reg$pfret)/sd(garch_table_monthly_DCC_reg$pfret),4)
ind_matrix[5,1] = round(min((cumprod(1+garch_table_monthly_DCC_reg$pfret_full)-cummax(cumprod(1+garch_table_monthly_DCC_reg$pfret_full)))/cummax(cumprod(1+garch_table_monthly_DCC_reg$pfret_full))),4)*100
ind_matrix[6,1] = round(lm(garch_table_monthly_DCC_reg$pfret ~ garch_table_monthly_DCC_reg$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(garch_table_monthly_DCC_reg$pfret~ garch_table_monthly_DCC_reg$Mkt.RF)[[1]][2],4)
ind_matrix[8,1] = round(lm(garch_table_monthly_DCC_reg$pfret ~ garch_table_monthly_DCC_reg$Mkt.RF)[[1]][1]/(sd(garch_table_monthly_DCC_reg$pfret)-sd(garch_table_monthly_DCC_reg$Mkt.RF)*lm(garch_table_monthly_DCC_reg$pfret ~ garch_table_monthly_DCC_reg$Mkt.RF)[[1]][2]),4)
ind_matrix[1,2] = round((tail(cumprod(1+garch_table_monthly_DCC_reg_275$pfret_full), 1)^(1/n_distinct(garch_table_monthly_DCC_reg_275$month))-1)*100,4)
ind_matrix[2,2] = round(mean(garch_table_monthly_DCC_reg_275$pfret)*100,4)
ind_matrix[3,2] = round(sd(garch_table_monthly_DCC_reg_275$pfret)*100,4)
ind_matrix[4,2] = round(mean(garch_table_monthly_DCC_reg_275$pfret)/sd(garch_table_monthly_DCC_reg_275$pfret),4)
ind_matrix[5,2] = round(min((cumprod(1+garch_table_monthly_DCC_reg_275$pfret_full)-cummax(cumprod(1+garch_table_monthly_DCC_reg_275$pfret_full)))/cummax(cumprod(1+garch_table_monthly_DCC_reg_275$pfret_full))),4)*100
ind_matrix[6,2] = round(lm(garch_table_monthly_DCC_reg_275$pfret ~ garch_table_monthly_DCC_reg_275$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(garch_table_monthly_DCC_reg_275$pfret~ garch_table_monthly_DCC_reg_275$Mkt.RF)[[1]][2],4)
ind_matrix[8,2] = round(lm(garch_table_monthly_DCC_reg_275$pfret ~ garch_table_monthly_DCC_reg_275$Mkt.RF)[[1]][1]/(sd(garch_table_monthly_DCC_reg_275$pfret)-sd(garch_table_monthly_DCC_reg_275$Mkt.RF)*lm(garch_table_monthly_DCC_reg_275$pfret ~ garch_table_monthly_DCC_reg_275$Mkt.RF)[[1]][2]),4)
ind_matrix[1,3] = round((tail(cumprod(1+garch_table_monthly_trans_DCC_reg_84$pfret_full), 1)^(1/n_distinct(garch_table_monthly_trans_DCC_reg_84$month))-1)*100,4)
ind_matrix[2,3] = round(mean(garch_table_monthly_trans_DCC_reg_84$pfret)*100,4)
ind_matrix[3,3] = round(sd(garch_table_monthly_trans_DCC_reg_84$pfret)*100,4)
ind_matrix[4,3] = round(mean(garch_table_monthly_trans_DCC_reg_84$pfret)/sd(garch_table_monthly_trans_DCC_reg_84$pfret),4)
ind_matrix[5,3] = round(min((cumprod(1+garch_table_monthly_trans_DCC_reg_84$pfret_full)-cummax(cumprod(1+garch_table_monthly_trans_DCC_reg_84$pfret_full)))/cummax(cumprod(1+garch_table_monthly_trans_DCC_reg_84$pfret_full))),4)*100
ind_matrix[6,3] = round(lm(garch_table_monthly_trans_DCC_reg_84$pfret ~ garch_table_monthly_trans_DCC_reg_84$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,3] = round(lm(garch_table_monthly_trans_DCC_reg_84$pfret~ garch_table_monthly_trans_DCC_reg_84$Mkt.RF)[[1]][2],4)
ind_matrix[8,3] = round(lm(garch_table_monthly_trans_DCC_reg_84$pfret ~ garch_table_monthly_trans_DCC_reg_84$Mkt.RF)[[1]][1]/(sd(garch_table_monthly_trans_DCC_reg_84$pfret)-sd(garch_table_monthly_trans_DCC_reg_84$Mkt.RF)*lm(garch_table_monthly_trans_DCC_reg_84$pfret ~ garch_table_monthly_trans_DCC_reg_84$Mkt.RF)[[1]][2]),4)
ind_matrix[1,4] = round((tail(cumprod(1+garch_table_monthly_trans_DCC_reg$pfret_full), 1)^(1/n_distinct(garch_table_monthly_trans_DCC_reg$month))-1)*100,4)
ind_matrix[2,4] = round(mean(garch_table_monthly_trans_DCC_reg$pfret)*100,4)
ind_matrix[3,4] = round(sd(garch_table_monthly_trans_DCC_reg$pfret)*100,4)
ind_matrix[4,4] = round(mean(garch_table_monthly_trans_DCC_reg$pfret)/sd(garch_table_monthly_trans_DCC_reg$pfret),4)
ind_matrix[5,4] = round(min((cumprod(1+garch_table_monthly_trans_DCC_reg$pfret_full)-cummax(cumprod(1+garch_table_monthly_trans_DCC_reg$pfret_full)))/cummax(cumprod(1+garch_table_monthly_trans_DCC_reg$pfret_full))),4)*100
ind_matrix[6,4] = round(lm(garch_table_monthly_trans_DCC_reg$pfret ~ garch_table_monthly_trans_DCC_reg$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,4] = round(lm(garch_table_monthly_trans_DCC_reg$pfret~ garch_table_monthly_trans_DCC_reg$Mkt.RF)[[1]][2],4)
ind_matrix[8,4] = round(lm(garch_table_monthly_trans_DCC_reg$pfret ~ garch_table_monthly_trans_DCC_reg$Mkt.RF)[[1]][1]/(sd(garch_table_monthly_trans_DCC_reg$pfret)-sd(garch_table_monthly_trans_DCC_reg$Mkt.RF)*lm(garch_table_monthly_trans_DCC_reg$pfret ~ garch_table_monthly_trans_DCC_reg$Mkt.RF)[[1]][2]),4)

rownames(ind_matrix) = stat_names
colnames(ind_matrix) = c('84 Obs wo TC', '275 Obs wo TC', '84 Obs w TC', '275 Obs w TC')
print(xtable(ind_matrix, caption = 'Performance of Reg. Est. Portfolios, With and Without Transaction Cost', digits = 3))



## Minimum Variance Portfolios ##

# Daily Rolling GO-GARCH Minimum Variance Porfolio Weights

MinVarFunction = function(var_weights, Sigmas){
  weight = append(1,var_weights)
  Var = weight%*%Sigmas%*%weight
  return(Var)
}

minvardaily = matrix(nrow=NROW(mu_gogarch), ncol = 8)
minvardaily[,1] = 1
for (i in 1:NROW(mu_gogarch)){
  minvardaily[i,2:8]=optim(par = var_weights, fn = MinVarFunction, Sigmas = as.matrix(cov_gogarch[,,i]), control = list(fnscale=1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}

minvardaily = as.data.frame(minvardaily)

minvardaily$date = daily$date[1256:NROW(daily)]

# Daily Weights Based on Backwards Looking 252 days, Rolling Methodology.

daily_backwards = daily[1:9] %>%
  nest(-date) %>%
  rolling_origin(
    initial = 252,
    assess = 1,
    skip =0,
    cumulative = F
  )



Sigmas_backwards = map(daily_backwards$splits, ~ analysis(.x) %>%
                             unnest() %>%
                             tk_xts(., date_var = date) %>%
                             cov(.)) %>%
  setNames(c(ListNamesDates))

minvarbackwards = matrix(nrow=NROW(Sigmas_backwards), ncol = 8)
minvarbackwards[,1] = 1
for (i in 1:NROW(Sigmas_backwards)){
  minvarbackwards[i,2:8]=optim(par = var_weights, fn = MinVarFunction, Sigmas = Sigmas_backwards[[i]], control = list(fnscale=1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}
# Merge the weights with the daily factor returns and consider the portfolio returns of the minimum variance portfolios
## Backwards estimation 252 days, daily rebalancing
minvarbackwards = as.data.frame(minvarbackwards)
minvarbackwards$date = daily$date[253:NROW(daily)]
minvarbackwards = minvarbackwards[minvarbackwards$date>as.Date('1968-06-30'),]
daily_backwards_minvar = merge(daily, minvarbackwards, by = 'date')
daily_backwards_minvar$pfret = rowSums(daily_backwards_minvar[2:9]*daily_backwards_minvar[13:20])
daily_backwards_minvar$pfret_full = daily_backwards_minvar$pfret+daily_backwards_minvar$RF

## Daily Rolling GO-GARCH
daily_gogarch_minvar = merge(daily, minvardaily, by = 'date')
daily_gogarch_minvar$pfret = rowSums(daily_gogarch_minvar[2:9]*daily_gogarch_minvar[13:20])
daily_gogarch_minvar$pfret_full = daily_gogarch_minvar$pfret+daily_gogarch_minvar$RF

# With trans costs
## Backwards Estimation
weight_chgs_daily = minvarbackwards[1:8]*(1+garch_table[2:9])-dplyr::lag(minvarbackwards[1:8])
weight_chgs_daily[1,] = minvarbackwards[1:8,1]
trans_costs_daily = abs(weight_chgs_daily)*0.002
shorting_costs_daily <- rowSums(apply(minvarbackwards[1:8], 2, function(x) ifelse(x < 0, abs(x)*0.000035, 0)))
trans_backwards = daily_backwards_minvar$pfret - rowSums(trans_costs_daily)-shorting_costs_daily
trans_backwards_full = trans_backwards + daily_gogarch_minvar$RF

## Daily Rolling GO-GARCH
weight_chgs_daily_garch = minvardaily[1:8]*(1+garch_table[2:9])-dplyr::lag(minvardaily[1:8])
weight_chgs_daily_garch[1,] = minvardaily[1,1:8]
trans_costs_daily_garch = abs(weight_chgs_daily_garch)*0.002
shorting_costs_daily_garch <- rowSums(apply(minvardaily[1:8], 2, function(x) ifelse(x < 0, abs(x)*0.000035, 0)))
trans_garch = daily_gogarch_minvar$pfret - rowSums(trans_costs_daily_garch)-shorting_costs_daily_garch
trans_garch_full = trans_garch + daily_gogarch_minvar$RF


# Compare the Performance of the Daily Estimated Minimum Variance Portfolios
ind_matrix = matrix(nrow = 8, ncol = 4)
rownames(ind_matrix) = stat_names
ind_matrix[1,1] = round((tail(cumprod(1+daily_backwards_minvar$pfret_full), 1)^(1/n_distinct(daily_backwards_minvar$month))-1)*100,4)
ind_matrix[1,2] = round((tail(cumprod(1+daily_gogarch_minvar$pfret_full),1)^(1/n_distinct(daily_backwards_minvar$month))-1)*100,4)
ind_matrix[1,3] = round((tail(cumprod(1+trans_backwards_full),1)^(1/n_distinct(daily_backwards_minvar$month))-1)*100,4)
ind_matrix[1,4] = round((tail(cumprod(1+trans_garch_full),1)^(1/n_distinct(daily_backwards_minvar$month))-1)*100,4)
ind_matrix[2,1] = round(mean(daily_backwards_minvar$pfret)*100,4)
ind_matrix[2,2] = round(mean(daily_gogarch_minvar$pfret)*100,4)
ind_matrix[2,3] = round(mean(trans_backwards)*100,4)
ind_matrix[2,4] = round(mean(trans_garch)*100,4)
ind_matrix[3,1] = round(sd(daily_backwards_minvar$pfret)*100,4)
ind_matrix[3,2] = round(sd(daily_gogarch_minvar$pfret)*100,4)
ind_matrix[3,3] = round(sd(trans_backwards)*100,4)
ind_matrix[3,4] = round(sd(trans_garch)*100,4)
ind_matrix[4,1] = round(mean(daily_backwards_minvar$pfret)/sd(daily_backwards_minvar$pfret),4)
ind_matrix[4,2] = round(mean(daily_gogarch_minvar$pfret)/sd(daily_gogarch_minvar$pfret),4)
ind_matrix[4,3] = round(mean(trans_backwards)/sd(trans_backwards),4)
ind_matrix[4,4] = round(mean(trans_garch)/sd(trans_garch),4)
ind_matrix[5,1] = round(min((cumprod(1+daily_backwards_minvar$pfret_full)-cummax(cumprod(1+daily_backwards_minvar$pfret_full)))/cummax(cumprod(1+daily_backwards_minvar$pfret_full))),4)*100
ind_matrix[5,2] = round(min((cumprod(1+daily_gogarch_minvar$pfret_full)-cummax(cumprod(1+daily_gogarch_minvar$pfret_full)))/cummax(cumprod(1+daily_gogarch_minvar$pfret_full)))*100,4)
ind_matrix[5,3] = round(min((cumprod(1+trans_backwards_full)-cummax(cumprod(1+trans_backwards_full)))/cummax(cumprod(1+trans_backwards_full)))*100,4)
ind_matrix[5,4] = round(min((cumprod(1+trans_garch_full)-cummax(cumprod(1+trans_garch_full)))/cummax(cumprod(1+trans_garch_full)))*100,4)
ind_matrix[6,1] = round(lm(daily_backwards_minvar$pfret ~ daily_backwards_minvar$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(daily_backwards_minvar$pfret~ daily_backwards_minvar$Mkt.RF)[[1]][2],4)
ind_matrix[8,1] = round(lm(daily_backwards_minvar$pfret ~ daily_backwards_minvar$Mkt.RF)[[1]][1]/(sd(daily_backwards_minvar$pfret)-sd(daily_backwards_minvar$Mkt.RF)*lm(daily_backwards_minvar$pfret ~ daily_backwards_minvar$Mkt.RF)[[1]][2]),4)
ind_matrix[6,2] = round(lm(daily_gogarch_minvar$pfret ~ daily_gogarch_minvar$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(daily_gogarch_minvar$pfret~ daily_gogarch_minvar$Mkt.RF)[[1]][2],4)
ind_matrix[8,2] = round(lm(daily_gogarch_minvar$pfret ~ daily_gogarch_minvar$Mkt.RF)[[1]][1]/(sd(daily_gogarch_minvar$pfret)-sd(daily_gogarch_minvar$Mkt.RF)*lm(daily_gogarch_minvar$pfret ~ daily_gogarch_minvar$Mkt.RF)[[1]][2]),4)
ind_matrix[6,3] = round(lm(trans_backwards ~ daily_gogarch_minvar$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,3] = round(lm(trans_backwards~ daily_gogarch_minvar$Mkt.RF)[[1]][2],4)
ind_matrix[8,3] = round(lm(trans_backwards ~ daily_gogarch_minvar$Mkt.RF)[[1]][1]/(sd(trans_backwards)-sd(daily_gogarch_minvar$Mkt.RF)*lm(trans_backwards ~ daily_gogarch_minvar$Mkt.RF)[[1]][2]),4)
ind_matrix[6,4] = round(lm(trans_garch ~daily_gogarch_minvar$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,4] = round(lm(trans_garch~  daily_gogarch_minvar$Mkt.RF)[[1]][2],4)
ind_matrix[8,4] = round(lm(trans_garch ~  daily_gogarch_minvar$Mkt.RF)[[1]][1]/(sd(trans_garch)-sd( daily_gogarch_minvar$Mkt.RF)*lm(trans_garch ~  daily_gogarch_minvar$Mkt.RF)[[1]][2]),4)

ind_matrix=as.data.frame(ind_matrix)
colnames(ind_matrix) = c('Backwards Looking Est', 'Daily GO-GARCH', 'Backwards Looking w. TC', 'Daily GO-GARCH w. TC')

print(xtable(ind_matrix, caption = 'Minimum Variance Portfolios - Daily Estimation', digits = 3))

rownames_to_column(ind_matrix)%>%
  gt()%>%
  tab_header('Table 51 - Minimum Variance Portfolios - Daily Estimation')


# Minimum Variance Portfolios With  Monthly Rebalancing, Estimating Weights
## Backwards est.
minvar_monthly_backwards = matrix(nrow=NROW(Sigmas), ncol = 8)
minvar_monthly_backwards[,1] = 1
for (i in 1:NROW(Sigmas)){
  minvar_monthly_backwards[i,2:8]=optim(par = var_weights, fn = MinVarFunction, Sigmas = Sigmas[[i]], control = list(fnscale=1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}

minvar_monthly_backwards = minvar_monthly_backwards[289:NROW(minvar_monthly_backwards),]

## Rolling GO-GARCH
minvar_rolling_gogarch = matrix(nrow=NROW(mu_monthly), ncol = 8)
minvar_rolling_gogarch[,1] = 1
for (i in 1:NROW(mu_monthly)){
  print(i)
  minvar_rolling_gogarch[i,2:8]=optim(par = var_weights, fn = MinVarFunction, Sigmas = as.matrix(cov_monthly[,,i]), control = list(fnscale=1), lower = rep(-2.001,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}

## Expanding GO-GARCH
minvar_expanding_gogarch = matrix(nrow=NROW(mu_monthly_expanding), ncol = 8)
minvar_expanding_gogarch[,1] = 1
for (i in 1:NROW(mu_monthly_expanding)){
  print(i)
  minvar_expanding_gogarch[i,2:8]=optim(par = var_weights, fn = MinVarFunction, Sigmas = as.matrix(cov_monthly_expanding[,,i]), control = list(fnscale=1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}

## Rolling DCC
minvar_DCC = matrix(nrow=NROW(mu_monthly_DCC), ncol = 8)
minvar_DCC[,1] = 1
for (i in 1:NROW(mu_monthly_DCC)){
  print(i)
  minvar_DCC[i,2:8]=optim(par = var_weights, fn = MinVarFunction, Sigmas = as.matrix(cov_monthly_DCC[,,i]), control = list(fnscale=1), lower = rep(-2,7), upper = rep(2,7), method = 'L-BFGS-B')$par
}
# Merging the weights and the daily returns

monthly_to_clean = function(matrix){
  df = as.data.frame(matrix)
  colnames(df) = c('Market','Size', 'Value', 'Earnings', 'Investment', 'Momentum', 'Quality', 'BAB')
  df$month = as.yearmon(monthly$month[301:713])
  df = merge(daily, df, by = 'month')
  df$pfret = rowSums(df[3:10]*df[13:20])
  df$pfret_full = df$pfret+df$RF
  return(df)
}

df = as.data.frame(minvar_monthly_backwards)
df$month = as.yearmon(monthly$month[301:713])
merge(daily,df,by = 'month')

minvar_monthly_backwards_df = monthly_to_clean(minvar_monthly_backwards)
minvar_rolling_gogarch_df = monthly_to_clean(minvar_rolling_gogarch[1:413,])
minvar_expanding_gogarch_df = monthly_to_clean(minvar_expanding_gogarch[1:413,])
minvar_DCC_df = monthly_to_clean(minvar_DCC[1:413,])

# Estimating with TC

calctranscosts = function(weight_matrix, monthly_ret_matrix, daily_ret_matrix){
  first_dates_monthly_garch = garch_table_monthly[!duplicated(garch_table_monthly$month),][2]
  weight_chgs_monthly = as.data.frame((1+monthly_ret_matrix))*weight_matrix-dplyr::lag(weight_matrix)
  weight_chgs_monthly[1,] = weight_matrix[1,1:8]
  trans_costs_monthly = rowSums(abs(weight_chgs_monthly)*0.002)
  shorting_costs_monthly = rowSums(apply(weight_matrix, 2, function(x) ifelse(x < 0, abs(x)*0.0007, 0)))
  daily_ret_matrix[daily_ret_matrix$date %in% first_dates_monthly_garch$date,][21] = daily_ret_matrix[daily_ret_matrix$date %in% first_dates_monthly_garch$date,][21]-shorting_costs_monthly
  daily_ret_matrix[daily_ret_matrix$date %in% first_dates_monthly_garch$date,][22] = daily_ret_matrix[daily_ret_matrix$date %in% first_dates_monthly_garch$date,][22]-shorting_costs_monthly
  return(daily_ret_matrix)
}

backwards_minvar_trans = calctranscosts(minvar_monthly_backwards,monthly_rets, minvar_monthly_backwards_df )
minvar_rolling_gogarch_trans = calctranscosts(minvar_rolling_gogarch,monthly_rets, minvar_rolling_gogarch_df )
minvar_expanding_gogarch_trans = calctranscosts(minvar_expanding_gogarch,monthly_rets, minvar_expanding_gogarch_df )
minvar_DCC_trans = calctranscosts(minvar_DCC, monthly_rets, minvar_DCC_df)

# Compare Performance of the Monthly Estimated Models
ind_matrix = matrix(nrow = 8, ncol = 8)
rownames(ind_matrix) = stat_names
ind_matrix[1,1] = round((tail(cumprod(1+minvar_monthly_backwards_df$pfret_full), 1)^(1/n_distinct(minvar_monthly_backwards_df$month))-1)*100,4)
ind_matrix[1,2] = round((tail(cumprod(1+minvar_rolling_gogarch_df$pfret_full),1)^(1/n_distinct(minvar_monthly_backwards_df$month))-1)*100,4)
ind_matrix[1,3] = round((tail(cumprod(1+minvar_expanding_gogarch_df$pfret_full),1)^(1/n_distinct(minvar_monthly_backwards_df$month))-1)*100,4)
ind_matrix[1,4] = round((tail(cumprod(1+minvar_DCC_df$pfret_full),1)^(1/n_distinct(minvar_monthly_backwards_df$month))-1)*100,4)
ind_matrix[2,1] = round(mean(minvar_monthly_backwards_df$pfret)*100,4)
ind_matrix[2,2] = round(mean(minvar_rolling_gogarch_df$pfret)*100,4)
ind_matrix[2,3] = round(mean(minvar_expanding_gogarch_df$pfret)*100,4)
ind_matrix[2,4] = round(mean(minvar_DCC_df$pfret)*100,4)
ind_matrix[3,1] = round(sd(minvar_monthly_backwards_df$pfret)*100,4)
ind_matrix[3,2] = round(sd(minvar_rolling_gogarch_df$pfret)*100,4)
ind_matrix[3,3] = round(sd(minvar_expanding_gogarch_df$pfret)*100,4)
ind_matrix[3,4] = round(sd(minvar_DCC_df$pfret)*100,4)
ind_matrix[4,1] = round(mean(minvar_monthly_backwards_df$pfret)/sd(minvar_monthly_backwards_df$pfret),4)
ind_matrix[4,2] = round(mean(minvar_rolling_gogarch_df$pfret)/sd(minvar_rolling_gogarch_df$pfret),4)
ind_matrix[4,3] = round(mean(minvar_expanding_gogarch_df$pfret)/sd(minvar_expanding_gogarch_df$pfret),4)
ind_matrix[4,4] = round(mean(minvar_DCC_df$pfret)/sd(minvar_DCC_df$pfret),4)
ind_matrix[5,1] = round(min((cumprod(1+minvar_monthly_backwards_df$pfret_full)-cummax(cumprod(1+minvar_monthly_backwards_df$pfret_full)))/cummax(cumprod(1+minvar_monthly_backwards_df$pfret_full))),4)*100
ind_matrix[5,2] = round(min((cumprod(1+minvar_rolling_gogarch_df$pfret_full)-cummax(cumprod(1+minvar_rolling_gogarch_df$pfret_full)))/cummax(cumprod(1+minvar_rolling_gogarch_df$pfret_full)))*100,4)
ind_matrix[5,3] = round(min((cumprod(1+minvar_expanding_gogarch_df$pfret_full)-cummax(cumprod(1+minvar_expanding_gogarch_df$pfret_full)))/cummax(cumprod(1+minvar_expanding_gogarch_df$pfret_full)))*100,4)
ind_matrix[5,4] = round(min((cumprod(1+minvar_DCC_df$pfret_full)-cummax(cumprod(1+minvar_DCC_df$pfret_full)))/cummax(cumprod(1+minvar_DCC_df$pfret_full)))*100,4)
ind_matrix[1,5] = round((tail(cumprod(1+backwards_minvar_trans$pfret_full),1)^(1/n_distinct(minvar_monthly_backwards_df$month))-1)*100,4)
ind_matrix[2,5] = round(mean(backwards_minvar_trans$pfret)*100,4)
ind_matrix[3,5] = round(sd(backwards_minvar_trans$pfret)*100,4)
ind_matrix[4,5] = round(mean(backwards_minvar_trans$pfret)/sd(backwards_minvar_trans$pfret),4)
ind_matrix[5,5] = round(min((cumprod(1+backwards_minvar_trans$pfret_full)-cummax(cumprod(1+backwards_minvar_trans$pfret_full)))/cummax(cumprod(1+backwards_minvar_trans$pfret_full)))*100,4)
ind_matrix[1,6] = round((tail(cumprod(1+minvar_rolling_gogarch_trans$pfret_full),1)^(1/n_distinct(minvar_monthly_backwards_df$month))-1)*100,4)
ind_matrix[2,6] = round(mean(minvar_rolling_gogarch_trans$pfret)*100,4)
ind_matrix[3,6] = round(sd(minvar_rolling_gogarch_trans$pfret)*100,4)
ind_matrix[4,6] = round(mean(minvar_rolling_gogarch_trans$pfret)/sd(minvar_rolling_gogarch_trans$pfret),4)
ind_matrix[5,6] = round(min((cumprod(1+minvar_rolling_gogarch_trans$pfret_full)-cummax(cumprod(1+minvar_rolling_gogarch_trans$pfret_full)))/cummax(cumprod(1+minvar_rolling_gogarch_trans$pfret_full)))*100,4)
ind_matrix[1,7] = round((tail(cumprod(1+minvar_expanding_gogarch_trans$pfret_full),1)^(1/n_distinct(minvar_monthly_backwards_df$month))-1)*100,4)
ind_matrix[2,7] = round(mean(minvar_expanding_gogarch_trans$pfret)*100,4)
ind_matrix[3,7] = round(sd(minvar_expanding_gogarch_trans$pfret)*100,4)
ind_matrix[4,7] = round(mean(minvar_expanding_gogarch_trans$pfret)/sd(minvar_expanding_gogarch_trans$pfret),4)
ind_matrix[5,7] = round(min((cumprod(1+minvar_expanding_gogarch_trans$pfret_full)-cummax(cumprod(1+minvar_expanding_gogarch_trans$pfret_full)))/cummax(cumprod(1+minvar_expanding_gogarch_trans$pfret_full)))*100,4)
ind_matrix[1,8] = round((tail(cumprod(1+minvar_DCC_trans$pfret_full),1)^(1/n_distinct(minvar_monthly_backwards_df$month))-1)*100,4)
ind_matrix[2,8] = round(mean(minvar_DCC_trans$pfret)*100,4)
ind_matrix[3,8] = round(sd(minvar_DCC_trans$pfret)*100,4)
ind_matrix[4,8] = round(mean(minvar_DCC_trans$pfret)/sd(minvar_DCC_trans$pfret),4)
ind_matrix[5,8] = round(min((cumprod(1+minvar_DCC_trans$pfret_full)-cummax(cumprod(1+minvar_DCC_trans$pfret_full)))/cummax(cumprod(1+minvar_DCC_trans$pfret_full)))*100,4)
ind_matrix[6,1] = round(lm(minvar_monthly_backwards_df$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,1] = round(lm(minvar_monthly_backwards_df$pfret~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2],4)
ind_matrix[8,1] = round(lm(minvar_monthly_backwards_df$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]/(sd(minvar_monthly_backwards_df$pfret)-sd(minvar_monthly_backwards_df$Mkt.RF)*lm(minvar_monthly_backwards_df$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2]),4)
ind_matrix[6,2] = round(lm(minvar_rolling_gogarch_df$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,2] = round(lm(minvar_rolling_gogarch_df$pfret~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2],4)
ind_matrix[8,2] = round(lm(minvar_rolling_gogarch_df$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]/(sd(minvar_rolling_gogarch_df$pfret)-sd(minvar_monthly_backwards_df$Mkt.RF)*lm(minvar_rolling_gogarch_df$pfret ~minvar_monthly_backwards_df$Mkt.RF)[[1]][2]),4)
ind_matrix[6,3] = round(lm(minvar_expanding_gogarch_df$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,3] = round(lm(minvar_expanding_gogarch_df$pfret~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2],4)
ind_matrix[8,3] = round(lm(minvar_expanding_gogarch_df$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]/(sd(minvar_expanding_gogarch_df$pfret)-sd(minvar_monthly_backwards_df$Mkt.RF)*lm(minvar_expanding_gogarch_df$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2]),4)
ind_matrix[6,4] = round(lm(minvar_DCC_df$pfret ~minvar_monthly_backwards_df$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,4] = round(lm(minvar_DCC_df$pfret~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2],4)
ind_matrix[8,4] = round(lm(minvar_DCC_df$pfret ~minvar_monthly_backwards_df$Mkt.RF)[[1]][1]/(sd(minvar_DCC_df$pfret)-sd(minvar_monthly_backwards_df$Mkt.RF)*lm(minvar_DCC_df$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2]),4)
ind_matrix[6,5] = round(lm(backwards_minvar_trans$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,5] = round(lm(backwards_minvar_trans$pfret~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2],4)
ind_matrix[8,5] = round(lm(backwards_minvar_trans$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]/(sd(backwards_minvar_trans$pfret)-sd(minvar_monthly_backwards_df$Mkt.RF)*lm(backwards_minvar_trans$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2]),4)
ind_matrix[6,6] = round(lm(minvar_rolling_gogarch_trans$pfret ~minvar_monthly_backwards_df$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,6] = round(lm(minvar_rolling_gogarch_trans$pfret~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2],4)
ind_matrix[8,6] = round(lm(minvar_rolling_gogarch_trans$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]/(sd(minvar_rolling_gogarch_trans$pfret)-sd(minvar_monthly_backwards_df$Mkt.RF)*lm(minvar_rolling_gogarch_trans$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2]),4)
ind_matrix[6,7] = round(lm(minvar_expanding_gogarch_trans$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,7] = round(lm(minvar_expanding_gogarch_trans$pfret~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2],4)
ind_matrix[8,7] = round(lm(minvar_expanding_gogarch_trans$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]/(sd(minvar_expanding_gogarch_trans$pfret)-sd(minvar_monthly_backwards_df$Mkt.RF)*lm(minvar_expanding_gogarch_trans$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2]),4)
ind_matrix[6,8] = round(lm(minvar_DCC_trans$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]*100,4)
ind_matrix[7,8] = round(lm(minvar_DCC_trans$pfret~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2],4)
ind_matrix[8,8] = round(lm(minvar_DCC_trans$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][1]/(sd(minvar_DCC_trans$pfret)-sd(minvar_monthly_backwards_df$Mkt.RF)*lm(minvar_DCC_trans$pfret ~ minvar_monthly_backwards_df$Mkt.RF)[[1]][2]),4)

ind_matrix=as.data.frame(ind_matrix)
colnames(ind_matrix) = c('Backwards Looking Est', 'Rolling GO-GARCH', 'Expanding GO-GARCH', 'DCC GARCH', 'Backwards Looking w TC', 'Rolling GO-GARCH w TC', 'Expanding GO-GARCH w TC', 'DCC GARCH w TC')

print(xtable(ind_matrix, caption = 'Minimum Variance Portfolios - Monthly Estimation', digits = 3))
