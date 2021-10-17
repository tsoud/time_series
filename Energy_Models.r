library(stats)
library(tseries)
library(TSA)
library(fpp)
library(fpp2)
library(dlm)
library(vars)
library(ggplot2)
library(forecast)
library(urca)

# Use these variables for easy conversion
btu2MW <- 3.412e-6
btu2kW <- 3.412e-3
MWh2MW <- (24*30)

dta <- load("Monthly_Data.RData")
ts <- load("Time_Series.RData")
ILdta <- load("IL_Data.RData")
cap <- load("capacity.RData")

print(c(dta, ts, ILdta, cap))

tot <- monthly_dta_ts$Elec_by_Source[,"V14"]  # total electrical generation from all sources
rnw_gen <- monthly_dta_ts$Elec_by_Source[,"V13"] + monthly_dta_ts$Elec_by_Source[,"V12"]  # electric output from renewables
rnw_prct <- (rnw_gen/tot)*100  # renewables as percent of total

options(repr.plot.width = 10, repr.plot.height = 7)

dta <- cbind("Output (Trillions Btu)" = rnw_gen, "Percent of Total Electricity" = rnw_prct, 
             "log(Pct.of.Total)" = log(rnw_prct), "BoxCox(Pct.of.Total)" = log(rnw_prct))

autoplot(dta, facets = T) + xlab("Year") + ylab("") + 
    ggtitle("Electricity from Renewables (Solar + Wind)")

# overwrite time-series with new ts starting in 2000
rnw_gen <- window(rnw_gen, start = c(1999,12))  # electric output from renewables
rnw_prct <- (rnw_gen/tot)*100  # renewables as percent of total

options(repr.plot.width = 10, repr.plot.height = 6)

dta <- cbind("Output (Trillions Btu)" = rnw_gen, "Percent of Total Electricity" = rnw_prct, 
             "log(Pct.of.Total)" = log(rnw_prct))

autoplot(dta, facets = T) + xlab("Year") + ylab("") + 
    ggtitle("Electricity from Renewables (Solar + Wind)")

options(repr.plot.width = 10, repr.plot.height = 6)

ggtsdisplay(rnw_prct, na.rm = T)

kpss.test(rnw_prct)

fit <- mstl(rnw_prct, lambda = 'auto')
autoplot(fit) + ggtitle("STL Decomposition")

rnw_prct %>% diff(lag = 12) %>% ggtsdisplay()

rnw_prct %>% diff(lag = 1) %>% ggtsdisplay()

log(rnw_prct) %>% diff(lag = 12) %>% ggtsdisplay()

log(rnw_prct) %>% diff(lag = 1) %>% ggtsdisplay()

rnw_prct %>% diff(lag = 12) %>% diff(lag = 1) %>% ggtsdisplay()

log(rnw_prct) %>% diff(lag = 12) %>% diff(lag = 1) %>% ggtsdisplay()

rnw_tr <- window(rnw_prct, start = c(2004, 1), end = c(2011, 12))

wins <- function(ts, k, n, p, H) {

    # ts = time series for cv
    # k = Min data length for fitting a model
    # n = Number of data points
    # p = Period
    # H = Forecast Horizon

    tr_exp <- vector("list", n-k)  # Expanding training set
    tr_roll <- vector("list", n-k)  # Rolling training set
    testSet <- vector("list", n-k)  # Test set

    start <- tsp(ts)[1]
    end <- tsp(ts)[1] + (k-2)/p
    step <- 1/p

    suppressWarnings(
        for (i in 1:(n-k)) {
        # Set up model windows
        tr_exp[[i]] <- window(ts, end = end + i*step)
        tr_roll[[i]] <- window(ts, start = start + (i-1)*step, end = end + i*step)
        testSet[[i]] <- window(ts, start = end + (i+1)*step, end = end + (i+H)*step)
    } )
    
    return(list("tr_exp" = tr_exp, "tr_roll" = tr_roll, "testSet" = testSet))
}

# set the variables for the subset models
k <- 60
n <- length(rnw_tr)
p <- 12
H <- 12
# create models for first cross-validation
rnw_tests <- wins(rnw_tr, k = k, n = n, p = p, H = H)

Model_1a <- ets(rnw_prct, model = 'ZZZ', lambda = 'auto')
summary(Model_1a)

Model_1b <- ets(rnw_prct, model = 'ZZZ', lambda = NULL)
summary(Model_1b)

Model_1c <- ets(rnw_prct, model = 'ZZZ', lambda = 0)
summary(Model_1c)

# create test models
m1a <- function(x, h) {forecast(ets(x, model = 'AAA', lambda = 'auto'), h = h)}
m1b <- function(x, h) {forecast(ets(x, model = 'MAM', lambda = NULL), h = h)}
m1c <- function(x, h) {forecast(ets(x, model = 'AAA', lambda = 0), h = h)}

# test each model on the expanding and rolling training set
m1a_exp <- lapply(rnw_tests$tr_exp, function(x) m1a(x, h=H))
m1a_roll <- lapply(rnw_tests$tr_roll, function(x) m1a(x, h=H))
m1b_exp <- lapply(rnw_tests$tr_exp, function(x) m1b(x, h=H))
m1b_roll <- lapply(rnw_tests$tr_roll, function(x) m1b(x, h=H))
m1c_exp <- lapply(rnw_tests$tr_exp, function(x) m1c(x, h=H))
m1c_roll <- lapply(rnw_tests$tr_roll, function(x) m1c(x, h=H))

aicc <- c()
rmse <- c()
mae <- c()
i <- 1

for (mdl in list(m1a_exp, m1a_roll, m1b_exp, m1b_roll, m1c_exp, m1c_roll)) {
    aicc[i] <- mean(sapply(mdl, function(x) x$model$aicc))
    rmse[i] <- mean(sapply(1:(n-k), function(j) accuracy(mdl[[j]], rnw_tests$testSet[[j]])[2,"RMSE"]))
    mae[i] <- mean(sapply(1:(n-k), function(j) accuracy(mdl[[j]], rnw_tests$testSet[[j]])[2,"MAE"]))
    i <- i+1
}

results <- data.frame(Model = as.character(substitute(c(m1a_exp, m1a_roll, m1b_exp, m1b_roll, m1c_exp, m1c_roll)))[-1], 
                      AICc = aicc,  RMSE = rmse,  MAE = mae)
results

options(repr.plot.width = 8, repr.plot.height = 4)

Model_2a <- auto.arima(rnw_prct, seasonal = T, lambda = "auto", stepwise = F)
summary(Model_2a)

checkresiduals(Model_2a)

Model_2b <- auto.arima(rnw_prct, seasonal = T, lambda = NULL, stepwise = F)
summary(Model_2b)

checkresiduals(Model_2b)

Model_2c <- auto.arima(rnw_prct, seasonal = T, lambda = 0, stepwise = F)
summary(Model_2c)

checkresiduals(Model_2c)

# create test models
m2a <- function(x, h) {forecast(Arima(x, order = c(2,1,1), seasonal = c(1,1,1), lambda = 'auto'), h = h)}
m2b <- function(x, h) {forecast(Arima(x, order = c(1,1,2), seasonal = c(0,1,1), lambda = NULL), h = h)}

# test each model on the expanding and rolling training set
m2a_exp <- lapply(rnw_tests$tr_exp, function(x) m2a(x, h=H))
m2a_roll <- lapply(rnw_tests$tr_roll, function(x) m2a(x, h=H))
m2b_exp <- lapply(rnw_tests$tr_exp, function(x) m2b(x, h=H))
m2b_roll <- lapply(rnw_tests$tr_roll, function(x) m2b(x, h=H))

aicc <- c()
rmse <- c()
mae <- c()
i <- 1

for (mdl in list(m2a_exp, m2a_roll, m2b_exp, m2b_roll)) {
    aicc[i] <- mean(sapply(mdl, function(x) x$model$aicc))
    rmse[i] <- mean(sapply(1:(n-k), function(j) accuracy(mdl[[j]], rnw_tests$testSet[[j]])[2,"RMSE"]))
    mae[i] <- mean(sapply(1:(n-k), function(j) accuracy(mdl[[j]], rnw_tests$testSet[[j]])[2,"MAE"]))
    i <- i+1
}

results <- data.frame(Model = as.character(substitute(c(m2a_exp, m2a_roll, m2b_exp, m2b_roll)))[-1], 
                      AICc = aicc,  RMSE = rmse,  MAE = mae)
results

mdl <- list()
K <- c()
mdl_nm <- c()
AICc <- c()
BIC <- c()

# Frequency is 12 so K max = 6
for (i in 1:6) {
    fit <- auto.arima(rnw_prct, xreg = fourier(rnw_prct, K = i), seasonal = F, lambda = 'auto', stepwise = F)
    mdl[[i]] <- fit
    K[i] <- i
    mdl_nm[i] <- paste("ARIMA", "(",toString(fit$arma[c(1,6,2)]),")", sep = "")
    AICc[i] <- fit$aicc
    BIC[i] <- fit$bic
}

res <- data.frame("K" = K, "Model" = mdl_nm, "AICc" = AICc, "BIC" = BIC)
res

checkresiduals(Arima(rnw_prct, order = c(4,1,1), xreg = fourier(rnw_prct, K=2), include.drift = T, lambda = 'auto'))

# create test models
m3a <- function(x, h) {forecast(Arima(x, order = c(4,1,1), xreg = fourier(x, K=2), include.drift = T, lambda = 'auto'), 
                                xreg = fourier(x, K = 2, h = h))}
m3b <- function(x, h) {forecast(Arima(x, order = c(4,1,1), xreg = fourier(x, K=3), include.drift = T, lambda = 'auto'), 
                                xreg = fourier(x, K = 3, h = h))}
m3c <- function(x, h) {forecast(Arima(x, order = c(4,1,1), xreg = fourier(x, K=2), include.drift = T, lambda = 0), 
                                xreg = fourier(x, K = 2, h = h))}

# test each model on the expanding and rolling training set
m3a_exp <- lapply(rnw_tests$tr_exp, function(x) m3a(x, h=H))
m3a_roll <- lapply(rnw_tests$tr_roll, function(x) m3a(x, h=H))
m3b_exp <- lapply(rnw_tests$tr_exp, function(x) m3b(x, h=H))
m3b_roll <- lapply(rnw_tests$tr_roll, function(x) m3b(x, h=H))
m3c_exp <- lapply(rnw_tests$tr_exp, function(x) m3c(x, h=H))
m3c_roll <- lapply(rnw_tests$tr_roll, function(x) m3c(x, h=H))

aicc <- c()
rmse <- c()
mae <- c()
i <- 1

for (mdl in list(m3a_exp, m3a_roll, m3b_exp, m3b_roll, m3c_exp, m3c_roll)) {
    aicc[i] <- mean(sapply(mdl, function(x) x$model$aicc))
    rmse[i] <- mean(sapply(1:(n-k), function(j) accuracy(mdl[[j]], rnw_tests$testSet[[j]])[2,"RMSE"]))
    mae[i] <- mean(sapply(1:(n-k), function(j) accuracy(mdl[[j]], rnw_tests$testSet[[j]])[2,"MAE"]))
    i <- i+1
}

results <- data.frame(Model = as.character(substitute(c(m3a_exp, m3a_roll, m3b_exp, m3b_roll, m3c_exp, m3c_roll)))[-1], 
                      AICc = aicc,  RMSE = rmse,  MAE = mae)
results

# ETS Model
rnw_ETS <- function(x, h) {forecast(ets(x, model = 'AAA', lambda = 0), h = h)}
# ARIMA Model
rnw_ARIMA <- function(x, h) {forecast(Arima(x, order = c(1,1,2), seasonal = c(0,1,1), lambda = NULL), h = h)}
# Dynamic Harmonic Regression
rnw_DHR <- function(x, h) {forecast(Arima(x, order = c(4,1,1), xreg = fourier(x, K=2), include.drift = T, lambda = 0), 
                                xreg = fourier(x, K = 2, h = h))}

# set the variables for the subset models
k <- 144
n <- length(rnw_prct)
p <- 12
H <- 12
# create models for first cross-validation
rnw_wins <- wins(rnw_prct, k = k, n = n, p = p, H = H)

# test each model on the expanding and rolling training set
rnw_ETS_exp <- lapply(rnw_wins$tr_exp, function(x) rnw_ETS(x, h=H))
rnw_ETS_roll <- lapply(rnw_wins$tr_roll, function(x) rnw_ETS(x, h=H))
rnw_ARIMA_exp <- lapply(rnw_wins$tr_exp, function(x) rnw_ARIMA(x, h=H))
rnw_ARIMA_roll <- lapply(rnw_wins$tr_roll, function(x) rnw_ARIMA(x, h=H))
rnw_DHR_exp <- lapply(rnw_wins$tr_exp, function(x) rnw_DHR(x, h=H))
rnw_DHR_roll <- lapply(rnw_wins$tr_roll, function(x) rnw_DHR(x, h=H))

err <- list()
aicc <- c()
rmse <- c()
mae <- c()
i <- 1

for (mdl in list(rnw_ETS_exp, rnw_ETS_roll, rnw_ARIMA_exp, rnw_ARIMA_roll, rnw_DHR_exp, rnw_DHR_roll)) {
    # absolute errors:
    err[[i]] <- sapply(1:(n-k), function(j) abs(rnw_wins$testSet[[j]] - mdl[[j]]$mean))
    # Overall RMSE of forecasts
    rmse[i] <- mean(sapply(1:(n-k), function(j) accuracy(mdl[[j]], rnw_wins$testSet[[j]])[2,"RMSE"]))
    # Overall MAE of forecasts
    mae[i] <- mean(sapply(1:(n-k), function(j) accuracy(mdl[[j]], rnw_wins$testSet[[j]])[2,"MAE"]))
    # Overall AICc (less important)                      
    aicc[i] <- mean(sapply(mdl, function(x) x$model$aicc))
    i <- i+1
}

results <- data.frame(Model = as.character(substitute(c(rnw_ETS_exp, rnw_ETS_roll, rnw_ARIMA_exp, rnw_ARIMA_roll, rnw_DHR_exp, rnw_DHR_roll)))[-1], 
                      AICc = aicc,  RMSE = rmse,  MAE = mae)
results

# RMSE and MAE by forecast horizon for each model
MAE_h <- list()
RMSE_h <- list()

for (i in 1:length(err)) {
    mat <- sapply(1:(n-k), function(j) as.vector(err[[i]][[j]][1:p]))
    MAE_h[[i]] <- rowMeans(mat, na.rm = T)
    RMSE_h[[i]] <- sqrt(rowMeans(mat^2, na.rm = T))
}

df_mae <- data.frame(H = 1:p, ETS_exp = MAE_h[[1]], ETS_roll = MAE_h[[2]], ARIMA_exp = MAE_h[[3]], ARIMA_roll = MAE_h[[4]], DHR_exp = MAE_h[[5]], DHR_roll = MAE_h[[6]])
df_rmse <- data.frame(H = 1:p, ETS_exp = RMSE_h[[1]], ETS_roll = RMSE_h[[2]], ARIMA_exp = RMSE_h[[3]], ARIMA_roll = RMSE_h[[4]], DHR_exp = RMSE_h[[5]], DHR_roll = RMSE_h[[6]])

options(repr.plot.width = 12, repr.plot.height = 6)

ggplot(data = df_mae, aes(H)) +  
    geom_line(aes(y = ETS_exp, color = "ETS Expanding Window")) +  
    geom_line(aes(y = ETS_roll, color = "ETS Rolling Window")) +  
    geom_line(aes(y = ARIMA_exp, color = "sARIMA Expanding Window")) +  
    geom_line(aes(y = ARIMA_roll, color = "sARIMA Rolling window")) +
    geom_line(aes(y = DHR_exp, color = "DHR Expanding window")) +
    geom_line(aes(y = DHR_roll, color = "DHR Rolling window")) +
    xlab("Forecast Horizon (Months)") + ylab("MAE") + ggtitle("MAE Results") +
    scale_x_continuous(breaks = seq(0,12,2)) + scale_y_continuous(breaks = seq(0,1.2,0.2)) +
    guides(color = guide_legend(title = "Model"))

ggplot(data = df_rmse, aes(H)) +  
    geom_line(aes(y = ETS_exp, color = "ETS Expanding Window")) +  
    geom_line(aes(y = ETS_roll, color = "ETS Rolling Window")) +  
    geom_line(aes(y = ARIMA_exp, color = "sARIMA Expanding Window")) +  
    geom_line(aes(y = ARIMA_roll, color = "sARIMA Rolling window")) +
    geom_line(aes(y = DHR_exp, color = "DHR Expanding window")) +
    geom_line(aes(y = DHR_roll, color = "DHR Rolling window")) +
    xlab("Forecast Horizon (Months)") + ylab("RMSE") + ggtitle("RMSE Results") +
    scale_x_continuous(breaks = seq(0,12,2)) + scale_y_continuous(breaks = seq(0,1.5,0.25)) +
    guides(color = guide_legend(title = "Model"))

head(rnw_prct, 20)

renewables_1yr <- rnw_ARIMA(x = rnw_prct, h = 12)

options(repr.plot.width = 10, repr.plot.height = 5)

autoplot(renewables_1yr, include = 120) + ggtitle("Renewables (Solar+Wind) 1-yr Forecast") +
    xlab("Year") + ylab("Portion of Total Electricity Generated (%)")

options(repr.plot.width = 10, repr.plot.height = 6)

avg <- c()
i <- 1

for (yr in 2009:2018) {
    avg[i] <- mean(window(rnw_prct, start = c(yr, 12), end = c(yr+1, 11)))
    i <- i+1
}

yr_avg <- data.frame(year = seq(2010,2019), avg_pct = avg)
yr_avg <- rbind(yr_avg, c(2020, mean(renewables_1yr$mean)))

# Plot yearly average
cols <- ifelse(yr_avg$year > 2019, "firebrick2", "dodgerblue")
barplot(avg_pct ~ year, yr_avg, 
        xlab = "Year", ylab = "% of Electricity Production", ylim = c(0,12), col = cols, border = NA)

print(t(yr_avg))

renewables_2yr <- rnw_ARIMA(x = rnw_prct, h = 24)

options(repr.plot.width = 10, repr.plot.height = 5)

autoplot(renewables_2yr, include = 120) + ggtitle("Renewables (Solar+Wind) 2-yr Forecast") +
    xlab("Year") + ylab("Portion of Total Electricity Generated (%)")

options(repr.plot.width = 10, repr.plot.height = 6)

yr_avg <- rbind(yr_avg, c(2021, mean(renewables_2yr$mean[13:24])))

# Plot yearly average
cols <- ifelse(yr_avg$year > 2019, "firebrick2", "dodgerblue")
barplot(avg_pct ~ year, yr_avg, 
        xlab = "Year", ylab = "% of Electricity Production", ylim = c(0,12), col = cols, border = NA)

print(t(yr_avg))

renewables_5yr <- rnw_ARIMA(x = rnw_prct, h = 60)

options(repr.plot.width = 10, repr.plot.height = 5)

autoplot(renewables_5yr, include = 120) + ggtitle("Renewables (Solar+Wind) 5-yr Forecast") +
    xlab("Year") + ylab("Portion of Total Electricity Generated (%)")

options(repr.plot.width = 10, repr.plot.height = 6)

y3 <- c(2022, mean(renewables_5yr$mean[25:36]))
y4 <- c(2023, mean(renewables_5yr$mean[37:48]))
y5 <- c(2024, mean(renewables_5yr$mean[49:60]))

yr_avg <- rbind(yr_avg, y3)
yr_avg <- rbind(yr_avg, y4)
yr_avg <- rbind(yr_avg, y5)

# Plot yearly average
cols <- ifelse(yr_avg$year > 2019, "firebrick2", "dodgerblue")
barplot(avg_pct ~ year, yr_avg, 
        xlab = "Year", ylab = "% of Electricity Production", col = cols, border = NA)

print(t(yr_avg))

# Renewables = (Wind + Solar Electrical Production)
head(rnw_prct)

rnw_cap <- solar_cap_ts + wind_cap_ts
rnw_cap <- window(rnw_cap, start = c(1999, 12), end = c(2019,11))

autoplot(cbind("Wind+Solar % Elec Output" = rnw_prct, "Installed Capacity (MW)" = rnw_cap), facets = T) + 
    xlab("Year") + ylab("")

options(repr.plot.width = 8, repr.plot.height = 4)

ggtsdisplay(rnw_cap)

rnw_cap %>% diff(n = 1) %>% ggtsdisplay

rnw_cap %>% diff(n = 12) %>% ggtsdisplay

rnw_cap %>% diff(n = 12) %>% diff(n = 1) %>% ggtsdisplay

fit <- mstl(rnw_cap, lambda = 'auto')
autoplot(fit) + ggtitle("STL Decomposition")

cap_model <- auto.arima(rnw_cap, lambda = 'auto', stepwise = F)
summary(cap_model)

checkresiduals(cap_model)

tot <- monthly_dta_ts$Elec_by_Source[,"V14"]
ng_prod <- monthly_dta_ts$Elec_by_Source[,"V4"]
ng_pct <- ng_prod/tot*100
ng_pct <- window(ng_pct, start = c(1999, 12), end = c(2019,11))

autoplot(cbind("Wind+Solar % Elec Output" = rnw_prct, "NatGas % Elec Output" = ng_pct), facets = T) + 
    xlab("Year") + ylab("")

options(repr.plot.width = 8, repr.plot.height = 4)

ggtsdisplay(ng_pct)

ng_pct %>% diff(n = 12) %>% ggtsdisplay

ng_pct %>% diff(n = 1) %>% ggtsdisplay

ng_pct %>% diff(n = 12) %>% diff(n = 1) %>% ggtsdisplay

fit <- mstl(ng_pct, lambda = 'auto')
autoplot(fit) + ggtitle("STL Decomposition")

BoxCox.lambda(ng_pct)

ng_mdl <- auto.arima(ng_pct, lambda = NULL, stepwise = T)
summary(ng_mdl)

checkresiduals(ng_mdl)

cap_fc1 <- forecast(cap_model, h = 12)
cap_fc2 <- forecast(cap_model, h = 24)
cap_fc5 <- forecast(cap_model, h = 60)

ng_fc1 <- forecast(ng_mdl, h = 12)
ng_fc2 <- forecast(ng_mdl, h = 24)
ng_fc5 <- forecast(ng_mdl, h = 60)

rnw_prod_ARerr <- auto.arima(rnw_prct, xreg = ng_pct, stepwise = F)
summary(rnw_prod_ARerr)

checkresiduals(rnw_prod_ARerr)

rnw_reg_fc1 <- forecast(rnw_prod_ARerr, xreg = ng_fc1$mean)
rnw_reg_fc2 <- forecast(rnw_prod_ARerr, xreg = ng_fc2$mean)
rnw_reg_fc5 <- forecast(rnw_prod_ARerr, xreg = ng_fc5$mean)

rnw_VAR <- VAR(cbind(rnw_prct, rnw_cap, ng_pct), type = 'both', season = 12)
summary(rnw_VAR)

rnw_fc_VAR1 <- forecast(rnw_VAR, h = 12)
rnw_fc_VAR2 <- forecast(rnw_VAR, h = 24)
rnw_fc_VAR5 <- forecast(rnw_VAR, h = 60)

options(repr.plot.width = 12, repr.plot.height = 6)

autoplot(rnw_prct) + 
    autolayer(renewables_5yr, series = "sARIMA", PI = F) + 
    autolayer(rnw_reg_fc5, series = "Regression w Nat Gas, ARIMA errors", PI = F) +
    autolayer(rnw_fc_VAR5$forecast$rnw_prct$mean, series = "Regression with Gas and Capacity, VAR", PI = F) +
    xlim(c(2015, NA)) + xlab("Year") + ylab("Percent of total Production")


