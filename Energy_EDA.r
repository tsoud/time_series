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

rbind(monthly_meta$Elec_by_Source, head(monthly_dta$Elec_by_Source))

rbind(monthly_meta$Consump_Elec, head(monthly_dta$Consump_Elec))

options(repr.plot.width = 10, repr.plot.height = 5)

coal_prod <- monthly_dta_ts$Elec_by_Source[,"V2"]
coal_cnsm <- monthly_dta_ts$Consump_Elec[,"V2"]

autoplot(cbind(Generated = coal_prod, Input = coal_cnsm)) + 
         xlab("Year") + ylab("Trillion Btu") + ggtitle("Electricity from Coal") + 
         guides(color = guide_legend(title = ""))

ng_prod <- monthly_dta_ts$Elec_by_Source[,"V4"]
ng_cnsm <- monthly_dta_ts$Consump_Elec[,"V3"]

autoplot(cbind(Generated = ng_prod, Input = ng_cnsm)) + 
         xlab("Year") + ylab("Trillion Btu") + ggtitle("Electricity from Natural Gas") + 
         guides(color = guide_legend(title = "Energy Source"))

rnw_prod <- monthly_dta_ts$Elec_by_Source[,"V13"] + monthly_dta_ts$Elec_by_Source[,"V12"]
rnw_cnsm <- monthly_dta_ts$Consump_Elec[,"V10"] + monthly_dta_ts$Consump_Elec[,"V13"]

autoplot(rnw_prod, series = "Generated") + 
         autolayer(rnw_cnsm, series = "Input", na.rm = T) + 
         xlab("Year") + ylab("Trillion Btu") + ggtitle("Electricity from Renewables (Solar + Wind)") + 
         guides(color = guide_legend(title = "Energy Source"))

eff_coal <- (coal_prod/coal_cnsm)*100
eff_ng <- (ng_prod/ng_cnsm)*100
eff_rnw <- (rnw_prod/rnw_cnsm)*100

autoplot(cbind("Coal" = eff_coal, "Nat.Gas" = eff_ng, "Wind+Solar" = eff_rnw)) + 
         xlab("Year") + ylab("Efficiency (%)") + ggtitle("Efficiency of Electricity Generation by Source") + 
         guides(color = guide_legend(title = "Energy Source"))

tot <- monthly_dta_ts$Elec_by_Source[,"V14"]
coal_p <- (coal_prod/tot)*100
ng_p <- (ng_prod/tot)*100
rnw_p <- (rnw_prod/tot)*100

autoplot(cbind("Coal" = coal_p, "Nat.Gas" = ng_p, "Wind+Solar" = rnw_p)) + 
         xlab("Year") + ylab("Proportion (%)") + ggtitle("Electricity by Source as Percentage of Total") + 
         guides(color = guide_legend(title = "Energy Source"))

head(capacity, 10)

cap_all <- cbind("Wind and Solar" = (wind_cap_ts + solar_cap_ts), "Natural Gas" = ng_cap_ts, "Coal" = coal_cap_ts)
window(cap_all, start = c(2012,1), end = c(2012,12))

autoplot(cap_all) + 
    xlab("Year") + ylab("Capacity(MW)") + ggtitle("Installed Capacity - Different Sources") + 
    xlim(c(1973,2020)) + guides(color = guide_legend(title = "Source"))

gen_ng <- monthly_dta_ts$Elec_by_Source[,"V4"]/(btu2MW*MWh2MW)
gen_rnw <- (monthly_dta_ts$Elec_by_Source[,"V12"] + monthly_dta_ts$Elec_by_Source[,"V13"])/(btu2MW*MWh2MW)

utl_ng <- gen_ng/ng_cap_ts
utl_rnw <- gen_rnw/(solar_cap_ts + wind_cap_ts)

autoplot(utl_ng, series = "Natural Gas") + autolayer(utl_rnw, series = "Wind + Solar", na.rm = T) + 
    xlab("Year") + ylab("Production/Capacity Ratio") + ggtitle("Utilization - Nat. Gas compared to Wind and Solar") + 
    guides(color = guide_legend(title = "Source"))


