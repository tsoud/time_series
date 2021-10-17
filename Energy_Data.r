library(tseries)
library(TSA)
library(fpp)
library(fpp2)
library(ggplot2)
library(forecast)

path <- paste(getwd(), "/Data/", sep = "")
files <- dir(path, pattern = ".csv")
# Drop 1st and last file to deal with later
files <- files[-1]

files

energy_data <- list()
metadata <- list()
i <- 1

for (file in files) {
    # Read the header
    header <- read.csv(paste(path, file, sep = ""), skip = 10, nrows = 1, header = F, as.is = T)
    # Find the units
    units <- read.csv(paste(path, file, sep = ""), skip = 11, nrows = 1, header = F, as.is = T)
    # Read the data
    dta <- read.csv(paste(path, file, sep = ""), skip = 12, header = F, check.names = T, blank.lines.skip = T, na.strings = "Not Available")

    # Make sure values are numeric
    dta[,2:ncol(dta)] <- sapply(dta[,2:ncol(dta)], function(x) trimws(x))  # get rid of whitespace
    suppressWarnings(dta[,2:ncol(dta)] <- sapply(dta[,2:ncol(dta)], function(x) as.numeric(x)))
    # Also substitute negative values with zeros
    dta[,2:ncol(dta)] <- sapply(dta[,2:ncol(dta)], function(x) (abs(x) + x)/2)
    # If energy units are quadrillions convert to trillions                            
    dta[,which(units == '(Quadrillion Btu)')] <- sapply(dta[,which(units == '(Quadrillion Btu)')], function(x) 1000*x)                               
    units[which(units == '(Quadrillion Btu)')] <- '(Trillion Btu)'  # update metadata                        

    # Assign to the list
    energy_data[[i]] <- dta
    metadata[[i]] <- rbind(header, units)
    i <- i+1
}

set_names <- c("Prod_by_Source", 
               "Consump_by_Source", 
               "Expenditures_Emissions_Indicators", 
               "Renewable_by_Source", 
               "CO2_by_Source", 
               "CO2_ElecPower", 
               "Consump_by_Sector", 
               "ElectricityOverview", 
               "NGPrices", 
               "Avg_Elec_Price", 
               "Fossil_Fuel_Costs", 
               "GDP_Pop_Info")

                                
names(energy_data) <- set_names
names(metadata) <- set_names

rbind(metadata$Renewable_by_Source, head(energy_data$Renewable_by_Source))

rbind(metadata$Expenditures_Emissions_Indicators, energy_data$Expenditures_Emissions_Indicators[30:35,])

rbind(metadata$GDP_Pop_Info, tail(energy_data$GDP_Pop_Info))

IL_data <- read.csv("Data/generation_by_state.csv", header = T, strip.white = T)
head(IL_data)

IL_data <- IL_data[IL_data$STATE == "IL",]
head(IL_data, 15)

IL_data <- aggregate(GENERATION..Megawatthours. ~  MONTH + YEAR + ENERGY.SOURCE, IL_data, sum)
colnames(IL_data) <- c("Month", "Year", "Source", "Output_MWh")
IL_data <- IL_data[, c(2,1,3,4)]  # flip year, month
head(IL_data, 10)

IL_wind <- IL_data[IL_data$Source == 'Wind',]
rownames(IL_wind) <- 1:nrow(IL_wind)
head(IL_wind)

IL_solar <- IL_data[IL_data$Source == 'Solar Thermal and Photovoltaic',]
rownames(IL_solar) <- 1:nrow(IL_solar)
head(IL_solar)

yearly_dta <- energy_data[c('Expenditures_Emissions_Indicators', 'GDP_Pop_Info')]
yearly_meta <- metadata[c('Expenditures_Emissions_Indicators', 'GDP_Pop_Info')]

monthly_dta <- energy_data[setdiff(names(energy_data), names(yearly_dta))]
monthly_meta <- metadata[setdiff(names(energy_data), names(yearly_dta))]

yearly_dta_ts <- lapply(yearly_dta, function(x) ts(x[2:ncol(x)], start = x[1,1], frequency = 1))

head(yearly_dta_ts$GDP_Pop_Info, 10)

monthly_dta_ts <- lapply(monthly_dta, 
                         function(x) { ts(x[2:ncol(x)],
                         start = c(as.integer(regmatches(x[1,1], regexpr("[0-9]{4}", x[1,1]))), 1),
                         frequency = 12) })

head(monthly_dta_ts$CO2_by_Source, 12)

IL_wind_ts <- ts(IL_wind[,'Output_MWh'], start = c(IL_wind[1,1], 1), frequency = 12)
IL_solar_ts <- ts(IL_solar[,'Output_MWh'], start = c(IL_solar[1,1], 1), frequency = 12)

options(repr.plot.width = 11, repr.plot.height = 5)

monthly_meta$Prod_by_Source

autoplot(cbind("Fossil Fuels"  = monthly_dta_ts$Prod_by_Source[,"V6"], 
               "Renewables" = monthly_dta_ts$Prod_by_Source[,"V13"])) + 
                xlab("Year") + ylab("Production (Trillion Btu)") + guides(color = guide_legend(title = "Energy Source"))

autoplot(cbind("Wind"  = IL_wind_ts, "Solar" = IL_solar_ts)) + 
                xlab("Year") + ylab("Production (MWh)") + ggtitle("Wind and Solar in Illinois") +
                guides(color = guide_legend(title = "Energy Source"))

save(monthly_dta, monthly_meta, file = "Monthly_Data.RData")
save(yearly_dta, yearly_meta, file = "Yearly_Data.RData")
save(IL_data, IL_solar, IL_wind, file = "IL_Data.RData")
save(monthly_dta_ts, yearly_dta_ts, file = "Time_Series.RData")

