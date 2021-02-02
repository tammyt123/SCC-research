##########################
#################  LIBRARY
##########################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c('data.table','dplyr', 'ggplot2','cowplot','ggpubr','tidyverse','magrittr','fs','stringr','latex2exp','viridis','rcartocolor','reader','here','tidyfast') ## you can add more packages here
lapply(packages, pkgTest)

##########################
################  PREAMBLE
##########################

## Set working directory
# local_path <- Sys.getenv("USERPROFILE")
# setwd(paste0(local_path,"\\Environmental Protection Agency (EPA)\\NCEE Social Cost of Carbon - General\\models\\mimi\\mimi_dev"))
setwd("C:\\Users\\TTAN\\Desktop\\SCC Research\\Misc")
# setwd(here())

##########################
####################  CODE
##########################

## Read in marginal damages
dice_co2 <- readRDS("dice_co2") 

# head(dice_co2, 15)
# summary(dice_co2)

## Change pulse_year to numeric type
dice_co2$pulse_year <- as.numeric(dice_co2$pulse_year)

## Take the average for each year, grouped by pulse year
average_md <- dice_co2 %>% 
    group_by(pulse_year, md_year) %>%
    summarise(average_md = mean(md))
# head(average_md)
# summary(average_md)

## Interpolate marginal damages -- calculate scc for pulse years, then linearly interpolate between pulse year sccs to get sccs for non-pulse years
scc_years_vector <- seq(2015, 2065, 10) # pulse years
scc_results <- data.frame(scc_year = scc_years_vector, scc = rep(0, length(scc_years_vector))) # create empty df to store results

for (scc_year in scc_years_vector) {

    horizon <- 2295 # end year
    annual_years <- scc_year:horizon

    md <- average_md %>%
        filter(pulse_year == scc_year) # year that scc is calculated from must match the year that the pulse is emitted
    
    # head(md)

    interpolated_md <- approx(md$md_year, md$average_md, xout = annual_years) # interpolate marginal damages across entire time horizon
    # head(interpolated_md)

    ## Calculate discount factors
    rate <- 0.025 # discount rate
    discount_factors <- rep(0, length(annual_years))

    for (i in 1:length(annual_years)) {
        discount_factors[i] <- 1/((1+rate)^(i-1))
    } # vector of discount factors

    ## Discount interpolated md
    scc <- sum(interpolated_md$y*discount_factors) # multiply marginal damages by respective discount factors, and take the sum to get SCC
    scc_results$scc[scc_results$scc_year == scc_year] <- scc # put into table
}

scc_results

## Compare to 2016 IWG DICE runs
dice_table <- as.data.frame(read_csv("dice_table.csv")) # table of DICE runs from summary document
dice_table %<>% spread(discount_rate,mean) # reshape long to wide
setnames(dice_table, c("year","5.0%","3.0%","2.5%"), c("Year","Average_5pct","Average_3pct","Average_2.5pct"))
setcolorder(dice_table,c("Year","Average_5pct","Average_3pct","Average_2.5pct"))

# scc_results
# dice_table

compare_sccs <- left_join(dice_table, scc_results, by = c("Year" = "scc_year"))
compare_sccs %>% select(c("Year", "Average_2.5pct", "scc"))

## Interpolate for non-pulse years
scc_interpolated <- approx(scc_results$scc_year, scc_results$scc, xout = compare_sccs$Year)
scc_interpolated <- as.data.frame(scc_interpolated)

compare_sccs <- left_join(dice_table, scc_interpolated, by = c("Year" = "x")) %>% 
    select(c("Year", "Average_2.5pct", "y")) %>%
compare_sccs


## Plot
plot(seq(2010, 2050, 5), dice_table[,"Average_2.5pct"], xlab = "Year", ylab = "SCC");
points(scc_results, col = 'blue', pch = 20);
lines(scc_results, col = 'blue'); # seems to diverge more for higher discount rates
legend(2010, 70, legend = c("IWG SCC estimate", "Interpolated SCC"), col = c("black", "blue"), lty = 1)

# ggplot(compare_sccs, aes(Year, y)) + 
#     geom_point() +
#     geom_path() +
#     geom_point(data = compare_sccs, aes(y = Average_2.5pct), colour = 'red')

##########################
################  OTHER ATTEMPTS
##########################

# Tried calculating the annual sccs directly (as the sum of the discounted annual mds), but this doesn't quite work for non-pulse years

scc_results <- data.frame(scc_year = seq(2015, 2050), scc = rep(0, length(seq(2015, 2050))))

for (scc_year in seq(2015, 2050)) {

    pulse_yr <- unique(average_md$pulse_year)[which.min(abs(unique(average_md$pulse_year) - (scc_year-4)))] # find the appropriate pulse year
    # pulse_yr
    horizon <- 2295
    annual_years <- scc_year:horizon

    md <- average_md %>%
        filter(pulse_year == pulse_yr)

    interpolated_md <- approx(md$md_year, md$average_md, xout = annual_years)
    # head(interpolated_md)

    ## Calculate discount factors
    rate <- 0.03

    discount_factors <- rep(0, length(annual_years))

    for (i in 1:length(annual_years)) {
        discount_factors[i] <- 1/((1+rate)^(i-1))
    }

    ## Discount interpolated md

    scc <- sum(interpolated_md$y*discount_factors)
    scc_results$scc[scc_results$scc_year == scc_year] <- scc
}

scc_results

## Compare to 2016 IWG DICE runs
dice_table <- as.data.frame(read_csv("dice_table.csv"))
dice_table %<>% spread(discount_rate,mean) # reshape long to wide
setnames(dice_table, c("year","5.0%","3.0%","2.5%"), c("Year","5% Average","3% Average","2.5% Average"))
setcolorder(dice_table,c("Year","5% Average","3% Average","2.5% Average"))

plot(seq(2010, 2050, 5), dice_table[,"3% Average"]);
points(scc_results, col = 'blue', pch = 20) # isn't matching up for non-pulse years...

# ---

## Interpolate md for given pulse year

scc_year <- 2050

# pulse_yr <- 2025 # must be member of unique(average_md$pulse_year)
pulse_yr <- unique(average_md$pulse_year)[which.min(abs(unique(average_md$pulse_year) - (scc_year-4)))] # find the appropriate pulse year
pulse_yr
horizon <- 2295
annual_years <- scc_year:horizon

md <- average_md %>%
    filter(pulse_year == pulse_yr)

interpolated_md <- approx(md$md_year, md$average_md, xout = annual_years)
# head(interpolated_md)

## Calculate discount factors
rate <- 0.03

discount_factors <- rep(0, length(annual_years))
for (i in 1:length(annual_years)) {
    discount_factors[i] <- 1/((1+rate)^(i-1))
}

## Discount interpolated md

scc <- sum(interpolated_md$y*discount_factors)
print(scc)


# ---

## Interpolate md for pulse year 2015
annual_years <- 2015:2295

md_2015 <- average_md %>% 
    filter(pulse_year == 2015)
md_2015

interpolated_md_2015 <- approx(md_2015$md_year, md_2015$average_md, xout = annual_years)
head(interpolated_md_2015)

## Calculate discount factors
rate <- 0.03

discount_factors <- rep(0, length(annual_years))
for (i in 1:length(annual_years)) {
    discount_factors[i] <- 1/((1+rate)^(i-1))
}

## Discount interpolated md

sum(interpolated_md_2015$y*discount_factors)


# ---

## Interpolate md for pulse year 2035
annual_years <- 2035:2295

md_2035 <- average_md %>% 
    filter(pulse_year == 2035)
# md_2035

interpolated_md_2035 <- approx(md_2035$md_year, md_2035$average_md, xout = annual_years)
head(interpolated_md_2035)

## Calculate discount factors
rate <- 0.03

discount_factors <- rep(0, length(annual_years))
for (i in 1:length(annual_years)) {
    discount_factors[i] <- 1/((1+rate)^(i-1))
}

## Discount interpolated md

sum(interpolated_md_2035$y*discount_factors)
