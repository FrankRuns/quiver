library(ggplot2) # for visualization
library(reshape2) # for visualization
library(dplyr) # for order_data aggregation
library(lubridate) # for dates

order_data <- read.csv("Historical Product Demand.csv", stringsAsFactors = FALSE)

#### Setup ####

# prelim information about the order_dataset
str(order_data) # columns and order_data types
dim(order_data) # number of rows and columns

# how many unique items are in each column?
for (lum in names(order_data)) {
  print(paste(lum, length(table(order_data[lum])), sep=": "))
}
# almost 5 years of order_data on 2160 products shipped from 4 warehouses

# top of the order_data
head(order_data)

#### Transforming Variables - Types ####

# transform order_data column into Date object
order_data$Date <- as.Date(order_data$Date, format = "%Y/%m/%d")

# turn the order demand variable from character to numeric
# order_data$Order_Demand <- as.numeric(order_data$Order_Demand) creates ~100 NA values which is odd
# the reason is that negative values (possibly returns) are recorded as '(1000)'
order_data$Order_Demand <- gsub("[(]", "-", order_data$Order_Demand)
order_data$Order_Demand <- gsub("[)]", "", order_data$Order_Demand)
order_data$Order_Demand <- as.numeric(order_data$Order_Demand)
# test to see it worked
order_data %>% filter(Product_Code == "Product_0031" & Warehouse == "Whse_A" & Product_Category == "Category_005" & Date == as.Date("2012-10-12"))

# create month, year, and day-of-week variables for order demand
order_data$Month <- month(order_data$Date)
order_data$Year <- year(order_data$Date)
order_data$Month_Year <- as.Date(paste(order_data$Year, "-", order_data$Month,"-01",sep=""))
order_data$DOW <- wday(order_data$Date)

# convert Order_Demand to numeric
order_data$Order_Demand <- as.numeric(order_data$Order_Demand)

#### Transforming Variables - Adding Week & Cube & Destination ####

# create week variable
order_data$The_Week <- strftime(order_data$Date, format = "%V")

# assume 1 order = 1 unit
# lets assign random cubic volume to product categories
set.seed(1234)
cubes <- data.frame(Product_Category=unique(order_data$Product_Category),
                    Cubic_Volume=rnorm(length(unique(order_data$Product_Category)), 2, 0.5))
order_data <- left_join(order_data, cubes, by=c("Product_Category"))

destinations <- paste0(rep("Dest", 8), "_", seq(1,8))
order_data$Dest <- sample(destinations, nrow(order_data), replace = TRUE)

# show me a random 5 rows of the order_dataset (sample_n from dplyr is awesome)
sample_n(order_data, 5)

#### Exploratory order_data Analysis - Missing & Incomplete order_data ####

a_count <- sapply(order_data, function(y) sum(length(which(is.na(y)))))
# there are 11239 dates with missing values

# subset df that have missing values
missing <- order_data %>% filter(is.na(Date))

# what warehouse(s) and product category(ies) are they in?
table(missing$Warehouse)
table(missing$Product_Category)
# all this missing date order_data is from Warehouse A and mostly on Product Cateogry 019

# what percent of orders are we talking about?
(order_data %>% filter(is.na(Date)) %>% summarise(sum(Order_Demand))) / sum(order_data$Order_Demand)
# that's smaller than a fraction of a percent... I'm OK ignoring that but would want to connect
# with the product teams (WMS team) to understand if there is additonal order_data that would help us
# figure out what happened with this Product Category in this warehouse that caused the dates to go
# missing. Moving on.

# remove those rows with missing dates
order_data <- order_data %>% filter(!is.na(Date))

# do the dates make sense?
table(order_data$Month_Year)
# we have some leading and lagging observations

# remove pre 2012 and post 2016 noise
order_data <- order_data %>% filter(Date > as.Date("2012-01-01") & Date < as.Date("2016-12-31"))

#### Exploratory order_data Analysis - Outliers ####

# basic summary
summary(order_data$Order_Demand)

upper_lim <- 0.99
lower_lim <- 0.01

wh_counts <- order_data %>% group_by(Warehouse) %>%
  summarise(total_orders = n(),
            sum_orders = sum(Order_Demand, na.rm = TRUE))

# understand what and how much is being removed
outs <- order_data %>% group_by(Warehouse, Product_Code) %>%
  mutate(upper = quantile(Order_Demand, upper_lim)) %>%
  mutate(lower = quantile(Order_Demand, lower_lim)) %>%
  mutate(take_out = ifelse(Order_Demand > upper | Order_Demand < lower, 1, 0)) %>%
  filter(take_out == 1) %>%
  group_by(Warehouse) %>%
  summarise(count_removed = n(),
            sum_removed = sum(Order_Demand, na.rm = TRUE),
            avg_remove = mean(Order_Demand, na.rm = TRUE)) %>%
  left_join(wh_counts, by="Warehouse") %>%
  mutate(pct_count_removed = count_removed / total_orders,
         pct_sum_removed = sum_removed / sum_orders)

# remove outliers - doing this by warehouse / product ID since scales are drastically different
# removing top and bottom 2% - be weary of this, understand what you are losing
# note that we are taking out ~10% of order volume!!
order_data <- order_data %>% group_by(Warehouse, Product_Code) %>%
  mutate(upper = quantile(Order_Demand, upper_lim)) %>%
  mutate(lower = quantile(Order_Demand, lower_lim)) %>%
  mutate(take_out = ifelse(Order_Demand > upper | Order_Demand < lower, 1, 0)) %>%
  filter(take_out == 0)

#### Exploratory order_data Analysis - Plotting Trends ####

# arrange order_data by date
order_data <- order_data %>% arrange(Product_Code, Warehouse, Date) %>% ungroup()

# we need to get a sense of trends - plots can help
overall_day <- order_data %>% group_by(Date) %>% summarise(orders=sum(Order_Demand)) # note, if I can keep it on one line I will
ggplot(overall_day, aes(x=Date, y=orders)) + geom_line() + # I keep this in because in the real world I find most of my initial plots look like this...
  labs(x="Date",y="Total Orders",title="Total Orders by Day") +
  theme_bw()
  
# plot time-series orders by month
by_month <- order_data %>%
  group_by(Month_Year) %>%
  summarise(orders=sum(Order_Demand, na.rm = TRUE))
ggplot(by_month, aes(x=Month_Year, y=orders)) +
  geom_line() +
  labs(x="Month_Year",y="Total Orders",title="Total Orders by Month") +
  theme_bw() +
  geom_smooth(size=0.2,se=FALSE,color='gray')

# plot time-series orders by month and warehouse
wh_by_month <- order_data %>%
  group_by(Warehouse, Month_Year) %>%
  summarise(orders=sum(Order_Demand, na.rm = TRUE))
ggplot(wh_by_month, aes(x=Month_Year, y=orders)) +
  geom_line() +
  facet_wrap(~Warehouse) +
  labs(x="Month_Year",y="Total Orders",title="Total Orders by Month") +
  theme_bw() +
  geom_smooth(size=0.2,se=FALSE,color='gray')

# find the coefficient of variation
cv_wh <- wh_by_month %>%
  group_by(Warehouse) %>%
  summarise(m = mean(orders),
            s = sd(orders),
            cv = s/m)

# inspect the dow curves
dow_chart <- order_data %>% group_by(Month_Year, DOW) %>% summarise(orders=sum(Order_Demand)) %>% mutate(pct=orders/sum(orders)) # note, if I can keep it on one line I will
# day of the week curves
ggplot(dow_chart, aes(x=DOW, y=pct, group=Month_Year)) + geom_line() +
  labs(x="Day of Week",y="Percent of Total Orders",title="Disribution of Orders Across Days of Week") +
  theme_bw()
ggplot(dow_chart %>% filter(!DOW %in% c(1,7)), aes(x=DOW, y=pct, group=Month_Year)) + geom_line() +
  labs(x="Day of Week",y="Percent of Total Orders",title="Disribution of Orders Across Days of Week (no weekends)") +
  theme_bw()

# day of the week boxplots
ggplot(dow_chart, aes(x=DOW, y=pct, group=DOW)) + geom_boxplot()  + labs(x="Day of Week",y="Percent of Total Orders",title="Disribution of Orders Across Days of Week") +
  theme_bw()

#### Setting up your framework ####

# date framework create function
date_framework <- function(start_date, weeks_to_forecast, cadence, f_horizon, p_horizon, yoy_comp_days) {
  
  # start_date = first date of forecast
  # end_date = last date of forecast
  # cadence = how frequently do you publish forecast? weekly would be 7 days
  # f_horizon = number of weeks you want to forecast for
  # p_horizon = number of weeks behind you'll use for comparable numbers
  # yoy_comp_days = calendar offset
  
  # create vector of dates on which you will publish forecast
  forecast_dates <- seq.Date(as.Date(start_date), as.Date(start_date) + (cadence * weeks_to_forecast), by=cadence) %>% as.data.frame()
  # create range for forecast dates
  names(forecast_dates) <- c("delivery")
  forecast_dates$target_start <- forecast_dates$delivery
  forecast_dates$target_end <- forecast_dates$target_start + ((7 * f_horizon) - 1)
  # create vectors of previous yard
  forecast_dates$comp_target_start <- forecast_dates$target_start - yoy_comp_days
  forecast_dates$comp_target_end <- forecast_dates$target_end - yoy_comp_days
  # 
  forecast_dates$comp_pre_delivery_start <- forecast_dates$delivery - (7 * p_horizon) # assuming prevous 8 weeks is reflective of current trend
  forecast_dates$comp_pre_delivery_end <- forecast_dates$delivery - (7 * 1) # to remove partial week of order_data
  # 
  forecast_dates$comp_pre_delivery_start_prev <- forecast_dates$comp_pre_delivery_start - yoy_comp_days
  forecast_dates$comp_pre_delivery_end_prev <- forecast_dates$comp_pre_delivery_end - yoy_comp_days
  # 
  forecast_dates <- arrange(forecast_dates, delivery)
  #
  return(forecast_dates)
}

forecast_horizon_weeks <- 10
previous_horizon_weeks <- 12

# create date framework
forecast_dates <- date_framework("2015-01-05", 104, 14, forecast_horizon_weeks, previous_horizon_weeks, 364)

#### Initial Benchmark ####

# create forecast using basic 4 week out method
bench1 <- order_data %>% group_by(Warehouse, Date) %>%
  summarise(Total_Orders = sum(Order_Demand)) %>%
  mutate(Wh_Dt = paste0(Warehouse, "_", Date)) %>%
  mutate(Base_Date = paste0(Warehouse, "_", (Date - 28)))

bench2 <- order_data %>% group_by(Warehouse, Date) %>%
  summarise(Forecast_Orders = sum(Order_Demand)) %>%
  mutate(Wh_Dt = paste0(Warehouse, "_", Date)) %>%
  select(Wh_Dt, Forecast_Orders) %>%
  ungroup(.)

benchmark <- left_join(bench1, bench2, by=c("Base_Date"="Wh_Dt")) %>% 
  select(Warehouse.x, Date, Total_Orders, Forecast_Orders) %>%
  ungroup()

# calculate overall accuracy of basic 4 week out method
overall_bm <- benchmark %>%
  filter(!is.na(Forecast_Orders)) %>%
  filter(Date >= forecast_dates$target_start[27] & Date <= forecast_dates$target_end[27]) %>%
  summarise(variance = (sum(Forecast_Orders, na.rm = TRUE) - sum(Total_Orders, na.rm = TRUE)) / sum(Total_Orders, na.rm = TRUE))

overall_wh_bm <- benchmark %>%
  filter(!is.na(Forecast_Orders)) %>%
  filter(Date >= forecast_dates$target_start[27] & Date <= forecast_dates$target_end[27]) %>%
  group_by(Warehouse.x) %>%
  summarise(variance = (sum(Forecast_Orders, na.rm = TRUE) - sum(Total_Orders, na.rm = TRUE)) / sum(Total_Orders, na.rm = TRUE))

# calculate MAPE of basic 4 week out method
mape_bm <- benchmark %>%
  filter(!is.na(Forecast_Orders)) %>%
  filter(Date >= forecast_dates$target_start[27] & Date <= forecast_dates$target_end[27]) %>%
  mutate(abs_variance = abs(Forecast_Orders - Total_Orders)) %>%
  mutate(pct_error = abs_variance / Forecast_Orders) %>%
  mutate(pct_error = ifelse(is.na(pct_error), 0, pct_error),
         pct_error = ifelse(is.infinite(pct_error), 1, pct_error)) %>%
  summarise(mape = mean(pct_error, na.rm = TRUE))

#### Initial Forecast - Forecast ####

# get expected year-over-year growth rate
recent_volume <- order_data %>%
  filter(Date >= forecast_dates$comp_pre_delivery_start[27] & Date <= forecast_dates$comp_pre_delivery_end[27]) %>%
  summarise(total_demand = sum(Order_Demand, na.rm = TRUE))
comp_recent_volume <- order_data %>%
  filter(Date >= forecast_dates$comp_pre_delivery_start_prev[27] & Date <= forecast_dates$comp_pre_delivery_end_prev[27]) %>%
  summarise(total_demand = sum(Order_Demand, na.rm = TRUE))
chg <- ((recent_volume - comp_recent_volume) / comp_recent_volume)$total_demand

# using expected growth rate, find total expected orders
total_volume <- order_data %>%
  filter(Date >= forecast_dates$comp_target_start[27] & Date <= forecast_dates$comp_target_end[27]) %>%
  summarise(total_demand = sum(Order_Demand))

expected_total_volume_a <- total_volume * (1+chg)

# find expected day-of-the-week curve
final_dow <- order_data %>%
  filter(Date >= forecast_dates$comp_pre_delivery_start[27] & Date <= forecast_dates$comp_pre_delivery_end[27]) %>%
  group_by(Warehouse, DOW) %>%
  summarise(orders=sum(Order_Demand)) %>%
  mutate(pct_orders=orders/sum(orders)) %>%
  select(Warehouse, DOW, pct_orders)

# find expected warehouse distribution
final_wh_dist <- order_data %>%
  filter(Date >= forecast_dates$comp_pre_delivery_start[27] & Date <= forecast_dates$comp_pre_delivery_end[27]) %>%
  group_by(Warehouse) %>%
  summarise(orders=sum(Order_Demand)) %>%
  mutate(pct_orders=orders/sum(orders))

# apply dow and warehouse distributions to expected volume, then apply to forecast period
warehouse_volume <- final_wh_dist %>%
  select(Warehouse, pct_orders) %>%
  mutate(total_volume = total_volume$total_demand) %>%
  mutate(warehouse_total = pct_orders * expected_total_volume_a$total_demand)

warehouse_volume$weekly_volume <- warehouse_volume$warehouse_total / forecast_horizon_weeks

warehouse_volume <- left_join(warehouse_volume, final_dow, by="Warehouse") %>%
  mutate(dow_volume = weekly_volume * pct_orders.y) %>%
  select(Warehouse, DOW, dow_volume) %>%
  rename("Fcast_Volume" = dow_volume)

forecast_range <- data.frame(Fcast_Date = seq.Date(forecast_dates$target_start[27], forecast_dates$target_end[27], "day"))
forecast_range$DOW <- wday(forecast_range$Fcast_Date)
warehouses <- data.frame(Warehouse = c("Whse_A", "Whse_C", "Whse_J", "Whse_S"))
forecast_range <- merge(forecast_range, warehouses)

# note - some dates don't exist in the comparable order_dataframe that we need to forecast for

f1 <- full_join(warehouse_volume, forecast_range, by=c("Warehouse", "DOW")) %>% arrange(Warehouse, Fcast_Date) %>%
  mutate(Fcast_Volume = ifelse(is.na(Fcast_Volume), 0, Fcast_Volume)) %>%
  select(Warehouse, Fcast_Date, DOW, Fcast_Volume)

# calculate expected year-over-year growth rate
expected_growth_rate <- (expected_total_volume_a - total_volume ) / total_volume

#### Initial Forecast - Confidence Intervals ####

# create daily forecast for 2015 for which we'll calculate our confidence intervals on our 2016 forecast

forecast_dates[forecast_dates$delivery == forecast_dates$delivery[27] - (previous_horizon_weeks*7),]

# based on recent growth trends, determine how much total volume will occur in the forecast period
recent_volume_cf <- order_data %>%
  filter(Date >= forecast_dates$comp_pre_delivery_start[21] & Date <= forecast_dates$comp_pre_delivery_end[21]) %>%
  summarise(total_demand = sum(Order_Demand, na.rm = TRUE))
comp_recent_volume_cf <- order_data %>%
  filter(Date >= forecast_dates$comp_pre_delivery_start_prev[21] & Date <= forecast_dates$comp_pre_delivery_end_prev[21]) %>%
  summarise(total_demand = sum(Order_Demand, na.rm = TRUE))
chg_cf <- ((recent_volume_cf - comp_recent_volume_cf) / comp_recent_volume_cf)$total_demand

total_volume_cf <- order_data %>%
  filter(Date >= forecast_dates$comp_target_start[21] & Date <= forecast_dates$comp_target_end[21]) %>%
  summarise(total_demand = sum(Order_Demand))

expected_total_volume_cf <- total_volume_cf * (1+chg_cf)

# get your expected dow curve
final_dow_cf <- order_data %>%
  filter(Date >= forecast_dates$comp_pre_delivery_start[21] & Date <= forecast_dates$comp_pre_delivery_end[21]) %>%
  group_by(Warehouse, DOW) %>%
  summarise(orders=sum(Order_Demand)) %>%
  mutate(pct_orders=orders/sum(orders)) %>%
  select(Warehouse, DOW, pct_orders)

# get your expected node distribution curve
final_wh_dist_cf <- order_data %>%
  filter(Date >= forecast_dates$comp_pre_delivery_start[21] & Date <= forecast_dates$comp_pre_delivery_end[21]) %>%
  group_by(Warehouse) %>%
  summarise(orders=sum(Order_Demand)) %>%
  mutate(pct_orders=orders/sum(orders))

# create df for overall volume by warehouse over period
warehouse_volume_cf <- final_wh_dist_cf %>%
  select(Warehouse, pct_orders) %>%
  mutate(total_volume = total_volume$total_demand) %>%
  mutate(warehouse_total = pct_orders * expected_total_volume_cf$total_demand)

warehouse_volume_cf$weekly_volume <- warehouse_volume_cf$warehouse_total / forecast_horizon_weeks

warehouse_volume_cf <- left_join(warehouse_volume_cf, final_dow_cf, by="Warehouse") %>%
  mutate(Fcast_Volume = weekly_volume * pct_orders.y) %>%
  select(Warehouse, DOW, Fcast_Volume)

# apply dow volumes by warehouse to date range of forecast
forecast_range_cf <- data.frame(Fcast_Date = seq.Date(forecast_dates$target_start[21], forecast_dates$target_end[21], "day"))
forecast_range_cf$DOW <- wday(forecast_range_cf$Fcast_Date)

# note - some dates don't exist in the comparable order_dataframe that we need to forecast for

f_conf <- full_join(warehouse_volume_cf, forecast_range_cf, by="DOW") %>%
  arrange(Warehouse, Fcast_Date) %>%
  select(Warehouse, Fcast_Date, DOW, Fcast_Volume)

# determine actuals during period against forecast, find sd of variance
acts_cf <- order_data %>% 
  filter(Date >= forecast_dates$target_start[21] & Date <= forecast_dates$target_end[21]) %>%
  group_by(Warehouse, Date) %>%
  summarise(Act_Orders = sum(Order_Demand)) %>%
  left_join(f_conf, by=c("Warehouse"="Warehouse", "Date"="Fcast_Date")) %>%
  mutate(Variance = Fcast_Volume - Act_Orders)

confid <- acts_cf %>% group_by(Warehouse, DOW) %>% summarise(vsd = sd(Variance))

#### Initial Forecast - Final Forecast ####

# 95% confidence
f1 <- full_join(f1, confid, by=c("Warehouse","DOW")) %>%
  mutate(Lower=qnorm(1-0.95, Fcast_Volume, vsd),
         Upper=qnorm(0.95, Fcast_Volume, vsd)) %>%
  select(-vsd) %>%
  mutate(Lower=ifelse(Lower<0,0,Lower))

# log forecast and forecas assumption files
# write.csv(f1, paste0("forecast_", Sys.Date(), ".csv"), row.names=FALSE)
# write.csv(final_dow, paste0("forecast_", Sys.Date(), "_dow-curve.csv"), row.names=FALSE)
# write.csv(final_wh_dist, paste0("forecast_", Sys.Date(), "_wh-dist.csv"), row.names=FALSE)

#### Stakeholder Charts for Stakeholders ####

acts <- order_data %>% 
  filter(Date >= forecast_dates$comp_pre_delivery_start[27] & Date <= forecast_dates$target_end[27]) %>%
  group_by(Warehouse, Date) %>%
  summarise(Act_Orders = sum(Order_Demand, na.rm = TRUE)) %>%
  full_join(f1, by=c("Warehouse"="Warehouse", "Date"="Fcast_Date")) %>%
  arrange(Warehouse, Date) %>%
  mutate(Act_Orders = ifelse(is.na(Act_Orders), 0, Act_Orders),
         Lower = ifelse(is.na(Lower), 0, Lower),
         Upper = ifelse(is.na(Upper), 0, Upper)) %>%
  mutate(Variance = Fcast_Volume - Act_Orders,
         Var_Pct = Variance / Act_Orders,
         Abs_Var_Pct = abs(Var_Pct)) %>%
  mutate(Abs_Var_Pct = ifelse(is.na(Abs_Var_Pct), 0, Abs_Var_Pct),
         Abs_Var_Pct = ifelse(is.infinite(Abs_Var_Pct), 1, Abs_Var_Pct))

m_acts <- melt(acts,
               id.vars=c("Warehouse","Date"),
               measure.vars=c("Act_Orders", "Fcast_Volume", "Lower", "Upper"))

# before we add in the actuals, this is the chart you would show 
# your stakeholders to communicate your expectations

deliv_date <- forecast_dates[27,]$delivery
total_forecast <- m_acts %>% filter(Date == deliv_date & variable == "Fcast_Volume") %>% summarise(total=sum(value))

# stake_chart <- m_acts %>% filter(variable != "act_orders") %>% group_by(Date, variable) %>% summarise(orders = sum(value, na.rm = TRUE)) 
stake_chart <- m_acts %>% group_by(Date, variable) %>% summarise(orders = sum(value, na.rm = TRUE)) 
stake_chart$orders <- ifelse(stake_chart$Date > as.Date("2016-01-04") & stake_chart$variable == "Act_Orders", NA, stake_chart$orders)
stake_chart$orders <- ifelse(stake_chart$Date == as.Date("2016-01-04") & stake_chart$variable == "Act_Orders", total_forecast$total, stake_chart$orders)
stake_chart$orders <- ifelse(stake_chart$Date < as.Date("2016-01-04") & stake_chart$variable != "Act_Orders", NA, stake_chart$orders)
# ugly
ggplot(stake_chart, 
       aes(x=Date, y=orders, group=variable, color=variable, linetype=variable)) + geom_line() +
  # scale_shape_manual(values=c(6,5)) +
  labs(x="Date",y="Total Orders",title="Total Forecasted Orders by Day") +
  theme_bw() +
  scale_color_manual(values=c("deep pink","deep pink","steelblue","steelblue")) +
  scale_linetype_manual(values=c(1,2,3,3))
# remove weekends
ggplot(stake_chart %>% filter(!wday(Date) %in% c(1,7)), 
       aes(x=Date, y=orders, group=variable, color=variable, linetype=variable)) + geom_line() +
  labs(x="Date",y="Total Orders",title="Total Forecasted Orders by Day (Exclude Weekends)") +
  geom_vline(xintercept = as.Date(deliv_date), color="gray") +
  theme_bw() +
  scale_color_manual(values=c("deep pink","deep pink","steelblue","steelblue")) +
  scale_linetype_manual(values=c(1,2,3,3))
# this should generate a lively S&OP discussion!
# if you are planning labor for the warehouse and your SLA > 7 days, you should consider planning at the week level
# if you are planning outbound trucks (which will depart everyday), you should consider creating your daily forecast from orders completed (which will likely be smoother than incoming orders)

# what about by warehouse?

wh_total_forecast <- m_acts %>% filter(Date == deliv_date & variable == "Fcast_Volume") %>%
  group_by(Warehouse) %>%
  summarise(total=sum(value))

# wh_stake_chart <- m_acts %>% filter(variable != "act_orders") %>% group_by(Warehouse, Date, variable) %>% summarise(orders = sum(value, na.rm = TRUE)) 
wh_stake_chart <- m_acts %>% group_by(Warehouse, Date, variable) %>% summarise(orders = sum(value, na.rm = TRUE)) 
wh_stake_chart$orders <- ifelse(wh_stake_chart$Date > as.Date("2016-01-04") & wh_stake_chart$variable == "Act_Orders", NA, wh_stake_chart$orders)
wh_stake_chart$orders <- ifelse(wh_stake_chart$Date == as.Date("2016-01-04") &
                                  wh_stake_chart$variable == "Act_Orders" &
                                  wh_stake_chart$Warehouse == "Whse_A", wh_total_forecast[wh_total_forecast$Warehouse == "Whse_A",]$total, wh_stake_chart$orders)
wh_stake_chart$orders <- ifelse(wh_stake_chart$Date == as.Date("2016-01-04") &
                                  wh_stake_chart$variable == "Act_Orders" &
                                  wh_stake_chart$Warehouse == "Whse_C", wh_total_forecast[wh_total_forecast$Warehouse == "Whse_C",]$total, wh_stake_chart$orders)
wh_stake_chart$orders <- ifelse(wh_stake_chart$Date == as.Date("2016-01-04") &
                                  wh_stake_chart$variable == "Act_Orders" &
                                  wh_stake_chart$Warehouse == "Whse_J", wh_total_forecast[wh_total_forecast$Warehouse == "Whse_J",]$total, wh_stake_chart$orders)
wh_stake_chart$orders <- ifelse(wh_stake_chart$Date == as.Date("2016-01-04") &
                                  wh_stake_chart$variable == "Act_Orders" &
                                  wh_stake_chart$Warehouse == "Whse_S", wh_total_forecast[wh_total_forecast$Warehouse == "Whse_S",]$total, wh_stake_chart$orders)
wh_stake_chart$orders <- ifelse(wh_stake_chart$Date < as.Date("2016-01-04") & wh_stake_chart$variable != "Act_Orders", NA, wh_stake_chart$orders)
# ugly
ggplot(wh_stake_chart, 
       aes(x=Date, y=orders, group=variable, color=variable, linetype=variable)) + geom_line() + facet_wrap(~Warehouse) +
  labs(x="Date",y="Total Orders",title="Total Forecasted Orders by Day & Warehouse") +
  geom_vline(xintercept = as.Date(deliv_date), color="gray") +
  theme_bw() +
  scale_color_manual(values=c("deep pink","deep pink","steelblue","steelblue")) +
  scale_linetype_manual(values=c(1,2,3,3))
# remove weekends
ggplot(wh_stake_chart %>% filter(!wday(Date) %in% c(1,7)), 
       aes(x=Date, y=orders, group=variable, color=variable, linetype=variable)) + geom_line() + facet_wrap(~Warehouse) +
  labs(x="Date",y="Total Orders",title="Total Forecasted Orders by Day & Warehouse (Exclude Weekends)") +
  geom_vline(xintercept = as.Date(deliv_date), color="gray") +
  theme_bw() +
  scale_color_manual(values=c("deep pink","deep pink","steelblue","steelblue")) +
  scale_linetype_manual(values=c(1,2,3,3))

# as you go through time you could add in the act_orders line
# assume it's end of February and you haven't changed the forecast

# stake_chart <- m_acts %>% filter(variable != "act_orders") %>% group_by(Date, variable) %>% summarise(orders = sum(value, na.rm = TRUE)) 
stake_chart_2 <- m_acts %>% group_by(Date, variable) %>% summarise(orders = sum(value, na.rm = TRUE)) 
stake_chart_2$orders <- ifelse(stake_chart_2$Date > as.Date("2016-02-20") & stake_chart_2$variable == "Act_Orders", NA, stake_chart_2$orders)
stake_chart_2$orders <- ifelse(stake_chart_2$Date < as.Date("2016-01-04") & stake_chart_2$variable != "Act_Orders", NA, stake_chart_2$orders)
# remove weekends
ggplot(stake_chart_2 %>% filter(!wday(Date) %in% c(1,7)), 
       aes(x=Date, y=orders, group=variable, color=variable, linetype=variable)) + geom_line() +
  labs(x="Date",y="Total Orders",title="Total Forecasted Orders by Day (Exclude Weekends)") +
  geom_vline(xintercept = as.Date("2016-02-20"), color="gray") +
  theme_bw() +
  scale_color_manual(values=c("black","deep pink","steelblue","steelblue")) +
  scale_linetype_manual(values=c(1,2,3,3))

#### Forecast Accuracy - Base order_dataset ####

acts <- order_data %>% 
  filter(Date >= forecast_dates$target_start[27] & Date <= forecast_dates$target_end[27]) %>%
  group_by(Warehouse, Date) %>%
  summarise(act_orders = sum(Order_Demand, na.rm = TRUE)) %>%
  full_join(f1, by=c("Warehouse"="Warehouse", "Date"="Fcast_Date")) %>%
  arrange(Warehouse, Date) %>%
  mutate(act_orders = ifelse(is.na(act_orders), 0, act_orders),
         Lower = ifelse(is.na(Lower), 0, Lower),
         Upper = ifelse(is.na(Upper), 0, Upper)) %>%
  mutate(variance = Fcast_Volume - act_orders,
         var_pct = variance / act_orders,
         abs_var_pct = abs(var_pct)) %>%
  mutate(abs_var_pct = ifelse(is.na(abs_var_pct), 0, abs_var_pct),
         abs_var_pct = ifelse(is.infinite(abs_var_pct), 1, abs_var_pct))

# create a week variable to inspect accuracy by week
acts$The_Week <- strftime(acts$Date, format = "%V")

m_acts <- melt(acts,
               id.vars=c("Warehouse","Date"),
               measure.vars=c("act_orders", "Fcast_Volume", "Lower", "Upper"))
#### Forecast Accuracy - Overall Error ####

# what was growth year over year?
actual_volume <- order_data %>%
  filter(Date >= forecast_dates$target_start[27] & Date <= forecast_dates$target_end[27]) %>%
  summarise(total_demand = sum(Order_Demand, na.rm = TRUE))
comp_actual_volume <- order_data %>%
  filter(Date >= forecast_dates$comp_target_start[27] & Date <= forecast_dates$comp_target_end[27]) %>%
  summarise(total_demand = sum(Order_Demand, na.rm = TRUE))
actual_chg <- ((actual_volume - comp_actual_volume) / comp_actual_volume)$total_demand

# overall error
forecast_error <- (sum(acts$Fcast_Volume, na.rm = TRUE) - sum(acts$act_orders, na.rm = TRUE) ) / comp_actual_volume

# overall error by warehouse
wh_var <- acts %>% group_by(Warehouse) %>%
  summarise(sum_variance = sum(Fcast_Volume, na.rm = TRUE) - sum(act_orders, na.rm = TRUE)) %>%
  mutate(pct_variance = sum_variance / comp_actual_volume$total_demand)

# overall error by week
lag_var <- acts %>% group_by(The_Week) %>%
  summarise(sum_variance = sum(Fcast_Volume, na.rm = TRUE) - sum(act_orders, na.rm = TRUE)) %>%
  mutate(pct_variance = sum_variance / comp_actual_volume$total_demand)

#### Forecast Accuracy - MAPE ####

# overall MAPE
mean(acts$abs_var_pct, na.rm = TRUE)

# MAPE by warehouse
wh_mape <- acts %>% 
  group_by(Warehouse) %>%
  summarise(MAPE = mean(abs_var_pct))

# MAPE by week
lag_mape <- acts %>% 
  group_by(The_Week) %>%
  summarise(MAPE = mean(abs_var_pct))

# the truth is, there's no foundation on which to forecast daily
# perhaps this operation doesn't require a daily forecast, what if weekly was sufficient
# at a weekly level, how much better is the MAPE?

# aggregate by week
week_acts <- acts %>% group_by(The_Week) %>%
  summarise(act_orders = sum(act_orders),
            Fcast_Volume = sum(Fcast_Volume)) %>%
  mutate(variance = Fcast_Volume - act_orders,
       var_pct = variance / act_orders,
       abs_var_pct = abs(var_pct)) %>%
  mutate(abs_var_pct = ifelse(is.na(abs_var_pct), 0, abs_var_pct),
         abs_var_pct = ifelse(is.infinite(abs_var_pct), 1, abs_var_pct))

# week-level overall MAPE
mean(week_acts$abs_var_pct, na.rm = TRUE)
# planning at the week level seems much more feasible

#### Inspect Input Assumptions - Warehouse Distribution ####

# here's the actual distribution
act_wh_dist <- acts %>%
  group_by(Warehouse) %>%
  summarise(v=sum(act_orders, na.rm = TRUE)) %>%
  mutate(pct_v=v/sum(v)) %>%
  left_join(final_wh_dist, act_wh_dist, by="Warehouse") %>%
  mutate(comp = pct_v - pct_orders) %>%
  select(Warehouse, pct_orders, pct_v, comp) %>%
  rename('Warehouse'=Warehouse,'Actual'=pct_orders,'Expected'=pct_v,'Actual_Less_Expected'=comp)
# much more went to Warehouse J than expected, much less went to Warehouse S than expected

#### Inspect Input Assumptions - Day-of-Week Distribution ####

# dow curve look as expected?

actual_dow <- acts %>%
  mutate(dow = wday(Date)) %>%
  group_by(Warehouse, DOW) %>%
  summarise(orders=sum(act_orders, na.rm = TRUE)) %>%
  mutate(pct_orders=orders/sum(orders)) %>%
  select(Warehouse, DOW, pct_orders)

dow_variance <- left_join(final_dow, actual_dow, by=c("Warehouse", "DOW")) %>% mutate(dow_variance = pct_orders.y - pct_orders.x)

#### Inspect Input Assumptions - Product Mix ####

# did the mix of products order change significantly? - we need to look at this. an intrinsic assumption was that product order mix remained the same.
# use a 45-degree line scatter plot to compare product mix for the 33 product categories...
actual_mix <- order_data %>%
  filter(Date >= forecast_dates$target_start[27] & Date <= forecast_dates$target_end[27]) %>%
  group_by(Product_Category) %>%
  summarise(total_demand = sum(Order_Demand)) %>%
  mutate(actual_pct = total_demand/sum(total_demand)) %>%
  select(Product_Category, actual_pct)
expected_mix <- order_data %>%
  filter(Date >= forecast_dates$comp_pre_delivery_start[27] & Date <= forecast_dates$comp_pre_delivery_end[27]) %>%
  group_by(Product_Category) %>%
  summarise(total_demand = sum(Order_Demand)) %>%
  mutate(expected_pct = total_demand/sum(total_demand)) %>%
  select(Product_Category, expected_pct)
mix <- full_join(actual_mix, expected_mix, by="Product_Category")
ggplot(mix, aes(x=actual_pct, y=expected_pct)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0.02, slope = 1, color = "blue") +
  geom_abline(intercept = -0.02, slope = 1, color = "blue") +
  labs(x="Actual Mix",y="Expected Mix",title="Product Mix") +
  theme_bw()
