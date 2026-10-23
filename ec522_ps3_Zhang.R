library(tidyverse)
library(fpp3)
retail_raw = read_csv("data/retail_sales_raw.csv")


# 2.2a
# Format is in <date>

retail_raw = read_csv(
  "data/retail_sales_raw.csv",
  na = ".", 
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

# 2.3 Long Format
retail_raw_long = retail_raw |> 
  select(observation_date, RSAFS, RSAFSNA) |> 
  pivot_longer(cols = c(RSAFS, RSAFSNA), 
               names_to = "series_id",
               values_to = "sales")

head(retail_raw_long)

# 2.4 Build Your tsibble

#2.4a
retail_ts = retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |> 
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts)


 # 2.5 Canadian Gas Production 

#2.5a
head(canadian_gas)
key_vars(canadian_gas)
index_var(canadian_gas)

#2.5b
can_gas = canadian_gas |>
  select(Month, Volume)
has_gaps(can_gas)

head(can_gas)

#2.6 Working with Dates in tsibble

#2.6b Extracting parts of a date
can_gas_recent = can_gas |> 
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

#2.6b Extracting parts of a date
can_gas_recent = can_gas_recent |>
  mutate(year = year(Month),
         month = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |> 
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume)) |> 
  arrange(avg_volume)

# December had the highest average production,
# June had the lowest average production

# 2.6c Date Arithmetic
can_gas_recent |> 
  mutate(next_month = Month + 1,
         same_month_last_year = Month - 12) |> 
  head()

# 2.6d Changing the frequency of a series
can_gas_q = can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |> 
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# FRQ
# Why do we sum() the montly values for gas production 
# rather than take their mean()? Also, does the quarterly
# series still appear to be seasonal?

# Using sum allows us to take the total volume in a quarter,
# taking the mean diminishes significance in the quarter to 
# quarter evaluation. Generally for quarterly reports we want 
# to see growth over time.
# The data still seems to be seasonal.

# 2.7 Plotting with autoplot()
# 2.7a 
retail_ts |> autoplot(sales)

#2.7b 
retail_ts |> 
  autoplot(sales) + 
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. Not seasonally adjusted", 
    x = NULL, y = "Millions of dollars", color = NULL) + 
  theme_minimal()
# FRQ
# Look at the plot and compare the two lines. In a few sentences, describe what seasonal adjustment is visibly doing to the NSA series. In your answer, address the following:
#   
#   What repeating pattern within each year do you see in the NSA series (for example, a spike or a drop at the same time every year)? Does the SA series have this pattern?
#   Do the two series follow the same path over the long run, or does seasonal adjustment also change the overall trend and the larger ups and downs (such as recessions)?
#   

# 2.7c
can_gas |> autoplot(Volume) + 
  labs(
    title = "Canadian Gas Production Monthly Data",
    x = NULL, y = "Billions of cubic meters"
  ) + theme_minimal() + geom_smooth()

# 2.7d # FRQ
# The geom_smooth() function takes the data points and creates a curved 
# regression line over the data to reveall a trend. This allows us to visualize
# a trend line without the noise in the data.

# 3.1 Log Tranformation 
# FRQ
# The seasonal swings in the can_gas data are present, and trends with time, but the 
# swings are not consistent over the entire series. The data has higher voilitility between
# 1975 and 1990. The swings also become more frequent after 1995. 

can_gas= can_gas |> 
  mutate(log_volume = log(Volume))

can_gas |> autoplot(log_volume) + 
  labs(
    title = "Canadian Gas Production Monthly Data",
    x = NULL, y = "Log(Billions of cubic meters)"
  ) + theme_minimal() + geom_smooth()

# 3.1b FRQ
# Logging the volume of the gas production changes the shape from a slight downward dip
# to a more convex shape. the transformation changed the distribution but neither distribution 
# works well for linear modeling. The variation between data points did decrease however the 
# trending line doesn't capture the variation of the data.
# Logs do not represent a good fit for can_gas.

# 3.1c FRQ
# This series has a much better use case for logging the sales data. Unlike the Canadian 
# gas production with a curve, there is a cleaner linear trend present with the growth. 
retail_ts |> 
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales)) |> 
  autoplot(log_sales) + 
  labs(
    title = "U.S. Retail and Food Services Sales",
    x = NULL, y = "Log(Seasonally adjusted vs. Not seasonally adjusted)"
  ) + theme_minimal() + geom_smooth()

# 3.2 Growth Rates
# 3.2a Month over rmonth percentage growth 
can_gas = can_gas |> 
  mutate( 
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume))

can_gas |> autoplot(growth_direct)

can_gas = can_gas |> 
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |> autoplot(growth_gap)

# FRQ
# The growth_gap becomes a better approximation to visualizing the clustering 
# of volatility we saw in the untransformed data set.

# 3.3 SA vs NSA Growth Rates 
retail_ts = retail_ts |>
  group_by_key() |> 
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |> autoplot(growth_mom, alpha = 0.5) + 
  labs(
    title = "Month-over-Month Growth : SA vs. NSA Retail Sales",
    x = NULL, y = "Growth  rate (%)", color = NULL) + 
  theme_minimal()
# FRQ 
# The SA series has a smoother growth rate compared to the NSA series. The NSA has 
# larger spikes that more evenly distributed with an underlying clustering of lower
# rates. Connect this back to your answer in 2.7b

