

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

setwd("/media/uarik/S2/Rstudio_projects/DataAir/") #sets your working directory to simplify calls to data ... make sure to use your OWN username instead of mine ;)
getwd() #displays your working directory


jobdata <- read_csv("ECOCITY_Archive_377_40_2019-02-20_2023-09-23.csv", col_names = FALSE)

#rename cols
colnames(jobdata)

jobdata <- rename(jobdata, station_ID=X1, coordinates = X2, date = X3, time = X4, num_of_mes = X5, indicator = X6, unit = X7, value = X8)
colnames(jobdata)

# Inspect the dataframes 
#str(jobdata)


temp_table <- jobdata %>%
  group_by(indicator, unit) %>%
  summarise(n = mean(value))

temp_table
#видно два глюки: NO₂ мірявся в ppm та ppb. більшість замірів в ppm, тому все в це й переводимо

jobdata <- jobdata %>%
  mutate(value = ifelse(indicator == 'NO₂' & unit == 'ppb', value * 1000, value)) %>%
  mutate(unit = ifelse(indicator == 'NO₂' & unit == 'ppb', 'ppm', unit))

#побідне з тиском. 

jobdata <- jobdata %>%
  mutate(value = ifelse(indicator == 'Pressure' & unit == 'hPa', value * 100, value)) %>%
  mutate(unit = ifelse(indicator == 'Pressure' & unit == 'hPa', 'Pa', unit))

temp_table <- jobdata %>%
  group_by(indicator, unit) %>%
  summarise(n = round(mean(value), 2), max = round(max(value), 2), min = round(min(value), 2), sd = round(sd(value), 2))

temp_table
#rename indicators
jobdata <- jobdata %>%
  mutate(indicator = ifelse(indicator == 'CO₂', 'CO2', indicator)) %>%
  mutate(indicator = ifelse(indicator == 'NH₃', 'NH3', indicator)) %>%
  mutate(indicator = ifelse(indicator == 'NO₂', 'NO2', indicator)) %>%
  mutate(indicator = ifelse(indicator == 'O₃', 'O3', indicator)) %>%
  mutate(indicator = ifelse(indicator == 'SO₂', 'SO2', indicator)) 


temp_table2 <- jobdata %>%
  group_by(indicator, unit) %>%
  summarise(n = mean(value))

temp_table2
# перед зведеною таблицею - перевірка, чи є дублі
jobdata %>%
  dplyr::group_by(station_ID, coordinates, date, time, indicator) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

jobdata %>%
  filter(station_ID == 8, date == "2019-09-07", time == "03", indicator == "PM10")


#piwot table

piw_tab <- jobdata %>% 
  pivot_wider(
    id_cols = c(station_ID, coordinates, date, time),
    names_from = indicator, 
    values_from = c(value, num_of_mes),
    values_fn =  mean,
    names_sep = "."
  )







#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
#colnames(jobdata)  #List of column names
#nrow(jobdata)  #How many rows are in data frame?
#dim(jobdata)  #Dimensions of the data frame?
#head(jobdata)  #See the first 6 rows of data frame.  Also tail(qs_raw)
#str(jobdata)  #See list of columns and data types (numeric, character, etc)
#summary(jobdata)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype
#table(jobdata$indicator)

# Reassign to the desired values (we will go with the current 2020 labels)
#jobdata <-  jobdata %>% 
#  mutate(member_casual = recode(member_casual
#                                ,"Subscriber" = "member"
#                                ,"Customer" = "casual"))



# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link

#jobdata$date <- as.Date(jobdata$started_at) #The default format is yyyy-mm-dd
jobdata$month <- format(as.Date(jobdata$date), "%m")
jobdata$day <- format(as.Date(jobdata$date), "%d")
jobdata$year <- format(as.Date(jobdata$date), "%Y")
jobdata$day_of_week <- format(as.Date(jobdata$date), "%A")

piw_tab$month <- format(as.Date(piw_tab$date), "%m")
piw_tab$day <- format(as.Date(piw_tab$date), "%d")
piw_tab$year <- format(as.Date(piw_tab$date), "%Y")
piw_tab$day_of_week <- format(as.Date(piw_tab$date), "%A")



# Add a "ride_length" calculation to jobdata (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
#jobdata$ride_length <- difftime(jobdata$ended_at,jobdata$started_at)

# Inspect the structure of the columns
#str(jobdata)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
#is.factor(jobdata$ride_length)
#jobdata$ride_length <- as.numeric(as.character(jobdata$ride_length))
#is.numeric(jobdata$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/

#витерди аномально низькі температури
#piw_tab <- piw_tab[!((piw_tab$value.Temperature < -34.1) | (piw_tab$value.Temperature > 37)),]
#витерди аномально високі температури
#jobdata_v2 <- jobdata_v2[!(jobdata_v2$indicator == "Temperature" & jobdata_v2$value > 37),]

#витерти невірні координати

#витерти аномальний тиск
jobdata |>
  filter(indicator == "Pressure") |> 
  group_by(unit) |> 
  summarize(maxs = max(value), mins = min(value))

#витерти аномальну вологість 

#jobdata_v2 <- jobdata_v2[!(jobdata_v2$indicator == "Temperature" & jobdata_v2$value > 37),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
#mean(jobdata_v2$ride_length) #straight average (total ride length / rides)
#median(jobdata_v2$ride_length) #midpoint number in the ascending array of ride lengths
#max(jobdata_v2$ride_length) #longest ride
#min(jobdata_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
#summary(jobdata_v2$ride_length)

temp_data <- jobdata_v2 |> filter(indicator == "Humidity")

ggplot(
  data = piw_tab,
  mapping = aes(x = month, y = value.PM10)
) +
  geom_point(aes(color = year)) +
  geom_smooth(method = "lm") #+
#  labs(
#    title = "Body mass and flipper length",
#    subtitle = "Розподіл температур",
#    x = "Дата", y = "С",
#    color = "температура"
#  ) 
#  scale_color_colorblind()

temp_data <- piw_tab |> filter(month == "01", year == "2023") |>   filter(value.Temperature > 12)


temp_data <- jobdata |> filter(is.na(station_ID))



temp_table <- piw_tab %>%
  group_by(station_ID) %>%
  summarise(avg = round(mean(value.Temperature), 2), 
            max = round(max(value.Temperature), 2), 
            min = round(min(value.Temperature), 2),
            sd = round(sd(value.Temperature), 2),
            n = n()
  )



ggplot(
  data = jobdata,
  mapping = aes(x = month, y = value) 
) + geom_point(aes(color = "red")) + facet_wrap( ~ indicator) +    geom_smooth(method = "lm")
#  geom_point(aes(color = "red")) +
#   geom_smooth(method = "lm")

ggplot(piw_tab, aes(value.Temperature, color = month)) +
  geom_density(position = "stack") 
#+  xlim(-0.5, 2.5)
#+  



bounds <- piw_tab %>%
  summarise(mean = mean(value.Humidity), sd = sd(value.Humidity)) %>%
  mutate(lower_bound = mean - 3*sd,
         upper_bound = mean + 3*sd)

ggplot(piw_tab, aes(x = value.Humidity)) + 
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) + 
  geom_vline(aes(xintercept = bounds$lower_bound), color = "red", linetype = "dashed") + 
  geom_vline(aes(xintercept = bounds$upper_bound), color = "red", linetype = "dashed")



bounds <- piw_tab %>%
  summarise(Q1 = quantile(value.Temperature, 0.25, na.rm = TRUE),
            Q3 = quantile(value.Temperature, 0.75, na.rm = TRUE)) %>%
  mutate(IQR = Q3 - Q1,
         lower_bound = Q1 - 1.5*IQR,
         upper_bound = Q3 + 1.5*IQR)

ggplot(piw_tab, aes(x = value.Temperature)) + 
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) + 
  geom_vline(aes(xintercept = bounds$lower_bound), color = "red", linetype = "dashed") + 
  geom_vline(aes(xintercept = bounds$upper_bound), color = "red", linetype = "dashed")


bounds <- piw_tab %>%
  summarise(Q1 = quantile(value.CO2, 0.25, na.rm = TRUE),
            Q3 = quantile(value.CO2, 0.75, na.rm = TRUE)) %>%
  mutate(IQR = Q3 - Q1,
         lower_bound = Q1 - 1.5*IQR,
         upper_bound = Q3 + 1.5*IQR)

ggplot(piw_tab, aes(x = value.CO2)) + 
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) + 
  geom_vline(aes(xintercept = bounds$lower_bound), color = "red", linetype = "dashed") + 
  geom_vline(aes(xintercept = bounds$upper_bound), color = "red", linetype = "dashed")




#ggplot(piw_tab, aes(value.NH3)) +
#  geom_density() + xlim(-0.01, 0.1)


#ggplot(piw_tab, aes(value.O3)) +
#  geom_density() #+ xlim(-0.01, 0.1)


# Compare members and casual users
#aggregate(jobdata_v2$ride_length ~ jobdata_v2$member_casual, FUN = mean)
#aggregate(jobdata_v2$ride_length ~ jobdata_v2$member_casual, FUN = median)
#aggregate(jobdata_v2$ride_length ~ jobdata_v2$member_casual, FUN = max)
#aggregate(jobdata_v2$ride_length ~ jobdata_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
#aggregate(jobdata_v2$ride_length ~ jobdata_v2$member_casual + jobdata_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
#jobdata_v2$day_of_week <- ordered(jobdata_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
#aggregate(jobdata_v2$ride_length ~ jobdata_v2$member_casual + jobdata_v2$day_of_week, FUN = mean)


#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
#counts <- aggregate(jobdata_v2$ride_length ~ jobdata_v2$member_casual + jobdata_v2$day_of_week, FUN = mean)
#write.csv(counts, file = '~/Desktop/Divvy_Exercise/avg_ride_length.csv')

#You're done! Congratulations!
for (val in temp_table$indicator) {
  # Отримання назви колонки
  column_name <- paste0("value.", val)
  
  bounds <- piw_tab %>%
    summarise(Q1 = quantile(piw_tab[[column_name]], 0.25, na.rm = TRUE),
              Q3 = quantile(piw_tab[[column_name]], 0.75, na.rm = TRUE)) %>%
    mutate(IQR = Q3 - Q1,
           lower_bound = Q1 - 1.5*IQR,
           upper_bound = Q3 + 1.5*IQR)
  
  ggplot(piw_tab, aes(x = piw_tab[[column_name]])) + 
    geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) + 
    geom_vline(aes(xintercept = bounds$lower_bound), color = "red", linetype = "dashed") + 
    geom_vline(aes(xintercept = bounds$upper_bound), color = "red", linetype = "dashed")
  
  bounds
} 
  
  # Передполагаємо, що наша операція - це множення значень на 2 (адаптуйте за потребами)
  data[[column_name]] <- data[[column_name]] * 2
  
  # Створення графіка
  plot <- ggplot(data, aes(x = date, y = data[[column_name]])) +
    geom_line() +
    labs(title = sensor, y = column_name)
  
  print(plot)
}
