
#####################
#my contribution is on the change in weight analysis

#kaggle link
#https://www.kaggle.com/code/macarenalacasa/capstone-case-study-bellabeat/notebook

install.packages(c("ggpubr", "tidyverse", "here", "skimr", "janitor", "lubridate", "ggrepel"))


library(ggpubr)
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)
library(ggrepel)

daily_activity <- read.csv("D:/mAYUR/coursera/capstone/data/dailyActivity_merged (1).csv")
daily_sleep <- read.csv("D:/mAYUR/coursera/capstone/data/sleepDay_merged.csv")
hourly_steps <- read.csv("D:/mAYUR/coursera/capstone/data/hourlySteps_merged (1).csv")


#Preview 
head(daily_activity)
str(daily_activity)

head(daily_sleep)
str(daily_sleep)

head(hourly_steps)
str(hourly_steps)

#Cleaning and formatting
n_unique(daily_activity$Id)
n_unique(daily_sleep$Id)
n_unique(hourly_steps$Id)



#DUPLICATES
sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_steps))

#NA
sum(is.na(daily_activity))
sum(is.na(daily_sleep))
sum(is.na(hourly_steps))


#Remove duplicates and N/A
daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()

daily_sleep <- daily_sleep %>%
  distinct() %>%
  drop_na()

hourly_steps <- hourly_steps %>%
  distinct() %>%
  drop_na()


#Clean and rename columns
clean_names(daily_activity)
daily_activity<- rename_with(daily_activity, tolower)
clean_names(daily_sleep)
daily_sleep <- rename_with(daily_sleep, tolower)
clean_names(hourly_steps)
hourly_steps <- rename_with(hourly_steps, tolower)

#Consistency of date and time columns
daily_activity <- daily_activity %>%
  rename(date = activitydate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>%
  rename(date = sleepday) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))



head(daily_activity)
head(daily_sleep)

hourly_steps<- hourly_steps %>% 
  rename(date_time = activityhour) %>% 
  mutate(date_time = as.POSIXct(date_time,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

head(hourly_steps)

#IMP
#merging them using both "id" and "date" ######## if we use only id for merging WE WILL GET TWO COLUMNS OF DATE AND PERHAPS DUPLICATE VALUES.
daily_activity_sleep <- merge(daily_activity, daily_sleep, by=c ("id", "date"))
glimpse(daily_activity_sleep)

head(daily_activity_sleep)
dim(daily_activity_sleep)


#Analyze Phase and Share Phase
daily_average <- daily_activity_sleep %>%
  group_by(id) %>%
  summarise (mean_daily_steps = mean(totalsteps), mean_daily_calories = mean(calories), mean_daily_sleep = mean(totalminutesasleep))

head(daily_average)

#####################################################
#5. Analyze Phase and Share Phase

daily_average <- daily_activity_sleep %>%
  group_by(id) %>%
  summarise (mean_daily_steps = mean(totalsteps), mean_daily_calories = mean(calories), mean_daily_sleep = mean(totalminutesasleep))

head(daily_average)


#Type of users per activity level
#since we don't have any demographic variables from our sample we want to determine the type of users with the data we have. We can classify the users by activity considering the daily amount of steps. We can categorize users as follows:

#Sedentary - Less than 5000 steps a day.
#Lightly active - Between 5000 and 7499 steps a day.
#Fairly active - Between 7500 and 9999 steps a day.
#Very active - More than 10000 steps a day.
daily_average <- daily_activity_sleep %>%
  group_by(id) %>%
  summarise (mean_daily_steps = mean(totalsteps), mean_daily_calories = mean(calories), mean_daily_sleep = mean(totalminutesasleep))



#
user_type <- daily_average %>%
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ "Sedentary",
    mean_daily_steps >= 5000 & mean_daily_steps <7499 ~ "Lightly Active",
    mean_daily_steps >= 7500 & mean_daily_steps < 9999  ~ "fairly active",
    mean_daily_steps >= 10000 ~ "very active",
    TRUE ~ "Other"
    
  )
    )


k = n_distinct(user_type$id)
#user_type_percent
user_type_percent <- user_type%>%
  group_by(user_type)%>%
  summarise(
    total = n(),
    total_percent = total/k)%>%
  mutate(labels = scales::percent(total_percent))

####################
#Below we can see that users are fairly distributed by their activity 
#considering the daily amount of steps. We can determine that based on
#users activity all kind of users wear smart-devices.
###########
user_type_percent %>%
  ggplot(aes(x="",y=total_percent, fill=user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
    scale_fill_manual(values = c("#85e085","#e6e600", "#ffd480", "#ff8080")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="User type distribution")


#5.2 Steps and minutes asleep per weekday 
#We want to know now what days of the week are the users more active and also 
#what days of the week users sleep more. We will also verify if the users walk 
#the recommended amount of steps and have the recommended amount of sleep.

#Below we are calculating the weekdays based on our column date. We are also 
#calculating the average steps walked and minutes sleeped by weekday.
weekday_steps_sleep <- daily_activity_sleep %>%
  mutate(weekday = weekdays(date))

weekday_steps_sleep$weekday <-ordered(weekday_steps_sleep$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                            "Friday", "Saturday", "Sunday"))

#using weekday_steps_sleep dataframe we can check if the user is following recommended distance and sleep or not.
weekday_steps_sleep <-weekday_steps_sleep%>%
  group_by(weekday) %>%
  summarize (daily_steps = mean(totalsteps), daily_sleep = mean(totalminutesasleep))

##########################################
#making bar plot for steps count and minutes slept
##############################
ggarrange(
  ggplot(weekday_steps_sleep) +
    geom_col(aes(weekday, daily_steps), fill = "#006699") +
    geom_hline(yintercept = 7500) +
    labs(title = "Daily steps per weekday", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1)),
  ggplot(weekday_steps_sleep, aes(weekday, daily_sleep)) +
    geom_col(fill = "#85e0e0") +
    geom_hline(yintercept = 480) +
    labs(title = "Minutes asleep per weekday", x= "", y = "") +
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
)

#################################################
#5.3 Hourly steps throughout the day ¶
#Getting deeper into our analysis we want to know when exactly are users more active in a day.
#We will use the hourly_steps data frame and separate date_time column.
################################################
hourly_steps1 <- hourly_steps %>%
  separate(date_time, into = c("date", "time"), sep= " ") %>%
  mutate(date = ymd(date)) 

head(hourly_steps1)

hourly_steps2<- hourly_steps1 %>%
  group_by(time) %>%
  summarize(average_steps = mean(steptotal)) 

ggplot(hourly_steps2) +
geom_col(mapping = aes(x=time, y = average_steps, fill = average_steps)) + 
labs(title = "Hourly steps throughout the day", x="", y="") + 
scale_fill_gradient(low = "green", high = "red")+
theme(axis.text.x = element_text(angle = 90))
#We can see that users are more active between 8am and 7pm. Walking more steps during lunch time from 12pm to 2pm and evenings from 5pm and 7pm.

##################################################################################
#5.4 Correlations
#We will now determine if there is any correlation between different variables:
#Daily steps and daily sleep
#Daily steps and calories
##########################################################################################
ggarrange(
  ggplot(daily_activity_sleep, aes(x=totalsteps, y=totalminutesasleep))+
    geom_jitter() +
    geom_smooth(color = "red") + 
    labs(title = "Daily steps vs Minutes asleep", x = "Daily steps", y= "Minutes asleep") +
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14)), 
  ggplot(daily_activity_sleep, aes(x=totalsteps, y=calories))+
    geom_jitter() +
    geom_smooth(color = "red") + 
    labs(title = "Daily steps vs Calories", x = "Daily steps", y= "Calories") +
    theme(panel.background = element_blank(),
          plot.title = element_text( size=14))
)

####################################################################
#5.5.1 Days used smart device ¶
#Now that we have seen some trends in activity, sleep and calories burned, we
#want to see how often do the users in our sample use their device. That way we can plan our marketing strategy and see what features would benefit the use of smart devices.
#We will calculate the number of users that use their smart device on a daily basis, classifying our sample into three categories knowing that the date interval is 31 days:
#high use - users who use their device between 21 and 31 days.
#moderate use - users who use their device between 10 and 20 days.
#low use - users who use their device between 1 and 10 days.
#First we will create a new data frame grouping by Id, calculating number of days used and creating a new column with the classification explained above.
##########################################################

daily_use <- daily_activity_sleep %>%
  group_by(id) %>%
  summarize(days_used=sum(n())) %>%
  mutate(usage = case_when(
    days_used >= 1 & days_used <= 10 ~ "low use",
    days_used >= 11 & days_used <= 20 ~ "moderate use", 
    days_used >= 21 & days_used <= 31 ~ "high use", 
  ))

k=n_distinct(daily_use$id)
daily_use_percent <- daily_use%>%
  group_by(usage)%>%
  summarise(
    count = n(),
    total_percent = count/k)%>%
  mutate(
    labels = scales::percent(total_percent)
  )

#5.5 Use of smart device 
#5.5.1 Days used smart device
#Now that we have seen some trends in activity, sleep and calories burned, we 
#want to see how often do the users in our sample use their device. That way we 
#can plan our marketing strategy and see what features would benefit the use of smart devices.

#We will calculate the number of users that use their smart device on a daily basis,
#classifying our sample into three categories knowing that the date interval is 31 days:
daily_use <- daily_activity_sleep %>%
  group_by(id) %>%
  summarize(days_used=sum(n())) %>%
  mutate(usage = case_when(
    days_used >= 1 & days_used <= 10 ~ "low use",
    days_used >= 11 & days_used <= 20 ~ "moderate use", 
    days_used >= 21 & days_used <= 31 ~ "high use", 
  ))

head(daily_use)

#Now that we have our new table we can create our plot:
daily_use_percent %>%
  ggplot(aes(x="",y=total_percent, fill=usage)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("#006633","#00e673","#80ffbf"),
                    labels = c("High use - 21 to 31 days",
                               "Moderate use - 11 to 20 days",
                               "Low use - 1 to 10 days"))+
  labs(title="Daily use of smart device")
###############

daily_use_merged <- merge(daily_activity, daily_use, by=c ("id"))
head(daily_use_merged)
###########
minutes_worn <- daily_use_merged %>% 
  mutate(total_minutes_worn = veryactiveminutes+fairlyactiveminutes+lightlyactiveminutes+sedentaryminutes)%>%
  mutate (percent_minutes_worn = (total_minutes_worn/1440)*100) %>%
  mutate (worn = case_when(
    percent_minutes_worn == 100 ~ "All day",
    percent_minutes_worn < 100 & percent_minutes_worn >= 50~ "More than half day", 
    percent_minutes_worn < 50 & percent_minutes_worn > 0 ~ "Less than half day"
  ))

head(minutes_worn)
########################
minutes_worn_percent<- minutes_worn%>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))


minutes_worn_highuse <- minutes_worn%>%
  filter (usage == "high use")%>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_moduse <- minutes_worn%>%
  filter(usage == "moderate use") %>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_lowuse <- minutes_worn%>%
  filter (usage == "low use") %>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_highuse$worn <- factor(minutes_worn_highuse$worn, levels = c("All day", "More than half day", "Less than half day"))
minutes_worn_percent$worn <- factor(minutes_worn_percent$worn, levels = c("All day", "More than half day", "Less than half day"))
minutes_worn_moduse$worn <- factor(minutes_worn_moduse$worn, levels = c("All day", "More than half day", "Less than half day"))
minutes_worn_lowuse$worn <- factor(minutes_worn_lowuse$worn, levels = c("All day", "More than half day", "Less than half day"))

head(minutes_worn_percent)
head(minutes_worn_highuse)
head(minutes_worn_moduse)
head(minutes_worn_lowuse)
####################################
ggarrange(
  ggplot(minutes_worn_percent, aes(x="",y=total_percent, fill=worn)) +
    geom_bar(stat = "identity", width = 1)+
    coord_polar("y", start=0)+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(), 
          panel.grid = element_blank(), 
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
    geom_text(aes(label = labels),
              position = position_stack(vjust = 0.5), size = 3.5)+
    labs(title="Time worn per day", subtitle = "Total Users"),
  ggarrange(
    ggplot(minutes_worn_highuse, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.position = "none")+
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text_repel(aes(label = labels),
                      position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "High use - Users"), 
    ggplot(minutes_worn_moduse, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none") +
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "Moderate use - Users"), 
    ggplot(minutes_worn_lowuse, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none") +
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "Low use - Users"), 
    ncol = 3), 
  nrow = 2)


#################################
#my contribution
weight_change <- read.csv("D:/mAYUR/coursera/capstone/data/weightLogInfo_merged.csv")

#no of duplicate rows
num_duplicated_rows <- sum(duplicated(weight_change))

# Remove duplicate rows
weight_change <- weight_change[!duplicated(weight_change), ]


#clean and standardize the column names of a dataframe.
clean_names(weight_change)
weight_change <- rename_with(weight_change, tolower)

#extracting date
library(dplyr)
library(lubridate)

# Convert the Date column to POSIXct format and extract the date part
Date1 = mdy_hms(weight_change$date)    # Convert to POSIXct format
weight_change <- weight_change %>%
  mutate(
    OnlyDate = as.Date(Date1)     # Extract only the date part
  )%>%
  select(id, OnlyDate, weightkg,bmi)

###############################################################heart rate
######################################################################
###############################################################
##########################################################################
#my contribution
heart_rate <- read.csv("D:/mAYUR/coursera/capstone/data/heartrate_seconds_merged.csv")

#no of duplicate rows
num_duplicated_rows <- sum(duplicated(heart_rate))

# Remove duplicate rows
heart_rate <- heart_rate[!duplicated(heart_rate), ]

#clean and standardize the column names of a dataframe.
clean_names(heart_rate)
heart_rate <- rename_with(heart_rate, tolower)

#extracting date
library(dplyr)
library(lubridate)

# Convert the Date column to POSIXct format and extract the date part
Date1 = mdy_hms(heart_rate$time)    # Convert to POSIXct format
heart_rate <- heart_rate %>%
  mutate(
    date = as.Date(Date1)     # Extract only the date part
  )%>%
  select(id, date,value)

################# merging with daily sleep

heart_rate <- heart_rate %>%
  left_join(daily_sleep, by = c("id" = "id", "date" = "date")) %>%
  group_by(id, date)


###########remove duplicates and null values

#DUPLICATES
sum(duplicated(heart_rate))

#NA
sum(is.na(heart_rate))

#
heart_rate <- heart_rate%>%
  distinct()%>%
  drop_na()

### finding mean values
#this shows there are 12 different ids along with their means
heart_rate_average <- heart_rate%>%
  group_by(id)%>%
  summarise(
    mean_value = mean(value),
    mean_totalminsasleep = mean(totalminutesasleep)
  )


################# merging with total steps a
daily_Activity_v1 <- daily_activity%>%
  select(id, date, totalsteps)

heart_rate_v1 <- heart_rate %>%
  left_join(daily_Activity_v1, by = c("id" = "id", "date" = "date")) %>%
  group_by(id, date)%>%
  select(id, date, value, totalsteps, totalminutesasleep)

#calculating mean value for heart rate
heart_rate_v2 <- heart_rate_v1%>%
  mutate(
    avg_value = mean(value)
  )%>%
  group_by(id, date)%>%
  select(id, date, value, totalsteps, totalminutesasleep, avg_value)
  
#checking duplicate
#DUPLICATES
sum(duplicated(heart_rate_v1))


#################we are not making categories because heart rate changes while exercising, resting, sleeping

#next we have to find correlation between the "heart rate and minutes slept" and the "heart rate and steps taken".

##################################################################################
# Correlations
#We will now determine if there is any correlation between different variables:
#"heart rate and minutes slept" and the "heart rate and steps taken".
##########################################################################################

# Scatter plot: Heart Rate vs Total Minutes Asleep
plot1 <- ggplot(heart_rate_v2, aes(x = avg_value, y = totalminutesasleep)) +
  geom_jitter() +
  geom_smooth(color = "red") +
  labs(title = "Heart Rate vs Minutes Asleep", x = "Heart Rate Value", y = "Minutes Asleep") +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 14))

# Displaying the plots separately
print(plot1)


# Scatter plot: Heart Rate vs Total Steps
plot2 <- ggplot(heart_rate_v2, aes(x = avg_value, y = totalsteps)) +
  geom_jitter() +
  geom_smooth(color = "red") +
  labs(title = "Heart Rate vs Total Steps", x = "Heart Rate Value", y = "Total Steps") +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 14))

# Displaying the plots separately
print(plot2)


###############################################################
#THE PLOTS show that there is a clear inverse relationship between heart rate and minutes asleep.
#the relationship between heart_rate and total_steps is not clear, but overall it appears that 
#the variables are directly proportional.
###############################################################
##then we can check on each day and on each hour how the heart rate is changing.

##########################################
#making bar plot for heart rate variation on weekdays
##############################
#Below we are calculating the weekdays based on our column date. We are also 
#calculating the average heart_rate by weekday.
weekday_heart_rate <- heart_rate_v2 %>%
  mutate(weekday = weekdays(date))

weekday_heart_rate$weekday <-ordered(weekday_heart_rate$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                            "Friday", "Saturday", "Sunday"))

#using weekday_steps_sleep dataframe we can monitor and check if they have a healthy heart rate or not,
#and make necessary lifestyle changes

weekday_heart_rate1 <-weekday_heart_rate%>%
  group_by(weekday) %>%
  summarize (avg_value_weekly = mean(avg_value))

#using the 90 beats per minute intercept the company can check and notify the 
#user about higher heart rate and recommend necessary lifestyle changes or 
#relaxation exercises
ggplot(weekday_heart_rate1) +
  geom_col(aes(weekday, avg_value_weekly), fill = "#006699") +
  geom_hline(yintercept = 90) +
  labs(title = "heart rate variation on weekdays", x= "", y = "") +
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1))
  

#how the heart rate changes throughout the day
################################################
# Convert the Date column to POSIXct format and extract the date part
heart_rate0 <- read.csv("D:/mAYUR/coursera/capstone/data/heartrate_seconds_merged.csv")

Date2 = mdy_hms(heart_rate0$Time)    # Convert to POSIXct format

heart_rate4 <- heart_rate0 %>%
  mutate(
    date = as.Date(Date2),    # Extract date part
    time = format(Date2, "%H:%M:%S"),  # Extract time part in HH:MM:SS format (optional)
    hour = hour(Date2)
  ) %>%
  select(Id, date, time, hour, Value) 


head(heart_rate4)

hourly_heart_rate<- heart_rate4 %>%
  group_by(hour) %>%
  summarize(average_heart_rate = mean(Value))

ggplot(hourly_heart_rate) +
  geom_col(mapping = aes(x=hour, y = average_heart_rate, fill = average_heart_rate)) + 
  labs(title = "Hourly heart rate throughout the day", x="", y="") + 
  scale_fill_gradient(low = "green", high = "red")+
  theme(axis.text.x = element_text(angle = 90))

#we can observe that the average heart rate is highest between 3 pm  and 8 pm.
#this information can be used by the company to deliver timed recommendations to regulate the heart rate.
