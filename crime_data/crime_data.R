## we need to load basic packages----
install.packages(c('tidyverse','janitor','lubridate','skimr','DataExplorer','ggthemes','scales','broom','rmarkdown'))
install.packages('here')

## activating packages
library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(DataExplorer)
library(ggthemes)
library(scales)
library(broom)
library(rmarkdown)
library(here)

## loading package
crime_data <- read_csv("Crime_Data_from_2020_to_Present.csv")

#checking the data types----
summarise(crime_data)
head(crime_data)
skim(crime_data)

#cleaning col names with help of Janitor----
crime_data <- clean_names(crime_data)

#I want to showcase the entire summary of crime_data for better understanding
summary(crime_data)
crime_data_details <- skim(crime_data)

view(crime_data_details)
crime_data_details <- crime_data_details %>%
  select(
    skim_variable,
    skim_type,
    n_missing,
    complete_rate
  )

##Lets drop the rows where we don't have enough data----
crimes_data <- crime_data %>%
  select (vict_age,
          vict_sex,
          vict_descent,
          date_rptd,
          date_occ,
          time_occ,
          crm_cd,
          crm_cd_desc,
          area,
          area_name,
          rpt_dist_no,
          status,
          status_desc,
          premis_cd,
          premis_desc)

#lets remove NA cols
crimes_data <- crimes_data %>% drop_na()
View(crimes_data)

# we don't need dr_no
crimes_data <- crimes_data %>%
  select(-dr_no)

# lets split the date_occ and date_rprtd into date, month, year----
skim(tibble(mdy_hms(crimes_data$date_rptd)))

crimes_data$date_occ <- mdy_hms(crimes_data$date_occ)
crimes_data$date_rptd <- mdy_hms(crimes_data$date_rptd)

crimes_data$time_occ <- sub("(\\d{2})(\\d{2})","\\1:\\2",str_pad(crimes_data$time_occ,4,pad="0"))

#saving the file in .csv format so that even if R script crashes I have a hear start----
write.csv(crimes_data,"crimes_data.csv",row.names = FALSE)

crimes_data %>%
  #group_by(vict_age) %>%
  filter(vict_age <= 0) %>%
  summarise(n())

crimes_data %>%
  filter(vict_age < 0)

# we gonna remove values where vict_age is less than 0----
crimes_data <- crimes_data %>%
  filter(vict_age >= 0)

#visualizing data using ggplot----
c_m <- table(month(ymd(crimes_data$date_occ),label=TRUE))
c_y <- table(year(ymd(crimes_data$date_occ)))

cm <- data.frame(c_m)
cy <- data.frame(c_y)

#iteration_1
ggplot(cm,aes(x=Var1,y=mean(Freq),color=-mean(Freq),size=mean(Freq)))+
  geom_point()+
  labs(x='month',y='number_of_crimes',title = 'US_crime_data_monthly')+
  geom_text(aes(label=Freq),vjust=2.5)

#iteration_2
ggplot(cm,aes(x=Var1,y=Freq,fill=-Freq))+
  geom_col(position="dodge")+
  geom_text(aes(label=Freq),vjust=-0.5)



cmy <- crimes_data %>%
  select(date_occ,
         vict_age,
         vict_sex,
         status)

 
View(year(cmy$date_occ))

#lets unpack the date_occ into 3 cols----
 cmy <- cmy %>%
   mutate(
     year = year(date_occ),
     month = month(date_occ),
     date = day(date_occ)
   )
 
 #skipping year 2025 and its entire data
 cmy <- cmy %>%
   filter(year < 2025)
 
 #now need to plot a col plot using years and months
 ggplot(cmy,aes(x=year,y=mean(year)))+
   geom_point()
 
 dailycount <- cmy %>%
   group_by(year,month,date) %>%
   summarise(dailycount = n(), .groups = "drop")

 View(dailycount) 

 ggplot(dailycount,aes(x=factor(year),y=dailycount),color=month)+
   geom_col()+
   geom_text(aes(label=dailycount),vjust=-2.5)
 
 ggplot(cmy,aes(x=factor(year),y=n))+
   geom_col()

 cmy1 <- cmy %>%
   count(year,month)

 #finally a col plot showing monthly crime details in a facet fr 2020-24----
 ggplot(cmy1,aes(x=factor(year),y=n,fill=-n))+
   geom_col()+
   facet_wrap(~month)+
   geom_text(aes(label=n),vjust=-0.1)+
   labs(x="Year",y="no_of_crimes",title="Crime data from 2020 to 2024 in LA")+
   theme(plot.title = element_text(hjust=0.5))+
   theme(legend.position = "none")

