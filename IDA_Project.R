# Loading Packages

library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)

## Reading the Data

data <- read_csv("IDA_Credits_and_Grants.csv")

## Sorting the dataset with Credit Number
## to check for the duplicates

data <- data %>% 
  arrange(`Credit Number`)

## Data Exploration

skim(data)

# Get summary of missing values
skim(data) %>% filter(complete_rate < 1)

# Data Cleaning

# Removing columns

data<- data %>% 
  select(! c(`End of Period`, `Country Code`, `Project ID`, Borrower,`First Repayment Date`,`Last Repayment Date`, `Agreement Signing Date`,`Board Approval Date`,
            `Effective Date (Most Recent)`,`Service Charge Rate`, `Last Disbursement Date`))

# Removing three missing values in `Closed Date (Most Recent)` Column

data <- data[!is.na(data$'Closed Date (Most Recent)'), ]

# Changing Data Types
# Closed Date and Last Disbursement should be stored as dates not characters

data$`Closed Date (Most Recent)`<- as.POSIXct(data$`Closed Date (Most Recent)`, 
                                              format = "%m/%d/%Y %I:%M:%S %p")

# Renaming the columns into proper format

data <- clean_names(data)

# Feature Engineering

data$year <- year(data$closed_date_most_recent) # creating the year column

# Further Cleaning and reshaping

data$disbursed_amount <- data$disbursed_amount/1000000 ## the column should be in millions.

data <- data[!is.na(data$disbursed_amount),] # removing the null values 
                                             # in disbursed_amount column

data <- data %>% 
  rename(disbursed_amount_million_usd = disbursed_amount) # renaming the column



         # --- Analysis ---


# The amount that has been disbursed by IDA since 1963

sum(data$disbursed_amount_million_usd) # the total amount disbursed


## The 10 Countries that have received the highest loan since 1933

top_10_countries_since_1933 <- data %>% 
  group_by(country) %>%
  summarise(loan = sum(disbursed_amount_million_usd)) %>% 
  arrange(-loan) %>% 
  top_n(10)

ggplot(top_10_countries_since_1933, aes(x = reorder(country,loan), y = loan))+
  geom_col(fill = "#dada72")+
  geom_text(aes(label = round(loan, 2), vjust = 0.5, hjust = 1))+
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())+
  coord_flip()+
  labs(title = "Top 10 Most Loaned Countries by IDA (Million Dollars")
  
# Loans Disbursed by Regions

disbursed_loans_per_region_since_1933 <- data %>% 
  group_by(region) %>% 
  summarise(loan = sum(disbursed_amount_million_usd)) %>% 
  arrange(-loan)


  ggplot(disbursed_loans_per_region_since_1933, 
       aes(x= reorder(region, loan), y = loan))+
  geom_col(fill = "#dada72")+
  theme(axis.title.y = element_blank(),
        axis.ticks = element_blank())+
  coord_flip()+
    labs(title = "Loans Disbursed by Region Since 1933")+
    ylab("Total Amount Disbursed (Millions)")
         



## The Kenyan Funded Projects by IDA

kenyan_projects <- data %>% 
  filter(country == "Kenya") %>% 
  group_by(project_name, year) %>% 
  summarise(loan = sum(disbursed_amount_million_usd)) %>% 
  arrange(-loan)

## The top 10 Funded Projects Since 1933

projects_since_1933 <- 
  data %>% 
  group_by(project_name, country, year) %>%
  summarise(loan = sum(disbursed_amount_million_usd)) %>% 
  arrange(-loan) 

top_10_projects_since_1933 <- projects_since_1933[1:10,]  

# Amounts disbursed for the past 10 Years

past_10_yrs <- data %>% 
  filter(year >= (2022-10)) # Subsetting Dataset for the past 10 years.

sum(past_10_yrs$disbursed_amount_million_usd)

# Comparing the Loaning Trends in East African Countries (Uganda, Kenya, Tanzania) for the past 10 years
# for the past 10 years

East_Africa <- past_10_yrs %>% 
  filter(country == c("Kenya", "Tanzania", "Uganda")) %>% 
  group_by(country, year) %>% 
  summarise(loan = sum(disbursed_amount_million_usd)) # Subsetting the dataset to only show information for 
                                                      #Kenya, Tanzania, and Uganda


ggplot(East_Africa, aes(x= year, y = loan, group = country))+
  geom_line(aes(color = country))+
  labs(title = "IDA Loan Disbursement to the East African Countries",
       y = "Loan Received",
       x = "Year")+
  scale_x_continuous(limits = c(2012, 2022),
                     breaks = seq(2012, 2022, by = 3),
                     labels = seq(2012, 2022, by = 3))





  










  








  
