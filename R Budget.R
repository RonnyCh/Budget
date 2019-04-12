

library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
setwd("H:/SharedLaptop/DS/MyBudget")

# import csv files
CBAOffset <- read.csv('CBAOffset.csv', header=FALSE)
CBAOffset <- CBAOffset[,1:3]
CBACards <- read.csv('CBACards.csv', header=FALSE)


consol <- full_join(CBAOffset,CBACards)
colnames(consol) <- c("Date","Amount","Description")


# make sure amount is numeric
a$Amount <- as.numeric(a$Amount)

# sort the data
index <- order(as.Date(consol$Date,"%d/%m/%Y"))
consol <- consol[index,]


# trying grouping

consol <- consol %>% 
  mutate(Group = case_when(
    str_detect(str_to_lower(Description),'salary') == TRUE ~ "Salary",
    
    str_detect(str_to_lower(Description),'778047926') == TRUE ~ "Mortgage Wolli",
    str_detect(str_to_lower(Description),'778047934') == TRUE ~ "Mortgage The Ponds CBA1",
    str_detect(str_to_lower(Description),'876891821') == TRUE ~ "Mortgage The Ponds CBA2",
    str_detect(str_to_lower(Description),'harcourts') == TRUE ~ "Rental Income The Ponds",
    str_detect(str_to_lower(Description),'admiral') == TRUE ~ "Mortgage The Ponds WBC",
    
    str_detect(str_to_lower(Description),'carena|slickprop') == TRUE ~ "Rental Income Wolli",
    str_detect(str_to_lower(Description),'fortnightly ebley') == TRUE ~ "Rental Expense",
    str_detect(str_to_lower(Description),'strata') == TRUE ~ "Strata Expense",
    str_detect(str_to_lower(Description),'sydney water') == TRUE ~ "Sydney Water",
    str_detect(str_to_lower(Description),'weekly pocket') == TRUE ~ "Weekly Pocket",
    
    str_detect(str_to_lower(Description),'helping|code camp|ps|bookshop') == TRUE ~ "School",
    str_detect(str_to_lower(Description),'coles|costco|woolworths') == TRUE ~ "Groceries",
    str_detect(str_to_lower(Description),'medibank|chemist|health|ultrasound|pharm|laverty|surya|pline') == TRUE ~ "Health",
    str_detect(str_to_lower(Description),'target|kmart|cotton|myer|h&m|rebel|zara|kogan|groupon|big w|crazysales') == TRUE ~ "Shopping",
    str_detect(str_to_lower(Description),'fitness|srg|interesting|ymca|decathlon') == TRUE ~ "Fitness",
 
    str_detect(str_to_lower(Description),'optus|tpg') == TRUE ~ "Internet & Phone",
    str_detect(str_to_lower(Description),'agl') == TRUE ~ "utility",
    str_detect(str_to_lower(Description),'bayside') == TRUE ~ "CouncilFee",
    str_detect(str_to_lower(Description),'insurance|ins') == TRUE ~ "Insurance",
    str_detect(str_to_lower(Description),'top one|nrma|gogetcar') == TRUE ~ "Car",
    str_detect(str_to_lower(Description),'transport') == TRUE ~ "Opal",
    str_detect(str_to_lower(Description),'netflix|prime') == TRUE ~ "TV",
    str_detect(str_to_lower(Description),'pizza|hoyts|opera|pancakes|din tai fung|sunny|tawandang|myeong dong|hot|fish|bakery') == TRUE ~ "Entertainment",
    str_detect(str_to_lower(Description),'trip|expedia|singapore|travel') == TRUE ~ "Travel",
    str_detect(str_to_lower(Description),'payment|transfer|received|bpay') == TRUE ~ "Payments",
    str_detect(str_to_lower(Description),'transfer|direct credit') == TRUE ~ "Transfer",
    str_detect(str_to_lower(Description),'atm|cash|branch') == TRUE ~ "Cash Transaction",
    str_detect(str_to_lower(Description),'bp|caltex|7-eleven') == TRUE ~ "Petrol",
    TRUE ~ "other"), Month=month(Date,label=TRUE), Year=year(as.Date(Date,"%d/%m/%Y"))) 





# create pivot table
final <- consol %>% group_by(Month,Group) %>% summarize(Total = sum(Amount)) %>% spread(Month,Total) 
final[is.na(final)] <- 0

mycolumn <- c('Group','Nov','Dec','Jan','Feb','Mar','Apr')
final <- final[,mycolumn]
final <- final %>% filter(Group!='Payments')

# convert to matrix
mymatrix <- as.matrix(final[,2:6])
myrow <- as.matrix(final[,1])
rownames(mymatrix) <- myrow

# sort the row by alphabetical order
rownames(mymatrix) <- sort(rownames(mymatrix))


# overall view
mymatrix

# 1. Net position
apply(mymatrix,2,sum)

# the ponds
tp <- c('Mortgage The Ponds CBA1','Mortgage The Ponds CBA2','Mortgage The Ponds WBC','Rental Income The Ponds')
theponds <- mymatrix[tp,]
apply(theponds,2,sum)
apply(theponds,2,sum)/4  # average per week




# filter lower level
consol %>% filter(Group=='Car')











## some useful code to add...... 
baskets.df <- rbind(baskets.df, “7th” = c(7, 4))



# create a forecast trend
Date <- seq(as.Date("1/1/2019","%d/%m/%Y"), by = "week", length.out = 12)
Amount <- seq(250, 250, length.out = 12)
df <- data.frame(Date,Amount)
df$Group <- 'Salary'

##### add another line
Date <- seq(as.Date("1/1/2019","%d/%m/%Y"), by = "week", length.out = 12)
Amount <- seq(250, 250, length.out = 12)
df2 <- data.frame(Date,Amount)
df2$Group <- 'Test'

rbind(df,df2)
