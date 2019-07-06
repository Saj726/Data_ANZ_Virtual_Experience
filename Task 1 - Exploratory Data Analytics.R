###############################################
# Data@ANZ Task 1 - Exploratory Data Analysis #
# By Samuel Khoo                              #
# www.linkedin.com/in/samuelkwz               #
# https://github.com/samuelkhoo               #
###############################################

# import dataset
library(readxl)
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(scales)
library(rworldmap)
library(stringr)
txn = read_excel("ANZ synthesised transaction dataset.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

# exploring dataset
str(txn) # 12043 rows
summary(txn) # txn data from 01/08/18 to 31/10/18

levels(factor(txn$status)) # authorized, posted
levels(factor(txn$card_present_flag)) # 0, 1
levels(factor(txn$bpay_biller_code)) # only 0
levels(factor(txn$currency)) # only AUD
levels(factor(txn$txn_description)) # INTER BANK, PAY/SALARY, PAYMENT, PHONE BANK, POS, SALES-POS
levels(factor(txn$gender)) # F, M
length(levels(factor(txn$merchant_suburb))) # 1609 merchant suburbs
levels(factor(txn$merchant_state)) # ACT, NSW, NT, QLD, SA, TAS, VIC, WA
levels(factor(txn$country)) # only Australia
levels(factor(txn$movement)) # credit, debit

# transaction status
anyNA(txn$status) # no NAs
status_df = data.frame(status = levels(factor(txn$status)), count = c(nrow(txn[txn$status == 'authorized',]), nrow(txn[txn$status == 'posted',])))
status_df = mutate(status_df, percent = paste0(round(status_df$count/sum(status_df$count)*100, 0), '%'))
pie1 = ggpie(status_df, "count", main = 'Pie chart of Transaction Status',
             label = "percent", lab.pos = "in",lab.font = "white", 
             fill = 'status', color = "white", palette = c("#00AFBB", "#E7B800", "#FC4E07")
            )
pie1 = pie1 + theme(legend.position = "right")

# card present flag
anyNA(txn$card_present_flag)
sum(is.na(txn$card_present_flag)) # 4326 NAs
cpf_df = data.frame(status = c("0", "1", "NA"), 
                    count = c(nrow(txn[txn$card_present_flag == '0',]), nrow(txn[txn$card_present_flag == '1',]), nrow(txn[is.na(txn$card_present_flag),]))
                   )
cpf_df = mutate(cpf_df, percent = paste0(round(cpf_df$count/sum(cpf_df$count)*100, 0), '%'))
pie2 = ggpie(cpf_df, "count", main = 'Pie chart of Card Present Flag',
             label = "percent", lab.pos = "in",lab.font = "white", 
             fill = 'status', color = "white", palette = c("#E7B800", "#00AFBB", "#FC4E07")
)
pie2 = pie2 + theme(legend.position = "right")

# transaction type
anyNA(txn$txn_description) # no NAs
td_df = data.frame(Type = levels(factor(txn$txn_description)), 
                   count = c(nrow(txn[txn$txn_description == 'INTER BANK',]), 
                            nrow(txn[txn$txn_description == 'PAY/SALARY',]),
                            nrow(txn[txn$txn_description == 'PAYMENT',]),
                            nrow(txn[txn$txn_description == 'PHONE BANK',]),
                            nrow(txn[txn$txn_description == 'POS',]),
                            nrow(txn[txn$txn_description == 'SALES-POS',])
                            )
                  )
td_df = mutate(td_df, percent = paste0(round(td_df$count/sum(td_df$count)*100, 0), '%'))
pie3 = ggpie(td_df, "count", main = 'Pie chart of Transaction Type',
             label = "percent", lab.pos = "in",lab.font = "black", 
             fill = 'Type', color = "white")
pie3 = pie3 + theme(legend.position = "right")

# number of merchants
anyNA(txn$merchant_id)
sum(is.na(txn$merchant_id)) # 4326 NAs (note: same number as card present flag NAs)
# investigate NAs
na_merchant_df = txn[is.na(txn$merchant_id),] 
levels(factor(na_merchant_df$card_present_flag)) # all merchant NA txns are also card present flag NAs
levels(factor(na_merchant_df$txn_description)) # does not have POS or SALES-POS transactions
# investigate non NAs
merchant_df = txn[!is.na(txn$merchant_id),]
nrow(merchant_df) # 7717 merchant txns
length(levels(factor(merchant_df$merchant_id))) # 5725 unique merchants
levels(factor(merchant_df$txn_description)) # only POS or SALES-POS transactions have merchants

# get list of unique customers
customer_df = txn %>% select(customer_id, gender, age) %>% unique

# gender breakdown
gender_df = data.frame(Type = levels(factor(customer_df$gender)), 
                   count = c(nrow(customer_df[customer_df$gender == 'F',]), 
                             nrow(customer_df[customer_df$gender == 'M',])
                   )
)
gender_df = mutate(gender_df, percent = paste0(round(gender_df$count/sum(gender_df$count)*100, 0), '%'))
pie4 = ggpie(gender_df, "count", main = 'Pie chart of Gender Breakdown',
             label = "percent", lab.pos = "in",lab.font = "black", 
             fill = 'Type', color = "white")
pie4 = pie4 + theme(legend.position = "right")

# age breakdown
age_hist = ggplot(customer_df, aes(x=age, fill = factor(gender))) + 
  geom_histogram(binwidth = 5, position = "stack") + 
  labs(fill = "Gender") + 
  scale_x_continuous(breaks = round(seq(min(customer_df$age), max(customer_df$age), by = 5), 0) ) +
  scale_y_continuous(breaks = round(seq(0, 30, by = 5), 0) ) +
  labs(y='Number of customers', x='Age')

# transaction time series
txn_date_df = txn %>% select(extraction, transaction_id, amount)
txn_date_df$extraction = floor_date(as.Date(txn_date_df$extraction), unit = "week")
txn_date_count_df = count(txn_date_df, extraction)
# number of txns
time_series_1 = ggplot(txn_date_count_df, aes(x = extraction, y = n)) +
  geom_line(color = '#00AFBB', size =1.3) +
  geom_point(color = '#00AFBB', size=3) +
  labs(y='Number of transactions', x='Date') +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))
# transaction value
time_series_2 = ggplot(txn_date_df, aes(x = extraction, y = amount)) +
  stat_summary(fun.y=sum, geom="line", color = '#E7B800', size =1.3) +
  stat_summary(fun.y=sum, geom="point", color = '#E7B800', size =3) +
  labs(y='Gross transaction value (GTV)', x='Date') +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))

# merchant location
merchant_df = txn %>% select(merchant_id, merchant_state, merchant_long_lat) %>% 
  na.omit %>% 
  unique %>% 
  separate(merchant_long_lat, c("long", "lat"), " ")# 5725 unique merchants
# histogram of merchants by suburbs
ggplot(merchant_df, aes(x = merchant_state)) + 
  geom_bar(fill = "#00AFBB") +
  labs(y='Number of merchants', x='State') +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))
# merchant map plot
newmap <- getMap(resolution = "high")
plot(newmap, xlim = c(100, 170), ylim = c(-65, -10), asp = 1)
points(merchant_df$long, merchant_df$lat, col = 'deepskyblue2', cex = .6)

# movement breakdown
movement_df = data.frame(Type = levels(factor(txn$movement)), 
                       count = c(nrow(customer_df[txn$movement == 'credit',]), 
                                 nrow(customer_df[txn$movement == 'debit',])
                       )
)
movement_df = mutate(movement_df, percent = paste0(round(movement_df$count/sum(movement_df$count)*100, 0), '%'))
pie5 = ggpie(movement_df, "count", main = 'Pie chart of Movement Breakdown',
             label = "percent", lab.pos = "in",lab.font = "black", 
             fill = 'Type', color = "white")
pie5 = pie5 + theme(legend.position = "right")

# average txn amount
avg_txn_amt = sum(txn$amount) / nrow(txn)

# average age
avg_age = sum(customer_df$age) / nrow(customer_df)
