data <- Pakistan.Largest.Ecommerce.Dataset
library(tidyverse)

glimpse(data)


### Converting Data Types ###
#############################
data$status <- as.factor(data$status)
data$category_name_1 <- as.factor(data$category_name_1)
data$MV <- as.numeric(data$MV)
data$BI.Status <- as.factor(data$BI.Status)
data$payment_method <- as.factor(data$payment_method)
data1$Customer.Since <- ym(data1$Customer.Since)
data1$FY <- as.factor(data1$FY)
data1$Year <- as.factor(data1$Year)
data1$Working.Date <- mdy(data1$Working.Date)
data3$created_at <- mdy(data3$created_at)
data3$Month <- as.factor(data3$Month)
data2$created_at <- Pakistan.Largest.Ecommerce.Dataset$created_at
data_clean$M.Y <- Pakistan.Largest.Ecommerce.Dataset$M.Y
data4$M.Y <- my(data4$M.Y)




#########################
data_clean <- data4
head(data_clean)
glimpse(data_clean)
#########################

levels(data_clean$status)

### Dealing with Missing Values ###
###################################

attach(data_clean)
sum(is.na(data_clean$status))

class(data5$status)
levels(data6$status)

data5 <- data_clean
data5 %>% mutate(status=recode(status, "\\N" = "unknown")) -> data6

glimpse(data6)
unique(data6$status)


### Exploratory Data Analysis ###
#################################

attact(data5)
names(data5)

# No of Orders by Status
data5 %>% select(item_id, status) %>% 
  group_by(status) %>%
  filter(status != "\\N") %>% 
  na.omit(status) %>% 
  summarise(Total_Orders = n()) %>% 
  arrange(desc(Total_Orders)) -> orders_by_status

 orders_by_status %>%ggplot(aes(status, Total_Orders)) +
  geom_bar(stat = "identity", fill = "#EA906C", alpha = 1) + 
   coord_flip()


# Total Earnings by Category
data5 %>% select(category_name_1, grand_total) %>% 
  group_by(category_name_1) %>%
  summarise(Total_Amount = sum(grand_total)/10^5) %>% 
  arrange(desc(Total_Amount)) -> cat_total



# Average Order Value by Category
data5 %>% select(category_name_1, grand_total) %>% 
  group_by(category_name_1) %>%
  summarise(Avg_Order_Value = mean(grand_total)) -> cat_avg
cat_avg %>% arrange(desc(Avg_Order_Value))




# No of Order by Payment Method
data5 %>% select(payment_method, item_id) %>% 
  group_by(payment_method) %>%
  summarise(Total_Payments = n()) %>% 
  arrange(desc(Total_Payments)) -> Payments_Frequency
print(Payments_Frequency)



# Average Discount by Top 10 Payment Method
data5 %>% select(payment_method, discount_amount, item_id) %>% 
  group_by(payment_method) %>% 
  summarise(Avg_Discount = sum(discount_amount)/n()) %>% 
  arrange(desc(Avg_Discount)) %>% 
  head(10) -> Avg_Discount


Avg_Discount %>% ggplot(aes(payment_method, Avg_Discount)) +
  geom_bar(stat = "identity", fill = "darkred" ) +
  coord_flip()


# No of Customers by Month
data5 %>% select(Customer.Since, Year) %>% 
  group_by(Customer.Since) %>% 
  summarise(Total_Customers = n()) -> customers_by_month

customers_by_month %>% ggplot(aes(Customer.Since, Total_Customers)) +
  geom_point(color="blue") +
  geom_line(color="darkblue") +
  geom_smooth(method = lm) +
  labs(title = "Customers by Year", x = "Year", y = "# of Customers") 



# No of Orders by Year
data5 %>% select(Year, item_id) %>% 
  group_by(Year) %>% 
  summarise(Total_Order = n()) -> Orders_Per_Year

bar_chart <- Orders_Per_Year %>%ggplot(aes(Year, Total_Order)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7)


# Sales by Fiscal Year
data5 %>% select(FY, grand_total) %>% 
  group_by(FY) %>% 
  summarise(Total_Sales = sum(grand_total/10^9)) -> sales_year


sales_year %>% ggplot(aes(FY, Total_Sales)) +
  geom_bar(fill="purple", stat = "identity")



##########################
### Data Visualization ###







