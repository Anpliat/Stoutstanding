# data = read.csv('C:/Users/tasso/Downloads/Stout - Case Study/Case Study 2/casestudy.csv', 
#                 header=TRUE)

data = read.csv('C:/Users/Tassos17/Desktop/Stout - Case Study/Case Study 2/casestudy.csv', 
                header=TRUE)

head(data)
unique(data$year)

library(dplyr) # for "%>%"

## Total (net) revenue per year
temp5<-data.frame(data %>%
                    group_by(year) %>%
                    summarise(tot_revenue = sum(net_revenue)))
temp5

# ---------------------------------------------------------------------

## New Customer Revenue (e.g. new customers not present in previous year only)
# New Customer: Any customer who has not worked with the company in the past 


# new customers per year
tab = table(data[,c(2,4)])
head(tab)
str(tab)
tab

dim(data)
dim(tab)
dim(temp9)
dim(data_new)

rownames(tab)

temp9<-data.frame(customer_email = rownames(tab), yr_2015 = tab[,1], 
                  yr_2016 = tab[,2], yr_2017 = tab[,3])
rownames(temp9)<-NULL
head(temp9)

### Full-Outer Join based on customer's email
data_new = merge(x=data, y=temp9,by="customer_email", all = TRUE)
data_new<-data_new[,c(2,1,3:7)]
head(data_new)

data_new$customer_status<-"Unknown"
data_new$customer_status[data_new$year==2016 & data_new$yr_2016==1 & data_new$yr_2015==0] = "New"
data_new$customer_status[data_new$year==2016 & data_new$yr_2016==1 & data_new$yr_2015==1] = "Old"

data_new$customer_status[data_new$year==2017 & data_new$yr_2017==1 & data_new$yr_2016==0] = "New"
data_new$customer_status[data_new$year==2017 & data_new$yr_2017==1 & data_new$yr_2016==1] = "Old"

data_new$customer_status[data_new$year==2015]<-"Unknown"

# --------------------------------
data_new$customer_losts2016<-"Unknown"
data_new$customer_losts2016[data_new$yr_2016==1] = "Kept/New"
data_new$customer_losts2016[data_new$yr_2015==1 & data_new$yr_2016==0] = "Lost"

data_new$customer_losts2017<-"Unknown"
data_new$customer_losts2017[data_new$yr_2017==1] = "Kept/New"
data_new$customer_losts2017[data_new$yr_2017==0 & data_new$yr_2016==1] = "Lost"

# --------------------------------

data_new<-data_new[,c(1:4,8:10)]
head(data_new)


# # new customers per year
# as.data.frame(table(factor(colnames(tab)[max.col(tab, "first")], colnames(tab))))
# # new customers per year (pct)
# as.data.frame(as.table((colSums((tab[,-1]==tab[,-ncol(tab)])*(tab[,-1]==1))/colSums(tab[,-1]))*100))


dim(data_new)
dim(data)



## New Customer Revenue
data_new[data_new$customer_status=="New",] %>%
  group_by(year) %>%
  summarise(new_cust_revenue = sum(net_revenue))



# ---------------------------------------------------------------------

##### Existing Customer Growth
# To calculate this, use the Revenue of existing customers for current year 
# -(minus) Revenue of existing customers from the previous year


### 1st way
temp5 %>%
  group_by(year) %>%
  mutate(customer_growth = tot_revenue - lag(tot_revenue, default = first(tot_revenue)))

### 2nd way
temp5$diff <- ave(temp5$tot_revenue, FUN = function(x) c(0, diff(x)))
temp5

### 3rd way
temp5 %>%  mutate(col3_diff = tot_revenue - lag(tot_revenue))
##### Existing Customer Growth (Percentage Change)
temp5 %>%  mutate(col3_diff_pct = round(100*(tot_revenue - lag(tot_revenue))/abs(lag(tot_revenue)),1))


# ---------------------------------------------------------------------

##### Revenue lost from attrition

## Revenue Attrition (Revenue Churn Rate)
# The formula for revenue attrition is beginning period reoccurring revenue 
# minus end-of-period reoccurring revenue, divided by beginning period revenue. 
rev3<-temp5 %>%  mutate(churn_rate = round(100*((lag(tot_revenue) - tot_revenue)/abs(lag(tot_revenue))),1))
rev3

# ---------------------------------------------------------------------

##### Existing Customer Revenue Current Year
# Revenue of existing customers that were onboarded this year

temp5<-data.frame(data_new[data_new$customer_status=="New",] %>%
                    group_by(year) %>%
                    summarise(tot_revenue = sum(net_revenue)))
temp5

# ---------------------------------------------------------------------

##### Existing Customer Revenue Prior Year
# Revenue of existing customers that were retained from last year

temp5<-data.frame(data_new[data_new$customer_status=="Old",] %>%
                    group_by(year) %>%
                    summarise(tot_revenue = sum(net_revenue)))
temp5

# ---------------------------------------------------------------------

##### Total Customers Current & Previous Year

head(data_new)


temp5<-data.frame(data_new %>%
                    group_by(year) %>%
                    summarise(tot_custs = n()))
temp5

# ---------------------------------------------------------------------

##### New Customers per year

## 1st way
temp5<-data.frame(data_new[data_new$customer_status=="New",] %>%
                    group_by(year) %>%
                    summarise(new_custs = n()))
temp5

### 2nd way
as.data.frame(table(factor(colnames(tab)[max.col(tab, "first")], colnames(tab))))
# new customers per year (pct)
# as.data.frame(as.table((colSums((tab[,-1]==tab[,-ncol(tab)])*(tab[,-1]==1))/colSums(tab[,-1]))*100))


# ---------------------------------------------------------------------

##### Lost Customers (2016, 2017)

tmp2<-data_new[data_new$customer_losts2016=="Lost",] %>%
  summarise(lost_custs = n())

tmp3<-data_new[data_new$customer_losts2017=="Lost",] %>%
  summarise(lost_custs = n())

print(paste('Customers lost in 2016 = ',tmp2[,1], sep=""))
print(paste('Customers lost in 2017 = ',tmp3[,1], sep=""))

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------


####### Customer CLV (assuming discount rate = 0%)
rev3$retention_rate<-1-rev3$churn_rate/100
rev3








# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

##############################  Plots

# -----------------------------------
temp5<-data.frame(data %>%
                    group_by(year) %>%
                    summarise(tot_revenue = sum(net_revenue)))
temp5<- temp5 %>%  mutate(cust_growth = round(100*(tot_revenue - lag(tot_revenue))/abs(lag(tot_revenue)),1))
temp5

temp6<-data.frame(data_new[data_new$customer_status=="New",] %>%
                    group_by(year) %>%
                    summarise(new_cust_revenue = sum(net_revenue)))
temp6

data_new = merge(x=temp5, y=temp6,by="year", all=TRUE)
data_new$cust_growth<-data_new$cust_growth/100
data_new$year<-factor(data_new$year)
data_new


##### Plot
ggplot(data_new, aes(new_cust_revenue, cust_growth)) +
  geom_point(size = 4) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  annotate(geom="text", x=18705518, y=-0.1125, label="2016",color="black", size=5) +
  annotate(geom="text", x=28296235, y=0.222, label="2017",color="black", size=5) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),
        plot.title = element_text(size=16)) +
  labs(title = "Correlation between customer growth and new customer revenue",
       x = "New Customer Revenue",y = "Customer Growth")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------


temp_tot<-data.frame(data_new %>%
                       group_by(year) %>%
                       summarise(tot_custs = n()))

temp5<-data.frame(data_new[data_new$customer_status=="New",] %>%
                    group_by(year) %>%
                    summarise(new_custs = n()))

temp_new<-cbind(temp5, c(tmp2[,1],tmp3[,1]))
colnames(temp_new)[3] = "Lost_Custs"

## Innter Join
pin_plot = merge(x=temp_new, y=temp_tot,by="year")
pin_plot


ggplot() + 
  theme_bw()+
  scale_x_continuous(breaks = c(2016, 2017)) + 
  geom_line(data = pin_plot, aes(x = year, y = new_custs), color = "darkseagreen", size = 1) +
  geom_line(data = pin_plot, aes(x = year, y = Lost_Custs), color = "indianred3", size = 1) +
  geom_line(data = pin_plot, aes(x = year, y = tot_custs), color = "deepskyblue3", size = 1) +
  
  scale_colour_manual("", breaks = c("New", "Lost", "Total"),
                      values = c("darkseagreen", "indianred3", "deepskyblue3")) +
  scale_linetype_manual(name = "Limit", values = 3, 
                        guide = guide_legend(override.aes = list(color = "black", linetype = 3))) +
  labs(title = "Customer base through the years",x = "Year",y = "Number of Customers") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),
        plot.title = element_text(size=16)) +
  annotate(geom="text", x=2016.05, y=147000, label="New",color="darkseagreen", size=6, angle = 25) +
  annotate(geom="text", x=2016.05, y=172000, label='Lost',color="indianred3", size=6, angle = 25) +
  annotate(geom="text", x=2016.05, y=203000, label='Total',color="deepskyblue3", size=6, angle = 20)









