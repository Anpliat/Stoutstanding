library(car)             # for "vif" function
library(caret)           # for "RMSE" function
library(choroplethr)     # for "state_choropleth" function
library(choroplethrMaps) # for retrieving the map of US states
library(data.table)      # for "data.table" function
library(doParallel)      # for paraller processing
library(dplyr)           # for "%>%" pipe operator
library(ggcorrplot)      # for "ggcorrplot" function
library(ggplot2)         # for "ggplot" function
library(glmnet)          # for "cv.glmnet"function
library(rpart)           # for fitting decision trees
library(rpart.plot)      # for plotting decision trees


data<- read.csv("C:/.../Stout - Case Study/Case Study 1/loans_full_schema.csv", header=TRUE)
class(data)
# head(data)
dim(data)


any(is.na(data)) # There are missing values
all(colSums(is.na(data)) != 0)
sum(sapply(data,is.na))
sum(sapply(data,is.null))
sum(sapply(data,is.nan))
sum(sapply(data,is.infinite))
any(duplicated.data.frame(data)==TRUE)  

print(paste('There are ',sum(sapply(data,is.na)), ' missing values in the data', sep=""))
print(paste('There are ',round(100*mean(is.na(data)),1), '% missing values in the data', sep=""))

# ========================================================================================
# ========================================================================================
# ===================================  Data Cleansing  ===================================
# ========================================================================================
# ========================================================================================

# temp1<-colSums(is.na(data)) # number of missing values per column
# temp1[temp1!=0]
temp1_pct<-(colMeans(is.na(data)))*100 # percentage of missing values per column
temp1_pct[temp1_pct!=0]

# Remove columns that have more than 40% missing values
# Note: Another way would be for example to run a model on top of non-null values 
#       and predict the missing values in that respective column.

data$annual_income_joint<-NULL
data$debt_to_income_joint<-NULL
data$months_since_last_delinq<-NULL
data$months_since_90d_late<-NULL

unique(data$num_accounts_120d_past_due) # Values: "0", "NA"
# data$num_accounts_120d_past_due<-NULL

# For columns with a little percentage of null values, we decided to  
# replace NA values with the median of their respective columns
data$emp_length[is.na(data$emp_length)] = median(data$emp_length[!is.na(data$emp_length)])
data$debt_to_income[is.na(data$debt_to_income)] = median(data$debt_to_income[!is.na(data$debt_to_income)])
data$months_since_last_credit_inquiry[is.na(data$months_since_last_credit_inquiry)] = median(data$months_since_last_credit_inquiry[!is.na(data$months_since_last_credit_inquiry)])


unique(data$verified_income)
unique(data$verification_income_joint)
prop.table(table(data$verification_income_joint))
# about 85% of the values are "". removed it from data
# data$verification_income_joint<-NULL

# G4: Only one occurence, which was only present in the train data => removed it
table(data$sub_grade)
data<-data[data$sub_grade!="G4",]


## Check variable types in the data
unique(sapply(data,class))
characters<-data[sapply(data,class)=='character']  
sinexeis<-data[sapply(data,class)=='numeric']  
diakrites<-data[sapply(data,class)=='integer'] 

# Count unique values per 'character' variables
apply(characters, 2, function(x) length(unique(x)))

# Convert character columns to factors
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
factors<-data[sapply(data,class)=='factor'] 
data$num_accounts_120d_past_due<-NULL
data$verification_income_joint<-NULL


# =========================================================================================
# =========================================================================================
# ==============================  Exploratory Data Analysis  ==============================
# =========================================================================================
# =========================================================================================

unique(sapply(data,class))
sum(sapply(data,class)=="integer")
sum(sapply(data,class)=="numeric")
sum(sapply(data,class)=="factor")


round(sapply(sinexeis,summary),1)
# round(sapply(diakrites,summary),1)

# Count unique values per column
apply(data, 2, function(x) length(unique(x)))


summary(data$interest_rate)

# Separate the response from the predictors
data_mod<-data
interest_rate<-data_mod$interest_rate
data_mod$interest_rate<-NULL

posotikes<-data_mod[sapply(data_mod,class)=='numeric' | sapply(data_mod,class)=='integer'] 


####################################################################################
##################################  Correlations  ##################################
####################################################################################

#### Correlation of all independent variables with the dependent

pin8f<-round(cor(posotikes, interest_rate),2)
pin8f<-as.data.frame(pin8f)
pin8f<-cbind(rownames(pin8f),pin8f)
colnames(pin8f)<-c('Variable', 'Cor_with_Y')
rownames(pin8f)<-NULL
pin8f <- pin8f[order(pin8f$Cor_with_Y, decreasing=TRUE),]
pin8f

# strong_med_cor_Y_full<-pin8f[abs(pin8f$Cor_with_Y)>=0.5,]
# strong_med_cor_Y_full
# pin8f[abs(pin8f$Cor_with_Y)<0.5,]

#### Show the correlation between the dependent and independent variables, 
#### where it was higher than 0.2
temp_cors<-pin8f[abs(pin8f$Cor_with_Y)>=0.2,]
temp_data_cors<-posotikes[,colnames(posotikes) %in% temp_cors$Variable]
temp_data_cors<-cbind(interest_rate, temp_data_cors)
colnames(temp_data_cors)[1]<-"Interest Rate"

ggcorrplot(round(cor(temp_data_cors),2), hc.order = TRUE, 
           type = "lower", lab = TRUE,
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726")) + 
  ggtitle("Highest correlation coefficients \n (Interest Rate vs Dependent Variables)") + 
  theme(plot.title = element_text(size = 12, face = "bold"))

#####################################################################################
################################  Contigency Tables  ################################
#####################################################################################

### Below, we showcase the cases where we could not reject the null hypothesis
sjt.xtab(factors[,3],factors[,10],var.labels=c(colnames(factors)[3],colnames(factors)[10]),show.cell.prc=F, show.row.prc=F, show.col.prc=F, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(factors[,3],factors[,13],var.labels=c(colnames(factors)[3],colnames(factors)[13]),show.cell.prc=F, show.row.prc=F, show.col.prc=F, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(factors[,4],factors[,12],var.labels=c(colnames(factors)[4],colnames(factors)[12]),show.cell.prc=F, show.row.prc=F, show.col.prc=F, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(factors[,6],factors[,10],var.labels=c(colnames(factors)[6],colnames(factors)[10]),show.cell.prc=F, show.row.prc=F, show.col.prc=F, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(factors[,7],factors[,10],var.labels=c(colnames(factors)[7],colnames(factors)[10]),show.cell.prc=F, show.row.prc=F, show.col.prc=F, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(factors[,7],factors[,11],var.labels=c(colnames(factors)[7],colnames(factors)[11]),show.cell.prc=F, show.row.prc=F, show.col.prc=F, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(factors[,7],factors[,12],var.labels=c(colnames(factors)[7],colnames(factors)[12]),show.cell.prc=F, show.row.prc=F, show.col.prc=F, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(factors[,8],factors[,10],var.labels=c(colnames(factors)[8],colnames(factors)[10]),show.cell.prc=F, show.row.prc=F, show.col.prc=F, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(factors[,9],factors[,10],var.labels=c(colnames(factors)[9],colnames(factors)[10]),show.cell.prc=F, show.row.prc=F, show.col.prc=F, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")
sjt.xtab(factors[,11],factors[,12],var.labels=c(colnames(factors)[11],colnames(factors)[12]),show.cell.prc=F, show.row.prc=F, show.col.prc=F, wrap.labels = 10,tdcol.n="black",tdcol.row="green",tdcol.cell="blue",tdcol.col="red")


####################################################################################
##############################  Other Visualizations  ##############################
####################################################################################

# ----------------------------------
########### Match the state abbrevations to actual state names

####### 1st way
table43=data.table(table(data$state))
setnames(table43,c("region","count"))
table43$region=sapply(state.name[match(table43$region,state.abb)],tolower)

####### 2nd way
data$region <- data$state
data$region <- factor(data$region)
levels(data$region)<- c("alaska", "alabama","arkansas", "arizona", "california","colorado","connecticut",
                        "district of columbia","delaware","florida","georgia","hawaii","idaho",
                        "illinois","indiana","kansas","kentucky","louisiana","massachusetts","maryland",
                        "maine","michigan","minnesota","missouri","mississippi","montana","north carolina",
                        "north dakota","nebraska","new hampshire","new jersey","new mexico","nevada",
                        "new york","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina",
                        "south dakota","tennessee","texas","utah","virginia","vermont","washington",
                        "wisconsin","west virginia","wyoming")
# ----------------------------------

############ Loan Counts per State
us_states <- map_data("state")
counter <- merge(us_states, table43, by="region")

ggplot(counter, aes(x=long, y=lat, map_id = region)) + 
  geom_map(aes(fill= count), map = us_states)+
  scale_fill_gradientn("",colours=terrain.colors(10),guide = "legend")+
  theme_bw() +
  labs(title="Loan Counts per State",x="",y="") +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        legend.title.align=0.5,
        legend.background = element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"),
        legend.box = "vertical", legend.position = c(0.935, 0.215),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  guides(fill=guide_legend(title="Loan Counts")) 



############ Loan Volume per State
volume_data <- select(data, region)
volume_data <-  volume_data %>% filter(region != "NA") %>% na.omit() %>% 
  group_by(region) %>% summarise(value = n())

state_choropleth(volume_data, num_colors = 5) +
  labs(title="Loan Volume per State",x="",y="") +
  theme_bw() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        legend.title.align=0.5,
        legend.background = element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"),
        legend.box = "vertical", legend.position = c(0.89, 0.215),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank())  +
  guides(fill=guide_legend(title="Loan Volume")) 



############ Loan Amount per State
amount_data <-data %>% group_by(region) %>%
  summarise(value = sum(loan_amount, na.rm=TRUE))

state_choropleth(amount_data,num_colors = 5) +
  labs(title="Loan Amount per State",x="",y="") +
  theme_bw() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        legend.title.align=0.5,
        legend.background = element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"),
        legend.box = "vertical", legend.position = c(0.88, 0.15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank())  +
  guides(fill=guide_legend(title="Loan Amount")) 



############ Grade and Home Ownership (along with disbursement method)
ownership_data <- data %>% select(homeownership, grade, disbursement_method)
ownership_data <- ownership_data[ownership_data$homeownership %in% c("MORTGAGE", "OWN", "RENT"),]

ggplot(ownership_data, aes(grade)) + 
  geom_bar(aes(fill = disbursement_method)) + 
  facet_wrap(~homeownership) + 
  labs(x="Grade of Loan", y="Number of Loans", title = "Issued loans per home ownership") + 
  theme_bw() +
  theme(legend.title.align=0.5,
        legend.background = element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"),
        legend.box = "horizontal",legend.position="bottom",
        axis.ticks = element_line(size = 0.8, color="grey40"),
        panel.grid.minor = element_line(colour = "grey88", size = 0.2)) +
  guides(fill=guide_legend(title="Dispersement Method"))



############ Loans' number and Grade (along with loan purpose)
ggplot(data=data, aes(x=grade, fill=loan_purpose)) +
  geom_bar() + 
  labs(x="Grade of Loan", y="Number of Loans", title = "Issued loans per grade") + 
  theme_bw() +
  theme(legend.title.align=0.5,
        legend.background = element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"),
        legend.box = "vertical",
        axis.ticks = element_line(size = 0.8, color="grey40"),
        panel.grid.minor = element_line(colour = "grey88", size = 0.2)) +
  guides(fill=guide_legend(title="Loan Purpose"))



############ Loan Percentage Paid per Loan Purpose
levels(data$loan_status)
# -----------------------
# Create new factor variable with the following two levels:
# 1) "Paid/Current" (i.e.the status is Current or Fully Paid)
# 2) "Other" (i.e.defaults, chargeroff and other status)
data$paid_status <- "Other"
data$paid_status[which(data$loan_status == "Fully Paid" | data$loan_status == "Current") ] <- "Paid/Current"
data$paid_status <- factor(data$paid_status)
data$paid_status <- factor(data$paid_status, levels = rev(levels(data$paid_status)))
table(data$paid_status)
# -----------------------

plot_data3 <- aggregate(loan_amount ~ loan_purpose + paid_status, data = data, sum)

ggplot(plot_data3, aes(loan_purpose, loan_amount, fill = paid_status)) + 
  geom_bar(position = "fill", stat = "identity") + 
  theme_bw() +
  scale_fill_manual(values = c("Other" = "tomato2", "Paid/Current" = "gray73")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Loan Percentage Paid per Loan Purpose",
       x = "Loan Purpose",y = "Loan Percentage") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background =element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"),
        legend.box = "vertical", legend.title.align=0.5,
        plot.title = element_text(size=18), plot.subtitle = element_text(size=13),
        axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=10, angle = 45, vjust=0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey88", size = 0.2)) 


# =========================================================================================
# =========================================================================================
# ======================================  Modelling  ======================================
# =========================================================================================
# =========================================================================================

data_mod<-data
# Bring Interest rate variable upfront
data_mod<-data_mod[,c(37,1:36,38:49)]

############## =>  Train-test split (70%-30%, without replacement)
train_size <- sample(1:nrow(data_mod),round(0.7*nrow(data_mod)),replace=FALSE) #xwris epana8esi
train <- data_mod[train_size ,]
test <-  data_mod[-train_size ,]
dim(train);dim(test)

####################################################################################
####################################################################################
############################  Linear Probability model  ############################
####################################################################################
####################################################################################

### Implementation of linear probability model with parallel computing
cl<-makePSOCKcluster(7)
registerDoParallel(cl)
mfull<-lm(train$interest_rate~.,data=train[,-1])
stopCluster(cl)

summary(mfull)
# We did not observed any NAs in the variables coefficients, so we procceed as is
options(scipen = 999)         ### Disabling scientific notation

#-----------------------------------------------------------
##############  LASSO  ##############

x<-model.matrix(mfull)[,-1] #xwris tin sta8era mesa

set.seed(9)
cl<-makePSOCKcluster(7)
registerDoParallel(cl)
lasso3<-cv.glmnet(x,train$interest_rate,alpha=1)
stopCluster(cl)

lasso3
lasso3$lambda.min                                                  
lasso3$lambda.1se

plot(lasso3)
title(main = "Plot for LASSO",cex.main = 1.7, font.main= 3, line = 2.5)

print("Names of Variables that Lasso left")
CF <- as.matrix(coef(lasso3, lasso3$lambda.1se))
names(CF[CF!=0,]) # coefficient names (not equal to zero)

## Dependent variables kept from lasso (with 1se)
lasso_vars_kept<-c("emp_title","debt_to_income","accounts_opened_24m","total_debit_limit",
                   "num_cc_carrying_balance","num_mort_accounts","term","grade","sub_grade",
                   "issue_month","disbursement_method","paid_interest")

length(lasso_vars_kept)
lasso_vars_kept<-c("interest_rate",lasso_vars_kept)
data65<-train[,which(colnames(train) %in% lasso_vars_kept)]
dim(data65)

cl<-makePSOCKcluster(7)
registerDoParallel(cl)
model1<-lm(data65$interest_rate~.,data=data65[,-1])
stopCluster(cl)
# summary(model1)

#-----------------------------------------------------------
##############  Variables selection  ##############

#####  => Stepwise selection according to AIC
# We could try either "back", "forward", or "both". Due to time contraints, we decided to only use the latter.
cl<-makePSOCKcluster(7)
registerDoParallel(cl)
model2c<-step(model1,direction="both")
stopCluster(cl)

summary(model2c)  
getCall(model2c)

## Dependent variables kept from variable selection with AIC
vars_kept_stepwise_AIC<-c( "term", "sub_grade", "issue_month", "paid_interest")
length(vars_kept_stepwise_AIC)
vars_kept_stepwise_AIC<-c("interest_rate",vars_kept_stepwise_AIC)
data68<-data65[,which(colnames(data65) %in% vars_kept_stepwise_AIC)]
dim(data65)
dim(data68)

cl<-makePSOCKcluster(7)
registerDoParallel(cl)
model4<-lm(data68$interest_rate~.,data=data68[,-1])
stopCluster(cl)

#-----------------------------------------------------------
##############  Multicollinearity Assessment  ##############
# Based on the Variance inflation factor (VIF) we will remove variables 1-1, 
# starting from the higher VIF until for all retained variables there is VIF<10 (or GFIV<<3.16)

sort(vif(model4)[,1])

# name<-names(vif(model4)[,1])[vif(model4)[,1]==max(vif(model4)[,1])]
# name
# data68b<-data68[,-which(colnames(data68)==name)]
# model4<-lm(as.numeric(data68b$shot_result)~.,data=data68b[,-1])
# sort(vif(model4)[,1])

#-----------------------------------------------------------
##############  Predictions  ##############

pr1<-predict(model4,newdata = test[,-1], type="response",interval = 'prediction',se.fit = T)
pr1_new<-pr1$fit
RMSE(pr1_new, test$interest_rate)
##  Overall estimate of interest rates
round(mean(pr1_new),2)


df21<-cbind.data.frame(round(mean(pr1_new[,1]),2),round(mean(pr1_new[,2]),2),round(mean(pr1_new[,3]),2))
colnames(df21)<-c("Overal Estimate","Lower","Upper")
df21

range(data_mod$interest_rate)
mean(data_mod$interest_rate)  


###################################################################################
###################################################################################
################################  Regression Tree  ################################
###################################################################################
###################################################################################

## Note: There is no need for variable selection.

# Build the initial tree
tree <- rpart(train$interest_rate~.,method="anova", data=train[,-1])
### "rpart" arguments: Used "anova" for a regression tree


printcp(tree) # display the results
plotcp(tree)  # visualize cross-validation results
summary(tree) # detailed summary of splits

# Create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(tree)   # visualize cross-validation results  

# Plot tree
plot(tree, uniform=TRUE, main="Regression Tree for Interest Rate")
text(tree, use.n=TRUE, all=TRUE, cex=.8)

# Identify best "cp" value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

# Create a pruned tree based on the best cp value
pruned_tree <- prune(tree, cp=best)

# Plot the pruned tree
prp(pruned_tree,
    faclen=0,   # use full names for factor labels
    extra=1,    # display number of obs. for each terminal node
    roundint=F, # don't round to integers in output
    digits=5)   # display 5 decimal places in output

#-----------------------------------------------------------
##############  Predictions  ##############

pr_tree<-predict(pruned_tree, newdata=test[,-1])
# pr_tree
RMSE(pr_tree, test$interest_rate)

##  Overall estimate of interest rates
round(mean(pr_tree),2)

range(data_mod$interest_rate)
mean(data_mod$interest_rate)

# =========================================================================================
# =========================================================================================
# ================================  Plot Predicted Values  ================================
# =========================================================================================
# =========================================================================================

##############  => For Linear Regression Model

# Add predictions 
# pr1<-predict(model4,newdata = test[,-1], type="response",interval = 'prediction',se.fit = T)
pred.int13 <- pr1
data_reg13 <- cbind(test, pred.int13)
dim(data_reg13)
colnames(data_reg13)[50:52]<-c("prediction", "lower", "upper")

# Regression line & confidence intervals
plot1 <- ggplot(data_reg13, aes(prediction, interest_rate)) +
  geom_point() +
  stat_smooth(method = lm)  +
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')

# Add prediction intervals
plot1 + geom_line(aes(y = lower), color = "red", linetype = "dashed") +
  geom_line(aes(y = upper), color = "red", linetype = "dashed")


##############  => For Regression Tree
pred.int17 <- pr_tree
data_reg17 <- cbind(test, pred.int17)
ggplot(data_reg17, aes(pred.int17, interest_rate)) +
  geom_point() +
  stat_smooth(method = lm)  +
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')
