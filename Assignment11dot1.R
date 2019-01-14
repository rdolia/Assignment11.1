#Importing marketing dataset.
bank_data<- read.csv("bank-full.csv",1,sep = ';')
bank_data<-as.data.frame(bank_data)

#Perform the below operations:
# a. Create a visual for representing missing values in the dataset.
options(max.print=999999)
library(naniar)
na_strings <- ("unknown")
na_strings <-as.character(na_strings)
#creating new set with missing value unknown changed to NA.
bank_data1<- bank_data %>% replace_with_na_all(condition = ~.x %in% na_strings)

#visualising missing values
vis_miss(bank_data1)

#remove/impute missing values
#As per the above visualisation poutcome variable has 81 % of data missing
#hence removing that variable.
bank_data2 <-bank_data1[-16]

#replace NA in contact column by 0

bank_data2[["contact"]][is.na(bank_data2[["contact"]])] <- 0

#job and eduducation are the only columns now with missing data.
#imputing data with mice package.
library(mice)
md.pattern(bank_data2)

head(bank_data2)
library(dplyr) 
bank_data2 <- bank_data2 %>%
  mutate(
    job = as.factor(job),
    education = as.factor(education),
    marital = as.factor(marital),
    default = as.factor(default),
    housing = as.factor(housing),
    loan = as.factor(loan),
    contact = as.factor(contact)
    )
str(bank_data2)
#running the mice function
bank_data3 <- mice(bank_data2,m=2,maxit=10,seed=500)
summary(bank_data3)
#check for imputed data in a field job
bank_data3$imp$job
#check for imputed data in a field education
bank_data3$imp$education
#Now we can get back the completed dataset using the complete() function.
bank_data4 <- complete(bank_data3,1)
md.pattern(bank_data4)
#We now have the full data without any missing values. 

#b. Show a distribution of clients based on a job.
hist(bank_data1$job)
#c. Check whether is there any relation between Job and Marital
#Status?

#converting below columns to numeric to calculate rank correlation
bank_data4 <- bank_data4 %>%
mutate(
  job = as.numeric(job),
  education = as.numeric(education),
  marital = as.numeric(marital)

)
cor(bank_data4$job,bank_data4$marital, method = 'spearman')


#Correlation between job and Marital is 0.056% hence we can say that there
#is no correlation between them.
#d. Check whether is there any association between Job and
#Education?
cor(bank_data4$job,bank_data4$education, method = 'spearman')
#19% correlation exists between job and education. Low correlation between the variables
