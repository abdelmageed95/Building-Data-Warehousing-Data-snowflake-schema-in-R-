
# First part 

library(dplyr)
library(ggplot2)
library(MASS) 
library(reshape2) 
library(reshape) 
library(corrplot)
library(reshape2)
library(ggplot2)
library(scales)
library(plyr)

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# import the data 

bank_data<-read.csv("bank-additional-full.csv", header =TRUE, sep =";")
View(bank_data)
str(bank_data)

# 1- select the predictors 
data <- data.frame(age =bank_data$age , education = bank_data$education ,
                   previous = bank_data$previous, pdays = bank_data$pdays , response = bank_data$y)

View(data)

#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------

# 2 - Change the field value 999 to "NA" to represent missing values.
hist(data$pdays , na.rm = T , 
     main = "pdays with Na ")
data$pdays[data$pdays == 999] <- 'NA'
View(data)
str(data)
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------

# 3 -  Explain why the field pdays is essentially useless until you handle the 999 code
print(sum( data$pdays == "NA"))
# 39673 Na value out of 41178 value so most of pdays values in missing  

#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------

# 4 - Create a histogram of the pdays variable showing the missing value excluded

data$pdays = sapply(data$pdays ,as.numeric)
sapply(data, mode) # show columns type 



#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------

#5. Transform the data values of the education field into numeric values

unique(data$education)
# replace education values 

data$education[data$education == 'illiterate'] <- 0
data$education[data$education == 'basic.4y'] <- 4
data$education[data$education == 'basic.6y'] <- 6
data$education[data$education == 'basic.9y'] <- 9 
data$education[data$education == 'high.school'] <-  12
data$education[data$education == 'professional.course'] <- 14
data$education[data$education == 'university.degree'] <- 16
data$education[data$education == 'unknown'] <- NA
data$education = sapply(data$education ,as.numeric)
sapply(data, mode) 
View(data)
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# 6 - Compute the mean, median & mode of the age variable. Using a box plot, 
#  give the five number summary of the data. Plot the quantile information.

mean <- mean(data$age)
print(mean)

median <- median(data$age)
print(median)

install.packages("modeest")
library(modeest)
mode<- mfv(data$age)
print(mode)

boxplot(data$age,
        main = 'boxplot for age column ',
        col = 'yellow', 
        ylab = 'Age',
        horizontal = TRUE )


summary(data$age)
qqnorm(data$age)
qqline(data$age , datax = FALSE , distribution =qnorm , probs = c(0.25,0.75))

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

#7 - Some machine learning algorithms perform better when the numeric fields are
#standardized. Standardize the age variable and save it as a new variable, age_z 
data$age_z <- scale(data$age)

boxplot(data$age_z,
        main = 'boxplot for age column ', 
        ylab = 'Age',
        horizontal = TRUE )

#------------------------------------------------------------------------------------
#8. Obtain a listing of all records that are outliers according to the field age_z.

outliers <- boxplot.stats(data$age_z)$out
print(outliers)





#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------


# Part 2 
#Using R, read the dimensions files and the profit fact table. Build an OLAP cube for your
#revenue and show the cells of a subset of the cells

library(readr)

Cheese<-read.csv("cheese.csv")

City<-read.csv("City.csv", header =TRUE, sep =",")

country<-read.csv("country.csv", header =TRUE, sep =",")

year<-read.csv("year.csv",)

Dough<-read.csv("Dough.csv", header =TRUE, sep =",")

Province<-read.csv("Province.csv", header =TRUE, sep =",")

Region<-read.csv("Region.csv", header =TRUE, sep =",")

size<-read.csv("size.csv", header =TRUE, sep =",")

Topping<-read.csv("Topping.csv", header =TRUE, sep =",")

Address<-read.csv("Address.csv", header =TRUE, sep =",")

month<-read.csv("month.csv", header =TRUE, sep =",")





#********************Create function to generate Fact table********************************************

# Function to generate the Sales table
gen_orders <- function(n) {
  
  # Generate transaction data randomly
  city_id <- sample(City$city_id, n, replace=T, prob=c(7,10,9,5,4,2,1,6,3,1,2,3,1,2,3,4,2,3,2))
  Province_ID <- sample(Province$province_id , n ,replace = T ,prob = c(8,5,7,3,2,1,6,3))
  Country_ID <- sample(country$country_id , n ,replace = T ,prob = c(8,6,4,3,2))
  REGION_ID <- sample(region$store_loc_id , n ,replace = T ,prob = c(8,6,3))
  year_id <- sample(year$year_id , n, replace=T ,prob = c(3 , 4,7 ))
  Month_id <- sample(month$month_id, n, replace=T)
  p_size <- sample(size$size_id ,n , replace = T )
  Dough_id <- sample(Dough$dough_id, n, replace=T, prob=c(1, 3, 2))
  cheese_id <- sample(Cheese$cheese_id, n, replace=T, prob=c(5,1, 3))
  topping_id <- sample(Topping$topping_id, n , replace = T ,prob = c(5 ,2 ,1,4))

  quantity <- sample(x= c(1,2,3,4) , n ,replace =T ,prob= c(10,6,4,2))
  
  orders <- data.frame(MONTH_ID=Month_id,
                       YEAR_ID=year_id,
                       CITY_ID = city_id ,
                       Province_ID = Province_ID , 
                       Country_ID = Country_ID ,
                       REGION_ID = REGION_ID , 
                       P_SIZE= p_size,
                       DOUGH_ID=Dough_id,
                       CHEESE_ID=cheese_id, 
                       TOPPING_ID = topping_id,
                       QUANTITY = quantity )
  
  # Sort the records by time order
  orders <- orders[order(orders$YEAR_ID, orders$MONTH_ID),]
  row.names(orders) <- NULL
  return(orders)
}
# Now create the sales fact table
orders_fact <- gen_orders(1000)

# Look at a few records
View(orders_fact)



#******************************merge data set ************************************
library(dplyr)
# 1 - join orders_fact table with Topping table in a new table (profit table) 
profit_fact <- left_join( orders_fact,Topping,by = c( "TOPPING_ID" = "topping_id"))
# 2 - join with year table by year id 
profit_fact <- left_join( profit_fact,year,by = c( "YEAR_ID"="year_id"))
# 3- join with month table by month id
profit_fact <- left_join( profit_fact,month,by = c( "MONTH_ID"="month_id"))
# 4- join with size table by p_size and size_id
profit_fact <- left_join( profit_fact,size,by = c( "P_SIZE"="size_id"))
# 5 - join with dough table by dough_id
profit_fact <- left_join( profit_fact,Dough,by = c( "DOUGH_ID"="dough_id"))
# 6 - join with cheese table by cheese_id 
profit_fact <- left_join( profit_fact,Cheese,by = c( "CHEESE_ID"="cheese_id"))

# nested join 
# join the address ,city  ,province ,country and region on Location_table 
Location_table <- left_join(City ,Address , by= c("city_id" = "city_id"))
Location_table <- left_join(Location_table ,Province , by= c("province_id" = "province_id"))
Location_table <- left_join(Location_table ,country , by= c("country_id" = "country_id"))
Location_table <- left_join(Location_table ,region , by= c("region_id" = "store_loc_id"))

# join Location_table with profit_fact to construct Pizza data Frame 
Pizza_df <- left_join(profit_fact ,Location_table , by= c("REGION_ID" = "region_id"))
Pizza_df$profit <- Pizza_df$QUANTITY * Pizza_df$size_price 

View(Pizza_df)

#*******************************************************************************
#*******************************************************************************


# Build up a cube
profit_cube <- 
  tapply(Pizza_df$profit, 
         Pizza_df[,c("size_type", "year","month","top_type" , "dough_type" , "cheese_type"  )], 
         FUN=function(x){return(sum(x))})

profit_cube
dimnames(profit_cube)

# Showing the cells of the cube

profit_cube[ "large",  , , "tomatoes" , "stuffed crust" , "Mozzarella" ]
profit_cube[ "large",  , , "pepperon" , "stuffed crust" , "Mozzarella"]
profit_cube[ "large",  , , "pepper"   , "stuffed crust" , "Mozzarella" ]
profit_cube[ "large",  , , "onions"   , "white regular" , "Mozzarella" ]



#************************size_type********************************

#                        ROLLUP

apply(profit_cube, c("year", "size_type"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})


#                     DRILL DOWN

apply(profit_cube, c("year", "month", "size_type"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})




#************************top_type********************************
#*
#*                      Roll up 

apply(profit_cube, c("year", "top_type"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})



#*                    Drill down 
#*                    
apply(profit_cube, c("year", "month", "top_type"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})




#************************cheese_type********************************
#*
#*                      Roll up 

apply(profit_cube, c("year", "cheese_type"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})



#*                    Drill down 
#*                    
apply(profit_cube, c("year", "month", "cheese_type"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})




#************************dough_type********************************
#*
#*                      Roll up 

apply(profit_cube, c("year", "dough_type"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})



#*                    Drill down 
#*                    
apply(profit_cube, c("year", "month", "dough_type"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})









