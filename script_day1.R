#these is the script for Programming in R day 1
#author: k.leuveld@vu.nl

#lesson 1: analyzing patient data


read.csv(file = "data/inflammation-01.csv", header = TRUE)

weight_kg <- 55
weight_kg

#do some calculations
2.2 * weight_kg

weight_kg <- 57.5
weight_kg

weight_lbs <- 2.2 * weight_kg
weight_lbs

weight_kg <- 100

weight_kg
weight_lbs

dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
dat

head(dat)
tail(dat)


#excercise: what goes where?
mass <- 47.5
age <- 122
mass <- mass * 2.0
age <- age - 20


#lesson 10: addressing data (lesson 1 continues below)
dat_backup <- dat

dat <- read.csv(file = "data/sample.csv",
                header = TRUE,
                stringsAsFactors = FALSE)

#some useful commands for getting info on dataframes
head(dat)
class(dat)
dim(dat)
str(dat)
head(dat)

#addressing by index

#get one value
dat[1,1]

#get a column
dat[,2]

#making vectors to get more values
6:9
1:10

head(dat[,6:9])

#making non-consecutive vectors
c(1,5,7,9)
dat[c(1,5,7,9),1:5]
c("this", "is", "a", "string")

colnames(dat)

#addressing by column name:
dat$Gender
class(dat$Gender)

class(dat$BloodPressure)
dat[,'Gender']

dat[,c('Gender',"BloodPressure")]
variablesiwant <- c('Gender', "BloodPressure")
dat[,variablesiwant]

#addressing by logical vector
c(TRUE,TRUE,TRUE,TRUE)

x <- c(1,2,3,11,12,13)
x
x < 10
x == 1
x != 3
x %in% 1:10

index <- dat$Group == 'Control'

dat[index,]

x
x[x < 10] <- 0

dat$Gender

#fix gender variable
dat$Gender[dat$Gender == "M"] <- "m"
dat[dat$Gender == "F",] <- "f"

dat <- dat_backup 
dat
dim(dat)

#put patient in variable
patient_1 <- dat[1,]

#calculate the max
max(patient_1)

max(dat[2,])
min(dat[,7])

mean(dat[,7])
median(dat[,7])
sd(dat[,7])

mean(as.numeric(dat[7,]))

summary(dat[,1:4])
?apply

avg_patient_inflammation <- apply(dat, 1,
                                  mean) 

#which is correct to get the max for
#patient 5
#days 3 to 7

max(dat[5,])
max(dat[3:7,5])
max(dat[5,3:7])
max(dat[5,3,7])

#comining addressing and updating data

#first five days of every second  (2,4,6..) twice as high

#1. index of affected patients (hint ?seq)
#2. create new data frame (dat2) in which you halve the first five days' values 
#   for those patients
#3. print the corrected data frame

#getting the index of the patients and rows I want
whichPatients <- seq(2,nrow(dat),2)
whichDays <- 1:5

#havling those values
dat[whichPatients,whichDays] / 2

#make a new data frame
dat2 <- dat

#insert the updated values into the new dataframe
dat2[whichPatients,whichDays] <- dat[whichPatients,whichDays] / 2

#plotting
#for nicer plots look into using the ggplot 2 package
plot(avg_patient_inflammation)


#plotting a bunch of useful summary statistics by day
avg_day_inflammation <- apply(dat,2,mean)
plot(avg_day_inflammation)

max_day_inflammation <- apply(dat,2,max)
plot(max_day_inflammation)

min_day_inflammation <- apply(dat,2,min)
plot(min_day_inflammation)


#lesson 2: functions
fahrenheit_to_celsius <- function(temp_F) {
  #a function that takes the temperature in F
  #and returns the temperature in Celsius
  temp_C <- (temp_F - 32) * 5/9
  return(temp_C)
}

#freezing point of water
freeze <- fahrenheit_to_celsius(32)

#boiling point of water
fahrenheit_to_celsius(212)

celsius_to_kelvin <- function(temp_C){
  #a function that takes the temperature in celsius
  #and returns the temperature in Kelvin
  temp_K <- temp_C + 273.15
  return(temp_K)
}

#freezing point
celsius_to_kelvin(0)


#this function combines the two previous functions:
fahrenheit_to_kelvin <- function(temp_F){
  #a function that takes the temperature in F
  #and returns the temperature in Kelvin
  temp_C <- fahrenheit_to_celsius(temp_F)
  temp_K <- celsius_to_kelvin(temp_C)
  return(temp_K)
}



fahrenheit_to_kelvin(32)

#another way to combine function is:
#but this is may get difficult to read if you 
#add more functions
celsius_to_kelvin(fahrenheit_to_celsius(32.0))


#reminder on how to use c() to combine elements into
#a vector
x <- c("A","B","C")
x
x <- c(x,"D")
x

#now write a function called highlight that takes two
#arguments: content and wrapper
#and output a vector of strings: wrapper content wrapper

highlight <- function(content,wrapper){
  answer <- c(wrapper,content,wrapper)
  return(answer)
}

#write a function that outputs the first and last element
#of a vector

edges <- function(v){
  answer <- c(v[1],v[length(v)])
  return(answer)
}



  
  
  

