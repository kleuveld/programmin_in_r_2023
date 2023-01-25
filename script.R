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

mass <- 47.5
age <- 122
mass <- mass * 2.0
age <- age - 20

dat_backup <- dat

dat <- read.csv(file = "data/sample.csv",
                header = TRUE,
                stringsAsFactors = FALSE)

head(dat)
class(dat)
dim(dat)
str(dat)
head(dat)

dat[1,1]

dat[,2]

6:9
1:10

head(dat[,6:9])

c(1,5,7,9)
dat[c(1,5,7,9),1:5]
c("this", "is", "a", "string")

colnames(dat)

dat$Gender
class(dat$Gender)

class(dat$BloodPressure)
dat[,'Gender']

dat[,c('Gender',"BloodPressure")]
variablesiwant <- c('Gender', "BloodPressure")
dat[,variablesiwant]


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

patient 5
days 3 to 7

max(dat[5,])
max(dat[3:7,5])
max(dat[5,3:7])
max(dat[5,3,7])


#first five days of every second  (2,4,6..) twice as high

#1. index of affected patients (hint ?seq)
#2. create new data frame (dat2) in which you halve the first five days' values 
#   for those patients
#3. print the corrected data frame

dim(dat)[1]
nrow(dat)

whichPatients <- seq(2,nrow(dat),2)
whichDays <- 1:5

dat[whichPatients,whichDays] / 2

dat2 <- dat
dat2[whichPatients,whichDays] <- dat2[whichPatients,1:5] / 2

#
plot(avg_patient_inflammation)


avg_day_inflammation <- apply(dat,2,mean)
plot(avg_day_inflammation)

max_day_inflammation <- apply(dat,2,max)
plot(max_day_inflammation)

min_day_inflammation <- apply(dat,2,min)
plot(min_day_inflammation)


fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5/9
  return(temp_C)
}

#freezing point of water
freeze <- fahrenheit_to_celsius(32)

#boiling point of water
fahrenheit_to_celsius(212)

celsius_to_kelvin <- function(temp_C){
  temp_K <- temp_C + 273.15
  return(temp_K)
}

#freezing point
celsius_to_kelvin(0)

fahrenheit_to_kelvin <- function(temp_F){
  temp_C <- fahrenheit_to_celsius(temp_F)
  temp_K <- celsius_to_kelvin(temp_C)
  return(temp_K)
}

fahrenheit_to_kelvin(32)

celsius_to_kelvin(fahrenheit_to_celsius(32.0))

x <- c("A","B","C")
x
x <- c(x,"D")
x

highlight
content
wrapper

highlight <- function(content,wrapper){
  answer <- c(wrapper,content,wrapper)
  return(answer)
}


edges <- function(v){
  answer <- c(v[1],v[length(v)])
  return(answer)
}



  
  
  

