#functions have a scope

input <- 86

add_two <- function(input){
  output <- input + 2
  return(output)
}

add_two(5)
input

center <- function(data, midpoint){
  new_data <- (data - mean(data)) + midpoint
  return(new_data)
}

quick_testdata <- c(0,0,0,0)
center(quick_testdata,86)

dat <- read.csv(file = "data/inflammation-01.csv",header=FALSE)

centered <- center(dat[,4],0)
centered

mean(dat[,4])
mean(centered)

sd(dat[,4])
sd(centered)

sd(dat[,4]) - sd(centered)

all.equal(sd(dat[,4]),sd(centered))



#missing values
datNA <- dat
datNA[10,4] <- NA

center(datNA[,4],0)

center <- function(data, midpoint){
  #function that returns a new vector containing the original
  #data centered around midpoint
  #example: center(c(1,2,3),0) ->  c(-1,0,1)
  new_data <- (data - mean(data,na.rm=TRUE)) + midpoint
  return(new_data)
}
center(datNA[,4],0)

datNA[,5] <- as.factor(datNA[,5])
datNA[,6] <- as.character(datNA[,6])

center(datNA[,5])
center(datNA[,6])


#plotting a bunch of useful summary statistics by day
dat <- read.csv(file = "data/inflammation-01.csv",header=FALSE)

avg_day_inflammation <- apply(dat,2,mean)
plot(avg_day_inflammation)

max_day_inflammation <- apply(dat,2,max)
plot(max_day_inflammation)

min_day_inflammation <- apply(dat,2,min)
plot(min_day_inflammation)







analyze <- function(filename){
  #function to produce plots of mean,min and max over time
  #takes a filename (csv) as argument
  dat <- read.csv(file = filename, header=FALSE)
  
  avg_day_inflammation <- apply(dat,2,mean)
  plot(avg_day_inflammation)
  
  max_day_inflammation <- apply(dat,2,max)
  plot(max_day_inflammation)
  
  min_day_inflammation <- apply(dat,2,min)
  plot(min_day_inflammation)
}

analyze("data/inflammation-03.csv")



#LESSON 3: ANALYZING MULTIPLE DATASETS


