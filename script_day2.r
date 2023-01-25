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

analyze("data/inflammation-07.csv")



#LESSON 3: ANALYZING MULTIPLE DATASETS

best_practice <- c("Let", "the", "computer", "do", "the", "work")

print_words <- function(sentence){
  print(sentence[1])
  print(sentence[2])
  print(sentence[3])
  print(sentence[4])
  print(sentence[5])
  print(sentence[6])
  
}
print_words(best_practice)

print_words(best_practice[-6])

#loop
print_words <- function(sentence){
  for (word in sentence){
    print(word)
  }
}

print_words(best_practice)
print_words(best_practice[-6])


len <- 0
vowels <- c("a","e","i","o","u","y")

for (v in vowels){
  len <- len + 1
}
len


seq(86)
print_N(3)

print_N <- function(N){
  #use the seq function for this (and rename collection):

  nseq <- seq(N)
  
  for (num in nseq){
    #print() should be used here somewhere
    print(num)
  }
}

print_N(86)


ex_vec <- c(4,8,15,16,23,42)
total(ex_vec)

total <- function(vec){
  
  #something like we did with the len variable above
  sum <- 0 
  for (num in vec){
    #do something with collection
    #len <- len + 1
    sum <- sum + num
  }
  return(sum)

}


list.files(path = "data",pattern="csv")
list.files(path="data" , pattern = "inflammation")
list.files(path="data" , pattern = "inflammation",full.names = TRUE)

#regular expression
filenames <- list.files(path = "data",
                        pattern="inflammation-[0-9]{2}.csv",
                        full.names = TRUE)


for (f in filenames){
  browser()
  print(f)
  analyze(f)
}


analyze_all <- function(folder = "data", pattern){
  filenames <- list.files(path = folder,
                          pattern = pattern,
                          full.names = TRUE)
  
  
  for (f in filenames){
    print(f)
    analyze(f)
  }
}

analyze_all(folder = "data",pattern = "inflammation")

#making choices

#exporting to pdf
pdf("inflammation-01and2.pdf")
analyze("data/inflammation-01.csv")
analyze("data/inflammation-02.csv")
dev.off()


num <- 101
num < 100
num > 100

if (num > 100){
  print("greater")
}else {
  print("not greater")
}
print("done")



sign <- function(num){
  if (num > 0){
    return(1)
  } else if (num == 0){
    return(0)
  } else {
    return(-1)
  }
}

sign(0)

if (1 > 0 && -1 > 0) {
  print('both parts are true')
} else {
  print('at least one part is not true')
}

if (1 > 0 || -1 > 0) {
  print('one part is true')
} else {
  print('neither part is not true')
}


a <- NA
a == 1

if (is.na(a)){
  print("hi")
}


dat <- read.csv(file ="inflammation-01.csv",
                header = FALSE)
plot_dist(dat[1:5,10],threshold = 10)


plot_dist <- function(x,threshold){
  if (length(x) > threshold){
    #do this if TRUE
    boxplot(x)
  } else {
    #do this if FALSE
    stripchart(x)
  }

}

plot_dist(dat[,10],threshold = 10)



analyze <- function(filename,output=NULL){
  #function to produce plots of mean,min and max over time
  #takes a filename (csv) as argument
  
  if(!is.null(output)){
    pdf(output)
  }
  
  dat <- read.csv(file = filename, header=FALSE)
  
  avg_day_inflammation <- apply(dat,2,mean)
  plot(avg_day_inflammation)
  
  max_day_inflammation <- apply(dat,2,max)
  plot(max_day_inflammation)
  
  min_day_inflammation <- apply(dat,2,min)
  plot(min_day_inflammation)
  
  if (!is.null(output)){
    dev.off()
  }
}




analyze("data/inflammation-03.csv",
        "inflammation-03.pdf")

dir.create("results")


analyze("data/inflammation-03.csv",
        "results/inflammation-03.pdf")

f <- "inflammation-03.csv"
sub('csv','pdf',f)

file.path("results",sub('csv','pdf',f))
