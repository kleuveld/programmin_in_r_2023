#this is the "cheat" file containing functions
#used in exercises
#I loaded it in R by typing source("answers.R")
#authour: koen leuveld

highlight <- function(content,wrapper){
  answer <- c(wrapper,content,wrapper)
  return(answer)
}

edges <- function(v){
  first <- v[1]
  last <- v[length(v)]
  answer <- c(first,last)
  return(answer)
}

print_N <- function(N){
  nseq <- seq(N)
  for (num in nseq){
    print(num)
  }
}

total <- function(vec){
  vec_sum <- 0
  for (num in vec){
    vec_sum <- vec_sum + num
  }
  return(vec_sum)
}


expo <- function(base,power){
  result <- 1
  for (i in seq(power)){
    result <- result * base
  }
  return(result)
}


#lesson 3
plot_dist <- function(x, threshold){
  if(length(x) > threshold){
    boxplot(x)
  } else {
    stripchart(x)
  }
  
}

plot_dist <- function(x, threshold, use_boxplot = TRUE) {
  if (length(x) > threshold && use_boxplot) {
    boxplot(x)
  } 
  else if (length(x) > threshold && !use_boxplot) {
    hist(x)
  } else {
    stripchart(x)
  }
}
  