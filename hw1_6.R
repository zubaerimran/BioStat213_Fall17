#Generate a sequence of 25 independent Bernoulli random variables, each having parameter p = 0.8. 
#How many random numbers were needed?

#Initilization of counter variable
count <- 0

#Initialization of maximum limit
n<- 1000

#Initialization of random variables X[1], X[2],.....X[n]
X<-numeric(n)

p<-0.8

for (i in 1:n){
   X[i] <- as.integer(log(runif(1))/log(1-p))   #Calculate the geometric distribution for the success of a random variable X[i]
   if(X[i] == 1)
        count<-count+1   #Success counts
   if(count == 25)       #When we have 25 random variables succeeded, we are done   
         break
}

message(i, "Random numbers needed ")
