#Generation of n values frin the probability mass function P1= 1/3 and P2 = 2/3

#a) n=100, X is a vector of n-values generated from the given probability mas functions

n = 100
P1 = 1/3
P2 = 2/3

X<- 1 + (runif(n) > P1)


#Proportion of values that are equal to 1, p is the Percentage of 1's in X

l <- sum(X==1)
p <- (l/n)*100

message("Percentage of the n= ", n, " values that are equal to 1: ", p, " percent")


#b) n=1000, Y is a vector of n-values generated from the given probability mas functions

n = 1000
P1 = 1/3
P2 = 2/3

Y<- 1 + (runif(n) > P1)


#Proportion of values that are equal to 1, p is the Percentage of 1's in Y

l <- sum(Y==1)
p <- (l/n)*100

message("Percentage of the n= ", n, " values that are equal to 1: ", p, " percent")



#c) n=10000, Z is a vector of n-values generated from the given probability mas functions

n = 10000
P1 = 1/3
P2 = 2/3

Z<- 1 + (runif(n) > P1)


#Proportion of values that are equal to 1, p is the Percentage of 1's in Z

l <- sum(Z==1)
p <- (l/n)*100

message("Percentage of the n= ", n, " values that are equal to 1: ", p, " myindepercent")