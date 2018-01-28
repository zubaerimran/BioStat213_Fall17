'''A pair of dice are to be continually rolled until all the possible outcomes 
2, 3, ..., 12 have occured at least once. Develop a simulation study to 
estimate the expected number of dice rolls that are needed.'''
 
n = 10000
myindex=numeric(n)

for (i in 1:n){
  X = c(1:6)
  index = 0
  Y = c(2:12)
  
  B = numeric(2)
  C = c(0)
  while(sum(Y)> 0){
    B<- sample(X,2,replace=T)
    C = sum(B)
    Y = setdiff(Y,C)
    index = index+1
    sum(Y)
  }
  myindex[i] = index 
}
mean(myindex)
