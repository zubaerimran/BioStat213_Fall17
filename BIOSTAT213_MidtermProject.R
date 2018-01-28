## subroutine ##
## intensity rate in which the customers arrive ##
lambda_t <- function(time) 	 # the banks open from 8am to 5pm, so we set  
	{
		if(time >= 4 && time <= 5)	 # the time from 0 to 9 and the rate is 4/hr
 			{
		 		lambda <- 6	 	 # but increase to 6 between 4 and 5.
 	    			}
	 	else if ((time>=0 && time<=1) || (time>=8 && time<=9))   #Adjustment1: Rate is higher first hour after the bank opens and last hour before it closes
		 {				
		 	lambda <- 6								    #Arrival rate is assumed to be 6/hr
			}

		else 
		 { 
		 	lambda <- 4 
	    		}
  		
		list(lamt=lambda)
  		}



### subroutine ###

### Generate Tt which will be assigned to ta: next arrival time ###
 
generate_Tt <- function(ta,t1,t2,t3,t) 
	{

 		Tt <- t - (1/lambda_t(t)$lamt)*log(runif(1))	 # next arrival time
 
		if (ta < 4)    	 	 # ta is less than 4
 		{
 	 		ta <- Tt
 	 		if (ta > 4)       #if greater than 4, change rate	 
				{ 
                   ta <- 4+((ta-4)*4/6) 
                    }	 
     		}
 
		else if ( 4 <= ta && ta <= 5 ) 
 			{	 
				# 4 <= ta <= 5
 				ta <- Tt
 	 			if (ta > 5) 
				 { 
					ta <- 5+((ta-5)*6/4)         #if greater than 5 , change rate
					}	 
 				} 
 	 	else 
 	 		ta <- Tt
 
	  	if ( ta > 9) 
			ta <- 99999	 # To insure that nobody can enter the system after 5 pm
 	 	
		list(ta=ta)
	   }




### Situation ONE: One line with parallel servers

n.rep <- 10000
ave.1 <- numeric(n.rep)

for (j in 1:n.rep) 
 {
	## Main Program ##
	## Initialization ##
	t <- 0	 	 # current time
	T <- 9	 	 # after T, no one can enter this system
	n <- 0	 	 # number of people is in the system in the time t
	Na <- 0	 	 # total number of arrival
	
	C1 <- C2 <- C3 <- 0	 # Ci is the number of customers served by i by time t
	Nd <- 0	 	 # total number of departure
	T0 <- numeric(1)
	r1 <- 6	 	 # the service time for server i has exponential distn with rate ri
	r2 <- 5
	r3 <- 4

	lambda <- 6
	SS <- c(0,0,0,0)	 # SS(n,i1,i2,i3) n:number of customers in the system
 	 	 			 # ij: person i is being severed by server j
 
	t <- t- (1/lambda_t(t)$lamt)*log(runif(1))	 # generate the first arrival time

	ta <- T0 <- t	 	 # next arrival
	t1 <- t2 <- t3 <- 99999	 # ti is the service completion time of the customer, 
                                #presently being served by server i
	
	ANa <- numeric()	 # the arrival time of customer n
	DNd <- numeric()	 # the departure time of customer n
 

	repeat #repeat starts
 	{
 		if(ta==99999 && ta==t1 && t1==t2 && t2==t3)   #if0 starts
				break          #if0 ends  #Abnormal termination   

		### CASE 1
	
	 	if (ta == min(ta,t1,t2,t3)) 	 #if1 starts within repeat  #Case 1  
 		 { 
 	 		t <- ta
 	 		Na <- Na + 1
 	 		ANa[Na] <- t	 	 
 	 	
			ta <- generate_Tt(ta,t1,t2,t3,t)$ta	 # NOtice: the change of rate
 	 
 		 	if (SS[1] == 0)  #if2 starts within if1 	 	 	 # nobody is in the system
 	 		 {
		 		SS <- c(1,Na,0,0)	 	 # go to server 1 if all are free
				abnormal_t<-0
				if (runif(1,0,1)>=0.9)       #Adjustment2: abnormal system behavior might interrupt the service
 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins

				t1 <- t - (1/r1)*log(runif(1)) + abnormal_t	 # the time of server being completed (delay is added if occurs))
 	 			}   #if2 ends

			else if (SS[1] == 1)  	#elseif1 starts within if1 	 # only one is in the system
 		 	 {
			 	 SS[1] <- 2
 	 	 		 if(SS[3]==0 && SS[4]==0) #if2 starts within elseif1
					{	 	 	 	 	 
 	 	 	 	 	SS[3] <- Na
					abnormal_t<-0

					if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
	 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins

 	 	 	 	 	t2 <- t-(1/0.95*r2)*log(runif(1)) + abnormal_t  #Teller2 is assumed to have 95% efficiency of the usual
 	 	 	 	  	}   #if2 ends

	        		 else if (SS[2] == 0 && SS[4] == 0) #elseif2 of if2 within elseif1
			 	  {
 	 				SS[2] <- Na
					abnormal_t<-0

					if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
	 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins

 	 	 			t1 <- t-(1/r1)*log(runif(1)) + abnormal_t
 	 	 	 		}      #elseif2 ends

				 else if (SS[2] == 0 && SS[3] == 0) #elseif3 of if2 within elseif1 
				  {
 	 		 	 	 SS[2] <- Na
					abnormal_t<-0					
					if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
	 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins
	
 	 	 		 	 t1 <- t-(1/r1)*log(runif(1)) + abnormal_t	 
 	 	 	 		}       #elseif3 ends
	 			}   #elseif1 ends

	  		else if (SS[1] == 2) 	#elseif4 starts within if1 	 # two customers are in the system
 			{
 	 			 SS[1] <- 3
 	 	 		if(SS[4]==0) #if3 starts within elseif4
					{	 	 	 	 	 
 	 	 				SS[4] <- Na
						abnormal_t<-0
						if (runif(1,0,1)>=0.9)       #abnormal system behavior might interrupt the service
		 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins


 	 	 	 			t3 <- t-(1/0.9*r3)*log(runif(1)) + abnormal_t     #Teller3 is assumed to have 90% efficiency of the usual efficiency
 	 	 	 			}    #if3 ends

		 		else if (SS[3] == 0)   #elseif5 starts within elseif4
					{
 	 		 	 	 SS[3] <- Na
					 
					 abnormal_t<-0
		       		 if (runif(1,0,1)>=0.9)       #abnormal system behavior might interrupt the service
	 	 	 		      abnormal_t2<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins

 	 	 		 	 t2 <- t-(1/0.95*r2)*log(runif(1)) + abnormal_t
 	 		 		 }       #elseif5 ends

				else if (SS[2] == 0)    #elseif6 starts within elseif4
					{
 	 		 	 	 SS[2] <- Na
					 abnormal_t<-0
					 if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		      	abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins

 	 	 		 	 t1 <- t-(1/r1)*log(runif(1)) + abnormal_t 
 	 		 		 }      #elseif6 ends
	
    		  	 }        #elseif4 ends

	
			else if (SS[1] > 2) 	#elseif7 starts within if1 	 	 # more than 2 customers
 	 			{ 
					SS[1] <- SS[1]+1
 	 	    			}     #elseif7 ends

		}    #if1 ends
	 

  
	### CASE 2	 
 	else if ( t1 == min(ta,t1,t2,t3))  #elseif8 starts  #Case 2
   	{
 		t <- t1
 		C1 <- C1 + 1	 	 	 	 # number of customers served by 1
 		DNd[SS[2]] <- t	 	 	 # the departure time for SS[2]
 
		if (SS[1]==1) 	#if4 starts  within elseif8	 	 	 # one customer in SS
 		   {
			t1 <- 99999
 
		 	SS <- c(0,0,0,0)
    	     }          #if4 ends
	
		else if (SS[1]==2) #elseif9 starts within elseif8	 	 	 # Two customer 
 	  	{	 
			t1 <- 99999
 	 		if (SS[4] == 0) #if5 starts within elseif9
		  	 {
 	 	    		SS <- c(1,0,SS[3],0)
 	 	 		}       #if5 ends

			else if ( SS[3] == 0)   #elseif10 starts within elseif9
			    {
 	 		 	 SS <- c(1,0,0,SS[4])
             		  }     #elseif10 ends

		 	 }    #elseif9 ends
 	
		else if (SS[1]==3) #elseif11 starts within elseif8  	 	 	 # Three Customer
 	  	{	 
			t1 <-99999
 	 		SS <- c(2,0,SS[3],SS[4])
 		 	}	#elseif11 ends
    
		else if(SS[1] >3) #elseif12 starts within elseif8	 	 	 # more than three customer
		  { 	 	
 			m <- max(SS[2],SS[3],SS[4])
 		 	SS <- c((SS[1]-1),(m+1), SS[3], SS[4])
			abnormal_t<-0
			if (runif(1,0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins
	
 	 		t1 <- t-(1/r1)*log(runif(1)) + abnormal_t
 	 		}    #elseif12 ends

	     }      #elseif8 ends




 
	### CASE 3
 	else if (t2 == min(ta,t1,t2,t3)) #elseif13 starts # Case 3
 		 {
			t <- t2
 	 		C2 <- C2 + 1	 	 	 	 # number of customers served by 2
 	 		DNd[SS[3]] <- t	 	 	 # the departure time for SS[3]
 
 	 		if (SS[1]==1) #if6 starts within elseif13 	 	 # one customer in SS
 	 	  	{	
				t2 <- 99999
 	 			SS <- c(0,0,0,0)
            		}      #if6 ends
		
			else if (SS[1]==2) #elseif14 of if6 starts within elseif13	 	 	 # Two customer 
 		 	  {
				t2 <- 99999
 	 	 		if (SS[4] == 0) #if7 starts within elseif14
			 	 {
 	 	 	 		SS <- c(1,SS[2],0,0)
 	 	 			} #if7 ends

				else if ( SS[2] == 0)    #elseif15 of if7 starts within elseif14
				  {
 	 		 	 	SS <- c(1,0,0,SS[4])
             		     }   #elseif15 ends

	 			 }     #elseif14 ends
 
			else if (SS[1]==3)  #elseif16 starts within elseif13	  # Three Customer
 	 		  {
				t2 <-99999
 	 	 		SS <- c(2,SS[2],0,SS[4])
 			  	}     #elseif16 ends
		
			else if(SS[1] >3)   #elseif17 starts within elseif13 	 	 	 # more than three customer
 		 	  {
				m <- max(SS[2],SS[3],SS[4])
 	 	 		SS <- c((SS[1]-1), SS[2],(m+1), SS[4])
				abnormal_t<-0
				if (runif(1,0,1)>0.9)       #abnormal system behavior might interrupt the service
 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins
				

 	 	 		t2 <- t-(1/0.95*r2)*log(runif(1)) + abnormal_t
 	 			}   #elseif17 ends

	       } #elseif13 ends 




	### CASE 4
  	else if ( t3 == min(ta,t1,t2,t3))   #elseif18 starts    # Case 4
    	{
 		 t <- t3
 	 	C3 <- C3 + 1	 	 	 	 # number of customers served by 3
 	 	DNd[SS[4]] <- t	 	 	 # the departure time for SS[4]
 
	 	 if (SS[1]==1)   #if8 starts within elseif18 	 # one customer in SS
 		   { 
			t3 <- 99999
 			SS <- c(0,0,0,0)
  		  	}     #if8 ends
     
		else if (SS[1]==2) #elseif19 starts within elseif18	 	 	 # Two customer
 		   { 
 	 		t3 <- 99999
 	 	 
			if (SS[3] == 0) #if9 starts within elseif19
			  {
 	 		 	SS <- c(1,SS[2],0,0)
 	 	 		 }    #if9 ends

	          else if ( SS[2] == 0)     #elseif20 starts within elseif19
				{
 	 		 	 SS <- c(1,0,SS[3],0)
 				   }      #elseif20 ends

	 	 	}    #elseif18 ends
 
		else if (SS[1]==3)    #elseif21 starts within elseif18	 	 	 # Three Customer
    	   {
			t3 <-99999
 	 		SS <- c(2,SS[2],SS[3],0)
 			}     #elseif21 ends

		else if(SS[1] >3)   #elseif22 starts within elseif18 	 	 	 # more than three customer
 		 { 
			m <- max(SS[2],SS[3],SS[4])
 	 		SS <- c((SS[1]-1), SS[2], SS[3], (m+1))

			if (runif(1,0,1)>0.9)       #abnormal system behavior might interrupt the service
 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins


 	 		t3 <- t-(1/0.9*r3)*log(runif(1)) + abnormal_t
 	 		}       #elseif22 ends
 
	 }      #elseif18 ends



    } #repeat1 ends

	ave.1[j]<- mean(DNd-ANa)

  }     #for-loop1 ends



  mean(ave.1)










### Situation TWO : Tandem system
 
n.rep <- 10
ave.2 <- numeric(n.rep)

for (j in 1:n.rep)    #for-loop2 starts
 {
 
 
 ### Main Program ###
 ## Initialization ##
 t <- 0	 	 	 # current time
 Na <- 0	 	 	 # total number of arrival
 Nd <- 0	 	 	 # total number of departure
 SS <- c(0,0,0)	 # SS(n1,n2,n3) ni:number of customers in the server i
 n1 <- n2 <- n3 <- 0
 
 t <- t- (1/lambda_t(t)$lamt)*log(runif(1))	 # generate the first arrival time
 ta <- T0 <- t	 	 # next arrival
 t1 <- t2 <- t3 <- 99999	 # ti is the service completion time of the customer
				 	 	 # presently being served by server i

 ANa.1 <- numeric()	 # the arrival time of customer n
 ANa.2 <- numeric()	 # the arrival time of customer n at server 2
 ANa.3 <- numeric()  # the arrival time of customer n at server 3
 ANa <- numeric()
 DNd <- numeric()	 # the departure time of customer n
 
 repeat          #repeat2 starts
  {
	if(ta==99999 && ta==t1 && t1==t2 && t2==t3) #if10 starts
		break         #if10 ends

     	### CASE 1
 	
	if (ta == min(ta,t1,t2,t3))   #if11 starts	 # Case 1
 	  {
		t <- ta
 		Na <- Na + 1
 	 	n1 <- n1 + 1
 	 	ta <- generate_Tt(ta,t1,t2,t3,t)$ta	 # NOtice: the change of rate
 	 	
		if (n1 == 1)    #if12 starts within if11
		  {   abnormal_t<-0
 	 		if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins

			t1 <- t - (1/r1)*log(runif(1)) + abnormal_t	 # the time of service being completed
 	 		}    #if12 ends

 	 	ANa.1[Na] <- t
#	 	ANa[Na] <- t	 
        }       #if11 ends 

		### CASE 2	 
 	else if ( t1 == min(ta,t1,t2,t3))   #elseif23 starts # Case 2
 	 {
  		t <- t1
 		n1 <- n1 - 1
 	 	n2 <- n2 + 1

 	 	if (n1 == 0)    #if13 starts within else23
		  {
 	 	 	t1 <- 99999
 	 		} 

		else 
		 {    abnormal_t<-0
 	 	 	if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins

			t1 <- t -(1/r1)*log(runif(1)) + abnormal_t
 	 		}

 		if (n2 == 1)
		 { 	abnormal_t<-0
			if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins

			t2 <- t -(1/0.95*r2)*log(runif(1)) + abnormal_t
 			  }

 	 	ANa.2[(Na-n1)] <- t
 
 	 } 	 	 
 

		### CASE 3
	else if ( t2 == min(ta,t1,t2,t3))  # Case 3
	   {
 	 	t <- t2
 	 	n2 <- n2 - 1
 	 	n3 <- n3 + 1
 	 	if (n2 == 0) 
		  {
 	 	 	t2 <- 99999
 	 		 } 
	 	else 
		{     abnormal_t<-0
			if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins

 	 		t2 <- t -(1/0.95*r2)*log(runif(1)) + abnormal_t
 	 		}

	 	if (n3 == 1)
		  { abnormal_t<-0

			if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins

			t3 <- t -(1/0.9*r3)*log(runif(1)) + abnormal_t 
			 }
     	
		ANa.3[(Na-n1-n2)] <- t
  	   }

		

		### CASE 4
  	else if ( t3 == min(ta,t1,t2,t3))  # Case 4
	   { 
 	 	t <- t3
 	 	n3 <- n3 - 1
 	 	Nd <- Nd + 1
 	 	
		if (n3 == 0) 
		  {
 	 	 	t3 <- 99999
 	 		 } 
		
		else 
		 {   abnormal_t<-0
 	 	 	if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		      abnormal_t<- runif(1, 0, 0.5)   #Assume it may pause the service for 0-30 mins

			t3 <- t -(1/0.9*r3)*log(runif(1)) + abnormal_t
 	 		}

	 	DNd[(Nd)] <- t
	    }

	}     #repeat ends

 ave.2[j] <- mean(DNd-ANa.1)
 }   #for-loop ends

mean(ave.2)







### Situation THREE: each teller has their own line

n.rep <- 10000
ave.3 <- numeric(n.rep)

for (j in 1:n.rep) 
 {

 ### Main Program ###
 ## Initialization ##
 t <- 0	 	 # current time
 T <- 9	 	 # after T, no one can enter this system
 Na <- 0	 	 # total number of arrival
 S1 <- S2 <- S3 <- numeric()	 # Si is the sequence of customers in the server i at time t
 Nd <- 0	 	 # total number of departure

 T0 <- numeric(1)
 r1 <- 6	 	 # the service time for server i has exponential distn with rate ri
 r2 <- 5
 r3 <- 4

 t <- t- (1/lambda_t(t)$lamt)*log(runif(1))	 # generate the first arrival time
 ta <- T0 <- t	 	 # next arrival
 t1 <- t2 <- t3 <- 99999	 # ti is the service completion time of the customer
				 	 	 # presently being served by server i
 ANa <- numeric()	 # the arrival time of customer n
 DNd <- numeric()	 # the departure time of customer n
 
 repeat {
	if(ta==99999 && ta==t1 && t1==t2 && t2==t3) 
			break

		### CASE 1
 	
	if (ta == min(ta,t1,t2,t3)) 	 # Case 1
 	  {
		t <- ta
 	 	Na <- Na + 1
 	 	ta <- generate_Tt(ta,t1,t2,t3,t)$ta  # new arrival time
 	 	ANa[Na] <- t

 	if (length(S1) == min(length(S1),length(S2),length(S3))) 	 # enter 1
 	   {	 
		S1[length(S1)+1] <- Na	 # customer Na enter the server 1
 	 	 
		if (length(S1) == 1) 
		  { abnormal_t<-0
 	 	 	if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		abnormal_t<-runif(1, 0, 0.5)
			t1 <- t - (1/r1)*log(runif(1)) + abnormal_t	 # the time of server being completed	 	 
 	 	 	}
 	 	} 
	
	else if (length(S2) == min(length(S1),length(S2),length(S3))) 	  # enter 2
 	 	{
		 S2[length(S2)+1] <- Na	 # customer Na enter the sever 2
 	 	 
 		 if (length(S2) == 1) 
		  {   abnormal_t<- 0
 	 	 	if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		abnormal_t<-runif(1, 0, 0.5)	
			t2 <- t - (1/0.95*r2)*log(runif(1)) + abnormal_t	 # the time of server being completed
 	 	 	  }
 	 	  } 
	
	else if (length(S3) == min(length(S1),length(S2),length(S3))) 	  # enter 2
		{
 	 	 S3[length(S3)+1] <- Na	 # customer Na enter the sever 3
 	 	
		 if (length(S3) == 1) 
			{abnormal_t<-0 
			if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		abnormal_t<-runif(1, 0, 0.5)
			t3 <- t - (1/0.9*r3)*log(runif(1)) + abnormal_t	 # the time of server being completed
 	 		  }
 	       }	 	 	 
 	 
       }

 

### CASE 2	 

	else if ( t1 == min(ta,t1,t2,t3))  # Case 2
      {
    		t <- t1
 	 	DNd[S1[1]] <- t1	 # The first in server will leave
 	 	S1 <- S1[-1]	 # the first leave
 	
	 	if (length(S1) == 0) 
		    	t1 <- 99999
 	
	 	if (length(S1) > 0) 
			{abnormal_t<-0
			if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		abnormal_t<-runif(1, 0, 0.5)
			t1 <- t - (1/r1)*log(runif(1)) + abnormal_t	 # the time of server being completed
			 }	 	   
		}
 

### CASE 3
  	else if ( t2 == min(ta,t1,t2,t3))  # Case 3
  	  { 
		t <- t2
 		DNd[S2[1]] <- t2	 # The first in server will leave
  		S2 <- S2[-1]	 # the first leave
 	 
		if (length(S2) == 0) 
		    	t2 <- 99999
 	
		if (length(S2) > 0) 
			{abnormal_t<-0
			if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		abnormal_t<-runif(1, 0, 0.5)
			t2 <- t - (1/0.95*r2)*log(runif(1)) + abnormal_t	 # the time of server being completed
          		}	
		}

     

### CASE 4
  else if ( t3 == min(ta,t1,t2,t3))  # Case 4
    {
		t <- t3
 		DNd[S3[1]] <- t3	 # The first in server will leave
 		S3 <- S3[-1]	 # the first leave
 	 
		if (length(S3) == 0) 
		 	t3 <- 99999
 	
		if (length(S3) > 0) 
		     {abnormal_t<-0
			if (runif(1, 0,1)>=0.9)       #abnormal system behavior might interrupt the service
 	 	 		abnormal_t<-runif(1, 0, 0.5)                    #Assume it may pause the service for 0-30 mins
			t3 <- t - (1/0.9*r3)*log(runif(1)) + abnormal_t	 # the time of server being completed
 			}
		}


  }      #repeat ends
 
ave.3[j] <- mean(DNd-ANa)

 }   #for-loop ends

mean(ave.3)





mean(ave.1)
sd(ave.1)

mean(ave.2)
sd(ave.2)

mean(ave.3)
sd(ave.3)

min(mean(ave.1), mean(ave.2), mean(ave.3))








