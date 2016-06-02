##Programming Assignment 1

#Part 1
###To be Added

##Part 2

complete<- function(directory, 	id=1:322){
		
		
		#initialize empty data frame
		result <- data.frame(id=numeric(),nobs=numeric())
		
		
		#Create a list of files
		files = dir(paste(getwd(),directory,sep="/"))
		
		for(i in id){
		#Extrace one of the relevant files
		dat  <- read.csv(paste(getwd(),directory,files[i],sep="/"))
		
		#Check no. of Obs
		nobs <- nrow(subset(dat,complete.cases(dat)))
		
		#Match it with its id
		add <- c(i, nobs)
		
		#Add it as another row
		result <- rbind(result, add)
		}
		
		#Give names to your columns
		names(result) <- c("id","nobs")
		print(result)
}

##Part 3

corr<- function(directory, threshold=0){
		
		#Initialize an empty result vector
		result = vector()
		
		#Get list of files
		files = dir(paste(getwd(),directory,sep="/"))
		
		k<- 1  #Counter Variable
		
		for(i in 1:length(files)){
			#Read one monitor
			dat  <- read.csv(paste(getwd(),directory,files[i],sep="/"))
			
			#Check for the number of observations
			nobs <- nrow(subset(dat,complete.cases(dat)))
				
				#Check for threshold
				if(nobs>=threshold){
					
					#Assign result
					result[k]<- cor(dat$nitrate,dat$sulfate,use="na.or.complete")
					k=k+1 #Increment Counter
				}	
		
		}
		
		#Check NAs introduced by cor()
		comp<-complete.cases(result)
		
		#Get the final observations
		result = subset(result, comp==T)
		return(result)
}
