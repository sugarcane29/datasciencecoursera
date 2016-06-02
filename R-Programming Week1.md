##Programming Assignment 1

#Part 1
##To be Added##

Part 2

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

