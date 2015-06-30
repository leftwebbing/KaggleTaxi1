dfNames <- c("TRIP_ID","CALL_TYPE","ORIGIN_CALL","ORIGIN_STAND","TAXI_ID",     
"TIMESTAMP","DAY_TYPE","MISSING_DATA","POLYLINE" ) # data frame names
trainDF <- read.csv("trainMod.csv", sep = ";", stringsAsFactors = FALSE,col.names = dfNames ,nrows =45000, skip = 248958 )  # read only few lines from train its 1.5gig BIG
testDF <- read.csv("test.csv", stringsAsFactors = FALSE ) #load all test data its short


parsedTest <- lapply(testDF$POLYLINE, parseIntoPoints)  # list of parsed TEST trips
parsedTrain <-lapply(trainDF$POLYLINE, parseIntoPoints) # returns list of parsed TRAIN coordinates

countIdx2 <-expand.grid(test=1:nrow(testDF),train= 1:nrow(trainDF)) # makes permutation 
countIdx2 <-data.frame(countIdx2 ) # convert to DF


trainStarts<-sapply(parsedTrain, function(s){ return(s[1,1:2])}) # returns vector of first Lon,Lat = srarts of TRAIN
trainEnds <- sapply(parsedTrain, function(s){ return(s[nrow(s),1:2])}) 
testStarts<- sapply(parsedTest, function(s){ return(s[1,1:2])}) # returns vector of first Lon,Lat = srarts of TRAIN
testEnds <-  sapply(parsedTest, function(s){ return(s[nrow(s),1:2])}) 

trainStarts<-t(trainStarts) #transposed
testStarts <-t(testStarts) # transposed
trainEnds  <-t(trainEnds ) # transposed
testEnds   <-t(testEnds ) # transposed

trainTotDist<- sapply(parsedTrain, function(s){ return(s[nrow(s),3])}) # returns vector of last elements = total lengths of TRAIN
trainTotDist<- as.vector(trainTotDist)  #vectorize
testTotDist <- sapply(parsedTest, function(s){ return(s[nrow(s),3])}) # vector of total distances of TEST
testTotDist <- as.vector(testTotDist )  # vectorize

# options(error = recover) # catches error
journeyAreas <-mapply(function(test,train){  # returns vector of each partial journey area 
areaInJourneyPartial(test,train)}, 
test = parsedTest[countIdx2[,1]], train = parsedTrain[countIdx2[,2]])


countIdx2 <- cbind(countIdx2, journeyAreas ) # append partial areas to countIdx 
countIdx2 <-countIdx2[countIdx2$journeyAreas > 0,] # select all without error codes 
#countIdx2 <- countIdx2[order(countIdx2[,1]),] #sort by TEST
countIdx2 <- cbind(countIdx2, trainEnds[countIdx2[,2], ] )#append ENDS for corresponding TRAIN journeys
colnames(countIdx2) <- c("test", "train","journeyAreas", "Lon", "Lat") #set colnames

testThreshold = 0.001 * pi * testTotDist**2 # set threshold to 0.001 * pi * r^2, 0.1% of area of circle starting from origin
# later subset all countIdx where area < testThreshold 

#function to threshold all countIdx2 according to group and testThreshold
countIdxThresholded <- apply(countIdx2, 1, function(x) {if(x[3]<testThreshold[x[1]] | is.na(x[3]) ) x else NULL  })

rmNull <- function(x) {
   x <- Filter(Negate(is.null), x)
   lapply(x, function(x) if (is.list(x)) rmNull(x) else x)
}
countIdxThresholded <-rmNull( countIdxThresholded ) # remove null entries 
countIdxThresholded <-data.frame(matrix(unlist(countIdxThresholded ), ncol=5, byrow=T),stringsAsFactors=FALSE)
names(countIdxThresholded ) <- names (countIdx2)

submit3<-countIdxThresholded %>%
  group_by(test) %>%
    summarize(meanLat =mean(Lat, na.rm =TRUE),meanLon =mean(Lon, na.rm =TRUE),meanArea = mean(journeyAreas, na.rm =TRUE) )
#plot(countIdxThresholded[526:548,4:5]) 

submit3 <-data.frame(submit3) # data from 2nd pass, thresholded then averaged
submitDF <- data.frame(testEnds) # copy last known TEST points
submitDF$copied <- FALSE # column indicates whether copied from countIdxAg

replaceIdx3 <- submit3[!is.nan(submit3$meanArea),]$test # get test values to replace
submitDF$allLon[replaceIdx3] <-submit3[!is.na(submit3$meanArea),]$meanLon #replace lon
submitDF$allLat[replaceIdx3] <-submit3[!is.na(submit3$meanArea),]$meanLat #replace lat
submitDF$copied[replaceIdx3] <-TRUE # set flag = TRUE 
submitDF$TRIP_ID <- testDF$TRIP_ID
submitDF <- submitDF[,c(4,2,1,3)]

names(submitDF) <- c("TRIP_ID", "LATITUDE","LONGITUDE","copied" )

write.csv(submitDF[,1:3], "submitEntry.csv", quote = FALSE, row.names = FALSE)
