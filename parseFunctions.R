#helper function
# parses list of GPS coordinates

#input = string of coordinates
#outputs = 
#    number of points on trip
#    total distance of trip from adding distances
#    start coordinate
#    end coordinate
#    measure of tortuosity / waviness / irregularity
#    total duration of trip by couning # points

#install.packages('geosphere')
library(geosphere)
library(stringr)
library(geosphere)

parseStringGPS <- function(inString) {

# format of GPS coordinates = [LONGITUDE, LATITUDE]
# list contains one pair of coordinates for each 15 seconds of trip

inString  <- str_split(inString , ",|\\[|\\]" )# separates out numbers only
inString  <- unlist(inString  ) # convert to vector
inString  <- inString [inString  != ""] # remove blank entries
inString  <- as.numeric(inString  )  #convert to number


if(is.na(inString[1]) ){ # protect against crashes from garbage input
  outDF <- data.frame(numPoints=0, crowDistance = NA, dotsDistance = NA, lonFirst = NA, latFirst = NA, lonLast = NA, latLast = NA, tortuosity = NA, timeMinutes = NA)
  return(outDF ) 
}

allLon <- inString [c(TRUE,FALSE)]  #select only evens
allLat <- inString [c(FALSE,TRUE)]  #select only odds
allPoints <- 

numPoints <- length(allLat ) # count number coordinates


if (numPoints ==1) { #catch case of one point
  outDF <- data.frame(numPoints, crowDistance =0, dotsDistance = 0, lonFirst=allLon , latFirst=allLat , lonLast=allLon , latLast=allLat , tortuosity = NA, timeMinutes = 0.25)
  return(outDF ) 
}

# select first, last coordinates
lonFirst <- allLon[1]
latFirst <- allLat[1]
lonLast <- allLon[numPoints ]
latLast <- allLat[numPoints ]

latLonMatrix <- cbind(allLon,allLat)  #bind into matrix [lon,lat]
latLonMatrix1 <- rbind(c(0,0),latLonMatrix ) # add dummy to start
latLonMatrix <- rbind(latLonMatrix, c(0,0)) # add dummy to end

distances <- distHaversine(latLonMatrix,latLonMatrix1  ) # compute distances
distances <- distances[-c(1,length(distances))] # remove first, last dummies
allBearings <- bearing(latLonMatrix1, latLonMatrix ) # compute bearings for each 
allBearings <- allBearings[-c(1,length(allBearings))] # remove first, last dummies

allBearings1 <- c(0,allBearings ) # add first 0, offset
allBearings <- c(allBearings, 0) # add filler at end

# use function bearing(p1, p2) from {geosphere}
angles = abs(allBearings - allBearings1) # calculate differences
if(length(angles) ==2) 
  { 
  angles <-angles[1]
  }
else angles <- angles[-c(1,length(angles))] # remove first, last dummies
angles <- sapply(angles, function( x ){ # limit to 180 
  if (is.na(x ) | is.nan(x ) |is.null(x)) return(x) 
  if(x> 180) return(360-x)
  return(x)
})

# remove last dummy, delete offset matrix
latLonMatrix <- latLonMatrix[-(numPoints+1) , ]# remove last dummy
latLonMatrix1 <- NULL

dotsDistance <-sum(distances )# total length of trip connecting each point
crowDistance <- distHaversine(latLonMatrix[1,], latLonMatrix[numPoints, ])
timeMinutes <- numPoints * 0.25

#simple measure of tortuosity = [dots distance, curve] / [linear crow distance]
#tortuosity <- dotsDistance / crowDistance 
#return(NULL)

# revised measure of tortuosity = [sum of angles between each pair segments] / [dots distance, curve]
# value of ZERO for straight path, of any number of segments
# value increases towards infinity with more angles and shorter distances

#plot(latLonMatrix)  #REMOVE after writing
#plot(latLonMatrix,xlim=c(-8.67,-8.63 ), ylim =c(41.167,41.19))  #REMOVE after writing
#lines(latLonMatrix)

#print(numPoints)
tortuosity <- sum(angles, na.rm=TRUE) / dotsDistance
outDF <- data.frame(numPoints, crowDistance, dotsDistance, lonFirst, latFirst, lonLast, latLast, tortuosity, timeMinutes)
return(outDF ) 

}

parseIntoPoints <- function(inString){ # return matrix of Lon, Lat, cumulativeDistances

if(is.null(inString)) return(cbind(NA,NA,0))

inString  <- str_split(inString , ",|\\[|\\]" )# separates out numbers only
inString  <- unlist(inString  ) # convert to vector
inString  <- inString[inString != ""] # remove blank entries
inString  <- as.numeric(inString  )  #convert to number
allLon <- inString [c(TRUE,FALSE)]  #select only evens
allLat <- inString [c(FALSE,TRUE)]  #select only odds

if(length(allLat) <2) return(cbind(allLon,allLat,0))
latLonMatrix <- cbind(allLon,allLat)  #bind into matrix [lon,lat]
latLonMatrix1 <- rbind(c(0,0),latLonMatrix[1:nrow(latLonMatrix )-1,] ) # add dummy to start

distances <- distHaversine(latLonMatrix[2:nrow(latLonMatrix ),],latLonMatrix1[2:nrow(latLonMatrix ),]  ) # compute distances from 2nd to last
distances <- c(0,distances) #add filler at start
return(cbind(allLon,allLat, cumDist= cumsum(distances )))  #bind Lon, Lat, cumDist

}


#References
#Sinnott, R.W, 1984. Virtues of the Haversine. Sky and Telescope 68(2): 159 


areaInJourney <-function(journey1, journey2) {
# accepts 2 GPS journeys in format [lon,lat]
# returns area inside polygon 
# algorithm:
# reverse order of journey2
# concatenate journey1 + reverse(journey2)
# calculate area with areaPolygon()


#reverse journey 2
journey2 <- journey2[ rev(seq_len(nrow(journey2 ))),]
# return area enclosed in looped route
return(areaPolygon(rbind(journey1 ,journey2 )) )
}

areaInJourneyPartial <-function(journeyTEST, journeyTRAIN) {
#areaInJourneyPartial <-function(journeyTEST, journeyTRAIN, typeTEST, typeTRAIN) {
# compares journeyTEST to partial journeyTRAIN 
# accepts 2 GPS journeys in format [lon,lat,cumDist]
# selects subset of journeyTRAIN that is just 1 point shorter of journeyTEST
# returns area inside polygon 
# algorithm:
# find total length of journeyTEST = testTotDist = journeyTEST[ncol(journeyTEST),3] 
# subset journeyTRAIN according to all points under testTotDist = journeyTRAIN[journeyTRAIN[,3]<testTotDist, 1:2] 
# concatenate subset journeyTRAIN + reverse(journeyTEST )
# calculate area with areaPolygon()
# error values : -1 = if fewer than 3 unique lon/lat
#                -2 = if endpoints too distant, beyond 200 meters

#journeyTRAIN <-matrix(unlist(journeyTRAIN),ncol=3)
#journeyTEST <-matrix(unlist(journeyTEST),ncol=3)

#if(length(journeyTEST) <2|length(journeyTRAIN) <2) return(NA)# if not polygon return NA
#if(!is.matrix(journeyTEST) |!is.matrix(journeyTRAIN) ) return(NA)# doesnt catch all errors
#if(nrow(journeyTEST) <3|nrow(journeyTRAIN) <3) return(NA)# if either input shorter than 3, return NA

if( is.null(journeyTRAIN ) |is.null(journeyTEST ) ) return( -5) # one or more NULL input 

testTotDist  <- journeyTEST[nrow(journeyTEST),3]  # pick last row,col 3, total length journeyTEST 
journeyTRAIN <- journeyTRAIN[journeyTRAIN[,3]<testTotDist, 1:2] # subset journeyTRAIN to points under testTotDist, select only lon,lat 

if( is.null(nrow(journeyTRAIN ))  ) return( -44) # only one row train
if( is.null(nrow(journeyTEST  ))  ) return( -4) # only one row test 

if(nrow(journeyTEST) <3|nrow(journeyTRAIN) <3) return(-3)# if either input shorter than 3, return NA

if(nrow(journeyTEST) <10|nrow(journeyTRAIN) <10){
  if(length(unique(journeyTRAIN[,1])) <3) return(-11)# if less than 2 unique longitudes TRAIN
  if(length(unique(journeyTEST[,1]) <3))  return(-1) # if less than 2 unique longitudes TEST 
}

# if either endpoint too far from each other, return -2 / -22
if(distHaversine(journeyTEST[1,1:2], journeyTRAIN[1,] ) >200 )  return( -2) #starts too distant
if(distHaversine(journeyTEST[nrow(journeyTEST),1:2], journeyTRAIN[nrow(journeyTRAIN),]) >500 )  return( -22) # ends too distant

# if close, return area enclosed in looped route
journeyTEST <- journeyTEST [ rev(seq_len(nrow(journeyTEST))),1:2]#reverse journeyTEST,select only lon,lat 
return(areaPolygon(rbind(journeyTEST ,journeyTRAIN )) ) # return area of enclosed polygon
}
