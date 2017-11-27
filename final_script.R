#Importing the essentials
library(tidyverse)
library(lubridate)
library(stringr)
library(ggmap)
library(data.table)


#Importing data
dataset <- read.csv("Mass Shootings Dataset Ver 2.csv", 
                    header = T, stringsAsFactors = F, na.strings = '')


#Know thy data
View(dataset)
colnames(dataset)
head(dataset,20)
summary(dataset)
str(dataset$Date)


#Renaming Column
colnames(dataset)[1] <- "SerialNo"
dataset <- setnames(dataset, old = 'Mental.Health.Issues', new = 'MentalHealthIssues')
colnames(dataset)[colnames(dataset) == 'Total.victims'] <- 'TotalVictims'


#Missing Values
sum(is.na(dataset))
sapply(dataset, function(x) sum(is.na(x)))

#Handling missing data in the Summary column
dataset$Summary[is.na(dataset$Summary)] <- "Unknown"


#Extracting Year, Month, Day from Date column
dataset$Date <- sapply(dataset$Date,
                       FUN= function(x) gsub(pattern = "-",replacement = "\\/",
                                             x = x))

dataset$Year <- year(as.Date(dataset$Date,format = '%m/%d/%Y'))
dataset$Month <- month(as.Date(dataset$Date,format = '%m/%d/%Y'))
dataset$Day <- day(as.Date(dataset$Date,format = '%m/%d/%Y'))
dataset$MonthAbbr <- months(as.Date(dataset$Date, format = '%m/%d/%Y'),abbreviate = T )

View(select(dataset,Date, Year, Month,MonthAbbr, Day))


#Splitting Location into City and State
Area <- sapply(dataset$Location, 
               FUN=function(x) str_split(string = x, pattern = ", ", 
                                         n = 2, simplify = TRUE))

dataset$City <- Area[1,]
dataset$State <- Area[2,]

View(select(dataset, Location, City, State))

#Inspecting State and City for abonormal data
unique(dataset$State)
unique(dataset$City)

#Dealing with Misssing Values of Location
sum(is.na(dataset$Location))
View(select(dataset, Title,Location, State, City, Latitude, Longitude))
geography_dataset <- select(dataset,Title, Location, State, City, Latitude, Longitude)

#missing_location is subset of dataset that contains missing data for Location
missing_location <- geography_dataset[is.na(geography_dataset$Location), 
                                      c("Title","Location", "State", "City", "Latitude","Longitude" )]

missing_location$Location <-NA
missing_location$State <- NA
missing_location$City <- NA 

repeat {
  for (i in 1:nrow(missing_location)) {
    if (is.na(missing_location$Location[i])){
      missing_location$Location[i] <- revgeocode(as.numeric(missing_location[i,c("Longitude", "Latitude")]))
    } 
  }
  if (sum(is.na(missing_location$Location))==0){break}
}

sum(is.na(missing_location$Location))  #No. more missing data in Location

#Stripping off digits and extraneous data from Location and 
#inserting State and City from the newly inserted Location data
for (i in 1:nrow(missing_location)){
  state <- strsplit(missing_location$Location[i], ", ")[[1]][3][1]
  missing_location$State[i] <- gsub("[[:digit:]]+", "", state)
  missing_location$City[i] <- strsplit(missing_location$Location[i], ", ")[[1]][2][1]
}

#Chaning the datatype to char
missing_location$Location <- as.character(missing_location$Location)
missing_location$State <- as.character(missing_location$State)
missing_location$City <- as.character(missing_location$City)

#No. missing data for Location, State and City in missing_location
sum(is.na(missing_location$Location))
sum(is.na(missing_location$State))
sum(is.na(missing_location$City))


#Merging the newly filled Location data back into original dataset

matched <- match(missing_location$Title, dataset$Title)
matched  #Contains the indexes that had missing data for Location

#Final merge
dataset[matched, c("Location", "City", "State")] <- missing_location[, c("Location", "City", "State")]

#Final check if Location, State, City contains any missing data
sum(is.na(dataset$Location))
sum(is.na(dataset$State))
sum(is.na(dataset$City))


#Dealing with abnormal data in City and State
washington<-which(dataset$Location=="Washington D.C.")
washington  #Contains the index where Washington is 'Washington D.C.'
#Splitting Washington from Washington D.C
dataset$City[washington] <- strsplit(dataset$Location[washington], " ")[[1]][[1]]
dataset$State[washington] <- strsplit(dataset$Location[washington], " ")[[1]][[2]]

#Standardizing the State of Washington
dataset$State[dataset$State=='D.C.'] <- 'Washington'
dataset$state.fullname[dataset$City=='Washington'] <- 'Washington'
dataset$state.abbreviated[dataset$City=='Washington'] <- "WA"


#Extracting the last word from State for abnormal states observed in 
#144, 173, 222, 238 and trimming spaces
for (i in 1:nrow(dataset)){
  dataset$State[i] <- sub('.*,\\s', '', dataset$State[i])
  dataset$State[i] <- str_trim(dataset$State[i])
}


#Standardizing State names

#Funtion that converts abbr to full and vice versa
#Source: https://gist.github.com/ligyxy/acc1410041fe2938a2f5

abb2state <- function(name, convert = F, strict = F){
  data(state)
  # state data doesn't include DC
  state = list()
  state[['name']] = c(state.name,"District Of Columbia")
  state[['abb']] = c(state.abb,"DC")
  
  if(convert) state[c(1,2)] = state[c(2,1)]
  
  single.a2s <- function(s){
    if(strict){
      is.in = tolower(state[['abb']]) %in% tolower(s)
      ifelse(any(is.in), state[['name']][is.in], NA)
    }else{
      # To check if input is in state full name or abb
      is.in = rapply(state, function(x) tolower(x) %in% tolower(s), how="list")
      state[['name']][is.in[[ifelse(any(is.in[['name']]), 'name', 'abb')]]]
    }
  }
  sapply(name, single.a2s)
}

#Separating State into acronyms and full forms
for (i in 1:nrow(dataset)){
  dataset$state.abbreviated[i] <- abb2state(dataset$State[i], convert=T)
  dataset$state.fullname[i]<- abb2state(dataset$State[i])
}

#Filling in missing values for Lattitude and Longitude
for (i in (which(is.na(dataset$Latitude) | is.na(dataset$Longitude)))){
  a<-geocode(dataset$City[i], output="latlon")
  dataset$Latitude[i]<-a[1,1]
  dataset$Longitude[i]<-a[1,2]
}



#Standardizing Gender data
unique(dataset$Gender)
table(dataset$Gender)

dataset$Gender[dataset$Gender=='M'] <- "Male"
dataset$Gender[dataset$Gender=='M/F'] <- "Male/Female"
dataset$Gender[is.na(dataset$Gender)] <- "Unknown"

dataset$Gender <- as.factor(dataset$Gender)
levels(dataset$Gender)

#Race
unique(dataset$Race)
table(dataset$Race)
dataset$Race[is.na(dataset$Race) | dataset$Race=='unclear'] <- "Unknown"
dataset$Race[dataset$Race=='unclear'] <- "Unknown"
dataset$Race[dataset$Race=='White ' | dataset$Race=='white'| dataset$Race== "White"]<-  "White American or European American"
dataset$Race[dataset$Race=='black' | dataset$Race=='Black'|
               dataset$Race=='Black American or African American/Unknown' ] <- "Black American or African American"
dataset$Race[dataset$Race=='Some other race'] <- "Other"
dataset$Race[dataset$Race=='Asian' | dataset$Race=='Asian American'] <- "Asian or American"
dataset$Race[dataset$Race=='Asian American/Some other race' |
               dataset$Race=='White American or European American/Some other Race'] <- "Two or more races"


#Mental Health Issues
unique(dataset$MentalHealthIssues)
dataset$MentalHealthIssues[dataset$MentalHealthIssues == 'unknown' |
                               dataset$MentalHealthIssues == 'Unclear '] <- 'Unknown'



#Factoring columns "MentalHealthIssues", "Race"
cols_to_factors <- c("MentalHealthIssues", "Race")
dataset[,cols_to_factors] <- lapply(dataset[,cols_to_factors],as.factor)



#Converting other columns to factors
dataset$State <- as.factor(dataset$State)

dataset$state.abbreviated <- unlist(dataset$state.abbreviated)
dataset$state.abbreviated <- as.factor(dataset$state.abbreviated)
dataset$state.fullname <- unlist(dataset$state.fullname)
dataset$state.fullname <- as.factor(dataset$state.fullname)


#Math error in Total Victims 
math_error <- which(dataset$TotalVictims != dataset$Fatalities + dataset$Injured)
math_error   #Contain indexes who mathematical caluculation doesnt add up
View(dataset[math_error, c("Injured", "Fatalities", "TotalVictims")])
dataset$TotalVictims[math_error] <- rowSums(dataset[math_error, c("Injured", "Fatalities")])  

#Check if the mismatch still exists
math_error <- which(dataset$Total.victims != dataset$Fatalities + dataset$Injured)
math_error


#Renaming 2 newly added columns
#dataset <- setnames(dataset, old = 'Mental.Health.Issues', new = 'MentalHealthIssues')
dataset <- setnames(dataset, old= c('state.abbreviated','state.fullname'), new=c('StateAbbreviated', 'StateFullName'))

#Removing Duplicate Records
duplicate_indexes <- which(duplicated(dataset[c('Date', 'State')]),)
duplicate_indexes
dataset <- dataset[!duplicated(dataset[c('Date', 'State')]),]
View(dataset)

#Creating an R object for futher use
save(dataset, file="US_Mass_Shooting_RObj.rdata")
