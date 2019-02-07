library(dplyr)
library(tidyr)
library(stringr)


# reading file
data <- read.csv("Anton.csv", stringsAsFactors = TRUE, header = TRUE)
str(data)

head(data, 3) #otherwise n is 6
class(data) #to verify it is a data frame
dim(data) # wise to check the dim
names(data) #wise to check var names

str(data) #then the strcuture
data[1,]

glimpse(data) #dplyr::

summary(data) #for distributions

hist(data$JOBTIME) #takes a vector (i.e. column) of data, breaks it up into intervals, then plots as a vertical bar the number of instances within each interval. 
plot(data$JOBTIME, data$worktime) # relationship between two vars

# REMOVE FIRST COL:
# First remove column of row names
without_x <- data[, -1]
head(without_x,2)
data <- without_x

# With stringsAsFactors, you can tell R whether it should convert strings in the flat file to factors.
# For all importing functions in the utils package, this argument is TRUE, which means that you import strings as factors. This only makes sense if the strings you import represent categorical variables in R. If you set stringsAsFactors to FALSE, the data frame columns corresponding to strings in your text file will be character.

# Option A
pools1 <- read.csv("Anton.csv", stringsAsFactors = TRUE)
str(pools1)
# Option B
pools2 <- read.csv("Anton.csv", stringsAsFactors = FALSE)
str(pools2)

#If you're dealing with more exotic flat file formats, you'll want to use read.table(). 
#It's the most basic importing function; you can specify tons of different arguments in this function. 
#Unlike read.csv() and read.delim(), the header argument defaults to FALSE and the sep argument is "" by default.
data2 <- read.table("Anton.csv", sep = ",", header = TRUE)
head(data2)
class(data2)
str(data2)

# csv2 when decimal points are comma
data3 <- read.csv2("Anton.csv", stringsAsFactors = TRUE, header = TRUE)
str(data3)
head(data3)



# data cleaning
head(data, 3)
new_inv <- gather(data[,5:18], INV, Value) # gathered all responses to INV

new_inv2 <- gather(data, INV, ValueINV, INVA:INVN) # gathered all responses to INV


# Apply separate() to bmi_cc
bmi_cc_clean <- separate(bmi_cc, col = Country_ISO, into = c("Country", "ISO"), sep = "/")

#Try to understand what the dataset is telling you by looking at the structure of your dataset. 
#There are some dates, a factor variable with 3 levels, and some numerical data. 
#Which column could be spread into new columns? = the factor column (e.g., type = low, med, high)
#Which column could be the observational data that is displayed as the values of the newly spread out columns?
# View first 50 rows of census_long
head(census_long,50)
# Spread the type column
census_long2 <- spread(census_long, type, amount)
# View first 20 rows of census_long2
head(census_long2, 20) 

# strings
job_trimmed<-str_trim(data$JOBFUNCOTHER) # trimming spaces
head(job_trimmed, 10)

str_detect(data$JOBFUNCOTHER, "Team Coach") # detects the value
str_detect(students3$dob, "1997") # Detect all dates of birth (dob) in 1997

str_replace(data$JOBFUNCOTHER, "Something", "Empty") #replaces
students3$sex <- str_replace(students3$sex, "F", "Female")

str_pad(c("23485W", "8823453Q", "994Z"), width = 9, side = "left", pad = "0") # Pad these strings with leading zeros


#REMOVE X's from a colum:
# Remove X's from day column
weather3$day <- str_replace(weather3$day, "X", "")

#SELECT some
data_reordered <- select(data, JOBFUNCOTHER, GENDER, condexp, age_range)

#OR select the rest, as well
data_reordered2 <- select(data, JOBFUNCOTHER, GENDER, condexp, age_range:COMPANY)
head(data_reordered2,3)
head(data,3)


# Missing values
missing <- is.na(data$JOBFUNCOTHER) #the whole matrix of indices
summary(missing)
data$JOBFUNCOTHER[missing] # Whoa, we can subset it with the whole matrix of the missing values
data$JOBFUNCOTHER[missing] <- 0  # and replace those with another value

any(is.na(data$JOBFUNCOTHER)) #gives yes or no

sum(is.na(data$JOBFUNCOTHER)) # then how many?

which(is.na(data$JOBFUNCOTHER)) # WHERE are they?

# OR, alternatively
# Find indices of NAs 
ind <- which(is.na(data$JOBFUNCOTHER))
# Look at the full rows for records missing
data$JOBFUNCOTHER[ind] 

# Find row with data 1000
ind <- which(data$Max.Humidity == 1000)

# Look at the data for that day
data[ind, ]
# Change 1000 to 100
data$Max.Humidity[ind] <- 100
data$JOBFUNCOTHER[2] 

# Find obvious errors and replace them:
summary(weather6$Mean.VisibilityMiles)
# Get index of row with -1 value
ind <- which(weather6$Mean.VisibilityMiles == -1)
# Look at full row
weather6[ind, ]
# Set Mean.VisibilityMiles to the appropriate value
weather6$Mean.VisibilityMiles[ind] <- 10

# Renaming a single col
names(data)[1] <- "Jobfo"
names(data)

# removing duplicate colms
# Define vector of duplicate cols
duplicates <- c(4, 6, 65, 158)

# Remove duplicates from
data_xx[, -duplicates]

# Create vector of column indices: INV
INV_ind <- str_detect(names(data), "INV")
data[, INV_ind]

# oh, summary already gives that
summary(data$JOBFUNCOTHER)

na.omit(data$JOBFUNCOTHER) #OR
data[complete.cases(data),] # gives whole complete cases
data[complete.cases(data$INVA),]

# Replace all empty strings in status with NA
social_df$status[social_df$status == ""] <- NA
data$JOBFUNCOTHER[data$JOBFUNCOTHER == ""] <-NA

summary(data$JOBFUNCOTHER) # now they are NA
sum(complete.cases(data$JOBFUNCOTHER))


# Find entries containing "account":
account <- str_detect(data$JOBFUNCOTHER, "product")

# Print the sum of account = how many entries
data$JOBFUNCOTHER[account]
summary(data$JOBFUNCOTHER[account])


# OUTLIERS
# Look at a summary() of data for general look, mins maxs etc. 
# View a histogram of the age variable
# View a histogram of the absences variable
# View a histogram of absences, but force zeros to be bucketed to the right of zero
hist(students3$absences, right = FALSE)

# PLOTTING

plot(data$INVD, data$INVC, col="red")

