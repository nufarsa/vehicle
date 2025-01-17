# Dataset online retail 

install.packages("arulesViz")
install.packages("dplyr")
install.packages("lubridate")
install.packages("mice")
install.packages("ddply")
install.packages("tidyverse")
library(lubridate)
library(arules)
library(arulesViz)
library(dplyr)
library(mice)
library(plyr)
library(tidyverse)


#---------------------
# Data Pre-Processing
#---------------------
cars<- read.csv(file.choose(), sep = ",", header = TRUE)

# check structure
str(cars)

#---------------------
# Data Exploration
#---------------------
# Check for bad data
md.pattern(cars)
summary(cars)

# Missing data
table(is.na(cars))

# Remove date_reg since its a date value
# Remove the first column
cars <- cars[, -1]

# check again the structure
str(cars)

#---------------------------------------------------------
# Combine columns into transactions to use apriori
#---------------------------------------------------------
# Convert to factors
cars$type <- as.factor(as.character(cars$type))
cars$maker <- as.factor(as.character(cars$maker))
cars$model <- as.factor(as.character(cars$model))
cars$colour <- as.factor(as.character(cars$colour))
cars$fuel <- as.factor(as.character(cars$fuel))
cars$state <- as.factor(as.character(cars$state))

# Convert each row to a list
#cars_list <- paste(apply(cars, 1, function(row) paste(row, collapse = ",")), 
#                   collapse = ",")

#cars <- cars %>% mutate(soldcar = paste(type, maker,
#                                              model,colour,fuel,
#                                               state, sep = ","))

# Force the list to be 'transactions'
transactions <- as(cars, "transactions")

# Print the transactions
inspect(head(transactions,10))

#---------------------------------------------------------
# Association rules for car charactristic that has been sold
# from dated XX to XX 
#----------------------------------------------------------------
# We want to see customer that buy the car with fuel type electric model toyota
# support at 0.01 and confidence of 0.05 
# In Johor
#----------------------------------------------------------------
# Make data smaller
# data too big cannot apriori
cars_new <- transactions[1:1000,]

# Display data from in count decreasing order
rules_1<- apriori(cars_new, 
                      parameter=list(supp=0.01, conf=0.05))

rules_1_sortedbycount<- sort(rules_1, by="count",
                      decreasing=T)
inspect(rules_1_sortedbycount[1:200])


#----------------------------------------------------------------
# We want to see customer how likeliest customer that buy the car 
# electric in Malaysia (we want to help the taikun to sell electric cars)
# we want to find suitable model and state to locate
# support at 0.01 and confidence of 0.05 
#----------------------------------------------------------------
# Filter all state
cars_by_state <- cars %>%
  distinct(state)

# check for customer preference for fuel=electric
rules_2 <- apriori(cars_new,
                   parameter=list(supp=0.01, conf=0.05),
                   appearance=list(lhs="fuel=electric", 
                                   rhs=c("state=Rakan Niaga", 
                                         "state=Johor",
                                         "state=Kelantan",
                                         "state=Kedah",
                                         "state=W.P. Kuala Lumpur",
                                         "state=Sabah",
                                         "state=Pulau Pinang",
                                         "state=Melaka",
                                         "state=Sarawak",
                                         "state=Perak",
                                         "state=Terengganu",
                                         "state=Selangor", 
                                         "state=W.P. Labuan",
                                         "state=Pahang",
                                         "state=Negeri Sembilan",
                                         "state=Perlis",
                                         "state=W.P. Putrajaya")))


rules_2_sortedbycount<- sort(rules_2,
                             decreasing=T)

inspect(rules_2_sortedbycount)

#----------------------------------------------------------------
# We want to see customer how likeliest customer that purchase 
# variety of car type  in Malaysia (we want to help the manufacturer 
# to minimize the supply of the car based on type ) at support at 0.01 
# and confidence of 0.05
#----------------------------------------------------------------
# Filter all state
cars_by_type <- cars %>%
  distinct(type)

# check for customer preference for fuel=electric
rules_3 <- apriori(cars_new,
                   parameter=list(supp=0.01, conf=0.05),
                   appearance=list(lhs=c("state=Rakan Niaga", 
                                         "state=Johor",
                                         "state=Kelantan",
                                         "state=Kedah",
                                         "state=W.P. Kuala Lumpur",
                                         "state=Sabah",
                                         "state=Pulau Pinang",
                                         "state=Melaka",
                                         "state=Sarawak",
                                         "state=Perak",
                                         "state=Terengganu",
                                         "state=Selangor", 
                                         "state=W.P. Labuan",
                                         "state=Pahang",
                                         "state=Negeri Sembilan",
                                         "state=Perlis",
                                         "state=W.P. Putrajaya"),
                                   rhs=c("type=jip",
                                         "type=pick_up",
                                         "type=motokar",
                                         "type=window_van",
                                         "type=motokar_pelbagai_utiliti")))


rules_3_sortedbylift<- sort(rules_3, by="lift",
                             decreasing=T)

inspect(rules_3_sortedbylift)

#----------------------------------------------------------------
# We want to see customer how likeliest customer that purchase 
# variety of car type  in Malaysia (we want to help the manufacturer 
# to minimize the supply of the car based on type ) at support at 0.01 
# and confidence of 0.05
#----------------------------------------------------------------
# Filter all color
cars_by_color <- cars %>%
  distinct(colour)

# Filter all color
cars_by_maker <- cars %>%
  distinct(maker)

# since the analysis too many maker involved. Will only select 
# Top10 makers in rules 1
top10maker<- sort(rules_1, by="count",
                            decreasing=T)
# inspect
inspect(head(top10maker,10))

# check for customer preference for fuel=electric
rules_3 <- apriori(cars_new,
                   parameter=list(supp=0.01, conf=0.05),
                   appearance=list(lhs=c("state=Rakan Niaga", 
                                         "state=Johor",
                                         "state=Kelantan",
                                         "state=Kedah",
                                         "state=W.P. Kuala Lumpur",
                                         "state=Sabah",
                                         "state=Pulau Pinang",
                                         "state=Melaka",
                                         "state=Sarawak",
                                         "state=Perak",
                                         "state=Terengganu",
                                         "state=Selangor", 
                                         "state=W.P. Labuan",
                                         "state=Pahang",
                                         "state=Negeri Sembilan",
                                         "state=Perlis",
                                         "state=W.P. Putrajaya"),
                                   rhs=c("type=jip",
                                         "type=pick_up",
                                         "type=motokar",
                                         "type=window_van",
                                         "type=motokar_pelbagai_utiliti")))


rules_3_sortedbylift<- sort(rules_3, by="lift",
                            decreasing=T)

inspect(rules_3_sortedbylift)


#----------------------------------------------------------------
# Sort the lift
# lift > 1 , there is a positive association between LHS and RHS ( occur together more frequent)
# lift = 1 , LHS and RHS occut together as expected
# lift < 1 , there is negetive association between LHS and RHS ( occur less frequent)
#----------------------------------------------------------------
rules_1_sorted<- sort(rules_1, by="lift",
                 decreasing=T)

# Check the positive lift first 10
inspect(rules_1_sorted[1:10])

# Customers who buy Weststar Maxus will usually take model T90
# Customers come from W.P Kuala Lumpur
# They take white color the most ( maybe its a luxurious color)

#----------------------------------------------------------------
# We want to see customer that buy the car with fuel type electric model toyota
# support at 0.01 and confidence of 0.05 
# Where customers usually comes from  Johor
#----------------------------------------------------------------

rules_2<- apriori(cars_new, 
                  parameter=list(supp=0.01, conf=0.05),
                  appearance=list(lhs=c("fuel=electric","maker=Toyota" ), 
                                  rhs="state=Johor"))

inspect(rules_2[1:100])
#-------------------------------------------------------
# Visualization
#------------------------------------------------------
#-------------------------------------------------------
# Item in the transaction that has the highest frequency
#--------------------------------------------------------
itemFrequencyPlot(transactions, topN=5, main="Most Frequent Item In Transaction Data")
#---------------------------------------------------------
plot(rules_1, by="lift", method="graph")
#---------------------------------------------------------
plot(rules_1, method="paracoord", control=list(reorder=T))
#---------------------------------------------------------



