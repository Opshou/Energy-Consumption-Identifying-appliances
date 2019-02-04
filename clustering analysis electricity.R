####UBIQUM MODULE 3, TASK 1: DATA ANALYTICS AND DATA VISUALIZATION
##We have information on 3 sub-meters and we analyze it 

#LOADING THE LIBRARIES####
library(dplyr) #Used for easily selecting data
library(tidyr) #Used for easily selecting data
library(chron) #This package lets you handle time data (hours, minutes and seconds)
library(ggplot2) #Plotting tool
library(scales) #It's used with ggplot when plotting time-series
library(reshape)
library(data.table) #Used to join tables
library(vegan) #Used to make a kmeans with a range of different k
library(cclust) #Used to analyze indexes of a set of clusterings done with different ks
library(factoextra) #Allows visualization of the kmeans clustering

#=================================================================================================#
#=================================================================================================#


#SET DIRECTORY, LOAD PACKAGES AND IMPORT DATA####
setwd("C:/Users/mgold/Desktop/Ubiqum/Module 3/Task 1 Deep Analytics and Data Visualization/household_power_consumption")

#Reading the dataset
power_cons <- read.delim2("household_power_consumption.txt", header = TRUE, sep = ";", dec = ".")

power_consumption <- power_cons #Creating a new variable so we don't have to load the file again

#=================================================================================================#
#=================================================================================================#

#PREPARING THE DATA: DATA TYPES, LOOKING FOR NAs####

#Creating a new variable that has date and time together
power_consumption <- cbind(power_consumption,paste(power_consumption$Date,power_consumption$Time),
                           stringsAsFactors=FALSE)
colnames(power_consumption)[10] <-"Date_time"
#Swapping to 1st position
power_consumption <- power_consumption[,c(ncol(power_consumption), 1:(ncol(power_consumption)-1))]

#Changing data types for date
power_consumption$Date <- as.Date(power_consumption$Date, "%d/%m/%Y")

#We look at when the NAs happen for the energy: the energy wasn't collected at that moment
na_general<- power_consumption[is.na(power_consumption$Global_active_power),]

#We find missing values for individual rows (these we can take out), and for some days
#Blackouts: from 2007-04-28 00:21 til 2007-30-04 14:23 --> 2 days 14'5 hours
#           from 2007-06-09 17:55 til 2007-06-09 18:31 --> 36 minutes
#           from 2007-07-15 16:49 til 2007-07-15 19:07 --> 2h 18 minutes
#           from 2007-08-01 08:12 til 2007-08-01 08:32 --> 20 minutes
#           from 2008-10-25 10:28 til 2008-10-25 11:10 --> 52 minutes
#           from 2008-12-10 10:48 til 2008-12-10 11:57 --> 1h 9 minutes
#           from 2009-06-13 00:30 til 2009-06-15 07:34 --> 2 days 7 hours
#           from 2009-08-13 05:00 til 2009-08-13 19:50 --> 14h 50 minutes
#           from 2010-01-12 14:53 til 2010-01-14 19:01 --> 2 days 4 hours
#           from 2010-03-20 03:52 til 2010-03-21 13:38 --> 1 days 10 hours
#           from 2010-08-17 21:02 til 2010-08-22 21:27 --> 5 days
#           from 2010-09-25 03:56 til 2010-09-28 19:12 --> 3 days 16 hours

#CREATING NEW COLUMNS & VARIABLES FOR DIFFERENT PURPOSES####

#Creating a year, month and remaining energy column
power_consumption$Month <- substr(power_consumption$Date, 1, 7)
power_consumption$Year <- substr(power_consumption$Date, 1, 4)
power_consumption$remaining_energy <- power_consumption$Global_active_power*1000/60 - 
  power_consumption$Sub_metering_1 - power_consumption$Sub_metering_2 - power_consumption$Sub_metering_3

#Creating a global in kWh
power_consumption$Global_active_power_Wh <- power_consumption$Global_active_power*1000/60

#Creating variables that we will use for our kmeans and for visualizing the data respectively
for_kmeans <- na.omit(power_consumption)
for_plotting <- na.omit(power_consumption)
for_plotting$Date_time <- strptime(for_plotting$Date_time, "%d/%m/%Y %H:%M:%S")
for_plotting$Date_time <- as.POSIXct(for_plotting$Date_time)

#BUILDING FUNCTIONS FOR OUR KMEANS ANALYSIS####

#============================================================================#
#============================================================================#
#       FUNCTION 1: Creating df that plots sum of squares VS #clusters       #
#                                                                            #
#This function provides information on all the possible clusters. It uses a  #
#range of clusters and provides a df with information of all of them. It also#
#plots tot_wss and betweens to help the user better decide on the optimal    #
#number of clusters.                                                         #
#                                                                            #
#PARAMETERS: dataset: dataset we want to make the analysis on.               #
#            meter_name: name of the meter we want to analyze and cluster.   #
#            min_clus_number: min. number of clusters.                       #
#            max_clus_number: max. number of clusters.                       #
#============================================================================#
#============================================================================#

creating_clusters <- function(dataset, meter_name, min_clus_number, max_clus_number) { 
  set.seed(983)
  #selecting the submeter/s we want to analyze 
  cluster_data <- dataset %>% select(Date_time, Time, meter_name) 
  #creating an empty vector that will hold the total distance values         
  tot_wss <- c()
  betweens <- c()
  centers <- list()
  size <- list()
  perc_size <- list()
  #running cluster from 2 to chosen #clusters                                
  for(i in min_clus_number:max_clus_number){                                                
    cl <- kmeans(cluster_data[3],centers = i, iter.max = 10, nstart = 10)     
    tot_wss[i] <- cl$tot.withinss
    betweens[i] <- cl$betweens
    centers[i] <- list(cl$centers)
    size[i] <- list(cl$size)
    perc_size[i] <- list((cl$size)/nrow(dataset)*100)
  }
  #merge centers and size in a single list
  cluster_metrics <- list(centers, size, perc_size)
  
  #plotting within-sum-squares VS number of clusters                                        
  par(mfrow=c(1,2))
  plot(x=1:max_clus_number,                                                               
       y=tot_wss,                                                            
       type = "b",                                                           
       xlab = "Number of clusters",                                          
       ylab = "Within groups sum of squares") 
  plot(x=1:max_clus_number,
       y = betweens,
       type = "b",
       xlab = "Number of clusters",
       ylab = "Betweens")
  return(cluster_metrics)
}
#============================================================================#  
#                             END OF FUNCTION                                #
#============================================================================#


#============================================================================#
#============================================================================#
#               FUNCTION 2: Visualizing our cluster results                  #
#                                                                            #
#This function returns a df with the information on the cluster we are inte- #
#rested learning about                                                       #
#                                                                            #
#PARAMETERS: meter_metrics: a list with information about all found clusters.#
#            cluster_number: number of clusters I want.                      #
#============================================================================#
#============================================================================#

visualizing_list <- function(meter_metrics, cluster_number){
  cluster_info <- c()
  for (i in (1:3)){
    cluster_info[i] <- as.data.frame(meter_metrics[[i]][[cluster_number]])
  }
  cluster_info <- as.data.frame(cluster_info)
  colnames(cluster_info) <- c("center", "size", "percentage")
  return(cluster_info)
}  
#============================================================================#  
#                             END OF FUNCTION                                #
#============================================================================#

#============================================================================#
#============================================================================#
#            FUNCTION 3: Merging clusters to original dataset                #
#                                                                            #
#This function merges the clusters with the original dataset. It uses the op-# 
#timal number of clusters.                                                   # 
#                                                                            #
#PARAMETERS: dataset: dataset used to build our kmeans.                      #
#            cluster: the result of the applying kmeans to our dataset.      #
#============================================================================#
#============================================================================#

merging_cluster_results <- function(dataset, cluster){
  
  #creating a column to label each row both in dataset and cluster$clusters
  dataset$label <- seq_along(dataset$Date_time)
  cluster_merge <- as.data.frame(seq_along(cluster$cluster))
  colnames(cluster_merge)[1] <-"label"
  #merging clusters$cluster to our cluster_merge
  cluster_merge$cluster <- as.factor(cluster$cluster)
  #adding the cluster results to the original dataset
  return(merge(dataset, cluster_merge, by= "label"))
}  
#============================================================================#  
#                             END OF FUNCTION                                #
#============================================================================#


#CLUSTER ANALYSIS ON 1st SUBMETER --> Dishwasher, oven and microwave####

#Using Function 1 in order to see the optimal number of clusters
metrics_sub_1 <- creating_clusters(for_kmeans, "Sub_metering_1", 2, 10)
visualizing_list(metrics_sub_1, 4)

#kmeans for 6, which seems to be the optimal number of clusters
set.seed(398)
cluster_sub1 <- kmeans(for_kmeans$Sub_metering_1,centers = 4, iter.max = 50, nstart = 50) 

#visualizing results
cluster_sub1$centers
cluster_sub1$size
cluster_sub1$size/nrow(for_kmeans)*100

#merging results to the measures
S1_cluster <-merging_cluster_results(for_kmeans, cluster_sub1)

#visualizing a specific cluster
filtered_s1_cluster <- S1_cluster%>%filter(cluster == 1)%>%select(Date_time, Sub_metering_1)

#CLUSTER ANALYSIS ON 2nd SUBMETER --> Washing-machine, tumble-drier, refrigerator, light####

#Using Function 1 in order to see the optimal number of clusters
metrics_sub_2 <- creating_clusters(for_kmeans, "Sub_metering_2", 2, 10)
visualizing_list(metrics_sub_2, 6)

#kmeans for 6, which seems to be the optimal number of clusters
set.seed(342)
cluster_sub2 <- kmeans(for_kmeans$Sub_metering_2,centers = 6, iter.max = 50, nstart = 50) 

#visualizing results
cluster_sub2$centers
cluster_sub2$size
cluster_sub2$size/nrow(for_kmeans)*100

#merging results to the measures
S2_cluster <-merging_cluster_results(for_kmeans, cluster_sub2)

#visualizing a specific cluster
filtered_s2_cluster <- S2_cluster%>%select(Date_time, Sub_metering_2, cluster)


#CLUSTER ANALYSIS ON 3rd SUBMETER --> Water-heater, air-conditioner####

#Using Function 1 in order to see the optimal number of clusters
metrics_sub_3 <- creating_clusters(for_kmeans, "Sub_metering_3", 2, 10)
visualizing_list(metrics_sub_3, 4)

#kmeans for 6, which seems to be the optimal number of clusters
set.seed(398)
cluster_sub3 <- kmeans(for_kmeans$Sub_metering_3,centers = 4, iter.max = 50, nstart = 50) 

#visualizing results
cluster_sub3$centers
cluster_sub3$size
cluster_sub3$size/nrow(for_kmeans)*100

#merging results to the measures
S3_cluster <-merging_cluster_results(for_kmeans, cluster_sub3)

#visualizing a specific cluster
filtered_s3_cluster <- S3_cluster%>%filter(cluster == 3)%>%select(Date_time, Sub_metering_3)


#CLUSTER ANALYSIS ON REMAINING ENERGY####

#Using Function 1 in order to see the optimal number of clusters
metrics_remaining <- creating_clusters(for_kmeans, "remaining_energy", 2, 9)
visualizing_list(metrics_remaining, 5)

#kmeans for 6, which seems to be the optimal number of clusters
cluster_remaining <- kmeans(for_kmeans$remaining_energy,centers = 9) 

#visualizing results
cluster_remaining$centers
cluster_remaining$size
cluster_remaining$size/nrow(for_kmeans)*100

#merging results to the measures
remaining_cluster <- merging_cluster_results(for_kmeans, cluster_remaining)

#visualizing a specific cluster
filtered_remaining_cluster <- remaining_cluster%>%filter(cluster == 3)%>%select(Date_time, remaining_energy)

#INDIVIDUAL WITH SUBMETER (we find that it's not as useful as clustering individual submeters####

#creating an empty vector that will hold the total distance values         
tot_wss <- c()
betweens <- c()
centers <- list()
size <- list()
perc_size <- list()
#running cluster from 2 to chosen #clusters
set.seed(834)
for(i in 2:40){                                                
  cl <- kmeans(for_kmeans %>% select(Sub_metering_1, Sub_metering_2, Sub_metering_3),centers = i)     
  tot_wss[i] <- cl$tot.withinss
  betweens[i] <- cl$betweens
  centers[i] <- list(cl$centers)
  size[i] <- list(cl$size)
  perc_size[i] <- list((cl$size)/nrow(for_kmeans)*100)
}
#merge centers and size in a single list
cluster_metrics <- list(centers, size, perc_size)

#plotting within-sum-squares VS number of clusters                                        
par(mfrow=c(1,2))
plot(x=1:20,                                                               
     y=tot_wss,                                                            
     type = "b",                                                           
     xlab = "Number of clusters",                                          
     ylab = "Within groups sum of squares") 
plot(x=1:20,
     y = betweens,
     type = "b",
     xlab = "Number of clusters",
     ylab = "Betweens")

set.seed(234)
all_submeters <- kmeans(for_kmeans %>% select(Sub_metering_2, Sub_metering_3),centers = 6) 

all_submeters$centers
all_submeters$size/nrow(for_kmeans)*100




#CLUSTER ANALYSIS ON ALL 3 SUBMETERS (we find that it's not as useful as clustering individual submeters####

#creating an empty vector that will hold the total distance values         
tot_wss <- c()
betweens <- c()
centers <- list()
size <- list()
perc_size <- list()
#running cluster from 2 to chosen #clusters
set.seed(834)
for(i in 2:20){                                                
  cl <- kmeans(for_kmeans %>% select(Sub_metering_1, Sub_metering_2, Sub_metering_3),centers = i)     
  tot_wss[i] <- cl$tot.withinss
  betweens[i] <- cl$betweens
  centers[i] <- list(cl$centers)
  size[i] <- list(cl$size)
  perc_size[i] <- list((cl$size)/nrow(for_kmeans)*100)
}
#merge centers and size in a single list
cluster_metrics <- list(centers, size, perc_size)

#plotting within-sum-squares VS number of clusters                                        
par(mfrow=c(1,2))
plot(x=1:20,                                                               
     y=tot_wss,                                                            
     type = "b",                                                           
     xlab = "Number of clusters",                                          
     ylab = "Within groups sum of squares") 
plot(x=1:20,
     y = betweens,
     type = "b",
     xlab = "Number of clusters",
     ylab = "Betweens")

set.seed(234)
all_submeters <- kmeans(for_kmeans %>% select(Sub_metering_1, Sub_metering_2, Sub_metering_3),centers = 64) 

all_submeters$centers
all_submeters$withins


#EXPORTING CLUSTERED DATA TO CSV FILE####
S1_cluster$Date_time <- strptime(S1_cluster$Date_time, "%d/%m/%Y %H:%M:%S")
S1_cluster$Date_time <- as.POSIXct(S1_cluster$Date_time)

final_clustered_data <- S1_cluster
colnames(final_clustered_data)[16] <-"Cluster_sub1"
final_clustered_data$Cluster_sub2 <- S2_cluster$cluster
final_clustered_data$Cluster_sub3 <- S3_cluster$cluster
final_clustered_data <- final_clustered_data[c(12, 13, 14)]

write.csv(final_clustered_data, file = "Final_clustered_data.csv")

#VISUALIZING OUR DATA####
#============================================================================#
#============================================================================#
#                   FUNCTION 3: Visualizing our data                         #
#                                                                            #
#This function returns a plot of the energy values for a desired period:     #
#            -It can plot a single day by the minute.                        #
#            -It can plot a period of days and plot the meter consumption.   #
#            -It can plot a period of months and plot the meter consumption. #
#            -It can plot a period of years and plot the meter consumption.  #
#                                                                            #
#PARAMETERS: period_division: minute, day, month, year.                      #
#            beginning_period: initial period (day: YYYY-MM-DD,              #
#                                              month: YYYY-MM, year: YYYY)   #
#                                              year: YYYY)                   #
#            end_period: final period (same format as beginning_period).     #
#            smaller_than_day: TRUE: plot less than a day.                   #
#                              FALSE (default): plot day.                    #
#============================================================================#
#============================================================================#
plotting_energies <- function(period_division, beginning_period, end_period, submeter = "None", smaller_than_day = FALSE){

  #Plotting for a smaller portion of a day
  if ((period_division == "Minute") && smaller_than_day == TRUE){
    plotting_minute <- for_plotting %>% filter(Date_time >= beginning_period, Date_time <= end_period)
    
    if (submeter == "None"){
      return(ggplot(plotting_minute, aes(x = Date_time, y = Sub_metering_1)) + geom_line() +
               geom_line(aes(x=Date_time,y=Sub_metering_2,col = 'Sub 2'))+
               geom_line(aes(x=Date_time,y=Sub_metering_3,col = 'Sub 3'))+
               geom_line(aes(x=Date_time,y=remaining_energy,col = 'remaining'))+
               labs(y="Energy spent (Wh)", 
                    x="Time", 
                    title="Live submetering and last 24h"))
    } else if (submeter == "Submeter 1"){
      return(ggplot(plotting_minute, aes(x = Date_time, y = Sub_metering_1)) + geom_line())
    } else if (submeter == "Submeter 2"){
      return(ggplot(plotting_minute, aes(x = Date_time, y = Sub_metering_2)) + geom_line())
    } else if (submeter == "Submeter 3"){
      return(ggplot(plotting_minute, aes(x = Date_time, y = Sub_metering_3)) + geom_line())
    } else if (submeter == "Remaining"){
      return(ggplot(plotting_minute, aes(x = Date_time, y = remaining_energy)) + geom_line())
    }
  }
  
  #Plotting a whole day
  if ((period_division == "Minute") && smaller_than_day == FALSE){
    plotting_minute <- for_plotting %>% filter(Date >= beginning_period, Date <= end_period)
    
    if (submeter == "None"){
      return(ggplot(plotting_minute, aes(x = Date_time, y = Sub_metering_1)) + geom_line() +
              geom_line(aes(x=Date_time,y=Sub_metering_2,col = 'Sub 2'))+
              geom_line(aes(x=Date_time,y=Sub_metering_3,col = 'Sub 3'))+
              geom_line(aes(x=Date_time,y=remaining_energy,col = 'remaining'))+
              labs(y="Energy spent (Wh)", 
                    x="Time", 
                    title="Live submetering and last 24h"))
    } else if (submeter == "Submeter 1"){
      return(ggplot(plotting_minute, aes(x = Date_time, y = Sub_metering_1)) + geom_line())
    } else if (submeter == "Submeter 2"){
      return(ggplot(plotting_minute, aes(x = Date_time, y = Sub_metering_2)) + geom_line())
    } else if (submeter == "Submeter 3"){
      return(ggplot(plotting_minute, aes(x = Date_time, y = Sub_metering_3)) + geom_line())
    } else if (submeter == "Remaining"){
      return(ggplot(plotting_minute, aes(x = Date_time, y = remaining_energy)) + geom_line())
    }
  }
    
  #Plotting daily total
  if (period_division == "Day"){
    plotting_daily <- for_plotting %>% group_by(Date)%>%
      filter(Date >= beginning_period, Date <= end_period)%>%
      summarize(total_Global = sum(Global_active_power_Wh), total_sub_1 = sum(Sub_metering_1), 
                total_sub_2 = sum(Sub_metering_2), total_sub_3 = sum(Sub_metering_3), 
                total_remaining = sum(remaining_energy))
    
    return(ggplot(plotting_daily, aes(x = Date, y = total_sub_1)) + geom_line() +
             geom_line(aes(x=Date,y=total_sub_2,col = 'Sub 2'))+
             geom_line(aes(x=Date,y=total_sub_3,col = 'Sub 3'))+
             geom_line(aes(x=Date,y=total_remaining,col = 'remaining'))+
             labs(y="Energy spent (Wh)", 
                  x="Time", 
                  title="Live submetering and last 24h"))
  }

  #Plotting monthly total
  if (period_division == "Month"){
    plotting_monthly <- for_plotting %>% group_by(Month)%>%
      filter(Month >= beginning_period, Month <= end_period)%>%
      summarize(total_Global = sum(Global_active_power_Wh), total_sub_1 = sum(Sub_metering_1),
                total_sub_2 = sum(Sub_metering_2), total_sub_3 = sum(Sub_metering_3),
                total_remaining = sum(remaining_energy))
    
    return(ggplot(plotting_monthly, aes(x = Month, y = total_sub_1)) + geom_point() +
             geom_point(aes(x=Month,y=total_sub_2,col = 'Sub 2'))+
             geom_point(aes(x=Month,y=total_sub_3,col = 'Sub 3'))+
             geom_point(aes(x=Month,y=total_remaining,col = 'remaining'))+
             labs(y="Energy spent (Wh)", 
                  x="Time", 
                  title="Live submetering and last 24h"))
  }
  
  #Plotting yearly total
  if (period_division == "Year"){
    plotting_yearly <- for_plotting %>% group_by(Year)%>%
      filter(Year >= beginning_period, Year <= end_period)%>%
      summarize(total_Global = sum(Global_active_power_Wh), total_sub_1 = sum(Sub_metering_1),
                total_sub_2 = sum(Sub_metering_2), total_sub_3 = sum(Sub_metering_3),
                total_remaining = sum(remaining_energy))
    
    return(ggplot(plotting_yearly, aes(x = Year, y = total_Global)) + geom_point() +
             geom_point(aes(x=Year,y=total_sub_2,col = 'Sub 2'))+
             geom_point(aes(x=Year,y=total_sub_3,col = 'Sub 3'))+
             geom_point(aes(x=Year,y=total_remaining,col = 'remaining'))+
             labs(y="Energy spent (Wh)", 
                  x="Time", 
                  title="Live submetering and last 24h"))
  }
}
#============================================================================#  
#                             END OF FUNCTION                                #
#============================================================================#

#Here you can change parameters in order to visualize different consumptions.
plotting_energies("Minute", "2009-12-27 00:00:00", "2009-12-29 23:59:00", "Submeter 3", smaller_than_day = TRUE)
plotting_energies("Minute", "2008-01-13", "2008-01-13", "Submeter 1")

for_plotting %>% filter(Date >= "2008-01-01", Date <= "2008-01-11", Sub_metering_3 >= 15, Sub_metering_3 <= 20)


cluster_with_datetime$label <- seq_along(cluster_with_datetime$Date_time)

cluster_merge <- as.data.frame(seq_along(cl$cluster))
colnames(cluster_merge)[1] <-"label"

cluster_merge$cluster <- as.factor(cl$cluster)

#adding the cluster to the original dataset
cluster_with_datetime <- merge(cluster_with_datetime, cluster_merge, by= "label")

ggplot(S1_cluster %>% filter(Date == "2009-06-27"), aes(x = Date_time, y = Sub_metering_1)) + geom_point(aes(color =cluster))
ggplot(S2_cluster %>% filter(Date == "2009-12-24"), aes(x = Date_time, y = Sub_metering_2)) + geom_point(aes(color =cluster))
ggplot(S3_cluster %>% filter(Date == "2009-12-24"), aes(x = Date_time, y = Sub_metering_3)) + geom_point(aes(color =cluster)) +
  labs(x="Date and time", y="Power consumption", col="# of clusters") +
  scale_color_manual(labels = c("AC", "Water heater", "AC + Water heater", "Appl. off"), values = c("blue", "red", "pink", "green"))
  
  


ggplot(S1_cluster %>% filter(Date_time >= "2009-12-24 00:00:00", Date_time <= "2009-12-24 07:00:00"), aes(x = Date_time, y = Sub_metering_1)) + geom_point(aes(color =cluster))





#IDENTIFYING CONSUME OF APPLIANCES####
#Through plotting we identify specific appliances and use the period they are used in to find
#total consumption.
#Selecting a dishwasher: they make one from "2010-03-14 21:00:00" to "2010-03-14 23:59:00":
dishwaser <- for_plotting %>% filter(Date_time >= "2010-03-14 21:00:00", Date_time <= "2010-03-14 23:59:00")%>%
  summarize(consumption = sum(Sub_metering_1))
#1053Wh ==> 1'053kWh

#Oven:
oven <- for_plotting %>% filter(Date_time >= "2006-12-19 07:00:00", Date_time <= "2006-12-19 11:00:00")%>%
  summarize(consumption = sum(Sub_metering_1))
#839Wh ==> 0'839kWh

#Microwave:
microwave <- for_plotting %>% filter(Date_time >= "2009-09-14 18:50:00", Date_time <= "2009-09-14 19:10:00")%>%
  summarize(consumption = sum(Sub_metering_1))
#416Wh ==> 0'416kWh



#TRYING A SECOND CLUSTERING####
cl <- kmeans(cluster_with_datetime[8],4) 
cl$cluster

cluster_with_datetime$label <- seq_along(cluster_with_datetime$Date_time)

cluster_merge <- as.data.frame(seq_along(cl$cluster))
colnames(cluster_merge)[1] <-"label"

cluster_merge$cluster <- as.factor(cl$cluster)

#adding the cluster to the original dataset
cluster_with_datetime <- merge(cluster_with_datetime, cluster_merge, by= "label")

ggplot(cluster_with_datetime, aes(x = Date_time, y = Sub_metering_3)) + geom_point(aes(color =cluster))

plotting_energies("Minute", "2009-12-24 00:00:00", "2009-12-24 23:59:00", "Submeter 2", smaller_than_day = TRUE)
