

final <- read.csv("C:/econometrics/final.csv", header=TRUE, sep=',')


#calculate average annual consumption per property within the sample
mean(final$Cons)*4

#or another way:
sum(final$Cons)
number_of_properties <- nrow(final)/12
number_of_properties
total_consumption <- sum(final$Cons)
aveannualcons <- total_consumption/3
aveannaulcons
aveannualcons/numcustomers

#or another way:
(sum(final$Cons)/(3))/nrow(as.data.frame.vector(unique(final$Property_Id)))


#total properties in dataset
nrow(final)/12





#what % of properties in sample used no potable water in any given quarter ? 
(nrow(final[final$Cons == 0,])/nrow(final))*100
#i.e. pretty much exactly 1%

#number of detached in sample
nrow(as.data.frame.vector(unique(final[final$Dwel_type == "Detached",][,1])))
#..and as a percent of the sample
nrow(as.data.frame.vector(unique(final[final$Dwel_type == "Detached",][,1])))/nrow(as.data.frame.vector(unique(final$Property_Id)))
#74.16%


#average annual consumption by oo/ten and detached/multi
mean(final[final$Dwel_type == "Detached" & final$X00_Ten == "Owner Occupier",][,8])*4
mean(final[final$Dwel_type == "Detached" & final$X00_Ten == "Tenant",][,8])*4
mean(final[final$Dwel_type == "Multi" & final$X00_Ten == "Owner Occupier",][,8])*4
mean(final[final$Dwel_type == "Multi" & final$X00_Ten == "Tenant",][,8])*4

#same but without quarters of zero consumption:
mean(final[final$Dwel_type == "Detached" & final$X00_Ten == "Owner Occupier" & !(final$Cons == 0),][,8])*4

#average for just one year:
mean(final[final$Dwel_type == "Detached" & final$X00_Ten == "Owner Occupier" & final$Year == 2016,][,8])*4


#### Labelling clusters
c <- 1:nrow(final)
d <- c[seq(1,length(c), 12)]
property_list <- final[d,c(1,9,11,25)]
row.names(property_list) <- NULL

just_properties <- as.data.frame.vector(property_list[,1])


#aside: how many multi res rcw are there?
nrow(final[final$Dwel_type == "Multi" & final$RCW == "Y", ])
#ans: 468

###CODE TO GIVE CLUSTERS

fcluster <- function(Dwel_type, X00_Ten, RCW, Area_m2) { ifelse(RCW == "Y", 
        ifelse(Dwel_type == "Detached", 
               ifelse(X00_Ten == "Owner Occupier", 7, 14),27
        ),
        ifelse(Dwel_type == "Detached", 
               ifelse(X00_Ten == "Owner Occupier",
                      ifelse(is.na(Area_m2), 6,
                      ifelse(Area_m2 < 495, 1,
                             ifelse(Area_m2 < 872, 2,
                                    ifelse(Area_m2 < 1729, 3,
                                           ifelse(Area_m2 < 3521, 4, 5))))),
                      ifelse(is.na(Area_m2), 13,
                             ifelse(Area_m2 < 418, 8,
                                    ifelse(Area_m2 < 636, 9,
                                           ifelse(Area_m2 < 869, 10,
                                                  ifelse(Area_m2 < 1517, 11, 12)))))),
               ifelse(X00_Ten == "Owner Occupier",
                      ifelse(is.na(Area_m2), 20,
                             ifelse(Area_m2 < 279, 15,
                                    ifelse(Area_m2 < 617, 16,
                                           ifelse(Area_m2 < 1242, 17,
                                                  ifelse(Area_m2 < 2283, 18, 19))))),
                      ifelse(is.na(Area_m2), 26,
                             ifelse(Area_m2 < 265, 21,
                                    ifelse(Area_m2 < 595, 22,
                                           ifelse(Area_m2 < 1298, 23,
                                                  ifelse(Area_m2 < 2670, 24, 25))))))
        ))
}
  
  
  
 


small$cluster <- mapply(fcluster, Dwel_type = small$Dwel_type, X00_Ten = small$X00_Ten, RCW = small$RCW, Area_m2 = small$Area_m2)

final$cluster <- mapply(fcluster, Dwel_type = final$Dwel_type, X00_Ten = final$X00_Ten, RCW = final$RCW, Area_m2 = final$Area_m2)



table <- table(final$cluster)
table1 <- as.data.frame(table)
write.csv(table1, "table1.csv")





####Joining/Merging 2016/17 data
cons201617 <- read.csv("201617quarterly.csv", sep=',', header=TRUE)
colnames(cons201617)[1] <- "Property_Id"
quarterly <- merge(property_list, cons201617, by = "Property_Id")



###getting averages per cluster/quarter
averages <- cbind(aggregate(q1_consumption_kl ~ cluster, quarterly, mean),
aggregate(q2_consumption_kl ~ cluster, quarterly, mean),
aggregate(q3_consumption_kl ~ cluster, quarterly, mean),
aggregate(q4_consumption_kl ~ cluster, quarterly, mean)
)

averages <- averages[,c(1,2,4,6,8)]
write.csv(averages,"cluster_averages.csv", col.names=TRUE)
