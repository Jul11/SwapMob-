source("~/swapmob/utilsFunctions.R")
source("~/swapmob/SwapMobFunctionsv3.R")
source("~/swapmob/plotsECDF.R")

################## DATA ##############################

traj.tot <- read.csv("~/swapmob/tdriveNou.txt", header = FALSE, sep = ",")
names(traj.tot) <- c( "id", "timestamp", "long", "lat")


#Change timestamp to unix
traj.tot$timestamp <- as.numeric(as.POSIXct(strptime(traj.tot$timestamp, "%Y-%m-%d %H:%M:%S")))

#parameters DATE for subset traces example
DATE <- "2008-02-02 18:02:06" 
FORMAT <- "%Y-%m-%d %H:%M:%S"

traces <- traj.tot[which(traj.tot$timestamp < as.numeric(as.POSIXct(strptime(DATE, FORMAT)))), ]
traces <- traces[order(traces$timestamp), ]

################## ALGORITHM ##############################

Totes.Coloc <- colocated.find(traces, 60, 3)
swapped.ID <- colocated.swap(Totes.Coloc)
traces$nou <- swapped.ID

################ plots #######

intersection <- calculate_intersection(traces)

generate_CDF(intersection[[1]])                             

link_with_X_points(traces, intersection[[1]], intersection[[2]], 10)

