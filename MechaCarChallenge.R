demo_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg ~ vehicle_weight + spoiler_angle + ground_clearance + AWD + vehicle_length,data=demo_table) #generate multiple linear regression model
summary(lm(mpg ~ vehicle_weight + spoiler_angle + ground_clearance + AWD + vehicle_length,data=demo_table)) #generate multiple linear regression model
table_coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
summarize_coil <- table_coil %>% summarize(Mean=mean(PSI), Median=median(PSI),Variance=sd(PSI)^2, SD=sd(PSI),.groups = 'keep') #create summary table
summarize_coil_lot <- table_coil %>% group_by(Manufacturing_Lot)%>% summarize(Mean=mean(PSI), Median=median(PSI),Variance=sd(PSI)^2, SD=sd(PSI),.groups = 'keep') #create summary table
sample_table <- table_coil %>% sample_n(50) #randomly sample 50 data points
t.test(sample_table$PSI,mu=mean(table_coil$PSI))
t.test(subset(sample_table,Manufacturing_Lot=="Lot1",select=PSI),mu=mean(table_coil$PSI))
t.test(subset(sample_table,Manufacturing_Lot=="Lot2",select=PSI),mu=mean(table_coil$PSI))
t.test(subset(sample_table,Manufacturing_Lot=="Lot3",select=PSI),mu=mean(table_coil$PSI))

