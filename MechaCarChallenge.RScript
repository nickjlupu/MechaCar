# MECHACAR PROTOTYPES
# Read in csv file to table
car_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

# Rename Columns
car_table <- rename(car_table, c('vehicle.length' = 'vehicle length','vehicle.weight' = 'vehicle weight','spoiler.angle' = 'spoiler angle', 'ground.clearance' = 'ground clearance'))
car_table

# Multiple Linear Regression
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=car_table) #generate multiple linear regression model

summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=car_table)) #generate summary statistics

# SUSPENSION COIL
# Read in csv file to table
coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# Suspension Coil Summary Stats
summarize_coil <- coil %>% summarize(Mean=mean(PSI),Median=median(PSI),Std.Dev.=sd(PSI),Variance=var(PSI))

# Grouped by Production Lot
coil_lot_summary <- coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Std.Dev.=sd(PSI),Variance=var(PSI))

# Student's t-Test
t.test(coil$PSI,mu=1500) #compare sample versus population means
