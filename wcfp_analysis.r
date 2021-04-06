library(ggplot2)

WCFMP <- read.csv("WCFMP_Data.csv", header = T)
MoarBay <- subset(WCFMP, Fish_Camp == "Moar Bay")
BlackStoneBay <- subset(WCFMP, Fish_Camp == "Black Stone Bay")
GooseIsland <- subset(WCFMP, Fish_Camp == "Goose Island")
OldFactory <- subset(WCFMP, Fish_Camp == "Old Factory")
PaintHillsBay <- subset(WCFMP, Fish_Camp == "Paint Hills Bay")
RabbitsRidge <- subset(WCFMP, Fish_Camp == "Rabbits Ridge")
SculpinIsland <- subset(WCFMP, Fish_Camp == "Sculpin Island")
ShephardsIsland <- subset(WCFMP, Fish_Camp == "Shephard's Island")

###################################################################
########################### Moar Bay ##############################
###################################################################
# Create the plot object based on the following ggplot2 parameters
# X-axis data = Year
MoarBayTotalCatchPlot <- qplot(Year, Catch, data = MoarBay, shape = Species, color = Species, facets = Species~., size = I(3), xlab = "Year", ylab = "Total Catch")
MoarBayTotalCatchPlot + geom_smooth(method = lm, se = FALSE, aes(group =1)) + theme_bw()
# Plot for PPTs
#MoarBayTotalCatchPlot + geom_smooth(method = lm, colour = "darkred", size = 0.5, se = FALSE, aes(group =1)) + theme_bw() + geom_point(size = 4, colour = "black")
# Create subset datafrom for Moar Bay cisco, trout, and whitefish
MoarBay_Cisco <- subset(MoarBay, Species == "Cisco")
MoarBay_Trout <- subset(MoarBay, Species == "Trout")
MoarBay_Whitefish <- subset(MoarBay, Species == "Lake whitefish")
#Create regression output for Moar Bay cisco, trout, and whitefish
# lm(y ~ x, data = dataframe_source)
MoarBay_Cisco_Regression <- lm(Catch ~ Year, data = MoarBay_Cisco)
MoarBay_Trout_Regression <- lm(Catch ~ Year, data = MoarBay_Trout)
MoarBay_Whitefish_Regression <- lm(Catch ~ Year, data = MoarBay_Whitefish)
#Display regression summary for Moar Bay cisco, trout, and whitefish
summary(MoarBay_Cisco_Regression)
summary(MoarBay_Trout_Regression)
summary(MoarBay_Whitefish_Regression)


###################################################################
########################### Old Factory ###########################
###################################################################
# Create the plot object based on the following ggplot2 parameters
# X-axis data = Year
OldFactoryTotalCatchPlot <- qplot(Year, Catch, data = OldFactory, shape = Species, color = Species, facets = Species~., size = I(3), xlab = "Year", ylab = "Total Catch")
OldFactoryTotalCatchPlot + geom_smooth(method = lm, se = FALSE, aes(group =1)) + theme_bw()
# Plot for PPTs
#OldFactoryTotalCatchPlot + geom_smooth(method = lm, colour = "darkred", size = 0.5, se = FALSE, aes(group =1)) + theme_bw() + geom_point(size = 4, colour = "black")
# Create subset datafrom for Old Factory cisco, trout, and whitefish
OldFactory_Cisco <- subset(OldFactory, Species == "Cisco")
OldFactory_Trout <- subset(OldFactory, Species == "Trout")
OldFactory_Whitefish <- subset(OldFactory, Species == "Lake whitefish")
#Create regression output for Old Factory cisco, trout, and whitefish
# lm(y ~ x, data = dataframe_source)
OldFactory_Cisco_Regression <- lm(Catch ~ Year, data = OldFactory_Cisco)
OldFactory_Trout_Regression <- lm(Catch ~ Year, data = OldFactory_Trout)
OldFactory_Whitefish_Regression <- lm(Catch ~ Year, data = OldFactory_Whitefish)
#Display regression summary for Old Factory cisco, trout, and whitefish
summary(OldFactory_Cisco_Regression)
summary(OldFactory_Trout_Regression)
summary(OldFactory_Whitefish_Regression)

###################################################################
########################### Black Stone Bay #######################
###################################################################
# Create the plot object based on the following ggplot2 parameters
# X-axis data = Year
BlackStoneBayTotalCatchPlot <- qplot(Year, Catch, data = BlackStoneBay, shape = Species, color = Species, facets = Species~., size = I(3), xlab = "Year", ylab = "Total Catch")
BlackStoneBayTotalCatchPlot + geom_smooth(method = lm, se = FALSE, aes(group =1)) + theme_bw()
# Plot for PPTs
#BlackStoneBayTotalCatchPlot + geom_smooth(method = lm, colour = "darkred", size = 0.5, se = FALSE, aes(group =1)) + theme_bw() + geom_point(size = 4, colour = "black")
# Create subset datafrom for Black Stone Bay cisco, trout, and whitefish
BlackStoneBay_Cisco <- subset(BlackStoneBay, Species == "Cisco")
BlackStoneBay_Trout <- subset(BlackStoneBay, Species == "Trout")
BlackStoneBay_Whitefish <- subset(BlackStoneBay, Species == "Lake whitefish")
#Create regression output for Black Stone Bay cisco, trout, and whitefish
# lm(y ~ x, data = dataframe_source)
BlackStoneBay_Cisco_Regression <- lm(Catch ~ Year, data = BlackStoneBay_Cisco)
BlackStoneBay_Trout_Regression <- lm(Catch ~ Year, data = BlackStoneBay_Trout)
BlackStoneBay_Whitefish_Regression <- lm(Catch ~ Year, data = BlackStoneBay_Whitefish)
#Display regression summary for Black Stone Bay cisco, trout, and whitefish
summary(BlackStoneBay_Cisco_Regression)
summary(BlackStoneBay_Trout_Regression)
summary(BlackStoneBay_Whitefish_Regression)

###################################################################
########################### Goose Island ##########################
###################################################################
# Create the plot object based on the following ggplot2 parameters
# X-axis data = Year
GooseIslandTotalCatchPlot <- qplot(Year, Catch, data = GooseIsland, shape = Species, color = Species, facets = Species~., size = I(3), xlab = "Year", ylab = "Total Catch")
GooseIslandTotalCatchPlot + geom_smooth(method = lm, se = FALSE, aes(group =1)) + theme_bw()
# Plot for PPTs
#GooseIslandTotalCatchPlot + geom_smooth(method = lm, colour = "darkred", size = 0.5, se = FALSE, aes(group =1)) + theme_bw() + geom_point(size = 4, colour = "black")
# Create subset datafrom for Goose Island cisco, trout, and whitefish
GooseIsland_Cisco <- subset(GooseIsland, Species == "Cisco")
GooseIsland_Trout <- subset(GooseIsland, Species == "Trout")
GooseIsland_Whitefish <- subset(GooseIsland, Species == "Lake whitefish")
#Create regression output for Goose Island cisco, trout, and whitefish
# lm(y ~ x, data = dataframe_source)
GooseIsland_Cisco_Regression <- lm(Catch ~ Year, data = GooseIsland_Cisco)
GooseIsland_Trout_Regression <- lm(Catch ~ Year, data = GooseIsland_Trout)
GooseIsland_Whitefish_Regression <- lm(Catch ~ Year, data = GooseIsland_Whitefish)
#Display regression summary for Goose Island cisco, trout, and whitefish
summary(GooseIsland_Cisco_Regression)
summary(GooseIsland_Trout_Regression)
summary(GooseIsland_Whitefish_Regression)

###################################################################
########################### Paint Hills Bay #######################
###################################################################
# Create the plot object based on the following ggplot2 parameters
# X-axis data = Year
PaintHillsBayTotalCatchPlot <- qplot(Year, Catch, data = PaintHillsBay, shape = Species, color = Species, facets = Species~., size = I(3), xlab = "Year", ylab = "Total Catch")
PaintHillsBayTotalCatchPlot + geom_smooth(method = lm, se = FALSE, aes(group =1)) + theme_bw()
# Plot for PPTs
#PaintHillsBayTotalCatchPlot + geom_smooth(method = lm, colour = "darkred", size = 0.5, se = FALSE, aes(group =1)) + theme_bw() + geom_point(size = 4, colour = "black")
# Create subset datafrom for Paint Hills Bay cisco, trout, and whitefish
PaintHillsBay_Cisco <- subset(PaintHillsBay, Species == "Cisco")
PaintHillsBay_Trout <- subset(PaintHillsBay, Species == "Trout")
PaintHillsBay_Whitefish <- subset(PaintHillsBay, Species == "Lake whitefish")
#Create regression output for Paint Hills Bay cisco, trout, and whitefish
# lm(y ~ x, data = dataframe_source)
PaintHillsBay_Cisco_Regression <- lm(Catch ~ Year, data = PaintHillsBay_Cisco)
PaintHillsBay_Trout_Regression <- lm(Catch ~ Year, data = PaintHillsBay_Trout)
PaintHillsBay_Whitefish_Regression <- lm(Catch ~ Year, data = PaintHillsBay_Whitefish)
#Display regression summary for Paint Hills Bay cisco, trout, and whitefish
summary(PaintHillsBay_Cisco_Regression)
summary(PaintHillsBay_Trout_Regression)
summary(PaintHillsBay_Whitefish_Regression)

###################################################################
########################### Rabbits Ridge #########################
###################################################################
# Create the plot object based on the following ggplot2 parameters
# X-axis data = Year
RabbitsRidgeTotalCatchPlot <- qplot(Year, Catch, data = RabbitsRidge, shape = Species, color = Species, facets = Species~., size = I(3), xlab = "Year", ylab = "Total Catch")
RabbitsRidgeTotalCatchPlot + geom_smooth(method = lm, se = FALSE, aes(group =1)) + theme_bw()
# Plot for PPTs
#RabbitsRidgeTotalCatchPlot + geom_smooth(method = lm, colour = "darkred", size = 0.5, se = FALSE, aes(group =1)) + theme_bw() + geom_point(size = 4, colour = "black")
# Create subset datafrom for Rabbits Ridge cisco, trout, and whitefish
RabbitsRidge_Cisco <- subset(RabbitsRidge, Species == "Cisco")
RabbitsRidge_Trout <- subset(RabbitsRidge, Species == "Trout")
RabbitsRidge_Whitefish <- subset(RabbitsRidge, Species == "Lake whitefish")
#Create regression output for Rabbits Ridge cisco, trout, and whitefish
# lm(y ~ x, data = dataframe_source)
RabbitsRidge_Cisco_Regression <- lm(Catch ~ Year, data = RabbitsRidge_Cisco)
RabbitsRidge_Trout_Regression <- lm(Catch ~ Year, data = RabbitsRidge_Trout)
RabbitsRidge_Whitefish_Regression <- lm(Catch ~ Year, data = RabbitsRidge_Whitefish)
#Display regression summary for Rabbits Ridge cisco, trout, and whitefish
summary(RabbitsRidge_Cisco_Regression)
summary(RabbitsRidge_Trout_Regression)
summary(RabbitsRidge_Whitefish_Regression)

###################################################################
########################### Sculpin Island ########################
###################################################################
# Create the plot object based on the following ggplot2 parameters
# X-axis data = Year
SculpinIslandTotalCatchPlot <- qplot(Year, Catch, data = SculpinIsland, shape = Species, color = Species, facets = Species~., size = I(3), xlab = "Year", ylab = "Total Catch")
SculpinIslandTotalCatchPlot + geom_smooth(method = lm, se = FALSE, aes(group =1)) + theme_bw()
# Plot for PPTs
#SculpinIslandTotalCatchPlot + geom_smooth(method = lm, colour = "darkred", size = 0.5, se = FALSE, aes(group =1)) + theme_bw() + geom_point(size = 4, colour = "black")
# Create subset datafrom for Sculpin Island cisco, trout, and whitefish
SculpinIsland_Cisco <- subset(SculpinIsland, Species == "Cisco")
SculpinIsland_Trout <- subset(SculpinIsland, Species == "Trout")
SculpinIsland_Whitefish <- subset(SculpinIsland, Species == "Lake whitefish")
#Create regression output for Sculpin Island cisco, trout, and whitefish
# lm(y ~ x, data = dataframe_source)
SculpinIsland_Cisco_Regression <- lm(Catch ~ Year, data = SculpinIsland_Cisco)
SculpinIsland_Trout_Regression <- lm(Catch ~ Year, data = SculpinIsland_Trout)
SculpinIsland_Whitefish_Regression <- lm(Catch ~ Year, data = SculpinIsland_Whitefish)
#Display regression summary for Sculpin Island cisco, trout, and whitefish
summary(SculpinIsland_Cisco_Regression)
summary(SculpinIsland_Trout_Regression)
summary(SculpinIsland_Whitefish_Regression)

###################################################################
########################### Shephards Island ######################
###################################################################
# Create the plot object based on the following ggplot2 parameters
# X-axis data = Year
ShephardsIslandTotalCatchPlot <- qplot(Year, Catch, data = ShephardsIsland, shape = Species, color = Species, facets = Species~., size = I(3), xlab = "Year", ylab = "Total Catch")
ShephardsIslandTotalCatchPlot + geom_smooth(method = lm, se = FALSE, aes(group =1)) + theme_bw()
# Plot for PPTs
#ShephardsIslandTotalCatchPlot + geom_smooth(method = lm, colour = "darkred", size = 0.5, se = FALSE, aes(group =1)) + theme_bw() + geom_point(size = 4, colour = "black")
# Create subset datafrom for Shephards Island cisco, trout, and whitefish
ShephardsIsland_Cisco <- subset(ShephardsIsland, Species == "Cisco")
ShephardsIsland_Trout <- subset(ShephardsIsland, Species == "Trout")
ShephardsIsland_Whitefish <- subset(ShephardsIsland, Species == "Lake whitefish")
#Create regression output for Shephards Island cisco, trout, and whitefish
# lm(y ~ x, data = dataframe_source)
ShephardsIsland_Cisco_Regression <- lm(Catch ~ Year, data = ShephardsIsland_Cisco)
ShephardsIsland_Trout_Regression <- lm(Catch ~ Year, data = ShephardsIsland_Trout)
ShephardsIsland_Whitefish_Regression <- lm(Catch ~ Year, data = ShephardsIsland_Whitefish)
#Display regression summary for Shephards Island cisco, trout, and whitefish
summary(ShephardsIsland_Cisco_Regression)
summary(ShephardsIsland_Trout_Regression)
summary(ShephardsIsland_Whitefish_Regression)
