###NPS Water - functions
#NKL
#7/23/2019

# Check for and install required packages
for (package in c('tidyverse', 'RColorBrewer', 'viridisLite', 'NADA', 'openair')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}  

###############################################
## function to plot spatio-temporal sampling effort across all locations for a given variable:
plot.sampling.effort <- function (data, metadata, variable) {
	# Create color palette for heatmaps
heat.pal.spectral <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
	#Calculate number of visits per year
waterDat$Year <- format(waterDat$Visit.Start.Date, "%Y")
temp <- waterDat %>%
	filter(Local.Characteristic.Name == variable) %>%
	group_by(StationID, Year) %>%
	dplyr::summarize(n.samps = length(unique(Visit.Start.Date))) %>%
	ungroup()
	#Plot spatio-temporal sampling effort	
	print(ggplot(data = temp, aes(x = Year, y = StationID, fill = n.samps)) +
  geom_raster() +
  scale_fill_gradientn(colours = heat.pal.spectral(100), name = "Visits") +
  theme_bw() +
  xlab("Year") +
  ylab("Site") +
  theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90),  text = element_text(size = 9)))
}


#####################################
## function to plot spring/summer depth profile for a variable, location, and date:
plot.depth.profiles <- function (data, location) {
waterDat$Month <- format(waterDat$Visit.Start.Date, "%m")
par(mfrow = c(1,3), oma = c(0,0,2,0))
variables <- c("Temp.C", "DO.mgL", "pH")
for (i in seq_along(variables)){
temp <- waterDat %>%
	filter(Local.Characteristic.Name == variables[i] & StationID == location) %>%
	filter(SampleDepth != "stream" & SampleDepth != "epilimnion" & SampleDepth != "bottom") %>%
	filter(Month == "04" | Month == "05" | Month == "06" | Month== "07" | Month == "08") %>%
	mutate(SampleDepth = as.numeric(SampleDepth)) %>%
	mutate(SampleDepth = plyr::round_any(SampleDepth, 1)) %>%
	mutate(Result.Value.Text = as.numeric(Result.Value.Text))
temp2 <- temp %>%
	group_by(SampleDepth) %>%
	dplyr::summarize(Median.Value = median(Result.Value.Text)) %>%
	ungroup()
	#Plot depth profile	
plot(temp$Result.Value.Text, -temp$SampleDepth, xaxt="n", xlim = c(min(temp$Result.Value.Text), max(temp$Result.Value.Text)), xlab = "", ylab = "Depth", cex.lab = 1.5, bty = "n", col = "gray")
points(temp2$Median.Value, -temp2$SampleDepth, pch = 19, cex = 2, lwd = 2, type = "o")
axis(3)
title(variables[i], line = 3)
	}
}




#######################################
## function to plot depth profiles for a variable, time period, location:
plot.depth.profile <- function (data, location, variable, months=4:10, years=min(data$Year):max(data$Year), add.legend = T) {
# waterDat$Month <- as.numeric(format(waterDat$Visit.Start.Date, "%m"))
# waterDat$Year <- as.numeric(format(waterDat$Visit.Start.Date, "%Y"))
temp <- data %>%
	dplyr::filter(DisplayName == variable & SiteName == location) %>%
	dplyr::filter(SampleDepth != "stream" & SampleDepth != "epilimnion") %>%
#   dplyr::filter(Month > min(months)-1 & Month < max(months)+1) %>%
# 	dplyr::filter(Month %in% months) %>%
	# dplyr::filter(Year %in% years) %>%
	mutate(SampleDepth = as.numeric(SampleDepth)) %>%
	mutate(SampleDepth = plyr::round_any(SampleDepth, 1)) %>%
	mutate(Result.Value.Text = as.numeric(Result.Value.Text))
temp2 <- temp %>%
	group_by(SampleDepth) %>%
	dplyr::summarize(Median.Value = median(Result.Value.Text)) %>%
	ungroup()
	#Plot depth profile	
#colours <-  rainbow(length(unique(temp$Visit.Start.Date)))
if (add.legend == T) {
	colours <-  viridis(length(unique(temp$Visit.Start.Date)))
	}
else {
	colours <- rep("lightgray", length(unique(temp$Visit.Start.Date)))
	}
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(temp$Result.Value.Text, -temp$SampleDepth, xaxt="n", xlim = c(min(temp$Result.Value.Text), max(temp$Result.Value.Text)), xlab = "", ylab = "Depth", cex.lab = 1.5, bty = "n", col = colours[as.numeric(as.factor(temp$Visit.Start.Date))])
points(temp2$Median.Value, -temp2$SampleDepth, pch = 19, cex = 2, lwd = 3, type = "o")
axis(3)
title(variable, line = 3)
if (add.legend == T) {
	legend("topright", col = colours, legend = unique(temp$Visit.Start.Date), pch = 1, title = location, inset = c(-0.2,0))
	}
}


###############################################
## function to plot time series of a variable at a location:
plot.ts <- function (data=waterDat, location, variable, months = min(data$Month):max(data$Month), years=min(data$Year):max(data$Year), add.legend = T) {
	waterDat$Month <- as.numeric(format(waterDat$Visit.Start.Date, "%m"))
waterDat$Year <- as.numeric(format(waterDat$Visit.Start.Date, "%Y"))
temp <- waterDat %>%
	dplyr::filter(Local.Characteristic.Name == variable & StationID == location) %>%
	dplyr::filter(SampleDepth == "stream" | SampleDepth == "epilimnion") %>%
	dplyr::filter(Month %in% months) %>%
	dplyr::filter(Year %in% years)
	#account for detection limits
	value <- as.numeric(ifelse(temp$Result.Value.Text == "*Present <QL", temp$Lower.Quantification.Limit, temp$Result.Value.Text))
	plotting.symbol <- ifelse(temp$Result.Value.Text == "*Present <QL", 1, 19)
	plotting.color <- ifelse(temp$Result.Value.Text == "*Present <QL", "red", "black")
	#Plot time series
plot(temp$Visit.Start.Date, value, bty = "l", type = "o", pch = plotting.symbol, col = plotting.color, xlab = "Date", ylab = variable)
	}