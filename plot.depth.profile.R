plot.depth.profile <- function (data, location, variable, months=4:10, years=min(data$Year):max(data$Year), add.legend = T) {

  library(RColorBrewer); library(viridisLite)
  
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
  temp <- temp[order(temp$Visit.Start.Date),]
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