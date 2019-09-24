###NPS Water - functions
#NKL


###############################################
## function to plot spatio-temporal sampling effort across all locations for a given variable:
plot.sampling.effort <- function (data, park, type, variable) {
	# Create color palette for heatmaps
heat.pal.spectral <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
	#Calculate number of visits per year
temp <- data %>%
	filter(LongName %in% park & Type %in% type & DisplayName %in% variable) %>%
	group_by(SiteName, Year) %>%
	dplyr::summarize(n.samps = length(unique(Visit.Start.Date))) %>%
	ungroup()
	#Plot spatio-temporal sampling effort	
	print(ggplot(data = temp, aes(x = Year, y = SiteName, fill = n.samps)) +
  geom_raster() +
  scale_fill_gradientn(colours = heat.pal.spectral(100), name = "Visits") +
  theme_bw() +
  xlab("Year") +
  ylab("Site") +
  theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90),  text = element_text(size = 9)))
}
