# Example of summary  plots in plotly

plot_ly(txhousing) %>% 
  add_boxplot(x = ~year, y = ~median, group = ~year, text = ~city)

plotly_data(.Last.value)

ggplotly(ggplot(txhousing) +
  geom_boxplot(aes(x = year, y = median, group = year)))

# How keys work with summary data
# In fact, anytime a key variable is supplied to ggplot2 layer with a non-identity statistic, it automatically attaches all the unique key values tied to all the observations that went into creating the group – enabling linked selections between raw and aggregated forms of the data.

### Animation is also very easy: ####

gg <- ggplot(txhousing.subset, aes(frame = interaction(month, year))) +
  geom_point(aes(y = median, x =interaction(month, year), color = city)) 

ggplotly(gg)  

# You can link animated views too, see https://plotly-book.cpsievert.me/linking-animated-views.html
 