# Assumes you've run code intro_to_interactive.R up to this point in the tutorial to create
# us.state and pizza.grouped, and load necessary packages

# Option 1 - create the plot in ggplot and then convert to plotly ----

plot.option1 = ggplot() + 
  geom_sf(data = us.state, lwd = .3, color = "black", fill = "gray") + 
  xlim(-175, -67) +
  geom_point(data = pizza.grouped, aes(x = lon, y = lat, size = Pizzas_delivered), color = "red", alpha = .5) + 
  ggtitle("Option 1 - Build in ggplot")

plotly.option1 = ggplotly(plot.option1, tooltip = "Pizzas_delivered")
plotly.option1

# Notes:
# By default, the tooltip includes all aesthetics. We set it to just show pizzas delivered.

# Option 2 - Hybrid: start the plot in ggplot and then use plotly ----

plot.option2 = ggplot() + 
  geom_sf(data = us.state, lwd = .3, color = "black", fill = "gray") + 
  xlim(-175, -67) +
  ggtitle("Option 3 - Hybrid")

plotly.option2 = ggplotly(plot.option2) %>%
  add_markers(data = pizza.grouped, x = ~lon, y = ~lat,
              size = ~Pizzas_delivered, sizes = c(1,175), color = I("red"), alpha = .5,
              text = ~Pizzas_delivered, hoverinfo = "text", showlegend = TRUE)  %>%
  layout(showlegend = TRUE)
plotly.option2

# Notes:
#For scatterplots, the size argument controls the area of markers (unless otherwise specified via sizemode), and must be a numeric variable. The sizes argument controls the minimum and maximum size of circles, in pixels. (https://plotly-book.cpsievert.me/scatter-traces.html)

# Option 3 - create the plot in plotly ----

plotly.option3 = 
  plot_ly() %>%
  add_sf(data = us.state, line = list(width = 1, color = "black"), fillcolor = "gray", showlegend = FALSE) %>%
  add_markers(data = pizza.grouped, x = ~lon, y = ~lat, size = ~Pizzas_delivered, 
              color = I("red"), alpha = .5,
              text = ~Pizzas_delivered, hoverinfo = "text", showlegend = TRUE, name = "reports made") %>%
  layout(xaxis = list(range = c(-175, -67)), title = "Option 2 - Build in plotly", showlegend = TRUE)
plotly.option2  

# Notes:
# Similar layering, syntactical differences
# We supply text and hoverinfo
# ignore "No trace type specified" warnings, it's known weirdness: https://github.com/ropensci/plotly/issues/1202

# Examine structure of output ----

plotly_json(plotly.option1)

plotly_json(plotly.option2)

plotly_json(plotly.option3)



