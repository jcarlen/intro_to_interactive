# R script companion to intro_to_interactive.Rmd
# Jane Carlen, March 2019

setwd("~/Documents/intro_to_interactive") # UPDATE path to intro_to_interactive

# Some participants had issues with code not running. In that case try updating your versions of the packages listed below and your version of R and RStudio to the latest (This code was written in R 3.5.2). 

# Some had issues with interactive plots not appearing in the viewer if a vpn was on. 

########################################## Basics ########################################################
# 0. Conditionally (if not already installed) install packages ----

package_list = c(
  #data processing
  "dplyr", "forcats", "reshape2",
  # plotting
  "ggplot2", "plotly", "crosstalk", "highcharter", "rbokeh", "DT", "listviewer", "widgetframe", "htmltools",
  # Additional packages for pizza example
  "sf", "emojifont", "USAboundaries", "USAboundariesData"
)

for (pkg in package_list) {
  if(!pkg %in% names(installed.packages()[,1])) {
    install.packages(pkg)
  }
}

# load all packages
lapply(package_list, require, character.only = TRUE)

# Some users had problems with the USAboundaries package. If so try installing directly from github
#devtools::install_github("ropensci/USAboundaries")
#devtools::install_github("ropensci/USAboundariesData")

# 1. Load Pizza Data ####

pizza = read.csv("pizza_midterms.csv")
names(pizza) # We're interested in a subset of these fields
head(pizza) # Look at the data

# Create data set grouped by polling place and status
pizza.grouped = pizza %>% group_by(lat, lon, Status) %>% 
  summarize(Pizzas_delivered = sum(Number_of_pizzas, na.rm = TRUE),
            Polling_place = first(Polling_place_address),
            City = first(City), # note there are some errors in city and state
            State = first(State))
pizza.grouped$Polling_place = gsub(", USA", "", pizza.grouped$Polling_place)
pizza.grouped$pizza = emoji(ifelse(pizza.grouped$Status == "Delivered", "pizza", "grey_question"))

#    Get geographic data (optional to add to plots) ####
us.state = us_boundaries(type = "state")
us.congressional = us_boundaries(type = "congressional")
us.county = us_boundaries(type = "county")

# 2. Example ####
names(txhousing) #If this doesn't work make sure plotly is loaded

tx.ggplot = ggplot(txhousing) + 
  geom_line(aes(x = interaction(month, year), y = median,
                group = city, color = city)) +
  theme(axis.text.x = element_text(angle = 90))

tx.ggplot

tx.ggplotly = ggplotly(tx.ggplot, tooltip = c("x", "median", "group")) 

tx.ggplotly

# 2a. Exercise start (create texas housing subset data) ####
txhousing.subset = txhousing %>%
  filter(year > 2006 & year < 2009, city %in% c("Dallas", "Houston", "San Antonio")) %>%
  mutate(year = as.factor(year))

txhousing1 = ggplotly(
  ggplot(txhousing.subset) +
    geom_line(aes(x = interaction(month, year), y = median,
                  group = city, color = city)) +
    theme(axis.text.x = element_text(angle = 90))
)

txhousing1

####################################### Customization ##################################################
# 3. Customizing plotly ####

txhousing1.modified = txhousing1 %>% 
  style(line = list(width  = 3), traces = c(2,3)) %>%
  layout(showlegend = FALSE, xaxis = list(title = "Time"), dragmode = "lasso")

txhousing1.modified
txhousing1.modified$x$data
plotly_json(txhousing1.modified)

txhousing3 = plot_ly(txhousing.subset) %>%
  add_markers(x = ~interaction(month, year), y = ~median, symbol = ~city, 
              symbols = c("cross", "square", "triangle-down"))

# 4. Data-plot-pipeline ####

tx.ggplotly2 = 
  tx.ggplotly %>%
  group_by(interaction(month, year)) %>%
  summarize(overall_med = median(median, na.rm = TRUE)) %>% 
  add_lines(y = ~overall_med, color = I("black"), size = I(3), name = "overall_med")

tx.ggplotly2

plotly_json(tx.ggplotly2)
plotly_data(tx.ggplotly2, id = 3)
names(plotly_data(tx.ggplotly2, id = 2)) 

# Branch off with `add_fun`. Or call external function with city name as argument

tx.ggplotly %>%
  add_fun(function(plot) {
    plot %>% ungroup() %>% filter(city == "Houston") %>%
      add_lines(y = ~median, name = "Houston", color = I("black"))
  }) %>%
  add_fun(function(plot) {
    plot %>% ungroup() %>% filter(city == "San Antonio") %>%
      add_lines(y = ~median, name = "San Antonio", linetype = I(3))
  })

# 5. Rangeslider ####

tx.ggplotly2 %>% rangeslider(start = 1, end = length(tx.ggplotly2$x$data[[1]]$x))

# 6. Interactive pizza plot (optional) ####
# First build the map plot (we'll use it later)
pizza.map = ggplot() + 
  geom_sf(data = us.congressional, lwd = .1) +   
  geom_sf(data = us.state, lwd = .3, fill = "darkgray", alpha = .5) +
  ylim(24,75) + xlim(-175, -67)

# Add points
pizza.points = pizza.map + 
  geom_text(data = filter(pizza.grouped %>% ungroup(), Status != "Delivered"),
            aes(x = lon, y = lat, label = pizza, label1 = City, label2 = Polling_place, label3 = Status), alpha = 1) +
  geom_text(data = filter(pizza.grouped %>% ungroup(), Status == "Delivered"), 
            aes(x = lon, y = lat, size = Pizzas_delivered, label = pizza, label1 = City, label2 = Polling_place, label3 = Status), alpha = 1)
#we incuded bad label aesthetics so they'd show up in the tool tip

# Clean it up
pizza.points = pizza.points +   
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "#FEEDAB")) +
  ggtitle("Pizza to the polls 2018 midterms: Locations of deliveries and reports")

# Convert to plotly
plotly.map.emoji = ggplotly(pizza.points, tooltip = c("Pizzas_delivered", "City", "Polling_place", "Status")) %>%
  layout(showlegend = TRUE, legend  = list(font = list(family = "EmojiOne"))) %>%
  style(traces = 4, name = "other report type \u2754", showlegend = TRUE) %>%
  style(traces = 5, name = "pizzas delivered \U0001f355", showlegend = TRUE) %>%
  layout(legend = list(bgcolor="#FEEDAB"))

plotly.map.emoji

########################################## Linking #######################################################
# 7. Linked plotting ####

sd <- SharedData$new(txhousing, ~city, "Select a city")

base <- plot_ly(sd, color = I("black"), height = 400) %>% group_by(city)

p1 <- base %>%
  summarise(miss = sum(is.na(median))) %>% 
  filter(miss > 0) %>%
  add_markers(x = ~miss, y = ~fct_reorder(city, miss), hoverinfo = "x+y") %>%
  layout(barmode = "overlay", xaxis = list(title = "Number of months missing")) 

p2 <- base %>% add_lines(x = ~date, y = ~median, alpha = 0.3)

subplot(p1, p2, titleX = TRUE, widths = c(0.3, 0.7)) %>% hide_legend() %>%
  highlight(dynamic = TRUE, selectize = TRUE)

# 8. Arranging linked plots ####

bscols(widths =c(4, 8), 
       list(div(align = "right", h1("These are")), p1), 
       list(h1("linked plots"), p2)) 

# 9. Other htmlwidgets ####

DT::datatable(txhousing, options = list(pageLength = 3))

# 10. Linked interactive pizza plot ####

##    Make necessary shared data sets ----

# Restrict to deliveries made and add a y variable for later
pizza.grouped.delivered = filter(pizza.grouped, Status == "Delivered") %>%
  group_by(Pizzas_delivered) %>% mutate(y = 1:n()) %>% ungroup()

shared_pizza_delivered = SharedData$new(pizza.grouped.delivered, key = ~Polling_place, group = "pizza_shared")
shared_pizza_delivered_small = SharedData$new(pizza.grouped.delivered %>% 
                                                mutate(n = Pizzas_delivered) %>%
                                                select("n", "City", "Polling_place"),
                                              key = ~Polling_place, group = "pizza_shared")

##    Main plot with shared data ----

plot.map.emoji = ggplot() + 
  # map
  geom_sf(data = us.state, lwd = .3, fill = NA) +
  geom_sf(data = us.congressional, lwd = .1, fill = NA) + 
  # points
  geom_text(data = shared_pizza_delivered, 
            #non-functioning aes label1 anpd label2 to pass these variables to plotly for the tooltip
            aes(x = lon, y = lat, size = Pizzas_delivered, label = pizza, label1 = City, label2 = Polling_place),
            alpha = 1) +
  ylim(24,75) + xlim(-175, -67) +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  ggtitle("Delivery locations")

plotly.map.emoji = ggplotly(plot.map.emoji, tooltip = c("size", "City", "Polling_place"), height = 500) %>%
  highlight(on = "plotly_selected", off = "plotly_doubleclick", opacityDim = .1, color = "red") #not recognizing highlight red?

##    Dotplot for right bar ----
#   I had issues with geom_dotplot converting to plotly so switched to geom_point

dotplot.pizzas.delivered = ggplot(shared_pizza_delivered) +
  geom_point(aes(x = Pizzas_delivered, y = y, group = Polling_place), size = .5) +
  ylab("Count") + 
  theme_bw() 

plotly.pizzas.delivered = 
  ggplotly(dotplot.pizzas.delivered, tooltip = c("x","group"), height = 200) %>% #"key" or "group" for tooltip. group gives a nicer label
  highlight(on = "plotly_selected", off = "plotly_doubleclick", opacityDim = .1, color = "red") 

##    Table for right bar ----
pizza.datatable = datatable(shared_pizza_delivered_small, options = list(pageLength = 5), #presentation version has only 3 rows, but here 5 looks better
                            selection = "multiple", height = "300px", rownames = F)

##    Panel plot ----
# unlike subplot, bscols naturally joins widgets of different types
pizza_linked = bscols(widths = c(8,4), plotly.map.emoji, list(plotly.pizzas.delivered, div(br()), pizza.datatable))
pizza_linked
#htmltools::save_html(pizza_linked, file = "pizza_linked.html")

###################################### Other packages ####################################################
# 11. rbokeh ####

p <- figure(data = txhousing.subset, tools = "wheel_zoom") %>%
  ly_points(x = listings, y = sales,
            glyph = city, color = year,
            hover = list(city, month, listings, sales)) %>%
  tool_pan()

p

class(p) #"rbokeh"   "htmlwidget"

# 12. highcharter ####

h <- highchart() %>%
  hc_add_series(txhousing.subset, "scatter",
                hcaes(x = listings, y = sales, color = year, group = city)) 

h

h <- hchart(txhousing.subset, "point",
            hcaes(x = listings, y = sales, color = year, group = city))

h

class(h) #"highchart"  "htmlwidget"

# 13. heatmap comparison ####

txhousing.heatmap.data = txhousing %>%
  mutate(time = as.character(interaction(year, month))) %>%
  select("city", "time", "median") %>%
  dcast(time ~ city, na.rm = T, value.var = "median") %>%
  select(-c("time")) %>%
  cor(use = "pairwise.complete.obs")

# base
image(as.matrix(txhousing.heatmap.data))
# ggplot
ggplot(melt(txhousing.heatmap.data)) + 
  geom_tile(aes(x = Var1, y = Var2, fill = value))
# plotly
ggplotly(ggplot(melt(txhousing.heatmap.data)) + 
           geom_tile(aes(x = Var1, y = Var2, fill = value)))
# rbokeh
figure(width = 600) %>% ly_crect(data = melt(txhousing.heatmap.data), 
                                 x = Var1, y = Var2, color = value)
# highcharter
hchart(melt(txhousing.heatmap.data), "heatmap", 
       hcaes(x = Var1, y = Var2, value = value))
##############################################################################################################