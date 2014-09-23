install.packages('maps')
library('maps')
state.data <- cbind(state.x77, long=state.center$x, lat=state.center$y)
ill.rates <- state.data[,'Illiteracy']
state.names <- row.names(state.data)
state.data <- data.frame(state.data)


### Barplots
# Create a barplot showing illiteracy rates by state. Do you notice any issues?
barplot(ill.rates)

# Adjust your plot so that each bar is labeled. To do this, you will need to
# change the orientation and size of the state names.
barplot(ill.rates, main = "Illiteracy Rates By State", 
        ylab = "Illiteracy Rate",
        las = 2, cex.names = .5)

# Make the plot more informative by adding a title and label for the y-axis.


# Suppose we want to easily compare each state's illiteracy rate to the country
# wide average. Add a line to this plot at this average to make to allow for
# easy visual comparison. Make this line red. 
abline(h = mean(ill.rates), col = 'red')


# Arrange the plot in decreasing order, keep all other parameters the same.
ill.rates.sorted <- sort(ill.rates, decreasing = TRUE)

barplot(ill.rates.sorted, main = "Illiteracy Rates By State", 
        ylab = "Illiteracy Rate",
        las = 2, cex.names = .5)

abline(h = mean(ill.rates), col = 'red')



## Maps
# Suppose we were interested in how illiteracy rates varied geographically. The
# bar plots we just produced would be a poor tool for this. Maps make such a
# comparison easy.

# Plot a map of the contiguoug states. Fill each state in with grey.
map(database = "state", fill = T, col = "grey")


# The map you just produced has axes determined by longitude and latittude
# coordinates. Use the coordinates in <state.data> to label each state with
# its name
text(state.data$long,
     state.data$lat,
     row.names(state.data),
     cex = .6)


# The names are too big for a presentable plot. Decrease the size of the
# text by setting the appropriate parameter. Color the text red.
map(database = "state", fill = T, col = "dodgerblue")
text(state.data$long,
     state.data$lat,
     row.names(state.data),
     cex = .6)
     
# Add a circle for each state whose area is proportional to its illiteracy
# rate. Do you notice any issues? Adjust your plot accordingly.
map(database = "state", fill = T, col = "dodgerblue")
symbols(state.data$long,
        state.data$lat,
        circles = sqrt(ill.rates),
        add = TRUE,
        inches = FALSE)


# The illiteracy rate differences don't make it entirely clear. Color the 5 worst
# states red and the best 5 states green
top5 <- names(tail(ill.rates.sorted, 5))
bottom5 <- names(head(ill.rates.sorted, 5))

map(database = "state", fill = T, col = "grey")

with(state.data[row.names(state.data) %in% top5, ], 
     symbols(long, lat, circles = sqrt(Illiteracy), 
             add = TRUE, inches = FALSE, bg = "green"))

with(state.data[row.names(state.data) %in% bottom5, ], 
     symbols(long, lat, circles = sqrt(Illiteracy), 
             add = TRUE, inches = FALSE, bg = "red"))

