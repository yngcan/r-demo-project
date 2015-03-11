# Modern data visualization in R
# ==============================
# 
# Graphics in R have been making huge strides recently thanks to
# projects like [htmlwidgets](https://htmlwidgets.org), which interface
# between R and rich interactive web graphics libraries like 
# [d3](https://d3js.org).
#
# This project shows off some of the awesome things you can do. Better
# still, you can use it to do your own graphical experiments from 
# right in your browser!

# Exploring recent Wikipedia edits
# --------------------------------
#
# Wikipedia's [public API](http://en.wikipedia.org/w/api.php) is an
# incredibly interesting data source. 
#
# We can use [networkd3](http://christophergandrud.github.io/networkD3/)
# to visualize the relationships between pages in a beautiful and
# interactive way. Here is a partial visualization of the network
# surrounding the Wikipedia page of a famous statistician.

source("wikipedia.utils/wikipedia.utils.r")
page.adjacents("David Blackwell")

# Now let's take a look at the cumulative revisions two different pages
# have been getting using the interactive time series library
# [dygraphs](http://rstudio.github.io/dygraphs/).

suppressWarnings(plot.two.revisions("J. K. Rowling", "George R. R. Martin"))

# Visualizing spatial datasets
# ----------------------------

# The [leaflet](http://rstudio.github.io/leaflet/) library generates
# beautiful interactive maps of geospatial datasets. Here's a map
# of the Wikipedia articles within 10km of a certain place, as
# geolocated by Google Maps via [ggmap](https://github.com/dkahle/ggmap). 
# Click on a point to see the title of the corresponding Wikipedia 
# page.

geo.search("Venice")

# Global datasets look awesome on [threejs](http://bwlewis.github.io/rthreejs/)'
# globe. Here are the last 24 hours' earthquakes, courtesy of
# [USGS](http://earthquake.usgs.gov/earthquakes/feed/v1.0/). The
# height of each spike is proportional to the Richter magnitude of 
# the quake.

globe.earthquakes()

# Maybe use the word cloud example? It's cool, but doesn't look great
# on Retina.
# wikipedia.word.cloud("Utopia", 5)

# Try it yourself!
# ----------------
# 
# If you're an R programmer yourself you might be tempted to replace 
# `geo.search("Venice")` with `geo.search("Kathmandu")` and see what you 
# come up with. And you can! Sign up for a Sense account, click the
# 'fork' button on this project to copy it to your account, and run a
# console. Then you can run these R functions with different arguments,
# write your own functions, and maybe even get some work done later!
# 
# If you come up with a visualization that you think is cool, please
# share it with us! Just grab the link to the console from your 
# browser's URL bar and tweet it to [@senseplatform](twitter.com/senseplatform).