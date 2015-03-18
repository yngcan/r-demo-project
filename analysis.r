# Modern data visualization in R
# ==============================
# 
# Graphics in R have been making huge strides recently thanks to
# projects like [htmlwidgets](https://htmlwidgets.org), which interface
# between R and rich interactive web graphics libraries like 
# [d3](https://d3js.org). This project shows off some of the awesome 
# things you can do with datasets drawn mostly from [Wikipedia](https://en.wikipedia.org).

# Visualizing Wikipedia Neighborhoods
# -----------------------------------
#
# Wikipedia's [public API](http://en.wikipedia.org/w/api.php) is an
# incredibly interesting data source. 
#
# We can use [networkd3](http://christophergandrud.github.io/networkD3/)
# to visualize the links between pages in a beautiful and
# interactive way. Here's the [neighborhood](http://en.wikipedia.org/wiki/Neighbourhood_(graph_theory)
# of the Wikipedia page of a famous statistician.

source("wikipedia.utils/wikipedia.utils.r")
page.neighborhood("David Blackwell")

# For a more meta experience, try `page.neighborhood("Neighbourhood (graph theory)")`!

# Comparing revision activity
# ---------------------------
#
# Now let's take a look at the cumulative revisions two different pages
# have been getting using the interactive time series library
# [dygraphs](http://rstudio.github.io/dygraphs/).

compare.cumulative.revisions("J. K. Rowling", "George R. R. Martin")

# If fiction isn't your thing, try `compare.cumulative.revisions("John von Neumann", "Alan Turing")`.
#
# Mapping nearby pages
# --------------------

# The [leaflet](http://rstudio.github.io/leaflet/) library generates
# beautiful interactive maps of geospatial datasets. Here's a map
# of the Wikipedia articles within 10km of a certain place, as
# geolocated by Google Maps via [ggmap](https://github.com/dkahle/ggmap). 
# Click on a point to see the title of the corresponding Wikipedia 
# page.

nearby.pages("Venice")

# Try replacing `geo.search("Venice")` with, e.ge, `geo.search("Taj Mahal")` and see what 
# you come up with! If you're not crazy about the black and white [map tiles](http://maps.stamen.com/#toner), 
# you can change the design by editing `wikipedia_utils/wikipedia_utils.r`

# The last 24 hours' earthquakes on a globe
# -----------------------------------------
#
# Global datasets look awesome on [threejs](http://bwlewis.github.io/rthreejs/)'
# globe. Here are the last 24 hours' earthquakes, courtesy of a non-Wikipedia
# data source: [USGS' earthquake feed](http://earthquake.usgs.gov/earthquakes/feed/v1.0/). The
# height of each spike is proportional to the Richter magnitude of 
# the quake.

previous.day.earthquakes()

# Try it yourself!
# ----------------
# 
# Try calling these functions with different arguments. If you want to see how they're
# implemented, or change the implementation, see `wikipedia_utils/wikipedia_utils.r`.
# 
# If you come up with a visualization that you think is cool, please
# share it with us! Just grab the link console link from your 
# browser's URL bar and tweet it to [@senseplatform](https://twitter.com/senseplatform).