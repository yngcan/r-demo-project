# Exploring Wikipedia in R

One of the nicest things about working in a browser-based environment like 
Sense is that you can use rich, interactive web-based visualizations. 
The possibilities are endless; if a vizualization works in a browser, then 
you can write a function analogous to `plot` that creates that visualization 
from your R data in your R session. 

#![Screen capture](https://s3.amazonaws.com/sense-files/R-cap-still.png)

For much more inspiration, check out [http://d3js.org](http://d3js.org)!

## User-friendly HTML widgets

Of course, some of those possibilities are easier to realize than others. This 
project will introduce you to four packages from the [htmlwidgets](http://www.htmlwidgets.org/)
collection, which bring web-based visualizations to your R session in an especially magical way: 
[threejs](http://www.htmlwidgets.org/showcase_threejs.html), 
[dygraphs](http://www.htmlwidgets.org/showcase_dygraphs.html), 
[networkD3](http://www.htmlwidgets.org/showcase_networkD3.html) and
[leaflet](http://www.htmlwidgets.org/showcase_leaflet.html). We'll demo each package 
using a live dataset from either Wikipedia or USGS, and you can get started experimenting with 
rich visualizations right away.