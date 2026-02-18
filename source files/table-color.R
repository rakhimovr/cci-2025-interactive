# Function by Greg Lin
# Notice bias here = a positive number. 
# Higher values give more widely spaced colors at the high end
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}
good_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)
# Generate a vector of example numbers between 0 and 1
seq(0.1, 0.9, length.out = 12)
# [1] 0.1000000 0.1727273 0.2454545 0.3181818 0.3909091 0.4636364
# [7] 0.5363636 0.6090909 0.6818182 0.7545455 0.8272727 0.9000000
# create matching colors
good_color(seq(0.1, 0.9, length.out = 12))
# [1] "#E9F8CB" "#D9F2C0" "#C9ECB4" "#BDE6B2" "#B0E0AF" "#A4DAAD"
# [7] "#97D5AB" "#88CFAB" "#79C9AB" "#69C3AB" "#5ABDAB" "#4AB8AB"
# display the colors
seq(0.1, 0.9, length.out = 12) %>% 
  good_color() %>% 
  scales::show_col()