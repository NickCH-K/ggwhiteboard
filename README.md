# ggwhiteboard
Static and Animated Line, Shading, and Intersection Whiteboard Plots in **ggplot2**

This package is still in development and only has part of its features, and is not guaranteed to work. You can install its current version with `remotes::install_github('NickCH-K/ggwhiteboard')`.

The goal of this package is to make it easy to use the **ggplot2** and **gganimate** packages in R to draw "whiteboards" - graphs consisting of lines, intersections, and shaded areas, as you might draw yourself on a whiteboard in an economics or physics class, as opposed to a graph of data. The package will also make it easy to animate those whiteboards. Show how equilibrium price changes as demand shifts right, for example.

Use the package by building up whiteboard objects, starting with a board base like `blank_board()` and using functions like `add_line` and `add_point`. 

Then, display what you've made with `print()`, `ggboard()` (which produces a `ggplot` object that can be further customized), or `board_animate()`.

Example code:

```r
library(ggwhiteboard)
my_board <- blank_board(title='Supply and Demand') %>%
  add_line('S', fun = function(x) x) %>%
  add_line('D', fun = function(x) 100 - x) %>%
  add_point('Equilibrium', at = list('S','D'), hline = TRUE, vline = TRUE)

ggboard(my_board)
```
