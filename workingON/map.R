# https://www.r-bloggers.com/2018/05/three-ways-of-visualizing-a-graph-on-a-map/
library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)


country_coords_txt <- "
 1     7.588576  47.559601         Freiburg/Basel
 2    7.842609  47.997791       NA
 3    13.404954  52.520008       Berlin/Potsdam
 4    13.063561  52.391842       NA
 5    -77.859909  40.798214       PennStateUniversity
"

# nodes come from the above table and contain geo-coordinates for some
# randomly picked countries
nodes <- read.delim(text = country_coords_txt, header = FALSE,
                    quote = "'", sep = "",
                    col.names = c('id', 'lon', 'lat', 'name'))


set.seed(123)  # set random generator state for the same output

N_EDGES_PER_NODE_MIN <- 1
N_EDGES_PER_NODE_MAX <- 1
N_CATEGORIES <- 1

# edges: create random connections between countries (nodes)
edges <- map_dfr(nodes$id, function(id) {
  n <- floor(runif(1, N_EDGES_PER_NODE_MIN, N_EDGES_PER_NODE_MAX+1))
  to <- sample(1:max(nodes$id), n, replace = FALSE)
  to <- to[to != id]
  categories <- sample(1:N_CATEGORIES, length(to), replace = TRUE)
  weights <- runif(length(to))
  data_frame(from = id, to = to, weight = weights, category = categories)
})

edges <- edges %>% mutate(category = as.factor(category))


g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

# nodes <- as_tibble(nodes)
# edges <- as_tibble(edges)
edges_for_plot <- edges %>%
  inner_join(nodes %>% dplyr::select(id, lon, lat), by = c('from' = 'id')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(nodes %>% dplyr::select(id, lon, lat), by = c('to' = 'id')) %>%
  rename(xend = lon, yend = lat)

assert_that(nrow(edges_for_plot) == nrow(edges))


nodes$weight =  .1 #degree(g)
edges_for_plot$weight = .1

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))


country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.05)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))



ggplot(nodes) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = category, size = weight),
             data = edges_for_plot, curvature = 0.33,
             alpha = 0.2) +
  scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
  geom_point(aes(x = lon, y = lat, size = weight),           # draw nodes
             shape = 1, fill = 'white',
             color = 'black', stroke = 0.5) +
  scale_size_continuous(guide = FALSE, range = c(1, 6)) +    # scale for node size
  geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
            hjust = 0, nudge_x = 1, nudge_y = 4,
            size = 5, color = "white", fontface = "bold") +
  mapcoords + maptheme
  # coord_cartesian(xlim=c(-150/zoomFactor,180/zoomFactor),
  #                 ylim=c(-55/zoomFactor2, 80/zoomFactor2))
