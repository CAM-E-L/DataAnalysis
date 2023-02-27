df <- data.frame("from" = c("Bob", "Klaus", "Edith"),
                 "to" = c("Edith", "Edith", "Bob"))

meta <- data.frame("name" = c("Bob", "Klaus", "Edith", "Tom"),
                   "lon" = c(650, 450, 850, 600),
                   "lat" = c(400, 400, 400, 50))
meta
g <- graph.data.frame(df, directed = TRUE, vertices = meta)

lo <- layout.norm(layout = as.matrix(meta[,2:3]), xmin = 0, xmax = 1300, ymin = 0, ymax = 800)
lo
plot.igraph(g,
            layout = lo, xlim = c(0, 1300), ylim = c(0,800))


#####################
n = 5 # no. of nodes
set.seed(15)
nodes.coord <- data.frame(
  x=runif(n,min=0,max=n),
  y=runif(n,min=0,max=n)
)
nodes.coord <- data.frame(
  x=c(650, 450, 850, 600, 100),
  y=c(400, 400, 400, 50, 750)
)
nodes.coord$y <- 800-nodes.coord$y
nodes.coord
gg <- graph.empty(n)
plot(gg, layout=as.matrix(nodes.coord[,c("x","y")]))


as.matrix(nodes.coord[,c("x","y")])
