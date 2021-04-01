library("plot3D")
library(rgl)
library("scatterplot3d")
library(scatterD3)
data(iris)

#data <- read.csv("sample_batshi.csv")
num_part <- 1000
sd <- 1.5
inc<-1
child.sensitivity <- rnorm(num_part, mean = 3, sd = sd) #sample(seq(from = 1, to = 6, by = inc), size = num_part, replace = TRUE)
child.narrowing <- rnorm(num_part, mean = 3, sd = sd)# sample(seq(from = 1, to = 6, by = inc), size = num_part, replace = TRUE)
child.plasticity <- rnorm(num_part, mean = 8, sd = sd)# sample(seq(from = 5, to = 9, by = inc), size = num_part, replace = TRUE)
child.age <- sample(seq(from = 1, to = 17, by = inc), size = num_part, replace = TRUE)
child.category <- rep("child",num_part)
child.data <- data.frame(child.age, child.sensitivity, child.narrowing, child.plasticity, child.category)
colnames(child.data) <- c("age","sensitivity", "narrowing", "plasticity", "category")

asd.sensitivity <- rnorm(num_part, mean = 2, sd = 0.5) #sample(seq(from = 1, to = 6, by = inc), size = num_part, replace = TRUE)
asd.narrowing <- rnorm(num_part, mean = 1, sd = 0.5)# sample(seq(from = 1, to = 6, by = inc), size = num_part, replace = TRUE)
asd.plasticity <- rnorm(num_part, mean = 4, sd = 0.5) #sample(seq(from = 5, to = 9, by = inc), size = num_part, replace = TRUE)
asd.age <- sample(seq(from = 80, to = 100, by = inc), size = num_part, replace = TRUE)
asd.category <- rep("asd",num_part)
asd.data <- data.frame(asd.age, asd.sensitivity, asd.narrowing, asd.plasticity, asd.category)
colnames(asd.data) <- c("age","sensitivity", "narrowing", "plasticity", "category")

adult.sensitivity <- rnorm(num_part, mean = 8, sd = sd)#sample(seq(from = 5, to = 9, by = inc), size = num_part, replace = TRUE)
adult.narrowing <- rnorm(num_part, mean = 8, sd = sd)#sample(seq(from = 1, to = 6, by = inc), size = num_part, replace = TRUE)
adult.plasticity <- rnorm(num_part, mean = 3, sd = sd)# sample(seq(from = 1, to = 6, by = inc), size = num_part, replace = TRUE)
adult.age <- sample(seq(from = 18, to = 38, by = inc), size = num_part, replace = TRUE)
adult.category <- rep("adult",num_part)
adult.data <- data.frame(adult.age, adult.sensitivity, adult.narrowing, adult.plasticity, adult.category)
colnames(adult.data) <- c("age","sensitivity", "narrowing", "plasticity", "category")

all.data <- rbind(child.data, adult.data, asd.data)
all.data$category <- as.factor(all.data$category)
dev.off() 
with(all.data, scatter3D(x = sensitivity, y = narrowing, z = plasticity, colvar = as.integer(category), 
                         pch = 16, cex = 1.6, xlab = "Sensitivity", ylab = "Narrowing", 
                         zlab = "Plasticity", 
                         main = "Sample data",  
                         theta = 50, phi=10, d = 5, 
                         pch = 16, cex = 2,  
                         col=alpha.col(c("blue2","chocolate2","lightblue"),0.5),
                         colkey = list(at = c(1,2,3), side = 4, 
                                       addlines = TRUE, length = 0.5, width = 0.5,
                                       labels = c("adults", "asd","infancy") )))
ggsave("sample_test.svg")
with(all.data, scatter3D(x = sensitivity, y = narrowing, z = plasticity, colvar = age, 
                         pch = 16, cex = 1.6, xlab = "Sensitivity", ylab = "Narrowing", 
                         zlab = "Plasticity",
                         main = "Sample data",  
                        theta = 70,  phi=20, d = 5, col = ramp.col(col = c("lightblue", "blue2","chocolate2"), 100),#type = "h", 
                       colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75), addlines = FALSE))
ggsave(filename="sample_plot.svg")

scatter3d(x = all.data$sensitivity, y = all.data$narrowing, z = all.data$plasticity, groups = all.data$category,
          surface=FALSE, grid = FALSE, ellipsoid = TRUE)
#data$category <- as.factor(data$category)
data(mtcars)
library(plotly)
ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  visible=FALSE
)
plot_ly(all.data, x = ~sensitivity, y = ~narrowing, z = ~plasticity,
        type = 'scatter3d', mode = 'markers',
        marker = list(color = ~ age,#as.integer(category),
                      # Use either a named palette in the above list:
                        #colorscale = 'Greens',
                      # Or use a list of vector scales.
                      colorscale = list(c(0, rgb(34,139,34, max = 255)),  c(1, rgb(30,144,255, max = 255))),
                      showscale = TRUE)) %>%   layout(scene = list(xaxis = list(title = 'sensitivity'),
                                                                 yaxis = list(title = 'narrowing'),
                                                                 zaxis = list(title = 'plasticity')),
                                                    annotations = list(
                                                      x = 1.13,
                                                      y = 1.05,
                                                      text = '',
                                                      xref = 'paper',
                                                      yref = 'paper',
                                                      showarrow = FALSE
                                                    ), xaxis = ax, yaxis = ax)

mycolors <- c('royalblue1', 'darkcyan')
data$color <- mycolors[as.numeric(data$category)]
shapes = c(16, 17) 
shapes <- shapes[as.numeric(data$category)]
colors <- c("#E69F00", "#56B4E9")
colors <- colors[as.numeric(data$category)]
scatterplot3d(data[,2:4], angle = 40, 
              xlab="Sensitivity", zlab="Plasticity", ylab="Narrowing",
              color=colors,
              pch=shapes,
              grid=TRUE, box=FALSE)
addgrids3d(data[,2:4], grid = c("xy", "yz", "xz"))
legend("bottom", legend = levels(data$category),
       col =  c("#E69F00", "#56B4E9"), pch = 16, inset = -0.7, xpd = TRUE, horiz = TRUE)
#data <- iris

# Plot
par(mar=c(0,0,0,0))
plot3d( 
  x=data$sensitivity, y=data$plasticity, z=data$narrowing, 
  col = data$color, 
  type = 's', 
  radius = .1,
  xlab="Sensitivity", ylab="Plasticity", zlab="Narrowing")

x <- data$sensitivity
z <- data$plasticity
y <- data$narrowing
group <- data$category


par(mfrow = c(1, 1)) 
# first way, use vertical spikes (type = "h")
with(quakes, scatter3D(x = long, y = lat, z = -depth, colvar = mag, 
                       pch = 15, cex = 1.5, xlab = "longitude", ylab = "latitude", 
                       zlab = "depth, km", clab = c("Richter","Magnitude"),
                       main = "Earthquakes off Fiji", ticktype = "detailed", 
                       type = "h", theta = 10, d = 2, 
                       colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
)


panelfirst <- function(pmat) {
  zmin <- min(-quakes$depth)
  XY <- trans3D(quakes$long, quakes$lat, 
                z = rep(zmin, nrow(quakes)), pmat = pmat)
  scatter2D(XY$x, XY$y, colvar = quakes$mag, pch = ".", 
            cex = 2, add = TRUE, colkey = FALSE)
  
  xmin <- min(quakes$long)
  XY <- trans3D(x = rep(xmin, nrow(quakes)), y = quakes$lat, 
                z = -quakes$depth, pmat = pmat)
  scatter2D(XY$x, XY$y, colvar = quakes$mag, pch = ".", 
            cex = 2, add = TRUE, colkey = FALSE)
}

with(quakes, scatter3D(x = long, y = lat, z = -depth, colvar = mag, 
                       pch = 16, cex = 1.5, xlab = "longitude", ylab = "latitude", 
                       zlab = "depth, km", clab = c("Richter","Magnitude"),
                       main = "Earthquakes off Fiji", ticktype = "detailed", 
                       panel.first = panelfirst, theta = 10, d = 2, 
                       colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75))
)
