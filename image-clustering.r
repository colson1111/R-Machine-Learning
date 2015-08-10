#  uses k means to determine the most prevalent colors in a jpg image
# uses k nearest neighbors to identify the english word for the RGB colors that are returned

library(jpeg)
library(class)

# color csv from xkcd
#colors <- read.csv("~\\colormap.csv")

# URL of the .jpg image
url <- "http://vignette3.wikia.nocookie.net/mixelland/images/9/98/1024px-NSMBW_Artwork_Mario.jpg/revision/latest?cb=20150208022854"

# download the image
fn <- paste(tempfile(), "jpeg", sep=".")
download.file(url, fn, mode = ifelse(Sys.info()['sysname'] == "Windows", 'wb', 'w'))
readImage <- readJPEG(fn)
dm <- dim(readImage)

# create data frame of hexadecimal colors by x-y coordinate
rgbImage <- data.frame(
  x = rep(1:dm[2], each=dm[1]),
  y = rep(dm[1]:1, dm[2]),
  rvalue = as.vector(readImage[,,1]),
  gvalue = as.vector(readImage[,,2]),
  bvalue = as.vector(readImage[,,3]))
rgbImage<-data.table(rgbImage)

# only keep observations with color values under 0.9 (filter out white background of image)
nonwhite<-rgbImage[rvalue<.9 & gvalue<.9 & bvalue<.9]

nonwhite$rvalue <- nonwhite$rvalue * 255
nonwhite$gvalue <- nonwhite$gvalue * 255
nonwhite$bvalue <- nonwhite$bvalue * 255

nonwhite2 <- nonwhite[, .(rvalue, gvalue, bvalue)]
nonwhiteid <- nonwhite[, .(x, y)]


# use k means clustering to get average values of red, green, and blue
k <- 15
kmeans <- kmeans(nonwhite2, centers = k)
cluster_color <- rgb(kmeans$centers[kmeans$cluster, ], maxColorValue = 255)

nonwhite3<-cbind(nonwhiteid, nonwhite2)

# plot the original image with only k colors
plot(y ~ x, data=nonwhite3, main=sprintf("Item with %s Colors", k),
     col = cluster_color, asp = 1, pch = ".",
     axes=FALSE, ylab="",
     xlab="")

# get the rbg value of the cluster centers
centers <- data.table(kmeans$centers)
centers <- round(centers, digits = 0)

# gets color name from the csv file (if it exists)
threecolor <- sqldf("SELECT a.rvalue, a.gvalue, a.bvalue, b.color
                    FROM centers a
                    LEFT JOIN colors b ON a.rvalue = b.r AND a.gvalue = b.g AND a.bvalue = b.b")


# use k nearest neighbors to find the closest color from the csv file
colors_sub <- colors[, c("r", "g", "b")]
colors_factor <- colors[, c("color")]
check <- knn(train = colors_sub, test = centers, k=10, cl = colors_factor)
check


########## get the hex color
cluster_color <- rgb(centers[1,], maxColorValue = 255)
