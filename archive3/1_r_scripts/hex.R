# devtools::install_github("56north/hexamapmaker")
library(hexamapmaker)
library(ggplot2)

shp_df <- geo_join(spatial_data = bg_rev,
                   data_frame = medianIncome2014,
                   by_sp = "NHOOD.ABBR",by_df = "NHOOD.ABBR")

shp_cnt <- gCentroid(shp_df,byid = TRUE)


# Create data frame
# Notice the spacing of the points

x <- c(5,3,2,4,3,5,6)
y <- c(1,1,3,3,5,5,3)
id <- c("test1", "test2", "test3", "test4", "test5", "test6", "test7")
z <- data.frame(id,x,y)

# Plot points


ggplot(z, aes(x, y, group = id)) +
        geom_point() +
        coord_fixed(ratio = 1) +
        ylim(0,max(y)) + xlim(0,max(x))

# Turn points into hexagons

library(hexamapmaker)

zz <- hexamap(z)
zz_other <- zz %>% filter(id %!in% c("test1","test4"))

bs <- c(0,1)

svg("./3_outputs/pic.svg")
ggplot(zz, aes(x, y, group = id)) +
        geom_polygon(fill = "lightblue",size = 2,linetype = 0) +
        geom_polygon(data = zz_other,colour="white", fill = "lightblue",size = 2,linetype = 1) +
        coord_fixed(ratio = 1) +
        theme_void() %>% print()

dev.off()

library(grImport)

pic <- readPicture(rgmlFile = "./3_outputs/pic.svg")




        