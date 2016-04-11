bounds_ycc <- read_csv(file = "./bounds_ycc.csv") %>% t()
colnames(bounds_ycc) <- c("x","y")
bounds_ycc_cntr <- c((bounds_ycc[1,1]-bounds_ycc[2,1] )/2 + bounds_ycc[2,1],
                     (bounds_ycc[2,2]-bounds_ycc[1,2] )/2 + bounds_ycc[1,2])
# orig_df <- read_csv(file = "./Capacity_For_All_Parcel_2015.csv")
orig_data_glimpse <- read_csv("./orig_data_glimpse.csv")

if(!exists("parcel_ycc_reduc")){
        parcel_ycc_reduc <- 
                readOGR(dsn = ".",layer = "parcel_ycc_reduc") %>% 
                spTransform(CRSobj = crs_proj)
        
        cn <- read_csv(file = "./parcel_ycc_reduc_cn.csv") %>% unlist(use.names = F)
        
        colnames(parcel_ycc_reduc@data) <- cn
        
        parcel_ycc_reduc@data %<>%
                mutate(PUBLIC = as.logical(PUBLIC)) %>% 
                mutate(TAX_EXEMPT = as.logical(TAX_EXEMPT)) %>% 
                mutate(REDEV = as.logical(REDEV)) %>% 
                mutate(PARKING = as.logical(PARKING)) %>% 
                mutate(CHURCH = as.logical(CHURCH)) %>% 
                mutate(HIST_LNDMRK = as.logical(HIST_LNDMRK)) 
        
        parcel_ycc_reduc
        
        
}

np <- {
  if (!file.exists("./np.shp")) {
    make_np <- function() {
      ...
      np <- CHANGE_THIS
      writeOGR(obj = np, dsn = "./2_inputs/", layer = "np", 
        driver = "ESRI Shapefile")
      
      colnames(np@data) %>% data_frame() %>% write_csv(path = "./2_inputs/np_cn.csv")
      
      view_np <<- function() {
        
        myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
        
        pal <- colorNumeric(palette = myYlOrRd, domain = range(CHANGE_THIS))
        
        myLflt() %>% addPolygons(data = CHANGE_THIS, 
          smoothFactor = 0, color = col2hex("white"), 
          weight = 1.5, opacity = 0.5, fillColor = pal(CHANGE_THIS), 
          fillOpacity = 0.75) %>% addLegend(position = "topright", 
          title = "CHANGE_THIS", pal = pal, values = range(CHANGE_THIS), 
          opacity = 0.75, labFormat = labelFormat())
      }
      np
      
    }
    
    np <- make_np()
    rm(make_np)
    np
  } else {
    make_np <- function() {
      np <- readOGR(dsn = ".", layer = "np") %>% 
        spTransform(CRSobj = crs_proj)
      cn <- read_csv(file = "./parcel_ycc_reduc_cn.csv") %>% unlist(use.names = F)
      
      colnames(np@data) <- cn
      
      np
    }
    np <- make_np()
    rm(make_np)
    np
  }
}

# if(!exists("tract_sea")){
#         tract_sea <- 
#                 readOGR(dsn = ".",layer = "tract_sea") %>% 
#                 spTransform(CRSobj = crs_proj)
# }
# 
# if(!exists("tract_sea_out")){
#         tract_sea_out <- 
#                 readOGR(dsn = ".",layer = "tract_sea") %>% 
#                 spTransform(CRSobj = crs_proj) %>% 
#                 as('SpatialLines') %>% 
#                 mySptlLinesDF()
# }
# 
# if(!exists("bg_sea")){
#         bg_sea <- 
#                 readOGR(dsn = ".",layer = "bg_sea") %>% 
#                 spTransform(CRSobj = crs_proj)
# }
# 
# if(!exists("bg_sea_out")){
#         bg_sea_out <- 
#                 readOGR(dsn = ".",layer = "bg_sea") %>% 
#                 spTransform(CRSobj = crs_proj) %>% 
#                 as('SpatialLines') %>% 
#                 mySptlLinesDF()
# }
# 
# if(!exists("bg_uvs")){
#         bg_uvs <- 
#                 readOGR(dsn = ".",layer = "bg_uvs") %>% 
#                 spTransform(CRSobj = crs_proj)
# }
# 
# if(!exists("blk_sea")){
#         blk_sea <- 
#                 readOGR(dsn = ".",layer = "blk_sea") %>% 
#                 spTransform(CRSobj = crs_proj)
# }
# 
# if(!exists("blk_sea_out")){
#         blk_sea_out <- 
#                 readOGR(dsn = ".",layer = "blk_sea") %>% 
#                 spTransform(CRSobj = crs_proj) %>% 
#                 as('SpatialLines') %>% 
#                 mySptlLinesDF()
# }
# 
# if(!exists("blk_uvs")){
#         blk_uvs <- 
#                 readOGR(dsn = ".",layer = "blk_uvs") %>% 
#                 spTransform(CRSobj = crs_proj)
# }
# 
# if(!exists("seaUvs")){
#         seaUvs <- 
#                 readOGR(dsn = ".",layer = "seaUvs") %>% 
#                 spTransform(CRSobj = crs_proj)
# }
# 
# if(!exists("hu")){
#         hu <- read_csv(file = "hu.csv",col_types = "cccn") 
# }
# 
# if(!exists("seaAcsUvs")){
#         seaAcsUvs <- 
#                 readxl::read_excel(path = "dpdd017073.xlsx") %>% 
#                 mutate(TRACT_10 = str_pad(TRACT_10,width = 6, pad = "0"))
# }
# 
