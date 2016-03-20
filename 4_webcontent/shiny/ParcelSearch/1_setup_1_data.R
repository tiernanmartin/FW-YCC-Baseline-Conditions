bounds_ycc <- {
        structure(c(-122.33967909, 47.59301, -122.292429, 47.630491), .Dim = c(2L, 
                                                                               2L), .Dimnames = list(c("x", "y"), c("min", "max")))
}

if(!exists("parcel_ycc")){
        parcel_ycc <- 
                readOGR(dsn = ".",layer = "parcel_ycc") %>% 
                spTransform(CRSobj = crs_proj)
        
        cn <- read_csv(file = "./parcel_ycc_cn.csv") %>% unlist(use.names = F)
        
        colnames(parcel_ycc@data) <- cn
        
        parcel_ycc %<>% 
                subset(!is.na(PUB_OWN_TYPE))
        
        parcel_ycc@data %<>%
                mutate(PUB_OWN_TYPE = ifelse(PUB_OWN_TYPE  == "CITY OF SEATTLE",
                                             "PUBLIC",
                                             PUB_OWN_TYPE))
        
        parcel_ycc
        
        
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
