bounds_sea <- {
        structure(c(-122.43586659, 47.484564, -122.227151, 47.734145), .Dim = c(2L, 
                                                                                2L), .Dimnames = list(c("x", "y"), c("min", "max")))
}


if(!exists("tract_sea")){
        tract_sea <-
                readOGR(dsn = ".",layer = "tract_sea") %>%
                spTransform(CRSobj = crs_proj)
}

if(!exists("tract_sea_out")){
        tract_sea_out <-
                readOGR(dsn = ".",layer = "tract_sea") %>%
                spTransform(CRSobj = crs_proj) %>%
                as('SpatialLines') %>%
                mySptlLinesDF()
}

if(!exists("bg_sea")){
        bg_sea <-
                readOGR(dsn = ".",layer = "bg_sea") %>%
                spTransform(CRSobj = crs_proj)
}

if(!exists("bg_sea_out")){
        bg_sea_out <-
                readOGR(dsn = ".",layer = "bg_sea") %>%
                spTransform(CRSobj = crs_proj) %>%
                as('SpatialLines') %>%
                mySptlLinesDF()
}

if(!exists("bg_uvs")){
        bg_uvs <-
                readOGR(dsn = ".",layer = "bg_uvs") %>%
                spTransform(CRSobj = crs_proj)
}

if(!exists("blk_sea")){
        blk_sea <-
                readOGR(dsn = ".",layer = "blk_sea") %>%
                spTransform(CRSobj = crs_proj)
}

if(!exists("blk_sea_out")){
        blk_sea_out <-
                readOGR(dsn = ".",layer = "blk_sea") %>%
                spTransform(CRSobj = crs_proj) %>%
                as('SpatialLines') %>%
                mySptlLinesDF()
}

if(!exists("blk_uvs")){
        blk_uvs <-
                readOGR(dsn = ".",layer = "blk_uvs") %>%
                spTransform(CRSobj = crs_proj)
}

if(!exists("seaUvs")){
        seaUvs <-
                readOGR(dsn = ".",layer = "seaUvs") %>%
                spTransform(CRSobj = crs_proj)
}

if(!exists("hu")){
        hu <- read_csv(file = "hu.csv",col_types = "cccn")
}

if(!exists("seaAcsUvs")){
        seaAcsUvs <-
                readxl::read_excel(path = "dpdd017073.xlsx") %>%
                mutate(TRACT_10 = str_pad(TRACT_10,width = 6, pad = "0"))
}

