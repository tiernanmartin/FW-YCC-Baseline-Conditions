# CAC BASELINE CONDITIONS REPORT

# SETUP: SOURCE R SCRIPTS -------------------------------------------------------------------------

system(command = "cp -f ~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/1_r_scripts/1_setup_1_functions.R ~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/shiny/UV_CensusGeo_Overlay/")

# -------------------------------------------------------------------------------------------------

# SETUP: SAVE SPATIAL DATA IN SHINY FOLDER --------------------------------------------------------

# tract_sea

readOGR(dsn = "./2_inputs/",layer = "tract_sea") %>% 
        spTransform(CRSobj = crs_proj) %>% 
        writeOGR(dsn = "./4_webcontent/shiny/UV_CensusGeo_Overlay",layer = "tract_sea",driver = "ESRI Shapefile",overwrite_layer = TRUE)

# tract_sea_out

readOGR(dsn = "./2_inputs/",layer = "tract_sea") %>% 
        spTransform(CRSobj = crs_proj) %>% 
        as('SpatialLines') %>% 
        mySptlLinesDF() %>% 
        writeOGR(dsn = "./4_webcontent/shiny/UV_CensusGeo_Overlay",layer = "tract_sea_out",driver = "ESRI Shapefile",overwrite_layer = TRUE)

# bg_sea

readOGR(dsn = "./2_inputs/",layer = "bg_sea") %>% 
        spTransform(CRSobj = crs_proj) %>% 
        writeOGR(dsn = "./4_webcontent/shiny/UV_CensusGeo_Overlay",layer = "bg_sea",driver = "ESRI Shapefile",overwrite_layer = TRUE)

# bg_sea_out

readOGR(dsn = "./2_inputs/",layer = "bg_sea") %>% 
        spTransform(CRSobj = crs_proj) %>% 
        as('SpatialLines') %>% 
        mySptlLinesDF() %>% 
        writeOGR(dsn = "./4_webcontent/shiny/UV_CensusGeo_Overlay",layer = "bg_sea_out",driver = "ESRI Shapefile",overwrite_layer = TRUE)

# blk_sea

readOGR(dsn = "./2_inputs/",layer = "blk_sea") %>% 
        spTransform(CRSobj = crs_proj) %>% 
        writeOGR(dsn = "./4_webcontent/shiny/UV_CensusGeo_Overlay",layer = "blk_sea",driver = "ESRI Shapefile",overwrite_layer = TRUE)

# blk_sea_out

readOGR(dsn = "./2_inputs/",layer = "blk_sea") %>% 
        spTransform(CRSobj = crs_proj) %>% 
        as('SpatialLines') %>% 
        mySptlLinesDF() %>% 
        writeOGR(dsn = "./4_webcontent/shiny/UV_CensusGeo_Overlay",layer = "blk_sea_out",driver = "ESRI Shapefile",overwrite_layer = TRUE)

# bg_uvs

readOGR(dsn = "./2_inputs/",layer = "bg_uvs") %>% 
        spTransform(CRSobj = crs_proj) %>% 
        writeOGR(dsn = "./4_webcontent/shiny/UV_CensusGeo_Overlay",layer = "bg_uvs",driver = "ESRI Shapefile",overwrite_layer = TRUE)

# blk_uvs

readOGR(dsn = "./2_inputs/",layer = "blk_uvs") %>% 
        spTransform(CRSobj = crs_proj) %>% 
        writeOGR(dsn = "./4_webcontent/shiny/UV_CensusGeo_Overlay",layer = "blk_uvs",driver = "ESRI Shapefile",overwrite_layer = TRUE)


# seaUvs

readOGR(dsn = "./2_inputs/",layer = "seaUvs") %>% 
        spTransform(CRSobj = crs_proj) %>% 
        writeOGR(dsn = "./4_webcontent/shiny/UV_CensusGeo_Overlay",layer = "seaUvs",driver = "ESRI Shapefile",overwrite_layer = TRUE)

# -------------------------------------------------------------------------------------------------

# SETUP: SAVE TABULAR DATA IN SHINY FOLDER --------------------------------------------------------

read_csv(file = "./2_inputs/wa_kc_blocks_hu/DEC_10_SF1_H1.csv",col_types = "cccn") %>% 
        write_csv(path = "./4_webcontent/shiny/UV_CensusGeo_Overlay/hu.csv")