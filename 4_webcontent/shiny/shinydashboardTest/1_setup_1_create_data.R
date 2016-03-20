# bounds_ycc --------

make_bounds_ycc <- function(){
        structure(c(-122.33967909, 47.59301, -122.292429, 47.630491), 
                                .Dim = c(2L,2L),
                                .Dimnames = list(c("x", "y"), c("min", "max"))) %>% 
                as.data.frame() %>% 
                write_csv(path = "./4_webcontent/shiny/ParcelSearch/bounds_ycc.csv")
                
                
}
make_bounds_ycc()

# parcel_ycc_reduc --------

make_parcel_ycc_reduc <- function(){
        
        parcel_ycc_reduc <- readOGR(dsn = "./4_webcontent/shiny/ParcelSearch/",layer = "parcel_ycc") %>% 
                spTransform(CRSobj = crs_proj)
        
        cn <- read_csv(file = "./4_webcontent/shiny/ParcelSearch/parcel_ycc_cn.csv") %>% unlist(use.names = F)
        
        colnames(parcel_ycc_reduc@data) <- cn
        
        parcel_ycc_reduc@data %<>%
                mutate(URL = paste0("http://blue.kingcounty.com/Assessor/eRealProperty/Dashboard.aspx?ParcelNbr=",PIN)) %>%
                replace_na(list(ADJRCAP_FL_AREA_MAX = 0,
                                BLDG_AV = 0,
                                LAND_AV = 0)) %>% 
                mutate(POT_UNITS = ifelse(is.na(ADJRCAP_FL_AREA_MAX) | ADJRCAP_FL_AREA_MAX == 0,
                                          0,
                                          ifelse(ADJRCAP_FL_AREA_MAX/1200 < 1,
                                                 1,
                                                 round_any(ADJRCAP_FL_AREA_MAX/1200,1,floor)))) %>% 
                mutate(SMPL_PUBOWN = ifelse(PUB_OWN_TYPE %in% c("CITY OF SEATTLE","PUBLIC") | RESSTAT %in% "PUBLIC",
                                                  "PUBLIC" ,
                                                  PUB_OWN_TYPE)) %>% 
                mutate(SMPL_TAXE = ifelse(PUB_OWN_TYPE %in% c("PRIVATE") & TAX_STATUS %in% c("X"),
                                          "Private & Tax-Exempt",
                                          "Public or Taxable")) %>% 
                mutate(ALL = TRUE) %>% 
                mutate(PUBLIC = ifelse(SMPL_PUBOWN %in% "PUBLIC",
                                       TRUE,
                                       FALSE)) %>% 
                mutate(TAX_EXEMPT = ifelse(PUB_OWN_TYPE %in% c("PRIVATE") & TAX_STATUS %in% c("X"),
                                           TRUE,
                                           FALSE)) %>% 
                mutate(REDEV = ifelse(RESSTAT %in% c("REDEV","VACANT"),
                                      TRUE,
                                      FALSE)) %>% 
                mutate(PARKING = ifelse(grepl("PARKING|PKG",PROP_NAME),
                                        TRUE,
                                        FALSE)) %>% 
                mutate(CHURCH = ifelse(grepl("CHURCH|TEMPLE",PROP_NAME),
                                       TRUE,
                                       FALSE)) %>% 
                mutate(HIST_LNDMRK = ifelse(RESSTAT %in% c("LANDMARK","HISTORIC"),
                                            TRUE,
                                            FALSE)) %>% 
                select(PROP_NAME,PIN,UV_TR,UV_BG,URL,ZONING,ZONELUT,PARCEL_DEV_SQFT,BLDG_AV,LAND_AV,PUB_OWN_TYPE,SMPL_PUBOWN,SMPL_TAXE,POT_UNITS,TAX_STATUS,RESSTAT,ADJRCAP_FL_AREA_MAX,ALL,PUBLIC,TAX_EXEMPT,REDEV,PARKING,CHURCH,HIST_LNDMRK)
        
        cn_reduc <- parcel_ycc_reduc@data %>% colnames() %>% as.data.frame()
        
        write_csv(x = cn_reduc,
                  path = "./4_webcontent/shiny/shinydashboardTest/parcel_ycc_reduc_cn.csv")
        
        writeOGR(obj = parcel_ycc_reduc,
                 dsn = "./4_webcontent/shiny/shinydashboardTest/",
                 layer = "parcel_ycc_reduc",
                 driver = "ESRI Shapefile",
                 overwrite_layer = T)
        
}
               
make_parcel_ycc_reduc()
