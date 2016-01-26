# CAC BASELINE CONDITIONS REPORT

# SETUP: LOAD PACKAGES AND PROJECT SETTINGS --------------------------------------------------------------

source("./1_r_scripts/1_setup_1_functions.R") # load the project settings, packages, and user-defined functions

source("./1_r_scripts/1_setup_2_spatialData.R") # load spatial data

# -------------------------------------------------------------------------------------------------        

# DEMOGRAPHIC DATA: SET UP ------------------------------------------------------------------------

# A function to create combined geographies of block.groups

make_nhood_acs <- function(nhood.abbr){
        
        tracts <- bg_rev@data %>% 
                filter(NHOOD.ABBR == nhood.abbr) %>% 
                select(TRACTCE) %>% 
                unlist() %>% 
                as.numeric()
        
        bgs <-  bg_rev@data %>% 
                filter(NHOOD.ABBR == nhood.abbr) %>% 
                select(BLKGRPCE) %>% 
                unlist() %>% 
                as.numeric()
        
        acs <- geo.make(state = "WA",
                 county = "King",
                 tract = tracts,
                 block.group = bgs,
                 combine = T,
                 combine.term = nhood.abbr)
        
        if(length(bgs) == 1){
                acs <- geo.make(state = "WA",
                                county = "King",
                                tract = tracts,
                                block.group = bgs)

        }
        return(acs)
}

# Use `make_nhood_acs` to create geographies for the study area neighborhoods

acs_bg_PS <- make_nhood_acs(nhood.abbr = "PS")

acs_bg_CID <- make_nhood_acs(nhood.abbr = "CID")

acs_bg_YTLS <- make_nhood_acs(nhood.abbr = "YTLS")

acs_bg_CD <- make_nhood_acs(nhood.abbr = "CD")

acs_bg_FH <- make_nhood_acs(nhood.abbr = "FH")

acs_bg_12AV <- make_nhood_acs(nhood.abbr = "12AV")

acs_sea <- geo.make(state = "WA", county = "King", place = "Seattle")

acs_KC <- geo.make(state = "WA", county = "King")

# Create a collection of all the neighborhood geographies

geo_bg <- c(acs_bg_CD,
         acs_bg_FH,
         acs_bg_12AV,
         acs_bg_PS,
         acs_bg_CID,
         acs_bg_YTLS)

geo_bg_plus <- c(acs_bg_CD,
                 acs_bg_FH,
                 acs_bg_12AV,
                 acs_bg_PS,
                 acs_bg_CID,
                 acs_bg_YTLS,
                 acs_sea,
                 acs_KC)
