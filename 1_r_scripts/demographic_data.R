# CAC BASELINE CONDITIONS REPORT

# SETUP: LOAD PACKAGES AND PROJECT SETTINGS --------------------------------------------------------------

source(file = "./1_r_scripts/setup_and_spatial_data.R")

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

# DEMOGRAPHIC DATA: MEDIAN INCOME -----------------------------------------------------------------

medianIncome2014 <- {
        if(!file.exists("./2_inputs/medianIncome2014.csv")){
                Sys.sleep(1)
                acs <- acs.fetch(endyear = 2014, span = 5,
                                 geography = geo_bg,
                                 table.number = "B19001", col.names = "pretty")
                
                rnames <- acs@estimate %>% t()  %>% as.data.frame() %>% rownames() %>% as.data.frame()
                
                medInc <- 
                        acs@estimate %>% 
                        t() %>% 
                        as.data.frame() %>% 
                        bind_cols(rnames) %>% 
                        gather(geo,count,-.) 
                
                colnames(medInc) <- c("Range","Nhood","HH_Count")
                
                medInc <- 
                        medInc %>% 
                        mutate(Range = gsub("Household\\sIncome:\\s","",x = Range),
                               Nhood = as.character(Nhood),
                               Nhood = ifelse(grepl("\\s2",x = Nhood),
                                              "CID",
                                              ifelse(grepl("\\s1",x = Nhood),
                                                     "YTLS",
                                                     Nhood))) %>% 
                        filter(!grepl("Total",Range)) %>% 
                        mutate(RangeNum = as.numeric(gsub("[^\\d]+", "", Range, perl=TRUE))) %>% 
                        mutate(RangeStr = as.character(RangeNum),
                               RangeStrLen = round(str_length(RangeStr)/2)) %>% 
                        mutate(RangeLower = ifelse(str_length(RangeStr) == 6,
                                                   RangeNum,
                                                   ifelse(str_length(RangeStr) < 6,
                                                          1,
                                                          str_extract(RangeStr,paste0("^\\d{",RangeStrLen,"}"))))) %>% 
                        mutate(RangeUpper = ifelse(str_length(RangeStr) == 6,
                                                   500000,
                                                   ifelse(str_length(RangeStr) < 6,
                                                          RangeNum,
                                                          str_extract(RangeStr,paste0("\\d{",RangeStrLen,"}$"))))) %>% 
                        select(RangeDesc = Range,
                               RangeLower,
                               RangeUpper,
                               Nhood,
                               HH_Count) %>% 
                        group_by(Nhood) %>% 
                        mutate(RangeLower = as.numeric(RangeLower),
                               RangeUpper = as.numeric(RangeUpper),
                               CumSum = cumsum(HH_Count)) %>% 
                        do({
                                cs <- .["CumSum"] %>% unlist() %>% as.numeric() %>% as.vector()
                                midRow <- findInterval(max(cs)/2, cs) + 1
                                
                                a <- .[midRow,"RangeLower"]
                                b <- .[midRow,"RangeUpper"]
                                Pa <- .[midRow-1,"CumSum"] / .[nrow(.),"CumSum"]
                                Pb <- .[midRow,"CumSum"] / .[nrow(.),"CumSum"]
                                
                                thedaNum <- log(1-Pa)-log(1-Pb)
                                thedaDen <- log(b)-log(a)
                                theda <- thedaNum/thedaDen
                                
                                kNum <- Pb - Pa
                                kDen <- 1 / (a^theda) - 1 / (b^theda)
                                k <- (kNum / kDen) ^ (1 / theda) 
                                
                                medianEst <- k * (2^(1 / theda))
                                
                                medianEst <- as.vector(medianEst) %>% round(digits = -1)
                                
                                data.frame(.,medianEst)
                        }) %>% 
                        summarise(first(medianEst))
                
                colnames(medInc) <- c("NHOOD.ABBR","MEDIAN")
                
                readr::write_csv(x = medInc,path = "./2_inputs/medianIncome2014.csv")
        }
        
        medianIncome2014 <- readr::read_csv("./2_inputs/medianIncome2014.csv")
        
            
} # Note: this uses Pareto interpolation to estimate the median income value

medianIncome2014_plus <- {
        
        if(!file.exists("./2_inputs/medianIncome2014_plus.csv")){
                Sys.sleep(1)
                acs <- acs.fetch(endyear = 2014, span = 5,
                                 geography = geo_bg_plus,
                                 table.number = "S1903", col.names = "pretty")
                
                rnames <- acs@estimate %>% t()  %>% as.data.frame() %>% rownames() %>% as.data.frame()
                
                medInc <- 
                        acs@estimate %>% 
                        t() %>% 
                        as.data.frame() %>% 
                        bind_cols(rnames) %>% 
                        gather(geo,count,-.) 
                
                colnames(medInc) <- c("Range","Geo","HH_Count")
                
                medInc <- 
                        medInc %>% 
                        mutate(NewGeo = Geo) %>% 
                        mutate(NewGeo = replace(x = NewGeo, grep("\\s2",NewGeo, perl = TRUE),values = "CID")) %>% 
                        mutate(NewGeo = replace(x = NewGeo, grep("\\s1",NewGeo, perl = TRUE),values = "YTLS")) %>% 
                        mutate(NewGeo = replace(x = NewGeo, grep("^Seattle",NewGeo, perl = TRUE),values = "SEA")) %>% 
                        mutate(NewGeo = replace(x = NewGeo, grep("^King",NewGeo, perl = TRUE),values = "KC")) %>% 
                        select(-Geo) %>% 
                        select(Range,Geo = NewGeo,HH_Count) %>% 
                        mutate(Range = gsub("Household\\sIncome:\\s","",x = Range),
                               Geo = as.character(Geo)) %>% 
                        filter(!grepl("Total",Range)) %>% 
                        mutate(RangeNum = as.numeric(gsub("[^\\d]+", "", Range, perl=TRUE))) %>% 
                        mutate(RangeStr = as.character(RangeNum),
                               RangeStrLen = round(str_length(RangeStr)/2)) %>% 
                        mutate(RangeLower = ifelse(str_length(RangeStr) == 6,
                                                   RangeNum,
                                                   ifelse(str_length(RangeStr) < 6,
                                                          1,
                                                          str_extract(RangeStr,paste0("^\\d{",RangeStrLen,"}"))))) %>% 
                        mutate(RangeUpper = ifelse(str_length(RangeStr) == 6,
                                                   500000,
                                                   ifelse(str_length(RangeStr) < 6,
                                                          RangeNum,
                                                          str_extract(RangeStr,paste0("\\d{",RangeStrLen,"}$"))))) %>% 
                        select(RangeDesc = Range,
                               RangeLower,
                               RangeUpper,
                               Geo,
                               HH_Count) %>% 
                        group_by(Geo) %>% 
                        mutate(RangeLower = as.numeric(RangeLower),
                               RangeUpper = as.numeric(RangeUpper),
                               CumSum = cumsum(HH_Count)) %>% 
                        do({
                                cs <- .["CumSum"] %>% unlist() %>% as.numeric() %>% as.vector()
                                midRow <- findInterval(max(cs)/2, cs) + 1
                                
                                a <- .[midRow,"RangeLower"]
                                b <- .[midRow,"RangeUpper"]
                                Pa <- .[midRow-1,"CumSum"] / .[nrow(.),"CumSum"]
                                Pb <- .[midRow,"CumSum"] / .[nrow(.),"CumSum"]
                                
                                thedaNum <- log(1-Pa)-log(1-Pb)
                                thedaDen <- log(b)-log(a)
                                theda <- thedaNum/thedaDen
                                
                                kNum <- Pb - Pa
                                kDen <- 1 / (a^theda) - 1 / (b^theda)
                                k <- (kNum / kDen) ^ (1 / theda) 
                                
                                medianEst <- k * (2^(1 / theda))
                                
                                medianEst <- as.vector(medianEst) %>% round(digits = -1)
                                
                                data.frame(.,medianEst)
                        }) %>% 
                        summarise(first(medianEst))
                
                colnames(medInc) <- c("GEO","MEDIAN")
                
                readr::write_csv(x = medInc,path = "./2_inputs/medianIncome2014_plus.csv")
        }
        
        medianIncome2014_plus <- readr::read_csv(file = "./2_inputs/medianIncome2014_plus.csv")
        
} # Note: this uses Pareto interpolation to estimate the median income value

medHhInc_bar <- function(){
        
        blues <- RColorBrewer::brewer.pal(n = 9, name = "Blues")
        
        pal <- colorNumeric(palette = blues,
                            domain = c(round(min(medianIncome2014_plus$MEDIAN),-4),round(max(medianIncome2014_plus$MEDIAN),-3)))
        
        mypal <- medianIncome2014_plus$MEDIAN %>% 
                sort() %>% 
                pal()
       
        g <- ggplot(medianIncome2014_plus, aes(x=reorder(GEO, MEDIAN), y=MEDIAN)) +
                geom_bar(stat='identity',fill = mypal,alpha = .5) +
                geom_text(data = medianIncome2014_plus,label = paste0(medianIncome2014_plus$GEO,": $",format(medianIncome2014_plus$MEDIAN,big.mark=",")), hjust = 1.1) +
                scale_y_continuous(labels = scales::dollar) +
                coord_flip() +
                theme(
                        # plot.margin = unit(c(-10,0,0,0), "points"),
                        plot.margin = unit(c(0,0,0,0), "points"),
                        panel.background = element_rect(fill = "grey95"),
                        # panel.grid = element_blank(),
                        # panel.grid.minor = element_blank(), 
                        # panel.grid.major = element_blank(),
                        plot.background = element_blank(),
                        axis.title.y = element_blank(),
                        axis.title.x = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.y = element_blank(),
                        axis.text.x = element_text(),
                        strip.background = element_blank()
                        )
        
        gg <- grid.arrange(g, ncol=1, bottom = textGrob("Source: U.S. Census Bureau; American Community Survey,\n2014 American Community Survey 5-Year Estimates, Table B19001",x = 0.05,
                                                        hjust = 0, vjust = .5,
                                                        gp = gpar(fontface = "italic",
                                                                  fontsize = 10)))
        
        
}

{

# set the plot result back: dev.set(which = 1)
        
# Print Bar Plot       
       
# png('./4_webcontent/images/medHhInc_bar.png',width=400,height=400,res = 72,units="px",bg = "transparent")
# medHhInc_bar()
# dev.off()

# Print Bar Plot labels      

# png('~/Pictures/medHhInc_bar_labels.png',width=170,height=170,res = 72,units="px",bg = "transparent")
# medHhInc_bar_labels()
# dev.off()

}

myLflt_medInc <- function(){
        
        shp_df <- geo_join(spatial_data = bg_rev,
                           data_frame = medianIncome2014,
                           by_sp = "NHOOD.ABBR",by_df = "NHOOD.ABBR")
        
        blues <- RColorBrewer::brewer.pal(n = 9, name = "Blues")
        
        pal <- colorNumeric(palette = blues,
                            domain = c(0,round(max(medianIncome2014_plus$MEDIAN),-3)))
        basemapLab <- paste0(paste(rep("&nbsp;",times = 7),collapse = ""),"Basemap<br><div align=\"right\">Labels</div>")
        nhoodLab   <- paste0("Neighborhood<br><div align=\"right\">Labels</div>")
        
        
        leaflet() %>% 
                addProviderTiles("CartoDB.PositronNoLabels") %>% 
                addProviderTiles("CartoDB.Positron",group = basemapLab) %>% 
                setView(lng = myCACbound_cntr@coords[[1]],lat = myCACbound_cntr@coords[[2]],zoom = 13) %>% 
                addPolygons(data = shp_df,
                            smoothFactor = 0,
                            stroke = F,
                            fillColor = ~pal(shp_df@data[["MEDIAN"]]), fillOpacity = .5,
                            popup = paste0("<h3>",shp_df@data[["NHOOD.FULL"]],"</h3>",
                                           "Median Household Income",": $",format(shp_df@data[["MEDIAN"]],big.mark=","))) %>% 
                addPolylines(data = nhoods_census_outline,
                             color = col2hex("white"), weight = 3, opacity = .5,stroke = T,
                             fill = F)  %>%
                addLegend(pal = pal, 
                          values = range(c(0,round(max(medianIncome2014_plus$MEDIAN),-3))),
                          labFormat = labelFormat(prefix = "$"),
                          position = "topright", 
                          title = "Median<br>Household<br>Income",
                          opacity = .5) %>% 
                addCircleMarkers(lng = bg_hood_cntrs@coords[,1],lat = bg_hood_cntrs@coords[,2],
                                 stroke = FALSE, fillOpacity = 0,
                                 group = nhoodLab,
                                  label = bg_hood_cntrs$NHOOD,
                                  labelOptions = lapply(1:nrow(bg_hood_cntrs),function(x){
                                          labelOptions(opacity = .5,noHide = TRUE, offset = c(0,-20))
                                  })) %>% 
                addLayersControl(overlayGroups = c(basemapLab,nhoodLab),position = "topright",
                                 options = layersControlOptions(collapsed = TRUE))
}

# myLflt_medInc() %>% saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_medHhInc.html")

medianIncom2014_disag <- {
        acs <- acs.fetch(endyear = 2014, span = 5,
                         geography = geo_bg_plus,
                         table.number = "B19001", col.names = "pretty")
        
        rnames <- acs@estimate %>% t()  %>% as.data.frame() %>% rownames() %>% as.data.frame()
        
        medInc <- 
                acs@estimate %>% 
                t() %>% 
                as.data.frame() %>% 
                bind_cols(rnames)
}


myLflt_medInc_disag <- function(){
        
        shp_df <- geo_join(spatial_data = bg_rev,
                           data_frame = medianIncome2014,
                           by_sp = "NHOOD.ABBR",by_df = "NHOOD.ABBR")
        
        blues <- RColorBrewer::brewer.pal(n = 9, name = "Blues")
        
        pal <- colorNumeric(palette = blues,
                            domain = c(round(min(medianIncome2014_plus$MEDIAN),-4),round(max(medianIncome2014_plus$MEDIAN),-3)))
        
        pal <- colorNumeric(palette = blues,
                            domain = c(0,round(max(medianIncome2014_plus$MEDIAN),-3)))
        basemapLab <- paste0(paste(rep("&nbsp;",times = 7),collapse = ""),"Basemap<br><div align=\"right\">Labels</div>")
        nhoodLab   <- paste0("Neighborhood<br><div align=\"right\">Labels</div>")
        
        
        leaflet() %>% 
                addProviderTiles("CartoDB.PositronNoLabels") %>% 
                addProviderTiles("CartoDB.Positron",group = basemapLab) %>% 
                setView(lng = myCACbound_cntr@coords[[1]],lat = myCACbound_cntr@coords[[2]],zoom = 13) %>% 
                addPolygons(data = shp_df,
                            smoothFactor = 0,
                            stroke = F,
                            fillColor = ~pal(shp_df@data[["MEDIAN"]]), fillOpacity = .5,
                            popup = paste0("<h3>",shp_df@data[["NHOOD.FULL"]],"</h3>",
                                           "Median Household Income",": $",format(shp_df@data[["MEDIAN"]],big.mark=","))) %>% 
                addPolylines(data = nhoods_census_outline,
                             color = col2hex("white"), weight = 3, opacity = .5,stroke = T,
                             fill = F)  %>%
                addLegend(pal = pal, 
                          values = range(c(0,round(max(medianIncome2014_plus$MEDIAN),-3))),
                          labFormat = labelFormat(prefix = "$"),
                          position = "topright", 
                          title = "Median<br>Household<br>Income",
                          opacity = .5) %>% 
                addCircleMarkers(lng = bg_hood_cntrs@coords[,1],lat = bg_hood_cntrs@coords[,2],
                                 stroke = FALSE, fillOpacity = 0,
                                 group = nhoodLab,
                                 label = bg_hood_cntrs$NHOOD,
                                 labelOptions = lapply(1:nrow(bg_hood_cntrs),function(x){
                                         labelOptions(opacity = .5,noHide = TRUE, offset = c(0,-20))
                                 })) %>% 
                addLayersControl(overlayGroups = c(basemapLab,nhoodLab),position = "topright",
                                 options = layersControlOptions(collapsed = TRUE))
}


# DEMOGRAPHIC DATA: LESS THAN 200% POVERTY --------------------------------------------------------

below200PctPov <- {
        
        make_below200PctPov <- function(){
                
                
                
                
        }
        
        below200PctPov <- make_below200PctPov()
        
        rm(make_below200PctPov)
        
        below200PctPov
        
}


# ARCHIVED CODE -----------------------------------------------------------------------------------

# Census data functions

getBgData <- function(df,tableNumber) {
        
        tracts <- tract_rev@data$TRACTCE %>% as.numeric()
        
        geo <-
                geo.make(
                        state = "WA",
                        county = "King",
                        tract = tracts,
                        block.group = "*"
                )
        
        df <-
                acs.fetch(
                        endyear = 2014,
                        span = 5,
                        geography = geo,
                        # fetch the corresponding census data
                        table.number = tableNumber,
                        col.names = "pretty"
                )
        
        dim <- dimnames(df@estimate)[[2]]
        
        df <- {
                data.frame(
                        paste0(
                                str_pad(
                                        string = df@geography$state,
                                        width = 2,
                                        side = "left",
                                        pad = "0"
                                ),
                                str_pad(
                                        string = df@geography$county,
                                        width = 3,
                                        side = "left",
                                        pad = "0"
                                ),
                                str_pad(
                                        string = df@geography$tract,
                                        width = 6,
                                        side = "left",
                                        pad = "0"
                                ),
                                df@geography$blockgroup
                        ),
                        df@estimate[, dim]
                )
        }
        
        rownames(df) <- c()
        
        return(df)
}

cleanBgData <- function(df, colNames){
        
        colnames(df) <- colNames
        
        readr::write_csv(df,path = path)
}


# Median Income


medianIncom2014_disag <- {
        dfName <- "medianIncome_disag"
        
        path <- paste0("./2_inputs/",dfName,".csv")
        
        df <- getBgData(tableNumber = "B19013")
        
        colnames(df) <- c("GEOID","MEDIAN")
        
        shp <- geo_join(spatial_data = bg_rev,data_frame = df, by_sp = "GEOID", by_df = "GEOID")
        
}

myLflt_medInc_disag <- function(){
        
        
        blues <- RColorBrewer::brewer.pal(n = 9, name = "Blues")
        
        pal <- colorNumeric(palette = blues,
                            domain = c(0,round(max(medianIncome2014_plus$MEDIAN),-3)))
        basemapLab <- paste0(paste(rep("&nbsp;",times = 7),collapse = ""),"Basemap<br><div align=\"right\">Labels</div>")
        nhoodLab   <- paste0("Neighborhood<br><div align=\"right\">Labels</div>")
        
        
        leaflet() %>% 
                addProviderTiles("CartoDB.PositronNoLabels") %>% 
                addProviderTiles("CartoDB.Positron",group = basemapLab) %>%  
                setView(lng = myCACbound_cntr@coords[[1]],lat = myCACbound_cntr@coords[[2]],zoom = 13) %>% 
                addPolygons(data = medianIncom2014_disag,
                            smoothFactor = 0,
                            stroke = F,
                            fillColor = ~pal(medianIncom2014_disag@data[["MEDIAN"]]), fillOpacity = .5,
                            popup = paste0("<h3>",medianIncom2014_disag@data[["GEOID"]],"</h3>",
                                           "Median Household Income",": $",format(medianIncom2014_disag@data[["MEDIAN"]],big.mark=","))) %>% 
                addPolygons(data = bg_rev,
                            fill = FALSE,
                            weight = 3, color = col2hex("white"), opacity = .5)  %>%
                addLegend(pal = pal, 
                          values = range(c(0,round(max(medianIncome2014_plus$MEDIAN),-3))),
                          labFormat = labelFormat(prefix = "$"),
                          position = "topright", 
                          title = "Median<br>Household<br>Income",
                          opacity = .5)
}

myLflt_medInc_disag() %>% 
        saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/myLflt_medInc_disag.html")

myLflt_medInc_disag_regZoom <- function(){
        
        
        blues <- RColorBrewer::brewer.pal(n = 9, name = "Blues")
        
        pal <- colorNumeric(palette = blues,
                            domain = c(0,round(max(medianIncome2014_plus$MEDIAN),-3)))
        basemapLab <- paste0(paste(rep("&nbsp;",times = 7),collapse = ""),"Basemap<br><div align=\"right\">Labels</div>")
        nhoodLab   <- paste0("Neighborhood<br><div align=\"right\">Labels</div>")
        
        
        leaflet() %>% 
                addProviderTiles("CartoDB.PositronNoLabels") %>% 
                addProviderTiles("CartoDB.Positron",group = basemapLab) %>%  
                addPolygons(data = medianIncom2014_disag,
                            smoothFactor = 0,
                            stroke = F,
                            fillColor = ~pal(medianIncom2014_disag@data[["MEDIAN"]]), fillOpacity = .5,
                            popup = paste0("<h3>",medianIncom2014_disag@data[["GEOID"]],"</h3>",
                                           "Median Household Income",": $",format(medianIncom2014_disag@data[["MEDIAN"]],big.mark=","))) %>% 
                addPolygons(data = bg_rev,
                            fill = FALSE,
                            weight = 3, color = col2hex("white"), opacity = .5)  %>%
                addLegend(pal = pal, 
                          values = range(c(0,round(max(medianIncome2014_plus$MEDIAN),-3))),
                          labFormat = labelFormat(prefix = "$"),
                          position = "topright", 
                          title = "Median<br>Household<br>Income",
                          opacity = .5)
}

myLflt_medInc_disag_regZoom() %>% 
        saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/myLflt_medInc_disagRegZoom.html")


# Poverty
        
below200PctPov <- {
 
        dfName <- "below200PctPov"
        
        path <- paste0("./2_inputs/",dfName,".csv")
        
        if(!file.exists(path)){
                
                # getBgData(tableNumber = "C17002") %>% glimpse()
                
                getBgData(tableNumber = "C17002") %>% 
                        cleanBgData(colNames = c("GEOID",
                                                 "Total",
                                                 "under50",
                                                 "x50to99",
                                                 "x100to124",
                                                 "x125to149",
                                                 "x150to184",
                                                 "x185to199",
                                                 "x200andOver"))   
        }
        
        readr::read_csv(file = path,col_types = "cnnnnnnnn")
}

below200PctPov%<>%
        mutate(PctBelow200 = 1 - (x200andOver/Total)) %>% 
        mutate(PctBelow200 = 100*PctBelow200) %>% 
        mutate(PctBelow200 = round(PctBelow200,0)) %>% 
        select(GEOID,PctBelow200)

# Income

hhIncomeMedian_bg <- {
        
        dfName <- "hhIncomeMedian_bg"
        
        path <- paste0("./2_inputs/",dfName,".csv")
        
        if(!file.exists(path)){
                
                # getBgData(tableNumber = "B19013") %>% glimpse()
                
                getBgData(tableNumber = "B19013") %>% 
                        cleanBgData(colNames = c("GEOID",
                                                 "MedianIncome"))   
        }
        
        readr::read_csv(file = path,col_types = "cn")
}

# create a map to test the validity of the data

myLflt_test <- function(sp = bg_rev,df,colName,popupHeading,legTitle){
        
        test <- myGeoJoin(spatial_data = sp,data_frame = df)
        
        pal <- colorNumeric(palette = "Blues",domain = test@data[,colName])
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = blk_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 1, opacity = .5,
                            group = "1. Blocks") %>% 
                addPolygons(data = test,
                            fillColor = ~pal(test@data[,colName]), fillOpacity = .5,
                            color = col2hex("white"), weight = 3, opacity = .75,dashArray = "5, 10",
                            group = "2. Block Groups",
                            popup = paste0("GEOID: ",test@data$GEOID,"<br>",
                                           popupHeading,": ",test@data[,colName],"<br>",
                                           test@data[,"NHOOD.FULL"])) %>% 
                addPolygons(data = tract_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 5, opacity = .9,
                            group = "3. Tracts") %>% 
                addPolygons(data = myCACbound,
                            fill = FALSE,
                            weight = 3,
                            color = "#ff9900",
                            dashArray = "5, 5",
                            opacity = 1,
                            group = "4. CAC Boundary") %>%
                addLayersControl(overlayGroups = c("1. Blocks","2. Block Groups","3. Tracts","4. CAC Boundary"),
                                 options = layersControlOptions(collapsed = FALSE)) %>% 
                addLegend(pal = pal, 
                          values = range(test@data[,colName]), 
                          position = "topright", 
                          title = legTitle,
                          opacity = 1)
        
        
}


# Example:
myLflt_test(df = hhIncomeMedian_bg,
            colName = "MedianIncome",
            popupHeading = "Median HH Income",
            legTitle = "Median Household Income (2014)")

# ------------------------------------------------------------------------------------------------- 
#  

