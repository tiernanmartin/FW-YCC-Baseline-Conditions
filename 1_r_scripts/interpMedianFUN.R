require(magrittr)
require(dplyr)       
require(tigris)
require(acs)
require(stringr)
require(tidyr)          
require(acs) 

interpMedian <- function(state, county, tracts, endyear = 2014, span = 5,
                         tableNumber = "B19001", round = TRUE){
        
        # Load the aggregated ACS data
        acs <- geo.make(state = state,
                 county = county,
                 tract = tracts,
                 combine = T) %>% 
                acs.fetch(endyear = endyear, span = span,
                          geography = .,
                          table.number = tableNumber,
                          col.names = "pretty")
        
        # It all gets pretty ugly from here on out...
        rnames <- acs@estimate %>% t()  %>% as.data.frame() %>% rownames() %>% as.data.frame()
        
        medIncEst <- 
                acs@estimate %>% 
                t() %>% 
                as.data.frame() %>% 
                bind_cols(rnames) %>% 
                gather(geo,count,-.)
        
        colnames(medIncEst) <- c("Range","Geo","Count")
        
        medIncEst <- 
                medIncEst %>%
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
                                           200000,
                                           ifelse(str_length(RangeStr) < 6,
                                                  RangeNum,
                                                  str_extract(RangeStr,paste0("\\d{",RangeStrLen,"}$"))))) %>% 
                select(RangeDesc = Range,
                       RangeLower,
                       RangeUpper,
                       Count) %>% 
                mutate(RangeLower = as.numeric(RangeLower),
                       RangeUpper = as.numeric(RangeUpper),
                       CumSum = cumsum(Count)) %>% 
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
                        
                        if(round == TRUE){
                                medianEst <- as.vector(medianEst) %>% round(digits = -1)
                        }
                        else{
                                medianEst <- as.vector(medianEst)
                        }
                        
                        data.frame(.,medianEst)
                }) %>% 
                summarise(first(medianEst))
        
        medIncEst
        
        
        
}


# An example

test_tracts <- c("007900", "008100", "008400", "008100", "008400", "007900", "007900", "008800",
  "007500", "008800", "008500", "008500", "008600", "008700", "008800", "008500",
  "009000", "009100", "009100", "009200", "009200", "009300", "009300", "009300",
  "009400", "008200", "008200", "008200", "008300", "008300", "009400", "009400",
  "009400", "008100", "007500", "007500", "007500", "007500", "008600", "009000",
  "008600", "007900", "008400", "007900", "009400", "008700", "008700")

interpMedian(state = "WA", county = "King",tracts = test_tracts) %>% print()

# Notes
# - As-is, this function only works at the tract level. You could change that by 
#   adding a "block.group" argument to 'geo.make' operation.
# - The default table is B19001 (Median Household Income), and I don't know if this script will work
#   for other types of median income.