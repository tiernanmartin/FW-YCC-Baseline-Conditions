library(dplyr)
library(tidyr)

set.seed(seed = 10)

df <- data.frame("Id" = 1:12,
                 "Group" = paste(sapply(toupper(letters[1:3]), rep, times = 4,simplify = T)),
                 "Var1" = sample(rep(c("good","bad"),times = 1000),size = 12),
                 "Var2" = sample(rep(1:10, times = 1000),size = 12)) 

print(df)

df %>% 
        group_by(Group, Var1) %>%
        summarise(Total = sum(Var2)) %>%
        spread(Var1,Total) %>% 
        filter(good>bad) %>% 
        print()
