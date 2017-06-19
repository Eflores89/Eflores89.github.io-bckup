# download data
getComtradeData <- function(months = c("201601", "201701")){
  
  mm <- paste(months, collapse = "%2C")
  s <- paste0("http://comtrade.un.org/api/get?type=C&freq=M&px=HS&ps=", 
              mm, "&r=484%2C842%2C124&p=484%2C842%2C124&rg=all&cc=AG4") 
  print(s)
  
  k <- rjson::fromJSON(file = s)
  data <- k$dataset
  
  if(length(data)> 0) {
    var.names<- names(data[[1]])
    data<- as.data.frame(t( sapply(data,rbind)))
    ndata<- NULL
    for(i in 1:ncol(data)){
      data[sapply(data[,i],is.null),i]<- NA
      ndata<- cbind(ndata, unlist(data[,i]))
    }
    ndata<- as.data.frame(ndata)
    colnames(ndata)<- var.names
  } 
  
  exp <- list("data" = ndata, 
              "valid" = k$validation) 
  exp
}

# theme 
hc_theme_eem <- hc_theme(
  colors = c('#A84A44','#E47D04','#D8A19E','#ae8b38','#4d7c28','#38b6a6','#2080c7','#94127a','#155685', 
             '#157d85','#731585','#848515','#d06347','#d0ca47','#d04785','#a19c9b','#b5bcbf','#62686b',
             '#021118', '#daf3ff'),
  chart = list(
    backgroundColor = "#ffffff"
  ),
  title = list(
    style = list(
      color = '#333333',
      fontFamily = "Lato"
    )
  ),
  subtitle = list(
    style = list(
      color = '#666666',
      fontFamily = "Shadows Into Light"
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = 'Tangerine',
      color = 'black'
    ),
    itemHoverStyle = list(
      color = 'gray'
    )   
  )
)


# Function for the export and import matrixes

summarise_df <- function(data, name2, name4, from, to, type = "exports", threshold = 5000){
  
  data <- data %>% mutate_if(is.factor, as.character)
  data <- data %>% mutate(cmdCode = as.character(cmdCode))
  
  # I get both dates programatically
  newdate <- max(as.numeric(as.character(unique(data$period))))
  olddate <- min(as.numeric(as.character(unique(data$period))))
  
  # i'm going to filter the data according to partners
  filtered_df <- data %>% 
    dplyr::filter(rtCode == switch(from, mx = "484", usa = "842", can = "124")) %>% 
    dplyr::filter(ptCode == switch(to, mx = "484", usa = "842", can = "124")) %>% 
    dplyr::filter(rgCode == switch(type, exports = "2", import = "1", reimport = "4", reexport = "3")) %>%
    dplyr::mutate("TradeValue" = as.numeric(as.character(TradeValue))) %>%
    dplyr::mutate("rgCode" = as.character(rgCode))
  
  # print(nrow(filtered_df))
  
  # i'm going to see which codes have fewer than "threshold" in movements
  other_cats <- filtered_df %>%
    dplyr::mutate("period" = as.character(period)) %>%
    dplyr::filter(period == as.character(newdate)) %>%
    group_by(cmdCode) %>% 
    summarise("v" = sum(as.numeric(TradeValue))) %>%
    ungroup() %>%
    mutate("NEWcmdCode" = ifelse(v>threshold, cmdCode, "SMALL"))
  
 # print(nrow(other_cats))
  
  # now, i add these two and get a new column to group
  filtered_df <- filtered_df %>%
    left_join(., other_cats, by = "cmdCode") %>%
    mutate("uppergrp" = substr(NEWcmdCode, 1,2))
  
  # print(nrow(filtered_df))
  
  df <- filtered_df %>% 
    ungroup() %>%
    dplyr::mutate("period" = as.character(period)) %>%
    dplyr::filter(period == as.character(olddate)) %>% 
    dplyr::group_by(uppergrp, NEWcmdCode) %>% 
    dplyr::summarise("old" = sum(as.numeric(TradeValue))) %>%
    ungroup() %>%
    left_join(., filtered_df %>%
                dplyr::mutate("period" = as.character(period)) %>%
                dplyr::filter(period == as.character(newdate)) %>% 
                group_by(uppergrp, NEWcmdCode) %>% 
                summarise("new" = sum(as.numeric(TradeValue))) %>%
                ungroup(), by = c("uppergrp", "NEWcmdCode")) %>%
    mutate("new" = ifelse(is.na(new), 0, new)) %>%
    mutate("change" = (new/old-1)*100) %>%
    mutate("change" = ifelse(change>100,100, change)) %>% 
    mutate("new" = round(new/1000000, 1)) %>% 
    mutate("change" = round(change, 2))
  
  df <- df[!is.na(df$NEWcmdCode),]
  
  # print(nrow(df))
  
  df <- df %>% 
    left_join(., name2, by = c("uppergrp" = "l2")) %>% 
    left_join(., name4, by = c("NEWcmdCode" = "l4")) %>% 
    mutate("l4_name" = ifelse(is.na(l4_name), 
                              paste0("NA (", NEWcmdCode, ")"), 
                              l4_name))
  
  
  tm <- treemap(df, index = c("l2_name"),
                vSize = "new", 
                vColor = "change", 
                palette = "RdYlGn",
                type = "value", 
                fun.aggregate = "weighted.mean", 
                mapping=c(-50, 0, 50), range=c(-100, 100))
  
  hctreemap(tm, allowDrillToNode = TRUE,
            layoutAlgorithm = "squarified",
            name = "tmdata") %>% 
    hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
               Value: {point.value:,.1f} (Millions)<br>
               Growth Rate: {point.valuecolor:,.1f}%") %>%
    hc_legend(enabled = TRUE)
}


# function for share-shift
summarise_mkt <- function(data, name2, name4, mkt, pivot, threshold = 1000000){
  
  data <- data %>% mutate_if(is.factor, as.character)
  
  newdate <- max(as.numeric(as.character(unique(data$period))))
  olddate <- min(as.numeric(as.character(unique(data$period))))
  
  reporters <- as.character(unique(unique(data$rtCode)))
  competitors <- reporters[reporters != switch(mkt, mx = "484", usa = "842", can = "124")]
  pvt <- switch(pivot, mx = "484", usa = "842", can = "124")
  other_comp <- competitors[competitors != pvt]
  
  market <- data %>%
    dplyr::filter(rgCode == 1) %>% 
    dplyr::mutate("period" = as.character(period)) %>%
    dplyr::filter(period == as.character(newdate)) %>%
    dplyr::filter(rtCode == switch(mkt, mx = "484", usa = "842", can = "124")) %>% 
    group_by(cmdCode) %>% 
    summarise("Market" = sum(as.numeric(TradeValue))) %>% 
    left_join(., data %>%
                dplyr::filter(rgCode == 1) %>% 
                dplyr::mutate("period" = as.character(period)) %>%
                dplyr::filter(period == as.character(newdate)) %>%
                dplyr::filter(rtCode == switch(mkt, mx = "484", usa = "842", can = "124")) %>%
                dplyr::filter(ptCode == pvt) %>% 
                group_by(cmdCode) %>% 
                summarise("Pivot" = sum(as.numeric(TradeValue)))
    ) %>%
    mutate("Pivot" = ifelse(is.na(Pivot), 0, Pivot)) %>%
    mutate("ShrPivotNew" = Pivot/Market*100) %>% 
    left_join(., 
              data %>%
                dplyr::filter(rgCode == 1) %>% 
                dplyr::mutate("period" = as.character(period)) %>%
                dplyr::filter(period == as.character(olddate)) %>%
                dplyr::filter(rtCode == switch(mkt, mx = "484", usa = "842", can = "124")) %>% 
                group_by(cmdCode) %>% 
                summarise("MarketOld" = sum(as.numeric(TradeValue)))
    ) %>%
    left_join(., data %>%
                dplyr::filter(rgCode == 1) %>% 
                dplyr::mutate("period" = as.character(period)) %>%
                dplyr::filter(period == as.character(olddate)) %>%
                dplyr::filter(rtCode == switch(mkt, mx = "484", usa = "842", can = "124")) %>%
                dplyr::filter(ptCode == pvt) %>% 
                group_by(cmdCode) %>% 
                summarise("PivotOld" = sum(as.numeric(TradeValue)))
    ) %>%
    mutate("PivotOld" = ifelse(is.na(PivotOld), 0, PivotOld)) %>%
    mutate("ShrPivotOld" = PivotOld/MarketOld*100) %>%
    mutate("Shift" = ShrPivotNew-ShrPivotOld)
  
  market_comp <- market[complete.cases(market), ]
  
  
  market_comp <- market_comp %>%
    mutate("uppergrp" = substr(cmdCode, 1,2)) %>%
    mutate("NewCmdCode" = ifelse(Market<threshold, "SMALL", cmdCode)) %>%
    mutate("Market" = Market/1000000)
  
  market_comp <- market_comp %>% 
    left_join(., name2, by = c("uppergrp" = "l2")) %>% 
    left_join(., name4, by = c("NewCmdCode" = "l4")) %>% 
    mutate("l4_name" = ifelse(is.na(l4_name), 
                              paste0("NA (", NewCmdCode, ")"), 
                              l4_name))
  
  tm <- treemap(market_comp, index = c("l2_name"),
                vSize = "Market", 
                vColor = "Shift", 
                palette = "RdYlGn",
                type = "value", 
                fun.aggregate = "weighted.mean", mapping = c(-20,0,20))
  
  hctreemap(tm, allowDrillToNode = TRUE,
            layoutAlgorithm = "squarified",
            name = "tmdata") %>% 
    hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
               Value: {point.value:,.1f} (Millions)<br>
               Share Shift (PP): {point.valuecolor:,.1f}") %>%
    hc_legend(enabled = TRUE)
  
}