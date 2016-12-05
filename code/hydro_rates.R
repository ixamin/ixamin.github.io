library(XML)
library(RCurl)
library(data.table)
library(tidyr)
library(ggplot2)
library(scales)
library(dtplyr)
library(lubridate)

reference_url <- "http://www.ontario-hydro.com/current-rates"
reference_tables <- getNodeSet(htmlParse(reference_url,encoding = "UTF-8"), "//table")
reference_rawdata <- readHTMLTable(reference_tables[[3]],stringsAsFactors = FALSE)
reference_data <- data.table(reference_rawdata)
reference_data <- separate(reference_data,"Summer Rate (May - Oct)",into = c("Summer","SummerRate","uom1"), sep = " " )
reference_data <- separate(reference_data,"Winter Rate (Nov - Apr)",into = c("Winter","WinterRate","uom2"), sep = " " )
reference_data <- separate(reference_data,"SummerRate",into = c("desc1","SummerRate"), sep = 4,convert = TRUE)
reference_data <- separate(reference_data,"WinterRate",into = c("desc2","WinterRate"), sep = 4,convert = TRUE)
reference_data[ ,`:=`(desc1 = NULL, uom1 = NULL, desc2 = NULL, uom2 = NULL, SummerRate = NULL, WinterRate = NULL)]
reference_data[, Summer := Summer[1], by = cumsum(!is.na(Summer))]
reference_data[, Winter := Winter[1], by = cumsum(!is.na(Winter))]
reference_data[From == "Midnight", From := "12:00 AM"]
reference_data[To == "Midnight", To := "12:00 AM"]
reference_data[Summer == "mid-peak", Summer := "MidPeak"]
reference_data[Summer == "on-peak", Summer := "OnPeak"]
reference_data[Summer == "off-peak", Summer := "OffPeak"]
reference_data[Winter == "mid-peak", Winter := "MidPeak"]
reference_data[Winter == "on-peak", Winter := "OnPeak"]
reference_data[Winter == "off-peak", Winter := "OffPeak"]
reference_data <- melt.data.table(reference_data, measure.vars = c("Summer","Winter"), variable.name = "Season", value.name = "Usage", variable.factor = TRUE, value.factor = TRUE)
reference_data[,`:=`(From=hour(parse_date_time(From, orders = c("I!M!p!"))),To=hour(parse_date_time(To, orders = c("I!M!p!"))))]
reference_data[, `:=`(ymin=0)]
setkey(reference_data,Season,From,Usage)
rm(reference_url,reference_tables,reference_rawdata)

historical_url <- "http://www.ontarioenergyboard.ca/OEB/Consumers/Electricity/Electricity+Prices/Historical+Electricity+Prices/"
hist_tables <- getNodeSet(htmlParse(historical_url,encoding = "UTF-8"), "//table")
hist_rawdata <- readHTMLTable(hist_tables[[1]],stringsAsFactors = FALSE, header = TRUE)
hist_data <- as.data.table(hist_rawdata)
setnames(hist_data,c("Effective_Date", "OffPeak","MidPeak","OnPeak"))
hist_data[, `:=`(From = parse_date_time(Effective_Date,"m!d!Y!"))]
hist_data <- melt.data.table(hist_data,measure.vars = c("OnPeak","OffPeak","MidPeak"), variable.name = "Usage", value.name = "Rate", variable.factor = FALSE, value.factor = FALSE)
hist_data[, Rate := as.numeric(Rate)]
setkey(hist_data,From,Usage)
rm(historical_url, hist_tables, hist_rawdata)


CPI_Utility_url <- "http://www5.statcan.gc.ca/cansim/results/cansim7880843501630058824.csv"
CPI_Utility_file <- getURL(CPI_Utility_url)
CPI_Utility_data <- fread(CPI_Utility_file, header = TRUE)
setnames(CPI_Utility_data, c("Year", "Territory" , "Utility", "IndexRaw"))
CPI_Utility_data[, Year := as.character(Year)]
CPI_Utility_data[, Year := substr(Year,3,4)]
setkey(CPI_Utility_data,Territory,Utility)
CPI_Utility_data[CPI_Utility_data[Year == "07"], Index := (IndexRaw/i.IndexRaw) * 100]
setkey(CPI_Utility_data,Utility,Year)
CPI_Utility_data[CPI_Utility_data[Territory == "Canada"], PctVarAvgCA := ((IndexRaw - i.IndexRaw)/i.IndexRaw) * 100]

# CANSIM Table 127-0005 Annual Cost of fuel consumed for electric power generation, by electric utility thermal plants (dollars x 1,000)
fuel_cost_url <- "http://www5.statcan.gc.ca/cansim/results/cansim6075873020500324624.csv"
fuel_cost_file <- getURL(fuel_cost_url)
fuel_cost_data <- fread(fuel_cost_file, na.strings = c("","..","0"))
fuel_cost_data[!(FUEL == "Uranium"), FUEL := "Hydrocarbon"]
fuel_cost_data <- fuel_cost_data[, sum(Value), by=c("Ref_Date","GEO","FUEL")]
setnames(fuel_cost_data, c("Year", "Territory" , "Fuel", "Dollars"))
fuel_cost_data[, Year := as.character(Year)]
fuel_cost_data[, Year := substr(Year,3,4)]
fuel_cost_data <- fuel_cost_data[Year == "14" & Territory == "Ontario"]
fuel_cost_data[, AnnualPCT := round((Dollars/sum(Dollars)) * 100,2), by = c("Year","Territory")]
setkey(fuel_cost_data,Year,Territory,Fuel,Dollars)

# CANSIM Table 127-0009 Annual capacity in kilowatts
generating_class_url <- "http://www5.statcan.gc.ca/cansim/results/cansim7420856805836222537.csv"
generating_class_file <-getURL(generating_class_url)
generating_class_data <- fread(generating_class_file, na.strings = c("","..","0"))
generating_class_data[!(CAPACITY %like% "Nuclear|Hydraulic|Wind|Solar"), Type := "Non-nuclear Thermal"]
generating_class_data[CAPACITY %like% "Nuclear", Type := "Nuclear Thermal"]
generating_class_data[CAPACITY %like% "Hydraulic", Type := "Hydraulic"]
generating_class_data[CAPACITY %like% "Wind", Type := "Wind"]
generating_class_data[CAPACITY %like% "Solar", Type := "Solar"]
setkey(generating_class_data,Ref_Date,GEO,Type)
generating_class_data <- generating_class_data[, sum(Value), by=c("Ref_Date","GEO","Type")]
setnames(generating_class_data, c("Year", "Territory" , "Type", "Capacity"))
generating_class_data[, Year := as.character(Year)]
generating_class_data[, Year := substr(Year,3,4)]
generating_class_data <- generating_class_data[Year == "14" & Territory == "Ontario"]
generating_class_data[, AnnualPCT := round((Capacity/sum(Capacity)) * 100,2), by = c("Year","Territory")]
setkey(generating_class_data,Year,Territory,Type,Capacity)

## CHARTS ##

Electricity_TOU_grob <- ggplotGrob(ggplot(data = reference_data) +
  scale_fill_manual(values = alpha(c("#FFD300","#00A368","#C40223"))) +
  # scale_x_continuous(breaks = seq(0,24,4)) +
  scale_x_continuous(breaks = c(0,7,11,17,19,24)) +
  geom_rect(aes(xmin = From, xmax = From + 1, ymin = ymin, ymax = ymin + 1, fill=Usage)) +
  facet_grid(. ~Season) +
  labs(title = "Weekday Time of Use Periods") +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "#E6B420"),
        strip.text = element_text(colour = "black")))

Ontario_Electricity_Rates <- ggplot(data = hist_data) +
  scale_color_manual(values = alpha(c("#FFD300","#00A368","#C40223"))) +
  geom_line(aes(x = From, y = Rate, group=Usage, col=Usage), size = 1) +
  ylab(paste0("Rate (cents/kWh)")) +
  labs(title = "Ontario Electricity Rates") +
  theme(strip.background = element_rect(fill = "#E6B420"),
        panel.background = element_rect(fill = "#333333")) +
  annotation_custom(grob = Electricity_TOU_grob, ymin = 11, ymax = 18, xmin = unclass(as.POSIXct("2006-07-01")), xmax = unclass(as.POSIXct("2014-01-01")))

ON_Utilities_CPI <- ggplot() +
  scale_color_manual(values = alpha(c("#003ee2","#ff5f81","#FFD300","#b0d261"))) +
  geom_line(data = subset(CPI_Utility_data, Territory %in% c("Ontario")),
            aes(x = Year, y = Index, group = Utility, col = Utility),
            size = 2) +
  facet_grid(. ~ Utility) +
  labs(title = "ON Utilities Consumer Price Index \n(by year 2007 - 2015)", y = "CPI Utilities Ontario (2007 = 100)") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#16738F"),
        strip.text = element_text(colour = "white"),
        panel.background = element_rect(fill = "#333333"))

CA_ON_Elect_CPI <- ggplot() +
  scale_color_manual(values = alpha(c("red","#003ee2"))) +
  geom_line(data = subset(CPI_Utility_data, Territory %in% c("Ontario","Canada") & Utility == "Electricity"), aes(x = Year, y = Index, group = Territory, col = Territory), size = 2) +
  # geom_line(data = subset(CPI_Utility_data, !(Territory %in% c("Newfoundland and Labrador","Canada","Prince Edward Island","Whitehorse, Yukon","Yellowknife, Northwest Territories")) &Utility == "Electricity"), aes(x = Year, y = Index, group = Territory, col = Territory), size = 1) +
  labs(title = "Annual Electricity Consumer Price Index \n(ON & CA 2007 - 2015)", y = "CPI - Electricity (2007=100)") +
  theme(panel.background = element_rect(fill = "#333333"))

Provincial_Elec_CPI_Var <- ggplot(data = CPI_Utility_data[Year == "15" & Utility == "Electricity" & !(Territory == "Canada")]) +
  aes(Territory, PctVarAvgCA, fill=Territory) +
  geom_bar(stat='identity', position='identity') +
  geom_text(data = CPI_Utility_data[Year == "15" & Utility == "Electricity" & Territory == "Ontario"], aes(label = paste(round(PctVarAvgCA,1),"%")), vjust=2, size=3, colour="white") +
  theme(legend.position = "bottom",axis.text.x=element_blank(),axis.title.x=element_blank(),panel.background = element_rect(fill = "#333333")) +
  # scale_fill_discrete(name = NULL) +
  # scale_fill_brewer(name = NULL, type = "qual", palette = "Set1") +
  scale_fill_manual(name = NULL, values = c("#00ad5b",
                                            "#b0d261",
                                            "#c01200",
                                            "#762100",
                                            "#00858c",
                                            "#460251",
                                            "#003ee2",
                                            "#ffa5d3",
                                            "#ebbe88",
                                            "#ff5f81",
                                            "#3b2600",
                                            "#d896ff")) +
  labs(title = "Electricity CPI variation from CA Average \n(by Province in 2015)", y = "%")

ON_Generating_Types <- ggplot(data = generating_class_data) +
  aes(x=reorder(Type,-AnnualPCT),AnnualPCT, fill = Type) +
  geom_bar(stat='identity') +
  geom_text(data = generating_class_data, aes(label = paste(round(AnnualPCT,1),"%")), vjust=0.93,size=3, color="white", fontface="bold") +
  theme(legend.position = "right",axis.text.x=element_blank(),axis.title.x=element_blank(),panel.background = element_rect(fill = "#333333")) +
  scale_fill_manual(name = NULL,
                    breaks=levels(with(generating_class_data, reorder(Type,-AnnualPCT))),
                    values = c("#00ad5b",
                               "#b0d261",
                               "#c01200",
                               "#762100",
                               "#00858c")) +
  labs(title = "Electric Generation Capacity by Type \n(Ontario in 2014)", y = "% of Annual Capacity")

ON_Generating_Fuel_Costs <- ggplot(data = fuel_cost_data) +
  aes(x=reorder(Fuel,-AnnualPCT),AnnualPCT, fill = Fuel) +
  geom_bar(stat='identity') +
  geom_text(data = fuel_cost_data, aes(label = paste("$",format(Dollars * 1000, big.mark=","),sep="")), vjust=2, size=3, colour="white") +
  theme(legend.position = "right",axis.text.x=element_blank(),axis.title.x=element_blank(),panel.background = element_rect(fill = "#333333")) +
  scale_fill_manual(name = NULL,
                    breaks=levels(with(fuel_cost_data, reorder(Fuel,-AnnualPCT))),
                    values = c("#b0d261",
                               "#c01200")) +
  labs(title = "Thermal Electric Generation Fuel Cost \n(Ontario in 2014)", y = "% of Annual Cost")

# print(Ontario_Electricity_Rates)
# print(CA_ON_Elect_CPI)
# print(ON_Utilities_CPI)
# print(ON_Generating_Types)
# print(ON_Generating_Fuel_Costs)
# print(Provincial_Elec_CPI_Var)
