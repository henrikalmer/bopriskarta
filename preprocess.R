library(dplyr)
library(magrittr)


setwd("/Users/Henrik/Repositories/bopriskarta/")


normalize.sold.df <- function(df) {
	##  Set column names
	colnames(df) <- c("indexAdjustedSoldPrice", "soldPrice", "soldDate", "livingArea",
		"additionalArea", "plotArea", "rooms", "floor", "rent", "rentPerSQM", "operatingCost",
		"operatingCostPerSQM", "objectType", "latitude", "longitude")

	##  Replace "None" with NA
	df[df == "None"] <- NA
	
	##  Cast columns
	cast.cols <- which(colnames(df) %in% c("livingArea", "additionalArea", "plotArea", "rooms",
		"floor", "rent", "rentPerSQM", "operatingCost", "operatingCostPerSQM"))
	df[, cast.cols] %<>% lapply(function(x) as.numeric(x))
	df$soldDate <- as.POSIXct(df$soldDate)
	df$objectType <- as.factor(df$objectType)

	##  Filter "bad" datapoints
	df <- df[!is.na(df$soldPrice), ]
	df <- df[df$soldPrice > 100000, ]
	df <- df[!is.na(df$livingArea), ]
	df <- df[df$livingArea > 10, ]
	df <- df[df$livingArea < 600, ]
	df <- df[df$indexAdjustedSoldPrice < 100000000, ]
	df <- df[df$indexAdjustedSoldPrice > df$soldPrice, ]

	##  Calculate sqm prices
	df$soldSQMPrice <- df$soldPrice / df$livingArea
	df$indexAdjustedSQMPrice <- df$indexAdjustedSoldPrice / df$livingArea
	
	return(df)
}


##  Load data from CSV
sold.2012 <- read.csv('data/csv/index_adjusted_objects_2012.csv', stringsAsFactors=FALSE)
sold.2013 <- read.csv('data/csv/index_adjusted_objects_2013.csv', stringsAsFactors=FALSE)
sold.2014 <- read.csv('data/csv/index_adjusted_objects_2014.csv', stringsAsFactors=FALSE)
sold.2015 <- read.csv('data/csv/index_adjusted_objects_2015.csv', stringsAsFactors=FALSE)
sold.2016 <- read.csv('data/csv/index_adjusted_objects_2016.csv', stringsAsFactors=FALSE)

##  Normalize data
sold.2012 <- normalize.sold.df(sold.2012)
sold.2013 <- normalize.sold.df(sold.2013)
sold.2014 <- normalize.sold.df(sold.2014)
sold.2015 <- normalize.sold.df(sold.2015)
sold.2016 <- normalize.sold.df(sold.2016)

##  Append dataframes and write to disk
sold <- rbind(sold.2012, sold.2013, sold.2014, sold.2015, sold.2016)
save(sold, file = 'data/Rdata/sold.Rdata')

## Aggregate on median values per location (point)
sold$objectType <- as.numeric(sold$objectType)
sold.agg <- sold %>% group_by(longitude, latitude) %>% summarise_all(funs(median))
sold.agg <- as.data.frame(sold.agg)
save(sold.agg, file = 'data/Rdata/sold-aggregate.Rdata')
