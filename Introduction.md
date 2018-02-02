---
title: "Introduction to farsdata Package"
author: "TK"
date: "`r Sys.Date()`"
output: rmarkdown::md_document
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

This is a very simple package to explore 2013-2015 data from the National Highway Traffic Safety Administration (NHTSA) Fatality Analysis Reporting System (FARS).  This package was written for the Week 4 final assignment for the "Building R Packages" course on Coursera, as part of the Johns Hopkins University "Mastering Software Development in R" specialization.

## The Data

The data in this package come from the National Highway Traffic Safety Administration (NHTSA) Fatality Analysis Reporting System (FARS) data.  It lists the fatal vehicle crashes in the United States for each year; each crash observation can has as many as 50 features.
For detailed information about the data, see the [NHTSA FARS Manuals & Documentation page](https://crashstats.nhtsa.dot.gov/#/DocumentTypeList/4). 

With this package, the available years of data are: 2013-2015.  

## Loading FARS Data

If you wish to load all of the data for a given year, use the `make_filename()` and `fars_read()` functions, as shown in the previous section.  Pulling multiple years at a time drastically cuts down on most interesting information.

```{r read the file }
fars_read <- function(filename) {
   if(!file.exists(filename))
     stop("file '", filename, "' does not exist")
   data <- suppressMessages({
     readr::read_csv(filename, progress = FALSE)
   })
   dplyr::tbl_df(data)
 }

make_filename <- function(year) {
   year <- as.integer(year)
   sprintf("accident_%d.csv.bz2", year)
 }

```

```{r, results='hide'}
setwd("~/COURSERA/Mastering Software Development in R/Building R packages/Farsdata")

fars_2013 <- fars_read("./data/accident_2013.csv.bz2")
fars_2014 <- fars_read("./data/accident_2014.csv.bz2")
fars_2015 <- fars_read("./data/accident_2015.csv.bz2")
```
### About the Filename

If you wish, you can add more data to the package.  You will need to find where the package data is stored on your machine.  You can use the `make_filename` command to track this down:

```{r }
fars_2013_fn <- make_filename(2013)
fars_2014_fn <- make_filename(2014)
fars_2015_fn <- make_filename(2015)
```

An example of the head of the 2013 data file is shown below:
```{r}
dim(fars_2013)
fars_2013
```

The danger with adding data in this way is that if you reinstall the package it may overwrite any new data you bring in.

### Single Year

If you wish to just look at fatality data for a a single year, use the `fars_read_years()` function with a single year as input. The only data columns selected are `MONTH` and `year`.  This returns a list of length one, and the first element in the list is the `tbl_df` (the `tidyverse` data frame) listing the month and year for each fatal accident.  By itself, this data is relatively meaningless unless you want to count number of fatalities by month.

```{r single_year}

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
```


### Multiple Years

If you wish to look at fatalities for multiple years, enter a vector of years as the argument for the `fars_read_years()` function (examples: `fars_read_years(years = c(2013, 2015))` or `fars_read_years(2013:2015)`.  Again, this returns a list of `tbl_df`s,  with each element of the list showing the month and year for each fatality. 


## Summarizing FARS Data

The `fars_summarize_years()` function take the same argument as the `fars_read_years()`, and produces a summary of the simple counts of fatalities by month and year: 

```{r summarize_data}

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

```


## Mapping Fatal Crashes

Finally, the `fars_map_state` function takes a state ID number and a year, and maps that state's fatalities with a dot at the fatality location.  Note that in order to use this function, you will likely need to load the `mapdata` package.  

```{r mapping_crashes, warning =FALSE, message=FALSE}
library(mapdata)

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

```

```{r, echo=TRUE}
setwd("~/COURSERA/Mastering Software Development in R/Building R packages/Farsdata/data")
fars_map_state(12, 2013)
![](https://github.com/xetaro/Farsdata-Project/blob/master/plot_1.png)
fars_map_state(12, 2015)
![](https://github.com/xetaro/Farsdata-Project/blob/master/plot_2.png)
```

