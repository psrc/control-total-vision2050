library(data.table)

interpolate.controls <- function(df, indicator, start.year = 2016, end.year = 2050, 
                                 years.to.fit = c(2016, seq(2020, 2050, by = 5)), 
                                 id.col = "CityID", totals = NULL) {
  
# interpolate.controls <- function(df, indicator, start.year = 2016, end.year = 2050, 
#                                  years.to.fit = c(2016, seq(2020, 2050, by = 5)), 
#                                  id.col = "CityID", totals = NULL, round.to.int = TRUE) {
  
    # The function interpolates for each geography between the start year and the end year.
    # "indicator" should be the prefix used in the column names, e.g. "Pop", "HHPop".
    # The data frame "df" should have columns composed of the indicator and start year,
    # as well as the indicator and the end year, e.g. HHPop2016 and HHPop2050.
    # The "id.col" determines which geography is used for the interpolation.
    # Argument "totals" is a named vector of regional control totals to be used for adjustments. 
    # Its names are the years. If NULL, no adjustments are made.
    # 
    start.col <- paste0(indicator, start.year)
    end.col <- paste0(indicator, end.year)
    geo.ids <- unique(df[[id.col]])
    result <- NULL
    for (id in geo.ids) {
        idx <- which(df[[id.col]] == id)
        start.value <- sum(df[[start.col]][idx])
        end.value <- sum(df[[end.col]][idx])
        fit <- approx(c(start.year, end.year), c(start.value, end.value), xout = years.to.fit)
        result <- rbind(result, fit$y)
    }
    if(!is.null(totals)) { # adjust to totals
        difs <- result[, 2:ncol(result)] - result[, 1:(ncol(result)-1)]
        for(i in 2:(length(years.to.fit) - 1)) {
            tot.dif <- totals[as.character(years.to.fit[i])] - sum(result[,i])
            if(tot.dif == 0) next
            shares <- difs[, i-1]/sum(difs[, i-1])
            result[,i] <- pmax(0, result[,i] + shares * tot.dif)
        }
    }
    if(round.interpolated)
      result <- round(result)
    result <- cbind(geo.ids, result)
    colnames(result) <- c(id.col, years.to.fit)
    return(result)
    
    # if(round.to.int)
    #   result <- round(result)
    # result <- cbind(geo.ids, result)
    # colnames(result) <- c(id.col, years.to.fit)
    # return(result)
}

unroll <- function(ct, indicator, totals = NULL, new.id.col = "city_id") {
    wide <- data.table(ct)
    setnames(wide, "2016", "2017") # rename column 2016 to 2017 
    long <- melt(wide, id.vars = colnames(wide)[1], variable.name = "year")
    colnames(long)[1] <- new.id.col
    long[, value := round(value)] # round
    if(!is.null(totals)) {
      # match to given totals (differences caused by rounding) 
      difs <- merge(long[, .(ct = sum(value)), by = year], 
                    data.table(year = names(totals), should_be = totals), by = "year")
      difs[, dif := should_be - ct]
      # distribute the difs proportianlly
      for (y in difs$year) {
        d <- difs[year == y, dif]
        slng <- long[year == y]
        sampl.jurs <- sample(slng[[new.id.col]], abs(d), prob = slng$value)
        idx <- which(long$year == y & long[[new.id.col]] %in% sampl.jurs)
        long[idx, value := value + sign(d)] # subtracts if d is negative, otherwise adds
      }
    }
    setnames(long, "value", paste0("total_", tolower(indicator)))
    return(long)
}
