interpolate.controls <- function(df, indicator, start.year = 2016, end.year = 2050, 
                                 years.to.fit = c(2016, seq(2020, 2050, by = 5)), 
                                 id.col = "CityID") {
    # indicator should be the prefix used in the column names, e.g. "Pop", "HHPop"
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
    result <- cbind(geo.ids, result)
    colnames(result) <- c(id.col, years.to.fit)
    return(result)
}
