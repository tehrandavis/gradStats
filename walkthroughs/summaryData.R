function (data = NULL, measurevar, betweenvars = NULL, withinvars = NULL, 
          idvar = NULL, na.rm = TRUE, conf.interval = 0.95) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- sapply(data[, c(betweenvars, withinvars), drop = FALSE], 
                       FUN = is.factor)
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ", 
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE2(data, measurevar, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval)
  
  # Drop all the unused columns (these will be calculated with normed data)
  #datac$sd <- NULL
  #datac$se <- NULL
  #datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin2(data, idvar, measurevar, betweenvars, na.rm)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "Normed", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE2(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                       na.rm=na.rm, conf.interval=conf.interval)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN= function(x) length(levels(x)),
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- unlist(ndatac$sd) * correctionFactor
  ndatac$se <- unlist(ndatac$se) * correctionFactor
  ndatac$ci <- unlist(ndatac$ci) * correctionFactor
  
  # Combine the un-normed means with the normed results
  merged <- list(datac, ndatac)
  names(merged) <- c("Actual", "Corrected")
  #merged[, 1] <- as.numeric(as.character(merged[, 1]))
  #merged <- merged[order(merged[, 1]), ]
  return(merged)
}
