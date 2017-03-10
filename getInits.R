getInits = function() { 

scale = 1.5
SDscale = 4

result = list()

result[["intercept"]] = sign(startingValues[["intercept" ]]) *
    runif(length(startingValues[["intercept" ]]),
       abs(startingValues[["intercept"]])/scale,
       scale * abs(startingValues[["intercept"]]))

result[["betaID"]] = sign(startingValues[["betaID" ]]) *
    runif(length(startingValues[["betaID" ]]),
       abs(startingValues[["betaID"]])/scale,
       scale * abs(startingValues[["betaID"]]))

result[["betaobservations"]] = sign(startingValues[["betaobservations" ]]) *
    runif(length(startingValues[["betaobservations" ]]),
       abs(startingValues[["betaobservations"]])/scale,
       scale * abs(startingValues[["betaobservations"]]))


result[["SDID"]] = sqrt(runif(1,
       startingValues$vars[["ID"]]/scale,
       startingValues$vars[["ID"]]*scale))

result[["RID"]] = rnorm(length(startingValues[["RID"]]),
        startingValues[["RID"]], startingValues$vars[["ID"]]/SDscale)


return(result)

}