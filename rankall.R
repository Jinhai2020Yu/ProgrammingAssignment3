rankall <- function(outcome, num = "best") {
        ## Read Data 
        file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        
        ## check input outcome is valid
        if (!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }
        ## Match outcome to column number
        input1 <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        ## Select data and split according to State
        
        select_data <- file[, c(input1, 2, 7)]
        select_data[, 1] <- suppressWarnings(as.numeric(select_data[, 1]))
        select_data1 <- select_data[!is.na(select_data[, 1]),]
        splitfile <- split(select_data1, select_data1$State)
        ## Arrange data with order
        order_data <- lapply(splitfile, function(x, num) {
                x = x[order(x[, 1], x$Hospital.Name), ]
                if (class(num) == "character") {
                        if (num == "best") {
                                return (x$Hospital.Name[1])
                        } else if (num == "worst") {
                                return (x$Hospital.Name[nrow(x)])
                        }
                } else {
                        return (x$Hospital.Name[num])
                }
        }, num)
        ## Return data frame as requested
        
        return (data.frame(hospital = unlist(order_data), state = names(order_data)))
}