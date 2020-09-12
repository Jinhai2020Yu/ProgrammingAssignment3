best <- function(state, outcome) {
        ## Read Data 
        file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Split data according to state
        splitfile <- split(file, file$State)
        input1 <- names(splitfile)
        ## Check input state is valid
        status <- FALSE
        for (i in 1:length(input1)) {
                if (state == input1[i]) {
                       status <- TRUE
                       
                } 
                
        }
        if(!status) {
                stop ("invalid state")
        }
        ## Retrieve dataset
        data <- data.frame(splitfile[state])
        ## check input outcome is valid
        if (!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }
        ## Match outcome to column number
        input2 <- if(outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        ## Select data
        select_data <- data[, c(input2, 2)]
        ## Arrange data with order
        order_data <- select_data[suppressWarnings(order(as.numeric(select_data[, 1]), select_data[, 2], na.last = NA)), ]
        ## Get result
        order_data[1, 2]
}