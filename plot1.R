plot1 <- function(){
    
    library(data.table)  
    library(dplyr)
    
    file_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    
    # create a temporary directory
    td = tempdir()
    
    # create the placeholder file
    tf = tempfile(tmpdir=td, fileext=".zip")
    
    # download into the placeholder file
    download.file(file_url, tf)
    
    # get the name of the first file in the zip archive
    fname = unzip(tf, list=TRUE)$Name[1]
    
    # unzip the file to the temporary directory
    unzip(tf, files=fname, exdir=td, overwrite=TRUE)
    
    # fpath is the full path to the extracted file
    fpath = file.path(td, fname)
    
    # read the file and delete the temporary directory
    h_data <- fread(fpath, na.strings = c("?"))
    
    unlink(td)
    
    # add a date column in correct format
    h_data <- mutate(h_data, 
                     newDate = as.POSIXct(strptime(h_data$Date, format = "%d/%m/%Y")))
    
    date1 <- as.POSIXct(strptime("1/2/2007", format="%d/%m/%Y"))
    date2 <- as.POSIXct(strptime("2/2/2007", format="%d/%m/%Y"))
  
    # select the data in the specified date range  
    h_data_range <- subset(h_data, 
                           (newDate >= date1 & newDate <= date2), 
                           select= -Date)

    # set up the output parameters and make the plot    
    png_out_file <- paste(getwd(), "/plot1.png", sep="")
       
    png(png_out_file, width=480, height=480)
    
    hist(h_data_range$Global_active_power, 
         main = "Global Active Power", 
         xlab = "Global Active Power (kilowatts)", 
         col = "red")
    
    dev.off()
    
}
    