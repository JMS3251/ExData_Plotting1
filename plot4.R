plot4 <- function(){
    
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
    
    # add a date/time column in correct format
    h_data <- mutate(h_data, 
                     newDate = as.POSIXct(strptime(paste(h_data$Date, h_data$Time),
                                                   format = "%d/%m/%Y %H:%M:%S")))
    
    date1 <- as.POSIXct(strptime("1/2/2007 00:00:00", format="%d/%m/%Y %H:%M:%S"))
    date2 <- as.POSIXct(strptime("3/2/2007 00:00:00", format="%d/%m/%Y %H:%M:%S"))
    
    # select the data in the specified date range
    h_data_range <- subset(h_data, 
                           (newDate >= date1 & newDate < date2), 
                           select= -Date)
    
    # set up the output parameters and make the plots
    png_out_file <- paste(getwd(), "/plot4.png", sep="")
       
    png(png_out_file, width=480, height=480)
    
    par(mfrow = c(2,2))
    
    plot(h_data_range$newDate, h_data_range$Global_active_power, 
         xlab = "",
         ylab = "Global Active Power (kilowatts)", 
         type = "l")
    
    plot(h_data_range$newDate, h_data_range$Voltage, 
         xlab="datetime", ylab="Voltage", type="l")
    
    plot(h_data_range$newDate, h_data_range$Sub_metering_1, 
         xlab = "",
         ylab = "Energy sub metering", 
         type = "l")
    lines(h_data_range$newDate, h_data_range$Sub_metering_2, col="red")
    lines(h_data_range$newDate, h_data_range$Sub_metering_3, col="blue")
    legend("topright", 
           c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           col=c("black", "red", "blue"), 
           lty = c(1,1,1))
    
    plot(h_data_range$newDate, h_data_range$Global_reactive_power, 
         xlab="datetime", ylab="Global_reactive_power", type="l")
    
    dev.off()
    
}
    