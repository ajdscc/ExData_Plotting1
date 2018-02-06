# Group of generic functions usefull in assignment
#
library(utils)
library(stringr)

# Download and unzip a file into current working directory
#
unzipFile <- function(fURL = char(0)) {
  temp <- tempfile()
  download.file(fURL,temp)
  unzip(temp)
  unlink(temp)
}

# Create a file that is a subset of another file based on lines that the "pattern" is present
#
subsetFile <- function(fileIn = char(0), fileOut = "chopped.txt", pattern = "", header = TRUE) {
  file.in <- file(fileIn, "rt")
  file.out <- file(fileOut, "wt")
  line <- readLines(file.in, n = 1, warn = FALSE)
  if (header) 
    cat(line, file = file.out, fill = TRUE)
  while (length(line)) {
    if (str_detect(line, pattern))
      cat(line, file = file.out, fill = TRUE)
    line <- readLines(file.in, n = 1, warn = FALSE)
  }
  close(file.in)
  close(file.out)
}


# define FileURL
sourcefileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

# download and unzip file (if not exist)
if (!file.exists("household_power_consumption.txt")) {
  
  unzipFile(sourcefileUrl)
  
  # create subset file with only dates under analysis interest
  pattern <- "^1/2/2007|^2/2/2007"
  
  subsetFile(fileIn = "household_power_consumption.txt", fileOut = "subsetfile.txt", pattern = pattern, header = TRUE)
}

# build a working data frame for analysis inserting a datetime column and removing DATE and TIME columns
# remove missing value rows
# convert to numeric all columns
# 
df <- read.table("subsetfile.txt", header = TRUE, sep = ";", na.strings = "?")
df <- cbind(datetime = strptime(paste(df$Date,df$Time), "%d/%m/%Y %H:%M:%S"), df)
df <- df[, -c(2,3)]
df <- na.omit(df)


png(filename = "plot4.png", width = 480, height = 480, units = "px")

op <- par(mfrow = c(2,2), mar = c(5,5,1,0), oma = c(0, 0, 0, 1))

plot(x = df$datetime, y = df$Global_active_power, type = "l",
     xlab = "", 
     ylab = "Global Active Power")

plot(x = df$datetime, y = df$Voltage, type = "l",
     xlab = "datetime", 
     ylab = "Voltage")

plot(x = df$datetime, y = df$Sub_metering_1, type = "l",
     xlab = "", 
     ylab = "Energy sub metering")
lines(df$datetime,df$Sub_metering_2, col = "red")
lines(df$datetime,df$Sub_metering_3, col = "blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n", col = c("black", "red", "blue"), lty = 1)

plot(x = df$datetime, y = df$Global_reactive_power, type = "l",
     xlab = "datetime", 
     ylab = "Globar_reactive_power")

par(op)

dev.off()

