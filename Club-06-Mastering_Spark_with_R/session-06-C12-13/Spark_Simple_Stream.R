##  Mastering Spark with R - Streaming  ##
##########################################
########Chapter 2#########################

install.packages("sparklyr")

library(sparklyr)

spark_install("2.3")

spark_installed_versions()

sc <- spark_connect(master = "local", version = "2.3.3")

cars <- copy_to(sc, mtcars)
cars
#####################################
#  Streaming One# -Static Data Sample
#####################################
library(dplyr)
#Creating the input stream
dir.create("input")
write.csv(mtcars, "input/cars_1.csv", row.names = F)

#Creating the stream process
stream <- stream_read_csv(sc, "input/") %>%
  select(mpg, cyl, disp) %>%
  stream_write_csv("output/")

#Creating and looking at the output steam
dir("output", pattern = ".csv")

###########################################
#  Streaming Two# -Dynamic Data Sample
#  Writing more data into the stream source
###########################################

write.csv(mtcars, "input/cars_2.csv", row.names = F)

#Checking the new output steam
dir("output", pattern = ".csv")

###########################################
#  Stopping the stream
###########################################

stream_stop(stream)

######################################
#  Logs  #  Showing logs and filtering
######################################
spark_log(sc)

spark_log(sc, filter = "sparklyr")

######################################
#  Disconnecting from Spark#
######################################

spark_disconnect(sc)

spark_disconnect_all()





