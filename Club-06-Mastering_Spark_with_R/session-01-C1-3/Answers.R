################################################################################
# Questions 1
################################################################################

java8_home = system("/usr/libexec/java_home -v 1.8", intern = TRUE)
Sys.setenv(JAVA_HOME = java8_home)
system("java -version")

# Question 1
spark_install("2.3")
install.packages("sparklyr")

# Question 2
spark_installed_versions()

# Question 3
spark_available_versions()

# Question 4
library(sparklyr)
sc <- spark_connect(master = 'local', version = "2.3")

# Question 5
library(tidyverse)
df <- read_csv("zillow.csv")
zillow <- copy_to(sc, df, name = "zillow", overwrite = TRUE)

# Or a way that does not need the data to fit into memory first
zillow2 <- spark_read_csv(sc, path = 'zillow.csv', name = "zillow2")

# Question 6
spark_web(sc) # Click on the jobs tab

# Question 7
spark_web(sc) # Click on the storage tab

# Question 8
# Click in the Connections tab in RStudio

# Question 9
library(DBI)
dbGetQuery(sc, "SELECT Zip, MEAN(List_Price_) AS Mean_Price FROM zillow GROUP BY Zip")

# Question 10
library(tidyverse)
zillow %>%
  select(Year, Living_Space_sq_ft) %>%
  collect() %>%
  plot()

# Question 11
zillow %>%
  ml_linear_regression(List_Price_ ~ Living_Space_sq_ft + Beds + Baths + Year) ->
  model

# Question 12
model %>%
  ml_predict(copy_to(sc, data.frame(Living_Space_sq_ft = 2000, Beds = 3,
                                    Baths = 2, Year = 2000)))

# Question 13
spark_web(sc) # Click on the storage tab and look at the last entry
# 568 Bytes

# Question 14
library(fs)
unlink('input', recursive = TRUE)
unlink('output', recursive = TRUE)
dir.create('input')
file_copy(rep('zillow.csv', 3), paste0('./input/zillow', 1:3, '.csv'), overwrite = TRUE)
stream <- stream_read_csv(sc, "input/") %>%
  select(Zip, List_Price_) %>%
  stream_write_csv("output/")

# Question 15
spark_log(sc, filter = 'WARN')

# Question 16
summarise_all(zillow, max)

################################################################################
# Questions 2
################################################################################

# Question 1
summarise_all(zillow, max) %>%
  show_query()

# Question 2
zillow %>%
  summarise(stddev_samp(List_Price_))

# Question 3
library(dbplot)
zillow %>%
  dbplot_histogram(List_Price_, binwidth = 100000)

# Question 4
zillow %>%
  select(Zip, List_Price_) %>%
  group_by(Zip) %>%
  summarise(mean(List_Price_)) %>%
  compute("MEAN_BY_ZIP")

# Question 5
# mean_by_zip

# Question 6
spark_disconnect(sc)
