##  Mastering Spark with R - Streaming  ##
##########################################
########Chapter 12#########################

install.packages("sparklyr")

library(sparklyr)

#spark_install("2.3")

# spark_installed_versions()

install.packages("dplyr")
library(dplyr)
install.packages("future")
library(future)
install.packages("shiny")
library(shiny)
install.packages("htmltools")
library(htmltools)


sc <- spark_connect(master = "local", version = "2.3.3")

#Creating the input stream
dir.create("source")

#Creating the stream process
stream <- stream_read_text(sc, "source/") %>%
  stream_write_text("destination/")

future::future(stream_generate_test(interval = 0.5))
stream_view(stream)

stream_stop(stream)

######################################
## Analysis #
######################################
install.packages("DBI")
library(DBI)
install.packages("later")
library(later)
library(dplyr)

stream_read_csv(sc, "source") %>%
  filter(x > 700) %>%
  mutate(y = round(x / 100))

# Aggregations over the entire stream with count
stream_read_csv(sc, "source") %>%
  filter(x > 700) %>%
  mutate(y = round(x / 100)) %>%
  count(y)

# Grouped aggregations with timestamp/watermark
stream_read_csv(sc, "source") %>%
  stream_watermark()
  
# Grouped aggregations with timestamp/watermark
stream_read_csv(sc, "source") %>%
  stream_watermark() %>%
  group_by(timestamp) %>%
  summarize(
    max_x = max(x, na.rm = TRUE),
    min_x = min(x, na.rm = TRUE),
    count= n()
  )

######################################
## Modeling using ft_bucketizer()
######################################
stream_read_csv(sc, "source") %>%
  mutate(x = as.numeric(x)) %>%
  ft_bucketizer("x", "buckets", splits = 0:10 * 100) %>%
  count(buckets) %>%
  arrange(buckets)
  
#####################################
## Scoring streams with Pipelines
#####################################
## Creating the model for scoring

cars <- copy_to(sc, mtcars)

model <- ml_pipeline(sc) %>%
  ft_binarizer("mpg", "over_30", 30) %>%
  ft_r_formula(over_30 ~ wt) %>%
  ml_logistic_regression() %>%
  ml_fit(cars)
  
## Stream using stream_generate & score using ml_transform

future::future(stream_generate_test(mtcars, "cars-stream", iterations = 5))

ml_transform(model, stream_read_csv(sc, "cars-stream"))
  
######
## Review of above: copied data in, feature engineering, trained a model
## and scored the model over a real time dataset
########################################################################

########################################
## Arbitrary R
########################################
stream_read_csv(sc, "cars-stream") %>%
  select(mpg) %>%
  spark_apply(~ round(.x), mpg = "integer") %>%
  stream_write_csv("cars-round")
  
 ## Look at output:
spark_read_csv(sc, "cars-round")
  
 spark_disconnect(sc) 
  
##########################################
##  Kafka
##########################################
 library(sparklyr)
 library(dplyr)

 spark_installed_versions()  # confirms which versions you have installed 
 
 # Connecting to the local cluster and Kafka (which you have previously installed- App A)
 sc <- spark_connect(master = "local", version = "2.3.3", config = list(
  sparklyr.shell.packages = "org.apache.spark:spark-sql-kafka-0-10_2.11:2.3.1"))


 # Reading data from a stream
 stream_read_kafka(
   sc, 
   options = list(
     kafka.bootstrap.server = "localhost:9092",
     subcribe = "test"))
 #######################################
 ## rkafka ##
 ######################################
 install.packages("rkafka")
 library(rkafka)
 
 producer1 = rkafka.createProducer("172.31.16.0:9092") # put your host name in the ()
 
 rkafka.send(producer1, "testKafka", "172.31.16.0:9092", "TestingKafka") #creating a topic
 
 consumer1 = rkafka.createConsumer("172.31.16.0:9092", "testKafka") #reading a topic
 
 rkafka.closeProducer(producer1) # always close the producer when you are done

#######################################
## Kafka Letters
#######################################
 spark_install("2.4.4")
 
hosts <- "localhost:9092"
 
 #hosts <- "172.31.16.0:9092"
 
 read_options <- list(kafka.bootstrap.servers = hosts, subscribe = "letters")
 write_options <- list(kafka.bootstrap.servers = hosts, subscribe = "test")
 
 stream_read_kafka(sc, options = read_options) %>%
   mutate(value = as.character(value)) %>%
   count(value) %>%
   mutate(value = paste0(value, "=", n)) %>%
   stream_write_kafka(options = write_options, mode = "update")
 
 stream_read_kafka(sc, options = totals_options)
                      
 #################################################
 ## Shiny
 #################################################
 library(sparklyr)
 library(shiny)
 
 
 unlink("shiny-stream", recursive = TRUE)
 dir.create("shiny-stream", showWarnings = FALSE)
 
 sc <- spark_connect(
   master = "local", version = "2.3.3",
   config = list(sparklyr.sanitize.column.names = FALSE))
 
 ui = pageWithSidebar(
   headerPanel('Iris k-means clustering from Spark Stream'),
   sidebarPanel(
     selectInput('xcol', 'X Variable', names(iris)),
     selectInput('ycol', 'Y Variable', names(iris),
                 selected = names(iris)[[2]]),
     numericInput('clusters', 'Cluster count',3,
                  min = 1, max =9)
 ),
 mainPanel(plotOutput('plot1'))
)
 

server <- function(input, output, session) {
  iris <- stream_read_csv(sc, "shiny-stream",
                          columns = sapply(datasets::iris, class)) %>%
    reactiveSpark()
  
  selectedData <- reactive(iris()[, c(input$xcol, input$ycol)])
  clusters <- reactive(kmeans(selectedData(), input$clusters))
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(), col = clusters()$cluster, pch = 20, cex =3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
   })
  }
  

shinyApp(ui, server)

# Run in a separate RStudio instance##
shiny::runApp("shiny/shiny-stream.R")

  
 
 
 
#######################################
#  Logs  #  Showing logs and filtering
######################################
spark_log(sc)

spark_log(sc, filter = "sparklyr")

######################################
#  Disconnecting from Spark#
######################################

spark_disconnect(sc)

spark_disconnect_all()

# To upgrade sparklyr and install the latest version
devtools::install_github("rstudio/sparklyr")


