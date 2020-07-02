library(mlbench)
library(tidyverse)
library(randomForest)
library(caret)
library(party)

num_tree <- 1000

set.seed(200)
simulated <- mlbench::mlbench.friedman1(200, sd = 1)
cbind(simulated$x, simulated$y) %>% 
  as.data.frame() -> simulated
colnames(simulated)[ncol(simulated)] <- 'y'

# (a)
randomForest::randomForest(
    y ~ ., data = simulated,
    importance = TRUE,
    ntree = num_tree)  %>% 
  caret::varImp( . , scale = FALSE) -> rfImp

rfImp[1:5, ]
rfImp[6:10,]
# No, the features have very low importance

# (b) 

simulated_dup <- simulated
simulated_dup$duplicate1 <- simulated$V1 + rnorm(nrow(simulated)) * 0.1
with(simulated_dup, cor(V1, duplicate1))
randomForest::randomForest(
  y ~ ., data = simulated_dup,
  importance = TRUE,
  ntree = num_tree)  %>% 
  caret::varImp( . , scale = FALSE) -> rfImp_duplicate1
data.frame(V1 = rfImp[1, ], V1_duplicated1 = rfImp_duplicate1[1, ],
           combined = sum(rfImp_duplicate1[c(1, 11), ]))

# Yes, the importance of V1 changed

# (c)

# unbiased, no duplication
party::cforest(y ~ ., data = simulated,
    controls = cforest_unbiased(ntree = num_tree)) -> cfImp_unbiased

# classical, no duplication
party::cforest(y ~ ., data = simulated,
    controls = cforest_classical(ntree = num_tree)) -> cfImp_classical

# unbiased, duplication
party::cforest(y ~ ., data = simulated_dup,
               controls = cforest_unbiased(ntree = num_tree)) -> cfImp_unbiased_dup

# classical, duplication
party::cforest(y ~ ., data = simulated_dup,
               controls = cforest_classical(ntree = num_tree)) -> cfImp_classical_dup

tibble(unbiased = c(party::varimp(cfImp_unbiased, conditional = FALSE), NA),
       classical = c(party::varimp(cfImp_classical, conditional = FALSE), NA),
       unbiased_dup = party::varimp(cfImp_unbiased_dup, conditional = FALSE),
       classical_dup = party::varimp(cfImp_classical_dup, conditional = FALSE),
       unbiased_strobl = c(party::varimp(cfImp_unbiased, conditional = TRUE), NA),
       classical_strobl = c(party::varimp(cfImp_classical, conditional = TRUE), NA),
       unbiased_dup_strobl = party::varimp(cfImp_unbiased_dup, conditional = TRUE),
       classical_dup_strobl = party::varimp(cfImp_classical_dup, conditional = TRUE)
       ) -> importance

# Yes the trend continues

