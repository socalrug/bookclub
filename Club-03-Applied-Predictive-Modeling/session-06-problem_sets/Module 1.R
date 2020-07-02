
p = 0.9
even = 0
odd = 0

  for (j in 1:10000){
    count = 1
    win = 0
    while (win == 0){
      outcome = rbinom(3, size = 1, prob = p)
      if (length(unique(outcome)) == 2){
        win = 1
      }
      else{
        count = count+1
      }
    }
    if (count %% 2 == 0){
      even = even + 1
    }
    else{
      odd = odd + 1
    }
  }

prob_of_odd = odd / (odd + even)

