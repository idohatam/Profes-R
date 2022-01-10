#d2ltf is a function that takes a plain text files (txt) with true or false
#questions and converts them to CSV files in a format compatible with BrightSpace
#quiz format.


d2ltf <- function(a,b="exam_tf.csv"){

  lns <- readLines(file(a, open = "r"))
  mtf1 <- matrix(ncol = 2,nrow = length(lns)*4)

  i = 1

  for(ln in lns){
    mtf1[i,] <- c("NewQuestion","TF")
    i <- i+1
    if(grepl(pattern = "\\*$",ln)){
      mtf1[i,] <- c("QuestionText",
                    str_remove(str_remove(str_split_fixed(ln,"\\.",2)[2], " "),"\\*"))
      i <- i +1
      mtf1[i,] <- c("TRUE","100")
      i <- i+1
      mtf1[i,] <- c("FALSE", "0")
      i <- i+1
    }else{
      mtf1[i,] <- c("QuestionText",
                    str_remove(str_split_fixed(ln,"\\.",2)[2], " "))
      i <- i+1
      mtf1[i,] <- c("TRUE","0")
      i <- i + 1
      mtf1[i,] <- c("FALSE", "100")
      i <- i+1
    }
  }

  write.table(mtf1, file = b, sep = "," ,
              row.names = F, col.names = F)

}
