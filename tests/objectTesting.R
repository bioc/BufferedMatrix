library(BufferedMatrix);library.dynam("BufferedMatrix");


### this is used to control how many repetitions in something below
### higher values result in more checks.
nreps <-100 ##20000


## test creation and some simple assignments and subsetting operations

## first on single elements
tmp <- createBufferedMatrix(1000,10)

tmp[10,5]
tmp[10,5] <- 10
tmp[10,5]
tmp[10,5] <- 12.445
tmp[10,5]



## now testing accessing multiple elements
tmp2 <- createBufferedMatrix(10,20)


tmp2[3,1] <- 51.34
tmp2[9,2] <- 9.87654
tmp2[,1:2]
tmp2[,-(3:20)]
tmp2[3,]
tmp2[-3,]
tmp2[2,1:3]
tmp2[3:9,1:3]
tmp2[-4,-4]

## now testing accessing/assigning multiple elements
tmp3 <- createBufferedMatrix(10,10)

for (i in 1:10){
  for (j in 1:10){
    tmp3[i,j] <- (j-1)*10 + i
  }
}

tmp3[2:4,2:4]
tmp3[c(-10),c(2:4,2:4,10,1,2,1:10,10:1)]
tmp3[-c(1:5),-c(6:10)]

## assignment of whole columns
tmp3[,1] <- c(1:10*100.0)
tmp3[,1:2] <- tmp3[,1:2]*100
tmp3[,1:2] <- tmp3[,2:1]
tmp3[,1:2]


tmp3[,-1] <- tmp3[,1:9]
tmp3[,1:10]

tmp3[,1:2] <- rep(1,10)
tmp3[,1:2] <- rep(1,20)
tmp3[,1:2] <- matrix(c(1:5),1,5)

tmp3[,-c(1:8)] <- matrix(c(1:5),1,5)

tmp3[1,] <- 1:10
tmp3[1,]
tmp3[-1,] <- c(1,2)
tmp3[1:10,]
tmp3[-c(1:8),] <- matrix(c(1:5),1,5)
tmp3[1:10,]


tmp3[1:2,1:2] <- 5555.04
tmp3[-(1:2),1:2] <- 1234.56789



## testing accessors for the directory and prefix
directory(tmp3)
prefix(tmp3)

## testing if we can remove these objects
rm(tmp, tmp2, tmp3)
gc()




##
## checking reads
##

tmp2 <- createBufferedMatrix(10,20)

test.sample <- rnorm(10*20)

tmp2[1:10,1:20] <- test.sample

test.matrix <- matrix(test.sample,10,20)

## testing reads
for (rep in 1:nreps){
  which.row <- sample(1:10,1)
  which.col <- sample(1:20,1)
  if (tmp2[which.row,which.col] != test.matrix[which.row,which.col]){
    cat("incorrect agreement")
    break;
  }
}


for (rep in 1:nreps){
  which.row <- sample(1:10,1)
  if (!all(tmp2[which.row,] == test.matrix[which.row,])){
    cat("incorrect agreement")
    break;
  }
}


for (rep in 1:nreps){
  which.col <- sample(1:20,1)
  if (!all(tmp2[,which.col] == test.matrix[,which.col])){
    cat("incorrect agreement")
    break;
  }
}



for (rep in 1:nreps){
  which.col <- sample(1:10,5,replace=TRUE)
  if (!all(tmp2[,which.col] == test.matrix[,which.col])){
    cat("incorrect agreement")
    break;
  }
}


date()
for (rep in 1:nreps){
  which.row <- sample(1:10,5,replace=TRUE)
  if (!all(tmp2[which.row,] == test.matrix[which.row,])){
    cat("incorrect agreement")
    break;
  }
}
date()


for (rep in 1:nreps){
  which.row <- sample(1:10,5,replace=TRUE)
  which.col <- sample(1:10,5,replace=TRUE)
  if (!all(tmp2[which.row,which.col] == test.matrix[which.row,which.col])){
    cat("incorrect agreement")
    break;
  }
}





RowMode(tmp2)



for (rep in 1:nreps){
  which.row <- sample(1:10,1)
  which.col <- sample(1:20,1)
  if (tmp2[which.row,which.col] != test.matrix[which.row,which.col]){
    cat("incorrect agreement")
    break;
  }
}


for (rep in 1:nreps){
  which.row <- sample(1:10,1)
  if (!all(tmp2[which.row,] == test.matrix[which.row,])){
    cat("incorrect agreement")
    break;
  }
}


for (rep in 1:nreps){
  which.col <- sample(1:20,1)
  if (!all(tmp2[,which.col] == test.matrix[,which.col])){
    cat("incorrect agreement")
    break;
  }
}



for (rep in 1:nreps){
  which.col <- sample(1:20,5,replace=TRUE)
  if (!all(tmp2[,which.col] == test.matrix[,which.col])){
    cat("incorrect agreement")
    break;
  }
}



for (rep in 1:nreps){
  which.row <- sample(1:10,5,replace=TRUE)
  if (!all(tmp2[which.row,] == test.matrix[which.row,])){
    cat("incorrect agreement")
    break;
  }
}


date()
for (rep in 1:nreps){
  which.row <- sample(1:10,5,replace=TRUE)
  which.col <- sample(1:20,5,replace=TRUE)
  if (!all(tmp2[which.row,which.col] == test.matrix[which.row,which.col])){
    cat("incorrect agreement")
    break;
  }
}
date()

ColMode(tmp2)



### Now testing assignments

for (rep in 1:nreps){
  which.row <- sample(1:10,1)

  new.data <- rnorm(20)
  tmp2[which.row,] <- new.data
  test.matrix[which.row,] <- new.data
  if (rep > 1){
    if (!all(tmp2[prev.row,] == test.matrix[prev.row,])){
      cat("incorrect agreement")
      break;
    }
  }
  prev.row <- which.row
  
}





for (rep in 1:nreps){
  which.col <- sample(1:20,1)
  new.data <- rnorm(10)
  tmp2[,which.col] <- new.data
  test.matrix[,which.col]<- new.data

  if (rep > 1){
    if (!all(tmp2[,prev.col] == test.matrix[,prev.col])){
      cat("incorrect agreement")
      break;
    }
  }
  prev.col <- which.col
}





for (rep in 1:nreps){
  which.col <- sample(1:20,5,replace=TRUE)
  new.data <- matrix(rnorm(50),5,10)
  tmp2[,which.col] <- new.data
  test.matrix[,which.col]<- new.data
  
  if (rep > 1){
    if (!all(tmp2[,prev.col] == test.matrix[,prev.col])){
      cat("incorrect agreement")
      break;
    }
  }
  prev.col <- which.col
}



for (rep in 1:nreps){
  which.row <- sample(1:10,5,replace=TRUE)
  new.data <- matrix(rnorm(50),5,10)
  tmp2[which.row,] <- new.data
  test.matrix[which.row,]<- new.data
  
  if (rep > 1){
    if (!all(tmp2[prev.row,] == test.matrix[prev.row,])){
      cat("incorrect agreement")
      break;
    }
  }
  prev.row <- which.row
}





for (rep in 1:nreps){
  which.row <- sample(1:10,5,replace=TRUE)
  which.col  <- sample(1:20,5,replace=TRUE)
  new.data <- matrix(rnorm(25),5,5)
  tmp2[which.row,which.col] <- new.data
  test.matrix[which.row,which.col]<- new.data
  
  if (rep > 1){
    if (!all(tmp2[prev.row,prev.col] == test.matrix[prev.row,prev.col])){
      cat("incorrect agreement")
      break;
    }
  }
  prev.row <- which.row
  prev.col <- which.col
}




###
###
### testing some more functions
###



## duplication function
tmp5 <- duplicate(tmp2)

# making sure really did copy everything.
tmp5[1,1] <- tmp5[1,1] +100.00

if (tmp5[1,1] == tmp2[1,1]){
  stop("Problem with duplication")
}




### testing elementwise applying of functions

tmp5[1:4,1:4]
ewApply(tmp5,abs)
tmp5[1:4,1:4]
ewApply(tmp5,sqrt)
tmp5[1:4,1:4]

my.function <- function(x,power){
  (x+5)^power
}

ewApply(tmp5,my.function,power=2)
tmp5[1:4,1:4]



## testing functions that elementwise transform the matrix
sqrt(tmp5)
exp(tmp5)
log(tmp5,2)
pow(tmp5,2)





## testing functions that apply to entire matrix
Max(tmp5)
Min(tmp5)
mean(tmp5)
Sum(tmp5)
Var(tmp5)


## testing functions applied to rows or columns

rowMeans(tmp5)
rowSums(tmp5)
rowVars(tmp5)
rowSd(tmp5)
rowMax(tmp5)
rowMin(tmp5)

colMeans(tmp5)
colSums(tmp5)
colVars(tmp5)
colSd(tmp5)
colMax(tmp5)
colMin(tmp5)


### setting a random element to NA and then testing with na.rm=TRUE or na.rm=FALSE (The default)


which.row <- sample(1:10,1,replace=TRUE)
which.col  <- sample(1:20,1,replace=TRUE)

tmp5[which.row,which.col] <- NA

Max(tmp5)
Min(tmp5)
mean(tmp5)
Sum(tmp5)
Var(tmp5)

rowMeans(tmp5)
rowSums(tmp5)
rowVars(tmp5)
rowSd(tmp5)
rowMax(tmp5)
rowMin(tmp5)

colMeans(tmp5)
colSums(tmp5)
colVars(tmp5)
colSd(tmp5)
colMax(tmp5)
colMin(tmp5)

Max(tmp5,na.rm=TRUE)
Min(tmp5,na.rm=TRUE)
mean(tmp5,na.rm=TRUE)
Sum(tmp5,na.rm=TRUE)
Var(tmp5,na.rm=TRUE)

rowMeans(tmp5,na.rm=TRUE)
rowSums(tmp5,na.rm=TRUE)
rowVars(tmp5,na.rm=TRUE)
rowSd(tmp5,na.rm=TRUE)
rowMax(tmp5,na.rm=TRUE)
rowMin(tmp5,na.rm=TRUE)

colMeans(tmp5,na.rm=TRUE)
colSums(tmp5,na.rm=TRUE)
colVars(tmp5,na.rm=TRUE)
colSd(tmp5,na.rm=TRUE)
colMax(tmp5,na.rm=TRUE)
colMin(tmp5,na.rm=TRUE)

# now set an entire row to NA

tmp5[which.row,] <- NA
rowMeans(tmp5,na.rm=TRUE)
rowSums(tmp5,na.rm=TRUE)
rowVars(tmp5,na.rm=TRUE)
rowSd(tmp5,na.rm=TRUE)
rowMax(tmp5,na.rm=TRUE)
rowMin(tmp5,na.rm=TRUE)


# now set an entire col to NA


tmp5[,which.col] <- NA
colMeans(tmp5,na.rm=TRUE)
colSums(tmp5,na.rm=TRUE)
colVars(tmp5,na.rm=TRUE)
colSd(tmp5,na.rm=TRUE)
colMax(tmp5,na.rm=TRUE)
colMin(tmp5,na.rm=TRUE)




copymatrix <- matrix(rnorm(200,150,15),10,20)

tmp5[1:10,1:20] <- copymatrix
which.row <- 3
which.col  <- 1
cat(which.row," ",which.col,"\n")
tmp5[which.row,which.col] <- NA
copymatrix[which.row,which.col] <- NA

rowVars(tmp5,na.rm=TRUE)
apply(copymatrix,1,var,na.rm=TRUE)



copymatrix <- matrix(rnorm(200,150,15),10,20)

tmp5[1:10,1:20] <- copymatrix
which.row <- 1
which.col  <- 3
cat(which.row," ",which.col,"\n")
tmp5[which.row,which.col] <- NA
copymatrix[which.row,which.col] <- NA

colVars(tmp5,na.rm=TRUE)-apply(copymatrix,2,var,na.rm=TRUE)










## making sure these things agree
##
## first when there is no NA


for (rep in 1:20){
  copymatrix <- matrix(rnorm(200,150,15),10,20)
  
  tmp5[1:10,1:20] <- copymatrix
  
  
  if(Max(tmp5) != max(copymatrix)){
    stop("No agreement in Max")
  }
  

  if(Min(tmp5) != min(copymatrix)){
    stop("No agreement in Min")
  }


  if (Sum(tmp5)!= sum(copymatrix)){
    stop("No agreement in Sum")
  }
  
  if(mean(tmp5)!= mean(copymatrix)){
    stop("No agreement in mean")
  }
  
  
  if(abs(Var(tmp5) - var(as.vector(copymatrix))) > 1e-12){
    stop("No agreement in Var")
  }
  
  

  if(any(rowMeans(tmp5) != apply(copymatrix,1,mean))){
    stop("No agreement in rowMeans")
  }
  
  
  if(any(colMeans(tmp5) != apply(copymatrix,2,mean))){
    stop("No agreement in colMeans")
  }
  
  
  if(any(rowSums(tmp5) != apply(copymatrix,1,sum))){
    stop("No agreement in rowSums")
  }
  
  
  if(any(colSums(tmp5) != apply(copymatrix,2,sum))){
    stop("No agreement in colSums")
  }
  
  
  if(any(abs(rowVars(tmp5) - apply(copymatrix,1,var))  > 1e-12)){
    stop("No agreement in rowVars")
  }
  
  
  if(any(abs(colVars(tmp5) - apply(copymatrix,2,var))  > 1e-12)){
    stop("No agreement in rowVars")
  }


  if(any(abs(rowMax(tmp5) - apply(copymatrix,1,max))  > 1e-12)){
    stop("No agreement in colMax")
  }
  

  if(any(abs(colMax(tmp5) - apply(copymatrix,2,max))  > 1e-12)){
    stop("No agreement in colMax")
  }
  
  
  
  if(any(abs(rowMin(tmp5) - apply(copymatrix,1,min))  > 1e-12)){
    stop("No agreement in colMin")
  }
  

  if(any(abs(colMin(tmp5) - apply(copymatrix,2,min))  > 1e-12)){
    stop("No agreement in colMin")
  }
  


  ## now lets assign some NA values and check agreement

  which.row <- sample(1:10,1,replace=TRUE)
  which.col  <- sample(1:20,1,replace=TRUE)
  
  cat(which.row," ",which.col,"\n")
  
  tmp5[which.row,which.col] <- NA
  copymatrix[which.row,which.col] <- NA
  
  if(Max(tmp5,na.rm=TRUE) != max(copymatrix,na.rm=TRUE)){
    stop("No agreement in Max")
  }
  
  
  if(Min(tmp5,na.rm=TRUE) != min(copymatrix,na.rm=TRUE)){
    stop("No agreement in Min")
  }
  
  
  if (Sum(tmp5,na.rm=TRUE)!= sum(copymatrix,na.rm=TRUE)){
    stop("No agreement in Sum")
  }
  
  if(mean(tmp5,na.rm=TRUE)!= mean(copymatrix,na.rm=TRUE)){
    stop("No agreement in mean")
  }
  
  
  if(abs(Var(tmp5,na.rm=TRUE) - var(as.vector(copymatrix),na.rm=TRUE)) > 1e-12){
    stop("No agreement in Var")
  }
  
  
  
  if(any(rowMeans(tmp5,na.rm=TRUE) != apply(copymatrix,1,mean,na.rm=TRUE))){
    stop("No agreement in rowMeans")
  }
  
  
  if(any(colMeans(tmp5,na.rm=TRUE) != apply(copymatrix,2,mean,na.rm=TRUE))){
    stop("No agreement in colMeans")
  }
  

  if(any(rowSums(tmp5,na.rm=TRUE) != apply(copymatrix,1,sum,na.rm=TRUE))){
    stop("No agreement in rowSums")
  }
  

  if(any(colSums(tmp5,na.rm=TRUE) != apply(copymatrix,2,sum,na.rm=TRUE))){
    stop("No agreement in colSums")
  }
  

  if(any(abs(rowVars(tmp5,na.rm=TRUE) - apply(copymatrix,1,var,na.rm=TRUE))  > 1e-12)){
    stop("No agreement in rowVars")
  }
  
  if(any(abs(colVars(tmp5,na.rm=TRUE) - apply(copymatrix,2,var,na.rm=TRUE))  > 1e-12)){
    stop("No agreement in rowVars")
  }
  

  if(any(abs(rowMax(tmp5,na.rm=TRUE) - apply(copymatrix,1,max,na.rm=TRUE))  > 1e-12)){
    stop("No agreement in colMax")
  }
  

  if(any(abs(colMax(tmp5,na.rm=TRUE) - apply(copymatrix,2,max,na.rm=TRUE))  > 1e-12)){
    stop("No agreement in colMax")
  }
  


  if(any(abs(rowMin(tmp5,na.rm=TRUE) - apply(copymatrix,1,min,na.rm=TRUE))  > 1e-12)){
    stop("No agreement in colMin")
  }


  if(any(abs(colMin(tmp5,na.rm=TRUE) - apply(copymatrix,2,min,na.rm=TRUE))  > 1e-12)){
    stop("No agreement in colMin")
  }




  ## make an entire row NA
  tmp5[which.row,] <- NA
  copymatrix[which.row,] <- NA
  
  if(Max(tmp5,na.rm=TRUE) != max(copymatrix,na.rm=TRUE)){
    stop("No agreement in Max")
  }
  
  
  if(Min(tmp5,na.rm=TRUE) != min(copymatrix,na.rm=TRUE)){
    stop("No agreement in Min")
  }
  
  
  if (Sum(tmp5,na.rm=TRUE)!= sum(copymatrix,na.rm=TRUE)){
    stop("No agreement in Sum")
  }
  
  if(mean(tmp5,na.rm=TRUE)!= mean(copymatrix,na.rm=TRUE)){
    stop("No agreement in mean")
  }
  
  
  if(abs(Var(tmp5,na.rm=TRUE) - var(as.vector(copymatrix),na.rm=TRUE)) > 1e-12){
    stop("No agreement in Var")
  }
  


  if(any(rowMeans(tmp5,na.rm=TRUE) != apply(copymatrix,1,mean,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in rowMeans")
  }
  
  
  if(any(colMeans(tmp5,na.rm=TRUE) != apply(copymatrix,2,mean,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in colMeans")
  }
  
  
  if(any(rowSums(tmp5,na.rm=TRUE) != apply(copymatrix,1,sum,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in rowSums")
  }
  
  
  if(any(colSums(tmp5,na.rm=TRUE) != apply(copymatrix,2,sum,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in colSums")
  }
  
  
  if(any(abs(rowVars(tmp5,na.rm=TRUE) - apply(copymatrix,1,var,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    cat(rowVars(tmp5,na.rm=TRUE),"\n")
    cat(apply(copymatrix,1,var,na.rm=TRUE),"\n")
    stop("No agreement in rowVars")
  }
  
  
  
  if(any(abs(colVars(tmp5,na.rm=TRUE) - apply(copymatrix,2,var,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    cat(colVars(tmp5,na.rm=TRUE),"\n")
    cat(apply(copymatrix,2,var,na.rm=TRUE),"\n")
    stop("No agreement in colVars")
  }
  
  
  if(any(abs(rowMax(tmp5,na.rm=TRUE) - apply(copymatrix,1,max,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMax")
  }
  
  
  if(any(abs(colMax(tmp5,na.rm=TRUE) - apply(copymatrix,2,max,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMax")
  }

  
  
  if(any(abs(rowMin(tmp5,na.rm=TRUE) - apply(copymatrix,1,min,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMin")
  }
  

  if(any(abs(colMin(tmp5,na.rm=TRUE) - apply(copymatrix,2,min,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMin")
  }


### also make an entire col NA
  tmp5[,which.col] <- NA
  copymatrix[,which.col] <- NA
  
  
  if(Max(tmp5,na.rm=TRUE) != max(copymatrix,na.rm=TRUE)){
    stop("No agreement in Max")
  }

  
  if(Min(tmp5,na.rm=TRUE) != min(copymatrix,na.rm=TRUE)){
    stop("No agreement in Min")
  }
  
  
  if (Sum(tmp5,na.rm=TRUE)!= sum(copymatrix,na.rm=TRUE)){
    stop("No agreement in Sum")
  }
  
  if(mean(tmp5,na.rm=TRUE)!= mean(copymatrix,na.rm=TRUE)){
    stop("No agreement in mean")
  }


  if(abs(Var(tmp5,na.rm=TRUE) - var(as.vector(copymatrix),na.rm=TRUE)) > 1e-12){
    stop("No agreement in Var")
  }
  


  if(any(rowMeans(tmp5,na.rm=TRUE) != apply(copymatrix,1,mean,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in rowMeans")
  }
  

  if(any(colMeans(tmp5,na.rm=TRUE) != apply(copymatrix,2,mean,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in colMeans")
  }
  
  
  if(any(rowSums(tmp5,na.rm=TRUE) != apply(copymatrix,1,sum,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in rowSums")
  }
  

  if(any(colSums(tmp5,na.rm=TRUE) != apply(copymatrix,2,sum,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in colSums")
  }
  
  
  if(any(abs(rowVars(tmp5,na.rm=TRUE) - apply(copymatrix,1,var,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in rowVars")
  }
  


  if(any(abs(colVars(tmp5,na.rm=TRUE) - apply(copymatrix,2,var,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in rowVars")
  }
  

  if(any(abs(rowMax(tmp5,na.rm=TRUE) - apply(copymatrix,1,max,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMax")
  }
  

  if(any(abs(colMax(tmp5,na.rm=TRUE) - apply(copymatrix,2,max,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMax")
  }
  


  if(any(abs(rowMin(tmp5,na.rm=TRUE) - apply(copymatrix,1,min,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMin")
  }


  if(any(abs(colMin(tmp5,na.rm=TRUE) - apply(copymatrix,2,min,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMin")
  }


  ### now make 1 element non NA with NA in the rest of row and column

  tmp5[which.row,which.col] <- rnorm(1,150,15)
  copymatrix[which.row,which.col] <- tmp5[which.row,which.col]


  if(Max(tmp5,na.rm=TRUE) != max(copymatrix,na.rm=TRUE)){
    stop("No agreement in Max")
  }

  
  if(Min(tmp5,na.rm=TRUE) != min(copymatrix,na.rm=TRUE)){
    stop("No agreement in Min")
  }
  
  
  if (Sum(tmp5,na.rm=TRUE)!= sum(copymatrix,na.rm=TRUE)){
    stop("No agreement in Sum")
  }
  
  if(mean(tmp5,na.rm=TRUE)!= mean(copymatrix,na.rm=TRUE)){
    stop("No agreement in mean")
  }


  if(abs(Var(tmp5,na.rm=TRUE) - var(as.vector(copymatrix),na.rm=TRUE)) > 1e-12){
    stop("No agreement in Var")
  }
  


  if(any(rowMeans(tmp5,na.rm=TRUE) != apply(copymatrix,1,mean,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in rowMeans")
  }
  

  if(any(colMeans(tmp5,na.rm=TRUE) != apply(copymatrix,2,mean,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in colMeans")
  }
  
  
  if(any(rowSums(tmp5,na.rm=TRUE) != apply(copymatrix,1,sum,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in rowSums")
  }
  

  if(any(colSums(tmp5,na.rm=TRUE) != apply(copymatrix,2,sum,na.rm=TRUE),na.rm=TRUE)){
    stop("No agreement in colSums")
  }
  
  
  if(any(abs(rowVars(tmp5,na.rm=TRUE) - apply(copymatrix,1,var,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in rowVars")
  }
  


  if(any(abs(colVars(tmp5,na.rm=TRUE) - apply(copymatrix,2,var,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in rowVars")
  }
  

  if(any(abs(rowMax(tmp5,na.rm=TRUE) - apply(copymatrix,1,max,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMax")
  }
  

  if(any(abs(colMax(tmp5,na.rm=TRUE) - apply(copymatrix,2,max,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMax")
  }
  


  if(any(abs(rowMin(tmp5,na.rm=TRUE) - apply(copymatrix,1,min,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMin")
  }


  if(any(abs(colMin(tmp5,na.rm=TRUE) - apply(copymatrix,2,min,na.rm=TRUE))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMin")
  }
  
}


### now test 1 by n and n by 1 matrix


rm(tmp5)

dataset1 <- rnorm(100)
dataset2 <- rnorm(100)

tmp <- createBufferedMatrix(1,100)
tmp[1,] <- dataset1

tmp2 <- createBufferedMatrix(100,1)
tmp2[,1] <- dataset2





Max(tmp)
Min(tmp)
mean(tmp)
Sum(tmp)
Var(tmp)

rowMeans(tmp)
rowSums(tmp)
rowVars(tmp)
rowSd(tmp)
rowMax(tmp)
rowMin(tmp)

colMeans(tmp)
colSums(tmp)
colVars(tmp)
colSd(tmp)
colMax(tmp)
colMin(tmp)



Max(tmp2)
Min(tmp2)
mean(tmp2)
Sum(tmp2)
Var(tmp2)

rowMeans(tmp2)
rowSums(tmp2)
rowVars(tmp2)
rowSd(tmp2)
rowMax(tmp2)
rowMin(tmp2)

colMeans(tmp2)
colSums(tmp2)
colVars(tmp2)
colSd(tmp2)
colMax(tmp2)
colMin(tmp2)


dataset1 <- matrix(dataset1,1,100)

  
  if(Max(tmp) != max(dataset1)){
    stop("No agreement in Max")
  }
  

  if(Min(tmp) != min(dataset1)){
    stop("No agreement in Min")
  }


  if (Sum(tmp)!= sum(dataset1)){
    stop("No agreement in Sum")
  }
  
  if(mean(tmp)!= mean(dataset1)){
    stop("No agreement in mean")
  }
  
  
  if(abs(Var(tmp) - var(as.vector(dataset1))) > 1e-12){
    stop("No agreement in Var")
  }
  
  

  if(any(rowMeans(tmp) != apply(dataset1,1,mean))){
    stop("No agreement in rowMeans")
  }
  
  
  if(any(colMeans(tmp) != apply(dataset1,2,mean))){
    stop("No agreement in colMeans")
  }
  
  
  if(any(rowSums(tmp) != apply(dataset1,1,sum))){
    stop("No agreement in rowSums")
  }
  
  
  if(any(colSums(tmp) != apply(dataset1,2,sum))){
    stop("No agreement in colSums")
  }
  
  
  if(any(abs(rowVars(tmp) - apply(dataset1,1,var))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in rowVars")
  }
  
  
  if(any(abs(colVars(tmp) - apply(dataset1,2,var))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in rowVars")
  }


  if(any(abs(rowMax(tmp) - apply(dataset1,1,max))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMax")
  }
  

  if(any(abs(colMax(tmp) - apply(dataset1,2,max))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMax")
  }
  
  
  
  if(any(abs(rowMin(tmp) - apply(dataset1,1,min))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMin")
  }
  

  if(any(abs(colMin(tmp) - apply(dataset1,2,min))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMin")
  }
  


dataset2 <- matrix(dataset2,100,1)
  
if(Max(tmp2) != max(dataset2)){
  stop("No agreement in Max")
}
  

if(Min(tmp2) != min(dataset2)){
  stop("No agreement in Min")
}


if (Sum(tmp2)!= sum(dataset2)){
  stop("No agreement in Sum")
}
  
if(mean(tmp2)!= mean(dataset2)){
  stop("No agreement in mean")
}
  
  
  if(abs(Var(tmp2) - var(as.vector(dataset2))) > 1e-12){
    stop("No agreement in Var")
  }
  
  

  if(any(rowMeans(tmp2) != apply(dataset2,1,mean))){
    stop("No agreement in rowMeans")
  }
  
  
  if(any(colMeans(tmp2) != apply(dataset2,2,mean))){
    stop("No agreement in colMeans")
  }
  
  
  if(any(rowSums(tmp2) != apply(dataset2,1,sum))){
    stop("No agreement in rowSums")
  }
  
  
  if(any(colSums(tmp2) != apply(dataset2,2,sum))){
    stop("No agreement in colSums")
  }
  
  
  if(any(abs(rowVars(tmp2) - apply(dataset2,1,var))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in rowVars")
  }
  
  
  if(any(abs(colVars(tmp2) - apply(dataset2,2,var))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in rowVars")
  }


  if(any(abs(rowMax(tmp2) - apply(dataset2,1,max))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMax")
  }
  

  if(any(abs(colMax(tmp2) - apply(dataset2,2,max))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMax")
  }
  
  
  
  if(any(abs(rowMin(tmp2) - apply(dataset2,1,min))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMin")
  }
  

  if(any(abs(colMin(tmp2) - apply(dataset2,2,min))  > 1e-12,na.rm=TRUE)){
    stop("No agreement in colMin")
  }
  


tmp <- createBufferedMatrix(10,10)

tmp[1:10,1:10] <- rnorm(100)
colApply(tmp,sum)
colApply(tmp,quantile)[,1]

rowApply(tmp,sum)
rowApply(tmp,rank)[1:10,]

tmp <- createBufferedMatrix(5,20)

tmp[1:5,1:20] <- rnorm(100)
colApply(tmp,sum)
colApply(tmp,quantile)[,1]

rowApply(tmp,sum)
rowApply(tmp,rank)[1:5,]


as.matrix(tmp)
