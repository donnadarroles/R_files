
#Define an R function that sorts a given vector in decreasing order. The output should be a vector of the same length. It should accept both numeric or character vectors.
#Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.
#Create a function that accepts a vector and integer n and returns nth highest number
#Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.


# Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1.
fact <- function(n) {
if(n <= 1) {
return(1)
} else {
return(n * fact(n-1))
}
}
fact(5)

#Define an R function that removes NA values from a vector.
x <- c(NA, 3, NA, 5, NA, 10)
removena <- function(x){
  x[!is.na(x)]
}
removena(x)

#Define an R function that computes the determinant of a given matrix. The output should be a vector of length 1.
DDeterminant<-function(a,b) {
  if (dim(a)[1] == 1 && dim(a)[2] == 1)
    return(a[1,1])
  if (dim(a)[1] == 2 && dim(a)[2] == 2)
    return(a[1,1]*a[2,2]-a[1,2]*a[2,1])
  else
    c = 0
  for (i in 1:dim(a)[2]) {
    c = c + a[b,i]*(-1)^(b+i)*
      DDeterminant(a[-b,-i],b)
  }
  return(c)
}
DDeterminant(matrix(c(4,2,1,5,6,7,1,0,3),nrow = 3,ncol = 3),1)
det(matrix(c(4,2,1,5,6,7,1,0,3),nrow = 3,ncol = 3),1)

#Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.
isPrime <- function(n) {
  if (n == 2) {
    TRUE
  } else if (any(n %% 2:(n-1) == 0)) {
    FALSE
  } else { 
    TRUE
  }
}
isPrime(1)
isPrime(3)
isPrime(5)
isPrime(10)

#Create a function that accepts a vector and integer n and returns nth highest number
testnth <- c(10,3,6,1,2,20) 
nHigh = function(v, n){
  getfirst = v[rev(order(v))][n]
  head(getfirst,1)
}
nHigh(testnth)

#Create a function to compute for your net pay at work.
NetPay = function(basic, wdays = 0){
  monthly = basic * 13
  if (monthly <=  250000){
    net = monthly
  } else if (monthly <= 400000) {
    net = monthly - (monthly - 250000) * 0.20
  } else if (monthly <= 800000) {
    net = monthly - (monthly - 400000) * 0.25 - 30000
  } else if (monthly <= 2000000) {
    net = monthly - (monthly - 800000) * 0.30 - 130000
  } else if (monthly <= 8000000) {
    net = monthly - (monthly - 2000000) * 0.32 - 490000
  } else {
    net = monthly - (monthly - 8000000) * 0.35 - 2410000
  }
  
  net_final = net / 13  - (net / 13 * wdays)
  return (net_final)
}
NetPay(20000)