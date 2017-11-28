#1 Assignment using equal operator.
var.1 = c(0,1,2,3)
#2 Assignment using leftward operator.
var.2 <- c("learn","R")
#3 Assignment using rightward operator.
c(FALSE,1) -> var_3
print(var.1)
print(var.2)
print(var_3)
cat ("var.1 is ", var.1 ,"\n")
cat ("var.2 is ", var.2 ,"\n")
cat ("var_3 is ", var_3 ,"\n")
#The vector c(FALSE,1) has a mix of logical and numeric class. So logical class is
#coerced to numeric class making TRUE as 1 and FALSE as 0.

#4 In R, a variable itself is not declared of any data type, rather it gets the data type of the
#R -object assigned to it. Dynamically typed (like javaScript is loosely types)
#Case does not matter though (opposite to javaScript)
var_x <- "Hello"
cat("The class of var_x is ",class(var_x),"\n")
var_x <- 34.5
cat(" Now the class of var_x is ",class(var_x),"\n")
var_x <- 27L
cat(" Next the class of var_x becomes ",class(var_x),"\n")

#5 variables currently available in workspace
print (ls())
# The ls() function can use patterns to match the variable names.
# List the variables starting with the pattern "var". Here, case matters.
print(ls(pattern="var"))
#The variables starting with dot(.) are hidden, they can be listed using "all.names=TRUE"
#argument to ls() function.
print(ls(all.name=TRUE))

#6 Variables can be deleted by using the rm() function.
rm(var.3)
#All the variables can be deleted by using the rm() and ls() function together.
