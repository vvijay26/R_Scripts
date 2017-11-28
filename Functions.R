#Functions
#1 Some built-in functions.
# Create a sequence of numbers from 32 to 44.
print(seq(32,44))
# Find mean of numbers from 25 to 82.
print(mean(25:82))
# Find sum of numbers frm 41 to 68.
print(sum(41:68))

#2 User defined function example
# Create a function to print squares of numbers in sequence.
new.function <- function(a,b) {
  for(i in 1:a) {
    print(i^2)
  }
  for(j in 1:b) {
    print(j^2)
  }
}
# Call the function new.function supplying 6 as an argument (call by position).
new.function(6,7)
# the function new.function can also be called by passing variable names (call by name)
new.function(a=7,b=6)
# if the parent function passes values, it overrides the values 
# already present in the called function
new.function2 <- function(a=1,b=2) {
  for(i in 1:a) {
    print(i^2)
  }
  for(j in 1:b) {
    print(j^2)
  }
}
print("default values ->")
new.function2()
print("overridden values ->")
new.function2(3,4)

#3 Lazy evaluation (the arguments are evaluated when they are encountered)
# in below example, a will be printed and then error for b is thrown.
new.function3 <- function(a,b){
  cat ("a is", a ,"\n")
  cat ("b is", b ,"\n")
}
new.function3(3)