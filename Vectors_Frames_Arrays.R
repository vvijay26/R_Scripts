#TutorialsPoint scripts for practice
#1 Console log
myString <- "Hello, World!"
print ( myString)

#2 class of vectors...class is like data type, in this example - character
v <- "TRUE"
print(class(v))

#3 Create a vector...use c() function to add more than 1 element
apple <- c('red','green',"yellow")
print(apple)

#4 Get the class of the vector...class is like data type
print(class(apple))

#5 Create a matrix. Matrices are always 2x2, more dimensions use array
M = matrix( c('a','a','b','c','b','a'), nrow=2,ncol=3,byrow = TRUE)
print(M)

#6 Create an array (3 elements each being 4x4 itself...
a <- array(c('green','yellow','red'),dim=c(4,4,3))
print(a)

#7 Factors
# Create a vector.
apple_colors <- c('green','green','yellow','red','red','red','green')
# Create a factor object.
factor_apple <- factor(apple_colors)
# Print the factor.
print(factor_apple)
print(nlevels(factor_apple))

#8 Create the data frame. Similar to array but can hold multiple
#data types
BMI <- data.frame(
  gender = c("Male", "Male","Female"),
  height = c(152, 171.5, 165),
  weight = c(81,93, 78),
  Age =c(42,38,26)
)
print(BMI)

