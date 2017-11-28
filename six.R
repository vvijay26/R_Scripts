#String functions
#1 Paste Sep and Collapse
a <- "Hello"
b <- 'How'
c <- "are you? "
print(paste(a,b,c))
#"Hello How are you? "
print(paste(a,b,c, sep = "-"))
#"Hello-How-are you? "
print(paste(a,b,c, sep = "", collapse = ""))
#"HelloHoware you? 

#2 everything else
# Total number of digits displayed. Last digit rounded off.
result <- format(23.123456789, digits = 9)
print(result)
# Display numbers in scientific notation.
result <- format(c(6, 13.14521), scientific = TRUE)
print(result)
# The minimum number of digits to the right of the decimal point.
result <- format(23.47, nsmall = 5)
print(result)
# Format treats everything as a string.
result <- format(6)
print(result)
# Numbers are padded with blank in the beginning for width.
result <- format(13.7, width = 6)
print(result)
# Left justify strings.
result <- format("Hello",width = 8, justify = "l")
print(result)
# Justfy string with center.
result <- format("Hello",width = 8, justify = "c")
print(result)

# Changing to Upper case.
result <- toupper("Changing To Upper")
print

# Extract characters from 5th to 7th position.
result <- substring("Extract", 5, 7)
print(result)