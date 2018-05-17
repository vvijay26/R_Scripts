#Decision syntax (if...else...)
#1 if...else...
x <- c("what","is","Truth")
if("Truth" %in% x){
  print("Truth is found")
} else {
  print("Truth is not found")
}
# Note that Truth and truth are different (first letter case...)

#2 Multi stage if..else..if...
x <- c("what","is","truth")
if("Truth" %in% x){
  print("Truth is found the first time")
} else if ("truth" %in% x) {
  print("truth is found the second time")
} else {
  print("No truth found")
}

#3 Switch
x <- switch(
  3,
  "first",
  "second",
  "third",
  "fourth"
)
print(x)
