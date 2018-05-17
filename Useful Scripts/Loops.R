#Loops
#1 Repeat Loop + break functionality below
v <- c("Hello","loop")
cnt <- 2
repeat{
  print(v)
  cnt <- cnt+1
  if(cnt > 5){
    break
  }
}

#2 While Loop
v <- c("Hello","while loop")
cnt <- 2
while (cnt < 7){
  print(v)
  cnt = cnt + 1
}

#3 For Loop (not restriceted to numbers, can pass characters, vectors etc.)
v <- LETTERS[25:27]
for ( i in v) {
  print(i)
}

#4 Next functionality
v <- LETTERS[1:6]
for ( i in v){
  if (i == "D"){
    next
  }
  print(i)
}
