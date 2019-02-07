rawdata <-
"2,3,4,5,1
1,2,3,4,5
3,4,5,2,1
4,3,5,1,2
2,3,4,5,1
3,2,5,4,1
1,4,5,3,2
4,3,2,5,1
3,4,5,1,2
4,3,5,2,1
1,3,4,5,2
4,3,5,2,1
4,3,1,5,2
2,3,1,4,5
1,3,2,4,5
4,2,3,5,1
3,2,1,4,5
1,5,3,4,2
1,4,2,5,3
1,2,3,5,4
2,5,4,3,1
2,3,4,5,1
3,4,5,2,1
3,4,5,2,1
5,1,3,2,4
1,3,5,4,2
4,1,3,2,5
2,3,4,5,1"

d <- read.csv(textConnection(rawdata), header=FALSE)

print("Calculating how many times each note appeared in each rank position")
table(d$V1)
table(d$V2)
table(d$V3)
table(d$V4)
table(d$V5)

print("Calculating the rank of each note (smaller value means higher rank)")
colSums(d)

print("Calculating the respondent agreement")
agree(d)

print("Conclusions:")
print("The notes were ranked as follows: 5 1 2 4 3")
print("The respondent agreement was very low and no final agreement on the ranking can be claimed")
