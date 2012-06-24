DE1yrClose <- read.table("~/R/CSV/DE1yrClose.csv", quote="\"")
LNT1yrClose <- read.table("~/R/CSV/LNT1yrClose.csv", quote="\"")
DEquarteryrClose <- DE1yrClose$V1[200:253]
LNTquarteryrClose <- LNT1yrClose$V1[200:253]
LNT1QClose <- as.data.frame(LNTquarteryrClose)
DE1QClose <- as.data.frame(DEquarteryrClose)
