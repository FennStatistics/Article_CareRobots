getwd()
dir()

dir.create("data")
dir()

a <-1 

?`+`

x<-5
x=5
x == 5

help("mean")
?mean

x <- c(1,2,3,4,5)
mean(x)
x[2] <- "Hallo"
mean(x)

is.character(x)
as.numeric(x)

x <- c(FALSE, FALSE, TRUE, TRUE, NA)
summary(x)

mean(x,na.rm=TRUE) #En R, la expresión mean(x, na.rm = TRUE) calcula la media (promedio) de los valores del objeto x, ignorando los valores faltantes (NA). #na.rm = TRUE → le dice a R que elimine (rm) los NA antes de calcular.
as.numeric(x)

getwd()
setwd(data)
dir()

data <- read.csv("/Users/lauralondono/Downloads/introductory-workshop-in-R-main/Slides/data/tramo1998etal_twins.csv")
View(data)

f1 <- function() {
  print("Hallo Welt")
}
[1]

data <- data.frame(id=1:26,
                   letters= letters,
                   constant=HelloWorld)
head(data)

summary(data)

attributes(data)

  