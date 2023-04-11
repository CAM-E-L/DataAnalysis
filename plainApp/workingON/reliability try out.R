setwd("C:/DATEN/PHD/CAMtools_CAMapp/workingON")
library(tibble)
library(xlsx)
library(stringdist)
library(tidyverse)
library(irr)
files <- list()
files[[1]] <- xlsx::read.xlsx2(file = "aaa.xlsx", sheetIndex = 1)
files[[2]] <- xlsx::read.xlsx2(file = "bbb.xlsx", sheetIndex = 1)
files[[3]] <- xlsx::read.xlsx2(file = "ccc.xlsx", sheetIndex = 1)







getOverallRaterList(files = files,
                    orderAlphabetically = TRUE,
                    raterNames = LETTERS[1:3])





cohensKappasMaximized <- computeCohensKappaMaximized(files = files, numberRaters = 3)
cohensKappasMaximized


out_summaryCohensKappa <- list(mean(cohensKappas[lower.tri(x = cohensKappas, diag = FALSE)]),
mean(cohensKappasMaximized[lower.tri(x = cohensKappasMaximized, diag = FALSE)]))
names(out_summaryCohensKappa) <- c("a", "b")
out_summaryCohensKappa



out_OverallRaterList <- getOverallRaterList(files = files,
                                           orderAlphabetically = TRUE,
                                           raterNames = LETTERS[1:3])



tmp <- out_OverallRaterList[, str_subset(string = colnames(out_OverallRaterList),
                                  pattern = "Superordinate")]
kappam.fleiss(ratings = tmp,
              detail=TRUE)


tmp <- out_OverallRaterList[, str_subset(string = colnames(out_OverallRaterList),
                                         pattern = "Rating_")]
tmp <- kappam.fleiss(ratings = tmp,
              detail=TRUE)



ovallRaterList <- out_OverallRaterList


number_wordCom <- sort(table(unlist(ovallRaterList[, str_subset(string = colnames(ovallRaterList),
                                                                      pattern = "Rating_")])), decreasing = TRUE)
# number_wordCom



tmp_Ratings <- ovallRaterList[, c("Words", str_subset(string = colnames(ovallRaterList),
                                                            pattern = "Rating_"))]
tmp_Ratings <- tmp_Ratings %>%
  gather(variable, value, -Words)


tmp_Superordinate <- ovallRaterList[, str_subset(string = colnames(ovallRaterList),
                                                            pattern = "Superordinate_")]
tmp_Superordinate <- tmp_Superordinate %>%
  gather(variable, value)



tmp_Ratings$Superordinate <- tmp_Superordinate$value
tmp_Ratings$variable <- str_remove(string = tmp_Ratings$variable, pattern = "Rating_")
colnames(tmp_Ratings) <- c("Words", "Raters", "Rating", "Superordinate")




for(w in 1:length(number_wordCom)){
  tmp_data <- data.frame(Raters = tmp_Ratings$Raters[tmp_Ratings$Rating %in% names(number_wordCom)[w]],
                         Superordinates = tmp_Ratings$Superordinate[tmp_Ratings$Rating %in% names(number_wordCom)[w]],
                         Words = tmp_Ratings$Words[tmp_Ratings$Rating %in% names(number_wordCom)[w]])

cat("\nword combination: ", names(number_wordCom)[w],
    "\n,        # of Superordinates:", length(unique(tmp_data$Superordinate)),
    ", # of Raters:", length(unique(tmp_data$Raters)),
    ", # of words:", number_wordCom[w],
    "\n")
  print(tmp_data)
}


names(number_wordCom)[1]
sort(tmp_Ratings$Superordinate[tmp_Ratings$Rating %in% names(number_wordCom)[w]])
sort(unique(tmp_Ratings$Words[tmp_Ratings$Rating %in% names(number_wordCom)[w]]))



ovallRaterList[, str_subset(string = colnames(ovallRaterList),
                                  pattern = "Rating_")]

tmp


##################################
table(files[[1]]$Rating,files[[2]]$Rating)
kappa2(ratings = cbind(files[[1]]$Rating,files[[2]]$Rating))


table(files[[1]]$Rating,files[[3]]$Rating)
kappa2(ratings = cbind(files[[1]]$Rating,files[[3]]$Rating))



kappam.fleiss(ratings = cbind(files[[1]]$Rating,files[[2]]$Rating,files[[3]]$Rating),
              detail=TRUE)  # Fleiss' and category-wise Kappa








##################################








numberRaters <- 3



# rownames(cohensKappas) <- colnames(diagnoses)
# colnames(cohensKappas) <- colnames(diagnoses)
















cohensKappas - cohensKappasMaximized






#############




tmp_longer
tmp_smaller
tmp_column[,1] %in% tmp_row[2,]




tmp_row[1,] %in% tmp_column



tmp_A[2,] %in% tmp_B[2,]

tmp_B %in% tmp_A

# files[[i]]$Rating[files[[i]]$Superordinate %in% vector_words[j]] <-
#   vector_dummy_words[j]


tmp_ratings <- unique(files[[1]]$Rating)
tmp_ratings <- tmp_ratings[order(tmp_ratings, na.last = NA)]

str_split(string = files[[1]]$Rating, pattern = "r", simplify = TRUE)
str_split(string = files[[3]]$Rating, pattern = "r", simplify = TRUE)



for(r in tmp_ratings){
  print(r)
  tmp_stringdist <- stringdist::stringdist(a = r,
                                           b = files[[3]]$Rating)
  print(tmp_stringdist)
}
files[[3]]$Rating
cbind(files[[1]]$Superordinate, files[[2]]$Superordinate)









cbind(files[[1]]$Words,
      files[[1]]$Superordinate, files[[1]]$Rating,
      files[[2]]$Superordinate, files[[2]]$Rating)

table(files[[1]]$Rating,files[[2]]$Rating)
kappa2(ratings = cbind(files[[1]]$Rating,files[[2]]$Rating))


files[[3]]$Rating[files[[3]]$Rating == "r1r3r7"] <- "w01"
files[[1]]$Rating[files[[1]]$Rating == "r1r3r7r10"] <- "w01"

table(files[[1]]$Rating,files[[3]]$Rating)
kappa2(ratings = cbind(files[[1]]$Rating,files[[3]]$Rating))


library(irr)
data(diagnoses)
head(diagnoses)
kappam.light(diagnoses)   # Light's Kappa

kappam.fleiss(diagnoses, exact=TRUE)   # Exact Kappa
kappam.fleiss(diagnoses, detail=TRUE)  # Fleiss' and category-wise Kappa




colnames(diagnoses)


cohensKappas <- matrix(data = NA, nrow = ncol(diagnoses), ncol = ncol(diagnoses))

rownames(cohensKappas) <- colnames(diagnoses)
colnames(cohensKappas) <- colnames(diagnoses)


for(r in 1:nrow(cohensKappas)){
  for(c in 1:ncol(cohensKappas)){
    tmp_kappa <- kappa2(ratings = diagnoses[, c(rownames(cohensKappas)[r], colnames(cohensKappas)[c])])
    cohensKappas[r,c] <- tmp_kappa$value
    }
}
cohensKappas
round(x = cohensKappas, digits = 2)


tmp <- round(x = cohensKappas, digits = 2)
tmp <- rbind(tmp, NA)
tmp[nrow(cohensKappas)+1,1] <- "Light's Kappa"
tmp[nrow(cohensKappas)+1,2] <- mean(cohensKappas[lower.tri(x = cohensKappas, diag = FALSE)])
xlsx::write.xlsx2(x = tmp, file = "aa.xlsx")

?xlsx::read.xlsx2()


mean(cohensKappas[lower.tri(x = cohensKappas, diag = FALSE)])
kappam.light(diagnoses)   # Light's Kappa


table(diagnoses$rater1, diagnoses$rater2)
table(diagnoses$rater1, diagnoses$rater6)



a <- kappam.fleiss(diagnoses, detail=TRUE)  # Fleiss' and category-wise Kappa
a
a$detail
a$value



round(x = 1.5, digits = -2)
floor(x = 1.5)
ceiling(x = 1.1)
