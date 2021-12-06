

library(ggplot2)
library(dplyr)
library(Hmisc)
library(psych)


PATH <- "G:/Chuyen De R/FullData.csv"

datas <- read.csv(PATH, stringsAsFactors = F)

tien_dao = c("ST", "CF", "LF", "RF", "RW", "LW")
datas = filter(datas, Club_Position %in% tien_dao)


lm_datamovie <- select(datas, c(Skill_Moves,
                                Ball_Control,
                                Standing_Tackle,
                                Attacking_Position,
                                Crossing,
                                Acceleration,
                                Speed,
                                Finishing,
                                Freekick_Accuracy,
                                Volleys,
                                Rating))

lm_soccer <- lm(Rating ~ ., data=lm_datamovie)
summary(lm_soccer)

lm_s <- step(lm_soccer, direction = "backward", trace = FALSE)
summary(lm_s)
anova(lm_s)
###s

pairs.panels(lm_datamovie, col='red')

mov_fh <- data.frame(Skill_Moves = 5,
                     Ball_Control = 93,
                     Standing_Tackle= 31,
                     Attacking_Position = 94,
                     Crossing = 84,
                     Acceleration = 91,
                     Speed = 92,
                     Finishing = 93,
                     Freekick_Accuracy = 76,
                     Volleys = 88)

prediction <- predict(lm_s, newdata=mov_fh, interval="confidence")
prediction