##
# title: "406 project"
# author: "Zhen Yang (zhenyz@umich.edu)"
# date: "11/21/2020"
#
# 


# load data
library(tidyverse)

personality = read_csv("2018-personality-data.csv")
ratings = read_csv("2018_ratings.csv")

names(personality)
head(personality)

ratings = ratings %>% transmute(userid = useri, movie_id, rating)
names(ratings)
head(ratings)

# histogram of personality
plotHist <- function(col){
ggplot(data = personality, mapping = aes(x = (!!as.name(col)))) +
  geom_histogram(binwidth = 0.5, color="white", fill="grey") +
  #labs(title = col) + 
  theme_classic()
}
plotHist("openness")
plotHist("agreeableness")
plotHist("emotional_stability")
plotHist("conscientiousness")
plotHist("extraversion")



# merge data
personality_1 = personality %>% 
  transmute(userid, openness, agreeableness, emotional_stability, conscientiousness, 
            extraversion, movie_id = movie_1, predicted_rating = predicted_rating_1)
d1 = merge(x = personality_1, y = ratings)
d1$diff = d1$predicted_rating - d1$rating

personality_2 = personality %>% 
  transmute(userid, openness, agreeableness, emotional_stability, conscientiousness, 
            extraversion, movie_id = movie_2, predicted_rating = predicted_rating_2)
d2 = merge(x = personality_2, y = ratings)
d2$diff = d2$predicted_rating - d2$rating

personality_3 = personality %>% 
  transmute(userid, openness, agreeableness, emotional_stability, conscientiousness, 
            extraversion, movie_id = movie_3, predicted_rating = predicted_rating_3)
d3 = merge(x = personality_3, y = ratings)
d3$diff = d3$predicted_rating - d3$rating

personality_4 = personality %>% 
  transmute(userid, openness, agreeableness, emotional_stability, conscientiousness, 
            extraversion, movie_id = movie_4, predicted_rating = predicted_rating_4)
d4 = merge(x = personality_4, y = ratings)
d4$diff = d4$predicted_rating - d4$rating

dat = union_all(
  union_all(
    union_all(
      mutate(d1, `Movie Position` = "1st"), 
      mutate(d2, `Movie Position` = "2nd")
    ),
    mutate(d3, `Movie Position` = "3rd")
  ),
  mutate(d4, `Movie Position` = "4th")
)

head(dat)

# density plot of movie ratings and predicted ratings
ggplot(data = dat, mapping = aes(x = diff, group = `Movie Position`, fill = `Movie Position`)) +
  geom_density(adjust=1.5, alpha=.4) + 
  theme_light()

rbind(transmute(dat, score = predicted_rating, category = "Predicted rating"), 
      transmute(dat, score = rating, category = "Rating")) %>%
  ggplot(mapping = aes(x = score, group = category, fill = category)) +
  geom_density(adjust=1.5, alpha=.4) + 
  theme_light()

# pairwise test

## overall

t.test(dat$predicted_rating, dat$rating, paired = TRUE)
t.test(dat$diff)
wilcox.test(dat$predicted_rating, dat$rating, paired = TRUE)
wilcox.test(dat$diff)

## Movie position 1-4

t.test(d1$predicted_rating, d1$rating, paired = TRUE)
t.test(d1$diff)
wilcox.test(d1$predicted_rating, d1$rating, paired = TRUE)
wilcox.test(d1$diff)

t.test(d2$predicted_rating, d2$rating, paired = TRUE)
t.test(d2$diff)
wilcox.test(d2$predicted_rating, d2$rating, paired = TRUE)
wilcox.test(d2$diff)

t.test(d3$predicted_rating, d3$rating, paired = TRUE)
t.test(d3$diff)
wilcox.test(d3$predicted_rating, d3$rating, paired = TRUE)
wilcox.test(d3$diff)

t.test(d4$predicted_rating, d4$rating, paired = TRUE)
t.test(d4$diff)
wilcox.test(d4$predicted_rating, d4$rating, paired = TRUE)
wilcox.test(d4$diff)

# regression

dat1 = dat %>% mutate(abs_diff = abs(diff))
m1 = lm(abs_diff ~ openness + agreeableness + emotional_stability + conscientiousness + extraversion + assigned_metric + assigned_condition, data = dat1)
summary(m1)


# simulation

# 95% confidence interval for the beta_1
confint(m1, 2, level = 0.95)

library(boot)
set.seed(1)
statistic <- function(data, indices){
  d <- data[indices,]
  coef(lm(abs_diff ~ openness + agreeableness + emotional_stability + conscientiousness + extraversion + assigned_metric + assigned_condition, data = d))[2]
}
m2 = boot(dat1, statistic, R = 2000)
boot.ci(m2)

set.seed(2)
statistic <- function(data, indices){
  d <- data[indices,]
  coef(lm(abs_diff ~ openness + agreeableness + emotional_stability + conscientiousness + extraversion + assigned_metric + assigned_condition, data = d))[2]
}
m2 = boot(dat1, statistic, R = 2000)
boot.ci(m2)

# compare among 1-4

t.test(d1$diff, d2$diff)
t.test(d1$diff, d3$diff)
t.test(d1$diff, d4$diff)
t.test(d2$diff, d3$diff)
t.test(d2$diff, d4$diff)
t.test(d3$diff, d4$diff)

wilcox.test(d1$diff, d2$diff)
wilcox.test(d1$diff, d3$diff)
wilcox.test(d1$diff, d4$diff)
wilcox.test(d2$diff, d3$diff)
wilcox.test(d2$diff, d4$diff)
wilcox.test(d3$diff, d4$diff)
