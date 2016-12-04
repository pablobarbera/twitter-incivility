setwd("~/Dropbox/research/twitter-trolls/replication")

# loading libraries
library(foreign)
library(lme4)
library(ggplot2)
library(scales)
library(ggthemes)
library(texreg)
library(rio)

options(stringsAsFactors=F)

######################################################
######################################################

cand_resp <- read.csv("candidate_tweet_responses.csv", 
                       header = TRUE)

## we assume that no responses implies 0 impolite responses
cand_resp2 <- cand_resp
cand_resp2$r_imp[is.na(cand_resp2$r_imp)] <- 0

# restricting dataset
cand_resp2 <- cand_resp2[,c("r_imp", "engaging_prob", "country")]

## Plot direct response "count" (attracted) dep. on engaging
## Poisson
df <- data.frame(
  r_imp = c(0, .03, 0, .15, 0, .008, 0, .020),
  engaging_prob = c(0, 1),
  country=rep(sort(unique(cand_resp2$country)), each=2))

cand_resp2$eng <- floor((cand_resp2$engaging_prob*100)/5)
agg <- aggregate(r_imp ~ eng + country, data=cand_resp2, mean)
agg$size <- aggregate(r_imp ~ eng + country, 
  data=cand_resp2, length)$r_imp
agg$engaging_prob <- ((agg$eng * 5)/100)


ggplot(cand_resp2, aes(y = r_imp, x = engaging_prob)) +
  stat_smooth(method = "lm", colour = "black") +
  theme_tufte() +
  geom_point(data=agg, aes(size=size, color=country), alpha=.33) +
  xlab("Probability that politician's tweet is engaging") +
  ylab("Average index of impoliteness in response to candidate's tweet") +
  facet_wrap(~country, scales = "free") +
  scale_size_continuous("Number of\ntweets in bin") +
  scale_color_discrete("Country") +
  guides(color = guide_legend(override.aes = list(size = 8))) +
  geom_rangeframe(data=df)

ggsave(file="figure-MC.pdf", height=6, width=8)
ggsave(file="figure-MC.png", height=6, width=8)




