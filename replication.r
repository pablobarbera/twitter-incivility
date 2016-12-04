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
### APPENDING ALL DATA TOGETHER
######################################################

countries <- c("greece", "uk", "germany", "spain")
d <- c()
for (country in countries){
	df <- read.dta(paste0(country, ".dta"))
	d <- rbind(d, df)
}

# do not include users with missing followers count (mostly inactive accounts)
d <- d[!is.na(d$followers_count),]

######################################################
### TABLE 1
######################################################

pp <- aggregate(d$engaging_tweets*d$ntweets, 
	by=list(country=d$country), 
	FUN=sum, na.rm=TRUE)
pp$x <- (pp$x/aggregate(d$ntweets, 
	by=list(country=d$country), 
	FUN=sum, na.rm=TRUE)$x)
pp # engaging tweets

pp <- aggregate(d$impolite_mentions*d$ntweets, 
	by=list(country=d$country), 
	FUN=sum, na.rm=TRUE)
pp$x <- (pp$x/aggregate(d$ntweets, 
	by=list(country=d$country), 
	FUN=sum, na.rm=TRUE)$x)
pp # impolite tweets

pp <- aggregate(d$engaging_tweets, 
	by=list(party=d$party, country=d$country), 
	FUN=mean, na.rm=TRUE)
pp <- pp[order(pp$x),]
pp <- pp[!is.na(pp$x),]
tail(pp, n=10)

d[d$twitter=="Nigel_Farage",]

d[d$country=="Germany",][which.max(d$impolite_mentions[d$country=="Germany"]),]

######################################################
### FIGURE 1
######################################################

cand_resp <- read.csv("candidate_tweet_responses.csv", 
                       header = TRUE)

## data for geom_rangeframe
df <- data.frame(
  engaging_prob = c(0, 1),
  n_res = c(0, 0.8),
  country=rep(unique(cand_resp$country), each=2))

## Plot direct response "count" (attracted) dep. on engaging
## Poisson
ggplot(cand_resp, 
  aes(x = engaging_prob, y = n_res, group=country)) +
  stat_smooth(method = "glm", method.args = list(family = "poisson"),
    aes(color=country)) +
  theme_tufte() +
  xlab("Probability of engaging tweet (candidate)") +
  ylab("Average number of responses (by public)") +
  #scale_y_continuous(limits=c(0, 1)) + 
  facet_wrap(~country, scales = "free") +
  scale_color_brewer("", palette = "Set1") +
  theme(legend.position="none") +
  geom_rangeframe(data=df)

ggsave(file="figure1.pdf", height = 4, width = 8)

######################################################
### FIGURE 2
######################################################

pd <- data.frame(
	value = c(d$engaging_tweets, d$impolite_mentions),
	country = c(d$country, d$country),
	type = rep(c("Engaging (based on candidates)", "Impolite (based on public)"),
		each=nrow(d)))

p <- ggplot(pd, aes(y=country, x=value))
pq <- p + geom_jitter(position=position_jitter(height=.50), size=.75,
	aes(color=country, shape=country)) +
    scale_x_continuous("Estimated proportion of tweets in each category",
    	label=percent) +
    facet_wrap(~type) +
    theme_tufte() + 
    scale_color_brewer("", palette = "Set1") +
    scale_shape_discrete("") +
    geom_rangeframe(sides="bl", data=data.frame(value=c(0, 1), country=NA)) +
    theme(axis.ticks.y=element_blank(), axis.title.y = element_blank()) +
      theme(#axis.ticks.x = element_blank(), 
        #axis.ticks.y = element_blank(),
        legend.position = "none",
        legend.margin=unit(0, "cm"))
pq

ggsave(file="figure2.pdf", height = 3.25, width = 8)

######################################################
### TABLE 2
######################################################

# Model 1
reg1 <- lmer(impolite_mentions*100 ~ 1 + 
			I(engaging_tweets*100) +
			factor(country) +
			(1 | party),
			data = d,
			weights = d$ntweets)

# Model 2
reg2 <- lmer(impolite_mentions*100 ~ 1 + 
			I(engaging_tweets*100) +
			incumbent + factor(electability) +
			male + 
			log(followers_count+1) + 
			I(votenl/100) + 
			pmnl + 
			factor(country) +
			(1 | party),
			data = d,
			weights = d$ntweets)

# Model 3
reg3 <- lmer(impolite_mentions*100 ~ 1 + 
			I(engaging_tweets*100) +
			incumbent + factor(electability) +
			male + 
			log(followers_count+1) + 
			I(votenl/100) + 
			pmnl + 
			ideology + 
			eu_position +
			factor(country) +
			(1 | party),
			data = d,
			weights = d$ntweets)

# Model 4
reg4 <- lmer(impolite_mentions*100 ~ 1 + 
			I(engaging_tweets*100)*factor(country) +
			incumbent + factor(electability) +
			male + 
			log(followers_count+1) + 
			I(votenl/100) + 
			pmnl + 
			(1 | party),
			data = d,
			weights = d$ntweets)

# Model 5
reg5 <- lmer(impolite_mentions * morality_mentions*100 ~ 1 + 
			I(engaging_tweets*100) +
			factor(country) +
			incumbent + factor(electability) +
			male + 
			log(followers_count+1) + 
			I(votenl/100) + 
			pmnl + 
			(1 | party),
			data = d,
			weights = d$ntweets)

# main marginal effect in reg 2
mean(d$engaging_tweets, na.rm=TRUE) # average
sd(d$engaging_tweets, na.rm=TRUE) * 100 # sd 
summary(d$engaging_tweets*100) # quantile: from 25% to 75% == 25 points
25 * mean(coef(reg2)[[1]][,2]) # expected effect for that increase
(0.25 * mean(coef(reg2)[[1]][,2]) * 100)/ sd(d$impolite_mentions, na.rm=TRUE) # magnitude of effect, in % of std dev in impolite mentions

# compute marginal effect in Greece, Spain, and UK
beta.hat <- coef(reg4)[[1]][1,]
cov <- vcov(reg4)

# marginal effect GERMANY
(dy.dx <- as.numeric(beta.hat["I(engaging_tweets * 100)"]))
(se.dy.dx <- sqrt(cov["I(engaging_tweets * 100)", "I(engaging_tweets * 100)"]))
dnorm(dy.dx / se.dy.dx)

# marginal effect GREECE
(dy.dx <- as.numeric(beta.hat["I(engaging_tweets * 100)"] + beta.hat["I(engaging_tweets * 100):factor(country)Greece"]))
# standard error of marginal effect
(se.dy.dx <- sqrt(cov["I(engaging_tweets * 100)", "I(engaging_tweets * 100)"] + 
	cov["I(engaging_tweets * 100):factor(country)Greece", "I(engaging_tweets * 100):factor(country)Greece"] + 
	2*cov["I(engaging_tweets * 100)", "I(engaging_tweets * 100):factor(country)Greece"]))
dnorm(dy.dx / se.dy.dx)

# marginal effect SPAIN
(dy.dx <- as.numeric(beta.hat["I(engaging_tweets * 100)"] + beta.hat["I(engaging_tweets * 100):factor(country)Spain"]))
# standard error of marginal effect
(se.dy.dx <- sqrt(cov["I(engaging_tweets * 100)", "I(engaging_tweets * 100)"] + 
	cov["I(engaging_tweets * 100):factor(country)Spain", "I(engaging_tweets * 100):factor(country)Spain"] + 
	2*cov["I(engaging_tweets * 100)", "I(engaging_tweets * 100):factor(country)Spain"]))
dnorm(dy.dx / se.dy.dx)

# marginal effect UK
(dy.dx <- as.numeric(beta.hat["I(engaging_tweets * 100)"] + beta.hat["I(engaging_tweets * 100):factor(country)UK"]))
# standard error of marginal effect
(se.dy.dx <- sqrt(cov["I(engaging_tweets * 100)", "I(engaging_tweets * 100)"] + 
	cov["I(engaging_tweets * 100):factor(country)UK", "I(engaging_tweets * 100):factor(country)UK"] + 
	2*cov["I(engaging_tweets * 100)", "I(engaging_tweets * 100):factor(country)UK"]))
dnorm(dy.dx / se.dy.dx)

# main marginal effect in reg 5
mean(d$engaging_tweets, na.rm=TRUE) # average
sd(d$engaging_tweets, na.rm=TRUE) * 100 # sd 
summary(d$engaging_tweets*100) # quantile: from 25% to 75% == 25 points
25 * mean(coef(reg5)[[1]][,2]) # expected effect for that increase
(0.25 * mean(coef(reg5)[[1]][,2]) * 100)/ sd(d$impolite_mentions * d$morality_mentions, na.rm=TRUE) # magnitude of effect, in % of std dev in impolite mentions

# output: regression table in latex format
texreg::texreg(list(reg1, reg2, reg3, reg4, reg5), digits = 2,
    custom.coef.names=c('Intercept', "\\% Engaging tweets sent", 
		"Greece (dummy)", "Spain (dummy)", "UK (dummy)",
		"Candidate is incumbent", "Viability: Safe", "Viability: Unpromising",
		"Candidate is male", "log(count of followers)", "Vote share (national)",
		"Prime minister (national)", 
		"LR position",
		"EU position", "Engaging $\\times$ Greece",
		 "Engaging $\\times$ Spain",  "Engaging $\\times$ UK"),
          dcolumn=TRUE, stars=c(.01, .05, .10))

######################################################
### FIGURE 3
######################################################

## we assume that no responses implies 0 impolite responses
cand_resp2 <- cand_resp
cand_resp2$r_imp[is.na(cand_resp2$r_imp)] <- 0

## Plot direct response "count" (attracted) dep. on engaging
## Poisson
df <- data.frame(
  r_imp = c(0, .03, .025, .075, 0, .006, 0, .015),
  engaging_prob = c(0, 1),
  country=rep(sort(unique(cand_resp2$country)), each=2))

ggplot(cand_resp2, aes(y = r_imp, x = engaging_prob)) +
  stat_smooth(method = "glm", colour = "black") +
  theme_tufte() +
  xlab("Probability of tweet being engaging") +
  ylab("Probability of impolite response tweet") +
  facet_wrap(~country, scales = "free") +
  geom_rangeframe(data=df)

ggsave(file="figure3.pdf", height = 4, width = 8)

######################################################
### TABLE 4
######################################################

de <- rio::import("germany.dta")
uk <- rio::import("uk.dta")
esp <- rio::import("spain.dta")
gr <- rio::import("greece.dta")
elect <- rbind(de[, c("twitter", "electability")],
               uk[, c("twitter", "electability")],
               esp[, c("twitter", "electability")],
               gr[, c("twitter", "electability")])

cand_resp1 <- merge(cand_resp, elect, by = "twitter")

## grand mean center party level predictors
cand_resp1$votenl_cent=cand_resp1$votenl/100-mean(cand_resp1$votenl/100)
cand_resp1$ideology_cent=cand_resp1$ideology-mean(cand_resp1$ideology, na.rm=T)
cand_resp1$eu_position_cent=cand_resp1$eu_position-mean(cand_resp1$eu_position, na.rm=T)

## we assume that no responses implies 0 impolite responses
cand_resp2 <- cand_resp1
cand_resp2$r_imp[is.na(cand_resp2$r_imp)] <- 0

# do not include users with missing followers count (mostly inactive accounts)
cand_resp2 <- cand_resp2[!is.na(cand_resp2$followers_count),]

###model 1, adding a random intercept for parties and using grand mean centered vote share
m.1_new <- lmer(r_imp*100 ~ 1 + 
              I(engaging_prob*100) + 
              as.factor(country) +
              (1 + engaging_prob | screen_name)+
                (1 | party), 
            data = cand_resp2)

summary(m.1_new)

###model 2, adding a random intercept for parties and using grand mean centered vote share
m.2_new <- lmer(r_imp*100 ~ 1 + 
              I(engaging_prob*100) + 
              incumbnl +
              log(followers_count + 1) +
              male + 
              pmnl + 
              electability +
                votenl_cent +
              as.factor(country) +
              (1 + engaging_prob | screen_name)+
                (1 | party), 
            data = cand_resp2)

summary(m.2_new)

###model 3, adding a random intercept for parties and using grand mean centered vote share
m.3_new <- lmer(r_imp*100 ~ 1 + 
              I(engaging_prob*100) + 
              incumbnl +
              log(followers_count + 1) +
              male + 
              pmnl + 
              electability +
                votenl_cent  +
              as.factor(country) * I(engaging_prob*100) +
                (1 + engaging_prob | screen_name)+
                (1 | party), 
            data = cand_resp2)

summary(m.3_new)


##controliing for ideology and party position
m.4_new <- lmer(r_imp*100 ~ 1 + 
                  I(engaging_prob*100) + 
                  incumbnl +
                  log(followers_count + 1) +
                  male + 
                  pmnl + 
                  electability +
                  votenl_cent  +
                  as.factor(country) +
                  ideology_cent +
                  eu_position_cent+
                  (1 + engaging_prob | screen_name)+
                  (1 | party), 
                data = cand_resp2)

summary(m.4_new)

# main marginal effect in model 2
mean(cand_resp2$engaging_prob, na.rm=TRUE) # average
sd(cand_resp2$engaging_prob, na.rm=TRUE) * 100 # sd 
summary(cand_resp2$engaging_prob*100) # quantile: from 25% to 75% == 60 points
61 * mean(coef(m.2_new)[[1]][,3]) # expected effect for that increase
(61 * mean(coef(m.2_new)[[1]][,3]))/ sd(cand_resp2$r_imp, na.rm=TRUE) # magnitude of effect, in % of std dev in impolite mentions

# compute marginal effect in Greece, Spain, and UK
beta.hat <- coef(m.3_new)[[1]][1,]
cov <- vcov(m.3_new)

# marginal effect: GERMANY
(dy.dx <- as.numeric(beta.hat["I(engaging_prob * 100)"]))
(se.dy.dx <- sqrt(cov["I(engaging_prob * 100)", "I(engaging_prob * 100)"]))
dnorm(dy.dx / se.dy.dx)

# marginal effect: GREECE
(dy.dx <- mean(beta.hat[,"I(engaging_prob * 100)"]) + mean(beta.hat[,"I(engaging_prob * 100):as.factor(country)Greece"]))
# standard error of marginal effect
(se.dy.dx <- sqrt(cov["I(engaging_prob * 100)", "I(engaging_prob * 100)"] + 
  cov["I(engaging_prob * 100):as.factor(country)Greece", "I(engaging_prob * 100):as.factor(country)Greece"] + 
  2*cov["I(engaging_prob * 100)", "I(engaging_prob * 100):as.factor(country)Greece"]))
dnorm(dy.dx / se.dy.dx)

# marginal effect: SPAIN
(dy.dx <- mean(beta.hat[,"I(engaging_prob * 100)"]) + mean(beta.hat[,"I(engaging_prob * 100):as.factor(country)Spain"]))
# standard error of marginal effect
(se.dy.dx <- sqrt(cov["I(engaging_prob * 100)", "I(engaging_prob * 100)"] + 
  cov["I(engaging_prob * 100):as.factor(country)Spain", "I(engaging_prob * 100):as.factor(country)Spain"] + 
  2*cov["I(engaging_prob * 100)", "I(engaging_prob * 100):as.factor(country)Spain"]))
dnorm(dy.dx / se.dy.dx)

# marginal effect: UK
(dy.dx <- mean(beta.hat[,"I(engaging_prob * 100)"]) + mean(beta.hat[,"I(engaging_prob * 100):as.factor(country)UK"]))
# standard error of marginal effect
(se.dy.dx <- sqrt(cov["I(engaging_prob * 100)", "I(engaging_prob * 100)"] + 
  cov["I(engaging_prob * 100):as.factor(country)UK", "I(engaging_prob * 100):as.factor(country)UK"] + 
  2*cov["I(engaging_prob * 100)", "I(engaging_prob * 100):as.factor(country)UK"]))
dnorm(dy.dx / se.dy.dx)