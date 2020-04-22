library(lme4)
data = read.csv("./TAGGED_CONTEXTS.csv")[,-1]
# glm with only fixed effects
fit0 <- glm(social~gender*race*bible, data=data, family=binomial)
fit1 <- glm(social~(gender+race+bible)^2, data=data, family=binomial)
fit2 <- glm(social~gender+race+bible+race:bible+gender:bible, data=data, family=binomial)
fit3 <- glm(social~gender+race+bible+race:bible, data=data, family=binomial)
anova(fit3,fit2,fit1,fit0,test="Chisq")
fit = glm(social~race+gender+bible,data=data,family=binomial)
m1 <- glmer(social~(gender+race+bible)^2+(1|title), data=data, family=binomial, nAGQ=0)
m2 <- glmer(social~gender+race+bible+race:bible+gender:bible+(1|title), data=data, family=binomial, nAGQ=0)
m3 <- glmer(social~gender+race+bible+race:bible+(1|title), data=data, family=binomial, nAGQ=0)
m <- glmer(social~gender+race+bible+(1|title), data=data, family=binomial, nAGQ=0)
summary(m)
summary(m3)

m3.coef = summary(m3)$coef[,1]
for (race in c(0,1)) {
	for (gender in c(0,1)) {
		for (bible in c(0,1)) {
			odds = exp(sum(m3.coef*c(1,race,gender,bible,race*bible)))
			cat(race,gender,bible,odds/(1+odds),1/(1+odds),odds,'\n')
		}
	}
}

score = coef(m3)$title
data$score = numeric(nrow(data))
for (title in unique(data$title)) {
	data$score[data$title==title] = score[title,1]
}

titles = unique(data$title)
data.unique = data.frame(title=titles,
                         race=factor(rep("white",length(titles)),levels=c("white","black")),
                         gender=factor(rep("female",length(titles)),levels=c("female","male")),
                         bible = factor(rep("yes", length(titles)), levels = c("yes", "no")),
                         score=numeric(length(titles)))
for (i in 1:length(titles)) {
	data.unique$race[i] = ifelse(data$race[data$title==titles[i]][1]==0,"white","black")
	data.unique$gender[i] = ifelse(data$gender[data$title==titles[i]][1]==0,"female","male")
	data.unique$bible[i] = ifelse(data$bible[data$title==titles[i]][1]==0,"yes","no")
	data.unique$score[i] = score[titles[i],1]
}

write.csv(data.unique,file="SOCIAL_SCORES_OUTPUT.csv",row.names=FALSE)

asdf <- read.csv("SOCIAL_SCORES_OUTPUT.csv")