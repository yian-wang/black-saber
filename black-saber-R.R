library(tidyverse)
library(knitr)
library(kableExtra)
library(lme4)
library(mgcv)

# HIRING 

phase1 = read_csv("data/phase1-new-grad-applicants-2020.csv")
phase2 = read_csv("data/phase2-new-grad-applicants-2020.csv")
phase3 = read_csv("data/phase3-new-grad-applicants-2020.csv")
final_hires = read_csv("data/final-hires-newgrad_2020.csv")

sum(is.na(phase1))
sum(is.na(phase2)) 
sum(is.na(phase3))

round1_2 = full_join(phase1, phase2)
round1_2 = round1_2 %>% 
  #technical skills is only under hiring2, which means if an applicant's id has NA for this column, they did not move onto the next round
  mutate(next_round = !is.na(technical_skills)) %>%  
  mutate(next_round = as.integer(next_round)) %>% 
  mutate(gender = fct_relevel(gender, "Prefer not to say", after = 2)) %>% 
  select(-c(technical_skills, writing_skills, leadership_presence, speaking_skills)) #remove phase 2 data

# counts for total round 1 gender
kable(table(round1_2$gender), col.names = c("Gender", "Quantity"), caption = "Gender Count for Phase 1")

#response is binary so we use glm so we can use family = binomial
#normal linear model
hiring1_model = glm(next_round ~ cv + gpa + cover_letter + extracurriculars + work_experience, family = binomial(link = "logit"), data = round1_2)

# add fixed effect for gender
hiring1_gender_model = glm(next_round ~ cv + gpa + cover_letter + extracurriculars + work_experience + gender, data = round1_2, family = binomial(link = "logit"))
summary(hiring1_gender_model)
#let's see if we add an effect for team applied to - maybe the process for data and software are different?
hiring1_team_gender = glmer(next_round ~ cv + gpa + cover_letter + extracurriculars + work_experience + gender + (1 |team_applied_for),
                            data = round1_2, family = binomial(link = "logit"), nAGQ=0)
summary(hiring1_team_gender)

#compare the effect between gender only and gender (rand int) + random slope for team 
kable(signif(lmtest::lrtest(hiring1_gender_model, hiring1_team_gender),4), col.names = c("# Df", "Log Likelihod", "Df","Chi-squared", "P-value"), caption = "Log Likelihood Test for Random Effect of Team in Phase 1")

#test the simple model against the original model with no random intercept for gender...investigate if there is a difference
kable(signif(lmtest::lrtest(hiring1_model, hiring1_gender_model),4), col.names = c("# Df", "Log Likelihod", "Df","Chi-squared", "P-value"), caption = "Log Likelihood Test for Fixed Effect of Gender in Phase 1")
#chi^2 p value small for both of them which means there's no difference in models if u add effect for gender, meaning model doesn't change based on gender and therefore there is no evidence of there being bias in phase 1 of the hiring process.

round2_3 = full_join(phase2, phase3)
#remove phase 2 columns
round2_3 = round2_3 %>% 
  #interviewer rating only in phase 3, which means if an applicant's id has NA for this column, they did not move onto the next round
  mutate(next_round = !is.na(interviewer_rating_1)) %>%  
  mutate(next_round = as.integer(next_round)) %>% 
  mutate(gender = fct_relevel(gender, "Prefer not to say", after = 2)) %>% 
  select(-c(cover_letter, cv, gpa, extracurriculars, work_experience, interviewer_rating_1, interviewer_rating_2)) #remove phase 2 data

#get counts for each gender in round 2
kable(table(round2_3$gender), col.names = c("Gender", "Quantity"), caption = "Gender Count for Phase 2")

#normal linear model
hiring2_model = glm(next_round ~ technical_skills + writing_skills + leadership_presence + speaking_skills, family = binomial(link = "logit"), data = round2_3)
summary(hiring2_model)
# add fixed effect for gender
hiring2_gender_model = glm(next_round ~ technical_skills + writing_skills + leadership_presence + speaking_skills + gender, data = round2_3, family = binomial(link = "logit"))
summary(hiring2_gender_model)
#let's see if we add an effect for team applied to - maybe the process for data and software are different?
hiring2_team_gender = glmer(next_round ~ technical_skills + writing_skills + leadership_presence + speaking_skills + gender + (1 | team_applied_for),
                            data = round2_3, family = binomial(link = "logit"), nAGQ=0)
summary(hiring2_team_gender)

#compare the effect between gender only and gender (rand int) + random slope for team 
kable(signif(lmtest::lrtest(hiring2_gender_model, hiring2_team_gender),4), col.names = c("# Df", "Log Likelihod", "Df","Chi-squared", "P-value"), caption = "Log Likelihood Test for Random Effect of Team in Phase 2")
#seems like not significant effect... we can continue with our simpler model without the difference between teams
#test the simple model against the original model with no random intercept for gender and investigate if there is a difference
kable(signif(lmtest::lrtest(hiring2_model, hiring2_gender_model),4), col.names = c("# Df", "Log Likelihod", "Df","Chi-squared", "P-value"), caption = "Log Likelihood Test for Fixed Effect of Gender in Phase 2")
#high chi squared for both - we're good! no diff between model that seps gender 

p2confint = cbind(est = round(summary(
  hiring2_gender_model)$coef[,1],3),
  round(confint(hiring2_gender_model),3))
rownames(p2confint) = c("Baseline", "Technical Skills", "Writing Skills", "Leadership Presence", "Speaking Skills", "Woman", "Prefer not to Say")
kable(p2confint, col.names = c("Estimate", "2.5%", "97.5%"), caption = "95\\% Confidence Interval for GLM with Gender Effect")

#combined stats of ppl that applied in round 3
round3_combined = right_join(phase2, phase3)
round3_combined = round3_combined %>% 
  select(applicant_id, team_applied_for, gender,interviewer_rating_1,interviewer_rating_2)
#people from round 3 who were hired
round3_h_int = right_join(round3_combined, final_hires)
round3_h_int = round3_h_int %>% 
  mutate(hired = 1)
round3_hired = full_join(round3_combined, round3_h_int)
#remove phase 2 columns
round3_hired = round3_hired %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  mutate(avg_interview = (interviewer_rating_1 + interviewer_rating_2)/2) # we see that those with the highest avg score get hired so i'm going to consolidate them 

#split into data/software
round3_data = round3_hired %>% 
  filter(team_applied_for == "Data") 
# no more "prefer not to say" so we don't need to rearrange
#%>% 
#mutate(gender = fct_relevel(gender, "Prefer not to say", after = 2))
round3_software = round3_hired %>% 
  filter(team_applied_for == "Software") #%>% 
#mutate(gender = fct_relevel(gender, "Prefer not to say", after = 2))
#separate by gender
round3_woman = round3_hired %>% 
  filter(gender == "Woman")
round3_man = round3_hired %>% 
  filter(gender == "Man")

final_genders=table(round3_hired$hired==1, round3_hired$gender)
rownames(final_genders)[rownames(final_genders) == TRUE] = "Hired"
rownames(final_genders)[rownames(final_genders) == FALSE] = "Not Hired"
kable(final_genders, caption = "Gender Breakdown of Final Hires")

finalmodel = glm(hired ~ avg_interview, family = binomial(link='logit'), data = round3_hired)
#summary(finalmodel)
finalmodel2 = glm(hired ~ avg_interview + gender, family = binomial(link='logit'), data = round3_hired)
#summary(finalmodel2)
kable(signif(lmtest::lrtest(finalmodel, finalmodel2),4), col.names = c("# Df", "Log Likelihod", "Df","Chi-squared", "P-value"), caption = "Log Likelihood Test for Fixed Effect of Gender in Final Hires")


# PROMOS

black_saber_current_employees <- read_csv("data/black-saber-current-employees.csv")
roles = c("Entry-level", "Junior I", "Junior II", "Senior I", "Senior II", "Senior III", "Manager", "Director", "Vice president")
smallyears = c("2013 Q2", "2013 Q3", "2013 Q4", "2014 Q1", "2014 Q2", "2014 Q3", "2014 Q4", "2015 Q1", "2015 Q2", "2015 Q3", "2015 Q4")

promotionsbyfinancialq <- black_saber_current_employees %>% mutate("role_seniority" = factor(role_seniority, levels = roles)) %>% mutate("order" = as.numeric(role_seniority)/100) %>% filter(!(financial_q %in%smallyears))


totalemployees <- promotionsbyfinancialq %>% filter(financial_q %in% smallyears) %>% group_by(financial_q) %>% summarise(totals = n())

sumemployees <- sum(totalemployees$totals)
sumemployees

ggplot(data = promotionsbyfinancialq, aes(x = factor(order), fill = gender)) + 
  geom_bar(data = dplyr::filter(promotionsbyfinancialq, gender=="Woman")) + geom_bar(data = dplyr::filter(promotionsbyfinancialq, gender=="Prefer not to say")) +
  geom_bar(data = dplyr::filter(promotionsbyfinancialq, gender=="Man"),aes(y=..count..*(-1))) + 
  scale_x_discrete(labels = roles) +
  xlab("roles") +
  coord_flip() +
  theme(text = element_text(size=12), axis.text.y =  element_text(size = 10)) +
  scale_y_continuous(breaks=seq(-100,100,50),labels=abs(seq(-100,100,50))) + 
  scale_fill_brewer(palette="Dark2") +
  facet_wrap(~financial_q) + 
  labs(caption = "Gender distribution for each role and financial quarter", x = "Number of people in each role", y = "Roles")
ggsave("images/promotion_genderdist.png", width = 20, height = 10)

#want to count the number of times a person has been promoted 
library(lme4)
barcol <- c("FFC300", "#C45DF0", "A3F07C")
promotions <- promotionsbyfinancialq %>% mutate("role_seniority" = factor(role_seniority, levels = roles))%>% mutate(role_seniority = factor(role_seniority)) %>% mutate_if(is.factor, as.numeric) %>% group_by(employee_id) %>% arrange(financial_q) %>% mutate("promoted" = ifelse(role_seniority > lag(role_seniority), 1, 0)) %>% mutate(promoted = factor(promoted)) %>% mutate(gender = factor(gender)) %>% mutate(gender =  fct_relevel(gender, "Woman", after = 1)) %>% 
  mutate(leadership_for_level = factor(leadership_for_level)) %>% mutate("leadership_for_level" = factor(leadership_for_level, levels = c("Needs improvement", "Appropriate for level","Exceeds expectations"))) %>% mutate(promoted = replace_na(promoted,0))


promotion_by_gender <- promotions %>% group_by(gender) %>% summarise(numberofpromos = n()) %>% mutate(prop = numberofpromos/sum(numberofpromos))

df = cbind(promotion_by_gender, barcol)

totalpromo_int <- ggplot(promotions, aes(x = promoted, fill = gender)) + geom_bar(position = "fill") #+ scale_fill_manual("Legend", values = c("Man" = "FFC300", "Woman" = "#C45DF0", "Prefer not to say" = "#A3F07C"))
totalpromo <- totalpromo_int + labs(caption = "Promotions By Gender Proportions", x = "Promoted", y = "Proportion of Each Gender")
totalpromo
#ggsave("images/promotion_genderprop.png", width = 10, height = 8) 

#by team - important factor to consider
teampromo <- totalpromo + facet_wrap(~team, ncol = 4) #+ scale_fill_manual("Legend", values = c("Man" = "FFC300", "Woman" = "#C45DF0", "Prefer not to say" = "#A3F07C"))
teampromo
ggsave("images/teampromo.png", width = 10, height = 8)

#however, there are a lot of things that can effect this such as team, leadership, productivity, role seniority etc 
#tells us that a larger proportion of men 

promomodel1 <- glm(promoted ~ gender, family = binomial(link = "logit"), data = promotions) 
#summary(promomodel1)
info1 <- round(summary((promomodel1))$coefficients, 3)
rownames(info1) = c("Baseline", "Woman", "Prefer not to Say")
#this tells us that for genders that are not male, you are exp(-0.41) = 0.66. This tells us that the odds of women being promoted is 0.66 times less likely than men (prefer not to say not significant)
kable(info1, caption = "Model 1: Promotion and Gender Values") #col.names = c("# Df", "Log Likelihod", "Df","Chi-squared", "P-value"), caption = "Log Likelihood Test for Fixed Effect of Gender in Final Hires")

#but what if we control for the team they are in 
promomodel2 <- glm(promoted ~ gender + team, family = binomial(link = "logit"), data = promotions) 
info2 <- round(summary(promomodel2)$coefficients, 3) #adding team as a coeff had no signicant change to the model 
rownames(info2) = c("Baseline", "Woman", "Prefer not to say", "Data", "Design", "Legal and Financial", "Marketing and Sales", "Operations", "People and Talent", "Software")
kable(info2, caption = "Model 2: Promotion with Gender and Team Values")

#what about accounting for leadership? 
promomodel3 <- glm(promoted ~ gender + leadership_for_level, family = binomial(link = "logit"), data = promotions) 
info3 <- round(summary(promomodel3)$coefficients,3)
rownames(info3) = c("Baseline", "Woman", "Prefer not to say", "Leadership Appropriate for Level", "Leadership Exceeds Expectations")
kable(info3, caption = "Model 3: Promotion with Gender and Leadership Values")#also no significant change, gender is still a significant factor in being promoted

#what about productivity?
productivity = promotions %>% mutate(productivity = ifelse(productivity == 50, "Satisfactory", (ifelse(productivity > 50, "Better than expected", "Unproductive")))) %>% mutate(productivity = factor(productivity, levels = c("Unproductive", "Satisfactory", "Better than expected")))

promomodel4 <- glm(promoted ~ gender + productivity, family = binomial(link = "logit"), data = promotions) 
info4 <- round(summary(promomodel4)$coefficients,3)
rownames(info4) = c("Baseline", "Woman", "Prefer not to say", "Productivity")
kable(info4, caption = "Model 4: Promotion with Gender and Productivity Values") #productivity is significant but it does not change the the difference we see in women being promoted 

#what about role seniority? more role seniority could increase your chances of being promoted but at the same time in very high levels there are less spots 
promomodel5 <- glm(promoted ~ gender + productivity + role_seniority, family = binomial(link = "logit"), data = promotions) 
info5 <- round(summary(promomodel5)$coefficients, 3)
rownames(info5) = c("Baseline", "Woman", "Prefer not to say", "Productivity", "Role Seniority")
kable(info5, caption = "Model 5: Promotion with Gender, Productivity and Role Seniority")

# SALARY 

current_employees = read_csv("data/black-saber-current-employees.csv")
unique(current_employees$team) # what teams are there?
count(current_employees, gender) #counts for everything, put these into a table but also do this for the hiring section
# note that we need to add a random effect for ID

current_employees = current_employees %>% mutate(salary = str_remove_all(salary,'\\$')) %>% mutate(salary = str_remove_all(salary, ",")) %>% mutate(salary = as.numeric(salary)) %>% mutate(gender = factor(gender)) %>% mutate(gender = fct_relevel(gender, "Woman", after = 1))

#want to visualize mean salary for each financial quarter per gender - helpful table but idk how to visualize
genderprop <- current_employees%>% group_by(gender,financial_q) %>% summarise(gendercount = n(), meansalary = mean(salary)) 

salary_by_quarter_gender <- ggplot(genderprop, aes(x= financial_q, y = meansalary)) + geom_bar(aes(fill = gender), position = "dodge", stat = "identity", width = 0.7) + theme_minimal()+theme(axis.text.x = element_text(angle = 90)) #CHANGE COLOUR AND TITLES 
salary_by_quarter_gender

current_employees %>%
  ggplot(aes(x = gender, y = salary, colour = role_seniority)) +
  geom_boxplot() +
  facet_wrap(~role_seniority) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_blank()) +
  labs(x = "Sex", y = "Salary") #Change colours

current_employees %>%
  mutate(role_seniority = factor(role_seniority, c("Entry-level","Junior I","Junior II","Senior I","Senior II","Senior III","Manager", "Director","Vice president"))) %>% 
  ggplot(aes(x = role_seniority, y = productivity, colour = role_seniority)) +
  geom_boxplot() +
  #facet_wrap(~role_seniority) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_blank()) +
  labs(x = "Seniority Level", y = "Salary")

# i know we need a random effect for (1+leadership | role_seniority) BECAUSE the leadership is dependent on their seniority, so like... the seniority is the random effect, but then there is a fixed effect for leadership *within* that role....
# buttttt what model do we run? 
model_sal = lmer(salary~gender+(1|employee_id)+(1|team)+(1|role_seniority), data=current_employees)
summary(model_sal)
model_sal2 = lmer(salary~gender+(1|employee_id)+(1|role_seniority), data=current_employees)
summary(model_sal2)
model_sal3 = lmer(salary~(1|employee_id)+(1|team)+(1|role_seniority), data=current_employees)
summary(model_sal3)
model_sal4 = lmer(salary~gender+(1|employee_id)+(1|team), data=current_employees)
summary(model_sal4) #removed role seniority random effect... test for difference
lmtest::lrtest(model_sal, model_sal2)
#this shows us that salaries differ depending on teams, so we use the more complex model to investigate the diff between gender
lmtest::lrtest(model_sal3, model_sal)
#significaant result.... when we have a fixed effect for gender, then the models differ, which means that salary might be biased, holding these random effects 
lmtest::lrtest(model_sal, model_sal4) #ok there is a difference, between RE for seniority or not so it has to be something else affecting it... 
#these test tells use we should probably use model_sal 

#should we have a random slope with leadership level and role seniority? 
model_sal5 = lmer(salary~gender+(1|employee_id)+(1|team)+(1+ leadership_for_level|role_seniority), data=current_employees) #added random slope because leadership requirements are different for different roles 
lmtest::lrtest(model_sal, model_sal5) #this tells us that there is an interaction between leadership and role seniority --> significant use model_sal5 
#lastly, does productivity effect how much someone is paid? 
model_sal6= lmer(salary~gender+(1|employee_id)+(1|team)+(1+ leadership_for_level|role_seniority) + (1|productivity), data=current_employees) #productivity added as a RE since it is different per employee
lmtest::lrtest(model_sal5, model_sal6) #model_sal6 seems to be significant- use as final model 
final_sal_model <- model_sal6

#Looks like we have found the best model to describe our data, now we wanted to see how including gender vs not including it effects our model 
finalmodel_nogender <- lmer(salary ~ (1|employee_id)+(1|team)+(1+ leadership_for_level|role_seniority) + (1|productivity), data=current_employees)

lmtest::lrtest(final_sal_model,finalmodel_nogender)
#more complicated final model with gender as a fixed effect explains our data better. We have strong evidence against the null hypothesis that the simpler model without gender fits the data just as well 
summary(final_sal_model)

























