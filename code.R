barplot(frequency_table_age,main="Different age groups involved",xlab="Age Groups",ylab="No.of students",col="skyblue")
frequency_table_age=table(career$a.Age)
frequency_table_age
frequency_table_educationlevel=table(career$b.Education.level)
frequency_table_educationlevel
pie_education=round((frequency_table_educationlevel/sum(frequency_table_educationlevel)*100),1)
pie_education
pie(frequency_table_educationlevel,main="Education Levels",labels=pie_education,col=legend_colors_education)
legend_colors_education=c("pink","lightgreen")
legend_labels=c("PG","UG")
legend("topright",legend=legend_labels,fill=legend_colors_education,cex=0.75)
frequency_table_program=table(career$c.Which.program.have.you.opted.or.doing)
frequency_table_program
pie_percent=round((frequency_table_program/sum(frequency_table_program)*100),1)
pie_percent
legend_colors=rainbow(length(frequency_table_program))
legend_colors
legend_labels=c("M.Sc(physical sciences)","M.Sc(Life sciences)","B.Sc(Life sciences)","B.Sc(Physical sciences)","B.Com","BBA","BA","M.Com","MBA","MCA")
legend_labels
pie(frequency_table_program,main="Various programs",labels=pie_percent,col=legend_colors)
legend("topright",legend=legend_labels,fill=legend_colors,cex=0.50)
frequency_table_reason=table(career$d.Why.did.you.opt.for.the.program)
frequency_table_reason
barplot(frequency_table_reason,main="Reasons for choosing the Programs",xlab="Reasons",ylab="Count",col=legend_colors1,names.arg=plot_labels,ylim=c(0,100))
legend_colors1=c("green","blue","red","orange")
legend_colors1
plot_labels=c("Fascinated by the demand","Interested in the program","Family pressure","Well wishers advised")
plot_labels
frequency_table_13public=table(career$l.Which.sector.do.you.prefer.to.work.in)
frequency_table_13public
lcf=cumsum(frequency_table_13public)
lcf
lcf1=c(0,lcf)
lcf1
rf=rev(frequency_table_13public)
rf
cf=cumsum(lcf1)
cf
gcf=rev(cf)
gcf
gcf1=c(gcf,0)
gcf1
lbx=seq(100,200,50)
lbx
ubx=seq(100,200,50)
ubx
plot(ubx,lcf1,type="o",xlim=c(100,200),main="ogive",xlab="sector",ylab="count",lwd=2)
lines(lbx,gcf,type="o",xlim=c(100,200),lwd=2)
frequency_table_15=table(career$n.Which.jobs.are.you.interested.in.the.Public.sector)
frequency_table_15
frequency_table_private=table(career$p.Which.jobs.are.you.interested.in.the.Private.Sector.)
frequency_table_private
plot(frequency_table_15,frequency_table_private,xlab="sector",ylab="jobs",xlim=c(1,7),ylim=c(1,7))
plot(frequency_table_private,type="o",col="green",xlab="jobs",ylab="count")
plot(frequency_table_15,type="o",col="red",xlab="jobs",ylab="count")
frequency_table_happy=table(career$e.How.happy.are.you.with.your.chosen.program)
frequency_table_happy
percent_happy=round((frequency_table_happy/sum(frequency_table_happy)*100),1)
percent_happy
pie(frequency_table_happy,main="Level of happiness",labels=percent_happy,col=legend_colours_happy)
legend_colours_happy=c("purple","pink","yellow")
legend_colours_happy
legend_labels_happy=c("Moderately happy","Not at all happy","Very happy")
legend_colours_happy
legend_labels_happy
legend("topright",legend=legend_labels_happy,fill=legend_colours_happy,cex=0.75)
frequency_table_13=table(career$l.Which.sector.do.you.prefer.to.work.in)
frequency_table_13
public=65
public
plot(public,type="o",col="red",xlab="count",ylab="sectors",main="sectors")
private=84
private
lines(private,type="o",col="blue")
sectors=c("public sector","private sector")
sectors
age=career$a.Age
age
age1=c("17-19","20-22","23-25")
age1
public_sector=c(53,12)
public_sector
ug=c(53,51)
ug
private_sector=c(51,30)
private_sector
pg=c(12,30)
pg
plot(public_sector,ug,main="scatter plot",xlab="public sector",ylab="ug",xlim=c(10,65),ylim=c(10,65),col="red")
points(private_sector,pg,col="blue",xlab="private sector",ylab="pg")
####stacked bar graph between sectors and education
colors=c("green","orange","brown")
colors
sector=c("Public sector","Private sector")
sector
education=c("UG","PG")
education
values=matrix(c(53,12,51,30),nrow=2,ncol=2,byrow=TRUE)
values
barplot(values,main="Education levels and Sectors preferred",names.arg=education,xlab="Education levels",ylab="Sectors",col=colors,ylim=c(0,130))
legend("topleft",sector,cex=0.7,fill=colors)
reasons_count=c(55,39,29,50,8)
reasons_count
reasons=c("Passion for Service","Good pay","Stress free life","Security of job","Other")
reasons
barplot(reasons_count,main="Reasons for choosing Public Sector",xlab="Reasons",ylab="Count",ylim=c(0,80),col="purple",names.arg=reasons,border=TRUE)
reasons1_count=c(26,82,26,61,9)
reasons1_count
reasons1=c("Glamorous","Availability of jobs","Less Competition","More money","other")
reasons1
barplot(reasons1_count,main="Reasons for choosing Private Sector",xlab="Reasons",ylab="Count",ylim=c(0,100),col="yellow",names.arg=reasons1,border=TRUE)
plot(career$l.Which.sector.do.you.prefer.to.work.in,type="o",col="red",xlab="",ylab="",main="")
jobs_count=c(13,13,16,35,36,56,12)
jobs_count
jobs=c("Army","Navy","Air force","Banking","UPSC","State level","other")
jobs
plot(jobs_count,type="o",col="green",xlab="jobs",ylab="count",main="jobs",names.arg=jobs)
####line graph of age and sectors choosen
seventeen=c(45,39)
seventeen
twenty=c(18,43)
twenty
twentythree=c(2,2)
twentythree
names=c("public","private")
names
plot(valuesa,type="o",names.arg=names,xlab="sectors",ylab="age")
valuesa=matrix(c(45,39,18,43,2,2),nrow=3,ncol=2,byrow=TRUE)
valuesa
cor(career$f.Did.your.gender.influence.your.decision.in.selecting.this.program,career$l.Which.sector.do.you.prefer.to.work.in)
cor
gender_numeric=table(career$f.Did.your.gender.influence.your.decision.in.selecting.this.program)
gender_numeric
sectorchoosing_numeric=table(career$l.Which.sector.do.you.prefer.to.work.in)
sectorchoosing_numeric
cor(gender_numeric,sectorchoosing_numeric)
cor
education_numeric=table(career$b.Education.level)
education_numeric
########correlation between student satisfaction and skill enhancement 
satisfaction_numeric=table(career$e.How.happy.are.you.with.your.chosen.program)
satisfaction_numeric
sec_numeric=table(career$h.How.often.do.you.take.part.in.various.skill.enhancement.programs.offered.or.organized.in.your.college.or.outside)
sec_numeric
cor(satisfaction_numeric,sec_numeric)
plot(age1,x,col="blue",main="sectors",abline(lm(x~age1)),cex=1.3,pch=16,xlab="public",ylab="age")
x=c(45,18,2)
x
age_numeric=sapply()
########correlation between student satisfaction and skill enhancement 
satisfaction_numeric=table(career$e.How.happy.are.you.with.your.chosen.program)
satisfaction_numeric
sec_numeric=table(career$h.How.often.do.you.take.part.in.various.skill.enhancement.programs.offered.or.organized.in.your.college.or.outside)
sec_numeric
cor(satisfaction_numeric,sec_numeric)
cor.test(satisfaction_numeric,sec_numeric)
internship=table(career$j.Are.you.doing.an.internship.program.or.did.you.do.any)
internship
inline=table(career$k.Is.your.program.more.inline.with.the.emerging.technologies.and.advancements.in.your.field)
inline
cor(internship,inline)
###cor between gender influence and inline with emerging technologies
gendereffect=table(career$f.Did.your.gender.influence.your.decision.in.selecting.this.program)
gendereffect
cor(gendereffect,inline)
cor.test(gendereffect,inline)
cor(career$d.Why.did.you.opt.for.the.program,career$e.How.happy.are.you.with.your.chosen.program)
optednumeric=as.numeric(factor(career$d.Why.did.you.opt.for.the.program,levels=c("I.am.interested.in.the.program.","My.family.pressurised.me.","My.well.wishers.advised.me.","I.am.fascinated.by.the.demand.of.my.program.")))
optednumeric
happynumeric=as.numeric(factor(career$e.How.happy.are.you.with.your.chosen.program,levels=c("Very.happy","Moderately.happy","Not.at.all")))
happynumeric
cor(optednumeric,happynumeric)
cor.test(optednumeric,happynumeric)
internshipnumeric=as.numeric(factor(career$j.Are.you.doing.an.internship.program.or.did.you.do.any,levels=c("Yes","No")))
internshipnumeric
inlinenumeric=as.numeric(factor(career$k.Is.your.program.more.inline.with.the.emerging.technologies.and.advancements.in.your.field,levels=c("Totally","Partially","Not.sure")))
inlinenumeric
cor(internshipnumeric,inlinenumeric)
cor.test(internshipnumeric,inlinenumeric)
lm(happynumeric~optednumeric)
plot(optednumeric,happynumeric)
A=lm(happynumeric~optednumeric)
A
summary(A)
plot(A)
plotlm(happynumeric~optednumeric))
program.opted.reason=as.numeric(factor(career$d.Why.did.you.opt.for.the.program,levels=c("I.am.interested.in.the.program.","My.family.pressurised.me.","My.well.wishers.advised.me.","I.am.fascinated.by.the.demand.of.my.program."))) 
happiness=as.numeric(factor(career$e.How.happy.are.you.with.your.chosen.program,levels=c("Very.happy","Moderately.happy","Not.at.all")))
program.opted.reason  
happiness
B=lm(happiness~program.opted.reason)
plot(lm(happiness~program.opted.reason))
summary(B)
sec_numeric
sec=as.numeric(factor(career$h.How.often.do.you.take.part.in.various.skill.enhancement.programs.offered.or.organized.in.your.college.or.outside,levels=c("Always","Sometimes","Never")))
sec
C=lm(sec~happiness)
C
summary(C)
plot(C)
##############regression
reason_for_choosing_the_program.=as.numeric(factor(career$d.Why.did.you.opt.for.the.program,levels=c("I.am.interested.in.the.program.","My.family.pressurised.me.","My.well.wishers.advised.me.","I.am.fascinated.by.the.demand.of.my.program.")))
reason_for_choosing_the_program.
happiness_level.=as.numeric(factor(career$e.How.happy.are.you.with.your.chosen.program,levels=c("Very.happy","Moderately.happy","Not.at.all")))

happiness_level.
lm(career$d.Why.did.you.opt.for.the.program,career$e.How.happy.are.you.with.your.chosen.program)
=c(6,6,5,6,6,5,6,6,6,6,5,6,5,6,6,6,5,6,6,5,6,7,5,5,6,5,5,6,6,6,5,5,6,6,6,6,5,6,6,5,6,6,5,5,6,5,6,6,5,6)
lm(happiness_level.~reason_for_choosing_the_program.)
plot(reason_for_choosing_the_program.,happiness_level.,col="green",pch=16,cex=1,xlab="Reasons for choosing the program",ylab="Happiness level")
abline(lm(happiness_level.~reason_for_choosing_the_program.))
###################333
#######regression 1
# Load required libraries
library(ggplot2)

# Create a data frame with your data
data <- data.frame(
  opt_reason = c("interested", "interested", "interested", "advised", "interested", "interested", "advised", "pressurised", "interested", "pressurised", "interested", "interested", "interested", "interested", "interested", "advised", "interested", "interested", "fascinated", "interested", "interested", "pressurised", "interested", "interested", "advised", "fascinated", "advised", "interested", "interested", "fascinated", "fascinated", "fascinated", "interested", "advised", "interested", "pressurised", "interested", "interested", "fascinated", "fascinated", "fascinated", "interested", "interested", "interested", "advised", "interested", "interested", "pressurised", "interested", "interested", "fascinated", "fascinated", "fascinated", "interested", "advised", "interested", "advised", "fascinated", "advised", "interested", "interested", "advised", "interested", "interested", "interested", "interested", "interested", "interested", "interested", "interested", "advised", "not_interested", "not_interested", "interested", "interested", "interested", "pressurised", "pressurised", "fascinated", "fascinated", "interested", "fascinated", "interested", "interested", "interested", "fascinated", "interested", "interested", "fascinated", "interested", "interested", "interested", "interested", "interested", "interested", "fascinated", "interested", "interested", "fascinated", "interested", "interested", "interested", "interested", "interested", "interested", "interested", "interested", "fascinated", "interested", "fascinated", "advised", "interested", "interested", "interested", "fascinated", "interested", "interested", "interested", "interested", "interested", "interested", "interested", "interested", "fascinated", "interested", "not_interested", "not_interested", "interested", "interested", "advised", "interested", "interested"),
  happiness = c("moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "not_at_all", "very_happy", "very_happy", "moderately_happy", "very_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "very_happy", "moderately_happy", "very_happy", "very_happy", "moderately_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "very_happy", "very_happy", "moderately_happy", "very_happy", "very_happy", "moderately_happy", "very_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "very_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "very_happy", "not_at_all", "not_at_all", "moderately_happy", "moderately_happy", "very_happy", "not_at_all", "not_at_all", "moderately_happy", "very_happy", "very_happy", "very_happy", "moderately_happy", "very_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "moderately_happy", "very_happy", "moderately_happy", "not_at_all", "not_at_all", "very_happy", "very_happy", "moderately_happy", "very_happy", "very_happy")
)

# Encode categorical variables as factors
data$opt_reason <- factor(data$opt_reason)
data$happiness <- factor(data$happiness, levels = c("not_at_all", "moderately_happy", "very_happy"))

# Fit logistic regression model
model <- glm(happiness ~ opt_reason, data = data, family = binomial(link = "logit"))

# Predict probabilities
data$predicted_prob <- predict(model, type = "response")

# Plot the predicted probabilities against the independent variable
ggplot(data, aes(x = opt_reason, y = predicted_prob, fill = happiness)) +
  geom_boxplot() +
  labs(x = "Reason for opting the program", y = "Predicted Probability", fill = "Happiness") +
  ggtitle("Logistic Regression Plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
data1=data.frame(program=career$d.Why.did.you.opt.for.the.program,happy=career$e.How.happy.are.you.with.your.chosen.program)
data1
data1$program=factor(data1$program)
data1$program
data1$happy=factor(data1$happy)
data1$happy
model=glm(happy~program,data=data1, family = binomial(link = "logit"))
model
data1$predicted_prob <- predict(model, type = "response")
data1$predicted_prob
ggplot(data1, aes(x =program, y = predicted_prob, fill = happiness)) +
  geom_boxplot() +
  labs(x = "Reason for opting the program", y = "Predicted Probability", fill = "Happiness") +
  ggtitle("Logistic Regression Plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################3333
# Load required libraries
library(ggplot2)

# Create a data frame with your data
data1 <- data.frame(
  program = career$d.Why.did.you.opt.for.the.program,
  happy = career$e.How.happy.are.you.with.your.chosen.program
)

# Convert variables to factors
data1$program <- factor(data1$program)
data1$happy <- factor(data1$happy)

# Fit logistic regression model
model <- glm(happy ~ program, data = data1, family = binomial(link = "logit"))

# Predict probabilities
data1$predicted_prob <- predict(model, type = "response")

# Plot the predicted probabilities against the independent variable
ggplot(data1, aes(x = program, y = predicted_prob, fill = happy)) +
  geom_boxplot() +
  labs(x = "Reason for opting the program", y = "Predicted Probability", fill = "Happiness") +
  ggtitle("Logistic Regression Plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############
# Load required libraries
library(ggplot2)

# Create a data frame with your data
data1 <- data.frame(
  program = career$d.Why.did.you.opt.for.the.program,
  happy = career$e.How.happy.are.you.with.your.chosen.program
)

# Convert variables to factors
data1$program <- factor(data1$program)
data1$happy <- factor(data1$happy)

# Fit logistic regression model
model <- glm(happy ~ program, data = data1, family = binomial(link = "logit"))

# Predict probabilities
data1$predicted_prob <- predict(model, type = "response")

# Make sure factor levels are consistent
data1$happy <- factor(data1$happy, levels = levels(data1$happy))

# Plot the predicted probabilities against the independent variable
ggplot(data1, aes(x = program, y = predicted_prob, fill = happy)) +
  geom_boxplot() +
  labs(x = "Reason for opting the program", y = "Predicted Probability", fill = "Happiness") +
  ggtitle("Logistic Regression Plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#############################################






chi square
internship1=c(career$j.Are.you.doing.an.internship.program.or.did.you.do.any)
internship1
inline1=c(career$k.Is.your.program.more.inline.with.the.emerging.technologies.and.advancements.in.your.field)
inline1
contingency_table=table(internship1,inline1)
contingency_table
print(contingency_table)
chi_square_test1=chisq.test(contingency_table)
# Perform Fisher's exact test
fisher_test <- fisher.test(contingency_table)

# Print the result
print(fisher_test)
opted=c(career$d.Why.did.you.opt.for.the.program)
opted
happy1=c(career$e.How.happy.are.you.with.your.chosen.program)
happy1
contingency_table1=table(opted,happy1)
contingency_table1
chisq.test(contingency_table1)
fisher.test(contingency_table1)
#############3
Internship_taken=as.numeric(factor(career$j.Are.you.doing.an.internship.program.or.did.you.do.any,levels=c("No","Yes")))
Internship_taken
Inline_with_technologies=as.numeric(factor(career$k.Is.your.program.more.inline.with.the.emerging.technologies.and.advancements.in.your.field,levels=c("Totally","Partially","Not.sure")))
Inline_with_technologies
plot(Internship_taken,Inline_with_technologies,col="red",pch=16,cex=1)
plot
lm(Inline_with_technologies~Internship_taken)
model2=lm(Inline_with_technologies~Internship_taken)
model2
summary(model2)
abline(model2)
skill_enhancement_programs=as.numeric(factor(career$h.How.often.do.you.take.part.in.various.skill.enhancement.programs.offered.or.organized.in.your.college.or.outside,levels=c("Always","Sometimes","Never")))
skill_enhancement_programs
happy_with_chosen_program=as.numeric(factor(career$e.How.happy.are.you.with.your.chosen.program,levels=c("Very.happy","Moderately.happy","Not.at.all")))
happy_with_chosen_program
plot(happy_with_chosen_program,skill_enhancement_programs,col="yellow",pch=16,cex=1)
plot
lm(skill_enhancement_programs~happy_with_chosen_program)
model3=lm(skill_enhancement_programs~happy_with_chosen_program)
model3
summary(model3)
abline(model3)
