library(ggplot2)
library(dplyr)
library(lubridate)
library("rjags")
library(devtools)
#install_github(repo = "michaelmalick/r-codatools")
library(codatools)


# From Kaggle
# Get the data
Soccer <- read.csv("~/Downloads/results.csv")

#  UEFA European Championship

euro <- Soccer %>%
    filter(tournament=="UEFA Euro") %>%
    filter(year(date)>=2000)
    

euro$difference_non_abs<-euro$home_score-euro$away_score

# summary of home_score and away_score
summary(euro$home_score)
summary(euro$away_score)
# and standard deviation
sd(euro$home_score)
sd(euro$away_score)


# Total home and away scores
home_sum_scores<-euro %>%
  group_by(home_team) %>% summarise(total_home = sum(home_score))

away_sum_scores<-euro %>%
  group_by(away_team) %>% summarise(total_away = sum(away_score))

# Graph of total total_home and total_away scores
home_away_sum<-cbind(home_sum_scores,away_sum_scores)
head(home_away_sum)

f <- ggplot(home_away_sum, aes(total_home,total_away))
f + geom_text(aes(label=home_team))+geom_point()

# Difference home-away plots

t <- ggplot(euro, aes(year(date),difference_non_abs)) + geom_point()
t + facet_wrap(vars(home_team))

t <- ggplot(euro, aes(year(date),difference_non_abs)) + geom_point()
t + facet_wrap(vars(away_team))

# New variables 
home<-euro$home_team
away<-euro$away_team

home_away<-unique(rbind(as.matrix(home),as.matrix(away)))
home_away<-data.frame(home_away)
home_away$ind<- seq.int(nrow(home_away))
euro<-left_join(euro,home_away,by = c("home_team" = "home_away"))


euro<-rename(euro,home_ind=ind)
euro<-left_join(euro,home_away,by = c("away_team" = "home_away"))
euro<-rename(euro,away_ind=ind)

# How many unique countries
nrow(home_away)
# At what years do they play. Every fourth year
euro_years<-unique(year(euro$date))
euro_years

# Jags model

mod_string = " model {
for (i in 1:length(date)) {
  home_score[i] ~ dpois(theta_home[i])
  away_score[i] ~ dpois(theta_away[i])
  theta_home[i] = exp(home+att[home_ind[i]]+def[away_ind[i]]) 
  theta_away[i] = exp(att[away_ind[i]]+def[home_ind[i]])
  }

for (i in 1:max(home_ind)) {
  att_star[i] ~ dnorm(mu_att, tau_att)
  def_star[i] ~ dnorm(mu_def, tau_def)
  att[i]=att_star[i]-mean(att_star)
  def[i]=def_star[i]-mean(def_star)
}

mu_att ~ dnorm(0.0,1000)
tau_att ~ dgamma(0.01,0.01)
mu_def ~ dnorm(0.0,1000)
tau_def ~ dgamma(0.01, 0.01)
home ~ dnorm(0.0,1)
} "

set.seed(102)

data_jags = as.list(euro)
params = c("att","def",   "mu_att", "tau_att","mu_def","tau_def","home")


mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 2e3) # burn-in

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=20e3)

mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combine multiple chains

## Convergence diagnostics
par(mar = rep(2, 4))
plot(mod_sim, ask=TRUE)
gelman.diag(mod_sim,multivariate = FALSE)
autocorr.diag(mod_sim)
#s<-autocorr.diag(mod_sim)
#write.table(s,file='corr.csv',sep=';')
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
dic.samples(mod, n.iter=1e3)
summary(mod_sim)
results<-(summary(mod_sim))


g<-data.frame(coda_table(mod_sim, parameters = NULL, gelman.diag = FALSE, quantiles = c(0.025, 0.1, 0.9, 0.975)))

write.table(g,file='stat.csv',sep=';')
attach<-g[1:32,1]
defence<-g[33:64,1]

attach_defence<-cbind(as.matrix(attach),as.matrix(defence))
attach_defence<-cbind(attach_defence,home_away)

attach_defence<-data.frame(attach_defence)
attach_defence<-select(attach_defence,-c(ind))
colnames(attach_defence)<-c('att','def','country')

# Plot of (att,def) variables
e <- ggplot(attach_defence, aes(def, att))
#e + geom_jitter(height = 2, width = 2)
e + geom_text(aes(label=country))+geom_point()


(pmed_coef = apply(mod_csim, 2, mean))
jags.samples(mod,c('home_score','away_score'),n.iter=2000,type=c('trace'))


# Probability that Denmark will win
# Denmark(16), Spain(8),Germany(5), Sweden(9),Netherlands(3)

# Denmark against Spain
theta_home= exp(mod_csim[,"home"]+mod_csim[,"att[16]"]+mod_csim[,"def[8]"])
theta_away=exp(mod_csim[,"att[8]"]+mod_csim[,"def[16]"])
n_sim=length(theta_home)
den_spain=rpois(n=n_sim,theta_home)
spain_den=rpois(n=n_sim,theta_away)
mean(den_spain>spain_den)

# Denmark against Netherlands
theta_home= exp(mod_csim[,"home"]+mod_csim[,"att[16]"]+mod_csim[,"def[3]"])
theta_away=exp(mod_csim[,"att[3]"]+mod_csim[,"def[16]"])
n_sim=length(theta_home)
n_sim
den_neth=rpois(n=n_sim,theta_home)
neth_den=rpois(n=n_sim,theta_away)
mean(den_neth>neth_den)
# Denmark against Sweden
# Denmark home against Sweden (away) (9)
theta_home= exp(mod_csim[,"home"]+mod_csim[,"att[16]"]+mod_csim[,"def[9]"])
theta_away=exp(mod_csim[,"att[9]"]+mod_csim[,"def[16]"])
n_sim=length(theta_home)
den_swe=rpois(n=n_sim,theta_home)
swe_den=rpois(n=n_sim,theta_away)
mean(den_swe>swe_den)

# Denmark against Germany
# Denmark home against Germany (away) (9)
theta_home= exp(mod_csim[,"home"]+mod_csim[,"att[16]"]+mod_csim[,"def[5]"])
theta_away=exp(mod_csim[,"att[5]"]+mod_csim[,"def[16]"])
n_sim=length(theta_home)
#n_sim
den_ger=rpois(n=n_sim,theta_home)
ger_den=rpois(n=n_sim,theta_away)
mean(den_ger>ger_den)

