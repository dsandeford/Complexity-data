#### SCALING ANALYSIS ####
library(latex2exp)
library(ggplot2)
library(ggpmisc)
library(gridExtra)
setwd("/")
load("TableData.Rdata")


### Regression analyses

# Intensity of agriculture, I
summary(lm(log10(Agri)~Pop, data=scalingData[which(scalingData$Agri>0),]))
summary(lm(log10(Agri)~Pop+Cap+NGA, data=scalingData[which(scalingData$Agri>0),]))

# Territory and complexity
summary(lm(Terr~NGA+Pop, data=scalingData))
summary(lm(Terr~Pop, data=scalingData))

summary(lm(Complexity~NGA+Pop, data=scalingData))
summary(lm(Complexity~Pop, data=scalingData))


# Scaling of complexity components
summary(lm(log10(Gov)~Pop, data=scalingData[which(scalingData$Gov>0 ),]))
summary(lm(log10(Idea)~Pop, data=scalingData[which(scalingData$Idea>0),])) 
summary(lm(log10(Infra)~Pop, data=scalingData[which(scalingData$Infra>0),]))
summary(lm(log10(Info)~Pop, data=scalingData[which(scalingData$Info>0),]))
summary(lm(log10(MilTech)~Pop, data=scalingData[which(scalingData$MilTech>0),]))
summary(lm(log10(Hier)~Pop, data=scalingData[which(scalingData$Hier>0),]))



# Scaling of infrastructural length with energy consumption in contemporary societies
large_scale_infra1 <- read.csv("/Roadways-Energy.csv")
summary(lm(log10(TFC_PJ_IEA)~log10(Total_km), data=large_scale_infra[which(large_scale_infra$Country!='Singapore'),]))
large_scale_infra2 <- read.csv("/Railways-Energy.csv")
summary(lm(log_total_E~log_rail_lines_km, data=large_scale_infra2))


## Scaling of inequality
inequality = read.csv('/Inequality.csv')
summary(lm(Inequality ~ Pop , data=inequality))


## Scaling of the division of labor
dol = read.csv('/Functional diversity - Data.csv')
summary(lm(Div ~ Pop, data=dol))
# The following equation was used to transform Cap to Pop in the above data set
summary(lm(Pop~Cap, data=scalingData))

## Scaling of duration
summary(lm(Duration ~ NGA + Pop, data=scalingData))




### Figure 1

fit1a <- lm(log10(TFC_PJ_IEA)~log10(Total_km), data=large_scale_infra[which(large_scale_infra$Country!='Singapore'),])
slope1a <- round(coef(fit1a)[2], 2)
intercept1a <- round(coef(fit1a)[1], 2)
r21a <- round(summary(fit1a)$adj.r.squared, 2)

fig1a = ggplot(data=large_scale_infra[which(large_scale_infra$Country!='Singapore'),], aes(x = log10(Total_km), y = log10(TFC_PJ_IEA))) +  
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 5.5, y = 5, hjust = 0.5,
           label = paste0("italic(log~E) == ", intercept1a, " + ", slope1a, 
                          " * italic(log~Ln) ~~ ~~ italic(R^2) == ", r21a),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(4, 7) ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  xlab(TeX("log Length of Road Network (km), $L_n$")) +
  ylab(TeX("log $E$ (PJ/y)")) +
  theme_minimal()



fit1b <- lm(log_total_E~log_rail_lines_km, data=large_scale_infra2)
slope1b <- round(coef(fit1b)[2], 2)
intercept1b <- round(coef(fit1b)[1], 2)
r21b <- round(summary(fit1b)$adj.r.squared, 2)

fig1b = ggplot(data=large_scale_infra2, aes(x = log_total_E, y = log_rail_lines_km)) +  
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 10.5, y = 6, hjust = 0.5,
           label = paste0("italic(log~E) == ", intercept1b, " + ", slope1b, 
                          " * italic(log~Ln) ~~ ~~ italic(R^2) == ", r21b),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),, limits = c(8.5, 13) ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  xlab(TeX("log Length of Rail Network (km), $L_n$")) +
  ylab(TeX("log $E$ (TOE/y)")) +
  theme_minimal()

fig1a + fig1b





## Figure 2

# Figure 2a
fitb <- lm(Terr ~ Pop, data = scalingData)
slopeb <- round(coef(fitb)[2], 3)
interceptb <- round(coef(fitb)[1], 3)
r2b <- round(summary(fitb)$r.squared, 3)

fig2a = ggplot(data=scalingData, aes(x = Pop, y = Terr)) +  
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 5.5, y = 7.5, hjust = 0.5,
           label = paste0("italic(log~A) == ", interceptb, " + ", slopeb, 
                          " * italic(log~N) ~~ ~~ italic(R^2) == ", r2b),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(2, 8)) + 
  xlab(TeX(" ")) +
  ylab(TeX("log Area")) +
  theme_minimal()

# Figure 2b
fitc <- lm(Complexity ~ Pop, data = scalingData)
slopec <- round(coef(fitc)[2], 3)
interceptc <- round(coef(fitc)[1], 3)
r2c <- round(summary(fitc)$r.squared, 3)

fig2b = ggplot(data=scalingData, aes(x = Pop, y = Complexity)) +  
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 5.5, y = 2.25, hjust = 0.5,
           label = paste0("italic(log~C) == ", interceptc, " + ", slopec, 
                          " * italic(log~N) ~~ ~~ italic(R^2) == ", r2c),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0.5, 2.5)) + 
  xlab(TeX("log Polity population")) +
  ylab(TeX("log Complexity")) +
  theme_minimal()


fig2a / fig2b


## Figure 3

fit2a <- lm(log10(Gov) ~ Pop, data = scalingData[which(scalingData$Gov > 0),])
slope2a <- round(coef(fit2a)[2], 3)
intercept2a <- round(coef(fit2a)[1], 3)
r2_2a <- round(summary(fit2a)$r.squared, 3)
fig2a <- ggplot(data=scalingData[which(scalingData$Gov > 0),], aes(x = Pop, y = log10(Gov))) +
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 5.5, y = 1, hjust = 0.5,
           label = paste0("italic(log~CC1) == ", intercept2a, " + ", slope2a,
                          " * italic(log~N) ~~ ~~ italic(R^2) == ", r2_2a),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  xlab(TeX("")) +
  ylab(TeX("log Gov")) +
  theme_minimal()

# Figure 3b
fit2b <- lm(log10(Idea) ~ Pop, data = scalingData[which(scalingData$Idea > 0),])
slope2b <- round(coef(fit2b)[2], 3)
intercept2b <- round(coef(fit2b)[1], 3)
r2_2b <- round(summary(fit2b)$r.squared, 3)
fig2b <- ggplot(data=scalingData[which(scalingData$Idea > 0),], aes(x = Pop, y = log10(Idea))) +
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 5.5, y = 1, hjust = 0.5,
           label = paste0("italic(log~CC2) == ", intercept2b, " + ", slope2b,
                          " * italic(log~N) ~~ ~~ italic(R^2) == ", r2_2b),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  xlab(TeX("")) +
  ylab(TeX("log Idea")) +
  theme_minimal()

# Figure 3c
fit2c <- lm(log10(Infra) ~ Pop, data = scalingData[which(scalingData$Infra > 0),])
slope2c <- round(coef(fit2c)[2], 3)
intercept2c <- round(coef(fit2c)[1], 3)
r2_2c <- round(summary(fit2c)$r.squared, 3)
fig2c <- ggplot(data=scalingData[which(scalingData$Infra > 0),], aes(x = Pop, y = log10(Infra))) +
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 5.5, y = 1, hjust = 0.5,
           label = paste0("italic(log~CC3) == ", intercept2c, " + ", slope2c,
                          " * italic(log~N) ~~ ~~ italic(R^2) == ", r2_2c),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  xlab(TeX("")) +
  ylab(TeX("log Infra")) +
  theme_minimal()

# Figure 3d
fit2d <- lm(log10(Info) ~ Pop, data = scalingData[which(scalingData$Info > 0),])
slope2d <- round(coef(fit2d)[2], 3)
intercept2d <- round(coef(fit2d)[1], 3)
r2_2d <- round(summary(fit2d)$r.squared, 3)
fig2d <- ggplot(data=scalingData[which(scalingData$Info > 0),], aes(x = Pop, y = log10(Info))) +
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 5.5, y = 1.3, hjust = 0.5,
           label = paste0("italic(log~CC4) == ", intercept2d, " + ", slope2d,
                          " * italic(log~N) ~~ ~~ italic(R^2) == ", r2_2d),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  xlab(TeX("")) +
  ylab(TeX("log Info")) +
  theme_minimal()

# Figure 3e
fit2e <- lm(log10(MilTech) ~ Pop, data = scalingData[which(scalingData$MilTech > 0),])
slope2e <- round(coef(fit2e)[2], 3)
intercept2e <- round(coef(fit2e)[1], 3)
r2_2e <- round(summary(fit2e)$r.squared, 3)
fig2e <- ggplot(data=scalingData[which(scalingData$MilTech > 0),], aes(x = Pop, y = log10(MilTech))) +
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 5.5, y = 1.5, hjust = 0.5,
           label = paste0("italic(log~CC5) == ", intercept2e, " + ", slope2e,
                          " * italic(log~N) ~~ ~~ italic(R^2) == ", r2_2e),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  xlab(TeX("log Polity population")) +
  ylab(TeX("log MilTech")) +
  theme_minimal()

# Figure 3f
fit2f <- lm(log10(Hier) ~ Pop, data = scalingData[which(scalingData$Hier > 0),])
slope2f <- round(coef(fit2f)[2], 3)
intercept2f <- round(coef(fit2f)[1], 3)
r2_2f <- round(summary(fit2f)$r.squared, 3)
fig2f <- ggplot(data=scalingData[which(scalingData$Hier > 0),], aes(x = Pop, y = log10(Hier))) +
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 5.5, y = .8, hjust = 0.5,
           label = paste0("italic(log~CC6) == ", intercept2f, " + ", slope2f,
                          " * italic(log~N) ~~ ~~ italic(R^2) == ", r2_2f),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  xlab(TeX("log Polity population")) +
  ylab(TeX("log Hier")) +
  theme_minimal()


# Figure 3

(fig2a + fig2b) / (fig2c + fig2d) / (fig2e + fig2f)
