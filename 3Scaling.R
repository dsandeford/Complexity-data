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
summary(lm(log10(Agri)~Pop+AL+NGA, data=scalingData[which(scalingData$Agri>0),]))

# Territory and complexity
summary(lm(Terr~Pop, data=scalingData))
summary(lm(Terr~NGA+Pop, data=scalingData))
summary(lm(Terr~factor(Time)+Pop, data=scalingData))
summary(lm(Terr~NGA+factor(Time)+Pop, data=scalingData))

summary(lm(Complexity~Pop, data=scalingData))
summary(lm(Complexity~NGA+Pop, data=scalingData))
summary(lm(Complexity~factor(Time)+Pop, data=scalingData))
summary(lm(Complexity~NGA+factor(Time)+Pop, data=scalingData))


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




###
### Figures 1-2
###


### Figure 1
fit1a <- lm(log10(TFC_PJ_IEA)~log10(Total_km), data=large_scale_infra1[which(large_scale_infra1$Country!='Singapore'),])
slope1a <- round(coef(fit1a)[2], 2)
intercept1a <- round(coef(fit1a)[1], 2)
r21a <- round(summary(fit1a)$adj.r.squared, 2)

fig1a = ggplot(data=large_scale_infra1[which(large_scale_infra1$Country!='Singapore'),], aes(x = log10(Total_km), y = log10(TFC_PJ_IEA))) +  
  geom_point(color='grey', alpha=0.5) +
  geom_smooth(formula = y~x, method='lm', se=FALSE, color = adjustcolor('navy', alpha.f = 0.5)) +
  annotate("text", x = 5.5, y = 5, hjust = 0.5,
           label = paste0("italic(log~Energy) == ", intercept1a, " + ", slope1a, 
                          " * italic(log~Ln) ~~ ~~ italic(R^2) == ", r21a),
           parse = TRUE) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(4, 7) ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  xlab(TeX("log Length of Road Network (km), $L_n$")) +
  ylab(TeX("log $E$ (PJ/y)")) +
  theme_minimal()

fig1a


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




### Bootstrap CIs


# Estimate bootstrapped 95% CI for energy~road network exponent
road <- subset(large_scale_infra1,
               Country != "Singapore" &
                 !is.na(TFC_PJ_IEA) & !is.na(Total_km) &
                 TFC_PJ_IEA > 0 & Total_km > 0)

slope_road <- function(d, i) {
  coef(lm(log10(TFC_PJ_IEA) ~ log10(Total_km), data = d[i, ]))[2]
}

set.seed(20261027)
bR <- boot(road, slope_road, R = 5000)

bR                                     # estimate, bias, SE
boot.ci(bR, type = c("perc", "bca"))
nrow(road)  



# Estimate bootstrapped 95% CIs for intensity, territory, and complexity exponents


# 1. Agricultural intensity
dI <- subset(scalingData, Agri > 0)
dI <- dI[complete.cases(dI[, c("Agri", "Pop", "NGA", "AL")]), ]
dI$logAgri <- log10(dI$Agri)
dI$NGA <- factor(dI$NGA)

beta_fun <- function(d, i) {
  b <- coef(lm(logAgri ~ Pop + NGA + AL, data = d[i, ]))["Pop"]
  c(beta = unname(b), A = 1 - 2 * unname(b), C = 2 * unname(b))
}

set.seed(20261027)
bI <- boot(dI, beta_fun, R = 5000)

bI                                                  # estimates, bias, SEs
boot.ci(bI, type = c("perc", "bca"), index = 1)     # beta
boot.ci(bI, type = c("perc", "bca"), index = 2)     # predicted range for territory, 1 - 2*beta
boot.ci(bI, type = c("perc", "bca"), index = 3)     # predicted range for complexity, 2*beta


# 2. Territory and complexity
dA <- subset(scalingData, !is.na(Terr) & !is.na(Pop))
dC <- subset(scalingData, is.finite(Complexity) & !is.na(Pop))
slope_rows <- function(d, i, y) {
  coef(lm(reformulate("Pop", y), data = d[i, ]))["Pop"]
}
set.seed(20261027)
bA_rows <- boot(dA, slope_rows, R = 5000, y = "Terr")
bC_rows <- boot(dC, slope_rows, R = 5000, y = "Complexity")
boot.ci(bA_rows, type = c("perc", "bca"))
boot.ci(bC_rows, type = c("perc", "bca"))


# 3. Complexity components
slope <- function(d, i) coef(lm(logy ~ Pop, data = d[i, ]))["Pop"]

comps <- list(
  Gov     = subset(scalingData, Gov > 0),
  Idea    = subset(scalingData, Idea > 0),
  Infra   = subset(scalingData, Infra > 0),
  Info    = subset(scalingData, Info > 0),
  MilTech = subset(scalingData, MilTech > 0),
  Hier    = subset(scalingData, Hier > 0)      

set.seed(20261027)
res <- do.call(rbind, lapply(names(comps), function(v) {
  d <- comps[[v]]
  d$logy <- log10(d[[v]])
  d <- d[!is.na(d$Pop) & is.finite(d$logy), ]
  b  <- boot(d, slope, R = 5000)
  ci <- boot.ci(b, type = "bca")$bca[4:5]
  s  <- summary(lm(logy ~ Pop, data = d))
  data.frame(component = v,
             estimate  = round(b$t0, 3),
             ci_low    = round(ci[1], 3),
             ci_high   = round(ci[2], 3),
             R2        = round(s$r.squared, 2),
             n         = nrow(d))
}))
res


### Materials and Methods analyses

# 1. Examine how dropped zeros bias estimates

for (v in c("Gov","Idea","Infra","Info","MilTech")) {
  s <- scalingData[!is.na(scalingData[[v]]) & !is.na(scalingData$Pop), ]
  cat(v, sum(s[[v]] == 0),
      coef(lm(log10(s[[v]][s[[v]] > 0]) ~ s$Pop[s[[v]] > 0]))[2],
      coef(lm(log10(s[[v]] + 1) ~ s$Pop))[2], "\n")
}


# 2. Measurement of error in log10 population 

raw <- read.csv("/home/david/work/complexity/full_stripped_oct2020.csv", stringsAsFactors = FALSE)

## 2.1. Measurement error variance of log10 population from Seshat's coded ranges
pop <- raw[raw$Variable == "Polity Population", ]
lo  <- suppressWarnings(as.numeric(pop$Value.From))
hi  <- suppressWarnings(as.numeric(pop$Value.To))
ranged <- !is.na(lo) & !is.na(hi) & lo > 0 & hi > 0 & hi != lo
logwidth <- log10(hi[ranged] / lo[ranged])

su2 <- mean(logwidth^2) / 12          # error variance if the true value is uniform over the range
c(entries = sum(!is.na(lo)), with_range = sum(ranged),
  median_range_factor = 10^median(logwidth), error_var = su2, error_sd = sqrt(su2))

## 2.2. Method-of-moments (reliability-corrected) exponents
mm <- function(d, y) {
  d   <- d[!is.na(d$Pop) & is.finite(d[[y]]), ]
  b   <- unname(coef(lm(d[[y]] ~ d$Pop))[2])
  rel <- 1 - su2 / var(d$Pop)
  data.frame(variable = y, n = nrow(d), OLS = round(b, 4),
             reliability = round(rel, 4), corrected = round(b / rel, 4),
             change = round(b / rel - b, 4))
}

comp <- function(v, sub = v) {
  d <- scalingData[which(scalingData[[sub]] > 0), ]
  d[[paste0("log", v)]] <- log10(d[[v]])
  mm(d, paste0("log", v))
}

me <- rbind(
  mm(scalingData, "Terr"),
  mm(scalingData, "Complexity"),
  comp("Gov"), comp("Idea"), comp("Infra"),
  comp("Info"), comp("MilTech"), comp("Hier", sub = "AL")
)
me

## 2.3. Breakdown: error needed to change the conclusions
## Error in population only steepens the slopes. The territory exponent leaves its
## predicted range when the corrected slope passes the upper bound of 1 - 2*beta.
upper_A <- 0.885                      # BCa upper bound of 1 - 2*beta (Table 2)

dA <- scalingData[!is.na(scalingData$Pop) & !is.na(scalingData$Terr), ]
bA <- unname(coef(lm(Terr ~ Pop, data = dA))[2])
sd_needed <- sqrt((1 - bA / upper_A) * var(dA$Pop))
c(error_sd_log10 = sd_needed, error_factor = 10^sd_needed)

## The same error applied to complexity moves it toward 1/6
dC <- scalingData[!is.na(scalingData$Pop) & is.finite(scalingData$Complexity), ]
bC <- unname(coef(lm(Complexity ~ Pop, data = dC))[2])
relC <- 1 - sd_needed^2 / var(dC$Pop)
c(complexity_OLS = bC, complexity_corrected = bC / relC)




### 3. Analysis of high-leverage data points
library(MASS)        # rlm: Huber M-estimator
library(robustbase)  # lmrob: MM-estimator
library(mblm)        # Theil-Sen

lev <- function(d, y) {
  d <- d[!is.na(d$Pop) & is.finite(d[[y]]), ]
  d$Y <- d[[y]]
  f  <- lm(Y ~ Pop, data = d)
  n  <- nrow(d); p <- 2
  h  <- hatvalues(f); cd <- cooks.distance(f)
  flag <- cd > 4 / n
  out <- data.frame(
    variable       = y, n = n,
    high_leverage  = sum(h > 2 * p / n),
    influential    = sum(flag),
    OLS            = coef(f)[2],
    drop_infl      = coef(lm(Y ~ Pop, data = d[!flag, ]))[2],
    Huber          = coef(rlm(Y ~ Pop, data = d, maxit = 100))[2],
    MM             = coef(lmrob(Y ~ Pop, data = d))[2],
    TheilSen       = coef(mblm(Y ~ Pop, dataframe = d, repeated = FALSE))[2],
    infl_mean_Pop  = mean(d$Pop[flag]),
    infl_pct_neg   = mean(resid(f)[flag] < 0),
    row.names = NULL)
  attr(out, "flagged") <- d$PolName[flag]
  out
}

logc <- function(v, sub = v) {
  d <- scalingData[which(scalingData[[sub]] > 0), ]
  d[[paste0("log", v)]] <- log10(d[[v]]); d
}

res <- list(
  lev(scalingData, "Terr"),
  lev(scalingData, "Complexity"),
  lev(logc("Gov"), "logGov"),         lev(logc("Idea"), "logIdea"),
  lev(logc("Infra"), "logInfra"),     lev(logc("Info"), "logInfo"),
  lev(logc("MilTech"), "logMilTech"), lev(logc("Hier", "AL"), "logHier")
)
tab <- do.call(rbind, res); tab[, 5:11] <- round(tab[, 5:11], 3); tab

attr(res[[2]], "flagged")   # influential polities in the complexity regression






### Counter factual test, C could scale linearly then saturate but doesn't, Discussion

d <- subset(scalingData, is.finite(Pop) & is.finite(Complexity))

# 0. Arithmetic check: linearity over 10^3-10^5 needs about one type near 10^3
range(10^d$Complexity[d$Pop >= 3 & d$Pop <= 5])

# 1. Within the window: the counterfactual predicts a slope of 1
fw <- lm(Complexity ~ Pop, data = subset(d, Pop >= 3 & Pop <= 5))
coef(fw)["Pop"]; confint(fw)["Pop", ]

# 2. Above the window: the counterfactual predicts a slope of 0
fa <- lm(Complexity ~ Pop, data = subset(d, Pop > 5))
coef(fa)["Pop"]; confint(fa)["Pop", ]

# 3. Linear-then-saturating vs. power law, both fit to Complexity
pow  <- lm(Complexity ~ Pop, data = d)
satF <- nls(Complexity ~ lImax + Pop - log10(10^lK + 10^Pop),
            data = d, start = list(lImax = 2, lK = 4))
AIC(pow, satF)
AIC(satF) - AIC(pow)   # the Delta AIC reported in the text
10^coef(satF)          # estimated ceiling and half-saturation population
