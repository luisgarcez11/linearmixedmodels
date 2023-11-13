library(nlme)
library(nlmeU)
library(lme4)

armd = nlmeU::armd

View(armd)

lm2.form <- formula(visual ~ visual0 + time + treat.f + treat.f:time)

fm16.1 <- nlme::lme(lm2.form,
          random = ~1|subject,
          data = armd)

printCoefmat(summary(fm16.1)$tTable, has.Pvalue = TRUE, P.values = TRUE)

getVarCov(fm16.1)

getVarCov(fm16.1, type = "conditional", individual = "6")

fm16.2 <- update(fm16.1,
                 weights = varPower(form = ~time),
                 data = armd)
fm16.2

getVarCov(fm16.2, type = "conditional", individual = "6")


plot(fm16.2)

plot(fm16.2,
     resid(., type = "pearson") ~ time|treat.f,
     id = 0.05)

qqnorm(fm16.2, ~resid(.)|time.f)

aug.Pred <- augPred(fm16.2,
        primary = ~time,
        level = 0:1,
        length.out = 2)
plot(aug.Pred, 
     layout = c(4,4,1),
     key = list(lines = list(lty = c(1,2)),
                text = list(c("Marginal", "Subject-specific")),
                columns = 2))


fm16.3 <- update(fm16.2,
                 random = ~1 + time|subject,
                 data = armd)


aug.Pred <- augPred(fm16.3,
                    primary = ~time,
                    level = 0:1,
                    length.out = 2)
plot(aug.Pred, 
     layout = c(4,4,1),
     key = list(lines = list(lty = c(1,2)),
                text = list(c("Marginal", "Subject-specific")),
                columns = 2))


anova(fm16.2,fm16.3 )



