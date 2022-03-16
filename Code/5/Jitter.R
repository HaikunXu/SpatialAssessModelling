library(r4ss)
library(tidyverse)

Path <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_rt_cw/"
JitterPath <- paste0("D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/test1_5_rt_cw/Jitter/",1:10)

profilemodels <- SSgetoutput(dirvec = JitterPath, getcovar = FALSE)

# summarize output
profilesummary <- SSsummarize(profilemodels)
# Likelihoods
Tot_likelihood <-
  as.numeric(profilesummary[["likelihoods"]][1, 1:10])
R0 <-
  as.numeric(profilesummary$pars[which(profilesummary$pars$Label == "SR_LN(R0)"), 1:10])

myreplist <-
  SS_output(
    dir = Path,
    ncols = 400,
    covar = F,
    verbose = FALSE,
    printstats = FALSE
  )

NLL <- data.frame(
  "Jitter" = 1:10,
  "NLL" = Tot_likelihood,
  "R0" = R0,
  "NLL_Diff" = sign(Tot_likelihood - myreplist$likelihoods_used$values[1])
)

f1 <-
  ggplot(data = NLL) +
  geom_point(aes(
    x = Jitter,
    y = NLL,
    color = factor(NLL_Diff)
  )) +
  geom_hline(yintercept = myreplist$likelihoods_used$values[1]) +
  xlim(c(1, 10))
f2 <-
  ggplot(data = NLL) +
  geom_point(aes(
    x = Jitter,
    y = R0,
    color = factor(NLL_Diff)
  )) +
  # geom_hline(yintercept = myreplist$likelihoods_used$values[1]) +
  xlim(c(1, 10))
library(patchwork)
ggsave(
  f1 / f2,
  file = paste0(JitterDir, model[m], "-", toString(steepness[s]), ".png"),
  width = 8,
  height = 8
)