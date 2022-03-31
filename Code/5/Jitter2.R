library(r4ss)

Path <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/Jitter/test1_5_rt_cw/"
# JitterPath <- "D:/OneDrive - IATTC/IATTC/2021/Spatial-SA/Model/diagnostics/Jitter/"

jit.likes <- SS_RunJitter(
  model = "ss",
  mydir = Path,
  Njitter = 1,
  verbose = TRUE,
  # systemcmd = TRUE,
  extras = "-nox -cbs 2000000000 -gbs 2000000000 -ams 200000000 –maxfn 10000 -nohess",
  jitter_fraction = 0.02)
