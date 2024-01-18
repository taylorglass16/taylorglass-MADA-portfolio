
###############################
#########PRELIMINARIES#########
###############################

install_dependencies <- FALSE

if (install_dependencies) {
  # Set CRAN mirror
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  
  ## Installation of carat 
  # From CRAN
  # install.packages("carat")
  # From tar.gz file (on your local machine)
  install.packages("carat_2.2.0.tar.gz", repos = NULL, type = "source")
  # From GitHub
  # devtools::install_github('yexiaoqingruc/carat/carat')
  
  
  ## S3_carat-examples.R
  install.packages("ggplot2")
  install.packages("tidyverse")
  install.packages("microbenchmark")
  
  ## S5_Figure-8.R
  install.packages("gridExtra")
  install.packages("AuxPkg_2.0.tar.gz", repos = NULL, type = "source")
  
  ## S5_Table-4.R
  install.packages("doSNOW")
  install.packages("foreach")
  install.packages("caratOMP_2.2.0.tar.gz", repos = NULL, type = "source")
}

library("AuxPkg")
library("carat")
library("caratOMP")
library("doSNOW")
library("foreach")
library("ggplot2")
library("tidyverse")
library("gridExtra")
library("microbenchmark")
install.packages("AuxPkg")
install.packages("carat")
install.packages("caratOMP")
install.packages("doSNOW")
install.packages("foreach")
install.packages("ggplot2")
install.packages("microbenchmark")

# RNGversion("3.6.0")
# RNGkind("Mersenne-Twister")
## set seed
set.seed(202009, kind = "Mersenne-Twister",
         normal.kind = "Inversion", sample.kind = "Rejection")

###############################
######Randomization part#######
###############################

## Generating randomization sequence with complete covariate data

# load the dataset "pats"
data("pats", package = "carat")
head(pats, 10)

# allocate "pats" with Pocock and Simon's minimization
PS_RealD <- PocSimMIN(data = pats, weight = c(1, 2, 1, 1), p = 0.85)
PS_RealD

# obtain the allocation sequence of the first 10 patients
PS_RealD$assignments[1:10]

## Generating randomization sequence with a covariate data-generating mechanism
HuHuCAR.sim(n = 1000, cov_num = 3, level_num = c(2, 5, 10), 
            pr = c(rep(0.5, 2), rep(0.2, 5), rep(0.1, 10)), p = 0.85)

## Note that the command-line user interface can be ran in S3_carat-CLI.R.

###############################
###Evaluation and Comparison###
###############################

# evaluate the Hu and Hu's general procedure through a covariate data generating mechanism
n <- 1000
N <- 500
p <- 0.85
cov_num <- 3
level_num <- c(2, 5, 2)
pr <- c(rep(0.5, 2), rep(0.2, 5), rep(0.5, 2))
omega <- c(1, 2, rep(1, cov_num))
evalR.HH_simD <- evalRand.sim(n, N, TRUE, cov_num, level_num, pr, 
                              "HuHuCAR", omega, p)
evalR.HH_simD

# compare the implemented randomization procedures in carat
weight <- rep(1, cov_num)
bsize <- 4
a <- 3
evalR.PS_simD <- evalRand.sim(n, N, TRUE, cov_num, level_num, pr,
                              "PocSimMIN", weight, p)
evalR.STR_simD <- evalRand.sim(n, N, TRUE, cov_num, level_num, pr,
                               "StrPBR", bsize)
pdf.options(reset = TRUE, onefile = FALSE) 
pdf("rand1.pdf", width = 10, height = 4)
compRand(evalR.HH_simD, evalR.PS_simD, evalR.STR_simD)
dev.off() 

###############################
######Hypothesis testing#######
###############################


## Bootstrap t test

# data generation
  # (1) The allocation sequence is generated with stratified biased coin design;
  # (2) The underlying is specified as the logistic model with binary outcomes.
dataS <- getData(n = 100, cov_num = 2, level_num = c(2, 2),
                 pr = rep(0.5, 4), type = "logit", beta = c(0.1, 0.2, 0.4, 0.8), mu1 = 0,
                 mu2 = 0, method = "StrBCD", p = 0.85)
dataS[, 1:4]

# bootstrap t test using "dataS"
boot.test(data = dataS, method = "StrBCD", p = 0.85)


## Corrected t test

# data generation
  # (1) The allocation sequence is generated with Hu and Hu's randomization;
  # (2) The underlying model is specified with a linear model.
dataH <- getData(n = 100, cov_num = 2, level_num = c(2, 2),
                 pr = rep(0.5, 4), type = "linear", beta = c(0.1, 0.2, 0.4, 0.8),
                 mu1 = 2.0, mu2 = 0, sigma = 1, method = "HuHuCAR",
                 omega = c(0.1, 0.1, 0.4, 0.4), p = 0.85)

# corrected t test
corr.test(data = dataH, conf = 0.95)


## Randomization test

# data generation
# (1) The allocation sequence is generated with covaraite-adjusted biased coin design;
# (2) The underlying model is specified with a linear model.
dataA <- getData(n = 100, cov_num = 2, level_num = c(2, 2),
                 pr = rep(0.5, 4), type = "linear", beta = c(0.1, 0.2, 0.4, 0.8), mu1 = 0,
                 mu2 = 0, method = "AdjBCD", a = 3)

# randomization test
pdf("infer1.pdf", width = 6, height = 4.5)
rand.test(data = dataA, method = "AdjBCD", a = 3)
dev.off()


###############################
########Power analysis#########
###############################

## Power Calculation

evalPower(n = 100, cov_num = 3, level_num = rep(2, 3),
          pr = rep(0.5, 6), type = "linear",
          beta = c(0.1, 0.2, 0.1, 0.2, 0.2, 0.4),
          sigma = 1, di = seq(0, 0.8, 0.1), Iternum = 1000, sl = 0.05,
          method = "StrPBR", bsize = 4, test = "rand.test", Reps = 200,
          plot = FALSE, nthreads = 1)

## Power Comparison
HHbtp <- evalPower(n = 100, cov_num = 2, level_num = c(2, 2),
                   pr = rep(0.5, 4), type = "linear", beta = c(1, 2, 2, 4),
                   sigma = 1, di = seq(0, 1.5, 0.3), Iternum = 1000, sl = 0.05,
                   method = "HuHuCAR", omega = c(0.1, 0.1, 0.4, 0.4), p = 0.85,
                   test = "boot.test", B = 200, plot = FALSE, nthreads = 1)
HHctp <- evalPower(n = 100, cov_num = 2, level_num = c(2, 2),
                   pr = rep(0.5, 4), type = "linear", beta = c(1, 2, 2, 4),
                   sigma = 1, di = seq(0, 1.5, 0.3), Iternum = 1000, sl = 0.05,
                   method = "HuHuCAR", omega = c(0.1, 0.1, 0.4, 0.4), p = 0.85,
                   test = "corr.test", plot = FALSE)
Doptbtp <- evalPower(n = 100, cov_num = 2, level_num = c(2, 2),
                     pr = rep(0.5, 4), type = "linear", beta = c(1, 2, 2, 4),
                     sigma = 1, di = seq(0, 1.5, 0.3), Iternum = 1000, sl = 0.05,
                     method = "DoptBCD", test = "boot.test", B = 200,
                     plot = FALSE, nthreads = 1)

di <- seq(0, 1.5, 0.3)
Iternum <- 1000
Hpvs <- matrix(0, nrow = length(di), ncol = Iternum)
Dpvs <- matrix(0, nrow = length(di), ncol = Iternum)
for(i in 1:length(di)) {
  for (j in 1:Iternum) {
    dataDt <- getData(n = 100, cov_num = 2, level_num = c(2, 2),
                      pr = rep(0.5, 4), type = "linear", beta = c(1, 2, 2, 4),
                      mu1 = di[i], mu2 = 0, sigma = 1, method = "DoptBCD")
    dataHt <- getData(n = 100, cov_num = 2, level_num = c(2, 2),
                      pr = rep(0.5, 4), type = "linear", beta = c(1, 2, 2, 4),
                      mu1 = di[i], mu2 = 0, sigma = 1, method = "HuHuCAR",
                      omega = c(0.1, 0.1, 0.4, 0.4), p = 0.85)
    dataDt <- data.frame(t(dataDt))
    dataHt <- data.frame(t(dataHt))
    ocD1 <- subset(dataDt, assignment == 1, select = outcome)
    ocD2 <- subset(dataDt, assignment == 2, select = outcome)
    ocH1 <- subset(dataHt, assignment == 1, select = outcome)
    ocH2 <- subset(dataHt, assignment == 2, select = outcome)
    reD <- t.test(ocD1, ocD2)
    reH <- t.test(ocH1, ocH2)
    Dpvs[i, j] <- (reD$p.value < 0.05)
    Hpvs[i, j] <- (reH$p.value < 0.05)
  }
}

powerD <- data.frame(diff = di, value = apply(Dpvs, 1, sum) / Iternum,
                     se = round(sqrt((apply(Dpvs, 1, sum) / Iternum) *
                                  (1 - apply(Dpvs, 1, sum) / Iternum) /
                                  Iternum), 3))
powerH <- data.frame(diff = di, value = apply(Hpvs, 1, sum) / Iternum,
                     se = round(sqrt((apply(Hpvs, 1, sum) / Iternum) *
                                  (1 - apply(Hpvs, 1, sum) / Iternum) /
                                  Iternum), 3))
Doptp <- list(Powers = powerD)
HHtp <- list(Powers = powerH)

powers_compare <- list(HHbtp, HHctp, HHtp, Doptbtp, Doptp)
testname_compare <- c("HH.boot", "HH.corr", "HH.simple",
                      "Dopt.boot", "Dopt.simple")
pdf("infer2.pdf", width = 6, height = 4.5)
compPower(powers_compare, di, testname_compare)
dev.off()

# Section 4-Application of carat

## set seed
set.seed(202009, kind = "Mersenne-Twister",
         normal.kind = "Inversion", sample.kind = "Rejection")

cont <- read.csv("CBASP.csv")

id1 <- which(cont$A2 == -1) # NEF alone
id2 <- which(cont$A2 == 0) # combo
id3 <- which(cont$A2 == 1) # CBT alone

cont_subset <- cont[-id3, ]
Fina <- which(is.na(cont_subset$FinalHAMD))
cont_subset <- cont_subset[-Fina, ] # remove the patients with NA FinalHAMD
cont$A2 <- cont$A2 + 2 # 1 for NEF alone, 2 for combo

#variables used for Pocock and Simon's randomization:
#ObsCompul, GENDER, RACE
profile <- cont_subset[,c("ObsCompul", "GENDER", "RACE", "A2")]
data <- cont_subset[, c("ObsCompul", "GENDER", "RACE", "A2","FinalHAMD")]

## Randomization

dataD <- data[ , 1:3]
rownames(dataD) <- 1:440

HH <- evalRand(dataD, "HuHuCAR", 500, c(1, 2, rep(1, 3)), 0.85);
PS <- evalRand(dataD, "PocSimMIN", 500, rep(1, 3), 0.85);
SH <- evalRand(dataD, "StrBCD", 500, 0.85);
STR <- evalRand(dataD, "StrPBR", 500, 4);
A <- evalRand(dataD, "AdjBCD", 500, 3);
D <- evalRand(dataD, "DoptBCD", 500)

pdf("rand2.pdf", width = 13.5, height = 5)
compRand(HH, PS, SH, STR, A, D)
dev.off()

RealD.HH <- HuHuCAR(dataD, c(2, 2, rep(1, 3)), 0.85)
RealD.HH

RealD.HH$assignments[1:10]

## Inference

Iternum <- 1000
sslm4 <- lm(FinalHAMD ~ A2 + ObsCompul + GENDER + RACE, data = data)
nop <- seq(50, 300, 50)
diffs <- seq(0, 11, by = 1.5)
nop.name <- rep('', length(nop))
PSnop <- list()
pvals <- matrix(0, length(diffs), Iternum)
for(i in 1:length(nop)) {
  for(j in 1:length(diffs)) {
    sslm4$coefficients[2] <- diffs[j]
    for (k in 1:Iternum) {
      PSind <- sample(1:nrow(cont_subset), nop[i], replace = TRUE)
      data_temp <- data[PSind, ]
      PSCovA <- PocSimMIN(profile[PSind, -4])$Cov_Assig
      data_temp$A2 <- PSCovA[4, ] - 1
      data_combine <- rbind(PSCovA, predict(sslm4, data_temp)+
                              rnorm(nrow(data_temp), sd = summary(sslm4)$sigma))
      pvals[j, k] <- (corr.test(data_combine)$p.value < 0.05)
    }
  }
  result.temp <- data.frame(diff = diffs, value = apply(pvals, 1, sum) / Iternum,
                            se = round(sqrt((apply(pvals, 1, sum) / Iternum) *
                                              (1 - apply(pvals, 1, sum) / Iternum) /
                                              Iternum), 3))
  eval(parse(text = paste('PSnop[[', i, ']]', '=list(Powers = result.temp)', sep = '')))
  nop.name[i] = nop[i]
}
PSnop.cm <- compPower(PSnop,diffs = diffs, nop.name)
pdf("infer3.pdf", width = 6, height = 4.5)
PSnop.cm
dev.off()

###############################
#####Computational details#####
###############################

## Figure-7
# Comparison of the run-time for R and Rcpp

set.seed(20200911, kind = "Mersenne-Twister",
         normal.kind = "Inversion", sample.kind = "Rejection")

nvec <- seq(200, 2500, by = 200)
R <- list()
for(i in 1:length(nvec)) {
  n <- nvec[i]
  df <- data.frame("gender" = sample(c("female", "male"), n, TRUE, c(1 / 3, 2 / 3)),
                   "age" = sample(c("0-30", "30-50", ">50"), n, TRUE),
                   "jobs" = sample(c("stu.", "teac.", "others"), n, TRUE),
                   "pills" = sample(c("0", "1-3", "3-6", ">6"), n, TRUE),
                   stringsAsFactors = TRUE)
  XZ <- t(Preprocess(df)$data)
  TIM <- microbenchmark(Atkinson(XZ),
                       DoptBCD(df), times = 20)
  R[[i]] <- TIM
}

dftime <- as.data.frame(matrix(NA, ncol = 2, nrow = length(nvec)))
colnames(dftime) <- c("Atkinson", "DoptBCD")
for(i in 1:length(nvec)) {
  RR <- R[[i]]
  expr <- as.numeric(RR$expr)
  ind2 <- which(expr == 2)
  ind1 <- which(expr == 1)
  dftime[i, 1] <- mean(RR$time[ind1])
  dftime[i, 2] <- mean(RR$time[ind2])
}

dftime <- dftime / (10 ^ 9)
dfp <- data.frame("time" = c(dftime$Atkinson, dftime$DoptBCD),
                 "size" = rep(nvec, 2),
                 "Compiled language" = rep(c("R", "Rcpp"), each = length(nvec)),
                 stringsAsFactors = TRUE)

pdf("rand3.pdf", width = 6, height = 4.5)
ggplot(
  data = dfp, mapping = aes(
    y = time, x = size, group = Compiled.language, color = Compiled.language,
    shape = Compiled.language)) +
  geom_line() +
  theme(legend.position = "bottom") +
  xlab("Sample size") +
  ylab("Time (s)") +
  geom_point(size = 2) +
  scale_colour_hue(name = "Compiled language") +
  scale_shape_discrete(name = "Compiled language") + 
  scale_x_continuous(limits = c(0,2550), breaks = seq(0, 2500, by = 500))
dev.off()

## Figure-8
# Comparison in standard deviations between all the implemented randomization and complete randomization

set.seed(20200911, kind = "Mersenne-Twister",
         normal.kind = "Inversion", sample.kind = "Rejection")

# set parameters
nvec <- seq(200, 2000, 200)
cov_num <- 2
level_num <- c(2, 2)
pr <- rep(0.5, 4)
omega <- c(1, 2, 1, 1)
p <- 0.85
weight <- rep(1, 2)
a <- 3.0
bsize <- 4


# calculate standard deviations for all procedures
std_CR <- Std_CR(nvec, cov_num, level_num, pr)
std_HH <- Std(nvec, cov_num, level_num, pr, "HuHuCAR", omega, p)
std_PS <- Std(nvec, cov_num, level_num, pr, "PocSimMIN", weight, p)
std_S <- Std(nvec, cov_num, level_num, pr, "StrBCD", p)
std_Str <- Std(nvec, cov_num, level_num, pr, "StrPBR", bsize)
std_A <- Std(nvec, cov_num, level_num, pr, "AdjBCD", a)
std_D <- Std(nvec, cov_num, level_num, pr, "DoptBCD")

# generate a dataframe
Randomization <- rep(c("DoptBCD", "CR", "HuHuCAR", "PocSimMIN", "StrBCD", "StrPBR", "AdjBCD"),
                    each = length(nvec))
pat.num <- rep(nvec, times = 7)
comb <- t(cbind(std_D, std_CR, std_HH, std_PS, std_S, std_Str, std_A))
df <- data.frame("Randomization" = Randomization, "pat.num" = pat.num, comb,
                stringsAsFactors = TRUE)
rownames(df) <- NULL

# plot std at the overall level
p1 <- ggplot(
  data = df, mapping = aes(
    y = overall, x = pat.num, group = Randomization, color = Randomization,
    shape = Randomization, linetype = Randomization)) +
  geom_point(size = 2) +
  geom_line(size = 0.5, alpha = 1) +
  theme(plot.title = element_text(hjust = 0.5),
                 legend.position = "bottom") +
  xlab("Sample size") +
  ylab("SD of overall imbalances") +
  scale_colour_hue(l = 65) +
  scale_linetype_discrete() +
  scale_shape_manual(values = 19 : 25)

# plot std at the within-stratum level (strata (1, 1))
p2 <- ggplot(
  data = df, mapping = aes(
    y = level1, x = pat.num, group = Randomization, color = Randomization,
    shape = Randomization, linetype = Randomization)) +
  geom_point(size = 2) +
  geom_line(size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5),
                 legend.position = "bottom") +
  xlab("Sample size") +
  ylab("SD of within-str. imbalances (1, 1)") +
  scale_colour_hue(l = 65) +
  scale_linetype_discrete() +
  scale_shape_manual(values = 19 : 25)

# plot std at the within-stratum level (strata (2, 2))
p3 <- ggplot(
  data = df, mapping = aes(
    y = level4, x = pat.num, group = Randomization, color = Randomization,
    shape = Randomization, linetype = Randomization)) +
  geom_point(size = 2) +
  geom_line(size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5),
                 legend.position = "bottom") +
  xlab("Sample size") +
  ylab("SD of within-str. imbalances (2, 2)") +
  scale_colour_hue(l = 65) +
  scale_linetype_discrete() +
  scale_shape_manual(values = 19 : 25)

# plot std at the within-covariate-margin level (margin (1; 1))
p4 <- ggplot(
  data = df, mapping = aes(
    y = margin11, x = pat.num, group = Randomization, color = Randomization,
    shape = Randomization, linetype = Randomization)) +
  geom_point(size = 2) +
  geom_line(size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5),
                 legend.position = "bottom") +
  xlab("Sample size") +
  ylab("SD of within-cov.-margin imbalances (1; 1)") +
  scale_colour_hue(l = 65) +
  scale_linetype_discrete() +
  scale_shape_manual(values = 19 : 25)

# plot std at the within-covariate-margin level (margin (2; 2))
p5 <- ggplot(
  data = df, mapping = aes(
    y = margin22, x = pat.num, group = Randomization, color = Randomization,
    shape = Randomization, linetype = Randomization)) +
  geom_point(size = 2) +
  geom_line(size = 0.5) +
  theme(plot.title = element_text(hjust = 0.5),
                 legend.position = "bottom") +
  xlab("Sample size") +
  ylab("SD of within-cov.-margin imbalances (2; 2)") +
  scale_colour_hue(l = 65) +
  scale_linetype_discrete() +
  scale_shape_manual(values = 22 : 25)

# combine the plots p1, p4, and p2
position <- "bottom"
ncol <- 3
nrow <- 1

plots <- list(p1, p2, p4)

g <- ggplotGrob(
  plots[[1]] + theme(legend.position = position))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)
lwidth <- sum(legend$width)
gl <- lapply(plots, function(x) x +
              theme(legend.position="none"))
gl <- c(gl, ncol = ncol, nrow = nrow)

combined <- gridExtra::arrangeGrob(
  do.call(gridExtra::arrangeGrob, gl), legend, ncol = 1,
  heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight))

pdf("rand4.pdf", width = 10, height = 4)
grid::grid.draw(combined)
dev.off()

## Table-4
# Comparison of run-time of different computing tools: OpenMP, doSNOW, and R only

## Note that this script should run on a machine with at least 16 cores.

set.seed(
  20200911,
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)

# Power calculation using bootstrap test with doSNOW and foreach 

HuHuCAR_BT_power_Rp = function(n, cov_num, level_num, pr, 
                               type, beta, di, sigma, 
                               Iternum, sl, omega, p, B, ncores = 1, 
                               plot = FALSE){
  mu1 = di
  mu2 = rep(0,length(di))
  a = Sys.time()
  if(length(mu1) != length(mu2)){
    stop("The length of two mu's must match!")
  }
  
  if(plot!= TRUE && plot!= FALSE){
    stop("Please specify whether to plot or not! Enter TRUE or FALSE")
  }
  
  
  N = Iternum
  N1 = length(mu1)
  p_all = matrix(0, nrow = N1, ncol = N)
  
  ivec = rep(1 : N1, each = N)
  jvec = rep(1 : N, times = N1)
  
  cl = makeCluster(ncores)
  registerDoSNOW(cl)
  
  temp = foreach(i = ivec, j = jvec, .combine = "rbind", .packages = "AuxRparallel") %dopar% {
    data = AuxRparallel::HuHuCAR_getData(n, cov_num, level_num, pr, type, beta, mu1[i], mu2[i], 
                                  sigma, omega, p)
    pval = AuxRparallel::HuHuCAR_BT_In(data, B, omega, p)
    (pval < sl / 2) + 1 - 1
  }
  
  stopCluster(cl)
  
  p_all = matrix(temp, nrow = N1, ncol = N, byrow = TRUE)
  
  result = rep(NA, 2 * N1)
  for(i in 1 : N1){
    result[i] = sum(p_all[i, ]) / Iternum
    result[i + N1] = sqrt(var(p_all[i, ]))
  }
  
  if(plot == TRUE){
    tgg=data.frame(diff = di,value = result[1:length(di)],
                   sd = round(result[(length(di)+1):(2*length(di))], 
                              digit = 2))
    pic = ggplot(tgg, aes(x=di, y=value)) + 
      geom_line() + 
      geom_point(size=4, shape=20)+
      xlab("Difference in means")+
      ylab("Power")
    b = Sys.time()
    res = list(Powers = tgg, Plot = pic, 
               Time = paste(paste("Execute time:", 
                                  round(as.numeric(b-a), 
                                        digit = 2),units(b-a))))
    return(res)
  }else{
    tgg=data.frame(diff = di,value = result[1:length(di)],
                   sd = round(result[(length(di)+1):(2*length(di))], 
                              digit = 2))
    b = Sys.time()
    res = list(Powers = tgg,
               Time = paste(paste("Execute time:", 
                                  round(as.numeric(b-a), 
                                        digit = 2),units(b-a))))
    return(res)
  }
}

# parameters for codes by Ma et al. (2020) 
p <- 2
q <- 2
N <- 10
u0 <- 0
u1 <- 0.7
betagamma <- c(0.1, 0.4, 0.3, 0.1)
it_max <- 25600
tval_XZ_HH <- rep(NA, it_max)
pval_XZ_HH <- rep(NA, it_max)

n <- 10
cov_num <- 4
level_num <- rep(2, 4)
pr <- rep(0.5, 8)
type <- "linear"
beta <- c(0.1, 0.4, 0.3, 0.1, 0.3, 0.2, 0.1, 0.2)
di <- 0.1
sigma <- 1
Iternum <- 25600
sl <- 0.05
omega <- c(0.1, 0.1, 0.2, 0.2, 0.2, 0.2)
B <- 10

p <- microbenchmark(
  p2019 = lapply(1:it_max, cal_fn),
  openmp1 = caratOMP::evalPower(
    n = n, cov_num = cov_num, level_num = level_num, pr = pr, type = type,
    beta = beta, di = di, sigma = sigma, Iternum = Iternum, sl = sl,
    method = "HuHuCAR", test = "boot.test", plot = FALSE, omega = omega,
    p = 0.85, B = B, nthreads = 2
  ),
  openmp2 = caratOMP::evalPower(
    n = n, cov_num = cov_num, level_num = level_num, pr = pr, type = type,
    beta = beta, di = di, sigma = sigma, Iternum = Iternum, sl = sl,
    method = "HuHuCAR", test = "boot.test", plot = FALSE, omega = omega,
    p = 0.85, B = B, nthreads = 4
  ),
  openmp4 = caratOMP::evalPower(
    n = n, cov_num = cov_num, level_num = level_num, pr = pr, type = type,
    beta = beta, di = di, sigma = sigma, Iternum = Iternum, sl = sl,
    method = "HuHuCAR", test = "boot.test", plot = FALSE, omega = omega,
    p = 0.85, B = B, nthreads = 8
  ),
  openmp8 = caratOMP::evalPower(
    n = n, cov_num = cov_num, level_num = level_num, pr = pr, type = type,
    beta = beta, di = di, sigma = sigma, Iternum = Iternum, sl = sl,
    method = "HuHuCAR", test = "boot.test", plot = FALSE, omega = omega,
    p = 0.85, B = B, nthreads = 16
  ),
  openmp16 = caratOMP::evalPower(
    n = n, cov_num = cov_num, level_num = level_num, pr = pr, type = type,
    beta = beta, di = di, sigma = sigma, Iternum = Iternum, sl = sl,
    method = "HuHuCAR", test = "boot.test", plot = FALSE, omega = omega,
    p = 0.85, B = B, nthreads = 32
  ),
  rp1 = HuHuCAR_BT_power_Rp(
    n = n, cov_num = cov_num, level_num = level_num, pr = pr, type = type,
    beta = beta, di = di, sigma = sigma, Iternum = Iternum, sl = sl,
    omega = omega, p = 0.85, B = B, ncores = 1, plot = FALSE
  ),
  rp2 = HuHuCAR_BT_power_Rp(
    n = n, cov_num = cov_num, level_num = level_num, pr = pr, type = type,
    beta = beta, di = di, sigma = sigma, Iternum = Iternum, sl = sl,
    omega = omega, p = 0.85, B = B, ncores = 2, plot = FALSE
  ),
  rp4 = HuHuCAR_BT_power_Rp(
    n = n, cov_num = cov_num, level_num = level_num, pr = pr, type = type,
    beta = beta, di = di, sigma = sigma, Iternum = Iternum, sl = sl,
    omega = omega, p = 0.85, B = B, ncores = 4, plot = FALSE
  ),
  rp8 = HuHuCAR_BT_power_Rp(
    n = n, cov_num = cov_num, level_num = level_num, pr = pr, type = type,
    beta = beta, di = di, sigma = sigma, Iternum = Iternum, sl = sl,
    omega = omega, p = 0.85, B = B, ncores = 8, plot = FALSE
  ),
  rp16 = HuHuCAR_BT_power_Rp(
    n = n, cov_num = cov_num, level_num = level_num, pr = pr, type = type,
    beta = beta, di = di, sigma = sigma, Iternum = Iternum, sl = sl,
    omega = omega, p = 0.85, B = B, ncores = 16, plot = FALSE
  ),
  times = 10
)

p