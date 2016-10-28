install.packages("stringr", dependencies = T)
install.packages("Rfacebook", dependencies = T)
install.packages("genderdata", dependencies = T)
install.packages("gender", dependencies = T)
install.packages("wru", dependencies = T)
install.packages("foreach", dependencies = T)
install.packages("doParallel", dependencies = T)
install.packages("coda", dependencies = T)
install.packages("BCRA", dependencies = T)
library(rvest)
library(stringr)
library(knitr)
library(Rfacebook)
library(wru)
library(foreach)
library(doParallel)
library(coda)
library(BCRA)
library(gender)



#--- Getting Data from Facebook--- #
fb_oauth <- fbOAuth(app_id = "1286708611360084", app_secret = "c5a2e978bb2b0425f0d02990ae60f9db", extended_permissions = TRUE)
start.time <- Sys.time()
page <- getPage(page="thebreastcancersite", token=fb_oauth, n = 1000,   since='2016/09/15', until='2016/09/16')
post_id <- which(page$id == "7210086721_10153795081091722")
post <- getPost(post= page$id[post_id], n=2000, token=fb_oauth)
comments <- post$comments
#-- Cant have access to 241 user ---#
comments_users_info <- getUsers(comments$from_id[-241], token=fb_oauth)
unique_comments_users_info <- comments_users_info[!duplicated(comments_users_info$id), ]

length_unique <- nrow(unique_comments_users_info)
access_comments <- comments[-241,]
trim_access_comments <- access_comments$message[1: length_unique]
comments_users_info_append_comments <- cbind(unique_comments_users_info, trim_access_comments)
names_first <- comments_users_info_append_comments$first_name

#---Checking validation of gender package----#
# gender_pred_25 <-c() 
# for(i in 1 :25)
# {
#   length_check <- length(gender(comments_users_info_append_comments$first_name[i])$gender)
#   print(length_check)
#   if(length_check !=0)
#    gender_pred_25 <- c(gender_pred_25, gender(comments_users_info_append_comments$first_name[i])$gender) 
#   else
#     gender_pred_25 <- c(gender_pred_25,"NA")
# }
# pred_25 <- cbind(comments_users_info_append_comments$name[1:25], gender_pred_25)

race_pred_sur_25 <- as.data.frame(comments_users_info_append_comments$last_name[1:25])
colnames(race_pred_sur_25) <- c("surname")
race_pred_25 <- race.pred(voters = race_pred_sur_25, races = c("white", "black", "latino", "asian", "other"),surname.only = TRUE)

#--- Identifying which are females--#

check_genderdata_package()
#1
females <- function(x)
{
  f <- gender(x, method = "ssa")
  if(length(f$gender) != 0 && f$gender == "female")
   return(1)     
  else
    return(0)
}

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
which_females <- foreach(i = 1:length(names_first), .export = c("gender")) %dopar%
  females(names_first[i])
stopCluster(cl)
which_females <- which(as.numeric(unlist(which_females)) == 1)
female_users <- comments_users_info_append_comments[which_females,]

colnames(female_users)[12] <- c("comments")
fb_data <- female_users[c(1,2,4,6,11,12)]
colnames(fb_data) <- c("fb_id","name","first_name","last_name","picture_url","comments")
saveRDS(fb_data,file="fb_data.Rda")

fb_data <- readRDS(file = "fb_data.Rda")
comments_fb <- fb_data$comments
md_hist <- rep(0, length(comments_fb))
biopsy <- rep(0, length(comments_fb))
for(i in 1 : length(comments_fb))
{
  c2 <- grep("Survivor", comments_fb[i], ignore.case = T)
  c3 <- grep("Free", comments_fb[i], ignore.case = T)
  c4 <- grep("Diagnosed", comments_fb[i], ignore.case = T)
  c5 <- grep("Diagnosis", comments_fb[i], ignore.case = T)
  c6 <- grep("Cancer", comments_fb[i], ignore.case = T)
  c7 <- grep("Lung", comments_fb[i], ignore.case = T)
  c8 <- grep("Masectomy", comments_fb[i], ignore.case = T)
  c9 <- grep("Lumpectomy", comments_fb[i], ignore.case = T)
  c10 <- grep("Biopsy", comments_fb[i], ignore.case = T)
  c11 <- grep("Biopsies", comments_fb[i], ignore.case = T)
  #k <- length(c2) + length(c3) + length(c4) + length(c5) 
  #print(k)
  if(length(c2) + length(c3) + length(c4) + length(c5) + length(c6) >= 1)
    md_hist[i] = 1
  else
    md_hist[i] = 0
  
  if(length(c7) == 1)
    md_hist[i] = 0
  if(length(c8) + length(c9) + length(c10) >= 1)
    biopsy[i] = 1
  else
    biopsy[i] = 0
}
processed_data <- cbind(fb_data, md_hist, biopsy)
colnames(processed_data)[7:8] <- c("Medical_History", "Biopsy")

#setwd("~/Documents/Age/F")
h <- as.character(processed_data$first_name)
h <- paste0(h,"-")
name_age_list <- list()
name_age_calc <- function(x)
{
  index <- which(str_detect(list.files(pattern = "-stats.txt$", recursive = TRUE),x) == TRUE)
  if(length(index) != 0)
  {
    d <- read.table(list.files(pattern = "-stats.txt$", recursive = TRUE)[index], header = F)
    g <- as.character(d[2,])
    return(c(as.numeric(str_sub(g,6,9)), as.numeric(str_sub(g,1,4)), as.numeric(str_sub(g,11,14)))) 
  }
  
  else
    return(c(0,0,0))
  
}
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
name_age_list <- foreach(i = 1:length(h), .packages = c("stringr")) %dopar%
  name_age_calc(h[i])
stopCluster(cl)

m <- do.call(rbind, name_age_list)
m <- as.data.frame(m)
colnames(m) <- c("25percentile","Median","75percentile")
data1 <- cbind(processed_data, m)
lowlimit_age <- 2015 - data1[,11] 
uplimit_age <- 2015 - data1[,9]
median_age <- 2015 - data1[,10]
data1 <- cbind(data1, lowlimit_age, uplimit_age, median_age)

colnames(data1)[12:14] <- c("Lower_age", "Upper_age", "Median_age")
data1 <- data1[which(data1$Lower_age >= 35 & data1$Upper_age <= 90 & data1$Upper_age != 2015 & data1$Medical_History == 0),]


#-- Identifying race based on surnames --#
female_surnames <- as.data.frame(data1$last_name)
colnames(female_surnames) <- c("surname")
race_prediction <- race.pred(voters = female_surnames, races = c("white", "black", "latino", "asian", "other"),surname.only = TRUE)
target <- female_surnames$surname

order_r_p <- race_prediction[match(target,race_prediction$surname),]

order_r_p_without_surnames <- order_r_p[,-1] 
race <- c()
for(i in 1 : nrow(order_r_p))
{
  if(which(order_r_p_without_surnames[i,] == max(order_r_p_without_surnames[i,])) == 1)
    race <- c(race,"White")
  if(which(order_r_p_without_surnames[i,] == max(order_r_p_without_surnames[i,])) == 2)
    race <- c(race,"Black")
  if(which(order_r_p_without_surnames[i,] == max(order_r_p_without_surnames[i,])) == 3)
    race <- c(race,"Hispanic")
  if(which(order_r_p_without_surnames[i,] == max(order_r_p_without_surnames[i,])) == 4)
    race <- c(race,"Asian")
  if(which(order_r_p_without_surnames[i,] == max(order_r_p_without_surnames[i,])) == 5)
    race <- c(race,"Others")
}
data1 <- cbind(data1,race)
colnames(data1)[15] <- c("Race")
code_fun <- function(x)
{
  if(x == "White")
    return(1)
  if(x == "Black")
    return(2)
  if(x == "Hispanic")
    return(3)
  if(x == "Asian")
    return(11)
}
Race <- apply(as.data.frame(data1$Race), 1, code_fun)
data1$Race <- Race

#---- Range of Race-------
race_high_prob <- function(x)
{
  return(which(as.matrix(x) > 0.001))
}
Race_individual <- apply(as.matrix(order_r_p[,-1]), 1, race_high_prob)

race_code <- function(x)
{
  if(length(which(x == 4)) > 0)
   x[which(x == 4)] = 11
  if(length(which(x == 5)) > 0)
    x = x[-which(x == 5)]
  return(x)
}

Race_code_individual <- lapply(Race_individual, race_code)

ID <- seq(1, nrow(data1), 1)
T1 <- data1$Median_age
T2_90 <- rep(90, nrow(data1))
T2_5 <- T1 + 5
T2_5 <- ifelse(T2_5 <= 90, T2_5, 90)
N_Biop <- rep(99, nrow(data1))
HypPlas <- rep(99, nrow(data1))
AgeMen <- rep(99, nrow(data1))
Age1st <- rep(99, nrow(data1))
N_Rels <- rep(99, nrow(data1))
mp_race <- data1$Race
Individual_5 <- cbind(ID, T1, T2_5, N_Biop, HypPlas, AgeMen, Age1st, N_Rels, mp_race)
colnames(Individual_5) <- c("ID", "T1", "T2", "N_Biop", "HypPlas", "AgeMen", "Age1st", "N_Rels", "Race")
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
registerDoParallel(cl)
Est_absolute_fiveyear_risk_ma_mpr <- foreach(i = 1:nrow(data1),.packages = ("BCRA")) %dopar%
  absolute.risk(as.data.frame(t(Individual_5[i,])), iloop = 1)
stopCluster(cl)

Est_gail_score <- as.data.frame(cbind(data1$name, data1$Median_age, data1$Race, as.numeric(Est_absolute_fiveyear_risk_ma_mpr)))
colnames(Est_gail_score) <- c("Name", "Median_age", "Most_probable_race", "Gail_score")
High_risk_individuals <- Est_gail_score[which(as.numeric(Est_absolute_fiveyear_risk_ma_mpr) >= 1.66),]
print(nrow(High_risk_individuals))
Individual_age <- list()
for(i in 1: nrow(data1))
{
  Range_age <- seq(data1$Lower_age[i], data1$Upper_age[i],1)
  m <- matrix(0, nrow = length(Range_age), ncol = 5)
  ID <- seq(1, length(Range_age),1)
  T2_90 <- rep(90, length(Range_age))
  T2_5 <- Range_age + 5
  T2_5 <- ifelse(T2_5 <= 90, T2_5, 90)
  N_Biop <- rep(99, length(Range_age))
  HypPlas <- rep(99, length(Range_age))
  AgeMen <- rep(99, length(Range_age))
  Age1st <- rep(99, length(Range_age))
  N_Rels <- rep(99, length(Range_age))
  race_i <- rep(data1$Race[i],length(Range_age) )
  final_data_five <- as.data.frame(cbind(ID, Range_age, T2_5, N_Biop, HypPlas, AgeMen, Age1st, N_Rels, race_i))
  colnames(final_data_five) <- c("ID", "T1", "T2", "N_Biop", "HypPlas", "AgeMen", "Age1st", "N_Rels", "Race")
  final_data_ninety <- as.data.frame(cbind(ID, Range_age, T2_90, N_Biop, HypPlas, AgeMen, Age1st, N_Rels, race_i))
  colnames(final_data_ninety) <- c("ID", "T1", "T2", "N_Biop", "HypPlas", "AgeMen", "Age1st", "N_Rels", "Race")
  Abs_risk_i_five <- absolute.risk(final_data_five, iloop = 1)
  Abs_avg_risk_i_five <- absolute.risk(final_data_five, iloop = 2)
  Abs_risk_i_ninety <- absolute.risk(final_data_ninety, iloop = 1)
  Abs_avg_risk_i_ninety <- absolute.risk(final_data_ninety, iloop = 2)
  m[,1] <- Abs_risk_i_five
  m[,2] <- Abs_avg_risk_i_five
  m[,3] <- Abs_risk_i_ninety
  m[,4] <- Abs_avg_risk_i_ninety
  m[,5] <- Range_age
  Individual_age[[i]] <- m
}

list_matrix_age <- do.call(rbind, Individual_age)
min_max_age <- c(min(list_matrix_age[,1]), min(list_matrix_age[,2]), min(list_matrix_age[,3]), min(list_matrix_age[,4]), max(list_matrix_age[,1]), max(list_matrix_age[,2]), 
             max(list_matrix_age[,3]), max(list_matrix_age[,4]))


x_names <- c("Absolute risk(5 years)","Average absolute risk(5 years)", "Lifetime absolute risk","Average lifetime risk")
age_min <- min(list_matrix_age[,5])
age_max <- max(list_matrix_age[,5])

#pdf("myOut1.pdf")
#for(j in 1:4)
#{
#   par(mfrow = c(2,2))
#   for(i in 1 :4)
#   {
#     hist(Individual_age[[i]][,j],probability=TRUE, main = paste(data1$first.name[i]), xlab = paste(x_names[j]))
#     lines(density(Individual_age[[i]][,j]),col="red")
#   }
# }
#dev.off()



for(j in 1:4)
{
  pdf(paste0("pic_", j, ".pdf"))
  par(mfrow = c(2,2))
  for(i in 1 :4)
  {
    median_abs_risk_age <- which(Individual_age[[i]][,5] == data1$Median_age[i] ) 
    plot(Individual_age[[i]][,5], Individual_age[[i]][,j], main = paste(data1$first_name[i]), ylab = paste(x_names[j]), xlab = "Age", type = "l", col = "red", 
         ylim = c(min_max_age[j],min_max_age[j+4]))
    points(data1$Median_age[i], Individual_age[[i]][median_abs_risk_age,j], col = "blue", pch = 16 )
    legend("topright", paste("Median age"), col = c("blue"),
           pch = 16, bty = "n")
  }
  dev.off()
}
#dev.off()




# Individual_race <- list()
# for(i in 1: nrow(data1))
# {
#   Range_race <- Race_code_individual[[i]]
#   age_i <- rep(data1$Median_age[i],length(Range_race))
#   m <- matrix(0, nrow = length(Range_race), ncol = 5)
#   ID <- seq(1, length(Range_race),1)
#   T2_90 <- rep(90, length(Range_race))
#   T2_5 <- age_i + 5
#   T2_5 <- ifelse(T2_5 <= 90, T2_5, 90)
#   N_Biop <- rep(99, length(Range_race))
#   HypPlas <- rep(99, length(Range_race))
#   AgeMen <- rep(99, length(Range_race))
#   Age1st <- rep(99, length(Range_race))
#   N_Rels <- rep(99, length(Range_race))
#   final_data_five <- as.data.frame(cbind(ID, age_i, T2_5, N_Biop, HypPlas, AgeMen, Age1st, N_Rels, Range_race))
#   colnames(final_data_five) <- c("ID", "T1", "T2", "N_Biop", "HypPlas", "AgeMen", "Age1st", "N_Rels", "Race")
#   final_data_ninety <- as.data.frame(cbind(ID, age_i, T2_90, N_Biop, HypPlas, AgeMen, Age1st, N_Rels, Range_race))
#   colnames(final_data_ninety) <- c("ID", "T1", "T2", "N_Biop", "HypPlas", "AgeMen", "Age1st", "N_Rels", "Race")
#   Abs_risk_i_five <- absolute.risk(final_data_five, iloop = 1)
#   Abs_avg_risk_i_five <- absolute.risk(final_data_five, iloop = 2)
#   Abs_risk_i_ninety <- absolute.risk(final_data_ninety, iloop = 1)
#   Abs_avg_risk_i_ninety <- absolute.risk(final_data_ninety, iloop = 2)
#   m[,1] <- Abs_risk_i_five
#   m[,2] <- Abs_avg_risk_i_five
#   m[,3] <- Abs_risk_i_ninety
#   m[,4] <- Abs_avg_risk_i_ninety
#   m[,5] <- Range_race
#   Individual_race[[i]] <- m
# }

list_matrix_race <- do.call(rbind, Individual_race)
min_max_race <- c(min(list_matrix_race[,1]), min(list_matrix_race[,2]), min(list_matrix_race[,3]), min(list_matrix_race[,4]), max(list_matrix_race[,1]), max(list_matrix_race[,2]), 
             max(list_matrix_race[,3]), max(list_matrix_race[,4]))


# for(j in 1:4)
# {
#   pdf(paste0("pic_race_",j,".pdf"))
#   par(mfrow = c(2,2))
#   for(i in 1 :4)
#   {
#     most_probable_race <- which(Individual_race[[i]][,5] == data1$Race[i] ) 
#     plot(Individual_race[[i]][,5], Individual_race[[i]][,j], main = paste(data1$first_name[i]), ylab = paste(x_names[j]), xlab =xlab = "Race: 1-Wh, 2-Bl, 3-His and 11-Asi", col = "red", pch = 16,  
#          ylim = c(min_max_race[j],min_max_race[j+4]))
#     points(data1$Race[i], Individual_race[[i]][most_probable_race,j], col = "blue", pch = 16)
#     legend("topright", paste("Most probable race"), col = c("blue"),
#            pch = 16)
#   }
#   dev.off()
# }






Individual_age_race_5 <- list()
for(i in 1: nrow(data1))
{
  Range_race <- Race_code_individual[[i]]
  Range_age <- seq(data1$Lower_age[i], data1$Upper_age[i],1)
  Range_age_race <- expand.grid(Range_age, Range_race)
  colnames(Range_age_race) <- c("Age", "Race")
  T1 <- Range_age_race$Age
  R <- Range_age_race$Race
  m <- matrix(0, nrow = nrow(Range_age_race), ncol = 6)
  ID <- seq(1, nrow(Range_age_race), 1)
  T2_5 <- T1 + 5
  T2_5 <- ifelse(T2_5 <= 90, T2_5, 90)
  N_Biop <- rep(99, nrow(Range_age_race))
  HypPlas <- rep(99, nrow(Range_age_race))
  AgeMen <- rep(99, nrow(Range_age_race))
  Age1st <- rep(99, length(Range_race))
  N_Rels <- rep(99, nrow(Range_age_race))
  final_data_five <- as.data.frame(cbind(ID, T1, T2_5, N_Biop, HypPlas, AgeMen, Age1st, N_Rels, R))
  colnames(final_data_five) <- c("ID", "T1", "T2", "N_Biop", "HypPlas", "AgeMen", "Age1st", "N_Rels", "Race")
  Individual_age_race_5[[i]] <- final_data_five
}

Individual_age_race_90 <- list()
for(i in 1: nrow(data1))
{
  Range_race <- Race_code_individual[[i]]
  Range_age <- seq(data1$Lower_age[i], data1$Upper_age[i],1)
  Range_age_race <- expand.grid(Range_age, Range_race)
  colnames(Range_age_race) <- c("Age", "Race")
  T1 <- Range_age_race$Age
  R <- Range_age_race$Race
  m <- matrix(0, nrow = nrow(Range_age_race), ncol = 6)
  ID <- seq(1, nrow(Range_age_race), 1)
  T2_90 <- rep(90, nrow(Range_age_race))
  N_Biop <- rep(99, nrow(Range_age_race))
  HypPlas <- rep(99, nrow(Range_age_race))
  AgeMen <- rep(99, nrow(Range_age_race))
  Age1st <- rep(99, length(Range_race))
  N_Rels <- rep(99, nrow(Range_age_race))
  final_data_ninety <- as.data.frame(cbind(ID, T1, T2_90, N_Biop, HypPlas, AgeMen, Age1st, N_Rels, R))
  colnames(final_data_ninety) <- c("ID", "T1", "T2", "N_Biop", "HypPlas", "AgeMen", "Age1st", "N_Rels", "Race")
  Individual_age_race_90[[i]] <- final_data_ninety
}
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
registerDoParallel(cl)
n <- length(Individual_age_race_5)
Est_absolute_fiveyear_risk <- foreach(i = 1:n,.packages = ("BCRA")) %dopar%
  absolute.risk(Individual_age_race_5[[i]], iloop = 1)
Est_avg_absolute_fiveyear_risk <- foreach(i = 1:n,.packages = ("BCRA")) %dopar%
  absolute.risk(Individual_age_race_5[[i]], iloop = 2)
Est_absolute_lifetime_risk <- foreach(i = 1:n,.packages = ("BCRA")) %dopar%
  absolute.risk(Individual_age_race_90[[i]], iloop = 1)
Est_avg_absolute_lifetime_risk <- foreach(i = 1:n,.packages = ("BCRA")) %dopar%
  absolute.risk(Individual_age_race_90[[i]], iloop = 2)
stopCluster(cl)



quant <- function(x)
{ return(quantile(x, probs = c(0.025, 0.975)))}

hpd <- function(x)
{
  HPDinterval(as.mcmc(x), prob=0.95)
}

quant_absolute_fiveyear_risk <- lapply(Est_absolute_fiveyear_risk, quant)
quant_avg_absolute_fiveyear_risk <- lapply(Est_avg_absolute_fiveyear_risk, quant)
quant_absolute_lifetime_risk <- lapply(Est_absolute_lifetime_risk, quant)
quant_avg_absolute_lifetime_risk <- lapply(Est_avg_absolute_lifetime_risk, quant)


# hpd_absolute_fiveyear_risk <- lapply(Est_absolute_fiveyear_risk, hpd)
# hpd_avg_absolute_fiveyear_risk <- lapply(Est_avg_absolute_fiveyear_risk, hpd)
# hpd_absolute_lifetime_risk <- lapply(Est_absolute_lifetime_risk, hpd)
# hpd_avg_absolute_lifetime_risk <- lapply(Est_avg_absolute_lifetime_risk, hpd)


list_matrix_quant_absolute_fiveyear_risk <- do.call(rbind, quant_absolute_fiveyear_risk)
list_matrix_quant_avg_absolute_fiveyear_risk <- do.call(rbind, quant_avg_absolute_fiveyear_risk)
list_matrix_quant_absolute_lifetime_risk <- do.call(rbind, quant_absolute_lifetime_risk)
list_matrix_quant_avg_absolute_lifetime_risk <- do.call(rbind, quant_avg_absolute_lifetime_risk)

# list_matrix_hpd_absolute_fiveyear_risk <- do.call(rbind, hpd_absolute_fiveyear_risk)
# list_matrix_hpd_avg_absolute_fiveyear_risk <- do.call(rbind, hpd_avg_absolute_fiveyear_risk)
# list_matrix_hpd_absolute_lifetime_risk <- do.call(rbind, hpd_absolute_lifetime_risk)
# list_matrix_hpd_avg_absolute_lifetime_risk <- do.call(rbind, hpd_avg_absolute_lifetime_risk)
full_names <- as.character(data1$name)

Confidence_Interval <- as.data.frame(cbind(full_names,list_matrix_quant_absolute_fiveyear_risk, list_matrix_quant_avg_absolute_fiveyear_risk, list_matrix_quant_absolute_lifetime_risk, list_matrix_quant_avg_absolute_lifetime_risk))
#HPD_Confidence_Interval <- as.data.frame(cbind(full_names,list_matrix_hpd_absolute_fiveyear_risk, list_matrix_hpd_avg_absolute_fiveyear_risk, list_matrix_hpd_absolute_lifetime_risk, list_matrix_hpd_avg_absolute_lifetime_risk))

end.time <- Sys.time()
end.time - start.time
