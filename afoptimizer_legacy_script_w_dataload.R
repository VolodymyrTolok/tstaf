#Algorithm for pairs creation - hedging and netting
#rm(list=ls())
#ptm <- proc.time()
#Starting_testing <- Sys.time() 
#source("F-F functions.r")
#sink("haircut_output.txt") 

#stream_constant <- f_conversion$stream_field[1]
#browser()
stream_iterator <- 'MSUSA'
log_extraction_obj <- 'Y'

library(readr)
f_conversion <- read_delim("f_conversion.tsv.txt", 
                               delim = "\t", escape_double = FALSE, 
                               trim_ws = TRUE)
securities_gov <- read_delim("securities_gov.tsv.txt", 
                             delim = "\t", escape_double = FALSE, 
                             trim_ws = TRUE)
securities <- read_delim("securities.tsv.txt", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)
other <- read_delim("other.tsv.txt", delim = "\t", 
                    escape_double = FALSE, trim_ws = TRUE)
futures_gov <- read_delim("futures_gov.tsv.txt", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)



securities <- securities[securities$stream_field_sec==stream_iterator,]
securities_gov <- securities_gov[securities_gov$stream_field_sec_gov==stream_iterator,]
f_conversion <- f_conversion[f_conversion$stream_field_f_conv==stream_iterator,]
futures_gov <- futures_gov[futures_gov$stream_field_f_gov==stream_iterator,]
if(nrow(other)>1) 
{other <- other[other$stream_field_other==stream_iterator,]}

options(digits=10)
if(is.null(log_extraction_obj)){prnt_f <- "N"} else {prnt_f   <- log_extraction_obj}
ContrAmt <- other$control_amt
pmd_flag <- other$primary_dealer_flag
if (pmd_flag == "Y"){
  Pdealer <- 0.75
} else {
  Pdealer <- 1
}

#Values for variable distrib: if there are no F positions, then 1; if there are no A positions then 2; if both are absent then 0; if both are present then 3.
if (nrow(securities) < 1 & nrow(securities_gov) < 1){
  distrib <- 0
} else if (nrow(securities) < 1 & nrow(securities_gov) >= 1){
  distrib <- 1
} else if (nrow(securities) >= 1 & nrow(securities_gov) < 1){
  distrib <- 2
} else {
  distrib <- 3
}
if (prnt_f == "Y"){print( "SContrAmt: "); print( ContrAmt); print("pmd_flag: "); print(pmd_flag); print("distrib: "); print( distrib);}
#Step 1 Section: A Data Preparation
securities$corp_mv <- round(securities$corp_mv, digits=2)
unique_id <- securities$unique_id
CorpCurCode <- securities$original_ccy_code
CorpMatur <- securities$corpmaturity_mnt
corp_mv <- securities$corp_mv
mv_rem <- abs(corp_mv)
mv_hff <- rep(0, times=length(securities$unique_id))
mv_h2fa <- rep(0, times=length(securities$unique_id))
securities <- data.frame(unique_id, CorpCurCode, CorpMatur, corp_mv, mv_rem, mv_hff, mv_h2fa, stringsAsFactors=FALSE)
securities <- securities[order(securities$CorpMatur, decreasing = FALSE ),]
tbl_FFPairs <- matrix(c(0, 0, 0, 0.02, 0, 0, 0, 0), nrow=1,ncol=8)
#Assign column names in pairs table
colnames(tbl_FFPairs) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id")
rownames(tbl_FFPairs) <- "dummy"
PairId   <- 10000000
FAPairId <- 1
Frates <- c(0.02, 0.03, 0.05, 0.06, 0.07, 0.075, 0.08, 0.085, 0.09)
AratesAdj <- Pdealer * c(0, 0.015, 0.02, 0.03, 0.04, 0.045, 0.05, 0.055, 0.06, 0, 0.005, 0.0075, 0.01)
rltv_mat   <- c(6, 9, 24, 120)
ff_x_hrcut <- c(0.0175, 0.03, 0.0325, 0.035)
vec_x51a <- c(1, 1, 1, 1, 2,  3,   4,   4,   4)
vec_x51b <- c(6, 6, 6, 6, 9, 24, 120, 120, 120)
vec_x51c <- c(0.015, 0.015, 0.015, 0.015, 0.025, 0.0275, 0.03, 0.03, 0.03)
CatListA <- list(c(10,11,12,13),c(2,3),c(4,5),c(6,7,8,9))
#Divide F securities into 4 hedging groups and by sides; also sorting all subsets used in hedging calculations
FhgrL <- list()
CorpHedgegrS <- list()
HdgTime <- c(0, 60, 120, 180, 10^9)
for (i in 1:4){
  FhgrL[[i]] <- subset(securities, corp_mv >=0 & CorpMatur >= HdgTime[i]  & CorpMatur < HdgTime[i+1] )
  FhgrL[[i]] <- FhgrL[[i]][order(FhgrL[[i]]$CorpMatur, decreasing = FALSE ),]
  CorpHedgegrS[[i]] <- subset(securities, corp_mv < 0 & CorpMatur >= HdgTime[i]  & CorpMatur < HdgTime[i+1] )
  CorpHedgegrS[[i]] <- CorpHedgegrS[[i]][order(CorpHedgegrS[[i]]$CorpMatur, decreasing = FALSE ),]
}
#Step 1 Section B: Function for F-F hedging
f_to_f <- function(x_HgrLn, x_HgrSh, MtrDstnc, x_MtchPair, x_hrcut, PairId, tbl_FFPairs){
  v_i <- nrow(x_HgrLn) 
  v_j <- nrow(x_HgrSh)
  while (x_HgrLn[v_i,"mv_rem"] < 0.01){v_i <- (v_i - 1)}
  while (x_HgrSh[v_j,"mv_rem"] < 0.01){v_j <- (v_j - 1)}
  var_exit <- 0
  while (v_i >= 1 & v_j >= 1 & var_exit == 0){
    if (round(x_HgrLn[v_i,"mv_rem"], digits = 2) < 0.01 || round(x_HgrSh[v_j,"mv_rem"], digits = 2) < 0.01){
      if (x_HgrLn[v_i,"mv_rem"] < 0.01){
        while (x_HgrLn[v_i,"mv_rem"] < 0.01){
          v_i <- (v_i - 1)
          if (v_i < 1){
            var_exit <- 1
            break
          }
        }
      }
      if (x_HgrSh[v_j,"mv_rem"] < 0.01){
        while (x_HgrSh[v_j,"mv_rem"] < 0.01){
          v_j <- (v_j - 1)
          if (v_j < 1){var_exit <- 1; 
          break}
        }
      } 
    } else {
      if (round(abs(x_HgrLn[v_i,"CorpMatur"] - x_HgrSh[v_j,"CorpMatur"]), digits = 2) <= MtrDstnc){
        match_amt <- min(c(x_HgrLn[v_i,"mv_rem"], x_HgrSh[v_j,"mv_rem"]))
        matching_pair <- c(nrow(tbl_FFPairs), x_HgrLn[v_i,"unique_id"], x_HgrSh[v_j,"unique_id"], match_amt, x_HgrLn[v_i,"CorpMatur"], x_HgrSh[v_j,"CorpMatur"], x_hrcut, PairId)
        PairId <- (PairId + 1)
        tbl_FFPairs <- rbind(tbl_FFPairs, matching_pair)
        x_HgrLn[v_i,"mv_hff"]  <- round(x_HgrLn[v_i,"mv_hff"] + match_amt,  digits = 2)
        x_HgrSh[v_j,"mv_hff"] <- round(x_HgrSh[v_j,"mv_hff"] + match_amt, digits = 2)
        x_HgrLn[v_i,"mv_rem"] <- round(x_HgrLn[v_i,"mv_rem"] - match_amt, digits = 2)
        x_HgrSh[v_j,"mv_rem"] <- round(x_HgrSh[v_j,"mv_rem"] - match_amt, digits = 2) 
        if (x_HgrLn[v_i,"mv_rem"] < 0.01){v_i <- (v_i - 1)}
        if (x_HgrSh[v_j,"mv_rem"] < 0.01){v_j <- (v_j - 1)}
        #Line 30
        rownames(tbl_FFPairs)[nrow(tbl_FFPairs)] <- x_MtchPair
      } else {
        if (x_HgrLn[v_i,"CorpMatur"] >= x_HgrSh[v_j,"CorpMatur"]){
          v_i <- (v_i - 1)
        } else {
          v_j <- (v_j - 1)
        }
      }
    }
  } 
  xyz <- list("longcat" = x_HgrLn, "shortcat" =  x_HgrSh, "pairtbl" = tbl_FFPairs)
  remove(x_HgrLn, x_HgrSh, tbl_FFPairs)
  return(xyz)
}
#Step 1 Section C: F-F hedging function run
print("Step 1 Function Run")
if (distrib >= 2){
  for (i in 4:1) {
    if (nrow(FhgrL[[i]]) > 0 & nrow(CorpHedgegrS[[i]]) > 0){
      if (prnt_f == "Y"){paste("Function f_to_f run for group ",i,sep="")}
      xyz <- f_to_f(FhgrL[[i]], CorpHedgegrS[[i]], rltv_mat[i], paste("ff_matching_pair_gr",i,sep=""), ff_x_hrcut[i], PairId, tbl_FFPairs)
      FhgrL[[i]] <- xyz[["longcat"]]
      CorpHedgegrS[[i]] <- xyz[["shortcat"]]
      tbl_FFPairs <- xyz[["pairtbl"]]
      PairId <- (tbl_FFPairs[nrow(tbl_FFPairs), "pair_id"] + 1)
      remove(xyz)
    }
  }
}
corpsec_hedgegr <- FhgrL[[4]]
corpsec_hedgegr <- rbind(corpsec_hedgegr, CorpHedgegrS[[4]])
for (i in 1:3) {
  corpsec_hedgegr <- rbind(corpsec_hedgegr, FhgrL[[i]])
  corpsec_hedgegr <- rbind(corpsec_hedgegr, CorpHedgegrS[[i]])
}
print(class(tbl_FFPairs))
#Step 2.1 Section A: Data Preparation
securities_gov$gov_mv <- round(securities_gov$gov_mv, digits=2) 
unique_id_gov <- securities_gov$unique_id_gov
UsgCurCode <- securities_gov$original_ccy_code_gov
UsgMatur <- securities_gov$govmaturity_mnt
gov_mv <- securities_gov$gov_mv
mv_gov_rem <- abs(gov_mv);
securities_gov <- data.frame(unique_id_gov, UsgCurCode, UsgMatur, gov_mv, mv_gov_rem, stringsAsFactors=FALSE);
securities_gov <- securities_gov[order(securities_gov$UsgMatur, decreasing = FALSE ),]
#Split F into bands according to maturity, sort them and refresh
FsecBnd_Ln <- list()
FsecBnd_Sh <- list()
#Maturity Borders for F Securities
F_MtrBrd <- c(0, 12,24,36,60,120,180,240,300,10^9)

for (i in 1:9) {
  FsecBnd_Ln[[i]] <- subset(corpsec_hedgegr, CorpMatur >= F_MtrBrd[i]  & CorpMatur < F_MtrBrd[i+1] & corp_mv >=0 )
  FsecBnd_Ln[[i]] <- FsecBnd_Ln[[i]][order(FsecBnd_Ln[[i]]$CorpMatur, decreasing = FALSE ),]
  FsecBnd_Sh[[i]] <- subset(corpsec_hedgegr, CorpMatur >= F_MtrBrd[i]  & CorpMatur < F_MtrBrd[i+1] & corp_mv < 0 )
  FsecBnd_Sh[[i]] <- FsecBnd_Sh[[i]][order(FsecBnd_Sh[[i]]$CorpMatur, decreasing = FALSE ),]
}
remove(F_MtrBrd)
#Sort F hedge groups
for (i in 1:4) {
  FhgrL[[i]] <- FhgrL[[i]][order(FhgrL[[i]]$CorpMatur, decreasing = FALSE ),]
  CorpHedgegrS[[i]] <- CorpHedgegrS[[i]][order(CorpHedgegrS[[i]]$CorpMatur, decreasing = FALSE ),]
}
# A Subgroups declaration
AsecBnd <- list()
AsecBnd_Ln <- list()
AsecBnd_Sh <- list()
#Maturity Borders for A Securities
A_MtrBrd <- c(0,3,6,9,12,24,36,60,120,180,240,300,10^9)
#Divide A securities by haircut subcategories (maturity 0 - 12 months)
for (i in 1:4) {AsecBnd[[i+9]]  <- subset(securities_gov, UsgMatur >= A_MtrBrd[i] & UsgMatur < A_MtrBrd[i+1])}
#Divide A securities by haircut subcategories (maturity over 12 months)
for (i in 2:9) {AsecBnd[[i]] <- subset(securities_gov, UsgMatur >= A_MtrBrd[i+3] & UsgMatur < A_MtrBrd[i+4])}
for (i in 2:13){}
#Divide haircut subcategories of A securities by side - long or short - and sort them
for (i in 2:13) {
  AsecBnd_Ln[[i]] <- subset(AsecBnd[[i]],   gov_mv >=0)
  AsecBnd_Ln[[i]] <- AsecBnd_Ln[[i]][order(AsecBnd_Ln[[i]]$UsgMatur, decreasing = FALSE ),]
  AsecBnd_Sh[[i]] <- subset(AsecBnd[[i]],   gov_mv < 0)
  AsecBnd_Sh[[i]] <- AsecBnd_Sh[[i]][order(AsecBnd_Sh[[i]]$UsgMatur, decreasing = FALSE ),]
}
A_SubgrBal <- rep(0, times = 13)
# A subcategory net balance
for (i in 13:2){A_SubgrBal[i]  <- as.double(sum(AsecBnd_Ln[[i]]$mv_gov_rem) - sum(AsecBnd_Sh[[i]]$mv_gov_rem))}
tbl_FAPairs <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1,ncol=10)
colnames(tbl_FAPairs) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "FOrigID", "f_sec_side", "HcutRate", "pair_id")
#"f_sec_side" column will have 1 designating long and 0 designating short side
rownames(tbl_FAPairs) <- "dummy"
#Step 2.1 Section B: Functions preparation
#F-security push-down function: This function is in charge of moving F security down to higher maturity by re-hedging
f_movement <- function(balance_table_band, v20_CorpHedgegrIn, v20_CorpHedgegrOut, v20_UsgHedgeband, xf_HdgSdId, xf_NhgSdId, x_NhdgSideMtr, var_curfsec_amt, ContrAmt, var_i1, var_j1, tbl_FFPairs, tbl_FAPairs, x_fhedge_side, v_Exit0, MtrDstnc, x_hrcut, x_hrcut_fa, funcnumber){
  # var_exit1 variable is an "exit" variable for the next while loop; funcnumber: 2 for movement in step 2.1 and 3 for movement in step 3.0.
  var_exit1 <- 0; v1_FAPairId <- (max(tbl_FAPairs[,"pair_id"]) + 1); x_pair_id <- (max(tbl_FFPairs[,"pair_id"]) + 1)
  var_k1 <- (var_j1 + 1)
  while (var_curfsec_amt >= ContrAmt & abs(balance_table_band) >= ContrAmt & var_exit1 == 0){
    if (prnt_f == "Y"){print("J16_1")}
    if (var_i1 == 1 & v20_UsgHedgeband[1, "mv_gov_rem"] < 0.01){
      v_Exit0 <- 1; print("J16_2") 
      break
    }
    TransAmt <- min(c(abs(balance_table_band), v20_CorpHedgegrIn[var_j1, "mv_rem"]))
    var_l1 <- var_j1
    var_k1 <- (var_j1 + 1)
    #Start of the F moving down re-hedging and A lifting cycle
    v20_CorpHedgegrOut_t <- v20_CorpHedgegrOut
    v20_CorpHedgegrIn_t <- v20_CorpHedgegrIn
    tbl_OldPairs <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1, ncol=10)
    colnames(tbl_OldPairs) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id", "GrNum", "PairNum")
    tbl_NewPairs <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1,ncol=10)
    colnames(tbl_NewPairs) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id", "GrNum", "PairNum")
    var_m2 <- 1; var_exit2 <- 0; # "var_m2" is group number, "var_m" is pair number
    if (var_k1 <= nrow(v20_CorpHedgegrIn_t)){
      if (prnt_f == "Y"){print("J16_3")}
      while (var_l1 >= 1 & var_l1 < nrow(v20_CorpHedgegrIn_t) & (v20_CorpHedgegrIn_t[var_k1, "CorpMatur"] - v20_UsgHedgeband[var_i1, "UsgMatur"]) <= MtrDstnc & v20_CorpHedgegrIn_t[var_k1, "mv_hff"] >= ContrAmt & TransAmt >= ContrAmt & var_exit2 == 0){
        #Start of the F moving down re-hedging, one cycle equals to moving F security down one position
        if (prnt_f == "Y"){print("J16_4")}
        var_x <- length(which(tbl_FFPairs[,xf_HdgSdId] == v20_CorpHedgegrIn_t[var_k1, "unique_id"]))
        tbl_Rehedge <- matrix(tbl_FFPairs[which(tbl_FFPairs[,xf_HdgSdId] == v20_CorpHedgegrIn_t[var_k1, "unique_id"]),], var_x, 8);
        colnames(tbl_Rehedge) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id")
        if (var_x > 1){
          if (prnt_f == "Y"){print("J16_5")}
          tbl_Rehedge <- tbl_Rehedge[order(tbl_Rehedge[ , x_NhdgSideMtr], decreasing = FALSE ),]
        }
        var_m <- 1
        transfer_amount_2 <- 0
        MaxRmTransAmt <- TransAmt; # MaxRmTransAmt and transfer_amount2 are temporary variables only used for re-hedging cycle in while loop below
        while (var_m <= nrow(tbl_Rehedge) & MaxRmTransAmt >= ContrAmt){
          if (prnt_f == "Y"){print("J16_6")}
          opposite_sec_id <- tbl_Rehedge[var_m, xf_NhgSdId]
          if (abs(balance_table_band) >=   ContrAmt  & abs(v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id), "CorpMatur"] - v20_CorpHedgegrIn_t[var_l1, "CorpMatur"]) <= MtrDstnc){
            if (prnt_f == "Y"){print("J16_7")}
            CurTransAmt <- min(c(MaxRmTransAmt, tbl_Rehedge[var_m, "MatchAmt"]))
            transfer_amount_2 <- round((transfer_amount_2 + CurTransAmt), digits = 2)
            existing_pair <- c(tbl_Rehedge[var_m, ], var_m2, var_m)
            if (funcnumber == 2){
              if (prnt_f == "Y"){print("J16_8")}
              if (balance_table_band >= 0){
                new_pair <- c(0, (v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id), "unique_id"]), (v20_CorpHedgegrIn_t[var_l1, "unique_id"]), CurTransAmt, (v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id), "CorpMatur"]), (v20_CorpHedgegrIn_t[var_l1, "CorpMatur"]), x_hrcut, x_pair_id, var_m2, var_m)
              } else {
                new_pair <- c(0, (v20_CorpHedgegrIn_t[var_l1, "unique_id"]), (v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id), "unique_id"]), CurTransAmt, (v20_CorpHedgegrIn_t[var_l1, "CorpMatur"]), (v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id), "CorpMatur"]), x_hrcut, x_pair_id, var_m2, var_m)
              }
            } else if (funcnumber == 3){
              if (prnt_f == "Y"){print("J16_9")}
              if (balance_table_band >= 0){
                new_pair <- c(0, (v20_CorpHedgegrIn_t[var_l1, "unique_id"]), (v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id), "unique_id"]), CurTransAmt, (v20_CorpHedgegrIn_t[var_l1, "CorpMatur"]), (v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id), "CorpMatur"]), x_hrcut, x_pair_id, var_m2, var_m)
              } else {
                new_pair <- c(0, (v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id), "unique_id"]), (v20_CorpHedgegrIn_t[var_l1, "unique_id"]), CurTransAmt, (v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id), "CorpMatur"]), (v20_CorpHedgegrIn_t[var_l1, "CorpMatur"]), x_hrcut, x_pair_id, var_m2, var_m)
              }
            }
            x_pair_id <- (x_pair_id + 1)
            tbl_OldPairs <- rbind(tbl_OldPairs, existing_pair)
            tbl_NewPairs <- rbind(tbl_NewPairs, new_pair)
            v20_CorpHedgegrIn_t[var_l1, "mv_rem"] <- (v20_CorpHedgegrIn_t[var_l1, "mv_rem"] - CurTransAmt)
            v20_CorpHedgegrIn_t[var_l1, "mv_hff"] <- (v20_CorpHedgegrIn_t[var_l1, "mv_hff"] + CurTransAmt)
            v20_CorpHedgegrIn_t[var_k1, "mv_rem"] <- (v20_CorpHedgegrIn_t[var_k1, "mv_rem"] + CurTransAmt)
            v20_CorpHedgegrIn_t[var_k1, "mv_hff"] <- (v20_CorpHedgegrIn_t[var_k1, "mv_hff"] - CurTransAmt)
            MaxRmTransAmt <- (MaxRmTransAmt - CurTransAmt)
            var_m <- (var_m + 1)
          } else {
            if (prnt_f == "Y"){print("J16_10")}
            break
          }
        }
        if (transfer_amount_2 >= ContrAmt){
          if (prnt_f == "Y"){print("J16_11")}
          TransAmt <- transfer_amount_2
          var_l1 <- (var_l1 + 1); 
          var_k1 <- (var_k1 + 1); 
          var_m2 <- (var_m2 + 1)
        } else {
          if (prnt_f == "Y"){print("J16_12")}
          var_exit2 <- 1; # if we cannot rehedge our current F sec with any of the pairs of the next F sec, then we need to leave the while loop
        }
        if (var_k1 > nrow(v20_CorpHedgegrIn_t)){
          if (prnt_f == "Y"){print("J16_13")}
          break
        }
      }
    }
    var_m2 <- (var_m2 - 1); #to move group indicator back to the last position since after each cycle it was moved up by 1
    while (var_i1 > 1 & (v20_UsgHedgeband[var_i1, "UsgMatur"] - v20_CorpHedgegrIn[var_l1, "CorpMatur"]) > MtrDstnc){
      #Start of A lifting cycle
      var_i1 <- (var_i1 - 1)
    }
    while (v20_UsgHedgeband[var_i1,"mv_gov_rem"] < 0.01 & (abs(v20_UsgHedgeband[var_i1, "UsgMatur"] - v20_CorpHedgegrIn[var_l1, "CorpMatur"]) <= MtrDstnc) & var_i1 > 1){
      var_i1 <- (var_i1 - 1)
    }
    if (abs(v20_UsgHedgeband[var_i1, "UsgMatur"] - v20_CorpHedgegrIn[var_l1, "CorpMatur"]) <= MtrDstnc){
      TransAmtTemp <- 0
      if (TransAmt <= v20_UsgHedgeband[var_i1, "mv_gov_rem"]){
        if (prnt_f == "Y"){print("J16_17")}
        if (funcnumber == 2){
          if (balance_table_band >= 0.01){
            MatchFA <- c(nrow(tbl_FAPairs), v20_UsgHedgeband[var_i1,"unique_id_gov"], v20_CorpHedgegrIn[var_l1,"unique_id"], TransAmt, v20_UsgHedgeband[var_i1,"UsgMatur"], v20_CorpHedgegrIn[var_l1,"CorpMatur"], v20_CorpHedgegrIn[var_j1,"unique_id"], x_fhedge_side, x_hrcut_fa, v1_FAPairId)
          } else if (balance_table_band <= -0.01){
            MatchFA <- c(nrow(tbl_FAPairs), v20_CorpHedgegrIn[var_l1,"unique_id"], v20_UsgHedgeband[var_i1,"unique_id_gov"], TransAmt, v20_CorpHedgegrIn[var_l1,"CorpMatur"], v20_UsgHedgeband[var_i1,"UsgMatur"], v20_CorpHedgegrIn[var_j1,"unique_id"], x_fhedge_side, x_hrcut_fa, v1_FAPairId)
          }
        } else if (funcnumber == 3){
          if (balance_table_band >= 0.01){
            MatchFA <- c(nrow(tbl_FAPairs), v20_CorpHedgegrIn[var_l1,"unique_id"], v20_UsgHedgeband[var_i1,"unique_id_gov"], TransAmt, v20_CorpHedgegrIn[var_l1,"CorpMatur"], v20_UsgHedgeband[var_i1,"UsgMatur"], v20_CorpHedgegrIn[var_j1,"unique_id"], x_fhedge_side, x_hrcut_fa, v1_FAPairId)
          } else if (balance_table_band <= -0.01){
            MatchFA <- c(nrow(tbl_FAPairs), v20_UsgHedgeband[var_i1,"unique_id_gov"], v20_CorpHedgegrIn[var_l1,"unique_id"], TransAmt, v20_UsgHedgeband[var_i1,"UsgMatur"], v20_CorpHedgegrIn[var_l1,"CorpMatur"], v20_CorpHedgegrIn[var_j1,"unique_id"], x_fhedge_side, x_hrcut_fa, v1_FAPairId)
          }
        }
        if (prnt_f == "Y"){print("J16_18")}
        v1_FAPairId <- (v1_FAPairId + 1)
        tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA) 
        v20_UsgHedgeband[var_i1, "mv_gov_rem"] <- (v20_UsgHedgeband[var_i1, "mv_gov_rem"] - TransAmt)
        if (v20_UsgHedgeband[var_i1, "mv_gov_rem"] < 0.01){
          var_i1 <- (var_i1 - 1)
        }
      } else {
        if (prnt_f == "Y"){print("J16_20")}
        TransAmtTemp <- TransAmt
        var_exit3 <- 0
        while (var_exit3 == 0 & TransAmtTemp >= ContrAmt & abs(v20_UsgHedgeband[var_i1, "UsgMatur"] - v20_CorpHedgegrIn[var_l1, "CorpMatur"]) <= MtrDstnc & v20_UsgHedgeband[var_i1, "mv_gov_rem"] >= 0.01){
          #This is the case where we match more than 1 A security; each cycle matches 1 A security
          if (prnt_f == "Y"){print("J16_21")}
          if (funcnumber == 2){
            if (balance_table_band >= 0.01){
              MatchFA <- c(nrow(tbl_FAPairs), v20_UsgHedgeband[var_i1, "unique_id_gov"], v20_CorpHedgegrIn[var_l1, "unique_id"], min(c(v20_UsgHedgeband[var_i1, "mv_gov_rem"], TransAmtTemp)), v20_UsgHedgeband[var_i1, "UsgMatur"], v20_CorpHedgegrIn[var_l1, "CorpMatur"], v20_CorpHedgegrIn[var_j1,"unique_id"], x_fhedge_side, x_hrcut_fa, v1_FAPairId)
            } else if (balance_table_band <= -0.01){
              MatchFA <- c(nrow(tbl_FAPairs), v20_CorpHedgegrIn[var_l1,"unique_id"], v20_UsgHedgeband[var_i1, "unique_id_gov"], min(c(v20_UsgHedgeband[var_i1, "mv_gov_rem"], TransAmtTemp)), v20_CorpHedgegrIn[var_l1, "CorpMatur"], v20_UsgHedgeband[var_i1, "UsgMatur"], v20_CorpHedgegrIn[var_j1, "unique_id"], x_fhedge_side, x_hrcut_fa, v1_FAPairId)
            }
          } else if (funcnumber == 3){
            if (balance_table_band >= 0.01){
              MatchFA <- c(nrow(tbl_FAPairs), v20_CorpHedgegrIn[var_l1, "unique_id"], v20_UsgHedgeband[var_i1, "unique_id_gov"], min(c(v20_UsgHedgeband[var_i1, "mv_gov_rem"], TransAmtTemp)), v20_CorpHedgegrIn[var_l1, "CorpMatur"], v20_UsgHedgeband[var_i1, "UsgMatur"], v20_CorpHedgegrIn[var_j1, "unique_id"], x_fhedge_side, x_hrcut_fa, v1_FAPairId)
            } else if (balance_table_band <= -0.01){
              MatchFA <- c(nrow(tbl_FAPairs), v20_UsgHedgeband[var_i1, "unique_id_gov"], v20_CorpHedgegrIn[var_l1, "unique_id"], min(c(v20_UsgHedgeband[var_i1, "mv_gov_rem"], TransAmtTemp)), v20_UsgHedgeband[var_i1, "UsgMatur"], v20_CorpHedgegrIn[var_l1, "CorpMatur"], v20_CorpHedgegrIn[var_j1, "unique_id"], x_fhedge_side, x_hrcut_fa, v1_FAPairId)
            }
          }
          if (prnt_f == "Y"){print("J16_22")}
          v1_FAPairId <- (v1_FAPairId + 1)
          tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA)
          if (v20_UsgHedgeband[var_i1, "mv_gov_rem"] <= TransAmtTemp){
            TransAmtTemp <- (TransAmtTemp - v20_UsgHedgeband[var_i1, "mv_gov_rem"])
            v20_UsgHedgeband[var_i1,"mv_gov_rem"] <- 0
            if (var_i1 == 1){
              var_exit3 <- 1
            } else {
              var_i1 <- (var_i1 - 1)
            }
          } else {
            v20_UsgHedgeband[var_i1, "mv_gov_rem"] <- (v20_UsgHedgeband[var_i1, "mv_gov_rem"] - TransAmtTemp)
            TransAmtTemp <- 0
          }
        } 
      }
      v20_CorpHedgegrIn[var_j1,"mv_rem"] <- (v20_CorpHedgegrIn[var_j1,"mv_rem"] - (TransAmt - TransAmtTemp))
      v20_CorpHedgegrIn[var_j1,"mv_hff"] <- (v20_CorpHedgegrIn[var_j1,"mv_hff"] + (TransAmt - TransAmtTemp))
      v20_CorpHedgegrIn[var_l1,"mv_hff"] <- (v20_CorpHedgegrIn[var_l1,"mv_hff"] - (TransAmt - TransAmtTemp))
      v20_CorpHedgegrIn[var_l1,"mv_h2fa"] <- (v20_CorpHedgegrIn[var_l1,"mv_h2fa"] + (TransAmt - TransAmtTemp))
      var_i2 <- 1
      while (var_i2 <= var_m2){
        if (prnt_f == "Y"){print("J16_23")}
        TransRem <- (TransAmt - TransAmtTemp)
        var_j2 <- 1
        while (var_j2 <= length(tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2)]) & TransRem >= 0.01){
          if (prnt_f == "Y"){print("J16_24")}
          if (tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"] > TransRem){
            if (prnt_f == "Y"){print("J16_25")}
            tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"] <- (tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"] - TransRem)
            tbl_FFPairs[which(tbl_FFPairs[,xf_NhgSdId] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), xf_NhgSdId] & tbl_FFPairs[,xf_HdgSdId] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), xf_HdgSdId] & tbl_FFPairs[, "pair_id"] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "pair_id"]), "MatchAmt"] <- tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"]
            tbl_NewPairs[which(tbl_NewPairs[ , "GrNum"] == var_i2 & tbl_NewPairs[ , "PairNum"] == var_j2), "MatchAmt"] <- TransRem
            matching_pair <- c(nrow(tbl_FFPairs), tbl_NewPairs[which(tbl_NewPairs[ , "GrNum"] == var_i2 & tbl_NewPairs[ , "PairNum"] == var_j2), c("id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id")])
            tbl_FFPairs <- rbind(tbl_FFPairs, matching_pair);
            TransRem <- 0
            break
          } else {
            if (prnt_f == "Y"){print("J16_26")}
            TransRem <- (TransRem - tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"])
            tbl_NewPairs[which(tbl_NewPairs[ , "GrNum"] == var_i2 & tbl_NewPairs[ , "PairNum"] == var_j2), "MatchAmt"] <- tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"]
            tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"] <- 0
            tbl_FFPairs[which(tbl_FFPairs[,xf_NhgSdId] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), xf_NhgSdId] & tbl_FFPairs[,xf_HdgSdId] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), xf_HdgSdId] & tbl_FFPairs[, "pair_id"] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "pair_id"]), "MatchAmt"] <- tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"]
            matching_pair <- c(nrow(tbl_FFPairs), tbl_NewPairs[which(tbl_NewPairs[ , "GrNum"] == var_i2 & tbl_NewPairs[ , "PairNum"] == var_j2), c("id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id")])
            tbl_FFPairs <- rbind(tbl_FFPairs, matching_pair);
            var_j2 <- (var_j2 + 1)                
          }
        }
        var_i2 <- (var_i2 + 1)
      }
      if (prnt_f == "Y"){print("J16_27")}
      if(nrow(tbl_FFPairs) > 1){tbl_FFPairs <- tbl_FFPairs[which(tbl_FFPairs[, "MatchAmt"] >= 0.01),]}
      var_curfsec_amt <- (var_curfsec_amt - (TransAmt - TransAmtTemp))
      if (balance_table_band >= 0.01){
        balance_table_band <- (balance_table_band - (TransAmt - TransAmtTemp))
      } else if (balance_table_band <= -0.01){
        balance_table_band <- (balance_table_band + (TransAmt - TransAmtTemp))
      }
    } else if (v20_UsgHedgeband[var_i1, "UsgMatur"] - v20_CorpHedgegrIn[var_l1, "CorpMatur"] > MtrDstnc){
      #If after moving F and A securities their maturity distance is still more than hedging maturity
      v_Exit0 <- 1; #v_Exit0 makes sure that entire A-F hedging cycle for this A securities subcategory ends
      break
    } else {
      var_exit1 <- 1
      if (var_j1 > 1){var_j1 <- (var_j1 - 1)}
      # Case where we moved up A security so that it has smaller maturity than moved-down F security, so we want to move back to main cycle 
    }
  }
  #Possible infinite loop fix. Need to check if introducing control amount into while loop of fa_hedging_part1 is better, i.e. abs(balance_table_band) >= ContrAmt
  if (abs(balance_table_band) < ContrAmt){
    if (var_j1 > 1){
      var_j1 <- (var_j1 - 1)
    } else {
      v_Exit0 <- 1
    }
  }
  if (prnt_f == "Y"){print("J16_29")}
  xyz <- list("balance_table_band" = balance_table_band, "govsec_band" = v20_UsgHedgeband, "CorpHedgegrIn" = v20_CorpHedgegrIn, "CorpHedgegrOut" = v20_CorpHedgegrOut, "pairtbl" = tbl_FFPairs, "pairtbl_fa" = tbl_FAPairs, "var_i1" = var_i1, "var_j1" = var_j1, "v_Exit0" = v_Exit0, "FAPairId" = v1_FAPairId, "PairId" = x_pair_id)
  remove(balance_table_band, v20_UsgHedgeband, v20_CorpHedgegrIn, v20_CorpHedgegrOut, tbl_FFPairs, tbl_FAPairs, var_i1, var_j1, v_Exit0, v1_FAPairId, x_pair_id)
  return(xyz)
}

#Main F-A hedging function (Explanation of variables below) 
#v_UsgBandBalnc - net balance (long positions minus short positions) of given A sub-category, 
#v_CorpHedgegrIn - table of selected hedging-group hedging-side (short or long) F securities, 
#v_CorpHedgegrOut - table of selected hedging-group non-hedging-side F securities, 
#v_UsgHedgeband - table of selected sub-category hedging-side A securities, 
#var_i1 - current position indicator for hedging A table, 
#var_j1 - current position indicator for hedging F table, 
#tbl_FFPairs - table of all existing (active) f-f (2 corporate debt securities) pairs, 
#tbl_FAPairs - table of all existing f-a (corporate debt security and US government security) pairs, 
#v_Exit0 - artificial 0/1 indicator that forces and exit from the main while loop in fa_hedging_part1 function, 
#y_rltv_maturity - relative maturity, that is the number of months of maturity within which 2 securities must be to hedge them, 
#y_hrcut - percentage haircut for f-f pairs expressed in decimal point, 
#y_hrcut_fa - percentage haircut for f-a pairs expressed in decimal point, 
#v_CorpHdgSideId - name of the column in tbl_FAPairs that indicates hedging F security side - "id_long" or "id_short", 
#v_CorpNhdgSideId - name of the column in tbl_FAPairs that indicates non-hedging A security side - "id_long" or "id_short",  
#y_NhdgSideMtr - name of the column in tbl_FFPairs, 
#v2_FAPairId - F-A pairs table unique identified of a pair, 
#v2_FFPairId    - F-F pairs table unique identified of a pair
fa_hedging_part1 <- function(v_UsgBandBalnc, v_CorpHedgegrIn, v_CorpHedgegrOut, v_UsgHedgeband, var_i1, var_j1, tbl_FFPairs, tbl_FAPairs, v_Exit0, y_rltv_maturity, y_hrcut, y_hrcut_fa, v_CorpHdgSideId, v_CorpNhdgSideId, y_NhdgSideMtr, v2_FAPairId, v2_FFPairId, ContrAmt, matching_pair_gr, y_fhedge_side, lower_bound){
  var_j1 <- nrow(v_CorpHedgegrIn)
  var_i1 <- nrow(v_UsgHedgeband)
  var_curfsec_amt <- v_CorpHedgegrIn[var_j1,"mv_rem"]
  while (var_i1 >= 1 & var_j1 >= 1 & abs(v_UsgBandBalnc) >= ContrAmt & v_Exit0 == 0){
    #Main Cycle 
    if (v_CorpHedgegrIn[var_j1, "CorpMatur"] < lower_bound){
      break
    }
    #Next line checks if current F security is fully hedged: if so, it will move to the next one with lower maturity
    if (v_CorpHedgegrIn[var_j1,"mv_rem"] < ContrAmt & var_j1 > 1){
      var_j1 <- (var_j1 - 1); 
    } else if (v_CorpHedgegrIn[var_j1,"mv_rem"] < ContrAmt & var_j1 <= 1){
      #F security indicator is standing at the top of the table, and current security non-offset value is 0, hence need to exit the cycle;
      v_Exit0 <- 1;
    } else {
      if (v_UsgHedgeband[var_i1,"mv_gov_rem"] < 0.01 & var_i1 > 1){
        var_i1 <- (var_i1 - 1)
      } else if (v_UsgHedgeband[var_i1,"mv_gov_rem"] < 0.01 & var_i1 <= 1){
        v_Exit0 <- 1
      } else {
        if (v_UsgHedgeband[var_i1, "UsgMatur"] >= v_CorpHedgegrIn[var_j1, "CorpMatur"]){
          if (nrow(v_CorpHedgegrOut) >= 1){
            var_curfsec_amt <- v_CorpHedgegrIn[var_j1,"mv_rem"]
            TransAmt <- min(c(v_UsgBandBalnc, v_CorpHedgegrIn[var_j1, "mv_rem"]))
            xyz_2_1m <- f_movement(v_UsgBandBalnc, v_CorpHedgegrIn, v_CorpHedgegrOut, v_UsgHedgeband, v_CorpHdgSideId, v_CorpNhdgSideId, y_NhdgSideMtr, var_curfsec_amt, ContrAmt, var_i1, var_j1, tbl_FFPairs, tbl_FAPairs, y_fhedge_side, v_Exit0, y_rltv_maturity, y_hrcut, y_hrcut_fa, 2)
            v_UsgBandBalnc <- xyz_2_1m[["balance_table_band"]]
            v_UsgHedgeband <- xyz_2_1m[["govsec_band"]]
            v_CorpHedgegrIn <- xyz_2_1m[["CorpHedgegrIn"]]
            v_CorpHedgegrOut <- xyz_2_1m[["CorpHedgegrOut"]]
            tbl_FFPairs <- xyz_2_1m[["pairtbl"]]
            tbl_FAPairs <- xyz_2_1m[["pairtbl_fa"]]
            var_i1 <- xyz_2_1m[["var_i1"]]
            var_j1 <- xyz_2_1m[["var_j1"]]
            v_Exit0 <- xyz_2_1m[["v_Exit0"]]
            v2_FAPairId <- xyz_2_1m[["FAPairId"]]
            v2_FFPairId    <- xyz_2_1m[["PairId"]]
            if (prnt_f == "Y"){print("J16_30")}
            if (v_CorpHedgegrIn[var_j1,"mv_rem"] < 0.01){
              var_j1 <- (var_j1 - 1)
            }
          } else {
            if ((v_UsgHedgeband[var_i1, "UsgMatur"]) - v_CorpHedgegrIn[var_j1, "CorpMatur"] <= y_rltv_maturity){
              if  (abs(v_UsgBandBalnc) >= min(c(v_UsgHedgeband[var_i1,"mv_gov_rem"], v_CorpHedgegrIn[var_j1,"mv_rem"]))){
                hedging_amount <- min(c(v_UsgHedgeband[var_i1,"mv_gov_rem"], v_CorpHedgegrIn[var_j1,"mv_rem"]))
                if (y_fhedge_side == 0){
                  MatchFA <- c(nrow(tbl_FAPairs), v_UsgHedgeband[var_i1,"unique_id_gov"], v_CorpHedgegrIn[var_j1,"unique_id"], hedging_amount, v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"CorpMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId)
                } else {
                  MatchFA <- c(nrow(tbl_FAPairs), v_CorpHedgegrIn[var_j1,"unique_id"], v_UsgHedgeband[var_i1,"unique_id_gov"], hedging_amount, v_CorpHedgegrIn[var_j1,"CorpMatur"], v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId)
                }
                v2_FAPairId <- (v2_FAPairId + 1)
                tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA)
                if (v_UsgHedgeband[var_i1,"mv_gov_rem"] >=  v_CorpHedgegrIn[var_j1,"mv_rem"]){
                  if (v_UsgBandBalnc >= 0){
                    v_UsgBandBalnc <- (v_UsgBandBalnc - hedging_amount)
                  } else {
                    v_UsgBandBalnc <- (v_UsgBandBalnc + hedging_amount)
                  }
                  v_UsgHedgeband[var_i1,"mv_gov_rem"] <- (v_UsgHedgeband[var_i1,"mv_gov_rem"] - hedging_amount)
                  v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); 
                  v_CorpHedgegrIn[var_j1,"mv_rem"] <- 0
                  var_j1 <- (var_j1 - 1)
                  if (v_UsgHedgeband[var_i1,"mv_gov_rem"] < 0.01){
                    if (var_i1 > 1){ 
                      var_i1 <- (var_i1 - 1)
                    } else {
                      v_Exit0 <- 1
                    }
                  }
                } else {
                  if (v_UsgBandBalnc >= 0){
                    v_UsgBandBalnc <- (v_UsgBandBalnc -  v_UsgHedgeband[var_i1,"mv_gov_rem"])
                  } else {
                    v_UsgBandBalnc <- (v_UsgBandBalnc +  v_UsgHedgeband[var_i1,"mv_gov_rem"])
                  }
                  v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- (v_CorpHedgegrIn[var_j1,"mv_rem"] - hedging_amount); 
                  v_UsgHedgeband[var_i1,"mv_gov_rem"] <- 0; 
                  if (var_i1 > 1){
                    var_i1 <- (var_i1 - 1)
                  } else {
                    v_Exit0 <- 1
                  }
                }
                rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr;
              } else {
                hedging_amount <- abs(v_UsgBandBalnc)
                if (y_fhedge_side == 0){
                  MatchFA <- c(nrow(tbl_FAPairs), v_UsgHedgeband[var_i1,"unique_id_gov"], v_CorpHedgegrIn[var_j1,"unique_id"], hedging_amount, v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"CorpMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); 
                  v2_FAPairId <- (v2_FAPairId + 1)
                } else {
                  MatchFA <- c(nrow(tbl_FAPairs), v_CorpHedgegrIn[var_j1,"unique_id"], v_UsgHedgeband[var_i1,"unique_id_gov"], hedging_amount, v_CorpHedgegrIn[var_j1,"CorpMatur"], v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); 
                  v2_FAPairId <- (v2_FAPairId + 1)
                }
                tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA); 
                v_UsgHedgeband[var_i1,"mv_gov_rem"] <- (v_UsgHedgeband[var_i1,"mv_gov_rem"] - hedging_amount); 
                v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- (v_CorpHedgegrIn[var_j1,"mv_rem"] - hedging_amount);   
                rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr; 
                v_UsgBandBalnc   <- 0
              }
            } else {
              var_i1 <- (var_i1 - 1)
            }
          }
        } else {
          if ((v_CorpHedgegrIn[var_j1, "CorpMatur"] - v_UsgHedgeband[var_i1, "UsgMatur"]) <= y_rltv_maturity){
            #Check whether we can hedge A and F securities
            if  (abs(v_UsgBandBalnc) >= min(c(v_UsgHedgeband[var_i1,"mv_gov_rem"], v_CorpHedgegrIn[var_j1,"mv_rem"]))){
              hedging_amount <- min(c(v_UsgHedgeband[var_i1,"mv_gov_rem"], v_CorpHedgegrIn[var_j1,"mv_rem"]))
              if (y_fhedge_side == 0){
                MatchFA <- c(nrow(tbl_FAPairs), v_UsgHedgeband[var_i1,"unique_id_gov"], v_CorpHedgegrIn[var_j1,"unique_id"], hedging_amount, v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"CorpMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId)
              } else {
                MatchFA <- c(nrow(tbl_FAPairs), v_CorpHedgegrIn[var_j1,"unique_id"], v_UsgHedgeband[var_i1,"unique_id_gov"], hedging_amount, v_CorpHedgegrIn[var_j1,"CorpMatur"], v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId)
              }
              v2_FAPairId <- (v2_FAPairId + 1)
              tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA)
              if (v_UsgHedgeband[var_i1,"mv_gov_rem"] >=  v_CorpHedgegrIn[var_j1,"mv_rem"]){
                if (v_UsgBandBalnc >= 0){
                  v_UsgBandBalnc <- (v_UsgBandBalnc - hedging_amount)
                } else {
                  v_UsgBandBalnc <- (v_UsgBandBalnc + hedging_amount)
                }
                v_UsgHedgeband[var_i1,"mv_gov_rem"] <- (v_UsgHedgeband[var_i1,"mv_gov_rem"] - hedging_amount)
                v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount)
                v_CorpHedgegrIn[var_j1,"mv_rem"] <- 0
                var_j1 <- (var_j1 - 1)
                if (v_UsgHedgeband[var_i1,"mv_gov_rem"] < 0.01){
                  if (var_i1 > 1){ 
                    var_i1 <- (var_i1 - 1)
                  } else {
                    v_Exit0 <- 1
                  }
                }
              } else {
                if (v_UsgBandBalnc >= 0){
                  v_UsgBandBalnc <- (v_UsgBandBalnc -  v_UsgHedgeband[var_i1,"mv_gov_rem"])
                } else {
                  v_UsgBandBalnc <- (v_UsgBandBalnc +  v_UsgHedgeband[var_i1,"mv_gov_rem"])
                }
                v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- (v_CorpHedgegrIn[var_j1,"mv_rem"] - hedging_amount); 
                v_UsgHedgeband[var_i1,"mv_gov_rem"] <- 0; 
                if (var_i1 > 1){
                  var_i1 <- (var_i1 - 1)
                } else {
                  v_Exit0 <- 1
                }
              }
              rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr;
            } else {
              hedging_amount <- abs(v_UsgBandBalnc)
              if (y_fhedge_side == 0){
                MatchFA <- c(nrow(tbl_FAPairs), v_UsgHedgeband[var_i1,"unique_id_gov"], v_CorpHedgegrIn[var_j1,"unique_id"], hedging_amount, v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"CorpMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
              } else {
                MatchFA <- c(nrow(tbl_FAPairs), v_CorpHedgegrIn[var_j1,"unique_id"], v_UsgHedgeband[var_i1,"unique_id_gov"], hedging_amount, v_CorpHedgegrIn[var_j1,"CorpMatur"], v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
              }
              tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA); 
              v_UsgHedgeband[var_i1,"mv_gov_rem"] <- (v_UsgHedgeband[var_i1,"mv_gov_rem"] - hedging_amount); 
              v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- (v_CorpHedgegrIn[var_j1,"mv_rem"] - hedging_amount);   
              rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr; 
              v_UsgBandBalnc   <- 0
            }
          } else {
            var_j1 <- (var_j1 - 1)
          }
        }
      }
    }
  }
  if (prnt_f == "Y"){print("J16_31")}
  tbl_FFPairs2 <- matrix(c(0, 0, 0, 0.02, 0, 0, 0, 0), nrow=1,ncol=8)
  colnames(tbl_FFPairs2) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id")
  if (length(tbl_FFPairs[,1]) > 1){
    temp_ff <- aggregate(tbl_FFPairs[,c("MatchAmt")], by=list(tbl_FFPairs[,2],tbl_FFPairs[,3],tbl_FFPairs[,5],tbl_FFPairs[,6],tbl_FFPairs[,7]), "sum")
    i <- 2
    while( i <= length(temp_ff[[1]]) ){
      if (prnt_f == "Y"){print("J16_32")}
      FFPairId <- max(tbl_FFPairs[,8])
      matching_pair_ff <- c(nrow(tbl_FFPairs2), temp_ff[i,1], temp_ff[i,2],temp_ff[i,6] , temp_ff[i,3], temp_ff[i,4], temp_ff[i,5], FFPairId)
      FFPairId <-  FFPairId + 1
      tbl_FFPairs2 <- rbind(tbl_FFPairs2, matching_pair_ff)
      i <- (i + 1)
    }
  } else {
    tbl_FFPairs2 <- tbl_FFPairs
  }
  
  f21_PairsFA <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1,ncol=10)
  colnames(f21_PairsFA) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "FOrigID", "f_sec_side", "HcutRate", "pair_id")
  
  if (length(tbl_FAPairs[,1]) > 1){
    temp_fa <- aggregate(tbl_FAPairs[,c("MatchAmt")], by=list(tbl_FAPairs[,2],tbl_FAPairs[,3],tbl_FAPairs[,5],tbl_FAPairs[,6], tbl_FAPairs[,8], tbl_FAPairs[,9]), "sum")  
    i <- 2
    if (prnt_f == "Y"){print("J16_33")}
    while( i <= length(temp_fa[[1]]) ){
      if (prnt_f == "Y"){print("J16_34")}
      MatchFA <- c(nrow(f21_PairsFA), temp_fa[i,1], temp_fa[i,2],temp_fa[i,7] , temp_fa[i,3], temp_fa[i,4], 0, temp_fa[i,5], temp_fa[i,6], FAPairId)
      FAPairId <-  FAPairId + 1
      f21_PairsFA <- rbind(f21_PairsFA, MatchFA) 
      i <- (i + 1)
    }
  } else {
    f21_PairsFA <- tbl_FAPairs
  }
  if (prnt_f == "Y"){print("J16_35")}
  xyz_2 <- list("v_UsgBandBalnc" = v_UsgBandBalnc, "y_govsec_band" = v_UsgHedgeband, "v_CorpHedgegrIn" = v_CorpHedgegrIn, "v_CorpHedgegrOut" = v_CorpHedgegrOut, "y_pairtbl" = tbl_FFPairs2, "y_pairtbl_fa" = f21_PairsFA, "FAPairId" = v2_FAPairId)
  remove(v_UsgBandBalnc, v_UsgHedgeband, v_CorpHedgegrIn, v_CorpHedgegrOut, tbl_FFPairs2, f21_PairsFA, v2_FAPairId)
  return(xyz_2)
}

#Step 2.1 Section C
# A-F hedging function run
#source("F-F functions.r")
print("Step 2.1 Function Run")
if (distrib >=3 ){
  vec_x21a <- c(     1,      1,      1,     2,      3,     4,     4,     4)
  vec_x21b <- c(     6,      6,      6,     9,     24,   120,   120,   120)
  vec_x21c <- c(0.0175, 0.0175, 0.0175,  0.03, 0.0325, 0.035, 0.035, 0.035)
  vec_x21d <- c( 0.015,  0.015,  0.015, 0.025, 0.0275,  0.03,  0.03,  0.03)
  vec_x21e <- c(    12,     12,     12,    60,    120,   180,   180,   180)
  for (i in 9:2){
    v_Exit0 <- 0
    if (A_SubgrBal[i] >= 0.01 & nrow(AsecBnd_Ln[[i]]) >= 1 & nrow(CorpHedgegrS[[vec_x21a[i-1]]]) >= 1){
      if (prnt_f == "Y"){paste("Function fa_hedging_part1 run for Long F group ",i,sep="")}
      xyz_2 <- fa_hedging_part1(v_UsgBandBalnc = A_SubgrBal[i], v_CorpHedgegrIn = CorpHedgegrS[[vec_x21a[i-1]]], v_CorpHedgegrOut = FhgrL[[vec_x21a[i-1]]], v_UsgHedgeband = AsecBnd_Ln[[i]], var_i1, var_j1, tbl_FFPairs, tbl_FAPairs, v_Exit0, y_rltv_maturity = vec_x21b[i-1], y_hrcut = vec_x21c[i-1], y_hrcut_fa = vec_x21d[i-1], v_CorpHdgSideId = "id_short", v_CorpNhdgSideId = "id_long", y_NhdgSideMtr = "MtrL", v2_FAPairId = FAPairId, v2_FFPairId = PairId, ContrAmt, matching_pair_gr = paste("fa_matching_pair_gr",vec_x21a[i-1],sep=""), 0, vec_x21e[i-1])
      A_SubgrBal[i] <- xyz_2[["v_UsgBandBalnc"]]
      AsecBnd_Ln[[i]]  <- xyz_2[["y_govsec_band"]]
      CorpHedgegrS[[vec_x21a[i-1]]]  <- xyz_2[["v_CorpHedgegrIn"]]
      FhgrL[[vec_x21a[i-1]]]  <- xyz_2[["v_CorpHedgegrOut"]]
      tbl_FFPairs <- xyz_2[["y_pairtbl"]]
      tbl_FAPairs <- xyz_2[["y_pairtbl_fa"]]
      FAPairId <- xyz_2[["FAPairId"]]
      PairId <- xyz_2[["PairId"]]
      remove(xyz_2)
    } else if (A_SubgrBal[i] <= -0.01 & nrow(AsecBnd_Sh[[i]]) >= 1 & nrow(FhgrL[[vec_x21a[i-1]]]) >= 1){
      if (prnt_f == "Y"){paste("Function fa_hedging_part1 run for Short F group ",i,sep="")}
      xyz_2 <- fa_hedging_part1(v_UsgBandBalnc = A_SubgrBal[i], v_CorpHedgegrIn = FhgrL[[vec_x21a[i-1]]], v_CorpHedgegrOut = CorpHedgegrS[[vec_x21a[i-1]]], v_UsgHedgeband = AsecBnd_Sh[[i]], var_i1, var_j1, tbl_FFPairs, tbl_FAPairs, v_Exit0, y_rltv_maturity = vec_x21b[i-1], y_hrcut = vec_x21c[i-1], y_hrcut_fa = vec_x21d[i-1], v_CorpHdgSideId = "id_long", v_CorpNhdgSideId = "id_short", y_NhdgSideMtr = "MtrSh", v2_FAPairId = FAPairId, v2_FFPairId = PairId, ContrAmt, matching_pair_gr = paste("fa_matching_pair_gr",vec_x21a[i-1],sep=""), 1, vec_x21e[i-1])
      A_SubgrBal[i] <- xyz_2[["v_UsgBandBalnc"]]
      AsecBnd_Sh[[i]]  <- xyz_2[["y_govsec_band"]]
      CorpHedgegrS[[vec_x21a[i-1]]]  <- xyz_2[["v_CorpHedgegrOut"]]
      FhgrL[[vec_x21a[i-1]]]  <- xyz_2[["v_CorpHedgegrIn"]]
      tbl_FFPairs <- xyz_2[["y_pairtbl"]]
      tbl_FAPairs <- xyz_2[["y_pairtbl_fa"]]
      FAPairId <- xyz_2[["FAPairId"]]
      PairId <- xyz_2[["PairId"]]
      remove(xyz_2)
    } else {
    }
  }
  v_Exit0 <- 0
}

###Step 3.0 Section A:  Update F sub-categories
vec_z30a <- c(0, 12, 24, 36, 60, 120, 180, 240, 300, 10^9 )
vec_z30b <- c(1, 1, 1, 1, 2, 3, 4, 4, 4)
for (i in 1:9){
  FsecBnd_Sh[[i]] <- subset(CorpHedgegrS[[vec_z30b[i]]], CorpMatur >= vec_z30a[i]  & CorpMatur < vec_z30a[i+1] )
  FsecBnd_Ln[[i]] <- subset(FhgrL[[vec_z30b[i]]], CorpMatur >= vec_z30a[i]  & CorpMatur < vec_z30a[i+1] )
}

#Initialize balance of F subcat
fcat_bal_l <- rep(0, times = 9)
fcat_bal_s <- rep(0, times = 9)
fcat_bal   <- rep(0, times = 9)
#Calculate balance of F subcat
for (i in 9:1){
  if (nrow(FsecBnd_Ln[[i]]) >=1){
    fcat_bal_l[i] <- sum(FsecBnd_Ln[[i]]["mv_rem"])
  } else {
    fcat_bal_l[i] <- 0 
  }
  if (nrow(FsecBnd_Sh[[i]]) >=1){
    fcat_bal_s[i] <- sum(FsecBnd_Sh[[i]]["mv_rem"])
  } else {
    fcat_bal_s[i] <- 0
  }
}

for (i in 9:1){fcat_bal[i] <- ( fcat_bal_l[i] - fcat_bal_s[i] )}
###

#Step 3.0 Section B: Functions set-up  -  F-movement without hedge function
f_movement_without_hedge <- function(x_potential_transfer, v20_CorpHedgegrIn, v20_CorpHedgegrOut, xf_HdgSdId, xf_NhgSdId, x_NhdgSideMtr, tbl_FFPairs, MtrDstnc, x_hrcut, x_lower_maturity_floor, x_lower_maturity_ceiling, x_higher_maturity_ceiling, ContrAmt, matching_pair_gr, ...){
  var_exit1 <- 0; 
  v1_FFPairId <- (max(tbl_FFPairs[,"pair_id"]) + 1)
  tmp_table_1 <- v20_CorpHedgegrIn[which(v20_CorpHedgegrIn[, "CorpMatur"] >= x_lower_maturity_floor & v20_CorpHedgegrIn[, "CorpMatur"] < x_lower_maturity_ceiling),]
  pos_temp <- max(which(tmp_table_1[,"mv_rem"] >= 0.01))
  pos_f <- which(v20_CorpHedgegrIn[, "unique_id"] == tmp_table_1[pos_temp, "unique_id"])
  pos_f_low <- (pos_f + 1)
  while (abs(x_potential_transfer) >= ContrAmt & v20_CorpHedgegrIn[pos_f, "CorpMatur"] < x_higher_maturity_ceiling & var_exit1 == 0){
    TransAmt <- min(c(x_potential_transfer, v20_CorpHedgegrIn[pos_f, "mv_rem"]))
    pos_f_t <- pos_f
    pos_f_low <- (pos_f + 1)
    #Start of the F moving down rehedging and A lifting cycle
    v20_CorpHedgegrOut_t <- v20_CorpHedgegrOut
    v20_CorpHedgegrIn_t <- v20_CorpHedgegrIn
    tbl_OldPairs <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1,ncol=10)
    colnames(tbl_OldPairs) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id", "GrNum", "PairNum")
    tbl_NewPairs <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1,ncol=10)
    colnames(tbl_NewPairs) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id", "GrNum", "PairNum")     
    var_m2 <- 1; # "var_m2" is group number, "var_m" is pair number
    var_exit2 <- 0;
    if (pos_f_low <= nrow(v20_CorpHedgegrIn_t)){
      while (pos_f_t >= 1 & pos_f_t < nrow(v20_CorpHedgegrIn_t) & v20_CorpHedgegrIn_t[pos_f_t, "CorpMatur"] < x_higher_maturity_ceiling & v20_CorpHedgegrIn_t[pos_f_low, "mv_hff"] >= ContrAmt & TransAmt >= ContrAmt & var_exit2 == 0){
        #Start of the F moving down re-hedging, one cycle equals to moving F security down one position
        var_x <- length(which(tbl_FFPairs[, xf_HdgSdId] == v20_CorpHedgegrIn_t[pos_f_low, "unique_id"]))   
        tbl_Rehedge <- matrix(tbl_FFPairs[which(tbl_FFPairs[,xf_HdgSdId] == v20_CorpHedgegrIn_t[pos_f_low, "unique_id"]),], var_x, 8);
        colnames(tbl_Rehedge) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id")
        if (var_x > 1){
          tbl_Rehedge <- tbl_Rehedge[order(tbl_Rehedge[ , x_NhdgSideMtr], decreasing = FALSE ),]
        }
        var_m <- 1
        transfer_amount_2 <- 0
        MaxRmTransAmt <- TransAmt; # MaxRmTransAmt and transfer_amount2 are temporary variables only used for rehedging cycle in while loop below
        while (var_m <= nrow(tbl_Rehedge) & MaxRmTransAmt >= ContrAmt){
          opposite_sec_id <- tbl_Rehedge[var_m, xf_NhgSdId]; 
          if (x_potential_transfer >= ContrAmt & abs(v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id), "CorpMatur"] - v20_CorpHedgegrIn_t[pos_f_t, "CorpMatur"]) <= MtrDstnc){
            CurTransAmt <- min(c(MaxRmTransAmt, tbl_Rehedge[var_m, "MatchAmt"]))
            transfer_amount_2 <- (transfer_amount_2 + CurTransAmt)
            existing_pair <- c(tbl_Rehedge[var_m, ], var_m2, var_m)
            if (xf_HdgSdId == "id_long"){
              new_pair      <- c(0, (v20_CorpHedgegrIn_t[pos_f_t, "unique_id"]),(v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id),"unique_id"]) , CurTransAmt, (v20_CorpHedgegrIn_t[pos_f_t, "CorpMatur"]), (v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id),"CorpMatur"]), x_hrcut, v1_FFPairId, var_m2, var_m); 
              v1_FFPairId <- (v1_FFPairId + 1);
            } else {
              new_pair      <- c(0, (v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id),"unique_id"]) , (v20_CorpHedgegrIn_t[pos_f_t, "unique_id"]), CurTransAmt, (v20_CorpHedgegrOut_t[which(v20_CorpHedgegrOut_t[, "unique_id"] == opposite_sec_id),"CorpMatur"]), (v20_CorpHedgegrIn_t[pos_f_t, "CorpMatur"]), x_hrcut, v1_FFPairId, var_m2, var_m); 
              v1_FFPairId <- (v1_FFPairId + 1);
            }
            tbl_OldPairs <- rbind(tbl_OldPairs, existing_pair);
            tbl_NewPairs <- rbind(tbl_NewPairs, new_pair); 
            v20_CorpHedgegrIn_t[pos_f_t, "mv_rem"] <- (v20_CorpHedgegrIn_t[pos_f_t, "mv_rem"] - CurTransAmt); 
            v20_CorpHedgegrIn_t[pos_f_t, "mv_hff"] <- (v20_CorpHedgegrIn_t[pos_f_t, "mv_hff"] + CurTransAmt); 
            v20_CorpHedgegrIn_t[pos_f_low, "mv_rem"] <- (v20_CorpHedgegrIn_t[pos_f_low, "mv_rem"] + CurTransAmt);
            v20_CorpHedgegrIn_t[pos_f_low, "mv_hff"] <- (v20_CorpHedgegrIn_t[pos_f_low, "mv_hff"] - CurTransAmt);
            MaxRmTransAmt <- (MaxRmTransAmt - CurTransAmt)
            var_m <- (var_m + 1)
          } else {
            break
          }
        }
        if (transfer_amount_2 >= ContrAmt){
          TransAmt <- transfer_amount_2
          pos_f_t <- (pos_f_t + 1); 
          pos_f_low <- (pos_f_low + 1); 
          var_m2 <- (var_m2 + 1)
        } else {
          var_exit2 <- 1
        }
      }
    }
    var_m2 <- (var_m2 - 1)
    # Below IF statement checks whether F security got pushed down to the neighboring category. If so, then all temporary tables (i.e. pairs and updated balances of individual F securities) get transmitted into permanent tables
    if (v20_CorpHedgegrIn[pos_f_t, "CorpMatur"] >= x_lower_maturity_ceiling){
      v20_CorpHedgegrIn[pos_f,"mv_rem"] <- (v20_CorpHedgegrIn[pos_f,"mv_rem"] - TransAmt)
      v20_CorpHedgegrIn[pos_f,"mv_hff"] <- (v20_CorpHedgegrIn[pos_f,"mv_hff"] + TransAmt)
      v20_CorpHedgegrIn[pos_f_t,"mv_hff"] <- (v20_CorpHedgegrIn[pos_f_t,"mv_hff"] - TransAmt)
      v20_CorpHedgegrIn[pos_f_t,"mv_rem"] <- (v20_CorpHedgegrIn[pos_f_t,"mv_rem"] + TransAmt)
      var_i2 <- 1
      while (var_i2 <= var_m2){
        TransRem <- TransAmt
        var_j2 <- 1
        while (var_j2 <= length(tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2)]) & TransRem >= 0.01){
          if (tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"] > TransRem){
            tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"] <- (tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"] - TransRem)
            tbl_FFPairs[which(tbl_FFPairs[,xf_NhgSdId] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), xf_NhgSdId] & tbl_FFPairs[,xf_HdgSdId] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), xf_HdgSdId] & tbl_FFPairs[, "pair_id"] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "pair_id"]), "MatchAmt"] <- tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"]
            tbl_NewPairs[which(tbl_NewPairs[ , "GrNum"] == var_i2 & tbl_NewPairs[ , "PairNum"] == var_j2), "MatchAmt"] <- TransRem
            matching_pair <- c(nrow(tbl_FFPairs), tbl_NewPairs[which(tbl_NewPairs[ , "GrNum"] == var_i2 & tbl_NewPairs[ , "PairNum"] == var_j2), c("id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id")]); 
            tbl_FFPairs <- rbind(tbl_FFPairs, matching_pair);
            rownames(tbl_FFPairs)[nrow(tbl_FFPairs)] <- matching_pair_gr
            TransRem <- 0
            break
          } else {
            TransRem <- (TransRem - tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"])
            tbl_NewPairs[which(tbl_NewPairs[ , "GrNum"] == var_i2 & tbl_NewPairs[ , "PairNum"] == var_j2), "MatchAmt"] <- tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"]
            tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"] <- 0
            tbl_FFPairs[which(tbl_FFPairs[,xf_NhgSdId] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), xf_NhgSdId] & tbl_FFPairs[,xf_HdgSdId] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), xf_HdgSdId] & tbl_FFPairs[, "pair_id"] == tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "pair_id"]), "MatchAmt"] <- tbl_OldPairs[which(tbl_OldPairs[ , "GrNum"] == var_i2 & tbl_OldPairs[ , "PairNum"] == var_j2), "MatchAmt"]
            matching_pair <- c(nrow(tbl_FFPairs), tbl_NewPairs[which(tbl_NewPairs[ , "GrNum"] == var_i2 & tbl_NewPairs[ , "PairNum"] == var_j2), c("id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id")])
            tbl_FFPairs <- rbind(tbl_FFPairs, matching_pair);
            rownames(tbl_FFPairs)[nrow(tbl_FFPairs)] <- matching_pair_gr
            tbl_FFPairs <- tbl_FFPairs[which(tbl_FFPairs[, "MatchAmt"] >= 0.01),]
            var_j2 <- (var_j2 + 1)                
          }
        }
        var_i2 <- (var_i2 + 1)
      }
      tbl_FFPairs <- tbl_FFPairs[which(tbl_FFPairs[, "MatchAmt"] >= 0.01),]
    } else {
      var_exit1 <- 1
    }
    temp_ff <- aggregate(tbl_FFPairs[,c("MatchAmt")], by=list(tbl_FFPairs[,2],tbl_FFPairs[,3],tbl_FFPairs[,5],tbl_FFPairs[,6],tbl_FFPairs[,7]), "sum")
    tbl_FFPairs2 <- matrix(c(0, 0, 0, 0.02, 0, 0, 0, 0), nrow=1,ncol=8)
    colnames(tbl_FFPairs2) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id")
    FFPairId <- max(tbl_FFPairs[,8])
    i <- 2
    while( i <= length(temp_ff[[1]]) ) {
      matching_pair_ff <- c(nrow(tbl_FFPairs2), temp_ff[i,1], temp_ff[i,2],temp_ff[i,6] , temp_ff[i,3], temp_ff[i,4], temp_ff[i,5], FFPairId)
      FFPairId <-  FFPairId + 1
      tbl_FFPairs2 <- rbind(tbl_FFPairs2, matching_pair_ff) 
      i<- i + 1
    } 
  }
  xyz_3 <- list("potential_transfer" = x_potential_transfer, "CorpHedgegrIn" = v20_CorpHedgegrIn, "CorpHedgegrOut" = v20_CorpHedgegrOut, "pairtbl" = tbl_FFPairs, "var_j1" = pos_f, "PairId" = v1_FFPairId)
  remove(x_potential_transfer, v20_CorpHedgegrIn, v20_CorpHedgegrOut, tbl_FFPairs, pos_f, pos_f, v1_FFPairId)
  return(xyz_3)
}

#F push-down function
#This function is in charge of moving F security down to higher maturity by re-hedging
#F-A hedging function

#Explanation of variables below: 
#y_FbandBal - net balance (long positions minus short positions) of given F category, 
#v_CorpHedgegrIn - table of selected hedging-group hedging-side (short or long) F securities, 
#v_CorpHedgegrOut - table of selected hedging-group non-hedging-side F securities, 
#v_UsgHedgeband - table of selected sub-category hedging-side A securities, 
#var_i1 - current position indicator for hedging A table, 
#var_j1 - current position indicator for hedging F table, 
#tbl_FFPairs - table of all existing (active) f-f (2 corporate debt securities) pairs, 
#tbl_FAPairs - table of all existing f-a (corporate debt security and US government security) pairs, 
#v_Exit0 - artificial 0/1 indicator that forces and exit from the main while loop in fa_hedging_part1 function, 
#y_rltv_maturity - relative maturity, that is the number of months of maturity within which 2 securities must be to hedge them, 
#y_hrcut - percentage haircut for f-f pairs expressed in decimal point, 
#y_hrcut_fa - percentage haircut for f-a pairs expressed in decimal point, 
#v_CorpHdgSideId - name of the column in tbl_FAPairs that indicates hedging F security side - "id_long" or "id_short", 
#v_CorpNhdgSideId - name of the column in tbl_FAPairs that indicates non-hedging A security side - "id_long" or "id_short",  
#y_NhdgSideMtr - name of the column in tbl_FFPairs, 
#v2_FAPairId - F-A pairs table unique identified of a pair, 
#v2_FFPairId    - F-F pairs table unique identified of a pair
fa_hedging_3_0 <- function(y_FbandBal, v_CorpHedgegrIn, v_CorpHedgegrOut, v_UsgHedgeband, tbl_FFPairs, tbl_FAPairs, v_Exit0, y_rltv_maturity, y_hrcut, y_hrcut_fa, v_CorpHdgSideId, v_CorpNhdgSideId, y_NhdgSideMtr, v2_FAPairId, v2_FFPairId, ContrAmt, matching_pair_gr, y_fhedge_side){
  var_i1 <- nrow(v_UsgHedgeband)
  var_j1 <- nrow(v_CorpHedgegrIn)
  var_curfsec_amt <- v_CorpHedgegrIn[var_j1,"mv_rem"];
  while (var_i1 >= 1 & var_j1 >= 1 & abs(y_FbandBal) >= ContrAmt & v_Exit0 == 0){
    #Next line checks if current F security is fully hedged: if so, it will move to the next one with lower maturity (line 365);
    if (v_CorpHedgegrIn[var_j1,"mv_rem"] < ContrAmt & var_j1 > 1){
      var_j1 <- (var_j1 - 1); 
    } else if (v_CorpHedgegrIn[var_j1,"mv_rem"] < ContrAmt & var_j1 <= 1){
      #F security indicator is standing at the top of the table, and current security non-offset value is 0, hence need to exit the cycle;
      v_Exit0 <- 1;
    } else {# Need to check if this "else" is appropriate or each of 2 remaining cases need to be considered alone
      if (v_UsgHedgeband[var_i1,"mv_gov_rem"] < 0.01 & var_i1 > 1){
        var_i1 <- (var_i1 - 1)
      } else if (v_UsgHedgeband[var_i1,"mv_gov_rem"] < 0.01 & var_i1 <= 1){
        v_Exit0 <- 1;
      } else {
        if (v_UsgHedgeband[var_i1, "UsgMatur"] >= v_CorpHedgegrIn[var_j1, "CorpMatur"]){
          if (nrow(v_CorpHedgegrOut)>=1){
            var_curfsec_amt <- v_CorpHedgegrIn[var_j1,"mv_rem"]
            TransAmt <- min(c(y_FbandBal, v_CorpHedgegrIn[var_j1, "mv_rem"]))
            xyz_3 <- f_movement(y_FbandBal, v_CorpHedgegrIn, v_CorpHedgegrOut, v_UsgHedgeband, v_CorpHdgSideId, v_CorpNhdgSideId, y_NhdgSideMtr, var_curfsec_amt, ContrAmt, var_i1, var_j1, tbl_FFPairs, tbl_FAPairs, y_fhedge_side, v_Exit0, y_rltv_maturity, y_hrcut, y_hrcut_fa, 3)
            y_FbandBal <- xyz_3[["balance_table_band"]]
            v_UsgHedgeband <- xyz_3[["govsec_band"]]
            v_CorpHedgegrIn <- xyz_3[["CorpHedgegrIn"]]
            v_CorpHedgegrOut <- xyz_3[["CorpHedgegrOut"]]
            tbl_FFPairs <- xyz_3[["pairtbl"]]
            tbl_FAPairs <- xyz_3[["pairtbl_fa"]]
            var_i1 <- xyz_3[["var_i1"]]
            var_j1 <- xyz_3[["var_j1"]]
            v_Exit0 <- xyz_3[["v_Exit0"]]
            v2_FAPairId <- xyz_3[["FAPairId"]]
            v2_FFPairId    <- xyz_3[["PairId"]]
            if (v_CorpHedgegrIn[var_j1,"mv_rem"] < 0.01){
              var_j1 <- (var_j1 - 1)
            }
          } else {
            if (v_UsgHedgeband[var_i1, "UsgMatur"] - v_CorpHedgegrIn[var_j1, "CorpMatur"] <= y_rltv_maturity){
              if  (abs(y_FbandBal) >= min(c(v_UsgHedgeband[var_i1,"mv_gov_rem"], v_CorpHedgegrIn[var_j1,"mv_rem"]))){
                hedging_amount <- min(c(v_UsgHedgeband[var_i1,"mv_gov_rem"], v_CorpHedgegrIn[var_j1,"mv_rem"]))
                if (y_fhedge_side == 0){
                  MatchFA <- c(nrow(tbl_FAPairs), v_UsgHedgeband[var_i1,"unique_id_gov"], v_CorpHedgegrIn[var_j1,"unique_id"], hedging_amount, v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"CorpMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
                } else {
                  MatchFA <- c(nrow(tbl_FAPairs), v_CorpHedgegrIn[var_j1,"unique_id"], v_UsgHedgeband[var_i1,"unique_id_gov"], hedging_amount, v_CorpHedgegrIn[var_j1,"CorpMatur"], v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
                }
                tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA)
                if (v_UsgHedgeband[var_i1,"mv_gov_rem"] >  v_CorpHedgegrIn[var_j1,"mv_rem"]){
                  if (y_FbandBal >= 0){
                    y_FbandBal <- (y_FbandBal - hedging_amount)
                  } else {
                    y_FbandBal <- (y_FbandBal + hedging_amount)
                  }
                  v_UsgHedgeband[var_i1,"mv_gov_rem"] <- (v_UsgHedgeband[var_i1,"mv_gov_rem"] - hedging_amount); 
                  v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); 
                  v_CorpHedgegrIn[var_j1,"mv_rem"] <- 0; 
                  var_j1 <- (var_j1 - 1); 
                  rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr; #Not sure what the purpose of this line is                                                                                                                                                
                } else if (v_UsgHedgeband[var_i1,"mv_gov_rem"] == v_CorpHedgegrIn[var_j1,"mv_rem"]){
                  if (y_FbandBal >= 0){
                    y_FbandBal <- (y_FbandBal - hedging_amount)
                  } else {
                    y_FbandBal <- (y_FbandBal + hedging_amount)
                  }
                  v_UsgHedgeband[var_i1,"mv_gov_rem"] <- 0; 
                  v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- 0; 
                  if (var_i1 > 1){
                    var_i1 <- (var_i1 - 1);
                  } else {
                    v_Exit0 <- 1
                  }
                  var_j1 <- (var_j1 - 1); 
                  rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr;
                } else {
                  if (y_FbandBal >= 0){
                    y_FbandBal <- (y_FbandBal -  v_UsgHedgeband[var_i1,"mv_gov_rem"])
                  } else {
                    y_FbandBal <- (y_FbandBal +  v_UsgHedgeband[var_i1,"mv_gov_rem"])
                  }
                  v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- (v_CorpHedgegrIn[var_j1,"mv_rem"] - hedging_amount); 
                  v_UsgHedgeband[var_i1,"mv_gov_rem"] <- 0; 
                  if (var_i1 > 1){
                    var_i1 <- (var_i1 - 1);
                  } else {
                    v_Exit0 <- 1
                  }
                  rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr;
                }
              } else {
                hedging_amount <- abs(y_FbandBal)
                if (y_fhedge_side == 0){
                  MatchFA <- c(nrow(tbl_FAPairs), v_UsgHedgeband[var_i1,"unique_id_gov"], v_CorpHedgegrIn[var_j1,"unique_id"], hedging_amount, v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"CorpMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
                } else {
                  MatchFA <- c(nrow(tbl_FAPairs), v_CorpHedgegrIn[var_j1,"unique_id"], v_UsgHedgeband[var_i1,"unique_id_gov"], hedging_amount, v_CorpHedgegrIn[var_j1,"CorpMatur"], v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
                }
                tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA); 
                v_UsgHedgeband[var_i1,"mv_gov_rem"] <- (v_UsgHedgeband[var_i1,"mv_gov_rem"] - hedging_amount); 
                v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- (v_CorpHedgegrIn[var_j1,"mv_rem"] - hedging_amount);   
                rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr; 
                y_FbandBal   <- 0
              }
            } else {
              var_i1 <- (var_i1 - 1)
            }
          }
        } else {
          #here maturity of F security is higher than maturity of A security 
          if (abs(v_CorpHedgegrIn[var_j1, "CorpMatur"] - v_UsgHedgeband[var_i1, "UsgMatur"]) <= y_rltv_maturity){
            #Check whether we can hedge A and F securities
            if  (abs(y_FbandBal) >= min(c(v_UsgHedgeband[var_i1,"mv_gov_rem"], v_CorpHedgegrIn[var_j1,"mv_rem"]))){
              
              hedging_amount <- min(c(v_UsgHedgeband[var_i1,"mv_gov_rem"], v_CorpHedgegrIn[var_j1,"mv_rem"]))
              if (y_fhedge_side == 0){
                MatchFA <- c(nrow(tbl_FAPairs), v_UsgHedgeband[var_i1,"unique_id_gov"], v_CorpHedgegrIn[var_j1,"unique_id"], hedging_amount, v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"CorpMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
              } else {
                MatchFA <- c(nrow(tbl_FAPairs), v_CorpHedgegrIn[var_j1,"unique_id"], v_UsgHedgeband[var_i1,"unique_id_gov"], hedging_amount, v_CorpHedgegrIn[var_j1,"CorpMatur"], v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
              }
              tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA)
              if (v_UsgHedgeband[var_i1,"mv_gov_rem"] >  v_CorpHedgegrIn[var_j1,"mv_rem"]){
                if (y_FbandBal >= 0){
                  y_FbandBal <- (y_FbandBal - hedging_amount)
                } else {
                  y_FbandBal <- (y_FbandBal + hedging_amount)
                }
                v_UsgHedgeband[var_i1,"mv_gov_rem"] <- (v_UsgHedgeband[var_i1,"mv_gov_rem"] - hedging_amount); 
                v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); 
                v_CorpHedgegrIn[var_j1,"mv_rem"] <- 0; 
                var_j1 <- (var_j1 - 1); 
                rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr; #Not sure what the purpose of this line is                                                                                                                                                
              } else if (v_UsgHedgeband[var_i1,"mv_gov_rem"] == v_CorpHedgegrIn[var_j1,"mv_rem"]){
                if (y_FbandBal >= 0){
                  y_FbandBal <- (y_FbandBal - hedging_amount)
                } else {
                  y_FbandBal <- (y_FbandBal + hedging_amount)
                }
                v_UsgHedgeband[var_i1,"mv_gov_rem"] <- 0; 
                v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- 0; 
                if (var_i1 > 1){
                  var_i1 <- (var_i1 - 1);
                } else {
                  v_Exit0 <- 1
                }
                var_j1 <- (var_j1 - 1); 
                rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr;
              } else {
                if (y_FbandBal >= 0){
                  y_FbandBal <- (y_FbandBal -  v_UsgHedgeband[var_i1,"mv_gov_rem"])
                } else {
                  y_FbandBal <- (y_FbandBal +  v_UsgHedgeband[var_i1,"mv_gov_rem"])
                }
                v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- (v_CorpHedgegrIn[var_j1,"mv_rem"] - hedging_amount); 
                v_UsgHedgeband[var_i1,"mv_gov_rem"] <- 0; 
                if (var_i1 > 1){
                  var_i1 <- (var_i1 - 1);
                } else {
                  v_Exit0 <- 1
                }
                rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr;
              }
            } else {
              hedging_amount <- abs(y_FbandBal)
              if (y_fhedge_side == 0){
                MatchFA <- c(nrow(tbl_FAPairs), v_UsgHedgeband[var_i1,"unique_id_gov"], v_CorpHedgegrIn[var_j1,"unique_id"], hedging_amount, v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"CorpMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
              } else {
                MatchFA <- c(nrow(tbl_FAPairs), v_CorpHedgegrIn[var_j1,"unique_id"], v_UsgHedgeband[var_i1,"unique_id_gov"], hedging_amount, v_CorpHedgegrIn[var_j1,"CorpMatur"], v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
              }
              tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA); 
              v_UsgHedgeband[var_i1,"mv_gov_rem"] <- (v_UsgHedgeband[var_i1,"mv_gov_rem"] - hedging_amount); 
              v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- (v_CorpHedgegrIn[var_j1,"mv_rem"] - hedging_amount);   
              rownames(tbl_FAPairs)[nrow(tbl_FAPairs)] <- matching_pair_gr; 
              y_FbandBal   <- 0
            }
          } else {
            var_j1 <- (var_j1 - 1)
          }
        }
      }
    }
  }
  temp_ff <- aggregate(tbl_FFPairs[,c("MatchAmt")], by=list(tbl_FFPairs[,2],tbl_FFPairs[,3],tbl_FFPairs[,5],tbl_FFPairs[,6],tbl_FFPairs[,7]), "sum")
  tbl_FFPairs2 <- matrix(c(0, 0, 0, 0.02, 0, 0, 0, 0), nrow=1,ncol=8)
  colnames(tbl_FFPairs2) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "HcutRate", "pair_id")
  FFPairId <- max(tbl_FFPairs[,8])
  i <- 2
  while( i <= length(temp_ff[[1]]) ){
    matching_pair_ff <- c(nrow(tbl_FFPairs2), temp_ff[i,1], temp_ff[i,2],temp_ff[i,6] , temp_ff[i,3], temp_ff[i,4], temp_ff[i,5], FFPairId)
    FFPairId <-  FFPairId + 1
    tbl_FFPairs2 <- rbind(tbl_FFPairs2, matching_pair_ff)
    i<- i + 1
  }
  temp_fa <- aggregate(tbl_FAPairs[,c("MatchAmt")], by=list(tbl_FAPairs[,2],tbl_FAPairs[,3],tbl_FAPairs[,5],tbl_FAPairs[,6],tbl_FAPairs[,8], tbl_FAPairs[,9]), "sum")
  f30_PairsFA <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=1,ncol=10)
  colnames(f30_PairsFA) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh", "FOrigID", "f_sec_side", "HcutRate", "pair_id")
  i <- 2
  while( i <= length(temp_fa[[1]]) ){
    MatchFA <- c(nrow(f30_PairsFA), temp_fa[i,1], temp_fa[i,2],temp_fa[i,7] , temp_fa[i,3], temp_fa[i,4], 0, temp_fa[i,5], temp_fa[i,6], FAPairId)
    FAPairId <-  FAPairId + 1
    f30_PairsFA <- rbind(f30_PairsFA, MatchFA) 
    i<- i + 1
  }
  xyz_3_0 <- list("y_FbandBal" = y_FbandBal, "y_govsec_band" = v_UsgHedgeband, "v_CorpHedgegrIn" = v_CorpHedgegrIn, "v_CorpHedgegrOut" = v_CorpHedgegrOut, "y_pairtbl" = tbl_FFPairs2, "y_pairtbl_fa" = f30_PairsFA, "FAPairId" = v2_FAPairId, "PairId" = v2_FFPairId)
  remove(y_FbandBal, v_UsgHedgeband, v_CorpHedgegrIn, v_CorpHedgegrOut, tbl_FFPairs2, f30_PairsFA, v2_FAPairId, v2_FFPairId)
  return(xyz_3_0)  
}

#Step 3.0 Section C:  Functions run
print("Step 3 Function Run")
if (distrib >= 3){
  if (fcat_bal[9] >= ContrAmt & nrow(FsecBnd_Ln[[9]]) > 0){
    tmp_AhdgGr <- AsecBnd_Sh[[7]]
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Sh[[8]])
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Sh[[9]])
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net long balance group 9")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[9], FsecBnd_Ln[[9]], CorpHedgegrS[[4]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 120, 0.035, 0.03, "id_long", "id_short", "MtrSh", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr4", 1)
    fcat_bal[9]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Ln[[9]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    CorpHedgegrS[[4]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Sh[[7]]   <- subset(tmp_AhdgGr, UsgMatur <  240                  )
    AsecBnd_Sh[[8]]   <- subset(tmp_AhdgGr, UsgMatur >= 240 & UsgMatur < 300 )
    AsecBnd_Sh[[9]]   <- subset(tmp_AhdgGr, UsgMatur >= 300                  )
    FsecBnd_Sh[[7]] <- subset(CorpHedgegrS[[4]], CorpMatur <  240                  )
    FsecBnd_Sh[[8]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 240 & CorpMatur < 300)
    FsecBnd_Sh[[9]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 300                  )
    FhgrL[[4]] <- FsecBnd_Ln[[7]]
    FhgrL[[4]] <- rbind(FhgrL[[4]], FsecBnd_Ln[[8]])
    FhgrL[[4]] <- rbind(FhgrL[[4]], FsecBnd_Ln[[9]])
    remove(xyz_3_0)  
  } else if (fcat_bal[9] <= -ContrAmt & nrow(FsecBnd_Sh[[9]]) > 0){
    tmp_AhdgGr <- AsecBnd_Ln[[7]]
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Ln[[8]])
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Ln[[9]])
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net short balance group 9")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[9], FsecBnd_Sh[[9]], FhgrL[[4]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 120, 0.035, 0.03, "id_short", "id_long", "MtrL", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr4", 0)
    fcat_bal[9]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Sh[[9]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    FhgrL[[4]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Ln[[7]]   <- subset(tmp_AhdgGr, UsgMatur <  240                                     )
    AsecBnd_Ln[[8]]   <- subset(tmp_AhdgGr, UsgMatur >= 240 & UsgMatur < 300)
    AsecBnd_Ln[[9]]   <- subset(tmp_AhdgGr, UsgMatur >= 300                                     )
    FsecBnd_Ln[[7]] <- subset(FhgrL[[4]], CorpMatur <  240                                 )
    FsecBnd_Ln[[8]] <- subset(FhgrL[[4]], CorpMatur >= 240 & CorpMatur < 300)
    FsecBnd_Ln[[9]] <- subset(FhgrL[[4]], CorpMatur >= 300                                 )
    CorpHedgegrS[[4]] <- FsecBnd_Sh[[7]]
    CorpHedgegrS[[4]] <- rbind(CorpHedgegrS[[4]], FsecBnd_Sh[[8]])
    CorpHedgegrS[[4]] <- rbind(CorpHedgegrS[[4]], FsecBnd_Sh[[9]])  
    remove(xyz_3_0)
  }
  
  if (fcat_bal[8] >= ContrAmt & nrow(FsecBnd_Ln[[8]]) > 0){
    if (fcat_bal[9] <= -ContrAmt & nrow(FhgrL[[4]]) > 0 & nrow(CorpHedgegrS[[4]]) > 0){
      transfer_potential <- min(abs(fcat_bal[8]), abs(fcat_bal[9]))
      if (prnt_f == "Y"){print("Function f_movement_without_hedge run for net long balance group 8")}
      xyz_3 <- f_movement_without_hedge(transfer_potential, FhgrL[[4]], CorpHedgegrS[[4]], "id_long", "id_short", "MtrSh", tbl_FFPairs, 120, 0.035, 240, 300, 360, ContrAmt, "ff_matching_pair_gr4")
      FhgrL[[4]]  <- xyz_3[["CorpHedgegrIn"]]
      CorpHedgegrS[[4]]  <- xyz_3[["CorpHedgegrOut"]]
      tbl_FFPairs <- xyz_3[["pairtbl"]]
      PairId <- xyz_3[["PairId"]]
      FsecBnd_Sh[[8]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 240  & CorpMatur < 300)
      FsecBnd_Sh[[9]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 300                                  )
      FsecBnd_Ln[[8]] <- subset(FhgrL[[4]], CorpMatur >= 240  & CorpMatur < 300)
      FsecBnd_Ln[[9]] <- subset(FhgrL[[4]], CorpMatur >= 300                                  )
      remove(xyz_3)	
    }
    tmp_AhdgGr <- AsecBnd_Sh[[7]]
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Sh[[8]])
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net long balance group 8")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[8], FsecBnd_Ln[[8]], CorpHedgegrS[[4]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 120, 0.035, 0.03, "id_long", "id_short", "MtrSh", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr4", 1)
    fcat_bal[9]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Ln[[8]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    CorpHedgegrS[[4]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Sh[[7]]   <- subset(tmp_AhdgGr, UsgMatur <  240                  )
    AsecBnd_Sh[[8]]   <- subset(tmp_AhdgGr, UsgMatur >= 240 & UsgMatur < 300 )
    FsecBnd_Sh[[7]] <- subset(CorpHedgegrS[[4]], CorpMatur <  240                  )
    FsecBnd_Sh[[8]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 240 & CorpMatur < 300)
    FsecBnd_Sh[[9]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 300                  )
    FhgrL[[4]] <- FsecBnd_Ln[[7]]
    FhgrL[[4]] <- rbind(FhgrL[[4]], FsecBnd_Ln[[8]])
    FhgrL[[4]] <- rbind(FhgrL[[4]], FsecBnd_Ln[[9]])
    remove(xyz_3_0)  
  } else if (fcat_bal[8] <= -ContrAmt & nrow(FsecBnd_Sh[[8]]) > 0){
    if (fcat_bal[9] >= ContrAmt & nrow(CorpHedgegrS[[4]]) > 0 & nrow(FhgrL[[4]]) > 0){
      transfer_potential <- min(abs(fcat_bal[8]), abs(fcat_bal[9]))
      if (prnt_f == "Y"){print("Function f_movement_without_hedge run for net short balance group 8")}
      xyz_3 <- f_movement_without_hedge(transfer_potential, CorpHedgegrS[[4]], FhgrL[[4]], "id_short", "id_long", "MtrL", tbl_FFPairs, 120, 0.035, 240, 300, 360, ContrAmt, "ff_matching_pair_gr4")
      CorpHedgegrS[[4]]  <- xyz_3[["CorpHedgegrIn"]]
      FhgrL[[4]]  <- xyz_3[["CorpHedgegrOut"]]
      tbl_FFPairs <- xyz_3[["pairtbl"]]
      PairId <- xyz_3[["PairId"]]
      FsecBnd_Sh[[8]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 240  & CorpMatur < 300)
      FsecBnd_Sh[[9]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 300                   )
      FsecBnd_Ln[[8]] <- subset(FhgrL[[4]], CorpMatur >= 240  & CorpMatur < 300)
      FsecBnd_Ln[[9]] <- subset(FhgrL[[4]], CorpMatur >= 300                   )
      remove(xyz_3)	
    }
    tmp_AhdgGr <- AsecBnd_Ln[[7]]
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Ln[[8]])
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net short balance group 8")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[8], FsecBnd_Sh[[8]], FhgrL[[4]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 120, 0.035, 0.03, "id_short", "id_long", "MtrL", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr4", 0)
    fcat_bal[8]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Sh[[8]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    FhgrL[[4]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Ln[[7]]   <- subset(tmp_AhdgGr, UsgMatur <  240                                     )
    AsecBnd_Ln[[8]]   <- subset(tmp_AhdgGr, UsgMatur >= 240 & UsgMatur < 300)
    FsecBnd_Ln[[7]] <- subset(FhgrL[[4]], CorpMatur <  240                                 )
    FsecBnd_Ln[[8]] <- subset(FhgrL[[4]], CorpMatur >= 240 & CorpMatur < 300)
    FsecBnd_Ln[[9]] <- subset(FhgrL[[4]], CorpMatur >= 300                                 )
    CorpHedgegrS[[4]] <- FsecBnd_Sh[[7]]
    CorpHedgegrS[[4]] <- rbind(CorpHedgegrS[[4]], FsecBnd_Sh[[8]])
    CorpHedgegrS[[4]] <- rbind(CorpHedgegrS[[4]], FsecBnd_Sh[[9]])
    remove(xyz_3_0)  
  }
  
  if (fcat_bal[7] >= ContrAmt & nrow(FsecBnd_Ln[[7]]) > 0){
    if (fcat_bal[8] <= -ContrAmt & nrow(CorpHedgegrS[[4]]) > 0 & nrow(FhgrL[[4]]) > 0){
      transfer_potential <- min(abs(fcat_bal[7]), abs(fcat_bal[8]))
      if (prnt_f == "Y"){print("Function f_movement_without_hedge run for net long balance group 7")}
      xyz_3 <- f_movement_without_hedge(transfer_potential, FhgrL[[4]], CorpHedgegrS[[4]], "id_long", "id_short", "MtrSh", tbl_FFPairs, 120, 0.035, 180, 240, 300, ContrAmt, "ff_matching_pair_gr4")
      FhgrL[[4]]  <- xyz_3[["CorpHedgegrIn"]]
      CorpHedgegrS[[4]]  <- xyz_3[["CorpHedgegrOut"]]
      tbl_FFPairs <- xyz_3[["pairtbl"]]
      PairId <- xyz_3[["PairId"]]
      FsecBnd_Sh[[7]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 180  & CorpMatur < 240)
      FsecBnd_Sh[[8]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 240  & CorpMatur < 300)
      FsecBnd_Ln[[7]] <- subset(FhgrL[[4]], CorpMatur >= 180  & CorpMatur < 240)
      FsecBnd_Ln[[8]] <- subset(FhgrL[[4]], CorpMatur >= 240  & CorpMatur < 300)
      remove(xyz_3)
    }
    tmp_AhdgGr <- AsecBnd_Sh[[7]]
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net long balance group 7")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[7], FsecBnd_Ln[[7]], CorpHedgegrS[[4]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 120, 0.035, 0.03, "id_long", "id_short", "MtrSh", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr4", 1)
    fcat_bal[7]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Ln[[7]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    CorpHedgegrS[[4]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Sh[[7]]   <- subset(tmp_AhdgGr, UsgMatur <  240                                     )
    FsecBnd_Sh[[7]] <- subset(CorpHedgegrS[[4]], CorpMatur <  240                                 )
    FsecBnd_Sh[[8]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 240 & CorpMatur < 300)
    FsecBnd_Sh[[9]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 300                                 )
    FhgrL[[4]] <- FsecBnd_Ln[[7]]
    FhgrL[[4]] <- rbind(FhgrL[[4]], FsecBnd_Ln[[8]])
    FhgrL[[4]] <- rbind(FhgrL[[4]], FsecBnd_Ln[[9]])
    remove(xyz_3_0)  
  } else if (fcat_bal[7] <= -ContrAmt & nrow(FsecBnd_Sh[[7]]) > 0){
    if (fcat_bal[8] >= ContrAmt & nrow(CorpHedgegrS[[4]]) > 0 & nrow(FhgrL[[4]]) > 0){
      transfer_potential <- min(abs(fcat_bal[7]), abs(fcat_bal[8]))
      if (prnt_f == "Y"){print("Function f_movement_without_hedge run for net short balance group 7")}
      xyz_3 <- f_movement_without_hedge(transfer_potential, CorpHedgegrS[[4]], FhgrL[[4]], "id_short", "id_long", "MtrL", tbl_FFPairs, 120, 0.035, 180, 240, 300, ContrAmt, "ff_matching_pair_gr4")
      CorpHedgegrS[[4]]  <- xyz_3[["CorpHedgegrIn"]]
      FhgrL[[4]]  <- xyz_3[["CorpHedgegrOut"]]
      tbl_FFPairs <- xyz_3[["pairtbl"]]
      PairId <- xyz_3[["PairId"]]
      FsecBnd_Sh[[7]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 180  & CorpMatur < 240)
      FsecBnd_Sh[[8]] <- subset(CorpHedgegrS[[4]], CorpMatur >= 240  & CorpMatur < 300)
      FsecBnd_Ln[[7]] <- subset(FhgrL[[4]], CorpMatur >= 180  & CorpMatur < 240)
      FsecBnd_Ln[[8]] <- subset(FhgrL[[4]], CorpMatur >= 240  & CorpMatur < 300)
      remove(xyz_3)
    }
    tmp_AhdgGr <- AsecBnd_Ln[[7]]
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net short balance group 7")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[7], FsecBnd_Sh[[7]], FhgrL[[4]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 120, 0.035, 0.03, "id_short", "id_long", "MtrL", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr4", 0)
    fcat_bal[7]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Sh[[7]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    FhgrL[[4]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Ln[[7]]   <- subset(tmp_AhdgGr, UsgMatur <  240                  )
    FsecBnd_Ln[[7]] <- subset(FhgrL[[4]], CorpMatur <  240                  )
    FsecBnd_Ln[[8]] <- subset(FhgrL[[4]], CorpMatur >= 240 & CorpMatur < 300)
    FsecBnd_Ln[[9]] <- subset(FhgrL[[4]], CorpMatur >= 300                  )
    CorpHedgegrS[[4]] <- FsecBnd_Sh[[7]]
    CorpHedgegrS[[4]] <- rbind(CorpHedgegrS[[4]], FsecBnd_Sh[[8]])
    CorpHedgegrS[[4]] <- rbind(CorpHedgegrS[[4]], FsecBnd_Sh[[9]])
    remove(xyz_3_0)
  }
  
  if (fcat_bal[6] >= ContrAmt & nrow(FsecBnd_Ln[[6]]) > 0){
    tmp_AhdgGr <- AsecBnd_Sh[[6]]
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net long balance group 6")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[6], FsecBnd_Ln[[6]], FsecBnd_Sh[[6]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 24, 0.0325, 0.0275, "id_long", "id_short", "MtrSh", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr3", 1)
    fcat_bal[6]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Ln[[6]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    FsecBnd_Sh[[6]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Sh[[6]] <- tmp_AhdgGr
    remove(xyz_3_0)
  } else if (fcat_bal[6] <= -ContrAmt & nrow(FsecBnd_Sh[[6]]) > 0){
    tmp_AhdgGr <- AsecBnd_Ln[[6]]
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net short balance group 6")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[6], FsecBnd_Sh[[6]], FsecBnd_Ln[[6]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 24, 0.0325, 0.0275, "id_short", "id_long", "MtrL", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr3", 0)
    fcat_bal[6]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Sh[[6]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    FsecBnd_Ln[[6]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Ln[[6]] <- tmp_AhdgGr
    remove(xyz_3_0)
  }
  CorpHedgegrS[[3]] <- FsecBnd_Sh[[6]]
  FhgrL[[3]] <- FsecBnd_Ln[[6]]
  if (fcat_bal[5] >= ContrAmt & nrow(FsecBnd_Ln[[5]]) > 0){
    tmp_AhdgGr <- AsecBnd_Sh[[5]]
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net long balance group 5")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[5], FsecBnd_Ln[[5]], FsecBnd_Sh[[5]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 9, 0.03, 0.025, "id_long", "id_short", "MtrSh", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr2", 1)
    fcat_bal[5]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Ln[[5]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    FsecBnd_Sh[[5]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Sh[[5]] <- tmp_AhdgGr
    remove(xyz_3_0)
  } else if (fcat_bal[5] <= -ContrAmt & nrow(FsecBnd_Sh[[5]]) > 0){
    tmp_AhdgGr <- AsecBnd_Ln[[5]]
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net short balance group 5")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[5], FsecBnd_Sh[[5]], FsecBnd_Ln[[5]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 9, 0.03, 0.025, "id_short", "id_long", "MtrL", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr2", 0)
    fcat_bal[5]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Sh[[5]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    FsecBnd_Ln[[5]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Ln[[5]] <- tmp_AhdgGr
    remove(xyz_3_0)
  }
  CorpHedgegrS[[2]] <- FsecBnd_Sh[[5]]
  FhgrL[[2]] <- FsecBnd_Ln[[5]]
  if (fcat_bal[4] >= ContrAmt & nrow(FsecBnd_Ln[[4]]) > 0){
    tmp_AhdgGr <- AsecBnd_Sh[[3]]
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Sh[[4]])
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net long balance group 4")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[4], FsecBnd_Ln[[4]], CorpHedgegrS[[1]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 6, 0.0175, 0.015, "id_long", "id_short", "MtrSh", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr1", 1)
    fcat_bal[4]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Ln[[4]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    CorpHedgegrS[[1]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Sh[[4]] <- subset(tmp_AhdgGr, UsgMatur >= 36                                      )
    AsecBnd_Sh[[3]] <- subset(tmp_AhdgGr, UsgMatur >= 24  & UsgMatur < 36 )
    FsecBnd_Sh[[1]] <- subset(CorpHedgegrS[[1]], CorpMatur <  12                                  )
    FsecBnd_Sh[[2]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 12  & CorpMatur < 24 )
    FsecBnd_Sh[[3]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 24  & CorpMatur < 36 )
    FsecBnd_Sh[[4]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 36                                  )
    FhgrL[[1]] <- FsecBnd_Ln[[1]]
    for (i in 2:4){FhgrL[[1]] <- rbind(FhgrL[[1]], FsecBnd_Ln[[i]])}
    remove(xyz_3_0)
  } else if (fcat_bal[4] <= -ContrAmt & nrow(FsecBnd_Sh[[4]]) > 0){
    tmp_AhdgGr <- AsecBnd_Ln[[3]]
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Ln[[4]])
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net short balance group 4")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[4], FsecBnd_Sh[[4]], FhgrL[[1]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 6, 0.0175, 0.015, "id_short", "id_long", "MtrL", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr1", 0)
    fcat_bal[4]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Sh[[4]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    FhgrL[[1]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Ln[[4]] <- subset(tmp_AhdgGr, UsgMatur >= 36                     )
    AsecBnd_Ln[[3]] <- subset(tmp_AhdgGr, UsgMatur >= 24  & UsgMatur < 36    )
    FsecBnd_Ln[[1]] <- subset(FhgrL[[1]], CorpMatur <  12                   )
    FsecBnd_Ln[[2]] <- subset(FhgrL[[1]], CorpMatur >= 12  & CorpMatur < 24 )
    FsecBnd_Ln[[3]] <- subset(FhgrL[[1]], CorpMatur >= 24  & CorpMatur < 36 )
    FsecBnd_Ln[[4]] <- subset(FhgrL[[1]], CorpMatur >= 36                   )
    CorpHedgegrS[[1]] <- FsecBnd_Sh[[1]]
    for (i in 2:4){CorpHedgegrS[[1]] <- rbind(CorpHedgegrS[[1]], FsecBnd_Sh[[i]])}
    remove(xyz_3_0)
  }
  
  if (fcat_bal[3] >= ContrAmt & nrow(FsecBnd_Ln[[3]]) > 0){
    if (fcat_bal[4] <= -ContrAmt & nrow(FhgrL[[1]]) > 0 & nrow(CorpHedgegrS[[1]]) > 0){
      transfer_potential <- min(abs(fcat_bal[3]), abs(fcat_bal[4]))
      if (prnt_f == "Y"){print("Function f_movement_without_hedge run for net long balance group 3")}
      xyz_3 <- f_movement_without_hedge(transfer_potential, FhgrL[[1]], CorpHedgegrS[[1]], "id_long", "id_short", "MtrSh", tbl_FFPairs, 6, 0.0175, 24, 36, 60, ContrAmt, "ff_matching_pair_gr1")
      FhgrL[[1]]  <- xyz_3[["CorpHedgegrIn"]]
      CorpHedgegrS[[1]]  <- xyz_3[["CorpHedgegrOut"]]
      tbl_FFPairs <- xyz_3[["pairtbl"]]
      PairId <- xyz_3[["PairId"]]
      FsecBnd_Sh[[3]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 24   & CorpMatur < 36 )
      FsecBnd_Sh[[4]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 36   & CorpMatur < 60 )
      FsecBnd_Ln[[3]] <- subset(FhgrL[[1]], CorpMatur >= 24   & CorpMatur < 36 )
      FsecBnd_Ln[[4]] <- subset(FhgrL[[1]], CorpMatur >= 36   & CorpMatur < 60 )
      remove(xyz_3)
    }
    tmp_AhdgGr <- AsecBnd_Sh[[2]]
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Sh[[3]])
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Sh[[4]])
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net long balance group 3")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[3], FsecBnd_Ln[[3]], CorpHedgegrS[[1]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 6, 0.0175, 0.015, "id_long", "id_short", "MtrSh", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr1", 1)
    fcat_bal[3]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Ln[[3]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    CorpHedgegrS[[1]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Sh[[4]] <- subset(tmp_AhdgGr, UsgMatur >= 36                  )
    AsecBnd_Sh[[3]] <- subset(tmp_AhdgGr, UsgMatur >= 24  & UsgMatur < 36 )
    AsecBnd_Sh[[2]] <- subset(tmp_AhdgGr, UsgMatur >= 12  & UsgMatur < 24 )
    FsecBnd_Sh[[1]] <- subset(CorpHedgegrS[[1]], CorpMatur <  12               )
    FsecBnd_Sh[[2]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 12  & CorpMatur < 24 )
    FsecBnd_Sh[[3]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 24  & CorpMatur < 36 )
    FsecBnd_Sh[[4]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 36                   )
    FhgrL[[1]] <- FsecBnd_Ln[[1]]
    for (i in 2:4){FhgrL[[1]] <- rbind(FhgrL[[1]], FsecBnd_Ln[[i]])}
    remove(xyz_3_0)
  } else if (fcat_bal[3] <= -ContrAmt & nrow(FsecBnd_Sh[[3]]) > 0){
    if (fcat_bal[4] >= ContrAmt & nrow(CorpHedgegrS[[1]]) > 0 & nrow(FhgrL[[1]]) > 0){
      transfer_potential <- min(abs(fcat_bal[3]), abs(fcat_bal[4]))
      if (prnt_f =="Y"){print("Function f_movement_without_hedge run for net short balance group 3")}
      xyz_3 <- f_movement_without_hedge(transfer_potential, CorpHedgegrS[[1]], FhgrL[[1]], "id_short", "id_long", "MtrL", tbl_FFPairs, 6, 0.0175, 24, 36, 60, ContrAmt, "ff_matching_pair_gr1")
      CorpHedgegrS[[1]]  <- xyz_3[["CorpHedgegrIn"]]
      FhgrL[[1]]  <- xyz_3[["CorpHedgegrOut"]]
      tbl_FFPairs <- xyz_3[["pairtbl"]]
      PairId <- xyz_3[["PairId"]]  
      FsecBnd_Sh[[3]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 24   & CorpMatur < 36 )
      FsecBnd_Sh[[4]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 36   & CorpMatur < 60 )
      FsecBnd_Ln[[3]] <- subset(FhgrL[[1]], CorpMatur >= 24   & CorpMatur < 36 )
      FsecBnd_Ln[[4]] <- subset(FhgrL[[1]], CorpMatur >= 36   & CorpMatur < 60 )
      remove(xyz_3)
    }
    tmp_AhdgGr <- AsecBnd_Ln[[2]]
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Ln[[3]])
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Ln[[4]])
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net short balance group 3")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[3], FsecBnd_Sh[[3]], FhgrL[[1]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 6, 0.0175, 0.015, "id_short", "id_long", "MtrL", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr1", 0)
    fcat_bal[3]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Sh[[3]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    FhgrL[[1]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]  
    AsecBnd_Ln[[4]] <- subset(tmp_AhdgGr, UsgMatur >= 36                  )
    AsecBnd_Ln[[3]] <- subset(tmp_AhdgGr, UsgMatur >= 24  & UsgMatur < 36 )
    AsecBnd_Ln[[2]] <- subset(tmp_AhdgGr, UsgMatur >= 12  & UsgMatur < 24 )
    CorpHedgegrS[[1]] <- FsecBnd_Sh[[1]]
    for (i in 2:4){CorpHedgegrS[[1]] <- rbind(CorpHedgegrS[[1]], FsecBnd_Sh[[i]])}
    remove(xyz_3_0)
  }
  
  if (fcat_bal[2] >= ContrAmt & nrow(FsecBnd_Ln[[2]]) > 0){
    if (fcat_bal[3] <= -ContrAmt & nrow(FhgrL[[1]]) > 0 & nrow(CorpHedgegrS[[1]]) > 0){
      transfer_potential <- min(abs(fcat_bal[2]), abs(fcat_bal[3]))
      if (prnt_f == "Y"){print("Function f_movement_without_hedge run for net long balance group 2")}
      xyz_3 <- f_movement_without_hedge(transfer_potential, FhgrL[[1]], CorpHedgegrS[[1]], "id_long", "id_short", "MtrSh", tbl_FFPairs, 6, 0.0175, 12, 24, 36, ContrAmt, "ff_matching_pair_gr1")
      FhgrL[[1]]  <- xyz_3[["CorpHedgegrIn"]]
      CorpHedgegrS[[1]]  <- xyz_3[["CorpHedgegrOut"]]
      tbl_FFPairs <- xyz_3[["pairtbl"]]
      PairId <- xyz_3[["PairId"]]
      FsecBnd_Sh[[2]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 12 & CorpMatur < 24 )
      FsecBnd_Sh[[3]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 24 & CorpMatur < 36 )
      FsecBnd_Ln[[2]] <- subset(FhgrL[[1]], CorpMatur >= 12 & CorpMatur < 24 )
      FsecBnd_Ln[[3]] <- subset(FhgrL[[1]], CorpMatur >= 24 & CorpMatur < 36 )
      remove(xyz_3)
    }
    tmp_AhdgGr <- AsecBnd_Sh[[12]]
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Sh[[13]])
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Sh[[2]])
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Sh[[3]])
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net long balance group 2")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[2], FsecBnd_Ln[[2]], CorpHedgegrS[[1]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 6, 0.0175, 0.015, "id_long", "id_short", "MtrSh", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr1", 1)
    fcat_bal[2]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Ln[[2]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    CorpHedgegrS[[1]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]
    AsecBnd_Sh[[3]]   <- subset(tmp_AhdgGr, UsgMatur >= 24  & UsgMatur < 36 )
    AsecBnd_Sh[[2]]   <- subset(tmp_AhdgGr, UsgMatur >= 12  & UsgMatur < 24 )
    AsecBnd_Sh[[13]] <- subset(tmp_AhdgGr, UsgMatur >=  9  & UsgMatur < 12 )
    AsecBnd_Sh[[12]] <- subset(tmp_AhdgGr, UsgMatur >=  6  & UsgMatur <  9 )
    FsecBnd_Sh[[1]] <- subset(CorpHedgegrS[[1]], CorpMatur <  12                   )
    FsecBnd_Sh[[2]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 12  & CorpMatur < 24 )
    FsecBnd_Sh[[3]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 24  & CorpMatur < 36 )
    FsecBnd_Sh[[4]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 36                   )  
    FhgrL[[1]] <- FsecBnd_Ln[[1]]
    for (i in 2:4){FhgrL[[1]] <- rbind(FhgrL[[1]], FsecBnd_Ln[[i]])}
    remove(xyz_3_0)
  } else if (fcat_bal[2] <= -ContrAmt & nrow(FsecBnd_Sh[[2]]) > 0){
    if (fcat_bal[3] >= ContrAmt & nrow(CorpHedgegrS[[1]]) > 0 & nrow(FhgrL[[1]]) > 0){
      transfer_potential <- min(abs(fcat_bal[2]), abs(fcat_bal[3]))
      if (prnt_f == "Y"){print("Function f_movement_without_hedge run for net short balance group 2")}
      xyz_3 <- f_movement_without_hedge(transfer_potential, CorpHedgegrS[[1]], FhgrL[[1]], "id_short", "id_long", "MtrL", tbl_FFPairs, 6, 0.0175, 12, 24, 36, ContrAmt, "ff_matching_pair_gr1")
      CorpHedgegrS[[1]]  <- xyz_3[["CorpHedgegrIn"]]
      FhgrL[[1]]  <- xyz_3[["CorpHedgegrOut"]]
      tbl_FFPairs <- xyz_3[["pairtbl"]]
      PairId <- xyz_3[["PairId"]]
      FsecBnd_Sh[[2]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 12   & CorpMatur < 24 )
      FsecBnd_Sh[[3]] <- subset(CorpHedgegrS[[1]], CorpMatur >= 24   & CorpMatur < 36 )
      FsecBnd_Ln[[2]] <- subset(FhgrL[[1]], CorpMatur >= 12   & CorpMatur < 24 )
      FsecBnd_Ln[[3]] <- subset(FhgrL[[1]], CorpMatur >= 24   & CorpMatur < 36 )
      remove(xyz_3)
    }
    tmp_AhdgGr <- AsecBnd_Ln[[12]]
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Ln[[13]])
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Ln[[2]])
    tmp_AhdgGr <- rbind(tmp_AhdgGr, AsecBnd_Ln[[3]])
    tmp_AhdgGr <- tmp_AhdgGr[order(tmp_AhdgGr$UsgMatur, decreasing = FALSE),]
    if (prnt_f == "Y"){print("Function fa_hedging_3_0 run for net short balance group 2")}
    xyz_3_0 <- fa_hedging_3_0(fcat_bal[2], FsecBnd_Sh[[2]], FhgrL[[1]], tmp_AhdgGr, tbl_FFPairs, tbl_FAPairs, v_Exit0, 6, 0.0175, 0.015, "id_short", "id_long", "MtrL", FAPairId, PairId, ContrAmt, "ff_matching_pair_gr1", 0)
    fcat_bal[2]  <- xyz_3_0[["y_FbandBal"]]
    tmp_AhdgGr        <- xyz_3_0[["y_govsec_band"]]
    FsecBnd_Sh[[2]] <- xyz_3_0[["v_CorpHedgegrIn"]]
    FhgrL[[1]] <- xyz_3_0[["v_CorpHedgegrOut"]]
    tbl_FFPairs            <- xyz_3_0[["y_pairtbl"]]
    tbl_FAPairs         <- xyz_3_0[["y_pairtbl_fa"]]
    FAPairId <- xyz_3_0[["FAPairId"]]
    PairId    <- xyz_3_0[["PairId"]]  
    AsecBnd_Ln[[3]]   <- subset(tmp_AhdgGr, UsgMatur >= 24  & UsgMatur < 36  )
    AsecBnd_Ln[[2]]   <- subset(tmp_AhdgGr, UsgMatur >= 12  & UsgMatur < 24  )
    AsecBnd_Ln[[13]] <- subset(tmp_AhdgGr, UsgMatur >=  9  & UsgMatur < 12     )
    AsecBnd_Ln[[12]] <- subset(tmp_AhdgGr, UsgMatur >=  6  & UsgMatur <  9     )
    FsecBnd_Ln[[1]] <- subset(FhgrL[[1]], CorpMatur <  12                   )
    FsecBnd_Ln[[2]] <- subset(FhgrL[[1]], CorpMatur >= 12  & CorpMatur < 24 )
    FsecBnd_Ln[[3]] <- subset(FhgrL[[1]], CorpMatur >= 24  & CorpMatur < 36 )
    FsecBnd_Ln[[4]] <- subset(FhgrL[[1]], CorpMatur >= 36                   )  
    CorpHedgegrS[[1]] <- FsecBnd_Sh[[1]]
    for (i in 2:4){CorpHedgegrS[[1]] <- rbind(CorpHedgegrS[[1]], FsecBnd_Sh[[i]])}
    remove(xyz_3_0)
  }
}

for (i in 7:4){
  CorpHedgegrS[[(i-3)]] <- FsecBnd_Sh[[i]]
  FhgrL[[(i-3)]] <- FsecBnd_Ln[[i]]
}
for (i in 8:9){
  CorpHedgegrS[[4]] <- rbind(CorpHedgegrS[[4]], FsecBnd_Sh[[i]])
  FhgrL[[4]] <- rbind(FhgrL[[4]], FsecBnd_Ln[[i]])  
}
for (i in 3:1){
  CorpHedgegrS[[1]] <- rbind(CorpHedgegrS[[1]], FsecBnd_Sh[[i]])
  FhgrL[[1]] <- rbind(FhgrL[[1]], FsecBnd_Ln[[i]])
}

CorpHedgegrS[[4]] <- CorpHedgegrS[[4]][order(CorpHedgegrS[[4]]$CorpMatur, decreasing = FALSE),] 
FhgrL[[4]] <- FhgrL[[4]][order(FhgrL[[4]]$CorpMatur, decreasing = FALSE),]
CorpHedgegrS[[1]] <- CorpHedgegrS[[1]][order(CorpHedgegrS[[1]]$CorpMatur, decreasing = FALSE),]
FhgrL[[1]] <- FhgrL[[1]][order(FhgrL[[1]]$CorpMatur, decreasing = FALSE),]
remove(CorpHedgegrS, FhgrL)
#Extra Step: A-A intercategory netting 1st round
tbl_AAPairs <- matrix(c(0, 0, 0, 0, 0, 0), nrow=1, ncol=6)
colnames(tbl_AAPairs) <- c("index", "id_long", "id_short", "MatchAmt", "MtrL", "MtrSh")
rownames(tbl_AAPairs) <- "dummy"
#Initialize haircuts for A bands
AbndHct <- rep(0, times = 13)
for (i in 13:2){
  A_SubgrBal[i] <- as.double(sum(AsecBnd_Ln[[i]]$mv_gov_rem) - sum(AsecBnd_Sh[[i]]$mv_gov_rem))
  AbndHct[i]    <- A_SubgrBal[i] * AratesAdj[i]
}
#Initialize A Category Plain side haircuts
ACatNetL <- c(0,0,0,0)
ACatNetS <- c(0,0,0,0)
for (i in 4:1){
  AbndHctTmp   <- AbndHct[CatListA[[i]]]
  ACatNetL[i]  <- sum(AbndHctTmp[AbndHctTmp >= 0])
  ACatNetS[i]  <- sum(AbndHctTmp[AbndHctTmp <  0])
}
a_categ_haircut <- c(0, 0, 0, 0)

aa_netting <- function(GrNum, x_AsecBnd_Ln, x_AsecBnd_Sh, tbl_AAPairs, AbndHct, AratesAdj, A_SubgrBal, ACatNetL, ACatNetS){
  MaturRef1   <- matrix(c(9, 24, 96, 15, 48, 144, 3, 12, 24), nrow=3, ncol=3, byrow=TRUE)
  SubgrRef    <- c(13, 3, 5, 2, 4, 6)
  
  x_Hsubgr <- AbndHct[SubgrRef[GrNum+3]]
  x_Hgr    <- ACatNetL[GrNum+1] + ACatNetS[GrNum+1]
  y_Lgr    <- AbndHct[SubgrRef[GrNum]]
  y_Lsubgr <- ACatNetL[GrNum] + ACatNetS[GrNum]
  var_exit5 <- 0
  var_u5 <- SubgrRef[GrNum]
  var_v5 <- SubgrRef[GrNum+3]
  #Assignment of correct subcategories and amount for netting
  if (x_Hsubgr < 0 & x_Hgr < 0){
    BandMhigh <- x_AsecBnd_Sh[[var_v5]]
    BandMlow  <- x_AsecBnd_Ln[[var_u5]]
    transfer_amt <- (abs(x_Hsubgr) / AratesAdj[var_v5])
    lwr_cat_side <- 0
    if(nrow(x_AsecBnd_Sh[[var_v5]]) < 1 || nrow(x_AsecBnd_Ln[[var_u5]]) < 1){var_exit5 <- 1}
  } else if (x_Hsubgr >= 0 & x_Hgr >= 0){
    BandMhigh <- x_AsecBnd_Ln[[var_v5]]
    BandMlow  <- x_AsecBnd_Sh[[var_u5]]
    transfer_amt <- (abs(x_Hsubgr) / AratesAdj[SubgrRef[var_v5]])
    lwr_cat_side <- 1
    if(nrow(x_AsecBnd_Ln[[var_v5]]) < 1 || nrow(x_AsecBnd_Sh[[var_u5]]) < 1){var_exit5 <- 1}
  } else {
    if (x_Hgr >= 0 & y_Lgr < 0){
      BandMhigh <- x_AsecBnd_Ln[[var_v5]]
      BandMlow  <- x_AsecBnd_Sh[[var_u5]]
      transfer_amt <- min((abs(y_Lgr) / AratesAdj[var_u5]), (abs(x_Hsubgr) / AratesAdj[var_v5]))
      lwr_cat_side <- 1
      if(nrow(x_AsecBnd_Ln[[var_v5]]) < 1 || nrow(x_AsecBnd_Sh[[var_u5]]) < 1){var_exit5 <- 1}
    } else if (x_Hgr >= 0 & y_Lgr >= 0 & y_Lsubgr < 0){
      BandMhigh <- x_AsecBnd_Ln[[var_v5]]
      BandMlow  <- x_AsecBnd_Sh[[var_u5]]
      transfer_amt	<- min((abs(y_Lsubgr) / AratesAdj[var_u5]), (abs(x_Hsubgr) / AratesAdj[var_v5]))
      lwr_cat_side <- 1
      if(nrow(x_AsecBnd_Ln[[var_v5]]) < 1 || nrow(x_AsecBnd_Sh[[var_u5]]) < 1){var_exit5 <- 1}
    } else if (x_Hgr < 0 & y_Lgr < 0 & y_Lsubgr < 0){
      BandMhigh <- x_AsecBnd_Ln[[var_v5]]
      BandMlow  <- x_AsecBnd_Sh[[var_u5]]
      transfer_amt <- min((abs(x_Hsubgr) / AratesAdj[var_v5]), (abs(y_Lgr) / AratesAdj[var_u5]), (abs(y_Lsubgr) / AratesAdj[var_u5]))
      lwr_cat_side <- 1
      if(nrow(x_AsecBnd_Ln[[var_v5]]) < 1 || nrow(x_AsecBnd_Sh[[var_u5]]) < 1){var_exit5 <- 1}
    } else {
      BandMhigh <- x_AsecBnd_Sh[[var_v5]]
      BandMlow  <- x_AsecBnd_Ln[[var_u5]]
      transfer_amt	<- min((abs(x_Hgr) / AratesAdj[var_v5]), (abs(x_Hsubgr) / AratesAdj[var_v5]), (abs(y_Lgr) / AratesAdj[var_u5]), (abs(y_Lsubgr) / AratesAdj[var_u5]))
      lwr_cat_side <- 0
      if(nrow(x_AsecBnd_Sh[[var_v5]]) < 1 || nrow(x_AsecBnd_Ln[[var_u5]]) < 1){var_exit5 <- 1}
    }
  }
  LowMtrLim            <- MaturRef1[1, GrNum]
  HighMtrLim           <- MaturRef1[2, GrNum]
  inter_cat_maturity   <- MaturRef1[3, GrNum]
  var_i1 <- 1
  var_h1 <- 1
  if (var_exit5 == 0){
    while (var_h1 < nrow(BandMlow) & BandMlow[var_h1, "UsgMatur"] < LowMtrLim){var_h1 <- (var_h1 + 1)}
    cumulative_tr_bal <- 0
    while ((var_h1 <= nrow(BandMlow)) & (BandMhigh[var_i1, "UsgMatur"] < HighMtrLim) & ((transfer_amt - cumulative_tr_bal) >= 0.01)){
      if ((BandMhigh[var_i1, "UsgMatur"] - BandMlow[var_h1, "UsgMatur"]) <= inter_cat_maturity){
        if (BandMhigh[var_i1, "mv_gov_rem"] < 0.01 & BandMlow[var_h1, "mv_gov_rem"] < 0.01){
          var_i1 <- (var_i1 + 1); var_h1 <- (var_h1 + 1); 
          if (var_i1 > nrow(BandMhigh) || var_h1 > nrow(BandMlow)){break}
        } else if (BandMhigh[var_i1, "mv_gov_rem"] <  0.01 & BandMlow[var_h1, "mv_gov_rem"] >= 0.01){
          var_i1 <- (var_i1 + 1); 
          if (var_i1 > nrow(BandMhigh)){break}
        } else if (BandMhigh[var_i1, "mv_gov_rem"] >= 0.01 & BandMlow[var_h1, "mv_gov_rem"] <  0.01){
          var_h1 <- (var_h1 + 1); 
          if (var_h1 > nrow(BandMlow)){break}
        } else {
          transf <- min(BandMhigh[var_i1, "mv_gov_rem"], BandMlow[var_h1, "mv_gov_rem"], (transfer_amt - cumulative_tr_bal))
          cumulative_tr_bal <- (cumulative_tr_bal + transf)  
          BandMhigh[var_i1, "mv_gov_rem"] <- (BandMhigh[var_i1, "mv_gov_rem"] - transf)
          BandMlow[var_h1, "mv_gov_rem"] <- (BandMlow[var_h1, "mv_gov_rem"] - transf)
          if (lwr_cat_side == 1){
            matching_pair_aa <- c(nrow(tbl_AAPairs), BandMhigh[var_i1, "unique_id_gov"], BandMlow[var_h1, "unique_id_gov"], transf, BandMhigh[var_i1, "UsgMatur"], BandMlow[var_h1, "UsgMatur"]); 
          } else {
            matching_pair_aa <- c(nrow(tbl_AAPairs), BandMlow[var_h1, "unique_id_gov"], BandMhigh[var_i1, "unique_id_gov"], transf, BandMlow[var_h1, "UsgMatur"], BandMhigh[var_i1, "UsgMatur"])
          }
          tbl_AAPairs <- rbind(tbl_AAPairs, matching_pair_aa)
        }
      } else {
        var_h1 <- (var_h1 + 1)
      }
    }
  }
  x_SecALong  <- x_AsecBnd_Ln
  x_SecAShort <- x_AsecBnd_Sh
  if (lwr_cat_side == 0){
    x_SecALong[[var_u5]]  <- BandMlow
    x_SecAShort[[var_v5]] <- BandMhigh
  } else {
    x_SecALong[[var_v5]]  <- BandMhigh
    x_SecAShort[[var_u5]] <- BandMlow
  }
  for (i in 13:2){
    A_SubgrBal[i] <- as.double(sum(x_AsecBnd_Ln[[i]]$mv_gov_rem) - sum(x_AsecBnd_Sh[[i]]$mv_gov_rem))
    AbndHct[i]    <- A_SubgrBal[i] * AratesAdj[i]
  }
  xyz4 <- list("x_SecALong" = x_SecALong, "x_SecAShort" = x_SecAShort,  "v_tbl_AAPairs" = tbl_AAPairs, "v_A_SubgrBal" = A_SubgrBal, "v_AbndHct" = AbndHct)
  return(xyz4)
  remove(BandMhigh, BandMlow, tbl_AAPairs, x_SecALong, x_SecAShort, A_SubgrBal, AbndHct)
}

if (distrib == 1 | distrib == 3){
  for (i in 3:1){
    if (prnt_f == "Y"){paste("Function aa_1234 run for group ",i,sep="")}
    aa_1234 <- aa_netting(i, AsecBnd_Ln, AsecBnd_Sh, tbl_AAPairs, AbndHct, AratesAdj, A_SubgrBal, ACatNetL, ACatNetS)
    AsecBnd_Ln  <- aa_1234[["x_SecALong"]]
    AsecBnd_Sh  <- aa_1234[["x_SecAShort"]]
    tbl_AAPairs <- aa_1234[["v_tbl_AAPairs"]]
    A_SubgrBal  <- aa_1234[["v_A_SubgrBal"]]
    AbndHct     <- aa_1234[["v_AbndHct"]]
  }
}
#Step 3.2 Section A - Data Setup
A_balance_group <- c(0, 0, 0, 0)
for (i in 1:4){A_balance_group[i] <- (ACatNetL[i] + ACatNetS[i])}
UsgNetgrL <- list()
UsgNetgrS <- list()
vec_z32a <- c(4, 4, 4, 3, 2)
vec_z32b <- c(7, 8, 9, 5, 3)
for (i in 3:1){
  UsgNetgrS[[1+i]] <- AsecBnd_Sh[[2*i]]
  UsgNetgrL[[1+i]] <- AsecBnd_Ln[[2*i]]
}
for (i in 1:5){
  UsgNetgrS[[vec_z32a[i]]] <- rbind(UsgNetgrS[[vec_z32a[i]]], AsecBnd_Sh[[vec_z32b[i]]])
  UsgNetgrL[[vec_z32a[i]]] <- rbind(UsgNetgrL[[vec_z32a[i]]], AsecBnd_Ln[[vec_z32b[i]]])
}
UsgNetgrS[[1]] <- AsecBnd_Sh[[10]]
UsgNetgrL[[1]] <- AsecBnd_Ln[[10]]
for (i in 2:4){
  UsgNetgrS[[1]] <- rbind(UsgNetgrS[[1]], AsecBnd_Sh[[i+9]])
  UsgNetgrL[[1]] <- rbind(UsgNetgrL[[1]], AsecBnd_Ln[[i+9]])
}
for (i in 1:4){
  UsgNetgrS[[i]] <- UsgNetgrS[[i]][order(UsgNetgrS[[i]]$UsgMatur, decreasing = FALSE),]
  UsgNetgrL[[i]] <- UsgNetgrL[[i]][order(UsgNetgrL[[i]]$UsgMatur, decreasing = FALSE),]
}

#Update balance of F categories
for (i in 9:1){
  if (nrow(FsecBnd_Ln[[i]]) >=1){
    fcat_bal_l[i] <- sum(FsecBnd_Ln[[i]]["mv_rem"])
  } else {
    fcat_bal_l[i] <- 0 
  }
  if (nrow(FsecBnd_Sh[[i]]) >=1){
    fcat_bal_s[i] <- sum(FsecBnd_Sh[[i]]["mv_rem"])
  } else {
    fcat_bal_s[i] <- 0
  }
}
for (i in 9:1){fcat_bal[i] <- ( fcat_bal_l[i] - fcat_bal_s[i] )}
#Step 3.2 Section B: Function set-up
fa_UNhedging_step3_2 <- function(net_total_group, corpsec_hedgegr_MIN,govsec_band, tbl_FAPairs,f_category, a_category, fside_req, AratesAdj){
  count <- nrow(tbl_FAPairs)
  counter <- 2
  
  while(counter <= count && abs(net_total_group) >= 0.01 ){
    if (tbl_FAPairs[counter,"f_sec_side"] == 1){
      f_matur <- tbl_FAPairs[counter,"MtrL"]
      a_matur <- tbl_FAPairs[counter,"MtrSh"]
      f_id <- tbl_FAPairs[counter,"id_long"]
      a_id <- tbl_FAPairs[counter,"id_short"]
    } else if (tbl_FAPairs[counter,"f_sec_side"] == 0){
      f_matur <- tbl_FAPairs[counter,"MtrSh"]
      a_matur <- tbl_FAPairs[counter,"MtrL"]
      f_id <- tbl_FAPairs[counter,"id_short"]
      a_id <- tbl_FAPairs[counter,"id_long"]
    } 
    
    match1 <- FALSE
    if (a_category > 0 & a_category < 5 & tbl_FAPairs[counter,"f_sec_side"] == fside_req){
      if ((a_category == 1)    & a_matur < (0.25*12)               )   {
        match1 <- TRUE  ; current_haircut <- AratesAdj[10];  current_category <- 1.00 ; 
      }
      if ((a_category == 1)    & a_matur >= (0.25*12)  & a_matur < (0.5*12)  )   {
        match1 <- TRUE  ; current_haircut <- AratesAdj[11];  current_category <- 1.25 ; 
      }
      if ((a_category == 1)    & a_matur >= (0.5*12) & a_matur < (0.75*12)   )   {
        match1 <- TRUE  ; current_haircut <- AratesAdj[12];  current_category <- 1.50 ; 
      }
      if ((a_category == 1)    & a_matur >= (0.75*12)  & a_matur < (01*12)   )   {
        match1 <- TRUE  ; current_haircut <- AratesAdj[13];  current_category <- 1.75 ; 
      }
      if ((a_category == 2)    & a_matur >= (01*12)  & a_matur < (02*12)   )   {
        match1 <- TRUE  ; current_haircut <- AratesAdj[2];  current_category <- 2    ; 
      }
      if ((a_category == 2)    & a_matur >= (02*12)  & a_matur < (03*12)   )   {
        match1 <- TRUE  ; current_haircut <- AratesAdj[3];  current_category <- 3    ; 
      }
      if ((a_category == 3)    & a_matur >= (03*12)  & a_matur < (05*12)   ) {
        match1 <- TRUE  ; current_haircut <- AratesAdj[4];  current_category <- 4    ; 
      }
      if ((a_category == 3)    & a_matur >= (05*12)  & a_matur < (10*12)   ) {
        match1 <- TRUE  ; current_haircut <- AratesAdj[5];  current_category <- 5    ; 
      }
      if ((a_category == 4)    & a_matur >= (10*12)  & a_matur < (15*12)   ) {
        match1 <- TRUE  ; current_haircut <- AratesAdj[6];  current_category <- 6    ; 
      }
      if ((a_category == 4)    & a_matur >= (15*12)  & a_matur < (20*12)   )   {
        match1 <- TRUE  ; current_haircut <- AratesAdj[7];  current_category <- 7    ;
      }
      if ((a_category == 4)    & a_matur >= (20*12)  & a_matur < (25*12)   )   {
        match1 <- TRUE  ; current_haircut <- AratesAdj[8];  current_category <- 8    ;
      }
      if ((a_category == 4)    & a_matur >= (25*12)              ) {
        match1 <- TRUE  ; current_haircut <- AratesAdj[9];  current_category <- 9    ;
      }
    }
    match2 <- FALSE
    if (match1){
      if (f_category == 1.00 & f_matur <  (1*12)               )   {
        match2 <- TRUE ;
      }
      if (f_category == 2    & f_matur >= (01*12)  & f_matur < (02*12)   )   {
        match2 <- TRUE ;
      }
      if (f_category == 3    & f_matur >= (02*12)  & f_matur < (03*12)   )   {
        match2 <- TRUE ; 
      }
      if (f_category == 4    & f_matur >= (03*12)  & f_matur < (05*12)   ) {
        match2 <- TRUE ; 
      }
      if (f_category == 5    & f_matur >= (05*12)  & f_matur < (10*12)   ) {
        match2 <- TRUE ;
      }
      if (f_category == 6    & f_matur >= (10*12)  & f_matur < (15*12)   ) {
        match2 <- TRUE ; 
      }
      if (f_category == 7    & f_matur >= (15*12)  & f_matur < (20*12)   )   {
        match2 <- TRUE ;
      }
      if (f_category == 8    & f_matur >= (20*12)  & f_matur < (25*12)   )   {
        match2 <- TRUE ;
      }
      if (f_category == 9    & f_matur >= (25*12)              ) {
        match2 <- TRUE ;
      }
    }
    if (match2){
      val_orig <- tbl_FAPairs[counter,"MatchAmt"]
      val <- val_orig * current_haircut
      if (round((abs(net_total_group) - val), digits=2) >= 0){
        unwound_amt <- val_orig
      } else {
        val <- net_total_group
        unwound_amt <- (val / current_haircut)
      }
      corpsec_hedgegr_MIN[which(corpsec_hedgegr_MIN$unique_id == f_id),]["mv_h2fa"] <- corpsec_hedgegr_MIN[which(corpsec_hedgegr_MIN$unique_id == f_id),]["mv_h2fa"] - unwound_amt
      corpsec_hedgegr_MIN[which(corpsec_hedgegr_MIN$unique_id == f_id),]["mv_rem"]  <- corpsec_hedgegr_MIN[which(corpsec_hedgegr_MIN$unique_id == f_id),]["mv_rem"]  + unwound_amt
      govsec_band[which(govsec_band$unique_id_gov == a_id),]["mv_gov_rem"] <- govsec_band[which(govsec_band$unique_id_gov == a_id),]["mv_gov_rem"] + unwound_amt
      tbl_FAPairs[counter,"MatchAmt"] <- tbl_FAPairs[counter,"MatchAmt"] - unwound_amt
      net_total_group <- net_total_group - val
    }
    counter <- counter + 1
  }
  xyz3 <- list("net_total_group" = net_total_group,"corpsec_hedgegr_MIN" = corpsec_hedgegr_MIN, "govsec_band" = govsec_band, "tbl_FAPairs" = tbl_FAPairs)
  remove(net_total_group, corpsec_hedgegr_MIN, govsec_band, tbl_FAPairs)
  return (xyz3);
}

#Step 3.2 Section C; Function run
print("Step 3.2 Function Run");
if (distrib >= 3){
  if (abs(ACatNetS[4]) > ACatNetL[4]){
    for (i in 9:6){
      if (fcat_bal[i] > 0.01 & nrow(FsecBnd_Sh[[i]]) >=1 & nrow(UsgNetgrL[[4]]) >=1){
        if (prnt_f == "Y"){paste("Function fa_UNhedging_step3_2 version 1 run for group ",i,sep="")}
        xyz3_2 <- fa_UNhedging_step3_2(A_balance_group[4], FsecBnd_Sh[[i]], UsgNetgrL[[4]], tbl_FAPairs, i, 4, 0, AratesAdj)
        A_balance_group[4] <-xyz3_2[["net_total_group"]]
        UsgNetgrL[[4]] <- xyz3_2[["govsec_band"]]
        FsecBnd_Sh[[i]] <-xyz3_2[["corpsec_hedgegr_MIN"]]
        tbl_FAPairs <- xyz3_2[["tbl_FAPairs"]]
        paste("RunLong",i,sep="")
        remove(xyz3_2)
      }
    }
  } else if (abs(ACatNetS[4]) < ACatNetL[4]){
    for (i in 9:6){
      if (fcat_bal[i] <= -0.01 & nrow(FsecBnd_Ln[[i]]) >=1 & nrow(UsgNetgrS[[4]]) >=1){
        if (prnt_f == "Y"){paste("Function fa_UNhedging_step3_2 version 2 run for group ",i,sep="")}
        xyz3_2 <- fa_UNhedging_step3_2(A_balance_group[4], FsecBnd_Ln[[i]], UsgNetgrS[[4]], tbl_FAPairs, i, 4, 1, AratesAdj)
        A_balance_group[4] <-xyz3_2[["net_total_group"]]
        UsgNetgrS[[4]] <- xyz3_2[["govsec_band"]]
        FsecBnd_Ln[[i]] <-xyz3_2[["corpsec_hedgegr_MIN"]]
        tbl_FAPairs <- xyz3_2[["tbl_FAPairs"]]
        paste("RunShort",i,sep="")
        remove(xyz3_2)
      }
    }
  }
  if (abs(ACatNetS[3]) < ACatNetL[3]){
    for (i in 5:4){
      if (fcat_bal[i] < -0.01 & nrow(FsecBnd_Ln[[i]]) >=1 & nrow(UsgNetgrS[[3]]) >=1){
        if (prnt_f == "Y"){paste("Function fa_UNhedging_step3_2 version 1 run for group ",i,sep="")}
        xyz3_2 <- fa_UNhedging_step3_2(A_balance_group[3], FsecBnd_Ln[[i]],UsgNetgrS[[3]], tbl_FAPairs, i, 3, 1, AratesAdj)
        A_balance_group[3]<-xyz3_2[["net_total_group"]]
        UsgNetgrS[[3]] <- xyz3_2[["govsec_band"]]
        FsecBnd_Ln[[i]] <-xyz3_2[["corpsec_hedgegr_MIN"]]
        tbl_FAPairs <- xyz3_2[["tbl_FAPairs"]]
        remove(xyz3_2)
      }
    }
  } else if(abs(ACatNetS[3]) > ACatNetL[3]){
    for (i in 5:4){
      if (fcat_bal[i] > 0.01 & nrow(FsecBnd_Sh[[i]]) >=1 & nrow(UsgNetgrL[[3]]) >=1){
        if (prnt_f == "Y"){paste("Function fa_UNhedging_step3_2 version 2 run for group ",i,sep="")}
        xyz3_2 <- fa_UNhedging_step3_2(A_balance_group[3], FsecBnd_Sh[[i]],UsgNetgrL[[3]], tbl_FAPairs, i, 3, 0, AratesAdj)
        A_balance_group[3]    <- xyz3_2[["net_total_group"]]
        UsgNetgrL[[3]] <- xyz3_2[["govsec_band"]]
        FsecBnd_Sh[[i]] <- xyz3_2[["corpsec_hedgegr_MIN"]]
        tbl_FAPairs      <- xyz3_2[["tbl_FAPairs"]]
        remove(xyz3_2)
      }
    }
  }
  if (abs(ACatNetS[2]) < ACatNetL[2]){
    for (i in 4:1){
      if (fcat_bal[i] < -0.01 & nrow(FsecBnd_Ln[[i]]) >=1 & nrow(UsgNetgrS[[2]]) >=1){
        if (prnt_f == "Y"){paste("Function fa_UNhedging_step3_2 version 1 run for group ",i,sep="")}
        xyz3_2 <- fa_UNhedging_step3_2(A_balance_group[2], FsecBnd_Ln[[i]], UsgNetgrS[[2]], tbl_FAPairs, i, 2, 1, AratesAdj)
        A_balance_group[2] <- xyz3_2[["net_total_group"]]
        UsgNetgrS[[2]] <- xyz3_2[["govsec_band"]]
        FsecBnd_Ln[[i]] <-xyz3_2[["corpsec_hedgegr_MIN"]]
        tbl_FAPairs <- xyz3_2[["tbl_FAPairs"]]
        remove(xyz3_2)
      }
    }
  } else if (abs(ACatNetS[2]) > ACatNetL[2]){
    for (i in 4:1){
      if (fcat_bal[i] > 0.01 & A_balance_group[2] > 0.01 & nrow(FsecBnd_Sh[[i]]) >=1 & nrow(UsgNetgrL[[2]]) >=1){
        if (prnt_f == "Y"){paste("Function fa_UNhedging_step3_2 version 2 run for group ",i,sep="")}
        xyz3_2 <- fa_UNhedging_step3_2(A_balance_group[2], FsecBnd_Sh[[i]], UsgNetgrL[[2]], tbl_FAPairs, i, 2, 0, AratesAdj)
        A_balance_group[2] <-xyz3_2[["net_total_group"]]
        UsgNetgrL[[2]] <- xyz3_2[["govsec_band"]]
        FsecBnd_Sh[[i]] <-xyz3_2[["corpsec_hedgegr_MIN"]]
        tbl_FAPairs <- xyz3_2[["tbl_FAPairs"]]
        remove(xyz3_2)
      }
    }
  }
  if(abs(ACatNetS[1]) < ACatNetL[1]){
    for (i in 2:1){
      if (fcat_bal[i] < -0.01 & nrow(FsecBnd_Ln[[i]]) >=1 & nrow(UsgNetgrS[[1]]) >=1){
        if (prnt_f == "Y"){paste("Function fa_UNhedging_step3_2 version 1 run for group ",i,sep="")}
        xyz3_2 <- fa_UNhedging_step3_2(A_balance_group[1], FsecBnd_Ln[[i]], UsgNetgrS[[1]], tbl_FAPairs, i, 1, 1, AratesAdj)
        A_balance_group[1] <-xyz3_2[["net_total_group"]]
        UsgNetgrS[[1]] <- xyz3_2[["govsec_band"]]
        FsecBnd_Ln[[i]] <-xyz3_2[["corpsec_hedgegr_MIN"]]
        tbl_FAPairs <- xyz3_2[["tbl_FAPairs"]]
        remove(xyz3_2)
      }
    }
  } else if (abs(ACatNetS[1]) > ACatNetL[1]){
    for (i in 2:1){
      if (fcat_bal[i] > 0.01 & nrow(FsecBnd_Sh[[i]]) >=1 & nrow(UsgNetgrL[[1]]) >=1){
        if (prnt_f == "Y"){paste("Function fa_UNhedging_step3_2 version 2 run for group ",i,sep="")}
        xyz3_2 <- fa_UNhedging_step3_2(A_balance_group[1], FsecBnd_Sh[[i]], UsgNetgrL[[1]], tbl_FAPairs, i, 1, 0, AratesAdj)
        A_balance_group[1]<-xyz3_2[["net_total_group"]]
        UsgNetgrL[[1]] <- xyz3_2[["govsec_band"]]
        FsecBnd_Sh[[i]] <-xyz3_2[["corpsec_hedgegr_MIN"]]
        tbl_FAPairs <- xyz3_2[["tbl_FAPairs"]]
        remove(xyz3_2)
      }
    }
  }
}

#refresh A groups
for (i in 1:4) {
  AsecBnd_Ln[[i+9]] <- subset(UsgNetgrL[[1]], UsgMatur >= ((i-1)*3) & UsgMatur < i*3 )
  AsecBnd_Sh[[i+9]] <- subset(UsgNetgrS[[1]], UsgMatur >= ((i-1)*3) & UsgMatur < i*3 )
}
var_z32k <- c(1, 2, 2, 3, 3, 4, 4, 4, 4)
for (i in 2:9) {
  AsecBnd_Ln[[i]]   <- subset(UsgNetgrL[[var_z32k[i]]], UsgMatur >= A_MtrBrd[i+3] & UsgMatur < A_MtrBrd[i+4] )
  AsecBnd_Sh[[i]]   <- subset(UsgNetgrS[[var_z32k[i]]], UsgMatur >= A_MtrBrd[i+3] & UsgMatur < A_MtrBrd[i+4] )
}
remove(A_MtrBrd)

#Step 4.0 Section A
for (i in 13:2){AbndHct[i] <- (sum(AsecBnd_Ln[[i]]$mv_gov_rem  ) - sum(AsecBnd_Sh[[i]]$mv_gov_rem  )) * AratesAdj[i]}
for (i in 4:1){
  AbndHctTmp   <- AbndHct[CatListA[[i]]]
  ACatNetL[i]  <- sum(AbndHctTmp[AbndHctTmp >= 0])
  ACatNetS[i]  <- sum(AbndHctTmp[AbndHctTmp <  0])
}
for (i in 1:4){a_categ_haircut[i] <- (abs(ACatNetL[i] + ACatNetS[i]) + min(ACatNetL[i], abs(ACatNetS[i]))*0.5)}
haircut_before_aa <- a_categ_haircut[1] + a_categ_haircut[2] + a_categ_haircut[3] + a_categ_haircut[4]
#Step 4.0 Section B: Function aa_netting  was set-up before step 3.2
#Section 4.0 Section C: Function run categories with different sign

if (distrib == 1 | distrib == 3){
  for (i in 3:1){
    if (prnt_f == "Y"){paste("Function aa_1234 2nd stage run for group ",i,sep="")}
    aa_1234 <- aa_netting(i, AsecBnd_Ln, AsecBnd_Sh, tbl_AAPairs, AbndHct, AratesAdj, A_SubgrBal, ACatNetL, ACatNetS)
    AsecBnd_Ln  <- aa_1234[["x_SecALong"]]
    AsecBnd_Sh  <- aa_1234[["x_SecAShort"]]
    tbl_AAPairs <- aa_1234[["v_tbl_AAPairs"]]
    A_SubgrBal  <- aa_1234[["v_A_SubgrBal"]]
    AbndHct     <- aa_1234[["v_AbndHct"]]
  }
}

#Step 3.1. Start : Part A - function definition
fa_hedging_part31 <- function(maxhedge_amt, v_CorpHedgegrIn, v_UsgHedgeband, var_i1, var_j1, tbl_FAPairs, v_Exit0, y_rltv_maturity, y_hrcut_fa, v_CorpHdgSideId, v_CorpNhdgSideId, y_NhdgSideMtr, v2_FAPairId, y_fhedge_side){
  while (var_i1 >= 1 & var_j1 >= 1 & abs(maxhedge_amt) >= 0.01 & v_Exit0 == 0){
    #Main Cycle 
    #Next line checks if current F security is fully hedged: if so, it will move to the next one with lower maturity
    if (v_CorpHedgegrIn[var_j1,"mv_rem"] < 0.01 & var_j1 > 1){
      var_j1 <- (var_j1 - 1)
    } else if (v_CorpHedgegrIn[var_j1,"mv_rem"] < 0.01 & var_j1 <= 1){
      v_Exit0 <- 1;
      #F security indicator is standing at the top of the table, and current security non-offset value is 0, hence need to exit the cycle;
    } else {
      if (v_UsgHedgeband[var_i1,"mv_gov_rem"] < 0.01 & var_i1 > 1){
        var_i1 <- (var_i1 - 1)
      } else if (v_UsgHedgeband[var_i1,"mv_gov_rem"] < 0.01 & var_i1 <= 1){
        v_Exit0 <- 1
      } else {
        if (abs(v_CorpHedgegrIn[var_j1, "CorpMatur"] - v_UsgHedgeband[var_i1, "UsgMatur"]) <= y_rltv_maturity){
          #Check whether we can hedge A and F securities
          if  (abs(maxhedge_amt) >= min(c(v_UsgHedgeband[var_i1,"mv_gov_rem"], v_CorpHedgegrIn[var_j1,"mv_rem"]))){
            hedging_amount <- min(c(v_UsgHedgeband[var_i1,"mv_gov_rem"], v_CorpHedgegrIn[var_j1,"mv_rem"]))
            if (y_fhedge_side == 0){
              MatchFA <- c(nrow(tbl_FAPairs), v_UsgHedgeband[var_i1,"unique_id_gov"], v_CorpHedgegrIn[var_j1,"unique_id"], hedging_amount, v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"CorpMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId)
            } else {
              MatchFA <- c(nrow(tbl_FAPairs), v_CorpHedgegrIn[var_j1,"unique_id"], v_UsgHedgeband[var_i1,"unique_id_gov"], hedging_amount, v_CorpHedgegrIn[var_j1,"CorpMatur"], v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId)
            }
            v2_FAPairId <- (v2_FAPairId + 1)
            tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA)
            if (v_UsgHedgeband[var_i1,"mv_gov_rem"] >=  v_CorpHedgegrIn[var_j1,"mv_rem"]){
              maxhedge_amt <- (maxhedge_amt - hedging_amount)
              v_UsgHedgeband[var_i1,"mv_gov_rem"] <- (v_UsgHedgeband[var_i1,"mv_gov_rem"] - hedging_amount)
              v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); 
              v_CorpHedgegrIn[var_j1,"mv_rem"] <- 0
              var_j1 <- (var_j1 - 1)
              if (v_UsgHedgeband[var_i1,"mv_gov_rem"] < 0.01){
                if (var_i1 > 1){ 
                  var_i1 <- (var_i1 - 1)
                } else {
                  v_Exit0 <- 1
                }
              }
            } else {
              maxhedge_amt <- (maxhedge_amt -  v_UsgHedgeband[var_i1,"mv_gov_rem"])
              v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- (v_CorpHedgegrIn[var_j1,"mv_rem"] - hedging_amount); 
              v_UsgHedgeband[var_i1,"mv_gov_rem"] <- 0; 
              if (var_i1 > 1){
                var_i1 <- (var_i1 - 1);
              } else {
                v_Exit0 <- 1
              }
            }
          } else {
            hedging_amount <- maxhedge_amt
            if (y_fhedge_side == 0){
              MatchFA <- c(nrow(tbl_FAPairs), v_UsgHedgeband[var_i1,"unique_id_gov"], v_CorpHedgegrIn[var_j1,"unique_id"], hedging_amount, v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"CorpMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
            } else {
              MatchFA <- c(nrow(tbl_FAPairs), v_CorpHedgegrIn[var_j1,"unique_id"], v_UsgHedgeband[var_i1,"unique_id_gov"], hedging_amount, v_CorpHedgegrIn[var_j1,"CorpMatur"], v_UsgHedgeband[var_i1,"UsgMatur"], v_CorpHedgegrIn[var_j1,"unique_id"], y_fhedge_side, y_hrcut_fa, v2_FAPairId); v2_FAPairId <- (v2_FAPairId + 1)
            }
            tbl_FAPairs <- rbind(tbl_FAPairs, MatchFA); 
            v_UsgHedgeband[var_i1,"mv_gov_rem"] <- (v_UsgHedgeband[var_i1,"mv_gov_rem"] - hedging_amount); 
            v_CorpHedgegrIn[var_j1,"mv_h2fa"] <- (v_CorpHedgegrIn[var_j1,"mv_h2fa"] + hedging_amount); v_CorpHedgegrIn[var_j1,"mv_rem"] <- (v_CorpHedgegrIn[var_j1,"mv_rem"] - hedging_amount);   
            maxhedge_amt   <- 0
          }
        } else {
          if ((v_CorpHedgegrIn[var_j1, "CorpMatur"] - v_UsgHedgeband[var_i1, "UsgMatur"]) >=0){
            if (var_j1 > 1){
              var_j1 <- (var_j1 - 1)
            } else {
              v_Exit0 <- 1
            }
          } else {
            if (var_i1 > 1){
              var_i1 <- (var_i1 - 1)
            } else {
              v_Exit0 <- 1
            }
          }
        }
      }
    }
  }
  xyz_31 <- list("y_govsec_band" = v_UsgHedgeband, "v_CorpHedgegrIn" = v_CorpHedgegrIn, "y_pairtbl_fa" = tbl_FAPairs)
  return(xyz_31);
}
#Part B - function run
if (nrow(FsecBnd_Ln[[1]]) >=1){
  fcat_bal_l[1] <- sum(FsecBnd_Ln[[1]]["mv_rem"])
} else {
  fcat_bal_l[1] <- 0
}
if (nrow(FsecBnd_Sh[[1]]) >=1){
  fcat_bal_s[1] <- sum(FsecBnd_Sh[[1]]["mv_rem"])
} else {
  fcat_bal_s[1] <- 0
}

if ((fcat_bal_l[1]  - fcat_bal_s[1]) >= 0.01 & nrow(FsecBnd_Ln[[1]]) >= 1){
  if (ACatNetL[2] < abs(ACatNetS[2])){
    if (AbndHct[2] <= -0.01 & nrow(AsecBnd_Sh[[2]]) >= 1){
      v_Exit0 <- 0
      var_i1 <- nrow(AsecBnd_Sh[[2]])
      var_j1 <- nrow(FsecBnd_Ln[[1]])
      maxhedge_amt <- min(sum(FsecBnd_Ln[[1]]$mv_rem) - fcat_bal_s[1], abs(AbndHct[2]/(AratesAdj[2])), abs(ACatNetL[2] + ACatNetS[2])/(AratesAdj[2]))
      if (prnt_f == "Y"){print("Function aa_31 Net Long run for group 2")}
      aa31 <- fa_hedging_part31(maxhedge_amt, FsecBnd_Ln[[1]], AsecBnd_Sh[[2]], var_i1, var_j1, tbl_FAPairs, v_Exit0, 6, 0.015, "id_long", "id_short", "MtrSh", 31, 1)
      AsecBnd_Sh[[2]]  <- aa31[["y_govsec_band"]]
      FsecBnd_Ln[[1]]  <- aa31[["v_CorpHedgegrIn"]]
      tbl_FAPairs <- aa31[["y_pairtbl_fa"]]
      AbndHct[2] <- (sum(AsecBnd_Ln[[2]]$mv_gov_rem  ) - sum(AsecBnd_Sh[[2]]$mv_gov_rem  )) * AratesAdj[2]
      ACatNetL[2] <- (max(c(AbndHct[3], 0)) + max(c(AbndHct[2], 0)))
      ACatNetS[2] <- (min(c(AbndHct[3], 0)) + min(c(AbndHct[2], 0)))
    }  
  }
  if (ACatNetL[1] < abs(ACatNetS[1])){
    if (AbndHct[13] <= -0.01 & nrow(AsecBnd_Sh[[13]]) >= 1){
      v_Exit0 <- 0
      var_i1 <- nrow(AsecBnd_Sh[[13]])
      var_j1 <- nrow(FsecBnd_Ln[[1]])
      maxhedge_amt <- min(sum(FsecBnd_Ln[[1]]$mv_rem) - fcat_bal_s[1], abs(AbndHct[13]/(AratesAdj[13])), abs(ACatNetL[1] + ACatNetS[1])/(AratesAdj[13]))
      if (prnt_f == "Y"){print("Function aa_31 version 1 run for group 13")}
      aa31 <- fa_hedging_part31(maxhedge_amt, FsecBnd_Ln[[1]], AsecBnd_Sh[[13]], var_i1, var_j1, tbl_FAPairs, v_Exit0, 6, 0.015, "id_long", "id_short", "MtrSh", 31, 1)
      AsecBnd_Sh[[13]]  <- aa31[["y_govsec_band"]]
      FsecBnd_Ln[[1]]  <- aa31[["v_CorpHedgegrIn"]]
      tbl_FAPairs <- aa31[["y_pairtbl_fa"]]
      AbndHct[13] <- (sum(AsecBnd_Ln[[13]]$mv_gov_rem  ) - sum(AsecBnd_Sh[[13]]$mv_gov_rem  )) * AratesAdj[13]
      ACatNetL[1] <- (max(c(AbndHct[13], 0)) + max(c(AbndHct[12], 0)) + max(c(AbndHct[11], 0)) + max(c(AbndHct[10], 0)))
      ACatNetS[1] <- (min(c(AbndHct[13], 0)) + min(c(AbndHct[12], 0)) + min(c(AbndHct[11], 0)) + min(c(AbndHct[10], 0)))
    }
    if (AbndHct[12] <= -0.01 & nrow(AsecBnd_Sh[[12]]) >= 1){
      v_Exit0 <- 0
      var_i1 <- nrow(AsecBnd_Sh[[12]])
      var_j1 <- nrow(FsecBnd_Ln[[1]])
      maxhedge_amt <- min(sum(FsecBnd_Ln[[1]]$mv_rem) - fcat_bal_s[1], abs(AbndHct[12]/(AratesAdj[12])), abs(ACatNetL[1] + ACatNetS[1])/(AratesAdj[12]))
      if (prnt_f == "Y"){print("Function aa_31 version 1 run for group 12")}
      aa31 <- fa_hedging_part31(maxhedge_amt, FsecBnd_Ln[[1]], AsecBnd_Sh[[12]], var_i1, var_j1, tbl_FAPairs, v_Exit0, 6, 0.015, "id_long", "id_short", "MtrSh", 31, 1) 
      AsecBnd_Sh[[12]]  <- aa31[["y_govsec_band"]]
      FsecBnd_Ln[[1]]  <- aa31[["v_CorpHedgegrIn"]]
      tbl_FAPairs <- aa31[["y_pairtbl_fa"]]
      AbndHct[12] <- (sum(AsecBnd_Ln[[12]]$mv_gov_rem  ) - sum(AsecBnd_Sh[[12]]$mv_gov_rem  )) * AratesAdj[12]
      ACatNetL[1] <- (max(c(AbndHct[13], 0)) + max(c(AbndHct[12], 0)) + max(c(AbndHct[11], 0)) + max(c(AbndHct[10], 0)))
      ACatNetS[1] <- (min(c(AbndHct[13], 0)) + min(c(AbndHct[12], 0)) + min(c(AbndHct[11], 0)) + min(c(AbndHct[10], 0)))   
    }
    if (AbndHct[11] <= -0.01 & nrow(AsecBnd_Sh[[11]]) >= 1){
      v_Exit0 <- 0
      var_i1 <- nrow(AsecBnd_Sh[[11]])
      var_j1 <- nrow(FsecBnd_Ln[[1]])
      maxhedge_amt <- min(sum(FsecBnd_Ln[[1]]$mv_rem) - fcat_bal_s[1], abs(AbndHct[11]/(AratesAdj[11])), abs(ACatNetL[1] + ACatNetS[1])/(AratesAdj[11]))
      if (prnt_f == "Y"){print("Function aa_31 version 1 run for group 11")}
      aa31 <- fa_hedging_part31(maxhedge_amt, FsecBnd_Ln[[1]], AsecBnd_Sh[[11]], var_i1, var_j1, tbl_FAPairs, v_Exit0, 6, 0.015, "id_long", "id_short", "MtrSh", 31, 1) 
      AsecBnd_Sh[[11]]  <- aa31[["y_govsec_band"]]
      FsecBnd_Ln[[1]]  <- aa31[["v_CorpHedgegrIn"]]
      tbl_FAPairs <- aa31[["y_pairtbl_fa"]]
      AbndHct[11] <- (sum(AsecBnd_Ln[[11]]$mv_gov_rem  ) - sum(AsecBnd_Sh[[11]]$mv_gov_rem  )) * AratesAdj[11]
      ACatNetL[1] <- (max(c(AbndHct[13], 0)) + max(c(AbndHct[12], 0)) + max(c(AbndHct[11], 0)) + max(c(AbndHct[10], 0)))
      ACatNetS[1] <- (min(c(AbndHct[13], 0)) + min(c(AbndHct[12], 0)) + min(c(AbndHct[11], 0)) + min(c(AbndHct[10], 0)))   
    }
  }
  if (nrow(AsecBnd_Sh[[10]]) >= 1){
    v_Exit0 <- 0
    var_i1 <- nrow(AsecBnd_Sh[[10]])
    var_j1 <- nrow(FsecBnd_Ln[[1]])
    maxhedge_amt <- (sum(FsecBnd_Ln[[1]]$mv_rem) - fcat_bal_s[1])
    if (prnt_f == "Y"){print("Function aa_31 version 1 run for group 10")}
    aa31 <- fa_hedging_part31(maxhedge_amt, FsecBnd_Ln[[1]], AsecBnd_Sh[[10]], var_i1, var_j1, tbl_FAPairs, v_Exit0, 6, 0.015, "id_long", "id_short", "MtrSh", 31, 1) 
    AsecBnd_Sh[[10]]  <- aa31[["y_govsec_band"]]
    FsecBnd_Ln[[1]]  <- aa31[["v_CorpHedgegrIn"]]
    tbl_FAPairs <- aa31[["y_pairtbl_fa"]]
    #No need to update A balances since haircut in group 1.1 is 0%    
  }  
} else if ((fcat_bal_l[1]  - fcat_bal_s[1]) <= -0.01 & nrow(FsecBnd_Sh[[1]]) >= 1){
  if (ACatNetL[2] > abs(ACatNetS[2])){
    if (AbndHct[2] >= 0.01 & nrow(AsecBnd_Ln[[2]]) >= 1){
      v_Exit0 <- 0
      var_i1 <- nrow(AsecBnd_Ln[[2]])
      var_j1 <- nrow(FsecBnd_Sh[[1]])
      maxhedge_amt <- min(abs(fcat_bal_l[1] - sum(FsecBnd_Sh[[1]]$mv_rem)), abs(AbndHct[2]/(AratesAdj[2])), abs(ACatNetL[2] + ACatNetS[2])/(AratesAdj[2]))
      if (prnt_f == "Y"){print("Function aa_31 version 2 run for group 2")}
      aa31 <- fa_hedging_part31(maxhedge_amt, FsecBnd_Sh[[1]], AsecBnd_Ln[[2]], var_i1, var_j1, tbl_FAPairs, v_Exit0, 6, 0.015, "id_short", "id_long", "MtrL", 31, 0)
      AsecBnd_Ln[[2]]  <- aa31[["y_govsec_band"]]
      FsecBnd_Sh[[1]]  <- aa31[["v_CorpHedgegrIn"]]
      tbl_FAPairs <- aa31[["y_pairtbl_fa"]]
      AbndHct[2] <- (sum(AsecBnd_Ln[[2]]$mv_gov_rem  ) - sum(AsecBnd_Sh[[2]]$mv_gov_rem  )) * AratesAdj[2]
      ACatNetL[2] <- (max(c(AbndHct[3], 0)) + max(c(AbndHct[2], 0)))
      ACatNetS[2] <- (min(c(AbndHct[3], 0)) + min(c(AbndHct[2], 0)))
    }  
  }
  if (ACatNetL[1] > abs(ACatNetS[1])){
    if (AbndHct[13] >= 0.01 & nrow(AsecBnd_Ln[[13]]) >= 1){
      v_Exit0 <- 0
      var_i1 <- nrow(AsecBnd_Ln[[13]])
      var_j1 <- nrow(FsecBnd_Sh[[1]])
      maxhedge_amt <- min(abs(fcat_bal_l[1] - sum(FsecBnd_Sh[[1]]$mv_rem)), abs(AbndHct[13]/AratesAdj[13]), abs(ACatNetL[1] + ACatNetS[1])/AratesAdj[13])
      if (prnt_f == "Y"){print("Function aa_31 version 2 run for group 13")}
      aa31 <- fa_hedging_part31(maxhedge_amt, FsecBnd_Sh[[1]], AsecBnd_Ln[[13]], var_i1, var_j1, tbl_FAPairs, v_Exit0, 6, 0.015, "id_short", "id_long", "MtrL", 31, 0)
      AsecBnd_Ln[[13]]  <- aa31[["y_govsec_band"]]
      FsecBnd_Sh[[1]]  <- aa31[["v_CorpHedgegrIn"]]
      tbl_FAPairs <- aa31[["y_pairtbl_fa"]]
      AbndHct[13] <- (sum(AsecBnd_Ln[[13]]$mv_gov_rem  ) - sum(AsecBnd_Sh[[13]]$mv_gov_rem  )) * AratesAdj[13]
      ACatNetL[1] <- (max(c(AbndHct[13], 0)) + max(c(AbndHct[12], 0)) + max(c(AbndHct[11], 0)) + max(c(AbndHct[10], 0)))
      ACatNetS[1] <- (min(c(AbndHct[13], 0)) + min(c(AbndHct[12], 0)) + min(c(AbndHct[11], 0)) + min(c(AbndHct[10], 0)))
    }
    if (AbndHct[12] >= 0.01 & nrow(AsecBnd_Ln[[12]]) >= 1){
      v_Exit0 <- 0
      var_i1 <- nrow(AsecBnd_Ln[[12]])
      var_j1 <- nrow(FsecBnd_Sh[[1]])
      maxhedge_amt <- min(abs(fcat_bal_l[1] - sum(FsecBnd_Sh[[1]]$mv_rem)), abs(AbndHct[12]/(AratesAdj[12])), abs(ACatNetL[1] + ACatNetS[1])/(AratesAdj[12]))
      if (prnt_f == "Y"){print("Function aa_31 version 2 run for group 12")}
      aa31 <- fa_hedging_part31(maxhedge_amt, FsecBnd_Sh[[1]], AsecBnd_Ln[[12]], var_i1, var_j1, tbl_FAPairs, v_Exit0, 6, 0.015, "id_short", "id_long", "MtrL", 31, 0)  
      AsecBnd_Ln[[12]]  <- aa31[["y_govsec_band"]]
      FsecBnd_Sh[[1]]  <- aa31[["v_CorpHedgegrIn"]]
      tbl_FAPairs <- aa31[["y_pairtbl_fa"]]
      AbndHct[12] <- (sum(AsecBnd_Ln[[12]]$mv_gov_rem  ) - sum(AsecBnd_Sh[[12]]$mv_gov_rem  )) * AratesAdj[12]
      ACatNetL[1] <- (max(c(AbndHct[13], 0)) + max(c(AbndHct[12], 0)) + max(c(AbndHct[11], 0)) + max(c(AbndHct[10], 0)))
      ACatNetS[1] <- (min(c(AbndHct[13], 0)) + min(c(AbndHct[12], 0)) + min(c(AbndHct[11], 0)) + min(c(AbndHct[10], 0)))   
    }
    if (AbndHct[11] >= 0.01 & nrow(AsecBnd_Ln[[11]]) >= 1){
      v_Exit0 <- 0
      var_i1 <- nrow(AsecBnd_Ln[[11]])
      var_j1 <- nrow(FsecBnd_Sh[[1]])
      maxhedge_amt <- min(abs(fcat_bal_l[1] - sum(FsecBnd_Sh[[1]]$mv_rem)), abs(AbndHct[11] / AratesAdj[11]), abs(ACatNetL[1] + ACatNetS[1])/(AratesAdj[11]))
      if (prnt_f == "Y"){print("Function aa_31 version 2 run for group 11")}
      aa31 <- fa_hedging_part31(maxhedge_amt, FsecBnd_Sh[[1]], AsecBnd_Ln[[11]], var_i1, var_j1, tbl_FAPairs, v_Exit0, 6, 0.015, "id_short", "id_long", "MtrL", 31, 0)  
      AsecBnd_Ln[[11]]  <- aa31[["y_govsec_band"]]
      FsecBnd_Sh[[1]]  <- aa31[["v_CorpHedgegrIn"]]
      tbl_FAPairs <- aa31[["y_pairtbl_fa"]]
      AbndHct[11] <- (sum(AsecBnd_Ln[[11]]$mv_gov_rem  ) - sum(AsecBnd_Sh[[11]]$mv_gov_rem  )) * AratesAdj[11]
      ACatNetL[1] <- (max(c(AbndHct[13], 0)) + max(c(AbndHct[12], 0)) + max(c(AbndHct[11], 0)) + max(c(AbndHct[10], 0)))
      ACatNetS[1] <- (min(c(AbndHct[13], 0)) + min(c(AbndHct[12], 0)) + min(c(AbndHct[11], 0)) + min(c(AbndHct[10], 0)))   
    }
  }
  if (nrow(AsecBnd_Ln[[10]]) >= 1){
    v_Exit0 <- 0
    var_i1 <- nrow(AsecBnd_Ln[[10]])
    var_j1 <- nrow(FsecBnd_Sh[[1]])
    maxhedge_amt <- abs(fcat_bal_l[1] - sum(FsecBnd_Sh[[1]]$mv_rem))
    if (prnt_f == "Y"){print("Function aa_31 version 2 run for group 10")}
    aa31 <- fa_hedging_part31(maxhedge_amt, FsecBnd_Sh[[1]], AsecBnd_Ln[[10]], var_i1, var_j1, tbl_FAPairs, v_Exit0, 6, 0.015, "id_short", "id_long", "MtrL", 31, 0)  
    AsecBnd_Ln[[10]]  <- aa31[["y_govsec_band"]]
    FsecBnd_Sh[[1]]  <- aa31[["v_CorpHedgegrIn"]]
    tbl_FAPairs <- aa31[["y_pairtbl_fa"]]
    #No need to update A balances since haircut in group 1.1 is 0%    
  }
}
#End of Step 3.1
#Step 5.1 for Futures on Government securities
#The table with information on futures will be named futures_gov with columns unique_id_f, original_ccy_code_f, MaturFut, market_value_f
#futures_gov <- read.csv(file="C:\\Users\\vmokhnatkin\\Documents\\1_FUT.csv", head=TRUE, dec=".")
futures_gov$market_value_f <- as.double(futures_gov$market_value_f) 
unique_id_f <- futures_gov$unique_id_f
original_ccy_code_f <- futures_gov$original_ccy_code_f
MaturFut <- futures_gov$residual_maturity_months_f
market_value_f <- futures_gov$market_value_f
mv_fut_rem <- abs(market_value_f)
futures_gov <- data.frame(unique_id_f, original_ccy_code_f, MaturFut, market_value_f, mv_fut_rem, stringsAsFactors=FALSE);
futures_hedgegr_l <- list()
futures_hedgegr_s <- list()
vec_z51a <- c(0, 60, 120, 180, 10^9)

for (i in 1:4){
  futures_hedgegr_l[[i]] <- subset(futures_gov, market_value_f >=0 & MaturFut >= vec_z51a[i]  & MaturFut < vec_z51a[i+1])
  futures_hedgegr_l[[i]] <- futures_hedgegr_l[[i]][order(futures_hedgegr_l[[i]]$MaturFut, decreasing = FALSE ),]
  futures_hedgegr_s[[i]] <- subset(futures_gov, market_value_f < 0 & MaturFut >= vec_z51a[i]  & MaturFut < vec_z51a[i+1])
  futures_hedgegr_s[[i]] <- futures_hedgegr_s[[i]][order(futures_hedgegr_s[[i]]$MaturFut, decreasing = FALSE ),]
}
print("Check Stop")
futures_gov <- futures_gov[order(futures_gov$MaturFut, decreasing = FALSE ),]

#Update balance of F categories
for (i in 9:1){
  if (nrow(FsecBnd_Ln[[i]]) >=1){
    fcat_bal_l[i] <- sum(FsecBnd_Ln[[i]]["mv_rem"])
  } else {
    fcat_bal_l[i] <- 0 
  }
  if (nrow(FsecBnd_Sh[[i]]) >=1){
    fcat_bal_s[i] <- sum(FsecBnd_Sh[[i]]["mv_rem"])
  } else {
    fcat_bal_s[i] <- 0
  }
}

for (i in 9:1){fcat_bal[i] <- (fcat_bal_l[i] - fcat_bal_s[i])}
###
remove(fcat_bal_l, fcat_bal_s)
id_long_fut <- 'l'; id_short_fut <- 's'; MatchAmtFut <- 0; MtrL_Fu <- 0; MtrSh_Fu <- 0; f_sec_side_fut <- 0; HcutPrcFut <- 0;
tbl_FutFAPairs <- data.frame(id_long_fut, id_short_fut, MatchAmtFut, MtrL_Fu, MtrSh_Fu, f_sec_side_fut, HcutPrcFut, stringsAsFactors=FALSE)
#"f_sec_side" column will have 1 designating long and 0 designating short side
###
futures_hedging <- function(f_long, f_short, x_futures_l, x_futures_s, rel_matur, f_balance, x_hrcut_fa, tbl_FutFAPairs){
  id_long_fut <- tbl_FutFAPairs$id_long_fut
  id_short_fut <- tbl_FutFAPairs$id_short_fut
  MatchAmtFut <- tbl_FutFAPairs$MatchAmtFut
  MtrL_Fu <- tbl_FutFAPairs$MtrL_Fu
  MtrSh_Fu <- tbl_FutFAPairs$MtrSh_Fu
  f_sec_side_fut <- tbl_FutFAPairs$f_sec_side_fut
  HcutPrcFut <- tbl_FutFAPairs$HcutPrcFut
  if (abs(f_balance) >= 0.01){
    print("function 1")
    if (f_balance >= 0.01 & nrow(x_futures_s) >= 1){
      pos_f <- nrow(f_long)
      pos_a <- nrow(x_futures_s)
      x_fhedge_side <- 1
      var_exit_inner <- 0
      while (pos_f >= 1 & pos_a >= 1 & abs(f_balance) >= 0.01 & var_exit_inner == 0){
        while (f_long[pos_f, "mv_rem"] < 0.01){
          if (pos_f > 1){
            pos_f <- (pos_f - 1)
          } else {
            var_exit_inner <- 1; 
            break
          }
        }
        while (x_futures_s[pos_a, "mv_fut_rem"] < 0.01){
          if (pos_a > 1){
            pos_a <- (pos_a - 1)
          } else {
            var_exit_inner <- 1; 
            break
          }
        }
        while (abs(f_long[pos_f, "CorpMatur"] - x_futures_s[pos_a, "MaturFut"]) > rel_matur){
          if (f_long[pos_f, "CorpMatur"] >= x_futures_s[pos_a, "MaturFut"]){
            if (pos_f > 1){
              pos_f <- (pos_f - 1)
            } else {
              break
            }
          } else {
            if (pos_a > 1){
              pos_a <- (pos_a - 1)
            } else {
              break
            }
          }
        }
        if (abs(f_long[pos_f, "CorpMatur"] - x_futures_s[pos_a, "MaturFut"]) <= rel_matur & f_long[pos_f, "mv_rem"] >= 0.01 & x_futures_s[pos_a, "mv_fut_rem"] >= 0.01){
          hedge_amount <- min(c(f_long[pos_f, "mv_rem"], x_futures_s[pos_a, "mv_fut_rem"], f_balance))
          id_long_fut <- c(id_long_fut, f_long[pos_f,"unique_id"])
          id_short_fut <- c(id_short_fut, x_futures_s[pos_a,1])
          MatchAmtFut <- c(MatchAmtFut, hedge_amount)
          MtrL_Fu <- c(MtrL_Fu, f_long[pos_f,"CorpMatur"])
          MtrSh_Fu <- c(MtrSh_Fu, x_futures_s[pos_a, "MaturFut"]);
          f_sec_side_fut <- c(f_sec_side_fut, x_fhedge_side)
          HcutPrcFut <- c(HcutPrcFut, x_hrcut_fa)
          f_long[pos_f, "mv_rem"] <- (f_long[pos_f, "mv_rem"] - hedge_amount)
          x_futures_s[pos_a, "mv_fut_rem"] <- (x_futures_s[pos_a, "mv_fut_rem"] - hedge_amount)
          f_balance <- (f_balance - hedge_amount)
          if (x_futures_s[pos_a, "mv_fut_rem"] < 0.01){
            pos_a <- (pos_a - 1)
          }
          if (f_long[pos_f, "mv_rem"] < 0.01){
            pos_f <- (pos_f - 1)
          }
        } else {
          var_exit <- 1
          break
        }
      }
    }
    if (f_balance <= -0.01 & nrow(x_futures_l) >= 1){
      pos_f <- nrow(f_short)
      pos_a <- nrow(x_futures_l)
      x_fhedge_side <- 0
      var_exit_inner <- 0
      while (pos_f >= 1 & pos_a >= 1 & abs(f_balance) >= 0.01 & var_exit_inner == 0){
        while (f_short[pos_f, "mv_rem"] < 0.01){
          if (pos_f > 1){
            pos_f <- (pos_f - 1)
          } else {
            var_exit_inner <- 1
            break
          }
        }
        while (x_futures_l[pos_a, "mv_fut_rem"] < 0.01){
          if (pos_a > 1){
            pos_a <- (pos_a - 1)
          } else {
            var_exit_inner <- 1; 
            break
          }
        }
        while (abs(f_short[pos_f, "CorpMatur"] - x_futures_l[pos_a, "MaturFut"]) > rel_matur){
          if (f_short[pos_f, "CorpMatur"] >= x_futures_l[pos_a, "MaturFut"]){
            if (pos_f > 1){
              pos_f <- (pos_f - 1)
            } else {
              break
            }
          } else {
            if (pos_a > 1){
              pos_a <- (pos_a - 1)
            } else {
              break
            }
          }
        }
        if (abs(f_short[pos_f, "CorpMatur"] - x_futures_l[pos_a, "MaturFut"]) <= rel_matur & f_short[pos_f, "mv_rem"] >= 0.01 & x_futures_l[pos_a, "mv_fut_rem"] >= 0.01){
          hedge_amount <- min(c(f_short[pos_f, "mv_rem"], x_futures_l[pos_a, "mv_fut_rem"], abs(f_balance)))
          id_long_fut <- c(id_long_fut, x_futures_l[pos_a,1])
          id_short_fut <- c(id_short_fut, f_short[pos_f,"unique_id"])
          MatchAmtFut <- c(MatchAmtFut, hedge_amount)
          MtrL_Fu <- c(MtrL_Fu, x_futures_l[pos_a, "MaturFut"])
          MtrSh_Fu <- c(MtrSh_Fu, f_short[pos_f,"CorpMatur"])
          f_sec_side_fut <- c(f_sec_side_fut, x_fhedge_side)
          HcutPrcFut <- c(HcutPrcFut, x_hrcut_fa)
          f_short[pos_f, "mv_rem"] <- (f_short[pos_f, "mv_rem"] - hedge_amount)
          x_futures_l[pos_a, "mv_fut_rem"] <- (x_futures_l[pos_a, "mv_fut_rem"] - hedge_amount)
          f_balance <- (f_balance + hedge_amount)
          if (x_futures_l[pos_a, "mv_fut_rem"] < 0.01){
            pos_a <- (pos_a - 1)
          }
          if (f_short[pos_f, "mv_rem"] < 0.01){
            pos_f <- (pos_f - 1)
          }
        } else {
          var_exit <- 1; 
          break
        }
      }
    }
  }
  tbl_FutFAPairs <- data.frame(id_long_fut, id_short_fut, MatchAmtFut, MtrL_Fu, MtrSh_Fu, f_sec_side_fut, HcutPrcFut, stringsAsFactors=FALSE);
  xyz5 <- list("f_long" = f_long,"f_short" = f_short, "x_futures_l" = x_futures_l, "x_futures_s" = x_futures_s, "tbl_FutFAPairs" = tbl_FutFAPairs)
  return (xyz5)
}

#Function Run
for (i in 9:1){
  if (nrow(FsecBnd_Ln[[i]]) > 0 & nrow(futures_hedgegr_s[[vec_x51a[i]]]) > 0 & fcat_bal[i] > 0 | nrow(FsecBnd_Sh[[i]]) > 0 & nrow(futures_hedgegr_l[[vec_x51a[i]]]) > 0 & fcat_bal[i] < 0){
    if (prnt_f == "Y"){paste("Function futures_hedging run for group ",i,sep="")}
    xyz5_1 <- futures_hedging(FsecBnd_Ln[[i]], FsecBnd_Sh[[i]], futures_hedgegr_l[[vec_x51a[i]]], futures_hedgegr_s[[vec_x51a[i]]], vec_x51b[i], fcat_bal[i], vec_x51c[i], tbl_FutFAPairs)
    FsecBnd_Ln[[i]] <-xyz5_1[["f_long"]]
    FsecBnd_Sh[[i]] <-xyz5_1[["f_short"]]
    futures_hedgegr_l[[vec_x51a[i]]] <- xyz5_1[["x_futures_l"]]
    futures_hedgegr_s[[vec_x51a[i]]] <- xyz5_1[["x_futures_s"]]
    tbl_FutFAPairs <- xyz5_1[["tbl_FutFAPairs"]]
  }
}
# Step 5.2 futures netting 
futures_band29_l <- list()
futures_band29_s <- list()
vec_z52b <- c(1, 1, 1, 1, 2, 3, 4, 4, 4)
vec_z52c <- c(0, 3, 6, 9, 12, 24, 36, 60, 120, 180, 240, 300, 10^9)
for (i in 1:4){
  futures_band29_l[[i+9]] <- subset(futures_hedgegr_l[[vec_z52b[i]]], MaturFut >= vec_z52c[i] & MaturFut < vec_z52c[i+1])
  futures_band29_s[[i+9]] <- subset(futures_hedgegr_s[[vec_z52b[i]]], MaturFut >= vec_z52c[i] & MaturFut < vec_z52c[i+1])
}
for (i in 2:9){
  futures_band29_l[[i]]  <- subset(futures_hedgegr_l[[vec_z52b[i]]], MaturFut >= vec_z52c[i+3] & MaturFut < vec_z52c[i+4])
  futures_band29_s[[i]]  <- subset(futures_hedgegr_s[[vec_z52b[i]]], MaturFut >= vec_z52c[i+3] & MaturFut < vec_z52c[i+4])
}

for (i in 13:2){A_SubgrBal[i]  <- as.double(sum(AsecBnd_Ln[[i]]$mv_gov_rem) - sum(AsecBnd_Sh[[i]]$mv_gov_rem))}
for (i in 13:2){AbndHct[i] <- A_SubgrBal[i] * AratesAdj[i]}
for (i in 4:1){
  AbndHctTmp   <- AbndHct[CatListA[[i]]]
  ACatNetL[i]  <- sum(AbndHctTmp[AbndHctTmp >= 0])
  ACatNetS[i]  <- sum(AbndHctTmp[AbndHctTmp <  0])
}
reserve_a29_l <- rep(0, times = 13)
reserve_a29_s <- rep(0, times = 13)

for (i in 13:2){
  reserve_a29_s[i] <- sum(futures_band29_s[[i]]$mv_fut_rem)
  reserve_a29_l[i] <- sum(futures_band29_l[[i]]$mv_fut_rem)
}

unique_id_f2 <- as.character("id"); MaturFut <- 0; market_value_f <- 0; mv_fut_rem <- 0;
futures_netting <- data.frame(unique_id_f2, MaturFut, market_value_f, mv_fut_rem, stringsAsFactors=FALSE)

futures_allocation1 <- function(categ_net_long, categ_net_short, y_futures_l, y_futures_s, x_a_band_balance, x_a_band_hrcut, reserve_a_l, reserve_a_s, h_prcnt, futures_netting){
  unique_id_f2 <- as.character(futures_netting$unique_id_f2)
  MaturFut <- futures_netting$MaturFut
  market_value_f <- futures_netting$market_value_f
  mv_fut_rem <- futures_netting$mv_fut_rem
  category_delta <- (categ_net_long + categ_net_short)
  if (prnt_f == "Y"){print("FH5_1"); print(futures_netting)}
  if (categ_net_long >= abs(categ_net_short)){
    if (x_a_band_balance >= 0.01 & nrow(y_futures_s) > 0){
      used_amount <- min(category_delta, reserve_a_s*h_prcnt, x_a_band_hrcut)/h_prcnt
      if (prnt_f == "Y"){print("FH5_3"); print(category_delta); print(reserve_a_s); print(x_a_band_hrcut); print(used_amount)}
      pos_fut <- nrow(y_futures_s)
      while (used_amount >= 0.01){
        if (prnt_f == "Y"){print("FH5_4")}
        if (y_futures_s[pos_fut, "mv_fut_rem"] >= 0.01){
          if (y_futures_s[pos_fut, "mv_fut_rem"] <= used_amount){
            unique_id_f2 <- c(unique_id_f2, y_futures_s[pos_fut, 1])
            MaturFut <- c(MaturFut, y_futures_s[pos_fut, 3])
            market_value_f <- c(market_value_f, y_futures_s[pos_fut, 4])
            mv_fut_rem <- c(mv_fut_rem, y_futures_s[pos_fut, "mv_fut_rem"])
            categ_net_long <- (categ_net_long - y_futures_s[pos_fut, "mv_fut_rem"]*h_prcnt)
            used_amount <- (used_amount - y_futures_s[pos_fut, "mv_fut_rem"])
            y_futures_s[pos_fut, "mv_fut_rem"] <- 0
            if (prnt_f == "Y"){print("FH5_6"); print(unique_id_f2); print(mv_fut_rem); print("UsedAmount"); print(used_amount)}
            if (pos_fut > 1){
              pos_fut <- (pos_fut - 1)
            } else {
              break
            }
          } else {
            unique_id_f2 <- c(unique_id_f2, y_futures_s[pos_fut, 1])
            MaturFut <- c(MaturFut, y_futures_s[pos_fut, 3])
            market_value_f <- c(market_value_f, y_futures_s[pos_fut, 4])
            mv_fut_rem <- c(mv_fut_rem, used_amount)
            categ_net_long <- (categ_net_long - used_amount*h_prcnt)
            y_futures_s[pos_fut, "mv_fut_rem"] <- (y_futures_s[pos_fut, "mv_fut_rem"] - used_amount)
            used_amount <- 0
            if (prnt_f == "Y"){print("FH5_9"); print(unique_id_f2); print(mv_fut_rem)}
          }
        } else {
          break
        }
      }
    }
  } else {
    if (x_a_band_balance <= -0.01 & nrow(y_futures_l) > 0){
      used_amount <- min(abs(category_delta), reserve_a_l*h_prcnt, abs(x_a_band_hrcut))/h_prcnt
      pos_fut <- nrow(y_futures_l)
      while (used_amount >= 0.01){
        if (prnt_f == "Y"){print("FH5_12")}
        if (y_futures_l[pos_fut, "mv_fut_rem"] >= 0.01){
          if (prnt_f == "Y"){print("FH5_13")}
          if (y_futures_l[pos_fut, "mv_fut_rem"] <= used_amount){
            unique_id_f2 <- c(unique_id_f2, y_futures_l[pos_fut, 1])
            MaturFut <- c(MaturFut, y_futures_l[pos_fut, 3])
            market_value_f <- c(market_value_f, y_futures_l[pos_fut, 4])
            mv_fut_rem <- c(mv_fut_rem, y_futures_l[pos_fut, "mv_fut_rem"])
            categ_net_short <- (categ_net_short + y_futures_l[pos_fut, "mv_fut_rem"]*h_prcnt)
            used_amount <- (used_amount - y_futures_l[pos_fut, "mv_fut_rem"])
            y_futures_l[pos_fut, "mv_fut_rem"] <- 0
            if (prnt_f == "Y"){print("FH5_14"); print(unique_id_f2); print(mv_fut_rem); print("UsedAmount"); print(used_amount)}
            if (pos_fut > 1){
              pos_fut <- (pos_fut - 1)
            } else {
              break
            }
          } else { 
            unique_id_f2 <- c(unique_id_f2, y_futures_l[pos_fut, 1])
            MaturFut <- c(MaturFut, y_futures_l[pos_fut, 3])
            market_value_f <- c(market_value_f, y_futures_l[pos_fut, 4])
            mv_fut_rem <- c(mv_fut_rem, used_amount)
            categ_net_short <- (categ_net_short + used_amount*h_prcnt)
            y_futures_l[pos_fut, "mv_fut_rem"] <- (y_futures_l[pos_fut, "mv_fut_rem"] - used_amount)
            used_amount <- 0
            if (prnt_f == "Y"){print("FH5_17"); print(used_amount); print(unique_id_f2); print(mv_fut_rem); print(y_futures_l)}
          }
        } else {
          break
        }  
      }
    }
  }
  futures_netting <- data.frame(unique_id_f2, MaturFut, market_value_f, mv_fut_rem, stringsAsFactors=FALSE)
  xyz55 <- list("y_futures_l" = y_futures_l, "y_futures_s" = y_futures_s, "futures_netting" = futures_netting, "categ_net_long" = categ_net_long, "categ_net_short" = categ_net_short)
  return (xyz55)
}

#Function futures_allocation1 run
vec_x52a <- c(1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4)
#@@@@changed to try
vec_55   <- c(9:2, 13:11)
for (i in vec_55){
  if (i <= 9){k <- (i+3)} else {k <- (i-9)}
  if (prnt_f == "Y"){print("Function futures_allocation1 run for group "); print(i)}
  xyz_55 <- futures_allocation1(ACatNetL[vec_x52a[k]], ACatNetS[vec_x52a[k]], futures_band29_l[[i]], futures_band29_s[[i]], A_SubgrBal[i], AbndHct[i], reserve_a29_l[i], reserve_a29_s[i], AratesAdj[i], futures_netting)
  futures_band29_l[[i]] <- xyz_55[["y_futures_l"]] 
  futures_band29_s[[i]] <- xyz_55[["y_futures_s"]]
  futures_netting       <- xyz_55[["futures_netting"]]
  ACatNetL[vec_x52a[k]] <- xyz_55[["categ_net_long"]]
  ACatNetS[vec_x52a[k]] <- xyz_55[["categ_net_short"]]
}

#2-side futures allocation function
f_2SideNetting <- function(futures_netting, y_fut01_l, y_fut00_s){
  unique_id_f2 <- as.character(futures_netting$unique_id_f2)
  MaturFut <- futures_netting$MaturFut
  market_value_f <- futures_netting$market_value_f
  mv_fut_rem <- futures_netting$mv_fut_rem
  if (nrow(y_fut01_l) > 0 & nrow(y_fut00_s) > 0){
    v_i41 <- 1
    v_j42 <- 1
    while (v_i41 <= nrow(y_fut01_l) & v_j42 <= nrow(y_fut00_s)){
      print("Inner 1")
      if (y_fut01_l[v_i41, "mv_fut_rem"] < 0.01){
        print("Inner 2")
        v_i41 <- (v_i41 + 1)
      } else if (y_fut00_s[v_j42, "mv_fut_rem"] < 0.01){
        print("Inner 3")
        v_j42 <- (v_j42 + 1)
      } else {
        print("Inner 4")
        UsageAmt <- min(y_fut01_l[v_i41, "mv_fut_rem"], y_fut00_s[v_j42, "mv_fut_rem"])
        if (prnt_f == "Y"){print("CHKP 33"); print(which(unique_id_f2 == y_fut01_l[v_i41, "unique_id_f"]))}
        if (length(unique_id_f2[which(unique_id_f2 == y_fut01_l[v_i41, "unique_id_f"])] ) > 0){
          mv_fut_rem[which(unique_id_f2 == y_fut01_l[v_i41, "unique_id_f"])] <- (mv_fut_rem[which(unique_id_f2 == y_fut01_l[v_i41, "unique_id_f"])] + UsageAmt)
        } else {
          unique_id_f2 <- c(unique_id_f2, y_fut01_l[v_i41, "unique_id_f"])
          MaturFut <- c(MaturFut, y_fut01_l[v_i41, "MaturFut"])
          market_value_f <- c(market_value_f, y_fut01_l[v_i41, "market_value_f"])
          mv_fut_rem <- c(mv_fut_rem, UsageAmt)
        }
        if (prnt_f == "Y"){print("CHKP 34"); print(which(unique_id_f2 == y_fut00_s[v_j42, "unique_id_f"]))}
        if (length(unique_id_f2[which(unique_id_f2 == y_fut00_s[v_j42, "unique_id_f"])]) > 0){
          mv_fut_rem[which(unique_id_f2 == y_fut00_s[v_j42, "unique_id_f"])] <- (mv_fut_rem[which(unique_id_f2 == y_fut00_s[v_j42, "unique_id_f"])] + UsageAmt)
        } else {
          unique_id_f2 <- c(unique_id_f2, y_fut00_s[v_j42, "unique_id_f"])
          MaturFut <- c(MaturFut, y_fut00_s[v_j42, "MaturFut"])
          market_value_f <- c(market_value_f, y_fut00_s[v_j42, "market_value_f"])
          mv_fut_rem <- c(mv_fut_rem, UsageAmt)		
        }
        y_fut01_l[v_i41, "mv_fut_rem"] <- (y_fut01_l[v_i41, "mv_fut_rem"] - UsageAmt)
        y_fut00_s[v_j42, "mv_fut_rem"] <- (y_fut00_s[v_j42, "mv_fut_rem"] - UsageAmt)
        if (prnt_f == "Y"){print("CHKP 35"); print(unique_id_f2); print(mv_fut_rem)}
      }
    }
    futures_netting <- data.frame(unique_id_f2, MaturFut, market_value_f, mv_fut_rem, stringsAsFactors=FALSE)
  }
  xyz58 <- list("y_fut01_l" = y_fut01_l, "y_fut00_s" = y_fut00_s, "futures_netting" = futures_netting)
}

##2-side futures allocation function run
for (i in vec_55){
  if (prnt_f == "Y"){print("Function f_2SideNetting run for group "); print(i)}
  xyz_58 <- f_2SideNetting(futures_netting, futures_band29_l[[i]], futures_band29_s[[i]])
  futures_band29_l[[i]] <- xyz_58[["y_fut01_l"]] 
  futures_band29_s[[i]] <- xyz_58[["y_fut00_s"]]
  futures_netting       <- xyz_58[["futures_netting"]]
}

#Last Step
id_long <- as.character(tbl_FutFAPairs[, 1])
id_short <- as.character(tbl_FutFAPairs[, 2])
MatchAmt <- tbl_FutFAPairs[, 3]
MtrL <- tbl_FutFAPairs[, 4]
MtrSh <- tbl_FutFAPairs[, 5]
f_sec_side <- tbl_FutFAPairs[, 6]
HcutRate <- tbl_FutFAPairs[, 7]
haircut_amount <- MatchAmt * HcutRate
tbl_FutFAPairs <- data.frame(id_long, id_short, MatchAmt, MtrL, MtrSh, f_sec_side, HcutRate, haircut_amount, stringsAsFactors=FALSE); 
for (i in 4:1){a_categ_haircut[i] <- (abs(ACatNetL[i] + ACatNetS[i]) + min(ACatNetL[i], abs(ACatNetS[i]))*0.5)}
for (i in 1:4){a_categ_haircut[i] <- abs(a_categ_haircut[i])}
haircut_after_aa <- abs(a_categ_haircut[1]) + abs(a_categ_haircut[2]) + abs(a_categ_haircut[3]) + abs(a_categ_haircut[4])
############################################
#ptm_f <- (proc.time() - ptm)
#sink()
#Finished_testing <- Sys.time()
id_long  <- as.character(tbl_FFPairs[, 2])
id_short <- as.character(tbl_FFPairs[, 3])
MatchAmt <- tbl_FFPairs[, 4]
MtrL <- tbl_FFPairs[, 5]
MtrSh <- tbl_FFPairs[, 6]
HcutRate <- tbl_FFPairs[, 7]
RST_Pairs3FF <- data.frame(id_long, id_short, MatchAmt, MtrL, MtrSh, HcutRate, stringsAsFactors=FALSE)
RST_Pairs3FF <- RST_Pairs3FF[-1,]
RST_Pairs2FA <- 1
RST_Pairs1AA <- 1
#Write 1 function for both F-A and F-F pairs (put A ids into the same old-new id table)
#f_conversion <- read.csv(file="C:\\Users\\vmokhnatkin\\Documents\\unique_id_conversion_tbl.csv", head=TRUE, dec=".")
col_1 <- as.character(f_conversion$f_orig_id)
col_2 <- as.character(f_conversion$f_algo_id)
col_3 <- as.double(f_conversion$f_month)
col_4 <- as.double(f_conversion$f_mv)
f_conversion <- data.frame(col_1, col_2, col_3, col_4, stringsAsFactors=FALSE)
copy_conversion <- f_conversion
#function; side variable is 1 when we split long positions id, otherwise -1
#sectype = 1 if FF table, 0 if FA, -1 if AA
assign_id <- function(RST_Pairs3FF, RST_Pairs2FA, f_conversion, side, sectype, RST_Pairs1AA){
  if (sectype == 1){
    id_long  <- RST_Pairs3FF$id_long
    id_short <- RST_Pairs3FF$id_short
    MatchAmt <- RST_Pairs3FF$MatchAmt
    MtrL     <- RST_Pairs3FF$MtrL
    MtrSh    <- RST_Pairs3FF$MtrSh
    HcutRate <- RST_Pairs3FF$HcutRate
    var_z <- RST_Pairs3FF
  } else if (sectype == 0){
    id_long  <- RST_Pairs2FA$id_long
    id_short <- RST_Pairs2FA$id_short
    MatchAmt <- RST_Pairs2FA$MatchAmt
    MtrL     <- RST_Pairs2FA$MtrL
    MtrSh    <- RST_Pairs2FA$MtrSh
    HcutRate <- RST_Pairs2FA$HcutRate
    f_sec_side <- RST_Pairs2FA$f_sec_side
    var_z <- RST_Pairs2FA
  } else {
    id_long  <- RST_Pairs1AA$id_long
    id_short <- RST_Pairs1AA$id_short
    MatchAmt <- RST_Pairs1AA$MatchAmt
    MtrL     <- RST_Pairs1AA$MtrL
    MtrSh    <- RST_Pairs1AA$MtrSh
    var_z <- RST_Pairs1AA
  }
  pos <- 1
  while (pos <= nrow(var_z)){
    if (side == 1){
      crsp_table <- f_conversion[which(f_conversion[,2] == id_long[pos]),]
      crsp_table <- crsp_table[order(crsp_table$col_4, decreasing = TRUE),]
    } else {
      crsp_table <- f_conversion[which(f_conversion[,2] == id_short[pos]),]
      crsp_table <- crsp_table[order(crsp_table$col_4, decreasing = FALSE),]
    }
    var_y <- MatchAmt[pos]
    sub_pos <- 1
    while (var_y >= 0.01 & sub_pos <= nrow(crsp_table)){
      if (round(abs(crsp_table[sub_pos,4]) - var_y, digits=2) >= 0){
        if (side == 1){
          id_long[pos] <- crsp_table[sub_pos,1]
        } else {
          id_short[pos] <- crsp_table[sub_pos,1]
        }
        if (crsp_table[sub_pos,4] >= 0){
          crsp_table[sub_pos,4] <- (crsp_table[sub_pos,4] - var_y)
        } else {
          crsp_table[sub_pos,4] <- (crsp_table[sub_pos,4] + var_y)
        }
        f_conversion[which(f_conversion[,1]==crsp_table[sub_pos,1]),4] <- crsp_table[sub_pos,4]
        var_y <- 0
      } else {
        if (side == 1){
          id_long <- c(id_long, crsp_table[sub_pos,1])
          id_short <- c(id_short, id_short[pos])
        } else {
          id_short <- c(id_short, crsp_table[sub_pos,1])
          id_long  <- c(id_long, id_long[pos])
        }
        MatchAmt <- c(MatchAmt, abs(crsp_table[sub_pos,4]))
        MtrL <- c(MtrL, MtrL[pos])
        MtrSh <- c(MtrSh, MtrSh[pos])
        if (sectype == 0){f_sec_side <- c(f_sec_side, f_sec_side[pos])}
        if (sectype >= 0){HcutRate <- c(HcutRate, HcutRate[pos])}
        var_y <- (var_y - abs(crsp_table[sub_pos,4]))
        MatchAmt[pos] <- (MatchAmt[pos] - abs(crsp_table[sub_pos,4]))
        crsp_table[sub_pos,4] <- 0
        f_conversion[which(f_conversion[,1]==crsp_table[sub_pos,1]),4] <- 0
        sub_pos <- (sub_pos + 1)
      }
    }
    pos <- (pos + 1)
  }
  if (sectype == 1){
    RST_Pairs3FF <- data.frame(id_long, id_short, MatchAmt, MtrL, MtrSh, HcutRate, stringsAsFactors=FALSE)
  } else if (sectype == 0){
    RST_Pairs2FA <- data.frame(id_long, id_short, MatchAmt, MtrL, MtrSh, f_sec_side, HcutRate, stringsAsFactors=FALSE)
  } else {
    RST_Pairs1AA <- data.frame(id_long, id_short, MatchAmt, MtrL, MtrSh, stringsAsFactors=FALSE)
  }
  xyz10 <- list("pairstable" = RST_Pairs3FF, "pairstable_fa" = RST_Pairs2FA, "conversion" = f_conversion, "pairstable_aa" = RST_Pairs1AA)
  return(xyz10)
}

#F-F Long ID
xyz10 <- assign_id(RST_Pairs3FF, RST_Pairs2FA, f_conversion, 1, 1, RST_Pairs1AA)
RST_Pairs3FF <- xyz10[["pairstable"]] 
f_conversion <- xyz10[["conversion"]]
#F-F Short ID
xyz10 <- assign_id(RST_Pairs3FF, RST_Pairs2FA, f_conversion, -1, 1, RST_Pairs1AA)
RST_Pairs3FF <- xyz10[["pairstable"]] 
f_conversion <- xyz10[["conversion"]]
id_long  <- RST_Pairs3FF[,1]
id_short <- RST_Pairs3FF[,2]
MatchAmt <- RST_Pairs3FF[,3]
MtrL <- RST_Pairs3FF[,4]
MtrSh <- RST_Pairs3FF[,5]
HcutRate <- RST_Pairs3FF[,6]
stream_field <- rep(stream_iterator, times=length(id_long))
haircut_amount <- MatchAmt * HcutRate
RST_Pairs3FF <- data.frame(id_long, id_short, MatchAmt, MtrL, MtrSh, HcutRate, haircut_amount, stream_field, stringsAsFactors=FALSE)
#tbl_FAPairs <- tbl_FAPairs[which(tbl_FAPairs[,4] != 0),]
id_long <- as.character(tbl_FAPairs[, 2])
id_short <- as.character(tbl_FAPairs[, 3])
MatchAmt <- tbl_FAPairs[, 4]
MtrL <- tbl_FAPairs[, 5]
MtrSh <- tbl_FAPairs[, 6]
f_sec_side <-tbl_FAPairs[, 8]
HcutRate <-tbl_FAPairs[, 9]
RST_Pairs2FA <- data.frame(id_long, id_short, MatchAmt, MtrL, MtrSh, f_sec_side, HcutRate, stringsAsFactors=FALSE)
RST_Pairs2FA <- RST_Pairs2FA[which(RST_Pairs2FA[,3] != 0),]
xyz10 <- assign_id(RST_Pairs3FF, RST_Pairs2FA, f_conversion, 1, 0, RST_Pairs1AA)
RST_Pairs2FA <- xyz10[["pairstable_fa"]] 
f_conversion <- xyz10[["conversion"]]
xyz10 <- assign_id(RST_Pairs3FF, RST_Pairs2FA, f_conversion, -1, 0, RST_Pairs1AA)
RST_Pairs2FA <- xyz10[["pairstable_fa"]] 
f_conversion <- xyz10[["conversion"]]
id_long  <- RST_Pairs2FA[,1]
id_short <- RST_Pairs2FA[,2]
MatchAmt <- RST_Pairs2FA[,3]
MtrL <- RST_Pairs2FA[,4]
MtrSh <- RST_Pairs2FA[,5]
f_sec_side <- RST_Pairs2FA[,6]
HcutRate <- RST_Pairs2FA[,7]
haircut_amount <- MatchAmt * HcutRate
stream_field <- rep(stream_iterator, times=length(id_long))
RST_Pairs2FA <- data.frame(id_long, id_short, MatchAmt, MtrL, MtrSh, f_sec_side, HcutRate, haircut_amount, stream_field, stringsAsFactors=FALSE);
id_long <- as.character(tbl_AAPairs[,2])
id_short <- as.character(tbl_AAPairs[,3])
MatchAmt <- tbl_AAPairs[,4]
MtrL <- tbl_AAPairs[,5]
MtrSh <- tbl_AAPairs[,6]
RST_Pairs1AA <- data.frame(id_long, id_short, MatchAmt, MtrL, MtrSh, stringsAsFactors=FALSE);
RST_Pairs1AA <- RST_Pairs1AA[-1,]
xyz10 <- assign_id(RST_Pairs3FF, RST_Pairs2FA, f_conversion, 1, -1, RST_Pairs1AA)
RST_Pairs1AA <- xyz10[["pairstable_aa"]] 
f_conversion <- xyz10[["conversion"]]
xyz10 <- assign_id(RST_Pairs3FF, RST_Pairs2FA, f_conversion, -1, -1, RST_Pairs1AA)
RST_Pairs1AA <- xyz10[["pairstable_aa"]] 
f_conversion <- xyz10[["conversion"]]
stream_field <- rep(stream_iterator, times=length(RST_Pairs1AA$id_long))
RST_Pairs1AA[, 'stream_field'] <- stream_field
RST_Pairs3FF <- subset(RST_Pairs3FF, RST_Pairs3FF$MatchAmt >= 1)

fa_total <- sum(tbl_FAPairs[1:nrow(tbl_FAPairs),4] * tbl_FAPairs[1:nrow(tbl_FAPairs),9]) + sum(tbl_FutFAPairs[1:nrow(tbl_FutFAPairs),3] * tbl_FutFAPairs[1:nrow(tbl_FutFAPairs),7])  
ff_total <- sum(tbl_FFPairs[1:nrow(tbl_FFPairs),4] * tbl_FFPairs[1:nrow(tbl_FFPairs),7])
#Residual F
fband_final <- rep(0, times = 9)
for (i in 9:1){fband_final[i] <- max(sum(FsecBnd_Sh[[i]][,"mv_rem"]) , sum(FsecBnd_Ln[[i]][,"mv_rem"])) *  Frates[i]}
f_final_group <- rep(0, times = 4)
f_final_group_mtx <- matrix(c(fband_final[1],fband_final[2],fband_final[3],fband_final[4],fband_final[5],0,0,0,fband_final[6],0,0,0,fband_final[7],fband_final[8],fband_final[9],0),nrow = 4, ncol = 4, byrow = TRUE)
for(i in 1:4){f_final_group[i] <- sum(f_final_group_mtx[i,])}
mv_fband_final <- rep(0, times = 9)
for (i in 9:1){mv_fband_final[i] <- (sum(FsecBnd_Sh[[i]][,"mv_rem"]) + sum(FsecBnd_Ln[[i]][,"mv_rem"]))}
mv_f_final_group <- rep(0, times = 4)
mv_f_final_group_mtx <- matrix(c(mv_fband_final[1],mv_fband_final[2],mv_fband_final[3],mv_fband_final[4],mv_fband_final[5],0,0,0,mv_fband_final[6],0,0,0,mv_fband_final[7],mv_fband_final[8],mv_fband_final[9],0),nrow = 4, ncol = 4, byrow = TRUE)
for(i in 1:4){mv_f_final_group[i] <- sum(mv_f_final_group_mtx[i,])}
mv_A_band29 <- rep(0, times = 9)
mv_A_band1  <- rep(0, times = 4)
for (i in 9:2){mv_A_band29[i] <- (sum(abs(AsecBnd_Sh[[i]][,"mv_gov_rem"])) + sum(AsecBnd_Ln[[i]][,"mv_gov_rem"]))}
for (i in 4:1){mv_A_band1[i] <- (sum(abs(AsecBnd_Sh[[i+9]][,"mv_gov_rem"])) + sum(AsecBnd_Ln[[i+9]][,"mv_gov_rem"]))}
mv_A_final_group <- rep(0, times = 4)
mv_A_final_group_mtx <- matrix(c(mv_A_band29[4], mv_A_band29[3], mv_A_band29[2], mv_A_band1[1], mv_A_band1[2], mv_A_band1[3], mv_A_band1[4],mv_A_band29[5],rep(0, times = 6), mv_A_band29[6],rep(0, times = 6),mv_A_band29[9], mv_A_band29[8], mv_A_band29[7],0,0,0,0), nrow = 4, ncol = 7, byrow = TRUE)
for (i in 1:4){
  mv_A_final_group[i] <- sum(mv_A_final_group_mtx[i,])
}
band_total <- sum(fband_final)
total_haircut <- band_total + fa_total + ff_total + haircut_after_aa
fband_initial <- rep(0, times = 9)
for (i in 9:1){fband_initial[i] <- max(sum(abs(FsecBnd_Sh[[i]][,"corp_mv"])) , sum(FsecBnd_Ln[[i]][,"corp_mv"])) *  Frates[i]}
band_f_total_init <- sum(fband_initial)
f_init_group <- rep(0, times = 4)
f_init_group_mtx <- matrix(c(fband_initial[1],fband_initial[2],fband_initial[3],fband_initial[4],fband_initial[5],0,0,0,fband_initial[6],0,0,0,fband_initial[7],fband_initial[8],fband_initial[9],0),nrow = 4, ncol = 4, byrow = TRUE)
for (i in 1:4){
  f_init_group[i] <- sum(f_init_group_mtx[i,])
}
mv_fband_init <- rep(0, times = 9)
for (i in 9:1){mv_fband_init[i] <- (sum(abs(FsecBnd_Sh[[i]][,"corp_mv"])) + sum(FsecBnd_Ln[[i]][,"corp_mv"]))}
mv_f_init_group <- rep(0, times = 4)
mv_f_init_group_mtx <- matrix(c(mv_fband_init[1],mv_fband_init[2],mv_fband_init[3],mv_fband_init[4],mv_fband_init[5],0,0,0,mv_fband_init[6],0,0,0,mv_fband_init[7],mv_fband_init[8],mv_fband_init[9],0),nrow = 4, ncol = 4, byrow = TRUE)
for (i in 1:4){mv_f_init_group[i] <- sum(mv_f_init_group_mtx[i,])}
mv_A_band29_init <- rep(0, times = 9)
mv_A_band1_init  <- rep(0, times = 4)
for (i in 9:2){
  mv_A_band29_init[i] <- (sum(abs(AsecBnd_Sh[[i]][,"gov_mv"])) + sum(AsecBnd_Ln[[i]][,"gov_mv"]))
}
for (i in 4:1){mv_A_band1_init[i] <- (sum(abs(AsecBnd_Sh[[i+9]][,"gov_mv"])) + sum(AsecBnd_Ln[[i+9]][,"gov_mv"]))}
mv_A_init_group <- rep(0, times = 4)
mv_A_init_group_mtx <- matrix(c(mv_A_band29_init[4], mv_A_band29_init[3], mv_A_band29_init[2], mv_A_band1_init[1], mv_A_band1_init[2], mv_A_band1_init[3], mv_A_band1_init[4],mv_A_band29_init[5],0,0,0,0,0,0,mv_A_band29_init[6],0,0,0,0,0,0,mv_A_band29_init[9], mv_A_band29_init[8], mv_A_band29_init[7],0,0,0,0), nrow = 4, ncol = 7, byrow = TRUE)
for (i in 1:4){mv_A_init_group[i] <- sum(mv_A_init_group_mtx[i,])}
#####
rslt_aband29_l <- rep(0, times = 13)
rslt_aband29_s <- rep(0, times = 13)
for (i in 13:2){
  rslt_aband29_l[i] <- abs(sum(AsecBnd_Ln[[i]][,"gov_mv"])) *  AratesAdj[i]
  rslt_aband29_s[i] <- abs(sum(AsecBnd_Sh[[i]][,"gov_mv"])) *  AratesAdj[i]
}
#####
a_l <- rep(0, times = 4)
a_s <- rep(0, times = 4)
a_l_mtx <- matrix(c(rslt_aband29_l[4], rslt_aband29_l[3], rslt_aband29_l[2], rslt_aband29_l[13], rslt_aband29_l[12], rslt_aband29_l[11], rslt_aband29_l[10],rslt_aband29_l[5],0,0,0,0,0,0,rslt_aband29_l[6],0,0,0,0,0,0,rslt_aband29_l[9],rslt_aband29_l[8],rslt_aband29_l[7],0,0,0,0), nrow = 4, ncol = 7, byrow = TRUE)
a_s_mtx <- matrix(c(rslt_aband29_s[4], rslt_aband29_s[3], rslt_aband29_s[2], rslt_aband29_s[13], rslt_aband29_s[12], rslt_aband29_s[11], rslt_aband29_s[10],rslt_aband29_s[5],0,0,0,0,0,0,rslt_aband29_s[6],0,0,0,0,0,0,rslt_aband29_s[9],rslt_aband29_s[8],rslt_aband29_s[7],0,0,0,0), nrow = 4, ncol = 7, byrow = TRUE)
for (i in 1:4){
  a_l[i] <- sum(a_l_mtx[i,])
  a_s[i] <- sum(a_s_mtx[i,])
}
a_init_group <- rep(0, times = 4)
for (i in 4:1){a_init_group[i] <- max(a_l[i],a_s[i]) - 0.5*min(a_l[i],a_s[i])}
ff_category <- list()
for (i in 1:4){ff_category[[i]] <- tbl_FFPairs[which(tbl_FFPairs[,"MtrL"] <  HdgTime[i+1] & tbl_FFPairs[,"MtrL"] >= HdgTime[i] ),]}
aa_category_s <- list()
aa_category_l <- list()
vec_abands <- matrix(c(12, 0, 15, 36, 24, 48, 120, 96, 10^9), nrow=3, ncol=3, byrow=TRUE)
for (i in 1:3){
  aa_category_s[[i]] <- tbl_AAPairs[which(tbl_AAPairs[,"MtrSh"] <  vec_abands[i, 1] & tbl_AAPairs[,"MtrSh"] >= vec_abands[i, 2] & tbl_AAPairs[,"MtrL"]  < vec_abands[i, 3] & tbl_AAPairs[,"MtrL"]  >= vec_abands[i, 1]),]
  aa_category_l[[i]] <- tbl_AAPairs[which(tbl_AAPairs[,"MtrL"]  <  vec_abands[i, 1] & tbl_AAPairs[,"MtrL"]  >= vec_abands[i, 2] & tbl_AAPairs[,"MtrSh"] < vec_abands[i, 3] & tbl_AAPairs[,"MtrSh"] >= vec_abands[i, 1]),]
}
fa_category_1     <- list()
fa_category_0     <- list()
fa_category_1_fut <- list()
fa_category_0_fut <- list()
for (i in 1:4){
  fa_category_1[[i]]     <- tbl_FAPairs[which(tbl_FAPairs[,"MtrL"]  <  HdgTime[i+1] & tbl_FAPairs[,"MtrL"]  >= HdgTime[i] & tbl_FAPairs[,"f_sec_side"] == 1),]
  fa_category_0[[i]]     <- tbl_FAPairs[which(tbl_FAPairs[,"MtrSh"] <  HdgTime[i+1] & tbl_FAPairs[,"MtrSh"] >= HdgTime[i] & tbl_FAPairs[,"f_sec_side"] == 0),]
  fa_category_1_fut[[i]] <- tbl_FutFAPairs[which(tbl_FutFAPairs[,4] <  HdgTime[i+1] & tbl_FutFAPairs[,4]    >= HdgTime[i] & tbl_FutFAPairs[,6] == 1),]
  fa_category_0_fut[[i]] <- tbl_FutFAPairs[which(tbl_FutFAPairs[,5] <  HdgTime[i+1] & tbl_FutFAPairs[,5]    >= HdgTime[i] & tbl_FutFAPairs[,6] == 0),]
}
empty_aa     <- tbl_AAPairs[1,]
empty_ff     <- tbl_FFPairs[1,]
empty_fa     <- tbl_FAPairs[1,]
empty_fa_fut <- tbl_FutFAPairs[1,]

for (i in 1:4){ff_category[[i]] <-rbind(ff_category[[i]] , empty_ff)}
for (i in 1:3){
  aa_category_s[[i]] <-rbind(aa_category_s[[i]] , empty_aa)
  aa_category_l[[i]] <-rbind(aa_category_l[[i]] , empty_aa)
}

for (i in 1:4){
  fa_category_1[[i]] <-rbind(fa_category_1[[i]], empty_fa)
  fa_category_0[[i]] <-rbind(fa_category_0[[i]], empty_fa)
  fa_category_1_fut[[i]] <- rbind(fa_category_1_fut[[i]], empty_fa_fut)
  fa_category_0_fut[[i]] <- rbind(fa_category_0_fut[[i]], empty_fa_fut)
}
ff_sum <- rep(0, times = 4)
for (i in 1:4){ff_sum[i] <-sum(ff_category[[i]][1:nrow(ff_category[[i]]),4] * ff_category[[i]][1:nrow(ff_category[[i]]),7])}
fa_sum_category_1 <- c(0, 0, 0, 0)
fa_sum_category_0 <- c(0, 0, 0, 0)
for (i in 1:4){
  fa_sum_category_1[i] <- sum(fa_category_1[[i]][, 4] * fa_category_1[[i]][, 9]) + sum(fa_category_1_fut[[i]][, 3] * fa_category_1_fut[[i]][, 7])
  fa_sum_category_0[i] <- sum(fa_category_0[[i]][ ,4] * fa_category_0[[i]][ ,9]) + sum(fa_category_0_fut[[i]][, 3] * fa_category_0_fut[[i]][, 7])
}

fa_sum    <- rep(0, times = 4)
mv_ff_sum <- rep(0, times = 4)
for (i in 1:4){
  fa_sum[i] <- fa_sum_category_1[i] + fa_sum_category_0[i]
  mv_ff_sum[i] <- sum(ff_category[[i]][, 4])*2
}
mv_aa_sum <- rep(0, times = 4)
mv_aa_sum[1] <- (sum(aa_category_s[[1]][, 4]) + sum(aa_category_l[[1]][, 4]))
mv_aa_sum[2] <- (sum(aa_category_s[[1]][, 4]) + sum(aa_category_l[[1]][, 4])) + ( sum(aa_category_s[[2]][, 4]) + sum(aa_category_l[[2]][, 4]) )
mv_aa_sum[3] <- (sum(aa_category_s[[3]][, 4]) + sum(aa_category_l[[3]][, 4])) + ( sum(aa_category_s[[2]][, 4]) + sum(aa_category_l[[2]][, 4]) )
mv_aa_sum[4] <- (sum(aa_category_s[[3]][, 4]) + sum(aa_category_l[[3]][, 4]))
mv_fa_sum_category_1 <- c(0, 0, 0, 0)
mv_fa_sum_category_0 <- c(0, 0, 0, 0)
mv_fa_sum <- c(0, 0, 0, 0)

for (i in 1:4){
  mv_fa_sum_category_1[i] <- (sum(fa_category_1[[i]][, 4]) + sum(fa_category_1_fut[[i]][,3]))
  mv_fa_sum_category_0[i] <- (sum(fa_category_0[[i]][, 4]) + sum(fa_category_0_fut[[i]][,3]))
  mv_fa_sum[i]            <- (mv_fa_sum_category_1[i] + mv_fa_sum_category_0[i])
}
remove(futures_band29_l, futures_band29_s, fa_category_1_fut, fa_category_0_fut)
####
band_a_total_init <- a_init_group[4] + a_init_group[3] + a_init_group[2] + a_init_group[1]
total_init <- band_f_total_init  + band_a_total_init
misc_data_2 <- data.frame(haircut_before_aa,haircut_after_aa, fa_total , ff_total , band_total , total_haircut,band_f_total_init , band_a_total_init, total_init, stream_iterator, stringsAsFactors=FALSE)
type <- c("FA" , "FF" , "F" , "A" , "Initial A", "Initial F" , "MV FA", "MV FF", "MV A" ,"MV Init F", "MV AA", "MV Initial A" , "MV Final A")
categ <- list()
for (i in 1:4){fa_sum[i] <- ifelse(is.na(fa_sum[i]), 0, fa_sum[i])}
for (i in 1:4){categ[[i]] <- c(fa_sum[i], ff_sum[i], f_final_group[i], a_categ_haircut[i], a_init_group[i], f_init_group[i], mv_fa_sum[i], mv_ff_sum[i], mv_f_final_group[i], mv_f_init_group[i], mv_aa_sum[i], mv_A_init_group[i], mv_A_final_group[i])}

#Translating corp IDs in futures pairs table
assign_id_v2 <- function(tbl_FutFAPairs, f_conversion){
  id_long   <- tbl_FutFAPairs$id_long
  id_short  <- tbl_FutFAPairs$id_short
  MatchAmt  <- tbl_FutFAPairs$MatchAmt
  MtrL      <- tbl_FutFAPairs$MtrL
  MtrSh     <- tbl_FutFAPairs$MtrSh
  f_sec_side <- tbl_FutFAPairs$f_sec_side
  HcutRate   <- tbl_FutFAPairs$HcutRate
  pos <- 1
  var_z <- nrow(tbl_FutFAPairs)
  while (pos <= var_z){
    if(f_sec_side[pos] == 1){side <- f_sec_side[pos]} else {side <- f_sec_side[pos]}
    var_y <- MatchAmt[pos]
    if (side == 1){
      crsp_table <- f_conversion[which(f_conversion[,2] == id_long[pos]),]
      crsp_table <- crsp_table[order(crsp_table$col_4, decreasing = TRUE),]  
    } else {
      crsp_table <- f_conversion[which(f_conversion[,2] == id_short[pos]),]
      crsp_table <- crsp_table[order(crsp_table$col_4, decreasing = FALSE),]  
    }
    sub_pos <- 1
    while (var_y >= 0.01 & sub_pos <= nrow(crsp_table)){
      if (round(abs(crsp_table[sub_pos,4]) - var_y, digits=2) >= 0){
        if (side == 1){
          id_long[pos] <- crsp_table[sub_pos,1]
        } else {
          id_short[pos] <- crsp_table[sub_pos,1]
        }
        if (crsp_table[sub_pos,4] >= 0){
          crsp_table[sub_pos,4] <- (crsp_table[sub_pos,4] - var_y)
        } else {
          crsp_table[sub_pos,4] <- (crsp_table[sub_pos,4] + var_y)
        }
        f_conversion[which(f_conversion[,1]==crsp_table[sub_pos,1]),4] <- crsp_table[sub_pos,4]
        var_y <- 0
      } else {
        if (side == 1){
          id_long <- c(id_long, crsp_table[sub_pos,1])
          id_short <- c(id_short, id_short[pos])
        } else {
          id_short <- c(id_short, crsp_table[sub_pos,1])
          id_long  <- c(id_long, id_long[pos])
        }
        MatchAmt <- c(MatchAmt, abs(crsp_table[sub_pos,4]))
        MtrL <- c(MtrL, MtrL[pos])
        MtrSh <- c(MtrSh, MtrSh[pos])
        f_sec_side <- c(f_sec_side, f_sec_side[pos])
        HcutRate <- c(HcutRate, HcutRate[pos])
        var_y <- (var_y - abs(crsp_table[sub_pos,4]))
        MatchAmt[pos] <- (MatchAmt[pos] - abs(crsp_table[sub_pos,4]))
        crsp_table[sub_pos,4] <- 0
        f_conversion[which(f_conversion[,1]==crsp_table[sub_pos,1]),4] <- 0
        sub_pos <- (sub_pos + 1)
      }
    }
    print("Checkpoint 10.11")
    pos <- (pos + 1)
  }
  
  haircut_amount <- MatchAmt * HcutRate
  tbl_FutFAPairs <- data.frame(id_long, id_short, MatchAmt, MtrL, MtrSh, f_sec_side, HcutRate, haircut_amount, stringsAsFactors=FALSE)
  xyz12 <- list("pairstable_fut" = tbl_FutFAPairs, "conversion" = f_conversion)
  return(xyz12)
}

tbl_FutFAPairs <- tbl_FutFAPairs[-1,]
xyz12 <- assign_id_v2(tbl_FutFAPairs, f_conversion)
tbl_FutFAPairs <- xyz12[["pairstable_fut"]] 
f_conversion <- xyz12[["conversion"]]
stream_field <- rep(stream_iterator, times=13)
report_results_2 <- data.frame(type,categ[[1]],categ[[2]],categ[[3]],categ[[4]], stream_field, stringsAsFactors=FALSE)
stream_field <- rep(stream_iterator, times=length(futures_netting$unique_id_f))
futures_netting[, 'stream_field'] <- stream_field
futures_incl_aa <- futures_netting[-1,]
stream_field <- rep(stream_iterator, times=length(tbl_FutFAPairs$id_long))
tbl_FutFAPairs[, 'stream_field'] <- stream_field
RST_Pairs2FA <- RST_Pairs2FA[which(RST_Pairs2FA$MatchAmt >=1),]
RST_Pairs3FF <- RST_Pairs3FF[which(RST_Pairs3FF$MatchAmt >=1),]
RST_Pairs1AA <- RST_Pairs1AA[which(RST_Pairs1AA$MatchAmt >=1),]
geterrmessage()