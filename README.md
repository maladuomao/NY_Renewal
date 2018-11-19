### Custom pricing for JE Renewals ###################################


library(data.table)
library(lubridate)
library(RODBC)

setwd("Y:/Custom_Pricing_R/ECL_Pricing_Model_R")

source("Functions/Useful.R")
source("Functions/Reference.R")

# General parameters ==================================

state = "NY"
csv_name = "NY_Renewals_Jan2019"

# =====================================================

## Import customer file with at least 12 months of HU
# Queries to generate the file are saved in "../Data/Renewals/SQL"
rnw = fread(paste0("../Data/Renewals/", csv_name, ".csv"), integer64 = "character")

### Initial clean up ####################################

rnw[, grep("Date", colnames(rnw)) := lapply(.SD, mdy)
    , .SD = grep("Date", colnames(rnw), value = TRUE)]

rnw[, grep("Month$", colnames(rnw)) := lapply(.SD, mdy)
    , .SD = grep("Month$", colnames(rnw), value = TRUE)]

rnw[, `:=` (CAP = as.numeric(CAP), NITS = as.numeric(NITS))]

rnw[, Usage := as.numeric(Usage)]

rnw[, MaxEndDate := NULL] # don't need this column (too lazy to remove it from query)

rnw[Commodity == "ELECTRIC" & ProductType == "Fixed"
    , Price := Price * 100] # calculations all assume c/kWh

rnw = ref.assign_msi(rnw, "MarketParticipantIdentifier"
                     , inc_zone = (state %in% c("NY", "MA")) # join by zone if MA or NY
                     , z_colname = "Zone")

rnw = ref.assign_units(rnw, "MarketStructureIdentifier")  # ref.btu_factors need to be updated (MCR has this information)

### Calendarize Usages ####################################

source("Functions/Calendarize.R")

rnw_nousage = rnw[is.na(Usage)]
rnw = rnw[!is.na(Usage)]

rnw[, ID := ContractOID] # set unique 'ID' column - needed for calendarization function
setkey(rnw, ID)

rnw[, Days := as.numeric(EndDate - StartDate) + 1] # length of billing period - start and end dates are non-overlapping

rnw = cal.main(rnw)
rnw = cal.cleanup(rnw)

rnw[, AnnualUsage_Cal := AnnualUsage] # back up calendarized usages

rnw_nousage[, setdiff(colnames(rnw), colnames(rnw_nousage)) := NA]
rnw_nousage[, setdiff(colnames(rnw_nousage), colnames(rnw)) := NULL]
rnw = rbind(rnw, rnw_nousage)
rm(rnw_nousage)

rnw[is.na(AnnualUsage) | Ct < 12, AnnualUsage := GBASS_Vol] # use gbass RCE when less than 12 months available
rnw[, RCE := AnnualUsage/Ref_Units_RCE]

## clean up
rm(list=grep("^cal.", ls(), value = TRUE))


rnw[,`:=` (ICAPEffectiveDate = NULL, NITSEffectiveDate = NULL)]


### Cost Data #######################################################

source("Functions/Queries.R")
source("Functions/Run_Queries.R")
source("Functions/Cost_Calc.R")

## costing parameters ==========================================


batchoid = 261399                 # usually use demand forecast
batchoid_gas = 261473             # Populate this if using different batch for gas (e.g. different demand forecast dates). Otherwise set to NA.
asofdate = Sys.Date()             # pull latest curves before this date
startdate = as.Date("2019-01-01") # cost start date

exclude_NITS = TRUE               # set this to TRUE if OH or MA, FALSE otherwise (will exclude NITS costs in calculations)
no_custom_costs = TRUE            # set this to TRUE to only pull cts (no capacity costs, nits, etc.) e.g. when only pulling gas costs, FALSE otherwise.

odbc_archimedes = "archimedes"    # odbc data source name for archimedes
odbc_mcr = "JE_External"          # odbc data source name for MCR database

term = sort(unique(c(6, 12, 24, 36, 48, 60, rnw[ProductType != "Variable", Term])))  # calculate costs for these terms

curve_list = c("CapacityRate"
               , "ForecastPoolRequirement"
               , "ZonalRPMScalingFactor"
               , "CapacityVolumeAdjustment"
               , "NITSRate"
               , "NITSVolumeAdjustment"
               , "ARRRate"
               , "TransmissionEnhancementRate"
               , "UCAPFactor" # Ameren
               # , "CapacityScaleFactor" # Massachusetts
               ) 

msi_list = unique(rnw[, MarketStructureIdentifier]) # marketstructureidentifiers to pull


# ==============================================================

vwresult = rq.pull_vwr(odbc_archimedes, batchoid, msi_list, cts_only = no_custom_costs)

if (no_custom_costs == FALSE) {
  curve_data = rq.pull_curves(odbc_archimedes, grep("PWR", msi_list, value = TRUE), curve_list, asofdate)
}



### don't run if only have power
if(!is.na(batchoid_gas)) { # remove and replace gas costs with ones from the gas batchoid
  
  vwresult = vwresult[grep("PWR", MarketStructureIdentifier)]
  
 vwresult_gas = rq.pull_vwr(odbc_archimedes, batchoid_gas, grep("GAS", msi_list, value = TRUE), cts_only = TRUE)
 vwresult_gas[, setdiff(colnames(vwresult), colnames(vwresult_gas)) := NA]

 vwresult = rbind(vwresult, vwresult_gas)
 rm(vwresult_gas)
 }

## build cts table

cts = cost.cts_table(msi_list, term, vwresult, startdate, cts_only = no_custom_costs)

if (no_custom_costs == FALSE) {
  cts = cost.sum_curves(curve_data, startdate)
}

## for reference pull por, grt, latest utility rate

por_grt = rq.por_grt(odbc_mcr, msi_list)

utility_rate = rq.latest_utility(odbc_mcr, msi_list)

### Calculate Custom Costs ################################################################

# rnw[is.na(CAP) | is.na(NITS)] # check customers that are missing cap or nits tags

if (no_custom_costs == TRUE) {
  
  rnw = cost.gas_cts(rnw, cts, msi_list, term)
  
} else {
  
  ## Electric only
  if(nrow(rnw[Commodity=="GAS"]) == 0) { 
    
    rnw = cost.calc_vol_mult(rnw, term, startdate 
                             , grep("PWR", msi_list, value = TRUE)
                             , vwresult[grep("PWR", MarketStructureIdentifier)])
    
    rnw = cost.missing_vol(rnw, cts
                           , grep("PWR", msi_list, value = TRUE), term) # use massmarket costs if less than 12 months of usage
    
    rnw = cost.calc_cap_nits(rnw, term, msi_list, cts, state)
    
    rnw = cost.missing_tags(rnw, cts, msi_list, term, state) # use massmarket costs if missing tags
    
    rnw = cost.add_total(rnw, term, state)
    
    
    ## Gas only
  } else if (nrow(rnw[Commodity=="ELECTRIC"]) == 0) {
    
    rnw = cost.gas_cts(rnw, cts, msi_list, term)
    
    ## Electric and gas - split, calculate costs separately, then combine
  } else {
    
    rnw_pwr = rnw[Commodity=="ELECTRIC"]
    
    rnw_gas = rnw[Commodity=="GAS"]
    
    ## PWR
    
    rnw_pwr = cost.calc_vol_mult(rnw_pwr, term, startdate 
                                 , grep("PWR", msi_list, value = TRUE)
                                 , vwresult[grep("PWR", MarketStructureIdentifier)])
    
    rnw_pwr = cost.missing_vol(rnw_pwr, cts
                               , grep("PWR", msi_list, value = TRUE), term) # use massmarket costs if less than 12 months of usage
    
    rnw_pwr = cost.calc_cap_nits(rnw_pwr, term, msi_list, cts, state)
    
    rnw_pwr = cost.missing_tags(rnw_pwr, cts, msi_list, term, state) # use massmarket costs if missing tags
    
    rnw_pwr = cost.add_total(rnw_pwr, term, state)
    
    
    ## GAS
    
    rnw_gas = cost.gas_cts(rnw_gas, cts, msi_list, term) # just massmarket cts for gas
    
    ## Combine
    rnw_gas[, setdiff(colnames(rnw_pwr), colnames(rnw_gas)) := NA]
    
    rnw = rbind(rnw_pwr, rnw_gas)
    rm(rnw_pwr, rnw_gas)
    
  }
  
}

## clean up
rm(list = c(grep("^rq.", ls(), value = TRUE)
            , grep("^qry.", ls(), value = TRUE)
            , grep("^cost.", ls(), value = TRUE)))

### Summarize Costs #############################################################

source("Functions/Summarize.R")

## weighted average costs in list
summ_cust_wa = summ.costs(rnw[Commodity == "ELECTRIC"]
                          , term, c(
                                    #"VolCost"
                                    # , "CAPCost" 
                                    # , "NITSCost"
                                    # , "TECandARRCost"
                                     "TotalCTS"
                          ))

## weighted average cap and nits tags
copy.table(
  rnw[Commodity == "ELECTRIC"
      , .(CAP = weighted.mean(CAP, RCE, na.rm = TRUE)
          , NITS = weighted.mean(NITS, RCE, na.rm = TRUE))
      , by = "MarketStructureIdentifier"]
)

## massmarket costs, formatted
summ_cts = cts[grep("PWR", Mktstr)
               , .(MarketStructureIdentifier = Mktstr
                   , Term
                   # , MM.VolCost = VolCost
                   # , MM.CAPCost = CapacityCost
                   # , MM.NITSCost = NITSCost
                   # , MM.TECandARRCost = TECCost + ARRCost
                   , MM.TotalCTS = CTS)]

## merge and calculate delta

summ_compare = merge(summ_cust_wa, summ_cts, by = c("MarketStructureIdentifier", "Term"))

for (k in c(
  #   "VolCost"
  # , "CAPCost"
  # , "NITSCost"
  # , "TECandARRCost"
  "TotalCTS")) {
  summ_compare[, paste0("Delta.", k) := get(k)/get(paste0("MM.", k)) - 1]
}

copy.table(summ_compare)

# clean up
rm(list = grep("^summ", ls(), value = TRUE))

save.image(paste0("../Output/Renewals/RData/", csv_name, "_Costs_Calculated.RData"))


source("Functions/Pricing.R")
source("Ref/Renewal_Rules.R") # rules for assigning prices in each market

source("Price_Grouping/Price_Grouping_East.R") # east offer restrictions
source("Price_Grouping/Price_Grouping_IL_2.R") # IL bucketing


### Pricing Parameters ==========================================
## re-loads the saved file (in case you need to clear and re-price)
state = "IN"
csv_name = "IN_Renewals_Dec2018_missing"
load(paste0("../Output/Renewals/RData/", csv_name, "_Costs_Calculated.RData"))


m_flat1 = 300 # $/premise, yearly margin
m_flat2 = 310 # $/premise
m_flat3 = 320 # $/premise

m_fix1 = 260 # $/RCE
m_fix2 = 270 # $/RCE
m_fix3 = 280 # $/RCE
m_fix4 = 290 # $/RCE
m_fix5 = 290 # $/RCE

t_flat1 = 48L
t_flat2 = 36L
t_flat3 = 24L

t_fix1 = 48L
t_fix2 = 36L
t_fix3 = 24L
t_fix4 = 12L
t_fix5 = 6L


floor = 0.1 # price may be no lower than 10% below utility

# ==============================================================

rnw = pr.add_info(rnw, por_grt, utility_rate)

# JE_External has rates in $/Dth but I changed it to $/th here b/c volumes and prices in GBASS seem to be in $/th
rnw[MarketStructureIdentifier == "NJ_GAS_PSEG_ALL_ALL_ALL_R"
    , Utility_Rate := Utility_Rate/10] 

rnw = pr.margin(rnw, "Price", "Term", "ProductType", "Current") # margins on current products

## apply pricing rule based on state
## apply pricing rule based on state

rnw = rr.all(rnw, m_flat1,m_flat2,m_flat3, m_fix1, m_fix2, m_fix3, m_fix4, m_fix5
             , t_flat1,t_flat2,t_flat3, t_fix1, t_fix2, t_fix3, t_fix4,t_fix5, floor)


## Round each of the prices to nearest rate code restriction
# Utilities CPA, CentralHudson, NatGrid (NY), NFG
for (k in c("Fix1", "Fix2", "Fix3", "Fix4","Fix5", "Flat1", "Flat2", "Flat3")) {
  rnw = pr.round_ratecodes(rnw, k)
}


# rnw[, Price_Increase := round(Effortless/Price - 1, 2)] # check how much prices are increasing


### Margins #############################################################


## calculate margins

rnw = pr.margin_flatbill(rnw, "Flat1", "Flat1_Term", "Flat1")
rnw = pr.margin_flatbill(rnw, "Flat2", "Flat2_Term", "Flat2")
rnw = pr.margin_flatbill(rnw, "Flat3", "Flat3_Term", "Flat3")
rnw = pr.margin_fixed(rnw, "Fix1", "Fix1_Term", "Fix1")
rnw = pr.margin_fixed(rnw, "Fix2", "Fix2_Term", "Fix2")
rnw = pr.margin_fixed(rnw, "Fix3", "Fix3_Term", "Fix3")
rnw = pr.margin_fixed(rnw, "Fix4", "Fix4_Term", "Fix4")
rnw = pr.margin_fixed(rnw, "Fix5", "Fix5_Term", "Fix5")

## summarize margins

copy.table(
  rnw[!is.infinite(Flat1_Margin_Pr) & !is.infinite(Flat1_Margin_RCE)
      & !is.infinite(Flat2_Margin_Pr) & !is.infinite(Flat2_Margin_RCE)
      & !is.infinite(Flat3_Margin_Pr) & !is.infinite(Flat3_Margin_RCE)
      & !is.infinite(Fix1_Margin_Pr) & !is.infinite(Fix1_Margin_RCE)
      & !is.infinite(Fix2_Margin_Pr) & !is.infinite(Fix2_Margin_RCE)
      & !is.infinite(Fix3_Margin_Pr) & !is.infinite(Fix3_Margin_RCE)
      & !is.infinite(Fix4_Margin_Pr) & !is.infinite(Fix4_Margin_RCE)
      & !is.infinite(Fix5_Margin_Pr) & !is.infinite(Fix5_Margin_RCE)
      , .(Count = .N
          , Flat1.MP = ceiling(mean(Flat1_Margin_Pr, na.rm = TRUE))
          , Flat1.MRCE = ceiling(mean(Flat1_Margin_RCE, na.rm = TRUE))
          , Flat2.MP = ceiling(mean(Flat2_Margin_Pr, na.rm = TRUE))
          , Flat2.MRCE = ceiling(mean(Flat2_Margin_RCE, na.rm = TRUE))
          , Flat3.MP = ceiling(mean(Flat3_Margin_Pr, na.rm = TRUE))
          , Flat3.MRCE = ceiling(mean(Flat3_Margin_RCE, na.rm = TRUE))
          , Fix1.MP = ceiling(mean(Fix1_Margin_Pr, na.rm = TRUE))
          , Fix1.MRCE = ceiling(mean(Fix1_Margin_RCE, na.rm = TRUE))
          , Fix2.MP = ceiling(mean(Fix2_Margin_Pr, na.rm = TRUE))
          , Fix2.MRCE = ceiling(mean(Fix2_Margin_RCE, na.rm = TRUE))
          , Fix3.MP = ceiling(mean(Fix3_Margin_Pr, na.rm = TRUE))
          , Fix3.MRCE = ceiling(mean(Fix3_Margin_RCE, na.rm = TRUE))
          , Fix4.MP = ceiling(mean(Fix4_Margin_Pr, na.rm = TRUE))
          , Fix4.MRCE = ceiling(mean(Fix4_Margin_RCE, na.rm = TRUE))
          , Fix5.MP = ceiling(mean(Fix5_Margin_Pr, na.rm = TRUE))
          , Fix5.MRCE = ceiling(mean(Fix5_Margin_RCE, na.rm = TRUE))
      ), by = .(Commodity)][order(Commodity)]
)

## convert electricity fixed rate to $/kwh
rnw[Commodity == "ELECTRIC", Fix1 := Fix1 / 100] 
rnw[Commodity == "ELECTRIC", Fix2 := Fix2 / 100] 
rnw[Commodity == "ELECTRIC", Fix3 := Fix3 / 100] 
rnw[Commodity == "ELECTRIC", Fix4 := Fix4 / 100] 
rnw[Commodity == "ELECTRIC", Fix5 := Fix5 / 100] 


## copy full customer list to clipboard
copy.table(rnw)

## clean up
rm(list = c(grep("^pr.", ls(), value = TRUE)
            , grep("^rr.", ls(), value = TRUE)
            , grep("^pgeast.", ls(), value = TRUE)
            , grep("^pgil.", ls(), value = TRUE)))

## save priced file
save.image(paste0("../Output/Renewals/RData/", csv_name, "_Renewals_Priced.RData"))































# 
# ## margins
# # min margins if bucketing
# m_flat = 310 # $/premise
# m_fix1 = 310 # $/RCE
# m_fix2 = 260 # $/RCE
# m_save = 240 # $/RCE lower margin product
# 
# t_flat = 36L
# t_fix1 = 48L
# t_fix2 = 36L
# t_save = 24L
# 
# 
# # m_flat = 310 # $/premise
# # m_fix1 = 310 # $/RCE
# # m_fix2 = 260 # $/RCE
# # m_save = 240 # $/RCE lower margin product
# # 
# # t_flat = 24L
# # t_fix1 = 48L
# # t_fix2 = 24L
# # t_save = 24L
# 
# 
# 
# floor = 0.1 # price may be no lower than 10% below utility
# 
# # ==============================================================
# 
# rnw = pr.add_info(rnw, por_grt, utility_rate)
# 
# # JE_External has rates in $/Dth but I changed it to $/th here b/c volumes and prices in GBASS seem to be in $/th
# rnw[MarketStructureIdentifier == "NJ_GAS_PSEG_ALL_ALL_ALL_R"
#     , Utility_Rate := Utility_Rate/10] 
# 
# rnw = pr.margin(rnw, "Price", "Term", "ProductType", "Current") # margins on current products
# 
# ## apply pricing rule based on state
# 
# if(state == "OH") {
#   rnw = rr.oh(rnw, m_flat, m_fix1, m_fix2, t_flat, t_fix1, t_fix2, floor)
#   
# } else if (state == "IL") {
#   rnw = rr.il(rnw, m_flat, m_fix1, m_fix2, m_save, t_flat, t_fix1, t_fix2, t_save, floor)
#   
# } else if (state == "PA") {
#   rnw_pwr = rr.pa(rnw[Commodity=="ELECTRIC"], m_flat, m_fix1, m_fix2, t_flat, t_fix1, t_fix2, floor)
#   rnw_gas = rr.pa(rnw[Commodity=="GAS"], m_flat, 330, 320, t_flat, t_fix1, t_fix2, floor) # higher margins for gas
#   rnw = rbind(rnw_pwr, rnw_gas)
#   rm(rnw_pwr, rnw_gas)
# 
# } else if (state == "MD") {
#   rnw = rr.md(rnw, m_flat, m_fix1, m_fix2, t_flat, t_fix1, t_fix2, floor)
#   
# } else if (state == "DE") {
#   rnw = rr.de(rnw, m_fix1, m_fix2, t_fix1, t_fix2, floor)
#   
# } else if (state == "NJ") {
#   rnw = rr.nj(rnw, m_fix1, m_fix2, t_fix1, t_fix2, floor)
# 
# } else if (state == "MA") {
#   rnw = rr.ma(rnw, m_fix1, m_fix2, t_fix1, t_fix2, floor)
#   
# } else if (state == "NY") {
#   rnw = rr.ny(rnw, m_fix1, m_fix2, t_fix1, t_fix2, floor)
# }
# 
# 
# 
# # #### Set fixed rates higher than 15 c/kWh or 1 $/CCF or 1 $/Th as NA
# # 
# # if("Effortless" %in% colnames(rnw)) {
# #   rnw[(Commodity == "ELECTRIC" & Effortless_Type == "Fixed" & Effortless >= 15)
# #       | (Commodity == "GAS" & Unit %in% c("$/th", "$/CCF") & Effortless_Type == "Fixed" & Effortless >= 1)
# #       | (Commodity == "GAS" & Unit %in% c("$/Dth", "$/MCF") & Effortless_Type == "Fixed" & Effortless >= 10)
# #       , Effortless := NA]
# # }
# #   
# # rnw[(Commodity == "ELECTRIC" & Lead_Type == "Fixed" & Lead >= 15)
# #     | (Commodity == "GAS" & Unit %in% c("$/th", "$/CCF") & Lead_Type == "Fixed" & Lead >= 1)
# #     | (Commodity == "GAS" & Unit %in% c("$/Dth", "$/MCF") & Lead_Type == "Fixed" & Lead >= 10)
# #     , Lead := NA]
# # 
# # rnw[(Commodity == "ELECTRIC" & Backup_Type == "Fixed" & Backup >= 15)
# #     | (Commodity == "GAS" & Unit %in% c("$/th", "$/CCF") & Backup_Type == "Fixed" & Backup >= 1)
# #     | (Commodity == "GAS" & Unit %in% c("$/Dth", "$/MCF") & Backup_Type == "Fixed" & Backup >= 10)
# #     , Backup := NA]
# # 
# 
# 
# 
# ## Price grouping to reduce the number of offer combinations
# 
# # !!! Note: I didn't put the OH price grouping into a nice function. It'll need to be run separately.
# # File is in the Price_Grouping subfolder
# 
# if (state %in% c("PA", "MD", "NJ", "MA", "DE", "NY")) {
#   rnw = pgeast.reduce_offers(rnw)
# } else if (state == "IL") {
#   rnw = pgil.reduce_offers(rnw) # you may want to adjust the bucket sizes in the function
# }
# 
# 
# ## fix the case where effortless or lead is less than current price
# if("Effortless" %in% colnames(rnw)) {
#   rnw[Effortless < Price & Effortless_Type == ProductType
#       , Effortless := Price]
# } else {
#   rnw[Lead < Price & Lead_Type == ProductType
#       , Lead := Price]
# }
# 
# 
# ## other data checks
# 
# # same price given for all offers
# # give a slight discount
# if("Effortless" %in% colnames(rnw)) {
#   rnw[Lead == Backup & Backup == Effortless & Commodity == "ELECTRIC"
#       , `:=` (Lead = Effortless - 0.1, Backup = Effortless - 0.2)]
#   
#   rnw[Lead == Backup & Backup == Effortless & Commodity == "GAS"
#       , `:=` (Lead = Effortless - 0.01, Backup = Effortless - 0.02)]
# }
# 
# rnw[Lead == Backup & Commodity == "ELECTRIC"
#     , Backup := Lead - 0.1]
# 
# rnw[Lead == Backup & Commodity == "GAS"
#     , Backup := Lead - 0.01]
# 
# 
# ## Round each of the prices to nearest rate code restriction
# # Utilities CPA, CentralHudson, NatGrid (NY), NFG
# for (k in c("Lead", "Backup")) {
#   rnw = pr.round_ratecodes(rnw, k)
# }
# 
# 
# # rnw[, Price_Increase := round(Effortless/Price - 1, 2)] # check how much prices are increasing
# 
# 
# ### Margins #############################################################
# 
# 
# ## calculate margins
# 
# if ("Effortless" %in% colnames(rnw)) {
#   rnw = pr.margin(rnw, "Effortless", "Effortless_Term", "Effortless_Type", "Effortless")
# }
# rnw = pr.margin(rnw, "Lead", "Lead_Term", "Lead_Type", "Lead")
# rnw = pr.margin(rnw, "Backup", "Backup_Term", "Backup_Type", "Backup")
# 
# 
# 
# ## summarize margins
# 
# copy.table(
#   rnw[#!is.infinite(Effortless) & !is.infinite(Effortless_Margin_RCE)
#       , .(Count = .N
#           # , Effortless.MP = mean(Effortless_Margin_Pr, na.rm = TRUE)
#           # , Effortless.MRCE = mean(Effortless_Margin_RCE, na.rm = TRUE)
#           , Lead.MP = mean(Lead_Margin_Pr, na.rm = TRUE)
#           , Lead.MRCE = mean(Lead_Margin_RCE, na.rm = TRUE)
#           , Backup.MP = mean(Backup_Margin_Pr, na.rm = TRUE)
#           , Backup.MRCE = mean(Backup_Margin_RCE, na.rm = TRUE)
#   ), by = .(Commodity
#             # , Pricing_Rule
#            # , Effortless_Type
#             , Lead_Type
#             , Backup_Type
#             )][
#     order(Commodity
#           # , Pricing_Rule
#           #, Effortless_Type
#           , Lead_Type
#           , Backup_Type
#           )]
# )
# 
# # total summary
# copy.table(
#   rnw[Commodity == "ELECTRIC" #& ProductType == "Unlimited"
#     #!is.infinite(Effortless)
#       , .(Count = .N
#           # , Effortless.MP = mean(Effortless_Margin_Pr, na.rm = TRUE)
#           # , Effortless.MRCE = mean(Effortless_Margin_RCE, na.rm = TRUE)
#           , Lead.MP = mean(Lead_Margin_Pr, na.rm = TRUE)
#           , Lead.MRCE = mean(Lead_Margin_RCE, na.rm = TRUE)
#           , Backup.MP = mean(Backup_Margin_Pr, na.rm = TRUE)
#           , Backup.MRCE = mean(Backup_Margin_RCE, na.rm = TRUE)
#   )]
# )
# 
# 
# ## copy full customer list to clipboard
# copy.table(rnw)
# 
# ## clean up
# rm(list = c(grep("^pr.", ls(), value = TRUE)
#             , grep("^rr.", ls(), value = TRUE)
#             , grep("^pgeast.", ls(), value = TRUE)
#             , grep("^pgil.", ls(), value = TRUE)))
# 
# ## save priced file
# save.image(paste0("../Output/Renewals/RData/", csv_name, "_Renewals_Priced.RData"))
# 
# 
