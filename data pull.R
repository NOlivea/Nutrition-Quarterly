# host <-  "172.27.1.99"
# dbname <- "dwh"
# user <- "nolivea"
# password <- "#sage"
# port <- 5432
# #Create a connection to the PostgreSQL database
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = dbname,
#                  host = host,
#                  port = port,
#                  user = user,
#                  password = password)
# query <- "
# SELECT 
#     region, 
#     district, 
#     sub_county,
#     dataelement,
#     value, 
#     category_combo,
#     year,
#     period
# FROM 
#     report.mv_hmis_summary
# WHERE  
#     dataelement IN (
#         '108-NA01a1. Total number of SAM admissions in ITC',
#         '108-NA01b1. Number of new SAM admissions in ITC',
#         '108-NA02a1. Number of SAM clients succesfully treated this month',
#         '108-NA02b1. Number of SAM clients who defaulted from care this month',
#         '108-NA02c1. Number of SAM clients who died during treatment this month',
#         '108-NA03a1. Number of readmissions into ITC this month-relapses',
#         '108-NA03b1. Number of readmissions into ITC this month-defaulters',
#         '105-NA01a1. Clients(<10) who received nutrition assessment in this month - MUAC Tape 0-6Mths, Male',
#         '105-NA01b1. Clients(<10) who received nutrition assessment in this month - W/H or L Z-Score',
#         '105-NA01c1. Clients(<10) who received nutrition assessment in this month - BMI for Age Z-Score',
#         '105-NA02a1. Children that are underweight (W/A â€šÃ¢Â§-2 SD)',
#         '105-NA03a1. Identified malnourished clients(<10) this month - MAM using MUAC',
#         '105-NA03b1. Identified malnourished clients(<10) this month - MAM using W/H or L',
#         '105-NA03c1. Identified malnourished clients(<10) this month - SAM using MUAC - Without Oedema',
#         '105-NA03e1. Identified malnourished clients(<10) this month - SAM With Oedema',
#         '105-NA03f1. Identified malnourished clients (<10) this month - Moderately Malnourished',
#         '105-NA03g1. Identified malnourished clients(<10) this month - Severely Malnourished',
#         '105-NA03h1. Identified malnourished clients(<10) this month - Overweight',
#         '105-NA03i1. Identified malnourished clients(<10) this month - Obese',
#         '105-NA04a1. SAM admissions(<10) in OTC this month - Old',
#         '105-NA04b1. SAM admissions(<10) in OTC this month - New using MUAC',
#         '105-NA04c1. SAM admissions(<10) in OTC this month - New using W/H or L Z-score',
#         '105-NA05a1. Readmissions(<10) into OTC this month - Relapses',
#         '105-NA05b1. Readmissions(<10) into OTC this month - Defaulters',
#         '105-NA06a1. MAM admissions(<10) in Supplementary Feeding Care (SFC) - Old',
#         '105-NA06b1. MAM admissions(<10) in Supplementary Feeding Care (SFC) - New using MUAC',
#         '105-NA07a1. Readmissions(<10) in SFC this Month - Relapses',
#         '105-NA07b1. Readmissions(<10) in SFC this Month - Defaulters',
#         '105-NA08a1. Clients(<10) with SAM admitted into treatment and discharged as cured within this month',
#         '105-NA08b1. Clients(<10) with SAM admitted into treatment - OTC (non-response) after 3 months',
#         '105-NA08c1. Clients(<10) with SAM admitted into treatment - Defaulted from care within this month',
#         '105-NA08d1. Clients(<10) with SAM admitted into treatment - Died during treatment',
#         '105-NA09a1. Total number of days spent in OTC for all clients(<10) discharged this month',
#         '105-NA10a1. No. of clients(<10) with MAM admitted into treatment - Discharged as cured',
#         '105-NA10b1. No. of clients(<10) with MAM admitted into treatment - SFC(non-response) after 3 months',
#         '105-NA10c1. No. of clients(<10) with MAM admitted into treatment - Defaulted from care',
#         '105-NA10d1. No. of clients(<10) with MAM admitted into treatment - Died during treatment',
#         '105-NA11a1. Total number of days spent in SFC for all clients(<10) discharged this month',
#         '105-NA12a1. No. of malnourished clients(<10) who are positive for TB',
#         '105-OA01. New attendance',
#         '105-OA02. Re-attendance',
#         '105-MA07. Low birth weight babies initiated on kangaroo (KMC) default',
#         '105-MA05c. Pre-Term births - <2.5 Kgs default',
#         '105-MA05b. Pre-Term births - Alive default',
#         '105-MA04b1. Deliveries in unit -Live births - Total default',
#         '105-EC11. Vitamin A Deficiency',
#         '105-CH03. Deworming (1st Dose)',
#         '105-CH04. Deworming (2nd Dose)',
#         '105-CH01. Vit A supplement (1st Dose)',
#         '105-CH02. Vit A supplement (2nd Dose)',
#         '105-AN21. Pregnant Women receiving atleast 30 tablets of Iron/Folic Acid at ANC 1st contact / Visit default',
#         '105-AN01a. ANC 1st Visit for women',
#         '105-MA14a. Mothers who initiated breastfeeding within the 1st hour after delivery - Total default',
#         '105-AN17. Pregnant women who were tested for Anaemia using Hb Test at ANC 1st contact / visit default',
#         '105-AN18. Pregnant women with Hb <10g/dl at ANC 1st contact / visit default'
#     )
# AND 
#     year::text ~ '2021|2022|2023|2024|2025';
# 
# "  
# 
# dwh_nut <- dbGetQuery(con, query)

#library(rio) 


dwh_nut <- import('mv_hmis_summary_202412231446.csv')


dwh_nut <- dwh_nut |>
  mutate(value = as.numeric(value))  |>
  pivot_wider(names_from = "dataelement", values_from = "value", , values_fn = sum)


nutrition <-  dwh_nut |> 
  rename( Category                        = "category_combo",
          Period                          = "period",
          n_attnd                         = "105-OA01. New attendance" ,
          VitA1                           = "105-CH01. Vit A supplement (1st Dose)",
          VitA_deficiency                 = "105-EC11. Vitamin A Deficiency",
          SCFdischarged                   = "105-NA11a1. Total number of days spent in SFC for all clients(<10) discharged this month",
          Deworming2                      = "105-CH04. Deworming (2nd Dose)",
          Deworming1                      = "105-CH03. Deworming (1st Dose)",
          VitA2                           = "105-CH02. Vit A supplement (2nd Dose)",
          SAMcured                        = "105-NA08a1. Clients(<10) with SAM admitted into treatment and discharged as cured within this month",
          reattnd                         = "105-OA02. Re-attendance",
          SAM_identified                  = "105-NA03g1. Identified malnourished clients(<10) this month - Severely Malnourished" ,
          MAM_identified_MAUC             = "105-NA03a1. Identified malnourished clients(<10) this month - MAM using MUAC",
          MAM_identified                  =  "105-NA03f1. Identified malnourished clients (<10) this month - Moderately Malnourished", 
          ANC1                            = "105-AN01a. ANC 1st Visit for women",
          Overweight_mal                  = "105-NA03h1. Identified malnourished clients(<10) this month - Overweight",
          SFCreadmissions                 = "105-NA07a1. Readmissions(<10) in SFC this Month - Relapses",
          OTCreadmissions                 = "105-NA05a1. Readmissions(<10) into OTC this month - Relapses",
          Overweight_obes                 = "105-NA03i1. Identified malnourished clients(<10) this month - Obese",
          MAMadmissionSFC                 = "105-NA06a1. MAM admissions(<10) in Supplementary Feeding Care (SFC) - Old",
          nut_ass_WH                      = "105-NA01b1. Clients(<10) who received nutrition assessment in this month - W/H or L Z-Score",
          SAM_Oedema                      = "105-NA03e1. Identified malnourished clients(<10) this month - SAM With Oedema",
          SAM_admission_OTC_WH            = "105-NA04c1. SAM admissions(<10) in OTC this month - New using W/H or L Z-score" ,
          SAM_admission_OTC_old           = "105-NA04a1. SAM admissions(<10) in OTC this month - Old",
          SAM_admission_muac_new          = "105-NA04b1. SAM admissions(<10) in OTC this month - New using MUAC",
          SAM_defaulters_OTC              = "105-NA08c1. Clients(<10) with SAM admitted into treatment - Defaulted from care within this month",
          SAM_died_OTC                    = "105-NA08d1. Clients(<10) with SAM admitted into treatment - Died during treatment",
          MAM_WH                          = "105-NA03b1. Identified malnourished clients(<10) this month - MAM using W/H or L",
          MAM_defaulters                  = "105-NA10c1. No. of clients(<10) with MAM admitted into treatment - Defaulted from care",
          MAM_died                        = "105-NA10d1. No. of clients(<10) with MAM admitted into treatment - Died during treatment",
          OTC_defaulters                  = "105-NA05b1. Readmissions(<10) into OTC this month - Defaulters",
          OTC_discharged                  = "105-NA09a1. Total number of days spent in OTC for all clients(<10) discharged this month",
          SFC_defaulters                  = "105-NA07b1. Readmissions(<10) in SFC this Month - Defaulters",
          SFC_nonresponse                 = "105-NA10b1. No. of clients(<10) with MAM admitted into treatment - SFC(non-response) after 3 months",
          TB_malnourished                 = "105-NA12a1. No. of malnourished clients(<10) who are positive for TB",
          nut_ass_BMI_age                 = "105-NA01c1. Clients(<10) who received nutrition assessment in this month - BMI for Age Z-Score",
          MAM_cured                       = "105-NA10a1. No. of clients(<10) with MAM admitted into treatment - Discharged as cured",
          MAM_admissionSFC_new            = "105-NA06b1. MAM admissions(<10) in Supplementary Feeding Care (SFC) - New using MUAC",
          SAM_admission_ITC               = "108-NA01a1. Total number of SAM admissions in ITC",
          SAM_admission_ITC_new           = "108-NA01b1. Number of new SAM admissions in ITC",
          SAM_defaulters_ITC              = "108-NA02b1. Number of SAM clients who defaulted from care this month",
          ITC_relapse                     = "108-NA03a1. Number of readmissions into ITC this month-relapses",
          ITC_defaulters                  = "108-NA03b1. Number of readmissions into ITC this month-defaulters",
          SAM_died_ITC                    = "108-NA02c1. Number of SAM clients who died during treatment this month",
          SAM_succesfullytreated_ITC      = "108-NA02a1. Number of SAM clients succesfully treated this month" )


nutrition <- change_to_date(nutrition) 

nutrition <- nutrition |> 
  mutate(
    year = as.character(year),
    month = as.numeric(month),
    month_year = format(as.Date(date), "%b %Y"),
    month = month(date),
    month_abbrev = month.abb[month],
    year = year(date))


pop <- read.csv('data/pop.csv') |>
  mutate(Districts = trimws(Districts))



dist_pop <- pop |>
  filter(Districts != "") |>
  mutate(across(Male_2015:Total_2030, ~ str_replace_all(.,",","")),
         across(Male_2015:Total_2030, ~ as.numeric(.))) |>
  pivot_longer(cols=Male_2015:Total_2030, names_to = "popln", values_to = "value")  |>
  separate("popln", c("variable", "year"),sep = "_") |>
  pivot_wider(names_from = variable,
              values_from = value) |>
  select(-LG) |>
  filter(year %in%c("2021", "2022","2023","2024", "2025"))



nutrition$district <- sub(" District", "", nutrition$district)
nutrition$district = toupper(trimws(nutrition$district))
dist_pop$Districts = toupper(trimws(dist_pop$Districts))

nutrition$district <- ifelse(nutrition$district == "MADI-OKOLLO", "MADI OKOLLO", nutrition$district)
nutrition$district <- ifelse(nutrition$district == "KAMPALA", "KAMPALA CITY", nutrition$district)
nutrition$district <- ifelse(nutrition$district == "SEMBABULE", "SSEMBABULE", nutrition$district)

dist_pop$Districts <- ifelse(dist_pop$Districts == "KAMPALA CITY AUTHORITY", "KAMPALA CITY", dist_pop$Districts)

nutrition_joined <- merge(dist_pop, nutrition, by.x = c("year", "Districts"), by.y = c("year", "district"))

# Save data

nutrition_joined <- nutrition_joined |>
  rename(district = Districts)



nutrition_joined <- nutrition_joined |>
  mutate(
    month = month(date, label = TRUE), 
    quarter = case_when(
      month %in% c("Jan", "Feb", "Mar") ~ paste0("Jan to Mar ", year),
      month %in% c("Apr", "May", "Jun") ~ paste0("Apr to Jun ", year),
      month %in% c("Jul", "Aug", "Sep") ~ paste0("Jul to Sep ", year),
      month %in% c("Oct", "Nov", "Dec") ~ paste0("Oct to Dec ", year),
      TRUE ~ NA_character_  # Ensure any non-recognized month results in NA
    )) |> 
  select(-Period)


save(nutrition_joined, file = "nutrition_joined.RData")

