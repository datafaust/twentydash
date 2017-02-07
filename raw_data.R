#source different data


dirs = list(shifts = "I:/COF/COF/Analytics_and_Automation_Engineering/tlc2020/final_tables")

#shift data--------------------------------------------------------------------------------------------------------------------------
setwd(dirs$shifts)
shifts = fread("shifts_across_industries.csv")
