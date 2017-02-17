#source different data


dirs = list(shifts = "I:/COF/COF/Analytics_and_Automation_Engineering/tlc2020/final_tables",
            shift_stats = "I:/COF/COF/Analytics_and_Automation_Engineering/tlc2020")

#shift data--------------------------------------------------------------------------------------------------------------------------
setwd(dirs$shifts)
shifts = fread("shifts_across_industries.csv", select = c("quarter_hour", "dayz", "mon_year", "N", "am_pm", "numeric_quarter_hour", "industry"))
shifts[,mon_year:=as.Date(paste0(mon_year, "-28"))]
#shifts =  shifts[sample(nrow(shifts), 10000),]


setwd(dirs$shift_stats)
#shift_statz = fread("shift_statistics.csv")

load("shift_statz.Rbin")
#shift_sum[,mon_year:=as.Date(paste0(mon_year, "-28"))]
