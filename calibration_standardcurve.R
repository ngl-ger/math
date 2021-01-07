# Standard curve based on '[Type:Soil | Timepoint: T3] standard results

  standard_curve <- 
    read_delim("C:/Users/jjohn/OneDrive/MScthesis/data/qPCR/com_standard_curve.csv",",",)
  
  # Gettin an exacter model equation
  model <- lm(ct ~ qty_log10, data = standard_curve)
  model
  #Coefficients:
  #(Intercept)    qty_log10  
  #40.366       -3.648  


# Recalculating the quantity values based on the standard curve in '[Type:Soil | Timepoint: T3]

  # Load the data 

  qpcr <- 
    read_delim("C:/Users/jjohn/OneDrive/MScthesis/data/comm.csv",",",)
  
  std <- qpcr %>%
    filter(id == "Standard 2")%>%
    select(type,time,id,ct)%>%
    group_by(time,type)%>%
    get_summary_stats(ct, type = "mean_sd")
  
 
  
  
# We use the Standard 2 as an interrun calibrator and the Standard 2 of [Type:Soil | Timepoint: T3] is the 1.0 (100%)
  
  #Value is [Type:Soil | Timepoint: T3] 13.188
  
 std <- std %>%
    mutate(corr = mean - 13.188 )%>%
    select(time, type, corr)
 
 write_csv(std, "corr_factor.csv")
 
 qpcr <- 
   read_delim("C:/Users/jjohn/OneDrive/MScthesis/data/comm.csv",",",)%>%
   filter(id!="Standard 2")
 
 
 qpcr_corr <- qpcr %>% 
   inner_join(std, by = c("time" = "time", "type" = "type"))%>%
   mutate(ct_corr = ct + corr)%>%
   select(time,type,id,ct_corr)%>%
   mutate(qty= 10^((ct_corr-40.36)/-3.648))%>%
   group_by(time,type,id)%>%
   get_summary_stats(qty, type = "mean_sd")
 