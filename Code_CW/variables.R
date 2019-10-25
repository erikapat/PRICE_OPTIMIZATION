


c(premium, annual_revenue ,ratio_carrier_winning, total_payroll, ratio_wins_products, Number_year_established,                               
  avg_carriers_winning_premium, Carriers_winning_premium_per_sell,                      
  Number_wins_products, num_employees, Carriers_winning_bids, Number_bids_products,                                  
  Carriers_bids, sector_Food.and.Accommodation,                         
  sector_Professional..Scientific.and.Technical.Services, sector_Contractors,                                    
  sector_Consultants, max_carriers_winning_premium,                          
  sector_Other.Services, Region_SOUTH,                                          
  business_structure_Limited.Liability.Company, sector_other,                                          
  sd_carriers_winning_premium, Region_NORTHEAST,                                      
  sector_Retail.Trade, business_structure_Individual,                         
  Region_WEST, rng_carriers_winning_premium,                          
  sector_Manufacturing, sector_Healthcare,                                     
  sector_UNKNOWN, sector_Transportation.and.Warehousing,                 
  business_structure_Non.Profit, business_structure_Partnership,                        
  business_structure_Other, business_structure_Not.sure.yet,                       
  business_structure_Limited.Partnership, Region_UNKNOWN,                                        
  min_carriers_winning_premium, business_structure_Trust,                              
  business_structure_UNKNOWN )

#--------------------------------------------------------------------------------------------------------------------------


# dt.train  <- dt.train %>%  select(convert, premium, annual_revenue, avg_carriers_winning_premium, ratio_wins_products, Carriers_winning_premium_per_sell,
#                      total_payroll, Number_year_established, ratio_carrier_winning, sector_Contractors,
#                      sector_Professional..Scientific.and.Technical.Services, num_employees, sector_Consultants, Carriers_winning_bids,
#                      Region_SOUTH, sector_Food.and.Accommodation, Number_bids_products, 
#                      sector_other,
#                      business_structure_Limited.Liability.Company, Number_wins_products, sector_Other.Services, max_carriers_winning_premium,
#                      Carriers_bids, business_structure_Individual, Region_NORTHEAST, sector_Retail.Trade, sector_Manufacturing,
#                      sector_Healthcare, rng_carriers_winning_premium, business_structure_Partnership
#                      )
#                      
#                     
# dt.test  <- dt.test  %>%  select(convert, premium, annual_revenue, avg_carriers_winning_premium, ratio_wins_products, Carriers_winning_premium_per_sell,
#                      total_payroll, Number_year_established, ratio_carrier_winning, sector_Contractors,
#                      sector_Professional..Scientific.and.Technical.Services, num_employees, sector_Consultants, Carriers_winning_bids,
#                      Region_SOUTH, sector_Food.and.Accommodation, Number_bids_products, sector_other, 
#                      business_structure_Limited.Liability.Company, Number_wins_products, sector_Other.Services, max_carriers_winning_premium,
#                      Carriers_bids, business_structure_Individual, Region_NORTHEAST, sector_Retail.Trade, sector_Manufacturing,
#                      sector_Healthcare, rng_carriers_winning_premium, business_structure_Partnership
#                      )
#  