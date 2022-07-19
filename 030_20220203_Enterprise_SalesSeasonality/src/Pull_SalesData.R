#Pull in sales data for seaonality report
# Thu Feb 03 13:29:16 2022 ------------------------------


library(DBI)
library(glue)
library(tidyverse)
library(lubridate)

# ----- Sales Report Date

Today <- Sys.Date()
# Today <- as.Date("2022-01-23")

RPTDate <- 
  if(as.numeric(lubridate::day(Today)) < 20){
    Today %m-% months(2) %>% lubridate::ceiling_date(., "month")-1
  } else {
    # statement(s) will execute if the boolean expression is false.
    Today %m-% months(1) %>% lubridate::ceiling_date(., "month")-1
  }


DatYear <- lubridate::year(RPTDate)

# --------------------
warehouse <- DBI::dbConnect(odbc::odbc(), dsn = "WAREHOUSESQL")

pull_sql <- glue_sql(
  "
select dimd.fulldate, dimc.CENTERNAME, dimc.BldgGroupName, dimc.[BLDGID], dimt.TangerCatDesc, dimt.TangerSubCatDesc, diml.TenantName, diml.ChainName, diml.leasid 
,dime.PORTID,fas.ISTEMP, fas.IsCurrentlyOpen,fas.ISNONREPORTING, fas.IsOpenForFullYear, diml.zPopUpProgram, lastlsf, [CurrentYearSales1M],
cast(iif(lastlsf = 0 or lastlsf is null, 0, [CurrentYearSales1M]/lastlsf) as money) as AdjustSPSF
from warehouse.dbo.FactAdjustedSales fas 
	left join warehouse.dbo.DimLease diml on fas.LeaseKey = diml.LeaseKey
	left join warehouse.dbo.DimCenter dimc on fas.CenterKey = dimc.CenterKey
	left join warehouse.dbo.DimTenant dimt on fas.TenantKey = dimt.TenantKey
	left join warehouse.dbo.DimEntity dime on fas.EntityKey = dime.EntityKey
	left join warehouse.dbo.DimDate dimd on fas.dateKey = dimd.DateKey
where fas.DateKey > 20161231
--and lastlsf > 0
order by CENTERNAME, fas.datekey desc, tenantname
  ",
.con = warehouse)

parm_sql <- dbSendQuery(warehouse, pull_sql)
Sales_center <- dbFetch(parm_sql)

dbClearResult(parm_sql)

#------ Clean sales -----

unique(Sales_center$BLDGID)

CleanSales <- 
  Sales_center %>% 
  mutate(fulldate = as.Date(fulldate),
         Year = lubridate::year(fulldate)) %>% 
  rename(GrossSales = CurrentYearSales1M,
         Date = fulldate) %>% 
  filter(Date <= RPTDate,
         !BLDGID %in% c("BROMO1", "WILLI1", "OCEAN1", "PARKC1", "TERRE1", "STSAU1", "JEFFE1", "CHLTT1", "NAGSH1", "WBROO1", "BARST1",
                        "KITTE1", "KITTE2", "KITTE3", "OCEAN1", "TUSCO1", "WEBRA1", "WILLI1", "WISCO1", "SANIB1")) %>%
  mutate(across(where(is.character), str_trim)) %>% 
  # select(-BLDGID) %>%
  mutate(All = "Portfolio")


UnqCenters <-
  CleanSales %>%
  distinct(BldgGroupName)

#--- Create aggregate ----------

#--- By Center ---
AggeCenters <- 
  CleanSales %>% 
  group_by(All, BldgGroupName, Year, Date) %>% 
  summarise(TotalSales = sum(GrossSales, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(Year, BldgGroupName) %>% 
  mutate(PercYear = TotalSales / sum(TotalSales)) %>% 
  ungroup()


UnqCenters <-
  AggeCenters %>%
  distinct(BldgGroupName)

#--- Portfolio --

Portf <-
  AggeCenters %>% 
  group_by(All, Year, Date) %>% 
  summarise(TotalSales = sum(TotalSales, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(Year, All) %>% 
  mutate(PercYear = TotalSales / sum(TotalSales)) %>% 
  ungroup()

#------------

save(Portf, AggeCenters,
     file = here::here("data", "SalesPerc.rds"))



