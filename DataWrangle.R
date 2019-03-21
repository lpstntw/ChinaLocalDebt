FinancialVehicleDebt <- read_excel("data/LocalGovernmentFinancialVehicleWIND.xlsx", 
                                   col_types = c("text", "text", "text", 
                                                 "text", "numeric", "numeric", "numeric", 
                                                 "numeric", "date", "numeric", 
                                                 "numeric", "blank", "text", "text", 
                                                 "text", "text", "blank", "blank", 
                                                 "blank", "blank", "blank", "blank", "blank", "blank", 
                                                 "blank", "blank", "blank", "blank", "blank", "blank", 
                                                 "blank", "blank", "blank", "blank", "blank", "blank", 
                                                 "blank"), skip = 1)

GovernmentDebt <- read_excel("data/LocalGovernmentDebt.xlsx", 
                             col_types = c("text", "text", "text", 
                                           "text", "numeric", "numeric", "numeric", 
                                           "numeric", "date", "numeric", "numeric", 
                                           "text", "text", "blank", "blank"), 
                             skip = 1)

FinancialVehicleDebt %<>%
  distinct(fullname, issueamount, issuerupdated, .keep_all = TRUE) %>%
  mutate(term = round(term*365/30, digits = 0)) %>%
  mutate(issue_date = maturitydate - months(term)) %>%
  mutate(issue_year = year(issue_date))

GovernmentDebt %<>%
  distinct(fullname, issueamount, issuerupdated, .keep_all = TRUE) %>%
  mutate(issue_date = maturitydate - years(term)) %>%
  mutate(issue_year = year(issue_date))


LGFV_industry <- FinancialVehicleDebt %>%
  filter(!is.na(abs_province) & !is.na(issue_year) & !is.na(abs_industry) == TRUE) %>%
  group_by(abs_province, issue_year, abs_industry) %>%
  summarise(FVdebt_issuance = sum(outstandingbalance, na.rm = TRUE))

LGFV_province <- FinancialVehicleDebt %>%
  filter(!is.na(abs_province) & !is.na(issue_year) == TRUE) %>%
  group_by(abs_province, issue_year) %>%
  summarise(FVdebt_issuance = sum(outstandingbalance, na.rm = TRUE))

GovernmentDebt_province <- GovernmentDebt %>%
  filter(!is.na(abs_province) & !is.na(issue_year) == TRUE) %>%
  group_by(abs_province, issue_year) %>%
  summarise(GOVdebt_issuance = sum(outstandingbalance, na.rm = TRUE))

LocalDebt_full <- full_join(LGFV_province, GovernmentDebt_province, by = c("abs_province", "issue_year"))

LocalDebt_full %<>%
  ungroup() %>%
  mutate(abs_province = recode(abs_province, 北京 = "北京市", 重庆 = "重庆市", 上海 = "上海市", 
                               天津 = "天津市")) %>%
  mutate(en_province = recode(abs_province, 安徽省 = "Anhui", 北京市 = "Beijing",
                              重庆市 = "Chongqing", 福建省 = "Fujian", 甘肃省 = "Gansu", 
                              广东省 = "Guangdong", 广西壮族自治区 = "Guangxi", 贵州省 = "Guizhou",
                              海南省 = "Hainan", 河北省 = "Hebei", 黑龙江省 = "Heilongjiang",
                              河南省 = "Henan", 湖北省 = "Hubei", 湖南省 = "Hunan", 
                              内蒙古自治区 = "Inner Mongolia", 江苏省 = "Jiangsu", 江西省 = "Jiangxi",
                              吉林省 = "Jilin", 辽宁省 = "Liaoning", 青海省 = "Qinghai",
                              陕西省 = "Shaanxi", 山东省 = "Shandong", 上海市 = "Shanghai",
                              山西省 = "Shanxi", 四川省 = "Sichuan", 天津市 = "Tianjin",
                              西藏自治区 = "Tibet", 新疆维吾尔自治区 = "Xinjiang", 云南省 = "Yunnan",
                              宁夏回族自治区 = "Ningxia", 浙江省 = "Zhejiang")) %>%
  mutate(region = case_when(
    abs_province %in% c("黑龙江省","吉林省","辽宁省") ~ "northeast",
    abs_province %in% c("山西省", "河南省", "湖北省", "湖南省", "江西省", "安徽省") ~ "central",
    abs_province %in% c("北京市", "天津市", "河北省", "山东省", "江苏省", "上海市", 
                        "浙江省", "福建省", "广东省", "海南省") ~ "east",
    abs_province %in% c("重庆市", "四川省", "广西壮族自治区", "贵州省", "云南省", "陕西省", "甘肃省", 
                        "内蒙古自治区", "宁夏回族自治区", "新疆维吾尔自治区", 
                        "青海省", "西藏自治区") ~ "west"))


GDP_by_province_1999_2017 <- read_csv("data/public finance/GDP by province 1999-2017.csv", 
                                      col_types = cols( `2018` = col_skip()), skip = 3)

GDP <- GDP_by_province_1999_2017 %>%
  select(Region, `2017`, `2016`, `2015`, `2014`, `2013`, `2012`) %>%
  gather(`2017`, `2016`, `2015`, `2014`, `2013`, `2012`, key = "year", value = "GDP") %>%
  drop_na(GDP) %>%
  mutate(year = as.numeric(year))

public_finance_all <- read_excel("data/public finance/Anhui 2014-2016.xlsx", 
                                 col_types = c("text", "text", "numeric", "numeric", "numeric")) %>%
  bind_rows((read_excel("data/public finance/Beijing 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Chongqing 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Fujian 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Gansu 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Guangdong 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Guangxi 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Guizhou 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Hainan 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Hebei 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Heilongjiang 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Henan 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Hubei 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Hunan 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/InnerMongolia 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Jiangsu 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Jiangxi 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Jilin 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Liaoning 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Ningxia 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Qinghai 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Shandong 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Shanghai 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/shaanxi 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Shanxi 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Sichuan 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Tianjing 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Tibet 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Xinjiang 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Yunnan 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric")))) %>%
  bind_rows((read_excel("data/public finance/Zhejiang 2014 - 2016.xlsx", 
                        col_types = c("text", "text", "numeric", "numeric", "numeric"))))

public_finance_all %<>%
  gather(`2014`, `2015`, `2016`, key = "year", value = "num") %>%
  spread(variable, num) %>%
  mutate(en_province = recode(province, 安徽省 = "Anhui", 北京市 = "Beijing",
                              重庆市 = "Chongqing", 福建省 = "Fujian", 甘肃省 = "Gansu", 
                              广东省 = "Guangdong", 广西壮族自治区 = "Guangxi", 贵州省 = "Guizhou",
                              海南省 = "Hainan", 河北省 = "Hebei", 黑龙江省 = "Heilongjiang",
                              河南省 = "Henan", 湖北省 = "Hubei", 湖南省 = "Hunan", 
                              内蒙古自治区 = "Inner Mongolia", 江苏省 = "Jiangsu", 江西省 = "Jiangxi",
                              吉林省 = "Jilin", 辽宁省 = "Liaoning", 青海省 = "Qinghai",
                              陕西省 = "Shaanxi", 山东省 = "Shandong", 上海市 = "Shanghai",
                              山西省 = "Shanxi", 四川省 = "Sichuan", 天津市 = "Tianjin",
                              西藏自治区 = "Tibet", 新疆维吾尔自治区 = "Xinjiang", 云南省 = "Yunnan",
                              宁夏回族自治区 = "Ningxia",浙江省 = "Zhejiang"))

source("2017PublicFinanceDataImport.R")

public_finance <- public_finance_2014_17_all_tidy %>%
  select(province, en_province, year, revenue_all, revenue_alltax, revenue_allnontax, 
         revenue_vat, revenue_business_tax, revenue_lease, revenue_landuse_tax, 
         revenue_landappr_tax, revenue_central_transfer, revenue_debt_onbook,
         spending_all, spending_debtservice, total_balance) %>%
  mutate(year_lag = year + 1)
population_full <- read_csv("data/Population by province.csv", 
                            col_types = cols(`2018` = col_skip()), skip = 3)

population <- population_full %>%
  select(Region, `2017`, `2016`, `2015`, `2014`, `2013`, `2012`) %>%
  gather(`2017`, `2016`, `2015`, `2014`, `2013`, `2012`, key = "year", value = "population") %>%
  drop_na(population) %>%
  mutate(year = as.numeric(year))


Debt_finance_GDP_population_full <- LocalDebt_full %>%
  left_join(public_finance, 
            by = c("abs_province" = "province", "issue_year" = "year", "en_province" = "en_province")) %>%
  left_join(GDP, by = c("en_province" = "Region", "issue_year" = "year")) %>%
  left_join(population, by = c("en_province" = "Region", "issue_year" = "year")) %>%
  rename(province = abs_province, year = issue_year)