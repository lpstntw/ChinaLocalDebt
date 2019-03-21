ImportData2017 <- function(province) {
  if (is.character(province)){
    path <- str_c("data/public finance/2017/", province, " 2017.csv")
    out <- read_csv(path, skip = 3)
    names(out) <- c("variable", province)
    return(out)
  } else warning("province must be character")
}


public_finance_2017 <- ImportData2017("Anhui") %>%
  left_join(ImportData2017("Beijing")) %>%
  left_join(ImportData2017("Chongqing")) %>%
  left_join(ImportData2017("Fujian")) %>%
  left_join(ImportData2017("Gansu")) %>%
  left_join(ImportData2017("Guangdong")) %>%
  left_join(ImportData2017("Guangxi")) %>%
  left_join(ImportData2017("Guizhou")) %>%
  left_join(ImportData2017("Hainan")) %>%
  left_join(ImportData2017("Hebei")) %>%
  left_join(ImportData2017("Heilongjiang")) %>%
  left_join(ImportData2017("Henan")) %>%
  left_join(ImportData2017("Hubei")) %>%
  left_join(ImportData2017("Hunan")) %>%
  left_join(ImportData2017("Inner Mongolia")) %>%
  left_join(ImportData2017("Jiangsu")) %>%
  left_join(ImportData2017("Jiangxi")) %>%
  left_join(ImportData2017("Jilin")) %>%
  left_join(ImportData2017("Liaoning")) %>%
  left_join(ImportData2017("Ningxia")) %>%
  left_join(ImportData2017("Qinghai")) %>%
  left_join(ImportData2017("Shandong")) %>%
  left_join(ImportData2017("Shanghai")) %>%
  left_join(ImportData2017("Shaanxi")) %>%
  left_join(ImportData2017("Shanxi")) %>%
  left_join(ImportData2017("Sichuan")) %>%
  left_join(ImportData2017("Tianjin")) %>%
  left_join(ImportData2017("Tibet")) %>%
  left_join(ImportData2017("Xinjiang")) %>%
  left_join(ImportData2017("Yunnan")) %>%
  left_join(ImportData2017("Zhejiang"))

public_finance_2017 %<>%
  mutate(year = 2017) %>%
  gather(Anhui, Beijing, Chongqing, Fujian, Gansu, Guangdong, Guangxi, Guizhou, Hainan, Hebei,
         Heilongjiang, Henan, Hubei, Hunan, `Inner Mongolia`, Jiangsu, Jiangxi, Jilin, Liaoning,
         Ningxia, Qinghai, Shandong, Shanghai, Shaanxi, Shanxi, Sichuan, Tianjin, Tibet, Xinjiang,
         Yunnan, Zhejiang, key = "en_province", value = "num") %>%
  drop_na(num) %>%
  spread(variable, num)

public_finance_2017_tidy <- public_finance_2017 %>%
  rename(revenue_all = `Local Governments General Budgetary Revenue(100 million yuan)`,
         revenue_alltax = `Local Governments Revenue Tax Revenue(100 million yuan)`,
         revenue_allnontax = `Local Governments Revenue Non-tax Revenue(100 million yuan)`,
         revenue_lease = `Local Governments Revenue State-owned Resources(Assets) Paid-use Income`,
         revenue_vat = `Local Governments Revenue Domestic Value Added Tax(100 million yuan)`,
         revenue_incometax_corp = `Local Governments Revenue Corporate Income Tax(100 million yuan)`,
         revenue_incometax_ind = `Local Governments Revenue Individual Income Tax(100 million yuan)`,
         revenue_resourcetax = `Local Governments Revenue Resource Tax(100 million yuan)`,
         revenue_cityconstrtax = `Local Governments Revenue City Maintenance and Construction Tax(100 million yuan)`,
         revenue_house_tax = `Local Governments Revenue House Property Tax(100 million yuan)`,
         revenue_stamp_tax = `Local Governments Revenue Stamp Tax(100 million yuan)`,
         revenue_landuse_tax = `Local Governments Revenue Urban Land Use Tax(100 million yuan)`,
         revenue_landappr_tax = `Local Governments Revenue Land Appreciation Tax(100 million yuan)`,
         revenue_vehicleoper_tax = `Local Governments Revenue Tax on Vehicles and Boat Operation(100 million yuan)`,
         revenue_vehiclepurch_tax = `Local Governments Revenue Vehicle Purchase Tax(100 million yuan)`,
         revenue_deed_tax = `Local Governments Revenue Deed Tax(100 million yuan)`,
         revenue_tobacco_tax = `Local Governments Revenue Tobacco Leaf Tax`,
         revenue_other_tax = `Local Governments Revenue Other Tax Revenue(100 million yuan)`,
         revenue_specialprogram = `Local Governments Revenue Special Program Receipts(100 million yuan)`,
         revenue_adminfee = `Local Governments Revenue Charge of Administrative and Institutional Units(100 million yuan)`,
         revenue_penalty = `Local Governments Revenue Penalty Receipts(100 million yuan)`,
         revenue_capitalincome = `Local Governments Revenue State-owned Capital Income`,
         revenue_other_nontax = `Local Governments Revenue Other Non-tax Receipts(100 million yuan)`,
         spending_all = `Local Governments General Budgetary Expenditure(100 million yuan)`,
         spending_publicservice = `Local Governments Expenditure General Public Services(100 million yuan)`,
         spending_foreign = `Local Governments Expenditure Foreign Affairs(100 million yuan)`,
         spending_defense = `Local Governments Expenditure National Defense(100 million yuan)`,
         spending_security = `Local Governments Expenditure Public Security(100 million yuan)`,
         spending_education = `Local Governments Expenditure Education(100 million yuan)`,
         spending_RD = `Local Governments Expenditure Science and Technology(100 million yuan)`,
         spending_culture = `Local Governments Expenditure Culture Sport and Media(100 million yuan)`,
         spending_socialsafety = `Local Governments Expenditure Social Safety Net and Employment Effort(100 million yuan)`,
         spending_health = `Local Governments Expenditure Medical and Health Care(100 million yuan)`,
         spending_environment = `Local Governments Expenditure Environmental Protection(100 million yuan)`,
         spending_urbanaffairs = `Local Governments Expenditure Urban and Rural Community Affairs(100 million yuan)`,
         spending_agricultureland = `Local Governments Expenditure Agriculture forestry and Water Conservancy(100 million yuan)`,
         spending_transportation = `Local Governments Expenditure Transportation(100 million yuan)`,
         spending_regulation = `Local Governments Expenditure Affairs of Exploration Power and Information(100 million yuan)`,
         spending_commerce = `Local Governments Expenditure Affairs of Commerce and Services`,
         spending_financialregulation = `Local Governments Expenditure Financial regulation(100 million yuan)`,
         spending_landmanage = `Local Governments Expenditure Land Resources and Meteorology`,
         spending_housesecure = `Local Governments Expenditure Housing security`,
         spending_grain = `Local Governments Expenditure Affairs of Management of Grain and Oil Reserves(100 million yuan)`,
         spending_debtinterest = `Local Governments Expenditure Interest payment domestic and eign debts(100 million yuan)`,
         spending_others = `Local Governments Expenditure Other(100 million yuan)`) %>%
  mutate(province = recode(en_province, Anhui = "安徽省", Beijing = "北京市",
                           Chongqing = "重庆市", Fujian = "福建省", Gansu = "甘肃省", 
                           Guangdong = "广东省", Guangxi =  "广西壮族自治区",Guizhou = "贵州省",
                           Hainan = "海南省", Hebei = "河北省", Heilongjiang = "黑龙江省",
                           Henan = "河南省", Hubei = "湖北省", Hunan = "湖南省", 
                           `Inner Mongolia` = "内蒙古自治区", Jiangsu =  "江苏省",Jiangxi = "江西省",
                           Jilin = "吉林省", Liaoning = "辽宁省", Qinghai = "青海省",
                           Shaanxi = "陕西省", Shandong =  "山东省", Shanghai = "上海市",
                           Shanxi = "山西省", Sichuan = "四川省", Tianjin = "天津市",
                           Tibet = "西藏自治区", Xinjiang = "新疆维吾尔自治区", Yunnan = "云南省",
                           Ningxia = "宁夏回族自治区", Zhejiang = "浙江省"))
  
public_finance_all_tidy <- public_finance_all %>%
  rename(revenue_all = `本年收入合计`,
         revenue_alltax = `一、税收收入`,
         revenue_allnontax = `二、非税收入`,
         revenue_lease = `国有资源(资产)有偿使用收入`,
         revenue_vat = `增值税`,
         revenue_business_tax = `营业税`,
         revenue_incometax_corp = `企业所得税`,
         revenue_incometax_ind = `个人所得税`,
         revenue_resourcetax = `资源税`,
         revenue_cityconstrtax = `城市维护建设税`,
         revenue_house_tax = `房产税`,
         revenue_stamp_tax = `印花税`,
         revenue_landuse_tax = `城镇土地使用税`,
         revenue_landappr_tax = `土地增值税`,
         revenue_vehicleoper_tax = `车船税`,
         revenue_agriland_tax = `耕地占用税`,
         revenue_deed_tax = `契税`,
         revenue_tobacco_tax = `烟叶税`,
         revenue_other_tax = `其他税收收入`,
         revenue_specialprogram = `专项收入`,
         revenue_adminfee = `行政事业性收费收入`,
         revenue_penalty = `罚没收入`,
         revenue_capitalincome = `国有资本经营收入`,
         revenue_donation = `捐赠收入`,
         revenue_housefund = `政府住房基金收入`,
         revenue_other_nontax = `其他收入`,
         revenue_central_transfer = `中央补助收入`,
         revenue_debt_onbook = `地方政府一般债务收入`,
         revenue_centralbond_reissue = `国债转贷收入`,
         revenue_centralbond_lastyear = `国债转贷资金上年结余`,
         revenue_normalbond_lastyear = `待偿债置换一般债券上年结余`,
         revenue_residual_lastyear = `上年结余`,
         revenue_budgetfund_transfer = `调入预算稳定调节基金`,
         revenue_fund_transfer = `调入资金`,
         total_balance = `收入总计`,
         spending_all = `本年支出合计`,
         spending_publicservice = `一、一般公共服务支出`,
         spending_foreign = `二、外交支出`,
         spending_defense = `三、国防支出`,
         spending_security = `四、公共安全支出`,
         spending_education = `五、教育支出`,
         spending_RD = `六、科学技术支出`,
         spending_culture = `七、文化体育与传媒支出`,
         spending_socialsafety = `八、社会保障和就业支出`,
         spending_health = `九、医疗卫生与计划生育支出`,
         spending_environment = `十、节能环保支出`,
         spending_urbanaffairs = `十一、城乡社区支出`,
         spending_agricultureland = `十二、农林水支出`,
         spending_transportation = `十三、交通运输支出`,
         spending_regulation = `十四、资源勘探信息等支出`,
         spending_commerce = `十五、商业服务业等支出`,
         spending_financialregulation = `十六、金融支出`,
         spending_donation = `十七、援助其他地区支出`,
         spending_landmanage = `十八、国土海洋气象等支出`,
         spending_housesecure = `十九、住房保障支出`,
         spending_grain = `二十、粮油物资储备支出`,
         spending_debtinterest = `廿二、债务付息支出`,
         spending_others = `廿一、其他支出`,
         spending_debt_admin = `廿三、债务发行费用支出`,
         spending_central_transfer = `上解中央支出`,
         spending_budgetbuffer = `增设预算周转金`,
         spending_centralbond_reissuance = `拨付国债转贷资金数`,
         spending_centralbond_balance = `国债转贷资金结余`,
         spending_debtservice = `地方政府一般债务还本`,
         spending_normalbond_balance = `待偿债置换一般债券结余`,
         spending_budgetfund = `安排预算稳定调节基金`,
         spending_fund_transfer = `调出资金`,
         spending_specialdonation = `援助其他地区支出`,
         total_residual = `年终结余`) %>%
  mutate(year = as.numeric(year))
  
  
public_finance_2014_17_all_tidy <- bind_rows(public_finance_all_tidy, public_finance_2017_tidy) 
write_csv(public_finance_2014_17_all_tidy, path = "data/public finance/All Province 2014 - 17.csv")

  
saveRDS(public_finance_2014_17_all_tidy, file = "public finance from 2014 to 2017")
