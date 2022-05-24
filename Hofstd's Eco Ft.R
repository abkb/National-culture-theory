library(readxl)
hfdt= read_excel("nefba_all_ed2022.xlsx", sheet = "national_data")
head(hfdt)
View(hfdt)
library(dplyr)

hfdt_sub= hfdt%>% filter ( year>2007,
                           country %in% c("Albania", "Algeria", "Andorra", "Argentina",
                                      "Australia",
                                      "Austria",
                                      "Azerbaijan",
                                      "Bangladesh",
                                      "Belarus",
                                      "Belgium",
                                      "Bosnia and Herzegovina",
                                      "Brazil",
                                      "Bulgaria",
                                      "Burkina Faso",
                                      "Canada",
                                      "Chile",
                                      "China",
                                      "Colombia",
                                      "Costa Rica",
                                      "Cyprus",
                                      "Czech Republic",
                                      "Denmark",
                                      "Dominican Republic",
                                      "Ecuador",
                                      "Egypt",
                                      "El Salvador",
                                      "Estonia",
                                      "Ethiopia",
                                      "Finland",
                                      "France",
                                      "Georgia",
                                      "Germany",
                                      "Ghana",
                                      "Greece",
                                      "Guatemala",
                                      "Hungary",
                                      "Hungary",
                                      "Iceland",
                                      "India",
                                      "Indonesia",
                                      "Iran Islamic Republic of",
                                      "Iraq",
                                      "Ireland",
                                      "Israel",
                                      "Italy",
                                      "Jamaica",
                                      "Japan",
                                      "Jordan",
                                      "Korea Republic of",
                                      "Kyrgyzstan",
                                      "Latvia",
                                      "Lithuania",
                                      "Luxembourg",
                                      "Malaysia",
                                      "Mali",
                                      "Malta",
                                      "Mexico",
                                      "Montenegro",
                                      "Morocco",
                                      "Netherlands",
                                      "New Zealand",
                                      "Nigeria",
                                      "Norway",
                                      "Pakistan",
                                      "Panama",
                                      "Peru",
                                      "Philippines",
                                      "Poland",
                                      "Portugal",
                                      "Puerto Rico",
                                      "Republic of Moldova",
                                      "Republic of North Macedonia",
                                      "Romania",
                                      "Russian Federation",
                                      "Rwanda",
                                      "Saudi Arabia",
                                      "Serbia",
                                      "Singapore",
                                      "Slovakia",
                                      "Slovenia",
                                      "South Africa",
                                      "Spain",
                                      "Suriname",
                                      "Suriname",
                                      "Sweden",
                                      "Switzerland",
                                      "Tanzania United Republic of",
                                      "Thailand",
                                      "Trinidad and Tobago",
                                      "Turkey",
                                      "Uganda",
                                      "Ukraine",
                                      "United Kingdom",
                                      "United States of America",
                                      "Uruguay",
                                      "Venezuela Bolivarian Republic of",
                                      "Viet Nam",
                                      "Zambia",
                                      "Zimbabwe"))


table(hfdt_sub$country)
table(hfdt_sub$year)
View(hfdt_sub)
# model 
library(plm)
ec_modll=plm (efc_total_gha~ area_grazing_ha+  pdi+ mas+ ivr+ idv+ ltowvs+ uai, 
              model = "random", data = hfdt_sub, index = c("country", "year"))
summary(ec_modll)

ec_modlq=plm (efc_total_gha~ area_forest_ha+ pdi+ mas+ ivr+ idv+ ltowvs+ uai, 
              model = "within", data = hfdt_sub, index = c("country", "year"))
summary(ec_modlq)

#hausman test for FE or RE model selection 
phtest(ec_modll, ec_modlq)


ec_modllw=plm ( efc_total_gha ~  hfdt_sub$area_crop_ha + pdi+ mas+ ivr+ idv+ ltowvs+ uai, 
              model = "random", data = hfdt_sub, index = c("country", "year"))
summary(ec_modllw)

########################    it ran with both country and year   #####################
q= plm(formula = efc_total_gha ~ hfdt_sub$area_crop_ha + hfdt_sub$pdi, 
    data = hfdt_sub, model = "random", index = c("country", "year"))
########################             $$$$$$$$$$$$            #####################


# model with panel data frame

pnl.dt= pdata.frame(hfdt_sub, index = c("country", "year"))
View(pnl.dt)
ec_modll=plm (efc_total_gha~ pdi+ mas+ ivr+ idv+ ltowvs+ uai, 
              model = 'random', data = pnl.dt)
summary(ec_modll)


# Package
library("pcse")
options(digits=3)

# Load the data.
data("agl")

ec_mod= lm (efi_total_gha ~ hfdt_sub$area_forest_ha+ pdi+ mas+ ivr+ idv+ ltowvs+ uai, data = hfdt_sub)
summary(ec_mod)

ec.pcse= pcse(ec_mod, groupN = hfdt_sub$country, groupT= hfdt_sub$year)

NROW(hfdt_sub$year)
table(hfdt_sub$country)
NROW(hfdt_sub$country)
# OLS Estimation for a model of growth in OECD countries

agl.lm <- lm(growth ~ lagg1 + opengdp + openex + openimp + central + leftc +
               inter + as.factor(year), data=agl)
summary(agl.lm)

# Estimate Panel-Corrected Standard-Errors and summarize the results.

agl.pcse <- pcse(agl.lm, groupN=agl$country, groupT=agl$year)
summary(agl.pcse)





phtest(modelec, modeler)
lnefc= log(hfdt$efc_total_gha+1)
lni=log(hfdt$idv+1) 
lniv=log(hfdt$ivr+1) 
lnlt=log(hfdt$ltowvs+1) 
lnu=log(hfdt$uai+1)
lnm= log(hfdt$mas+1)
lnp= log(hfdt$pdi+1)



hfdtc= cbind(hfdt, lnefc, lni, lniv, lnlt, lnu, lnm, lnp)
head(hfdtc)
ecol= plm(hfdtc$efc_total_gha~ hfdtc$idv,+ hfdtc$uai, data = hfdtc, 
            model = "random", index = c("country_name", "year"))
summary(ecol)


phtest(model2, model3)
hfdt$ivr
table(hfdt$ivr)


which( hfdt$ivr==0, arr.ind = T)
