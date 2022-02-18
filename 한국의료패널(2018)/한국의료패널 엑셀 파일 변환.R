# 한국의료패널 데이터 spss -> xlsx 변환

# install.packages("haven")
# install.packages("writexl")
library(writexl)
library(haven)


list(list.files(path = "rdata/khealth/sav"))

write_xlsx(read_spss('rdata/khealth/sav/mt18_h.sav'), path = "rdata/khealth/xlsx/mt18_h.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/mt18_i.sav'), path = "rdata/khealth/xlsx/mt18_i.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18appen.sav'), path = "rdata/khealth/xlsx/t18appen.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18cd.sav'), path = "rdata/khealth/xlsx/t18cd.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18er.sav'), path = "rdata/khealth/xlsx/t18er.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18hh.sav'), path = "rdata/khealth/xlsx/t18hh.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18in.sav'), path = "rdata/khealth/xlsx/t18in.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18income_ind.sav'), path = "rdata/khealth/xlsx/t18income_ind.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18ind.sav'), path = "rdata/khealth/xlsx/t18ind.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18ltc.sav'), path = "rdata/khealth/xlsx/t18ltc.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18md.sav'), path = "rdata/khealth/xlsx/t18md.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18ou.sav'), path = "rdata/khealth/xlsx/t18ou.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18phi.sav'), path = "rdata/khealth/xlsx/t18phi.xlsx")
write_xlsx(read_spss('rdata/khealth/sav/t18phr.sav'), path = "rdata/khealth/xlsx/t18phr.xlsx")

