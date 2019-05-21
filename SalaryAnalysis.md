107-2 大數據分析方法 作業一
================
游佳容

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**- [開放資料連結](https://data.gov.tw/dataset/6647)，可初步了解台灣近幾年各行各業、各學歷的起薪。

比較103年度和106年度大學畢業者的薪資資料
----------------------------------------

### 資料匯入與處理

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 3.5.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
X103_EducationLevel <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/103_EducationLevel.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Career = col_character(),
    ##   RecurrentSalary = col_double(),
    ##   `RecurrentSalaryF/M` = col_double(),
    ##   JuniorAndBelowSalary = col_character(),
    ##   `JuniorAndBelowF/M` = col_character(),
    ##   SeniorSalary = col_character(),
    ##   `SeniorF/M` = col_character(),
    ##   AcademySalary = col_character(),
    ##   `AcademyF/M` = col_character(),
    ##   UniversitySalary = col_character(),
    ##   `UniversityF/M` = col_character(),
    ##   InstituteAndAboveSalary = col_character(),
    ##   `InstituteAndAboveF/M` = col_character()
    ## )

``` r
X104_EducationLevel <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/104_EducationLevel.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Career = col_character(),
    ##   RecurrentSalary = col_double(),
    ##   `RecurrentSalaryF/M` = col_character(),
    ##   JuniorAndBelowSalary = col_character(),
    ##   `JuniorAndBelowF/M` = col_character(),
    ##   SeniorSalary = col_character(),
    ##   `SeniorF/M` = col_character(),
    ##   AcademySalary = col_character(),
    ##   `AcademyF/M` = col_character(),
    ##   UniversitySalary = col_character(),
    ##   `UniversityF/M` = col_character(),
    ##   InstituteAndAboveSalary = col_character(),
    ##   `InstituteAndAboveF/M` = col_character()
    ## )

``` r
X105_EducationLevel <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/105_EducationLevel.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Career = col_character(),
    ##   RecurrentSalary = col_double(),
    ##   `RecurrentSalaryF/M` = col_character(),
    ##   JuniorAndBelowSalary = col_character(),
    ##   `JuniorAndBelowF/M` = col_character(),
    ##   SeniorSalary = col_character(),
    ##   `SeniorF/M` = col_character(),
    ##   AcademySalary = col_character(),
    ##   `AcademyF/M` = col_character(),
    ##   UniversitySalary = col_character(),
    ##   `UniversityF/M` = col_character(),
    ##   InstituteAndAboveSalary = col_character(),
    ##   `InstituteAndAboveF/M` = col_character()
    ## )

``` r
X106_EducationLevel <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/106_EducationLevel.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Career = col_character(),
    ##   RecurrentSalary = col_double(),
    ##   `RecurrentSalaryF/M` = col_character(),
    ##   JuniorAndBelowSalary = col_character(),
    ##   `JuniorAndBelowF/M` = col_character(),
    ##   SeniorSalary = col_character(),
    ##   `SeniorF/M` = col_character(),
    ##   AcademySalary = col_character(),
    ##   `AcademyF/M` = col_character(),
    ##   UniversitySalary = col_character(),
    ##   `UniversityF/M` = col_character(),
    ##   InstituteAndAboveSalary = col_character(),
    ##   `InstituteAndAboveF/M` = col_character()
    ## )

``` r
df103106 <- data.frame(Career = X103_EducationLevel$Career,
                       Year3 = "103",
                       UniversitySalary103 = X103_EducationLevel$UniversitySalary,
                       Year6 = "106",
                       UniversitySalary106 = X106_EducationLevel$UniversitySalary,
                       stringsAsFactors = F)
```

### 106年度薪資較103年度薪資高的職業有哪些?

``` r
df103106$UniversitySalary103 <- as.numeric(gsub("—|…","",df103106$UniversitySalary103))
df103106$UniversitySalary106 <- as.numeric(gsub("—|…","",df103106$UniversitySalary106))

df103106$UniRate <- df103106$UniversitySalary106/df103106$UniversitySalary103
df103106 <- df103106[complete.cases(df103106[,6]),]

df103106 <- arrange(df103106,desc(UniRate))
knitr::kable(head(df103106,10))
```

| Career                                    | Year3 |  UniversitySalary103| Year6 |  UniversitySalary106|   UniRate|
|:------------------------------------------|:------|--------------------:|:------|--------------------:|---------:|
| 其他服務業-技術員及助理專業人員           | 103   |                24688| 106   |                27929|  1.131278|
| 住宿及餐飲業-服務及銷售工作人員           | 103   |                22564| 106   |                25486|  1.129498|
| 用水供應及污染整治業-技術員及助理專業人員 | 103   |                27944| 106   |                31560|  1.129402|
| 專業、科學及技術服務業-專業人員           | 103   |                29977| 106   |                33384|  1.113654|
| 其他服務業-技藝、機械設備操作及組裝人員   | 103   |                24222| 106   |                26880|  1.109735|
| 營造業-服務及銷售工作人員                 | 103   |                27164| 106   |                30125|  1.109005|
| 其他服務業-專業人員                       | 103   |                29000| 106   |                32000|  1.103448|
| 資訊及通訊傳播業-專業人員                 | 103   |                28839| 106   |                31817|  1.103263|
| 不動產業-專業人員                         | 103   |                30637| 106   |                33632|  1.097758|
| 教育服務業-事務支援人員                   | 103   |                22334| 106   |                24471|  1.095684|

-   結果說明：
    從結果看來**增加比例最高的為其他服務業-技術員及助理專業人員**，接下來是增加幅度差不多的住宿及餐飲業-服務及銷售工作人員以及用水供應及污染整治業-技術員及助理專業人員

### 提高超過5%的的職業有哪些?

``` r
df103106UP5 <- df103106[df103106$UniRate > 1.05,]
knitr::kable(df103106UP5)
```

| Career                                              | Year3 |  UniversitySalary103| Year6 |  UniversitySalary106|   UniRate|
|:----------------------------------------------------|:------|--------------------:|:------|--------------------:|---------:|
| 其他服務業-技術員及助理專業人員                     | 103   |                24688| 106   |                27929|  1.131278|
| 住宿及餐飲業-服務及銷售工作人員                     | 103   |                22564| 106   |                25486|  1.129498|
| 用水供應及污染整治業-技術員及助理專業人員           | 103   |                27944| 106   |                31560|  1.129402|
| 專業、科學及技術服務業-專業人員                     | 103   |                29977| 106   |                33384|  1.113654|
| 其他服務業-技藝、機械設備操作及組裝人員             | 103   |                24222| 106   |                26880|  1.109735|
| 營造業-服務及銷售工作人員                           | 103   |                27164| 106   |                30125|  1.109005|
| 其他服務業-專業人員                                 | 103   |                29000| 106   |                32000|  1.103448|
| 資訊及通訊傳播業-專業人員                           | 103   |                28839| 106   |                31817|  1.103263|
| 不動產業-專業人員                                   | 103   |                30637| 106   |                33632|  1.097758|
| 教育服務業-事務支援人員                             | 103   |                22334| 106   |                24471|  1.095684|
| 住宿及餐飲業-技術員及助理專業人員                   | 103   |                25633| 106   |                28009|  1.092693|
| 專業、科學及技術服務業-技藝、機械設備操作及組裝人員 | 103   |                26211| 106   |                28595|  1.090954|
| 運輸及倉儲業-技藝、機械設備操作及組裝人員           | 103   |                28087| 106   |                30618|  1.090113|
| 其他服務業-事務支援人員                             | 103   |                23863| 106   |                26007|  1.089846|
| 教育服務業-服務及銷售工作人員                       | 103   |                24491| 106   |                26668|  1.088890|
| 用水供應及污染整治業                                | 103   |                27456| 106   |                29834|  1.086611|
| 用水供應及污染整治業-專業人員                       | 103   |                31444| 106   |                34107|  1.084690|
| 資訊及通訊傳播業                                    | 103   |                27055| 106   |                29198|  1.079209|
| 支援服務業-服務及銷售工作人員                       | 103   |                24166| 106   |                26001|  1.075933|
| 藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員 | 103   |                24895| 106   |                26768|  1.075236|
| 資訊及通訊傳播業-事務支援人員                       | 103   |                25276| 106   |                27156|  1.074379|
| 教育服務業                                          | 103   |                24027| 106   |                25784|  1.073126|
| 營造業-專業人員                                     | 103   |                30580| 106   |                32785|  1.072106|
| 專業、科學及技術服務業                              | 103   |                27663| 106   |                29648|  1.071757|
| 支援服務業-技藝、機械設備操作及組裝人員             | 103   |                25365| 106   |                27169|  1.071122|
| 住宿及餐飲業                                        | 103   |                24646| 106   |                26398|  1.071087|
| 住宿及餐飲業-技藝、機械設備操作及組裝人員           | 103   |                24823| 106   |                26585|  1.070983|
| 電力及燃氣供應業-服務及銷售工作人員                 | 103   |                26013| 106   |                27837|  1.070119|
| 運輸及倉儲業-技術員及助理專業人員                   | 103   |                28974| 106   |                30993|  1.069683|
| 運輸及倉儲業-事務支援人員                           | 103   |                25886| 106   |                27685|  1.069497|
| 醫療保健服務業-技術員及助理專業人員                 | 103   |                28465| 106   |                30392|  1.067697|
| 專業、科學及技術服務業-技術員及助理專業人員         | 103   |                27195| 106   |                29016|  1.066961|
| 支援服務業-技術員及助理專業人員                     | 103   |                26933| 106   |                28696|  1.065459|
| 用水供應及污染整治業-服務及銷售工作人員             | 103   |                28736| 106   |                30593|  1.064623|
| 礦業及土石採取業-技藝、機械設備操作及組裝人員       | 103   |                26647| 106   |                28367|  1.064548|
| 用水供應及污染整治業-技藝、機械設備操作及組裝人員   | 103   |                26087| 106   |                27758|  1.064055|
| 服務業部門-技藝、機械設備操作及組裝人員             | 103   |                26722| 106   |                28376|  1.061897|
| 教育服務業-技術員及助理專業人員                     | 103   |                25675| 106   |                27250|  1.061344|
| 服務業部門-專業人員                                 | 103   |                30806| 106   |                32632|  1.059274|
| 運輸及倉儲業                                        | 103   |                28143| 106   |                29808|  1.059162|
| 資訊及通訊傳播業-技術員及助理專業人員               | 103   |                27288| 106   |                28902|  1.059147|
| 醫療保健服務業-技藝、機械設備操作及組裝人員         | 103   |                27409| 106   |                29028|  1.059068|
| 用水供應及污染整治業-事務支援人員                   | 103   |                25430| 106   |                26924|  1.058750|
| 金融及保險業-事務支援人員                           | 103   |                29070| 106   |                30771|  1.058514|
| 服務業部門-技術員及助理專業人員                     | 103   |                27882| 106   |                29504|  1.058174|
| 藝術、娛樂及休閒服務業-事務支援人員                 | 103   |                23602| 106   |                24970|  1.057961|
| 藝術、娛樂及休閒服務業                              | 103   |                25204| 106   |                26614|  1.055943|
| 營造業-事務支援人員                                 | 103   |                24892| 106   |                26256|  1.054797|
| 工業及服務業部門-專業人員                           | 103   |                30449| 106   |                32108|  1.054485|
| 服務業部門-事務支援人員                             | 103   |                25554| 106   |                26930|  1.053847|
| 電力及燃氣供應業-技藝、機械設備操作及組裝人員       | 103   |                27253| 106   |                28717|  1.053719|
| 教育服務業-專業人員                                 | 103   |                25722| 106   |                27101|  1.053612|
| 專業、科學及技術服務業-服務及銷售工作人員           | 103   |                26245| 106   |                27649|  1.053496|
| 服務業部門                                          | 103   |                27258| 106   |                28715|  1.053452|
| 其他服務業                                          | 103   |                24232| 106   |                25517|  1.053029|
| 製造業-專業人員                                     | 103   |                30035| 106   |                31612|  1.052505|
| 工業部門-專業人員                                   | 103   |                30215| 106   |                31775|  1.051630|
| 資訊及通訊傳播業-服務及銷售工作人員                 | 103   |                25995| 106   |                27296|  1.050048|

### 主要的職業種別是哪些種類呢?

``` r
df103106UP5Num <- strsplit(df103106UP5$Career,"-") %>%
  sapply("[",1)
knitr::kable(table(df103106UP5Num))
```

| df103106UP5Num         |  Freq|
|:-----------------------|-----:|
| 工業及服務業部門       |     1|
| 工業部門               |     1|
| 不動產業               |     1|
| 支援服務業             |     3|
| 用水供應及污染整治業   |     6|
| 住宿及餐飲業           |     4|
| 其他服務業             |     5|
| 服務業部門             |     5|
| 金融及保險業           |     1|
| 專業、科學及技術服務業 |     5|
| 教育服務業             |     5|
| 資訊及通訊傳播業       |     5|
| 運輸及倉儲業           |     4|
| 電力及燃氣供應業       |     2|
| 製造業                 |     1|
| 營造業                 |     3|
| 醫療保健服務業         |     2|
| 藝術、娛樂及休閒服務業 |     3|
| 礦業及土石採取業       |     1|

男女同工不同酬現況分析
----------------------

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到106年度的大學畢業薪資。

### 103到106年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

``` r
# 103年度
X103_Edu <- data.frame(Year = "103",
                       Career = X103_EducationLevel$Career,
                       `UniversityF/M` = X103_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X103_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X103_Edu$`UniversityF.M`))
X103_EduRENA <- na.omit(X103_Edu)
knitr::kable(head(arrange(X103_EduRENA,`UniversityF.M`),10))
```

| Year | Career                                              |  UniversityF.M|
|:-----|:----------------------------------------------------|--------------:|
| 103  | 礦業及土石採取業-技藝、機械設備操作及組裝人員       |          84.97|
| 103  | 教育服務業-技藝、機械設備操作及組裝人員             |          88.49|
| 103  | 其他服務業-技術員及助理專業人員                     |          89.36|
| 103  | 電力及燃氣供應業-技藝、機械設備操作及組裝人員       |          91.77|
| 103  | 礦業及土石採取業-服務及銷售工作人員                 |          92.57|
| 103  | 營造業                                              |          95.58|
| 103  | 教育服務業-事務支援人員                             |          95.83|
| 103  | 教育服務業                                          |          95.91|
| 103  | 藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員 |          96.13|
| 103  | 其他服務業                                          |          96.21|

``` r
# 104年度
X104_Edu <- data.frame(Year = "104",
                       Career = X104_EducationLevel$Career,
                       `UniversityF/M` = X104_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X104_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X104_Edu$`UniversityF.M`))
X104_EduRENA <- na.omit(X104_Edu)
knitr::kable(head(arrange(X104_EduRENA,`UniversityF.M`),10))
```

| Year | Career                                            |  UniversityF.M|
|:-----|:--------------------------------------------------|--------------:|
| 104  | 電力及燃氣供應業-技藝、機械設備操作及組裝人員     |          91.69|
| 104  | 教育服務業-服務及銷售工作人員                     |          91.90|
| 104  | 礦業及土石採取業-技術員及助理專業人員             |          92.42|
| 104  | 礦業及土石採取業-技藝、機械設備操作及組裝人員     |          93.10|
| 104  | 礦業及土石採取業                                  |          95.28|
| 104  | 其他服務業-事務支援人員                           |          95.47|
| 104  | 營造業-技藝、機械設備操作及組裝人員               |          95.64|
| 104  | 用水供應及污染整治業-技藝、機械設備操作及組裝人員 |          95.90|
| 104  | 營造業                                            |          96.35|
| 104  | 教育服務業                                        |          96.44|

``` r
# 105年度
X105_Edu <- data.frame(Year = "105",
                       Career = X105_EducationLevel$Career,
                       `UniversityF/M` = X105_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X105_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X105_Edu$`UniversityF.M`))
X105_EduRENA <- na.omit(X105_Edu)
knitr::kable(head(arrange(X105_EduRENA,`UniversityF.M`),10))
```

| Year | Career                                        |  UniversityF.M|
|:-----|:----------------------------------------------|--------------:|
| 105  | 不動產業-技藝、機械設備操作及組裝人員         |          91.38|
| 105  | 醫療保健服務業-專業人員                       |          94.98|
| 105  | 用水供應及污染整治業-事務支援人員             |          95.04|
| 105  | 營造業-事務支援人員                           |          95.65|
| 105  | 不動產業-事務支援人員                         |          95.66|
| 105  | 營造業                                        |          95.78|
| 105  | 營造業-專業人員                               |          96.52|
| 105  | 資訊及通訊傳播業-技藝、機械設備操作及組裝人員 |          96.64|
| 105  | 不動產業-服務及銷售工作人員                   |          96.68|
| 105  | 其他服務業                                    |          96.72|

``` r
# 106年度
X106_Edu <- data.frame(Year = "106",
                       Career = X106_EducationLevel$Career,
                       `UniversityF/M` = X106_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X106_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X106_Edu$`UniversityF.M`))
X106_EduRENA <- na.omit(X106_Edu)
knitr::kable(head(arrange(X106_EduRENA,`UniversityF.M`),10))
```

| Year | Career                                        |  UniversityF.M|
|:-----|:----------------------------------------------|--------------:|
| 106  | 電力及燃氣供應業-技藝\_機械設備操作及組裝人員 |          95.51|
| 106  | 營造業-服務及銷售工作人員                     |          95.93|
| 106  | 其他服務業-事務支援人員                       |          96.23|
| 106  | 電力及燃氣供應業-技術員及助理專業人員         |          96.54|
| 106  | 其他服務業                                    |          96.57|
| 106  | 住宿及餐飲業-技藝\_機械設備操作及組裝人員     |          96.58|
| 106  | 營造業                                        |          96.71|
| 106  | 教育服務業-專業人員                           |          96.71|
| 106  | 運輸及倉儲業-事務支援人員                     |          96.83|
| 106  | 其他服務業-技術員及助理專業人員               |          96.84|

### 哪些行業女生薪資比男生薪資多?

``` r
# 103年度
X103_Edu <- data.frame(Year = "103",
                       Career = X103_EducationLevel$Career,
                       `UniversityF/M` = X103_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X103_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X103_Edu$`UniversityF.M`))
X103_EduRENA <- na.omit(X103_Edu)
X103_EduG <- X103_EduRENA[X103_EduRENA$UniversityF.M > 100.00,]
knitr::kable(head(arrange(X103_EduG,desc(`UniversityF.M`)),10))
```

| Year | Career |  UniversityF.M|
|:-----|:-------|--------------:|

``` r
# 104年度
X104_Edu <- data.frame(Year = "104",
                       Career = X104_EducationLevel$Career,
                       `UniversityF/M` = X104_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X104_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X104_Edu$`UniversityF.M`))
X104_EduRENA <- na.omit(X104_Edu)
X104_EduG <- X104_EduRENA[X104_EduRENA$UniversityF.M > 100.00,]
knitr::kable(head(arrange(X104_EduG,desc(`UniversityF.M`)),10))
```

| Year | Career                                              |  UniversityF.M|
|:-----|:----------------------------------------------------|--------------:|
| 104  | 專業、科學及技術服務業-技藝、機械設備操作及組裝人員 |         100.26|

``` r
# 105年度
X105_Edu <- data.frame(Year = "105",
                       Career = X105_EducationLevel$Career,
                       `UniversityF/M` = X105_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X105_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X105_Edu$`UniversityF.M`))
X105_EduRENA <- na.omit(X105_Edu)
X105_EduG <- X105_EduRENA[X105_EduRENA$UniversityF.M > 100.00,]
knitr::kable(head(arrange(X105_EduG,desc(`UniversityF.M`)),10))
```

| Year | Career                |  UniversityF.M|
|:-----|:----------------------|--------------:|
| 105  | 金融及保險業-專業人員 |         100.11|

``` r
# 106年度
X106_Edu <- data.frame(Year = "106",
                       Career = X106_EducationLevel$Career,
                       `UniversityF/M` = X106_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X106_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X106_Edu$`UniversityF.M`))
X106_EduRENA <- na.omit(X106_Edu)
X106_EduG <- X106_EduRENA[X106_EduRENA$UniversityF.M > 100.00,]
knitr::kable(head(arrange(X106_EduG,desc(`UniversityF.M`)),10))
```

| Year | Career                              |  UniversityF.M|
|:-----|:------------------------------------|--------------:|
| 106  | 資訊及通訊傳播業-服務及銷售工作人員 |         100.33|

-   結果說明：
    由上面兩題可以看出大部分的行業在**相同的條件下的薪資大多還是男性多於女性**，四個年度中女性薪資大於男性幾乎可以說是微乎其微；
    但可以發現106年度的差異已較103年小，在不論職業別的情況下，可以看見差異最大的大學畢業薪資男女比已從84.97上升至95.51(比例越大差異越小)。

研究所薪資差異
--------------

以106年度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

``` r
df06UniIns <- data.frame(Career = X106_EducationLevel$Career,
                         UniversitySalary = X106_EducationLevel$UniversitySalary,
                         InstituteAndAboveSalary = X106_EducationLevel$InstituteAndAboveSalary,
                         stringsAsFactors = F)

df06UniIns$InstituteAndAboveSalary <- as.numeric(gsub("—","",df06UniIns$InstituteAndAboveSalary))
df06UniIns$UniversitySalary <- as.numeric(gsub("—","",df06UniIns$UniversitySalary))

df06UniIns$SalaryRate <- df06UniIns$InstituteAndAboveSalary/df06UniIns$UniversitySalary
df06UniIns <- arrange(df06UniIns,desc(SalaryRate))
knitr::kable(head(df06UniIns,10))
```

| Career                              |  UniversitySalary|  InstituteAndAboveSalary|  SalaryRate|
|:------------------------------------|-----------------:|------------------------:|-----------:|
| 礦業及土石採取業-事務支援人員       |             24815|                    30000|    1.208946|
| 專業\_科學及技術服務業              |             29648|                    35666|    1.202982|
| 其他服務業-技術員及助理專業人員     |             27929|                    33500|    1.199470|
| 專業\_科學及技術服務業-事務支援人員 |             27035|                    32234|    1.192306|
| 批發及零售業                        |             27611|                    32910|    1.191916|
| 製造業                              |             28155|                    33458|    1.188350|
| 藝術\_娛樂及休閒服務業-事務支援人員 |             24970|                    29657|    1.187705|
| 工業部門                            |             28263|                    33448|    1.183455|
| 工業及服務業部門                    |             28446|                    33633|    1.182346|
| 服務業部門                          |             28715|                    33922|    1.181334|

-   結果說明：
    從分析看來薪資增加比最大的職業為**礦業及土石採取業-事務支援人員，薪資增加比為1.208946**，所以最划算為此行業；
    第二為**專業\_科學及技術服務業**，薪資增加比為1.202982；
    第三為**其他服務業-技術員及助理專業人員**，薪資增加比為1.199470。；

我有興趣的職業別薪資狀況分析
----------------------------

### 有興趣的職業別篩選，呈現薪資

有興趣職業為：資訊及通訊傳播業-專業人員、資訊及通訊傳播業-技術員及助理專業人員、資訊及通訊傳播業-事務支援人員。

``` r
myFav <- data.frame(Career = X106_EducationLevel$Career,
                    UniversitySalary = X106_EducationLevel$UniversitySalary,
                    InstituteSalary = X106_EducationLevel$InstituteAndAboveSalary,
                    stringsAsFactors = F)

myFav <- myFav[myFav$Career == c("資訊及通訊傳播業-專業人員", 
                                 "資訊及通訊傳播業-技術員及助理專業人員", 
                                 "資訊及通訊傳播業-事務支援人員"), ]
```

    ## Warning in myFav$Career == c("資訊及通訊傳播業-專業人員", "資訊及通訊傳播
    ## 業-技術員及助理專業人員", : 較長的物件長度並非較短物件長度的倍數

``` r
knitr::kable(myFav)
```

|     | Career                                | UniversitySalary | InstituteSalary |
|-----|:--------------------------------------|:-----------------|:----------------|
| 79  | 資訊及通訊傳播業-專業人員             | 31817            | 36545           |
| 80  | 資訊及通訊傳播業-技術員及助理專業人員 | 28902            | 32354           |
| 81  | 資訊及通訊傳播業-事務支援人員         | 27156            | 30856           |

-   請問此薪資與妳想像中的一樣嗎?
    從列表來看目前薪資看來跟所想出入並不大

### 這些職業別研究所薪資與大學薪資差多少呢？

``` r
myFav$UniversitySalary <- as.numeric(myFav$UniversitySalary)
myFav$InstituteSalary <- as.numeric(myFav$InstituteSalary)
myFav$SalaryAdd <- myFav$InstituteSalary - myFav$UniversitySalary
knitr::kable(myFav)
```

|     | Career                                |  UniversitySalary|  InstituteSalary|  SalaryAdd|
|-----|:--------------------------------------|-----------------:|----------------:|----------:|
| 79  | 資訊及通訊傳播業-專業人員             |             31817|            36545|       4728|
| 80  | 資訊及通訊傳播業-技術員及助理專業人員 |             28902|            32354|       3452|
| 81  | 資訊及通訊傳播業-事務支援人員         |             27156|            30856|       3700|

-   會因為這樣改變心意，決定念/不念研究所嗎?
    會，畢竟一個月相差快五千這樣累積下來其實一年就相差將近兩個月的薪資。
