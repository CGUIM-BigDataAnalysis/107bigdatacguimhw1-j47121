knitr::kable()

## (1) ##
library(readr)
library(dplyr)
X103_EducationLevel <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/103_EducationLevel.csv")
X104_EducationLevel <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/104_EducationLevel.csv")
X105_EducationLevel <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/105_EducationLevel.csv")
X106_EducationLevel <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/106_EducationLevel.csv")

X103_CareerCategory <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/103_CareerCategory.csv")
X104_CareerCategory <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/104_CareerCategory.csv")
X105_CareerCategory <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/105_CareerCategory.csv")
X106_CareerCategory <- read_csv("C:/Users/ASUS/Desktop/Big Data Analytical Methods/106_CareerCategory.csv")

df103106 <- data.frame(Career = X103_EducationLevel$Career,
                       Year3 = "103",
                       UniversitySalary103 = X103_EducationLevel$UniversitySalary,
                       Year6 = "106",
                       UniversitySalary106 = X106_EducationLevel$UniversitySalary,
                       stringsAsFactors = F)
# 106年度薪資較103年度薪資高的職業有哪些? 
df103106$UniversitySalary103 <- as.numeric(gsub("—|…","",df103106$UniversitySalary103))
df103106$UniversitySalary106 <- as.numeric(gsub("—|…","",df103106$UniversitySalary106))

df103106$UniRate <- df103106$UniversitySalary106/df103106$UniversitySalary103
df103106 <- df103106[complete.cases(df103106[,6]),]

df103106 <- arrange(df103106,desc(UniRate))
knitr::kable(head(df103106,10))

# 提高超過5%的的職業有哪些
df103106UP5 <- df103106[df103106$UniRate > 1.05,]
knitr::kable(df103106UP5)

# 主要的職業種別是哪些種類呢
df103106UP5Num <- strsplit(df103106UP5$Career,"-") %>%
  sapply("[",1)
table(df103106UP5Num)
knitr::kable(table(df103106UP5Num))

## (2) ##
                             # 男多 # 
# 103年度
X103_Edu <- data.frame(Year = "103",
                       Career = X103_EducationLevel$Career,
                       `UniversityF/M` = X103_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X103_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X103_Edu$`UniversityF.M`))
X103_EduRENA <- na.omit(X103_Edu)
knitr::kable(head(arrange(X103_EduRENA,`UniversityF.M`),10))
# 104年度
X104_Edu <- data.frame(Year = "104",
                       Career = X104_EducationLevel$Career,
                       `UniversityF/M` = X104_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X104_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X104_Edu$`UniversityF.M`))
X104_EduRENA <- na.omit(X104_Edu)
knitr::kable(head(arrange(X104_EduRENA,`UniversityF.M`),10))
# 105年度
X105_Edu <- data.frame(Year = "105",
                       Career = X105_EducationLevel$Career,
                       `UniversityF/M` = X105_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X105_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X105_Edu$`UniversityF.M`))
X105_EduRENA <- na.omit(X105_Edu)
knitr::kable(head(arrange(X105_EduRENA,`UniversityF.M`),10))
# 106年度
X106_Edu <- data.frame(Year = "106",
                       Career = X106_EducationLevel$Career,
                       `UniversityF/M` = X106_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X106_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X106_Edu$`UniversityF.M`))
X106_EduRENA <- na.omit(X106_Edu)
knitr::kable(head(arrange(X106_EduRENA,`UniversityF.M`),10))

                            # 女多 #
# 103年度
X103_Edu <- data.frame(Year = "103",
                       Career = X103_EducationLevel$Career,
                       `UniversityF/M` = X103_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X103_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X103_Edu$`UniversityF.M`))
X103_EduRENA <- na.omit(X103_Edu)
X103_EduG <- X103_EduRENA[X103_EduRENA$UniversityF.M > 100.00,]
knitr::kable(head(arrange(X103_EduG,desc(`UniversityF.M`)),10))
# 104年度
X104_Edu <- data.frame(Year = "104",
                       Career = X104_EducationLevel$Career,
                       `UniversityF/M` = X104_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X104_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X104_Edu$`UniversityF.M`))
X104_EduRENA <- na.omit(X104_Edu)
X104_EduG <- X104_EduRENA[X104_EduRENA$UniversityF.M > 100.00,]
knitr::kable(head(arrange(X104_EduG,desc(`UniversityF.M`)),10))
# 105年度
X105_Edu <- data.frame(Year = "105",
                       Career = X105_EducationLevel$Career,
                       `UniversityF/M` = X105_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X105_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X105_Edu$`UniversityF.M`))
X105_EduRENA <- na.omit(X105_Edu)
X105_EduG <- X105_EduRENA[X105_EduRENA$UniversityF.M > 100.00,]
knitr::kable(head(arrange(X105_EduG,desc(`UniversityF.M`)),10))
# 106年度
X106_Edu <- data.frame(Year = "106",
                       Career = X106_EducationLevel$Career,
                       `UniversityF/M` = X106_EducationLevel$`UniversityF/M`,
                       stringsAsFactors = F)
X106_Edu$`UniversityF.M` <- as.numeric(gsub("—|…","",X106_Edu$`UniversityF.M`))
X106_EduRENA <- na.omit(X106_Edu)
X106_EduG <- X106_EduRENA[X106_EduRENA$UniversityF.M > 100.00,]
knitr::kable(head(arrange(X106_EduG,desc(`UniversityF.M`)),10))

## (3) ##
df06UniIns <- data.frame(Career = X106_EducationLevel$Career,
                         UniversitySalary = X106_EducationLevel$UniversitySalary,
                         InstituteAndAboveSalary = X106_EducationLevel$InstituteAndAboveSalary,
                         stringsAsFactors = F)

df06UniIns$InstituteAndAboveSalary <- as.numeric(gsub("—","",df06UniIns$InstituteAndAboveSalary))
df06UniIns$UniversitySalary <- as.numeric(gsub("—","",df06UniIns$UniversitySalary))
df06UniIns$SalaryRate <- df06UniIns$InstituteAndAboveSalary/df06UniIns$UniversitySalary

df06UniIns <- arrange(df06UniIns,desc(SalaryRate))
df06UniInsHead10 <- head(df06UniIns,10)
knitr::kable(head(df06UniIns,10))

## (4) ##
# 資訊及通訊傳播業-專業人員、資訊及通訊傳播業-技術員及助理專業人員、資訊及通訊傳播業-事務支援人員
myFav <- data.frame(Career = X106_EducationLevel$Career,
                    UniversitySalary = X106_EducationLevel$UniversitySalary,
                    InstituteSalary = X106_EducationLevel$InstituteAndAboveSalary,
                    stringsAsFactors = F)

myFav <- myFav[myFav$Career == c("資訊及通訊傳播業-專業人員", 
                                 "資訊及通訊傳播業-技術員及助理專業人員", 
                                 "資訊及通訊傳播業-事務支援人員"), ]
knitr::kable(myFav)

myFav$UniversitySalary <- as.numeric(myFav$UniversitySalary)
myFav$InstituteSalary <- as.numeric(myFav$InstituteSalary)
# 薪資差異
myFav$SalaryAdd <- myFav$InstituteSalary - myFav$UniversitySalary
knitr::kable(myFav)


install.packages("kableExtra")
library(kableExtra)





