## Examining a CSV generated from City of Memphis Fiscal Year Budgets
## FY2011-FY2018.

# Load required libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(magrittr)
library(gridExtra)

# Import all of our 8 years worth of Memphis budgetary data from the individual
# csv files.

FY11_Adopted <- read.csv("FY11_ADOPTED_export.CSV")

FY12_Adopted <- read.csv("FY12_ADOPTED_export.CSV")

FY13_Adopted <- read.csv("FY13_ADOPTED_export.CSV")

FY14_Adopted <- read.csv("FY14_ADOPTED_export.CSV")

FY15_Adopted <- read.csv("FY15_ADOPTED_export.CSV")

FY16_Adopted <- read.csv("FY16_ADOPTED_export.CSV")

FY17_Adopted <- read.csv("FY17_ADOPTED_export.CSV")

FY18_Adopted <- read.csv("FY18_ADOPTED_export.CSV")

# Bind them in the R programming environment because it would be a miserable
# process in excel given that each has 8 columns and ~52k rows.

FY_8yr_Adopted <- rbind(FY11_Adopted, FY12_Adopted, FY13_Adopted, FY14_Adopted,
                        FY15_Adopted, FY16_Adopted, FY17_Adopted, FY18_Adopted)

# write.csv(FY_8yr_Adopted, file = "FY_8yr_Adopted.csv", row.names=FALSE)

# Replace numerical values for "Fund" with their descriptions from another csv.
# Everyone says FOR loops are slow, and they're right, but this is just 46
# easily copy-pasted items, so why not?

for (i in 1:nrow(FY_8yr_Adopted)) {
        FY_8yr_Adopted$FUND[i] <-
                if (FY_8yr_Adopted$FUND[i] == 111) {
                        "General Fund"
                } else if (FY_8yr_Adopted$FUND[i] == 121) { 
                        "Park Special Service Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 131) { 
                        "Life Insurance Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 201) { 
                        "State Street Aid Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 203) { 
                        "Midtown Corridor Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 204) { 
                        "Solid Waste Management Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 205) { 
                        "Miscellaneous Grants Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 206) { 
                        "JTPA Grant Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 207) { 
                        "US Deptartment of Labor Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 210) { 
                        "Youth Opportunity Grant Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 211) { 
                        "Workforce Investment Act Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 216) { 
                        "Drug Enforcement Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 218) { 
                        "Law Enforcement Block Grant III Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 219) { 
                        "MLK Park Improvement Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 220) { 
                        "Law Enforcement Block Grant IV Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 221) { 
                        "Community Development Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 222) { 
                        "Law Enforcement Block Grant V Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 223) { 
                        "Hotel/Motel Occupancy Tax Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 224) { 
                        "Law Enforcement Block Grant VI Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 225) { 
                        "New Memphis Arena Special Revenue Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 226) { 
                        "Law Enforcement Block Grant VII Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 227) { 
                        "Law Enforcement Block Grant VIII Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 228) { 
                        "CRA Programs" 
                } else if (FY_8yr_Adopted$FUND[i] == 301) { 
                        "Debt Service Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 400) { 
                        "Capital Projects Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 601) { 
                        "Sewer Treatment & Collection - Operating Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 602) { 
                        "Sewer Treatment & Collection - CIP Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 631) { 
                        "Port Commission - Operating Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 632) { 
                        "Port Commission - CIP Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 651) { 
                        "Golf Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 661) { 
                        "Metro Alarm Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 671) { 
                        "Storm Water Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 701) { 
                        "Printing & Mail Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 711) { 
                        "Information Systems Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 721) { 
                        "Health Insurance Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 722) { 
                        "Health Insurance Fund - GenAm PPO" 
                } else if (FY_8yr_Adopted$FUND[i] == 723) { 
                        "Health Insurance Fund - CIGNA POS" 
                } else if (FY_8yr_Adopted$FUND[i] == 724) { 
                        "Health Insurance Fund - United Health Care" 
                } else if (FY_8yr_Adopted$FUND[i] == 731) { 
                        "Unemployment Compensation Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 741) { 
                        "Fleet Management Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 804) { 
                        "Beale Street District Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 814) { 
                        "International Cultural Series Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 851) { 
                        "City Retirement System Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 853) { 
                        "Other Post Employment Benefit Trust Fund" 
                } else if (FY_8yr_Adopted$FUND[i] == 889) { 
                        "Board of Education Property Tax Fund" 
                } else { 
                        "Unknown" 
                }
        
}

# write.csv(FY_8yr_Adopted, file = "FY_8yr_Adopted_FundRep.csv",
# row.names=FALSE)

sum(FY_8yr_Adopted$FUND == "Unknown")

# That first FOR loop was easy enough, but the csv containing the "Accounts"
# descripitions has 863 rows of data to be cross-checked against our 420k+
# budget records. Furthermore, the csv containing "Service Center" descriptions
# contains 340 entries. That would be some terrible overhead using nested FOR
# loops, and other solutions with dplyr and data.table seem clumsy for this
# task. So, I followed the path of least resistance and used VBA: 
# https://github.com/danielbenson/MemRep.VBA/blob/master/MemRep.VBA

# Read in the csv with added descriptions. Note: Project column was not used in
# these reports so I removed it.

Budget_FY11_FY18 <- read.csv("FY_8yr_Adopted_Fund_Clean.csv")

summary(Budget_FY11_FY18)

summary(Budget_FY11_FY18$ACCOUNT)

summary(Budget_FY11_FY18$PTD_BALANCE)

# A "quick" FOR loop to apply quarters based on Memphis' fiscal calendar.

Quarter <- vector(mode = "character", length = nrow(Budget_FY11_FY18))

for (i in 1:nrow(Budget_FY11_FY18)) {
        Quarter[i] <- if (Budget_FY11_FY18$PERIOD_NAME[i] == "July"|
                          Budget_FY11_FY18$PERIOD_NAME[i] == "August"|
                          Budget_FY11_FY18$PERIOD_NAME[i] == "September") {
                "Q1"
        } else if (Budget_FY11_FY18$PERIOD_NAME[i] == "October"|
                   Budget_FY11_FY18$PERIOD_NAME[i] == "November"|
                   Budget_FY11_FY18$PERIOD_NAME[i] == "December") { 
                "Q2" 
        } else if (Budget_FY11_FY18$PERIOD_NAME[i] == "January"|
                   Budget_FY11_FY18$PERIOD_NAME[i] == "February"|
                   Budget_FY11_FY18$PERIOD_NAME[i] == "March") { 
                "Q3" 
        } else { 
                "Q4" 
        }
        
}

QBudget <- cbind(Budget_FY11_FY18, Quarter)

# write.csv(QBudget, file = "QBudget.csv", row.names=FALSE)

Balance_8yrAVG <- QBudget %>%
        group_by(BUDGET_NAME) %>%
        summarize(Average_Balance = round(mean(PTD_BALANCE),3))%>%
        mutate(Basis = "Aggregate_AVG")

Balance_8yrMED <- QBudget %>%
        group_by(BUDGET_NAME) %>%
        summarize(Median_Balance = median(PTD_BALANCE))%>%
        mutate(Basis = "Aggregate_Median")

Balance_8yrSUM <- QBudget %>%
        group_by(BUDGET_NAME) %>%
        summarize(Sum_Balance = sum(PTD_BALANCE))%>%
        mutate(Basis = "Aggregate_Sum")

Balance_8yrMin <- QBudget %>%
        group_by(BUDGET_NAME, ACCOUNT) %>%
        summarize(Min_Balance = round(min(PTD_BALANCE),3))%>% 
        filter(Min_Balance == min(Min_Balance))%>% 
        mutate(Basis = "Aggregate_Min")

Balance_QSum <- QBudget %>%
        group_by(Quarter) %>%
        summarize(Sum_Balance = sum(PTD_BALANCE))%>%
        mutate(QBasis = "Quarterly_Sum")

Balance_QAVG <- QBudget %>%
        group_by(Quarter) %>%
        summarize(Average_Balance = round(mean(PTD_BALANCE),3))%>%
        mutate(QBasis = "Quarterly_AVG")

Balance_QMed <- QBudget %>%
        group_by(Quarter) %>%
        summarize(Median_Balance = median(PTD_BALANCE))%>%
        mutate(QBasis = "Quarterly_Median")

Balance_QMin <- QBudget %>%
        group_by(Quarter, ACCOUNT) %>%
        summarize(Min_Mean_Balance = round(min(mean(PTD_BALANCE)),3))%>% 
        filter(Min_Mean_Balance == min(Min_Mean_Balance))%>% 
        mutate(QBasis = "Quarterly_Min")

Health <- sum(QBudget$PTD_BALANCE)

Balance_Aggregates <- bind_rows(Balance_8yrAVG, Balance_8yrMED, Balance_8yrMin,
                                Balance_8yrSUM)

P1 <- ggplot(Balance_8yrAVG, aes(y = Average_Balance, x = BUDGET_NAME)) + 
        labs(y = "Avg Balance", x = "Fiscal Year") +
        geom_bar(stat = "identity", fill = "blue") +
        scale_y_continuous(labels = scales::dollar_format("$")) +
        ggtitle("Memphis, TN Adopted Budgets - Avg Monthly Balance (Accounts)")

P1

P2 <- ggplot(Balance_8yrMED, aes(y = Median_Balance, x = BUDGET_NAME)) + 
        labs(y = "Median Balance", x = "Fiscal Year") +
        geom_bar(stat = "identity", fill = "blue") +
        scale_y_continuous(labels = scales::dollar_format("$")) +
    ggtitle("Memphis, TN Adopted Budgets - Median Monthly Balance (Accounts)")

P2

P3 <- ggplot(Balance_8yrMin, aes(y = Min_Balance, x = BUDGET_NAME)) + 
        geom_label(vjust = 1, aes(fill = factor(ACCOUNT), label=ACCOUNT),
        colour = "white", fontface = "bold", show.legend = FALSE,
        size = 1.7) +
        labs(y = "Min Balance", x = "Fiscal Year") +
        geom_bar(stat = "identity", fill = "red") +
        scale_y_continuous(labels = scales::dollar_format("$")) +
        ggtitle("Memphis, TN Adopted Budgets - Min Yearly Balance (Account)")

P3

P4 <- ggplot(Balance_8yrSUM, aes(y = Sum_Balance, x = BUDGET_NAME)) + 
        labs(y = "Sum Balance", x = "Fiscal Year") +
        geom_bar(stat = "identity", fill = "blue") +
        scale_y_continuous(labels = scales::dollar_format("$")) +
        ggtitle("Memphis, TN Adopted Budgets - Sum Yearly Balance")

P4

FY_Group <- arrangeGrob(P4, P2, P3, P1, ncol=2, nrow=2,
                        top = "Memphis, TN Budget\nFY11-FY18 (Aggregate)\n")

ggsave("FY Budget Analysis.png",
       dpi=600, dev='png', height=8.1, width=20, units="in", FY_Group)

P5 <- ggplot(Balance_QAVG, aes(y = Average_Balance, x = Quarter)) + 
        labs(y = "Avg Balance", x = "Quarter") +
        geom_bar(stat = "identity", fill = "blue") +
        scale_y_continuous(labels = scales::dollar_format("$")) +
        ggtitle("Memphis, TN Adopted Budgets - Avg Quarterly Balance")

P5

P6 <- ggplot(Balance_QMed, aes(y = Median_Balance, x = Quarter)) + 
        labs(y = "Median Balance", x = "Quarter") +
        geom_bar(stat = "identity", fill = "blue") +
        scale_y_continuous(labels = scales::dollar_format("$")) +
        ggtitle("Memphis, TN Adopted Budgets - Median Quarterly Balance")

P6

P7 <- ggplot(Balance_QMin, aes(y = Min_Mean_Balance, x = Quarter)) + 
        geom_label(vjust = 1, aes(fill = factor(ACCOUNT), label=ACCOUNT),
                   colour = "white", fontface = "bold", show.legend = FALSE,
                   size = 2) +
        labs(y = "Min Balance (Avg)", x = "Quarter") +
        geom_bar(stat = "identity", fill = "red") +
        scale_y_continuous(labels = scales::dollar_format("$")) +
        ggtitle("Memphis, TN Adopted Budgets - Min Quarterly Balance (Account)")

P7

P8 <- ggplot(Balance_QSum, aes(y = Sum_Balance, x = Quarter)) + 
        labs(y = "Sum Balance", x = "Quarter") +
        geom_bar(stat = "identity", fill = "blue") +
        scale_y_continuous(labels = scales::dollar_format("$")) +
        ggtitle("Memphis, TN Adopted Budgets - Sum Quarterly Balance")

P8

Q_Group <- arrangeGrob(P8, P6, P7, P5, ncol=2, nrow=2,
                        top = "Memphis, TN Budget\nFY11-FY18 (Quarterly)\n")

ggsave("Quarterly Budget Analysis.png",
       dpi=600, dev='png', height=8.1, width=13, units="in", Q_Group)

## End Part 1 ##