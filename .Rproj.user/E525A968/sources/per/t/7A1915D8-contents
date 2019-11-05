# Loading libraries
pacman::p_load(tidyverse, caret, readr, VIM, rstudioapi, arules, arulesViz)
install.packages("arulesViz", dependencies = T)
install.packages("rio", dependencies = T)
install.packages("VIM")
install.packages("DEoptimR")
install.packages("caTools", dependencies = T)
library(readr)
library(VIM)
library(DEoptimR)
library(rstudioapi)
library(arules)
library(arulesViz)


# Importing data

current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))

ElectronidexTrans2017_df <- read.csv("ElectronidexTransactions2017.csv", header = F)

ElectronidexTrans2017 <- read.transactions("ElectronidexTransactions2017.csv", 
                                           header = F, sep = ",", 
                                           format = "basket", 
                                           rm.duplicates = F)

ProductType_key <- read.csv("ProductType_csv.csv", header = T, sep = ";")

existingprod <- read.csv("Data_BW/existingproductattributes2017.csv")
existingprod$ProfitMonthly <- existingprod$Volume * existingprod$Price * existingprod$ProfitMargin


newprod <- read.csv("Data_BW/newprod_pred.csv", header = T)
newprod$Volume <- NULL
colnames(newprod)[colnames(newprod) == "finalpredictedvolume"] <- "Volume"
colnames(newprod) == "finalpredictedvolume"

blackwell_prod <- rbind(existingprod, newprod)

# VIM::aggr(ElectronidexTrans2017)

# Exploring the transactions file
summary(ElectronidexTrans2017)
inspect(head(ElectronidexTrans2017,2))
length(ElectronidexTrans2017)

size(ElectronidexTrans2017)
summary(size(ElectronidexTrans2017))
str(size(ElectronidexTrans2017))

LIST(ElectronidexTrans2017)

# Electronidex Plots

itemFrequencyPlot(ElectronidexTrans2017, topN = 10, type = "absolute")

Electronidex_sample <- sample(ElectronidexTrans2017, 100)

image(x = Electronidex_sample)

str(ElectronidexTrans2017)

dev.off()


view(ElectronidexTrans2017@itemInfo$labels)


ElectronidexTrans2017_df <- ElectronidexTrans2017_df %>% 
  mutate_if(is.factor, as.character)

str(ElectronidexTrans2017_df)

which(ElectronidexTrans2017_df[1,] == "iMac")

summary(ElectronidexTrans2017_df)



summaryElec <- data.frame(table(unlist(ElectronidexTrans2017_df)))

summaryElec <- summaryElec[-1,]
summaryElec <- summaryElec[-127,]
rownames(summaryElec) <- NULL

summaryElec <- summaryElec[order(summaryElec$Var1),]

summaryElec[71,2] <- summaryElec[71,2] + summaryElec[72,2]
summaryElec <- summaryElec[-72,]

rownames(summaryElec) <- NULL
summaryElec[72,]
summaryElec[73,]
summaryElec[72,2] <- summaryElec[72,2] + summaryElec[73,2]
summaryElec <- summaryElec[-73,]

rownames(summaryElec) <- NULL
summaryElec[77,]
summaryElec[78,]
summaryElec[77,2] <- summaryElec[77,2] + summaryElec[78,2]
summaryElec <- summaryElec[-77,]

rownames(summaryElec) <- NULL
summaryElec[98,]
summaryElec[99,]
summaryElec[98,2] <- summaryElec[98,2] + summaryElec[99,2]
summaryElec <- summaryElec[-99,]

rownames(summaryElec) <- NULL
summaryElec[101,]
summaryElec[125,]
summaryElec[101,2] <- summaryElec[101,2] + summaryElec[125,2]
summaryElec <- summaryElec[-125,]

rownames(summaryElec) <- NULL
summaryElec

summaryElec$Var1 <- as.character(summaryElec$Var1)

summaryElec[1,1] <- "Intel Desktop"

summaryElec$Var1 <- as.factor(summaryElec$Var1)
rownames(summaryElec) <- NULL

summaryElec <- summaryElec[order(summaryElec$Var1),]
rownames(summaryElec) <- NULL

write.csv(summaryElec, file = "Elec_volume")

colnames(summaryElec) <- c("ProductName", "Volume")

ggplot(summaryElec, aes(Volume)) + geom_histogram(binwidth = 200, 
                                                  color ="white")

#for (i in 1:nrow(summaryElec)){
#  replace(summaryElec[i,1], " ","")
#}

#grep(pattern = "iMac", 
#     x = ElectronidexTrans2017_df)

#data("Income")
#itemsets <- eclat(Income)[1:5]
#inspect(itemsets)





# BW product plots

ggplot(existingprod, aes(x = ProductType, y = Volume)) +
  geom_bar(stat = "identity") + 
  ggtitle("BW's existing products - sales volume")

ggplot(newprod, aes(x = ProductType, y = Volume)) +
  geom_bar(stat = "identity") +
  ggtitle("BW's new products - sales volume")

ggplot(existingprod, aes(x = ProductType, y = ProfitMonthly)) +
  geom_bar(stat = "identity") +
  ggtitle("BW's existing products - Profit")

ggplot(newprod, aes(x = ProductType, y = ProfitMonthly)) +
  geom_bar(stat = "identity") +
  ggtitle("BW's new products - Profit")

ggplot(blackwell_prod, aes(x = ProductType, y = Volume)) +
  geom_bar(stat = "identity") +
  ggtitle("BW's products (whole portfolio) - sales volume")

ggplot(blackwell_prod, aes(x = ProductType, y = ProfitMonthly)) +
  geom_bar(stat = "identity") +
  ggtitle("BW's products (whole portfolio) - Profit")


view(existingprod)

ggplot(existingprod, aes(x = ProductType, y = ProfitMargin, 
                         color = ProductType)) +  geom_boxplot()

# Converting itemMatrix into data frame
trans_matrix <- as(ElectronidexTrans2017, "matrix")
trans_df <- as.data.frame(trans_matrix)

trans_df
view(trans_df)

str(trans_df)

# Distribution  of transactions
summary(ElectronidexTrans2017)
summary(size(ElectronidexTrans2017))
dist_trans <- sort(size(ElectronidexTrans2017), decreasing = F )
dist_trans <- as.data.frame(dist_trans)

ggplot(dist_trans, aes(x= dist_trans)) + geom_histogram(binwidth = 1, 
                                                        color = "white")
sum(trans_df[1,1], trans_df[1,2])

colnames(trans_df)

size(ElectronidexTrans2017)

sort(itemFrequency(subset(ElectronidexTrans2017,
                     subset = ElectronidexTrans2017 %pin% "iMac"), 
              type = "absolute"), decreasing = F)



# Splitting the data between B2B and B2C



ElectronidexTrans2017

trans_matrix <- as(ElectronidexTrans2017, "matrix")
trans_df <- as.data.frame(as(ElectronidexTrans2017, "matrix"))



ElectronidexTrans2017_conv <- as(trans_df, "transactions")

ElectronidexTrans2017_conv

Desktop <- ElectronidexTrans2017@itemInfo[which(ProductType_key$BlackWell == 
                                                  "Desktop"), 1]
Desktop[-1]

test <- c(1,2,3,4)

test[-c(1,2)]

str(Desktop)
ind <- c()



for (i in 1:2){
  prin(Desktop[-i])
  print(which(ElectronidexTrans2017 %in% Desktop[i]))
}

for (i in (1:9)[-5]){
  print(i)
}



ElectronidexTrans2017[1,1] %in% Desktop


ElectronidexTrans2017_df %in% Desktop

set1 <- which(ElectronidexTrans2017 %in% Desktop[1] & 
  ElectronidexTrans2017 %in% Desktop[2] &
    ElectronidexTrans2017 %in% Desktop[3])

set2 <- which(ElectronidexTrans2017 %in% Desktop[1] & 
                ElectronidexTrans2017 %in% Desktop[2] &
                ElectronidexTrans2017 %in% Desktop[4])

set1
set2

set1_2 <- c(set1, set2)

str(set1_2)

str(ElectronidexTrans2017[set1_2])

ind <- c()

for (i in 1:9){
  for (j in (1:9)[-i]){
    for (k in (1:9)[-c(i,j)]){
      ind <- c(ind, which(ElectronidexTrans2017 %in% Desktop[i] & 
               ElectronidexTrans2017 %in% Desktop[j] &
               ElectronidexTrans2017 %in% Desktop[k]))
    }
  }
}

str(ind)

uniq_ind <- unique(ind)

str(uniq_ind)

summary(ind)

ElectronidexTrans2017_B2B[c(1,1),]



ElectronidexTrans2017_B2B_3Desktop <- ElectronidexTrans2017[ind]

ElectronidexTrans2017_B2B_3Desktop

Desktop_matrix <- as(ElectronidexTrans2017_B2B_3Desktop, "matrix")

Desktop_trans_df <- as.data.frame(Desktop_matrix)

Desktop_trans_df_uniq <- Desktop_trans_df %>% distinct()



str(which(ElectronidexTrans2017_B2B_3Desktop %pin% "Desktop" | 
        ElectronidexTrans2017_B2B_3Desktop %in% "iMac"))

summary(ElectronidexTrans2017[ind])



inde <- cbind(inde,which(ElectronidexTrans2017 %in% Desktop[1] & 
        ElectronidexTrans2017 %in% Desktop[2] &
        ElectronidexTrans2017 %in% Desktop[3]))

str(which(ElectronidexTrans2017 %in% Desktop))

str(which(ElectronidexTrans2017 %ain% Desktop[-1]))

str(which(ElectronidexTrans2017 %in% Desktop[-1] & 
        ElectronidexTrans2017 %in% Desktop[-1]))

sort(itemFrequency(ElectronidexTrans2017[which(ElectronidexTrans2017 %in% 
                                            Desktop[1])], type = "absolute"),
     decreasing = F)

ElectronidexTrans2017[ind]

#& ElectronidexTrans2017 %in% "Acer Desktop")]


str(which(ElectronidexTrans2017 %pin% "iMac"))
str(which(ElectronidexTrans2017 %pin% "iMac" & 
            ElectronidexTrans2017 %pin% "Desktop"))


ElectronidexTrans2017_B2B <- subset(ElectronidexTrans2017,
                                    subset = ElectronidexTrans2017 %pin% "iMac" 
                                    & ElectronidexTrans2017 %pin% "Desktop"
                                    | size(ElectronidexTrans2017) > 5)

ElectronidexTrans2017_B2C_3 <- subset(ElectronidexTrans2017,
                                      subset = !(ElectronidexTrans2017 %pin% "iMac" 
                                      & ElectronidexTrans2017 %pin% "Desktop"
                                      | size(ElectronidexTrans2017) > 5))


ElectronidexTrans2017_B2C_3 <- subset(ElectronidexTrans2017,
                                      subset = !(ElectronidexTrans2017 %pin% 
                                                   "iMac"))

ElectronidexTrans2017_B2C_3
ElectronidexTrans2017_B2B
summary(ElectronidexTrans2017_B2C_3)
head(sort(itemFrequency(ElectronidexTrans2017_B2C_3), decreasing = T))

ElectronidexTrans2017_B2C_2 <- subset(ElectronidexTrans2017_pt, 
                                      subset = items %in% "Monitors")

ElectronidexTrans2017@itemInfo[["ProductType"]]

ElectronidexTrans2017_pt

itemFrequency(ElectronidexTrans2017_B2C_2)

itemFrequencyPlot(ElectronidexTrans2017_B2C_2)

LIST(ElectronidexTrans2017)


summary(size(ElectronidexTrans2017_B2C))

ggplot(dist_trans, aes(x= dist_trans)) + geom_histogram(binwidth = 1, 
                                                        color = "white")


# apriori algorithm

association_rules <- apriori(ElectronidexTrans2017, 
                             parameter = list(supp = 0.002, conf = 0.8, 
                                              minlen = 2))
inspect(head(sort(association_rules, by = "confidence")))

inspect(head(association_rules, 5))
summary(association_rules)

inspect(sort(association_rules, by = "lift"))
inspect(subset(association_rules, subset = rhs %pin% "Laptop" & lift >4))
inspect(subset(association_rules, subset = lift >4))

# Adding "ProductType" to transactions itemMatrix

colnames(ProductType_key) <- c("labels", "ProductType")

# Checking for typing errors between ProductType_key 
# and transactions itemMatrix

checking_vector <- c()
view(ElectronidexTrans2017@itemInfo)
ProductType_key[1,1] == ElectronidexTrans2017@itemInfo[1,1]

checking_vector[1] <- 100

for (i in 1:125){
  checking_vector[i] <- ProductType_key[i,1] == 
                                        ElectronidexTrans2017@itemInfo[i,1]
}

summary(checking_vector)

Comparison_vector <- cbind(ProductType_key$ProductName, 
                           ElectronidexTrans2017@itemInfo, 
                           checking_vector)

# ElectronidexTrans2017@itemInfo <- merge(ElectronidexTrans2017@itemInfo, ProductType_key, by = "labels")

ElectronidexTrans2017@itemInfo$ProductType <- ProductType_key$ProductType

attributes(ElectronidexTrans2017@itemInfo)
summary(is.na(ElectronidexTrans2017@itemInfo))

view(ElectronidexTrans2017@itemInfo)

view(ElectronidexTrans2017)

  ElectronidexTrans2017_pt <- arules::aggregate(ElectronidexTrans2017, 
                                                by = "ProductType")
# Apriori for transactions per product type

association_rules_pt <- apriori(ElectronidexTrans2017_pt, 
                             parameter = list(supp = 0.05, conf = 0.8, 
                                              minlen = 2))
inspect(association_rules_pt)
summary(association_rules_pt)

inspect(sort(association_rules, by = "lift"))
inspect(subset(association_rules, subset = rhs %pin% "Laptop" & lift >4))
inspect(subset(association_rules, subset = lift >4))



# Calculation of profit margin of Black Well
