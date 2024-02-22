install.packages("remotes")
library(remotes)
remotes::install_github("massimoaria/bibliometrix")


install.packages("bibliometrix")
library(bibliometrix)
biblioshiny()
1#####################################################
library("robumeta")
library("metafor")
library("dplyr")
#######################
library(readxl)
DHS_2022_STUNTING_INDIA_Zscore <- read_excel("Desktop/NFHS 5 AND NFHS 4/NITISH SIR ARTICLE/DHS_2022_STUNTING_INDIA_Zscore.xlsx")
View(DHS_2022_STUNTING_INDIA_Zscore)
mean(DHS_2022_STUNTING_INDIA_Zscore$m17)
India_c_sect <-read_excel("/Users/soumyajitsingharoy/Desktop/NFHS 5 AND NFHS 4/NITISH SIR ARTICLE/file137347ab0544.xlsx")
View(India_c_sect)
mean(India_c_sect$m17)
var(India_c_sect$m17,India_c_sect$m15 = NULL, na.rm = FALSE, use)
cor.test(India_c_sect$m17, India_c_sect$m15, alternative = "two.sided")
corrplot::corrplot(India_c_sect$m17, India_c_sect$m15, alterenatives = "two.sided")
scatter.smooth(data_frame3$V2, data_frame3$V17, span = 2/3, degree = 1, family = c("symmetric", "gaussian"),xlab = NULL, ylab = NULL, ylim = range(y, pred$y,na.rm = TRUE), evaluation = 50, ...,lpars = list())
x <-India_c_sect$m17
y <-India_c_sect$m15
mean(x,y)
mean(x)
mean(y)
install.packages("readxl")
library(readxl)
sd(x, na.rm = FALSE)
sd(y, na.rm = FALSE)
################################
terminator_for_R_try <- read_csv("Desktop/DHS_C section NFHS 5 and 4/csection_2022/Stata fies/stata data file/terminator for R try.csv")
install.packages("writexl")
library(writexl)
write_xlsx(data_frame3,"/Users/soumyajitsingharoy/Desktop/DHS_C section NFHS 5 and 4/Terminator/R try\\terminator-try-r.xlsx")
ls(terminator_for_R_try)
library(openxlsx)
library(haven)
write_sav(terminator_for_R_try,"/Users/soumyajitsingharoy/Desktop/DHS_C section NFHS 5 and 4/Terminator/R try.sav")
########################################################################3

read.csv("/Users/soumyajitsingharoy/Desktop/DHS_C section NFHS 5 and 4/Terminator/C_Section-DHS_2022_Terminator csv/C_Section-DHS_2022_Terminator.csv")
View(C_Section_DHS_2022_Terminator)
ggplot(data = C_Section_DHS_2022_Terminator) + geom_point(mapping = aes(x = ANC_VISIT, y = DELIVERY_PLACE, colour = 'red', size = 3))
y <- data_frame3$V2
x <- data_frame3$V18
view(x)
print(min(y, na.rm = TRUE))
scatter.smooth(x, y, span = 2/3, degree = 1, family = c("gaussian"), xlab = NULL, ylab = NULL, ylim = NULL)
qqnorm(x, na.rm = TRUE, ylab = "c_section")
library(tidyr)
replace_na(y)
recode(x, yes = "1", no = "2", .default = NA_character_)
z <-recode(x, yes = "1", no = "2", .default = NA_character_)
view(z)
scatter.smooth(x = z, y = NULL, span = 2/3, degree = 1, family = )
range(y,na.rm = TRUE)
a <- (y + z)
view(a)
remove(a)
library(plyr)
xd <- terminator.for.R.try
df <- xd [,c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]
library(tidyverse)
read_csv("/Users/soumyajitsingharoy/Desktop/DHS_C section NFHS 5 and 4/csection_2022/Stata fies/stata data file/terminator for R try.csv")
ggplot(data = data_frame3) + geom_point(mapping = aes(x = 'V4', y = 'V18', color = 'V17', shape = 'V18'))
install.packages("rpart")
install.packages("rpart.plot")
xd <-(terminator.for.R.try)
View(df)
df <- xd [,c('V2','V17', 'V18','V3')]
str(df)
###########################################
library(openxlsx)
write.xlsx(df, '/Users/soumyajitsingharoy/Desktop/DHS_C section NFHS 5 and 4/csection_2022/Stata fies/stata data file')
############################################
df <-terminator.for.R.try
remove(data_frame2)
df <-terminator.for.R.try
data_frame2 = df[,-c(1,5:8,24:26,32)]
View(data_frame3)
remove(y)
x <- (data_frame3$V17)
y <- (data_frame3$V21)
view(z)
z <- c(x,y)
library(igraph)
links = (data_frame3)
nodes = (csection)
summary(terminator.for.R.try)
####################################################
library(rpart)
remove(m2)
m2 <-rpart(V31 ~ ., data = data_frame3, method = "anova")
library(rpart.plot)
rpart.plot(m2, type = 5, digits = 3, fallen.leaves = TRUE, cex = .5)
boxplot(count ~ z, data = data_frame3 , col = "lightgray")
# *add* notches (somewhat funny here <--> warning "notches .. outside hinges"):
boxplot(count ~ spray, data = InsectSprays,
        notch = TRUE, add = TRUE, col = "blue")
#coursera#######
install.packages("palmerpenguins")
library(palmerpenguins)
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
ggplot(data = penguins) + geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g))
ggplot(data = penguins) + geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = species))
View('palmerpenguins')pguien

############################ R code for PCA##################
#install
install.packages('princomp')

# Load required library
library(prcomp)

# Load data
data(iris)

# Perform PCA
iris_pca <- prcomp(iris[, -5], scale = TRUE)

# Summarize results
summary(iris_pca)

# Plot results
plot(iris_pca$x[,1], iris_pca$x[,2],
     xlab = paste("PC1 (", round(iris_pca$sdev[1] / sum(iris_pca$sdev) * 100, 1), "%)", sep = ""),
     ylab = paste("PC2 (", round(iris_pca$sdev[2] / sum(iris_pca$sdev) * 100, 1), "%)", sep = ""),
     pch = 21, bg = c("red", "green3", "blue")[as.numeric(iris$Species)])
legend("topright", levels(iris$Species), pch = 21, bg = c("red", "green3", "blue"))

###################test##################

data("cars")
cars_pca <-prcomp(cars[, -5], scale. = TRUE)
summary(cars_pca)
View(cars_pca)
#####PCA#####
plot(cars_pca$x[,1], iris_pca$x[,2],
     xlab = paste("PC1 (", round(iris_pca$sdev[1] / sum(iris_pca$sdev) * 100, 1), "%)", sep = ""),
     ylab = paste("PC2 (", round(iris_pca$sdev[2] / sum(iris_pca$sdev) * 100, 1), "%)", sep = ""),
     pch = 21, bg = c("red", "green3", "blue")[as.numeric(iris$Species)])
legend("topright", levels(iris$Species), pch = 21, bg = c("red", "green3", "blue"))

###########Factominer##############
df_PCA <-data_frame3
install.packages("FactoMineR")
library(FactoMineR)
install.packages("missMDA")
data("decathlon")
summary(decathlon)
View(decathlon)
summary(df_PCA)
res_pca <- PCA (quanti.sup = 1:2, quali.sup = 13, graph = FALSE)
data("decathlon" )
library(ggplot2)



################ c section ####################
library(readxl)
R_try_terminator_try_r <- read_excel("Desktop/DHS_C section NFHS 5 and 4/Terminator/R try\\terminator-try-r.xlsx")
View(R_try_terminator_try_r)
summary(R_try_terminator_try_r)
cor.test(R_try_terminator_try_r$MOTHER_EDUCAT, R_try_terminator_try_r$DELIV_CESAREAN, alternative = "two.sided")
corplot <-cor.test(R_try_terminator_try_r$MOTHER_EDUCAT, R_try_terminator_try_r$DELIV_CESAREAN, alternative = "two.sided")
boxplot(R_try_terminator_try_r$DELIV_CESAREAN, data = R_try_terminator_try_r, main  = "Cesarean delivery", xlab = " number of c section", ylab = "mothers age")
summary(R_try_terminator_try_r)
ggplot(data = R_try_terminator_try_r) + geom_point(mapping = aes(x = SEX_CHILD, y = DELIV_CESAREAN))
ggplot(data = R_try_terminator_try_r) + geom_boxplot(mapping = aes(x = DELIV_CESAREAN, y = SEX_CHILD))




##################stunting_India and Karnataka#################
install.packages("haven")
library(haven)
stu_India <- read_dta("/Volumes/Research/NFHS 5_2023_PhD/Excel data file_NFHS 5/India_Z score_NFHS 5.dta")
class(stu_India)
dim(stu_India)
install.packages("dplyr")
library(dplyr)
new_df <-df %>% na.omit()
describe(stu_India, fast= TRUE)
drop_na(stu_India$MOTHER_EDUCAT)



############ R for social science #########
View(stu_India)
mean(stu_India$MOTHER_EDUCAT, na.rm = T)
median(stu_India$MOTHER_EDUCAT, na.rm = T)
var(stu_India$stunting, na.rm = T)
sd(stu_India$underweight, na.rm = T)
install.packages("moments")
library(moments)
skewness(stu_India$cage, na.rm = T)
install.packages("psych")
library(psych)
describe(stu_India[ ,c("stunting","wasting","underweight")],trim = 0.05, type=3)
stu_desc <-describe(stu_India[ ,c("stunting","wasting","underweight")],trim = 0.05, type=3)
View(stu_desc)
write.csv(describe(stu_India[ ,c("stunting","wasting","underweight")],trim = 0.05, type=3))
write.csv(stu_desc,file = "stu_desc1")
library(writexl)
write_xlsx(stu_desc)
install.packages(apaStyle)
library(apaStyle)
install.packages("apaTables")
library(apaTables)
apa.d.table(data = stu_desc, variables = c("Stunt", "Wasting","Underweight"), report = c("M", "SD"), title("APAtable_suw"), filename = "APAtable_suw.docx", note = NULL, position = "lower", merge = FALSE, landscape = FALSE, save = TRUE)
########################################


######################################
install.packages("corrplot")
library(corrplot)
terminator_csection <- read.csv("~/Desktop/DHS_C section NFHS 5 and 4/Terminator/terminator_csection.csv", stringsAsFactors=TRUE)
head(terminator_csection)
complete_csect <-na.omit(terminator_csection)
edited_complete_csect <- complete_csect[, - c(1, 24)]
str(terminator_csection)
remove(terminator_csection.cor)
terminator_csection.cor = cor(terminator_csection)
edited_csect_var = cor(edited_complete_csect)
terminator_csection.cor = cor(terminator_csection,method = c("spearman"))
edited_csect_spear = cor(edited_csect_var,method = c("spearman"))
View(terminator_csection.cor)
library(corrplot)
corchart <-cor(terminator_csection.cor)
corrplot(terminator_csection.cor,na.label = " ", method = "circle", type = "full", tl.col = "black", number.cex = 0.90, addCoef.col = "black")

corrplot(edited_csect_spear,na.label = " ", method = "pie", type = "full", tl.col = "black", number.cex = 0.75, addCoef.col = "black",tl.cex = 0.4)


# Load and install heatmaply package
install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
install.packages("heatmaply")
library(heatmaply)

# plotting corr heatmap
heatmaply_cor(x = cor(edited_csect_spear), xlab = "Factors", ylab = "Factors", k_col = 3, k_row = 3, label = value, scale = "column") 
heatmaply_cor(x = cor(edited_csect_spear), xlab = "Factors", ylab = "Factors", k_col = 4, k_row = 4, label = value, scale = "column", grid::get.gpar(col = "white", 1wd = 2)) 
library(grid)
                   
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("reshape2")
library(reshape2)
install.packages("reshape2")
library(reshape2)
melted_cor_map <- melt(edited_csect_spear)
library(ggplot2)
ggplot(melted_cor_map,aes(x=Var1,
                 y=Var2,
                 fill=value))+geom_tile()+
  scale_fill_gradient(low = "#FF0000",
                      high = "#FFE4E1",
                      guide = "colorbar") + theme(axis.text.x = element_text
                                                  (angle = 90,hjust = 1))

install.packages("reshape2")
library(reshape2)
melted_cor_map <- melt(edited_csect_spear)
library(ggplot2)
ggplot(melted_cor_map,aes(x=Var1,
                 y=Var2,
                 fill=value))+geom_tile()+
  scale_fill_gradient(low = "#86ebc9",
                      high = "#FFE4E1",
                      guide = "colorbar")


# Install and load reshape2 package
install.packages("reshape2")
library(reshape2)

# creating correlation matrix
corr_mat <- round(cor(data),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_cor_map, aes(x= " ", y=" ", fill=value)) + geom_tile() + geom_text(aes(Var2, Var1, label = value),color = "black", size = 2)



install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(terminator_csection.cor, histogram=TRUE,pch=19)

hist(edited_complete_csect$MOTHER_EDUCAT)


###############################
###############################
csection_pca <-prcomp(complete_csect[,c(1:4)],center = TRUE,scale. = TRUE)
str(csection_pca)
install.packages("ggfortify")
library(ggfortify)
csection_pca_plot <-prcomp(complete_csect,scale. = TRUE)
csection_pca_plot
csection_pca_plot <- 1*csection_pca
biplot(csection_pca_plot,scale = 0)
class(complete_csect$DELIV_CESAREAN)
df$csect <-csection_pca_plot

####### menarche_Sherpa ############ date-8 July 2023 ################

menarche <-read.csv(file = "/Volumes/Research/Menarche/Heatmap.csv")
edited_menarche <-menarche[, -c(1)]
cor_menarch <-cor(edited_menarche)
summary(cor_menarch)
corrplot(cor_menarch,na.label = " ", method = "circle", type = "full", tl.col = "black", number.cex = 0.90, addCoef.col = "black")
heatmaply_cor(x = cor(edited_menarche), xlab = "Factors", ylab = "Factors", k_col = 4, k_row = 3, label = value, scale = "row") 

####### install reshape and melt ########
library(RColorBrewer)
library(reshape2)
#### menarche_melt #######
nitmon_menarche.cor_spear = cor(edited_menarche,method = c("spearman"))

melted_cor_nitmon_menarche <-melt(nitmon_menarche.cor_spear)

ggplot(data = melted_cor_nitmon_menarche, aes(x= " ", y=" ", fill=value)) + geom_tile() + geom_text(aes(Var2, Var1, label = value),color = "black", size = 2)

#### generate-heatmap #######

ggplot(melted_cor_nitmon_menarche,aes(x=Var1,
                          y=Var2,
                          fill=value))+geom_tile()+
  scale_fill_gradient(low = "#FF0000",
                      high = "#FFE4E1",
                      guide = "colorbar") + theme(axis.text.x = element_text
                                                  (angle = 90,hjust = 1))

###### removed age and digit type  (correlation) #########

menarche <-read.csv(file = "/Volumes/Research/Menarche/Heatmap.csv")

edited_menarche <-menarche[, -c(1,2,10,11)]
cor_menarch_2 <-cor(edited_menarche)
summary(cor_menarch)
corrplot(cor_menarch_2,na.label = " ", method = "circle", type = "full", tl.col = "black", number.cex = 0.90, addCoef.col = "black")
heatmaply_cor(x = cor(edited_menarche), xlab = "Factors", ylab = "Factors", k_col = 4, k_row = 3, label = value, scale = "column") 

nitmon_menarche.cor_spear_2 = cor(edited_menarche,method = c("spearman"))

melted_cor_nitmon_menarche_2 <-melt(nitmon_menarche.cor_spear_2)

ggplot(data = melted_cor_nitmon_menarche_2, aes(x= " ", y=" ", fill=value)) + geom_tile() + geom_text(aes(Var2, Var1, label = value),color = "black", size = 2)

ggplot(melted_cor_nitmon_menarche_2,aes(x=Var1,
                                      y=Var2,
                                      fill=value))+geom_tile()+
  scale_fill_gradient(low = "#36454F",
                      high = "#FBFCFC",
                      guide = "colorbar") + theme(axis.text.x = element_text
                                                  (angle = 45,hjust = 1))

##################################################################################################################################################################################################################
#####################################################################################################################################################################################

#######meta analysis##########

a1 <- escalc(measure ="OR", ai= metaCIAF$Stunting, bi= metaCIAF$Underweight,
             ci= metaCIAF$Wasting, di= metaCIAF$CIAF,
             data = metaCIAF,slab = c(metaCIAF$Studied.Area, metaCIAF$Refferences.selected.for.systematic.review.),
             drop00 = FALSE,
             var.names=  c("yi","vi"), append =  TRUE )

install.packages("metafor")
library(meta)
library(metafor)
library(dplyr)
install.packages("xlsx")
library(xlsx)
metaCIAF<- read(file = "/Volumes/Research/CIAF/CIAF_Lit_2023/CIAF_met_csv.csv")
metaCIAF <-metaCIAF[, -c(2,3,4,5,6,7)]
sortvar = metaCIAF

###########################################

dat1 <- escalc(measure = "IRD", ai= metaCIAF$CIAF, bi= metaCIAF$Underweight,
              ci= metaCIAF$Wasting, di= metaCIAF$Stunting, 
              slab= paste(metaCIAF$Refferences.selected.for.systematic.review., ",", metaCIAF$Studied.Area,",", metaCIAF$Year.of.publication,",", sep = ""), 
              data= metaCIAF)

 
  dat_2<- escalc(measure = "OR", 
               ai= metaCIAF$CIAF, bi= metaCIAF$Underweight,
               ci= metaCIAF$Wasting, di= metaCIAF$Stunting, 
               slab= paste(metaCIAF$Refferences.selected.for.systematic.review., ",", metaCIAF$Year.of.publication, sep = "  "),
               data= metaCIAF)


res1 <-metacont(metaCIAF$CIAF.., bi= metaCIAF$Stunting, ci= metaCIAF$Underweight)
res2 <-rma(yi,vi, data = a1)
res1_ <-rma.uni(yi,vi, data = dat1)
res3 <- rma(yi,vi ,data = dat_2)
res3
metarate(stunting)
predict(res3, transf = exp, digits = 2)
na.omit(res3)


##############################

m.bin <- metabin(metaCIAF$Stunting,metaCIAF$Underweight, metaCIAF$Wasting, metaCIAF$CIAF, 
                 data = metaCIAF,
                 studlab = paste(metaCIAF$Refferences.selected.for.systematic.review.), 
                 comb.fixed = T,comb.random = T, method = 'MH',sm = 'RR')
m.bin

forest(m.bin, leftcols = c('studlab'), 
       annotate= TRUE, 
       header = TRUE, 
       col.diamond = "red"
       ,col.square = "blue")

funnel(m.bin, col = "blue", cex = 2)


m.bin2 <- metabin(metaCIAF$CIAF, metaCIAF$Stunting, metaCIAF$Underweight,metaCIAF$Wasting, 
                  data = metaCIAF,
         studlab = paste(metaCIAF$Refferences.selected.for.systematic.review.), 
                 comb.fixed = T,comb.random = T, method = 'MH',sm = 'RR')


m.bin2



m.bin3 <- metabin(metaCIAF$CIAF, metaCIAF$Stunting, metaCIAF$Underweight,metaCIAF$Wasting, 
                  data = metaCIAF,
                  studlab = paste(metaCIAF$Refferences.selected.for.systematic.review.), 
                  comb.fixed = T,comb.random = T, method = 'MH',sm = 'RR')


#########################################

forest(res1_, addpred = TRUE, header = TRUE, cex = 1, col = "red",)
forest(res2, addpred = TRUE, header = TRUE, cex = 1, col = "red")
forest(res1_, annotate = TRUE,
       header = TRUE, 
       cex = 0.8, 
           col = "red",
           psize = 2, 
       colout = "blue", 
       method = "ORL" , 
leftcols = ("studlab"))

 forest(res2, leftcols = ("Studlab"))

###################################################

install.packages("effsize")
library(effsize)
install.packages("esc")
library(esc)
#############################################
#############################################

funnel(res)
funnel(res1, ylim = c(0,.4), xlim = c(0,5), 
       digits = list(2L,1), 
       cex = 1, 
       col = "blue", 
       bg = "blue", 
       studlab = FALSE, 
       cex.studlab = 1,  )


########################################## Final set of metanalysis ##########################################

############################## import the dataset #################

install.packages("exceldata")
library(readxl)
meta_CIAF <- read_excel("/Volumes/Research/CIAF/Final excel sheet/Final file/NM_suwc_2023_Terminator.xlsx")
meta_CIAF


meta_CIAF <- meta_CIAF[-c(2,3,4,5,6,7)]

############################################################################################################################################################################################################ 
 
############################################################################################################################################################################################################ 

install.packages("metadat")
library(metadat)
library(meta) 

MA <- metaprop(meta_CIAF$CIAF, meta_CIAF$`TOTAL CASES`,
               data = meta_CIAF, studlab = paste(STUDIES),
               sm = "PRAW", common = FALSE ,random = T, byvar = ordered(meta_CIAF$Region))

forest(MA, header.line = TRUE, 
       col.subgroup = "darkblue", 
       label = TRUE,plotwidth = "5cm", spacing = 1, just.studlab = "left",
       colgap = "15mm",col.diamond = "red")

funnel(MA, col = "blue")



############################## prepapre the risk ratio  ########################################################################

install.packages("metadat")
library(metadat)
library(meta)


meta_CIAF$`Wasting (Event)` = as.numeric(as.character(meta_CIAF$`Wasting (Event)`))

MA <- metaprop(meta_CIAF$CIAF, meta_CIAF$`TOTAL CASES`,
               data = meta_CIAF, studlab = paste(STUDIES),
               sm = "PLOGIT", common = FALSE ,random = T, method = "GLMM", backtransf = T, method.ci = "NAsm" )
MA

forest(MA, header.line = TRUE, 
       col.subgroup = "darkblue", 
       label = TRUE,plotwidth = "5cm", spacing = 1, just.studlab = "left",
       colgap = "15mm",col.diamond = "red")

funnel.meta(MA, col = "blue", studlab = F,
            random = T,
            common = F,
            cex = 2,
            pch =16,
            lwd = 2, 
            level = 0.95,
            cex.studlab = .60,
            pos.studlab = 4,
            ref.triangle = FALSE,
            lty.random = 1)

metabias(MA, method.bias = "Egger", plotit = T, correct =T, JAMA.pval = gs("JAMA.pval"),big.mark = gs("big.mark"))


#############################################################################################################################################################
################################################# 
# PRISMA flow chart for systematic review
############################################################################################################################################################# 

install.packages("PRISMAstatement")
install.packages("metagear")

library(PRISMAstatement)

prisma_CIAF <- prisma(found = 463, found_other = 2, no_dupes = 282,screened = 183, 
                      screen_exclusions = 108,full_text = 75,
                      full_text_exclusions = 26,quantitative = 49, qualitative = 49, width = 200, height = 200)
prisma_CIAF
################################################################################################################################################################
############################################################################################################################################################# 







