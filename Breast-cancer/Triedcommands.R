xtabs() --> for making contingency tables
#makes a contingency table for the variables MST and DEG
xtabs(~MST+DEG,data=d)

#package name dplyr for the following functions
count --> for frequency of each factor in a categorical variable

select --> for selecting certain columns of a datasets
#using select to get only the columns base_cd4 and cd4_at_12 in exam.sav file
select(Dataset,base_cd4,cd_at_12)

filter -->for selecting certain values in rows
#using filter function to get only the rows with value 
#"Lamivudine+Stavudine+Efavirenz" in there column haart_reg 
filter(Dataset,haart_reg=="Lamivudine+Stavudine+Efavirenz")

na.omit() -->for removing na values

#package used RcmdrMisc
numSummary() -->for getting entire numerical summary of a vector
