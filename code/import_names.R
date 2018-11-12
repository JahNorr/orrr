require(excel.link)
require(RODBC)

sex_list<-c("Male","Female")
n_names<-200
import<-F

if(import) {
  fname<-"./data_raw/names.xlsx"

  df_last_names<-xl.read.file(fname, xl.sheet = "LastNames")
  df_female_names<-xl.read.file(fname, xl.sheet = "FemaleNames")
  df_male_names<-xl.read.file(fname, xl.sheet = "MaleNames")

  df_last_names<-df_last_names[order(df_last_names$Rank),]
  df_male_names<-df_male_names[order(df_male_names$Rank),]
  df_female_names<-df_female_names[order(df_female_names$Rank),]
}

sex<-sample(x=2,size=n_names,replace=T)
sexes<-sex_list[sex]

################################
#
#   last name

tops<-0

t<-sapply(df_last_names$OccPer100K, function(occ) {
  tops<<-tops+occ*10
  tops
})

df_last_names$top<-t

topest<-tops

rnd<-sample(x = topest,size = n_names,replace = T)

nmlast<-sapply(rnd,function(r) {
  ws<-which(df_last_names$top<r)
  w<-max(ws)

  df_last_names$Name[w]
})

############################################
#
# first names Male

tops<-0

t<-sapply(df_male_names$Number, function(n) {
  tops<<-tops+n
  tops
})

df_male_names$top<-t

topest<-tops

rnd<-sample(x = topest,size = n_names,replace = T)

nmmales<-sapply(rnd,function(r) {
  ws<-which(df_male_names$top<r)
  w<-max(ws)

  df_male_names$Name[w]
})

###################################################
#
#   First name - Female

tops<-0

t<-sapply(df_female_names$Number, function(n) {
  tops<<-tops+n
  tops
})

df_female_names$top<-t

topest<-tops
rnd<-sample(x = topest,size = n_names,replace = T)

nmfemales<-sapply(rnd,function(r) {
  ws<-which(df_female_names$top<r)
  w<-max(ws)



  df_female_names$Name[w]
})

yn<-(sexes=="Male")

first_names<-character(n_names)
first_names[yn]<-nmmales[yn]
first_names[!yn]<-nmfemales[!yn]

df<-data.frame(last_name=nms,first_name=first_names, sex=sexes)

df


con<-RODBC::odbcConnect(dsn = "AbsPlusHelper",pwd = "C4nc3r")
qryRes<- RODBC::sqlQuery(channel = con,query = "SELECT LastName,FirstName.Sex FROM ABSTRACTS")

