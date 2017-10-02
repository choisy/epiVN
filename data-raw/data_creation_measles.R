# Warning: "year_patch.txt" does not seem to be used here.

# Data from the South ----------------------------------------------------------
load("data-raw/measles/Cases rest of VN.RData")
south35 <- a
rm(a)

# selecting the variables of interest
sel <- c("Region", "Province", "Sex", "Date.of.birth", "Age", "cboAgeIn", "Rash",
         "Date.of.onset")
south35 <- south35[, sel]
rm(sel)

# Variable names
names(south35) <- c("region", "province", "gender", "birth", "age", "age_unit",
                    "rash","onset")

# Regions
south35$region <- tolower(as.character(south35$region))

# Dealing with accents of the provinces
tmp <- south35$province
accents <- c("\341\273\223", "\303\255", "\341\272\277", "\303\240",
             "\341\272\241", "\303\252", "\303\254", "\304\220", "\341\272\255",
             "\341\272\257", "\341\272\247", "\306\241", "\306\260", "\341\273\201",
             "\303\242", "\341\273\213", "\341\273\233", "\303\241", "\341\272\243",
             "\341\272\265", "\304\251", "\303\263", "\304\203", "\303\272",
             "\303\243", "\303\264")
noaccents <- c("o", "i", "e", "a", "a", "e", "i", "D", "a", "a", "a", "o", "u",
               "e", "a", "i", "o", "a", "a", "a", "i", "o", "a", "u", "a", "o")
for(i in seq_along(accents)) tmp <- gsub(accents[i], noaccents[i], tmp)

# renaming some provinces
tmp <- sub("Ba Ria- VT.", "Vung Tau - Ba Ria", tmp)
tmp <- sub("TP. Ho Chi Minh", "Tp. Ho Chi Minh", tmp)
tmp <- sub("TT- Hue", "Thua Thien - Hue", tmp)
south35$province <- trimws(tmp)

# Gender
tmp <- tolower(as.character(south35$gender))
south35$gender <- factor(sub("unknown", NA, tmp))

# Rash
hash <- c(Yes = TRUE, No = FALSE, Unknown = NA)
south35$rash <- hash[south35$rash]

# Provinces names:
south35$province <- dictionary::provinces[south35$province]

# Some reformating:
south35$age <- as.numeric(as.character(south35$age))
south35$age_unit <- factor(gsub("s", "", south35$age_unit))

# Data from the North ----------------------------------------------------------

library(readxl) # for "read_excel"
north28 <- data.frame(read_excel("data-raw/measles/Cases north VN.xlsx"))

# Selecting the variables of interest
sel <- c("sCaseID", "sLevel1", "dNotification", "sSex", "dBirth", "fAge",
         "iMeaslesClassif", "sAgeIn", "sVaccReceivedMeasles", "iVaccDosesMeasles",
         "dVaccLastDoseMeasles", "dOnsetFever", "dInvestigation", "dOnsetRash")
north28 <- north28[, sel]

# Variables names
names(north28) <- c("id", "province", "notification", "gender", "birth", "age",
                    "classification", "age_unit", "vaccination", "nb_doses",
                    "last_dose", "fever", "investigation", "rash_onset")

# Provinces
codes <- read_excel("data-raw/measles/GeoData.xlsx")
codes <- unique(codes[, c("sLevel1ID", "sLevel1Name")])
write.table(codes, "tmp.txt")
codes <- read.table("tmp.txt", TRUE, colClasses = "character")
file.remove("tmp.txt")
codes <- with(codes, setNames(sLevel1Name, sLevel1ID))
accents <- c("<U\\+00E0>", "<U\\+1EB1>", "<U\\+1EAF>", "<U\\+1EA3>", "<U\\+1EA1>",
             "<U\\+00E1>", "<U\\+00E2>", "<U\\+00EA>", "<U\\+1EC7>", "<U\\+1ECB>",
             "<U\\+0129>", "<U\\+00EC>", "<U\\+1ED9>", "<U\\+00F2>", "<U\\+01A1>",
             "<U\\+1ECD>", "<U\\+00FA>", "<U\\+01B0>", "<U\\+0110>")
noaccents <- c(rep("a", 7), "e", "e", rep("i", 3), rep("o", 4), "u", "u", "D")
for(i in seq_along(accents)) codes <- gsub(accents[i], noaccents[i], codes)
north28$province <- codes[north28$province]

# Gender
hash <- c(F = "female", M = "male", U = NA)
north28$gender <- factor(hash[north28$gender])

# Age unit
hash <- c(Y = "year", M = "month", D = "day")
north28$age_unit <- factor(hash[north28$age_unit])

# Vaccination
hash <- c(Y = TRUE, N = FALSE, U = NA)
north28$vaccination <- hash[north28$vaccination]
north28$nb_doses[which(north28$nb_doses > 4)] <- NA

# Classification symptoms
# (1: tested and confirmed; 2: tested and not measles; 3: test result pending;
#  4: no samples but symptoms compatible; 9: no sample and symptoms incompatible)
north28$symptoms <- north28$classification < 9
north28$tested <- north28$classification < 3
north28$confirmed <- north28$classification < 2
north28$classification <- NULL

# Dates
disease <- c("fever", "rash_onset", "notification", "investigation")
for(i in c("birth", "last_dose", disease))
  north28[, i] <- as.Date(north28[, i])

# Patches on dates
#toobig <- sapply(disease,function(x)which(north28[,x]>as.Date("2016-05-01")))   ## by hand
#toosmall <- sapply(disease,function(x)which(north28[,x]<as.Date("2011-04-01"))) ## by hand
patch <- read.table("data-raw/measles/patch.txt", TRUE, as.is = TRUE)
patch$date <- as.Date(patch$date)
for(i in 1:nrow(patch))
  north28[north28$id==patch[i, "id"], patch[i, "variable"]] <- patch[i, "date"]

# Provinces names:
north28$province <- dictionary::provinces[north28$province]

# Some reformating:
north28$age <- as.integer(north28$age)
north28$nb_doses <- as.integer(north28$nb_doses)

# Some tests -------------------------------------------------------------------

if(FALSE) {

# This function filters the cases where the day could be confounded with a month:
day_month <- function(x) x[with(x,which(day(fever)<13 &
                                        day(rash_onset)<13 &
                                        day(notification)<13 &
                                        day(investigation)<13)),]

# This function filters the cases where the months are differents:
different_months <- function(x) {
  tmp <- data.frame(lapply(disease,function(y)month(x[,y])))
  tmp <- apply(tmp,1,function(x)length(unique(x)))
  x[tmp>1,]
}

# This functions filters the cases where the months/days are the same/different:
test <- function(x,fct,crit) {
  tmp <- data.frame(lapply(disease,function(y)fct(x[,y])))
  tmp <- apply(tmp,1,function(x)length(unique(x)))
  if(crit=="identical") return(x[tmp<2,])
  else return(x[tmp>1,])
}

# Pipeline:
tmp <- north28 %>%
  day_month %>%
  test(month,"different") %>%
  test(day,"identical")

exchange_day_month <- function(x) sub("([0-9][0-9])-([0-9][0-9])$","\\2-\\1",x)

fct <- function(var,x) {
  tmp <- cbind(x[,c("id",var)],variable=var,stringsAsFactors=F)
  tmp <- tmp[,c(1,3,2)]
  names(tmp)[3] <- "date"
  tmp$date <- as.Date(tmp$date)
  tmp
}

rash_after_fever <- with(north28,which(rash_onset-fever>7))
rash_before_fever <- with(north28,which(fever-rash_onset>1))
notification_before_sympto <- with(north28,which(notification<fever & notification<rash_onset))

with(north28[-unlist(c(toobig,toosmall)),],
     hist(as.numeric(rash_onset-fever),1000,xlim=c(-5,10)))
with(north28[-unlist(c(toobig,toosmall)),],
     hist(as.numeric(notification-rash_onset),1000,xlim=c(-10,20)))
with(north28[-unlist(c(toobig,toosmall)),],
     hist(as.numeric(investigation-notification),1000,xlim=c(-5,5)))
with(north28[-unlist(c(toobig,toosmall)),],
     hist(as.numeric(notification-fever),1000,xlim=c(-5,20)))

}

# End of tests -----------------------------------------------------------------

devtools::use_data(north28, south35, overwrite = TRUE)
