###DATA PROJECT WITH Rstudio

#Task 1: Data cleaning

# a)

#Eφόσον έχω ένα αρχείο της μορφής txt,θα xρειαστούμε την εντολή read.table, μετά θα κάνουμε το εξής,
#θα πρέπει να ανοίξουμε το αρχείο μας για να δούμε αν περιλαμβάνει επικεφαλίδες ή όχι και δεύτερον
#πρέπει να βρούμε το file path, εφόσον το αρχείο περιλαμβάνει και επικεφαλίδες  θα βάλω ότι
#header=T και επίσης θα βάλουμε και τo file path άρα:


dataproject=read.table("C:/Users/sarel/OneDrive/Υπολογιστής/ΣΕΜΙΝΑΡΙΟ DATA ANALYST/project/dataproject.txt",header = T)
View(dataproject)

#Αντικατάσταση "*" με ΝΑ :

dataproject[dataproject == "*"] <- NA

#Eλεγχος τύπων δεδομένων :

str(dataproject)

#Μετατροπή αριθμητικών στηλών : 
#Βρίσκω ποιες στήλες έχουν missing values(NA)
dataproject$PRICE <- as.numeric(dataproject$PRICE)
dataproject$SQFT <- as.numeric(dataproject$SQFT)
dataproject$AGE <- as.numeric(dataproject$AGE)
dataproject$TAX <- as.numeric(dataproject$TAX)

#Μετατροπή δίτιμων(λογικών) μεταβλητών :
dataproject$NE <- as.integer(dataproject$NE)
dataproject$COR <- as.integer(dataproject$COR)

#Bρίσκω πόσα NA υπάρχουν ανά στήλη :
colSums(is.na(dataproject))

#Βρίσκω σε ποιες γραμμές υπάρχουν τα ΝΑ :
which(is.na(dataproject), arr.ind = TRUE)

#Σύνοψη δεδομένων, για να δω τα βασικά στατιστικά κάθε στήλης :
summary(dataproject)
str(dataproject)

# b)

#Για να μετατρέψεις τις δίτιμες μεταβλητές "NE" και "COR" σε κατηγορικές(factor) με 
#ετικέτες "Υes" και "Νο" θα πρέπει θα χρησιμοποιήσω την εντολή factor άρα :

dataproject$NE <- factor(dataproject$NE, levels = c(0,1), labels = c("NO","YES"))
dataproject$COR <- factor(dataproject$COR, levels = c(0,1), labels = c("NO", "YES"))

#Έλεγχος για να δω αν έχουν μετατραπεί τα 0 και το 1 με τις τιμές "Νο" και "Υes" :
head(dataproject$NE)
head(dataproject$COR)
str(dataproject)

# c)

#To rowSums(is.na(dataproject)) υπολογίζει το πλήθος των ΝΑ ανά γραμμή :
dataproject <- dataproject[rowSums(is.na(dataproject)) <2, ]

#Έλεγχος για τις γραμμές που έχουν τουλάχιστον 2 ΝΑ :
sum(rowSums(is.na(dataproject)) >= 2 )

#Μέγεθος του dataproject μετά την αφαίρεση των γραμμών που έχουν από 2 ΝΑ και πάνω :
dim(dataproject)

# d)

#Μετατροπή από τετραγωνικά πόδια(SQFT) σε τετραγωνικά μέτρα(SQM) :
dataproject$SQM <- dataproject$SQFT /10.764

#Διώχνω την στήλη SQFT άρα :
dataproject$SQFT <- NULL

#Έλέγχω το dataset οπότε :
#Eπιτρέπει να δω τα πρώτα δεδομένα και να επαληθεύσω την αλλαγή :
head(dataproject)
str(dataproject)

#e) Αρχικά θα φτιάξω ένα υποσύνολο των δεδομένων που δεν περιέχουν NA :
#Δημιουργία δείγματος χωρίς ΝΑ :
complete_data <- dataproject[complete.cases(dataproject), ]

#Θα εντοπίσω τις στήλες που έχουν ΝΑ και θα τρέξουμε ένα ΜΟΝΤΕΛΟ ΓΡΑΜΜΙΚΗΣ ΠΑΛΙΝΔΡΟΜΗΣΗΣ
#για καθεμία από αυτές, χρησιμοποιώντας τις υπόλοιπες μεταβλητές σαν ανεξάρτητες οπότε
# δημιουργώ μία λίστα με τις στήλες που περιέχουν ΝΑ άρα :
na_columns <- colnames(dataproject)[colSums(is.na(dataproject)) > 0]

#Βρόχος για κάθε στήλη με ΝΑ άρα :
for (col in na_columns) {
  #Δημιουργία τύπου του μοντέλου(AGE ~ .) :
  formula <- as.formula(paste(col, "~ . "))
  
  #Tρέξιμο του μοντέλου γραμμικής παλινδρόμησης :
  model <- lm(formula, data = complete_data)
  
  #Υπολογισμός των fitted values για τις γραμμές με ΝΑ :
  na_rows <- which(is.na(dataproject[[col]]))
  fitted_values <- predict(model, newdata = dataproject[na_rows, ])
  
  #Aντικατάσταση των ΝΑ με τα fitted values (στρογγυλοποίηση στις μονάδες)
  dataproject[[col]][na_rows] <- round(fitted_values)
  
}

#Έλεγχος για το αν υπάρχουν NA άρα:
colSums(is.na(dataproject))

#Task 2: Descriptive statistics and visualization  

#a)

#Περιγραφικά μέτρα για αριθμητικές μεταβλητές :
#To supply(dataproject, is.numeric) εντοπίζει τις αριθμητικές τιμές
summary_stats <- data.frame(
  Mean=sapply(dataproject[,  sapply(dataproject, is.numeric)], mean, na.rm = TRUE),
  Median=sapply(dataproject[,  sapply(dataproject, is.numeric)], median, na.rm = TRUE),
  SD=sapply(dataproject[,  sapply(dataproject, is.numeric)], sd, na.rm = TRUE),
  Min=sapply(dataproject[,  sapply(dataproject, is.numeric)], min, na.rm = TRUE),
  Max=sapply(dataproject[,  sapply(dataproject, is.numeric)], max, na.rm = TRUE)
  
)

summary_stats

#Περιγραφικά μέτρα για κατηγορικές μεταβλητές :
#To supply(dataproject, is.factor) εντοπίζει τις κατηγορικές μεταβλητές
#Χρησιμοποιώ τη συνάρτηση table για το πλήθος των παρατηρήσεων ανά κατηγορία
categorical_vars <- dataproject[, sapply(dataproject, is.factor)]
lapply(categorical_vars, table)

#Σχετικές συχνότητες για την μεταβλητή FEATS 
#Αρχικά υπολογίζω τις σχετικές συχνότητες, διαιρώντας τις συχνότητες με το πλήθος των παρατηρήσεων
feats_table <- table(dataproject$FEATS) #table δημιουργεί ΠΙΝΑΚΑ ΣΥΧΝΟΤΗΤΩΝ
feats_real_freq <- feats_table/sum(feats_table)

#Πίνακας Σχετικών Συχνοτήτων
data.frame(FEATS = names(feats_real_freq), Relative_Frequency = feats_real_freq)
sum(feats_table) #Συνολικά σε κάθε γραμμή του ΠΙΝΑΚΑ FEATS πόσες φορές εμφανίζεται η κάθε τιμή

#b)

#Για να δημιουργήσω ένα συγκεντρωτικό scatterplot για όλες τις αριθμητικές μεταβλητές
#θα χρησιμοποιήσω την συνάρτηση pairs.

#Eπιλογή μόνο των αριθμητικών μεταβλητών :
numeric_vars <- dataproject[, sapply(dataproject, is.numeric)]

#Δημιουργία SCATTERPLOT MATRIX με την συνάρτηση pairs :
pairs(numeric_vars, main = "Scatterplot Matrix των Αριθμητικών Μεταβλητών")

#Για να δω πιο ακριβής συσχετίσεις μεταξύ των μεταβλητών, η παρακάτω εντολή θα μας δώσει
#έναν πίνακα ΣΥΝΤΕΛΕΣΤΩΝ ΣΥΣΧΕΤΙΣΗΣ για τις αριθμητικές μεταβλητές. Αρα:
cor(numeric_vars, use = "complete.obs")

#c)

#Για το πολλαπλό γράφημα με τα barplots σχετικών συχνοτήτων των κατηγορικών και διακριτών
#μεταβλητών θα χρησιμοποιήσω την barplot(), όπου οι FEATS,COR,NE είναι τέτοιες μετ/τές:
#Kατασκευάζω πολλαπλό γράφημα για να βάλω πολλά plots σε ένα παράθυρο:par(mfrow=c(1,3)) άρα:

#Ορισμός παραθύρου για 3 παράθυρα:
par(mfrow=c(1,3))

#Barplot για τη μεταβλητή FEATS:
feats_freq <- prop.table(table(dataproject$FEATS))
barplot(feats_freq, main = "Σχετικές Συχνότητες FEATS", col="skyblue")

#Barplot για τη μεταβλητή COR:
cor_freq <- prop.table(table(dataproject$COR))
barplot(cor_freq, main = "Σχετικές Συχνότητες COR", col = "lightcoral")

#Barplot για τη μεταβλητή NE:
ne_freq <- prop.table(table(dataproject$NE))
barplot(ne_freq, main = "Σχετικές Συχνότητες NE", col = "lightgreen")

#Eπαναφορά του παραθύρου σε 1 plot:
par(mfrow=c(1,1))

#d) 

#Για να δημιουργήσω τα boxplots της μεταβλητής PRICE για κάθε κατηγορική μεταβλητή σε
#ένα γράφημα, θα χρησιμοποιήσω την εντολή par(mfrow=c(1,2)) επειδή είναι 2 οι κατηγορικές
par(mfrow=c(1,2), oma = c(0,0,2,0))

#Δημιουργώ τα boxplots με διαφορετικά χρώματα για κάθε κατηγορική μετ/τή (NE,COR) άρα:

boxplot(PRICE ~ NE, data = dataproject, col=rainbow(length(unique(dataproject$NE))),
        main = "PRICE by NE", xlab = "NE", ylab = "PRICE")

boxplot(PRICE~ COR, data = dataproject, col=rainbow(length(unique(dataproject$COR))),
        main = "PRICE by COR", xlab = "COR", ylab = "PRICE")

#Γενικός τίτλος για όλο το γράφημα:
mtext("Boxplots of PRICE for the categorical variables", outer = TRUE, cex = 1.5, font = 2)

#e)

#Αρχικά θα υπολογίσω την ΜΕΣΗ ΤΙΜΗ της μεταβλητής PRICE για κάθε κατηγορία των μετ/τών
# NE,COR.

#MΕΣΗ ΤΙΜΗ PRICE για κάθε επίπεδο της NE:
tapply(dataproject$PRICE, dataproject$NE, mean, na.rm = TRUE)

#MΕΣΗ ΤΙΜΗ PRICE για κάθε επίπεδο της COR:
tapply(dataproject$PRICE, dataproject$COR,mean, na.rm =TRUE)

#Tώρα για να δούμε αν οι μέσες τιμές διαφέρουν ΣΤΑΤΙΣΤΙΚΑ ΣΗΜΑΝΤΙΚΑ,κάνουμε t-test(ανεξάρτητοι πληθυσμοί)

#T-test για PRICE ανά NE:
t.test(PRICE ~ NE, data = dataproject)

#T-test για PRICE ανά COR:
t.test(PRICE ~ COR, data = dataproject)

#Αν p-value < 0.05,τότε ΥΠΑΡΧΕΙ στατιστικά σημαντική διαφορά μεταξύ των μέσων τιμών των δύο ομάδων
#Αν p-value > 0.05, τότε ΔΕΝ ΥΠΑΡΧΕΙ σημαντική διαφορά στις μέσες τιμές


#f)

#Aρχικά βρίσκω τις μεταβλητές που έχουν υψηλή θετική ασυμμετρία(skewness>1)
#Για να υπολογίσω την ασυμετρία(skewness) θα χρησιμοποιήσω την συνάρτηση skewness άρα:
install.packages("e1071")

#Φόρτωση πακέτου
library(e1071)

#Επιλογή μόνο αριθμητικών μεταβλητών:
numeric_vars <- dataproject[, sapply(dataproject, is.numeric)]

#Υπολογισμός ασυμμετρίας (skewness) για κάθε αριθμητική μεταβλητή:
skew_values <- sapply(numeric_vars, skewness, na.rm = TRUE)

#Βρίσκουμε τις μεταβλητές με skewness>1 :
high_skew_vars <- names(skew_values[skew_values >1])

#Aκολούθως θα ελένξουμε αν ακολουθούν κανονική κατανομή με Shapiro-Wilk test
#Θα κάνω ΕΛΕΝΧΟ ΚΑΝΟΝΙΚΟΤΗΤΑΣ(Shapiro-Wilk test) για κάθε μεταβλητή με υψηλή ασυμμετρία

#Tέστ κανονικότητας για τις μεταβλητές με υψηλή ασυμμετρία:
shapiro_results <- sapply(numeric_vars[high_skew_vars], function(x) shapiro.test(x)$p.value)

#Όλα τα p-values της γραμμής 244 των μεταβλητών PRICE,TAX,SQM είναι μικρότερα από 0.05
#επομένως οι μεταβλητές ΔΕΝ ΑΚΟΛΟΥΘΟΥΝ ΚΑΝΟΝΙΚΗ ΚΑΤΑΝΟΜΗ και χρειάζονται μετασχηματισμό

#Εφόσον η κατανομή δεν είναι κανονική, θα εφαρμόσουμε λογαριθμικό μετασχηματισμό(log())
#Aφού απορρίψαμε την κανονικότητα,λογαριθμούμε τις μεταβλητές, άρα βάζω το log μπροστά:

for (var in high_skew_vars) {
  dataproject[[paste0("log", var)]] <- log(dataproject[[var]] +1) #+1 για να αποφύγω τυχόν log(0)
  
}

#Ξανακάνω το ΤΕΣΤ ΚΑΝΟΝΙΚΟΤΗΤΑΣ για να δούμε αν ο μετασχηματισμός βελτίωσε την κατανομή.
#ΕΛΕΝΧΟΣ ΚΑΝΟΝΙΚΟΤΗΤΑΣ ΞΑΝΑ μετά το log, άρα τρέχω ξανά το Shapiro Wilk test στις λογαριθμημένες μετ/τές.

#Νέος έλενχος κανονικότητας μετά τον λογαριθμικό μετασχηματισμό :
log_shapiro_results <- sapply(dataproject[paste0("log", high_skew_vars)], function(x) shapiro.test(x)$p.value)
#Aν p-values>0.05 ΤΟΤΕ ΟΙ ΜΕΤΑΒΛΗΤΕΣ ΕΙΝΑΙ ΚΑΝΟΝΙΚΕΣ

#g)

#1.Scatterplot Matrix για τις αριθμητικές μεταβλητές (b ερώτημα)
#Χρησιμοποιώ το τελικό dataset, που τώρα περιέχει τις μεταβητές:logPRICE,logTAX,logSQM
#αντί για τις αρχικές PRICE,TAX,SQM :

#Επιλογή αριθμητικών μεταβλητών από το τελικό dataset:
numeric_vars_final <- dataproject[, sapply(dataproject, is.numeric)]

#Δημιουργία scatterplot matrix :
pairs(numeric_vars_final, main = "Scatterplot Matrix των ΝΕΩΝ ΑΡΙΘΜΗΤΙΚΩΝ ΜΕΤΑΒΛΗΤΩΝ")

#2.Boxplots της logPRICE ανά κατηγορική μεταβλητή (d ερώτημα)
#Πολλαπλό Boxplot για logPRICE ανά ΝΕ και COR
#Επίσης χρησιμοποιώ τις νέες λογαρηθμιμλενες τιμές αντί της PRICE άρα:

par(mfrow=c(1,2), oma = c(0,0,2,0)) #Ρύθμιση για πολλαπλά διαγράμματα

#Boxplot logPRICE ~ NE 
boxplot(logPRICE ~ NE, data = dataproject, col = rainbow(length(unique(dataproject$NE))),
        main = "logPRICE by NE", xlab = "NE", ylab = "logPRICE")

#Boxplot logPRICE ~ COR
boxplot(logPRICE ~ COR, data = dataproject, col = rainbow(length(unique(dataproject$COR))),
        main = "logPRICE by COR", xlab = "COR", ylab = "logPRICE")

#Γενικός τίτλος για το γράφημα :
mtext("Boxplots of logPRICE for the categorical variables", outer = TRUE, cex = 1.5, font = 2)

#Επαναφορά σε μονό γράφημα:
par(mfrow=c(1,1))

#Task 3: Data mining

#a)

#Θα τρέξουμε γραμμική παλινδρόμηση με εξαρτημένη μεταβλητή logPRICE και όλες τις άλλες
#μεταβλητές ως ανεξάρτητες.

#Εκτέλεση του μοντέλου γραμμικής παλινδρόμησης, χρησιμoποιώντας την συνάρτηση lm(), 
#τρέχω το μοντέλο με logPRICE ως εξαρτημένη μεταβλητή άρα:
model <- lm(logPRICE ~ ., data = dataproject)

#Προβολή αποτελεσμάτων:
summary(model)

#Aφαίρεση της μεταβλητής PRICE από τις ανεξάρτητες μεταβλητές :
model <-  lm(logPRICE ~ . -PRICE, data = dataproject)
summary(model)

#b)

#Εφαρμόζω τον ΑΛΓΟΡΙΘΜΟ ΕΠΙΛΟΓΗΣ ΜΕΤΑΒΛΗΤΩΝ Stepwise για να κρατήσουμε μόνο τις πιο 
#σημαντικές μεταβλητές στο μοντέλο (εξαρτημένη μεταβλητή logPRICE).
#Χρησιμοποιούμε τη step() για να βελτιστοποιήσουμε το μοντέλο, ξεκινώντας από το πλήρες
#μοντέλο και αφαιρώντας σταδιακά τις μη σημαντικές μεταβλητές.

#Ξεκινάμε με το πλήρες μοντέλο (όλες τις ανεξάρτητες μεταβλητές εκτός της PRICE) :
full_model <- lm(logPRICE ~ . -PRICE, data = dataproject)

#Εφαρμόζουμε Stepwise επιλογή με βάση το κριτήριο AIC :
stepwise_model <- step(full_model, direction = "both", trace = FALSE)

#Προβολή του τελικού μοντέλου :
summary(stepwise_model)

#ΔΙΑΓΝΩΣΤΙΚΑ ΓΡΑΦΗΜΑΤΑ και ΕΛΕΓΧΟΙ ΥΠΟΘΕΣΕΩΝ
#Για να ελέχξουμε αν το μοντέλο πληροί τις προυποθέσεις της ΓΡΑΜΜΙΚΗΣ ΠΑΛΙΝΔΡΟΜΗΣΗΣ
#εκτελώ διαγνωστικά τεστ.

#Ελεγχος κανονικότητας των υπολοίπων :
hist(residuals(stepwise_model), main = "Histogram of Residuals", col = "lightblue", breaks = 20)

#Q-Q Plot (Ελεγχος κανονικότητας) :
qqnorm(residuals(stepwise_model))
qqline(residuals(stepwise_model), col = "red")

#Eλεγχος Ετεροσκεδαστικότητας (Breusch-Pegan Test) :
library(lmtest)
bptest(stepwise_model)

#Ελεγχος Πολυσυγγραμμικότητας (VIF Test):
library(car)
vif(stepwise_model)

#c)

#Αρχικά θα δημιουργήσω τη νέα κατηγορική μεταβλητή catFEATS χρησιμοποιώντας την cut() άρα: 
dataproject$catFEATS <- cut(dataproject$FEATS,
                            breaks = c(-Inf, 3, 7, Inf),
                            labels = c("Low", "Moderate", "High"))

#Ελέγχω αν η μεταβλητή έχει δημιουργηθεί σωστά :
table(dataproject$catFEATS)

#Για να δω αν οι μέσες τιμές της logPRICE διαφέρουν μεταξύ των 3 επιπέδων της catFEATS,
#εφαρμόζω το μοντέλο ANOVA :
anova_model <- aov(logPRICE ~ catFEATS, data = dataproject)
summary(anova_model)

#Εφόσον το p-value=0.00081 της γραμμής 362 που είναι πολύ μικρότερο από (p-value<0.05),
#που σημαίνει ότι το ANOVA δείχνει ΣΤΑΤΙΣΤΙΚΑ ΣΗΜΑΝΤΙΚΗ ΔΙΑΦΟΡΑ, γι'αυτό θα χρησιμοποιήσω
#το TUKEY HSD Test για να δούμε ποιες ομάδες διαφέρουν μεταξύ τους:
TukeyHSD(anova_model)

#boxplot που θα απεικονίζει τη διασπορά της logPRICE ανά κατηγορία catFEATS, με το 
#boxplot οπτικοποιώ τις διαφορές μεταξύ των κατηγοριών:
boxplot(logPRICE ~ catFEATS, data = dataproject,
        col = c("lightblue","lightgreen","lightcoral"),
        main = "Διασπορά της logPRICE ανά κατηγορία catFEATS",
        xlab = "Κατηγορία catFEATS",
        ylab = "logPRICE")

#d)

#Για να καταλήξω στο ΒΕΛΤΙΣΤΟ ΜΟΝΤΕΛΟ ΛΟΓΙΣΤΙΚΗΣ ΠΑΛΙΝΔΡΟΜΗΣΗΣ(LOGISTIC REGRESSION MODEL)
#για τη μεταβλητή NE, θα κατασκευάσω το αρχικό LOGISTIC REGRESSION MODEL, η μετ/τή NE
#είναι δυαδική(π.χ 0 = όχι βορειοανατολικά, 1 = ναι βορειοανατολικά), οπότε χρησιμοποιούμε
#λογιστική παλινδρόμηση :

log_model <- glm(NE ~ PRICE + SQM + AGE + FEATS + COR + TAX, data = dataproject, family = binomial)
summary(log_model)

#Από την ανάλυση τις γραμμής 385 βλέπω ότι οι μεταβλητές: PRICE,COR έχουν p-value>0.05
#επομένως οι μεταβλητές αυτές δεν είναι σημαντικές, άρα πρέπει να τις αφαιρέσω απ'το dataset:
#Aφαιρώ τις μη σημαντικές μεταβλητές και ξανατρέχω το μοντέλο:

optimized_log_model <- glm(NE ~ SQM + AGE + FEATS + TAX, data = dataproject, family = binomial)
summary(optimized_log_model)

#Υπολογισμός Πιθανότητας:
new_house <- data.frame(SQM = 180, AGE = 15, FEATS = 5, TAX =1000)
predicted_prob <- predict(optimized_log_model, newdata = new_house, type = "response")
print(predicted_prob)

#Αφού predicted_prob>0.5 σημαίνει ότι το σπίτι είναι πιο πιθανό να ανήκει στην βορειοανατολική πλευρά(ΝΕ=1)


#e)

#Για να κατασκευάσω ένα Decision Tree για τη μεταβλητή COR, αρχικά θα φορτώσω τη βιβλιοθήκη,
#και θα κατασκευάσω το Decision Tree, χρησιομοποιώ τη βιβλιοθήκη rpart για να φτιάξω το δέντρο απόφασης:

#Φόρτωση της βιβλιοθήκης :
library(rpart)

#Κατασκευή του Decision Tree με εξαρτημένη μεταβλητή COR:
tree_model <- rpart(COR ~ PRICE + SQM + AGE + FEATS + NE + TAX, data = dataproject, method = "class")

#Προβολή του μοντέλου:
print(tree_model)

#Για να κάνω ΟΠΤΙΚΗ ΑΝΑΠΑΡΑΣΤΑΣΗ του μοντέλου, και να δω το δέντρο γραφικά θα χρησιμοποιήσω 
#τη βιβλιοθήκη rpart.plot :
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(tree_model, type = 4, extra = 101, box.palette = "Blues")
#H γραμμή 421 του διαγράμματος Decision Tree δείχνει πως οι μεταβλητές χωρίζουν τα δεδομένα
#για να προβλέψουν αν ένα σπίτι είναι γωνιακό ή όχι (COR = YES/NO)

#Υπολογισμός του ποσοστού σωστά εκτιμώμενων τιμών (Accuracy):

#Για να αξιολογήσουμε το μοντέλο, προβλέπουμε τις τιμές του COR στο ίδιο dataset και
#σωγκρίνουμε με τις πραγματικές τιμές:

#Προβλέψεις στο training set :
predicted_values <- predict(tree_model, type = "class")

#Yπολογισμός ακρίβειας (ποσοστό σωστών προβλέψεων)
accuracy <- sum(predicted_values == dataproject$COR)/nrow(dataproject)
print(accuracy)

#Η γραμμή 434 θα μας δώσει το ποσοστό των σωστών προβλέψεων (accuracy) του Decision Tree


#Πρόβλεψη για νέο σπίτι: Δημιουργούμε ένα νέο dataset με τις διαδεδομένες τιμές και 
#προβλέπουμε αν το σπίτι είναι γωνιακό (COR = YES ή COR = N0) :

new_house <- data.frame(PRICE = 1000, SQM = 150, AGE = 17, FEATS = 4, NE ="YES", TAX = 800)

#Πρόβλεψη για το νέο σπίτι :
predicted_COR <- predict(tree_model, newdata = new_house, type = "class")

#Το αποτέλεσμα της γραμμής 446 θα μας δείξει αν το σπίτι είναι πιθανό να είναι γωνιακό
#(COR = YES) ή (COR = NO)

sum(predicted_values == dataproject$COR)
length(predicted_values)
table(Predicted = predicted_values, Actual = dataproject$COR)

#Βοηθητική συνάρτηση για τον υπολογισμό του diff(γραμμή 367) :
aggregate(logPRICE ~ catFEATS, data = dataproject, FUN = mean)



