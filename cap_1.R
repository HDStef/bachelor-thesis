library(dplyr)
library(ggplot2)

dati <- read.csv('set_dati.csv')

table(dati$is_fraud)
dim(dati)
str(dati)

# Ricodifica variabili
dati$is_fraud<- factor(dati$is_fraud,levels=c(0,1),labels=c("no", "si"))
dati$city_pop <- as.numeric(dati$city_pop)
dati$trans_date_trans_time <- as.POSIXct(dati$trans_date_trans_time, format = "%Y-%m-%d %H:%M:%S")
dati$category <-as.factor(dati$category)
dati$state <- as.factor(dati$state)
dati$job <- as.factor(dati$job)
dati$dob <- as.Date(dati$dob, "%Y-%m-%d")
str(dati)
summary(dati)

# is_fraud
# t=table(dati$is_fraud) # problema dataset non bilanciato
# t
# barplot(t)
# barplot(t/length(dati$is_fraud), col = 'lightblue', ylim = c(0, 1), xlab = 'Transazione Fraudolenta', main = 'Composizione del set di dati')
# 
# 
# t/length(dati$is_fraud)

dati %>%
  group_by(is_fraud) %>%
  summarise(count = n()) %>%
  mutate(relative_frequency = count / sum(count)) %>%
  ggplot(aes(x = is_fraud, y = relative_frequency, fill = is_fraud)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#8ecae6", "#219ebc")) +
  labs(title = "Frequenze relative di is_fraud", y = "Frequenze relative", x = "is_fraud") +
  theme_minimal()



# # Creo un grafico a barre per ogni categoria di 'category'
# ggplot(dati, aes(x = is_fraud, fill = factor(category))) +
#   geom_bar(position = "stack", stat = "count") +
#   facet_wrap(~category, scales = "free_x", ncol = 2) +
#   labs(title = "Distribuzione di is_fraud per ogni categoria di 'category'",
#        x = "is_fraud",
#        y = "Conteggio")

# Raggruppo categorie in base alla distribuzione
dati$categoria_trans <- case_when(
  dati$category %in% c("grocery_pos", "misc_pos", "shopping_pos") ~ "pos",
  dati$category %in% c("misc_net", "shopping_net",'grocery_net') ~ "internet",
  dati$category %in% c("entertainment", 'gas_transport','home','personal_care','food_dining','health_fitness','kids_pets','travel') ~ "altro",
)

dati$categoria_trans <- factor(dati$categoria_trans)
chisq.test(dati$is_fraud, dati$categoria_trans)

a=table(dati$categoria_trans) # distribuzione doppia
a
prop.table(a)

dati %>%
  ggplot(aes(x = categoria_trans, fill = is_fraud)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#003566","#ffc300")) +  # Specify your desired colors
  theme(axis.text.x = element_text(size = 10), panel.background = element_rect(fill = "white")) +
  labs(y = "Frequenze relative")

# Raggruppo la variabile state
dati$region <- case_when(
  dati$state %in% c("CT", "ME", "MA", "NH", "RI", "VT", "NY", "NJ", "PA") ~ "Est",
  dati$state %in% c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD") ~ "Nord",
  dati$state %in% c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "AL", "AR", "KY", "LA", "MS", "OK", "TN", "TX") ~ "Sud",
  dati$state %in% c("AK", "CA", "HI", "NV", "OR", "WA", "AZ", "CO", "ID", "MT", "NM", "UT", "WY") ~ "Ovest",
)
dati$region <- factor(dati$region)

a <- table(dati$region)
prop.table(a)

dati %>% 
  ggplot(aes(x = region, fill = is_fraud)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_manual(values = c("#003566","#ffc300")) +  # Specify your desired colors
  theme(axis.text.x = element_text(size = 10), panel.background = element_rect(fill = "white")) +
  labs(y = "Frequenze relative")



chisq.test(dati$is_fraud, dati$region)

# job variable

chisq.test(dati$is_fraud, dati$job)
unique(dati$job)

dati <- dati %>%
  mutate(jobs_group = case_when(
    grepl("developer|engineer|materials|architect", job, ignore.case = TRUE) ~ "Tecnico",
    grepl("accountant|finance", job, ignore.case = TRUE) ~ "Finanza",
    grepl("nurse|doctor|optometrist|ophthalmologist|orthoptistmental|health nurse|nurse, children's|nurse, hospital|nurse, mental health|paediatric nurse|oncologist|Community pharmacist", job, ignore.case = TRUE) ~ "Assistenza Sanitaria",
    grepl("teacher|professor|Lecturer, higher education|Librarian, academic", job, ignore.case = TRUE) ~ "Educazione",
    grepl("producer, radio|producer, television/film/video|production assistant, television|production assistant, radio", job, ignore.case = TRUE) ~ "Arte e ruoli creativi",
    grepl("public house manager|catering manager|restaurant manager|chef|commercial/residential surveyor|quantity surveyor|wellsite geologist|public librarian|probation officer|event organiser|aeronautical engineer|therapist, horticultural|investment banker, corporate|psychotherapist, child|research officer, trade union|engineer, electronics|armed forces training and education officer|herpetologist|airline pilot|architectural technologist|toxicologist|land/geomatics surveyor|naval architect|camera operator|therapist, music|academic librarian|energy engineer", job, ignore.case = TRUE) ~ "Gestione e Servizi",
    grepl("call centre manager|child psychotherapist|paramedic|therapist, art|exhibition designer|sports development officer|medical physicist|dance movement psychotherapist|arts development officer|editor, magazine features|analytical chemist|applications developer|chartered public finance accountant|engineer, aeronautical|scientist, audiological|lexicographer|radio broadcast assistant|surveyor, rural practice|herbalist|librarian, academic|immigration officer|community pharmacist|trading standards officer|public relations account executive|fine artist|museum/gallery conservator|travel agency manager|tax inspector|barista|lecturer, higher education|therapist, sports|podiatrist|surgeon|museum education officer|retail merchandiser|magazine features editor|landscape architect|tourism officer|historic buildings inspector/conservation officer|archaeologist", job, ignore.case = TRUE) ~ "Servizi e arte",
    grepl("water engineer|engineer, land|communications engineer|physicist, medical|exercise physiologist|occupational hygienist|petroleum engineer|licensed conveyancer|pathologist|psychologist, clinical|audiological scientist|multimedia programmer|physiotherapist|sub|amenity horticulturist|fisheries officer|occupational psychologist|barrister|comptroller|operations geologist|training and development officer|town planner|administrator, charities/voluntary organisations|hydrologist|trade mark attorney|furniture designer|sales executive|advertising account planner|equities trader|accountant, chartered public finance|research scientist (physical sciences)|psychiatrist|radiographer, therapeutic|accounting technician|agricultural consultant|tourist information centre manager|physiological scientist|horticulturist, commercial|research scientist (medical)|counsellor|financial trader|sport and exercise psychologist|interpreter|surveyor, minerals|animal nutritionist|primary school teacher|psychotherapist|records manager|dealer|claims inspector/assessor|retail banker|company secretary|designer, interior/spatial|teacher, English as a foreign language|film/video editor|scientific laboratory technician|farm manager|therapist, occupational|education officer, museum|accountant, chartered certified|retail buyer", job, ignore.case = TRUE) ~ "Ingegneria e Amministrazione",
    grepl("administrator, arts|drilling engineer|mining engineer|engineer, technical sales|illustrator|designer, ceramics/pottery|administrator|building surveyor|market researcher|human resources officer|teacher, special educational needs|engineer, biomedical|facilities manager|social researcher|heritage manager|radio producer|make|chief technology officer|scientist, research (maths)|scientist, research (physical sciences)|mechanical engineer|media buyer|glass blower/designer|IT trainer|health promotion specialist|systems analyst|tree surgeon|musician|sales professional, IT|waste management officer|insurance risk surveyor|hospital doctor|engineer, control and instrumentation|structural engineer|higher education careers adviser|network engineer|scientist, biomedical|further education lecturer|pension scheme manager|music tutor|quarry manager|learning disability nurse|development worker, community|mudlogger|surveyor, land/geomatics|lecturer, further education|prison officer|futures trader|engineer, civil (contracting)|pensions consultant|secondary school teacher|intelligence analyst|teacher, secondary school|electrical engineer|theatre director|general practice doctor|regulatory affairs officer|English as a second language teacher|copywriter, advertising|risk analyst|ceramics designer|chartered loss adjuster|presenter, broadcasting|archivist|armed forces logistics/support/administrative officer|forensic psychologist|senior tax professional/tax inspector|insurance underwriter|surveyor, mining|investment analyst|biomedical scientist|geologist, engineering|insurance broker|pharmacist, community|occupational therapist|engineer, petroleum|gaffer|television production assistant|manufacturing engineer|emergency planning/management officer|arboriculturist|contractor|scientist, clinical (histocompatibility and immunogenetics)|restaurant manager, fast food|programme researcher, broadcasting/film/video|teacher, early years/pre|ambulance person", job, ignore.case = TRUE) ~ "Altro",
    TRUE ~ "Altro"
  ))

dati$jobs_group <- as.factor(dati$jobs_group)
chisq.test(dati$is_fraud, dati$jobs_group)


a=table(dati$jobs_group) # distribuzione doppia
a
prop.table(a)

a=table(dati$is_fraud,dati$jobs_group) # distribuzione doppia
a
prop.table(a,2)

dati %>%
  ggplot(aes(x = jobs_group, fill = is_fraud)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10)) +
  scale_fill_manual(values = c("#003566", "#ffc300")) +  
  theme(panel.background = element_rect(fill = "white")) +
  labs(y = "Frequenze Relative")


# city_pop and city
# dati$pop_group <- cut(dati$city_pop,
#                       breaks = c(-Inf, quantile(dati$city_pop, 1/3), quantile(dati$city_pop, 2/3), Inf),
#                       labels = c("Piccola", "Media", "Grande"),
#                       include.lowest = TRUE)


soglie <- c(-Inf, 1000, 100000, 250000, 1000000, Inf)
etichette <- c("Densità molto bassa", "Densità bassa", "Densità medio-inferiore", "Densità superiore", "Densità Alta")
dati$pop_group <- cut(dati$city_pop, breaks = soglie, labels = etichette)


chisq.test(dati$is_fraud, dati$pop_group)

a=table(dati$pop_group) # distribuzione doppia
a
prop.table(a)

dati %>% 
  ggplot(aes(x = pop_group, fill = is_fraud)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10)) +
  scale_fill_manual(values = c("#003566","#ffc300")) +  # Specify your desired colors
  theme(axis.text.x = element_text(size = 10), panel.background = element_rect(fill = "white")) +
  labs(y = "Frequenze relative")


# Amount
# summary(dati$amt)
# tapply(dati$amt, dati$is_fraud, mean)
# tapply(dati$amt, dati$is_fraud, sd)
# 
# soglie <- c(-Inf, 50, 250, 500, 2000, +Inf)
# dati$amt_category <- cut(dati$amt, breaks = soglie, labels = c("A", "B", "C", "D", "E"))


dati$amt_category <- cut(dati$amt,
                         breaks = quantile(dati$amt, probs = seq(0, 1, by = 1/3)),
                         labels = c("Piccola", "Media", "Grande"),
                         include.lowest = TRUE)


table(dati$is_fraud, dati$amt_category)

# old<-c("Molto Piccola", "Piccola", "Media", "Grande")
# new<-factor(c("Piccola", "Piccola", "Media", "Grande"))
# 
# dati$amt_category <-new[match(dati$amt_category,old)]

# category_avg_amt <- dati %>%
#   group_by(category) %>%
#   summarise(avg_amt_category = mean(amt))
# 
# # Step 2: Merge the average transaction amount back to the original data frame
# dati <- left_join(dati, category_avg_amt, by = "category")
# 
# # Step 3: Calculate the deviation of each transaction amount from the average for the category
# dati <- dati %>%
#   mutate(deviation_from_category_avg = amt - avg_amt_category)
# 
# dati$dev_category <- cut(dati$deviation_from_category_avg,
#                          breaks = quantile(dati$deviation_from_category_avg, probs = seq(0, 1, by = 1/3)),
#                          labels = c("Piccola", "Media", "Grande"),
#                          include.lowest = TRUE)
# 
# table(dati$is_fraud, dati$dev_category)
# 
# # Step 1: Calculate quartiles for each category
# category_quartiles <- dati %>%
#   group_by(category) %>%
#   summarise(Q1 = quantile(amt, 0.25),
#             Q3 = quantile(amt, 0.75))
# 
# # Step 2: Calculate interquartile range (IQR) for each category
# category_quartiles <- category_quartiles %>%
#   mutate(IQR = Q3 - Q1)
# 
# # Step 3: Merge category quartiles back to the original data frame
# dati <- left_join(dati, category_quartiles, by = "category")
# 
# # Step 4: Identify outliers
# dati$outliers <- dati$amt < dati$Q1 - 1.5 * dati$IQR | dati$amt > dati$Q3 + 1.5 * dati$IQR
# dati$outliers <- as.factor(dati$outliers)
# 
# # Now 'outliers' contains TRUE for transactions considered outliers, FALSE otherwise
# 
# table(dati$is_fraud, dati$outliers)
# chisq.test(dati$is_fraud, dati$outliers)


# # Creazione categorie usando cut()
# dati$amt_category <- cut(dati$amt,
#                          breaks = c(-Inf, quantile(dati$city_pop, 1/4), quantile(dati$city_pop, 1/2), Inf),
#                          labels = c("Piccola", "Media", "Grande"),
#                          include.lowest = TRUE)


chisq.test(dati$is_fraud, dati$amt_category)

a=table(dati$amt_category) # distribuzione doppia
a
prop.table(a)


# dati %>%
#   group_by(amt_category) %>% 
#   summarise(count = n()) %>%
#   mutate(relative_frequency = count / sum(count)) %>%
#   ggplot(aes(x = amt_category, y = relative_frequency, fill = amt_category)) +
#   geom_col(position = "stack") +
#   scale_fill_manual(values = c("#fb8500", "#ffb703", "#023047")) +
#   labs(title = "Frequenze relative di amt_category", y = "Frequenze relative", x = "Dimensione transazione") +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   theme_minimal()


a=table(dati$is_fraud,dati$amt_category) # distribuzione doppia
a
prop.table(a,2)

dati %>% 
  ggplot(aes(x = amt_category, fill = is_fraud)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_manual(values = c("#003566","#ffc300")) +  # Specify your desired colors
  theme(axis.text.x = element_text(size = 10), panel.background = element_rect(fill = "white")) +
  labs(y = "Frequenze relative")



# Istogramma della variabile amt

ggplot(dati, aes(x = amt)) +
  geom_histogram(aes(y = ..density..), binwidth = 30, colour = "black", fill = "#0096FF") +
  geom_density(fill = "red", alpha = 0.5, color = "black") +
  xlim(c(0, 2000)) +
  labs(title = "Densità del valore monetario per transazione",
       x = "Amt",
       y = "Densità") +
  theme_minimal()


# boxplot(amt~is_fraud,data=dati, xlab="Fraudolenta", ylab="Ammontare",col="light green")

# t.test(amt~is_fraud, data=dati)

data_fraud <- subset(dati, dati$is_fraud == 'si')
summary(data_fraud$amt)

# Calcolo età proprietario carta di credito

calculate_age <- function(born) {
  today <- as.Date("2024-02-04")
  age_in_days <- difftime(today, born, units = "days")
  age_in_years <- as.numeric(age_in_days) / 365.25     # tengo conto anni bisestili
  return(floor(age_in_years))
}

dati$age <- sapply(dati$dob, calculate_age)

age_avg_amt <- dati %>%
  group_by(age) %>%
  summarise(avg_amt_age = mean(amt))

# Step 2: Merge the average transaction amount back to the original data frame
dati <- left_join(dati, age_avg_amt, by = "age")

# Step 3: Calculate the deviation of each transaction amount from the average for the category
dati <- dati %>%
  mutate(deviation_from_age_avg = amt - avg_amt_age)

dati$dev_age <- cut(dati$deviation_from_age_avg,
                    breaks = quantile(dati$deviation_from_age_avg, probs = seq(0, 1, by = 1/5)),
                    labels = c("Molto Moderata", "Moderata", "Media",  "Alta", "Molto Alta"),
                    include.lowest = TRUE)

table(dati$is_fraud, dati$dev_age)




# 
# summary(dati$age)
# 
# tapply(dati$age, dati$is_fraud, mean)
# tapply(dati$age, dati$is_fraud, sd)
# 
# boxplot(age~is_fraud,data=dati, xlab="fraudolenta", ylab="età", col="red")
# 
# t.test(age~is_fraud, data=dati)

# raggruppo per età
dati_by_dob <- table(dati$age, dati$is_fraud)

ages <- c(35, 70)

# Creo una variabile 'age_category' con le categorie giovane-adulto-anziano
dati$age_category <- ifelse(dati$age <= ages[1], "giovane",
                                ifelse(dati$age <= ages[2], "adulto",
                                       ifelse(dati$age > ages[2], "anziano", "anziano")))

dati$age_category<-as.factor(dati$age_category)
a = table(dati$age_category)
prop.table(a)

dati %>% 
  ggplot(aes(x = age_category, fill = is_fraud)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_manual(values = c("#003566","#ffc300")) +  # Specify your desired colors
  theme(axis.text.x = element_text(size = 10), panel.background = element_rect(fill = "white")) +
  labs(y = "Frequenze relative")


a=table(dati$is_fraud, dati$age_category)
prop.table(a,2)
chisq.test(dati$is_fraud, dati$age_category)

# city pop
summary(dati$city_pop)  
sd(dati$city_pop)       

tapply(dati$city_pop, dati$is_fraud, mean)
tapply(dati$city_pop, dati$is_fraud, sd)

# boxplot(dati$city_pop~is_fraud,data=dati, xlab="Fraudolenta", ylab="Popolazione",col="light green")

t.test(dati$city_pop~is_fraud, data=dati) # non rifiuto h0
summary(dati$city_pop)



# pop <- c(1000000, 1500000, 2500000)
# dati$pop_category <- ifelse(dati$city_pop < pop[1], "bassa_popolazione",
#                             ifelse(dati$city_pop < pop[2], "media_popolazione",
#                                    ifelse(dati$city_pop <= pop[3], "alta_popolazione", "alta_popolazione")))
# 
# dati$pop_category <- as.factor(dati$pop_category)
# table(dati$pop_category)
# 
# a=table(dati$is_fraud, dati$pop_category) 
# prop.table(a,2)
# chisq.test(dati$is_fraud,dati$pop_category)



# category

table(dati$category)
chisq.test(dati$is_fraud, dati$category) # non sono indipendenti

# giorno e notte
dati <- na.omit(dati) # tolgo le ultime 33 osservazioni con data e ora NA
dati$orario_bin <- ifelse(as.numeric(format(as.POSIXct(dati$trans_date_trans_time), format = "%H")) >= 06 & as.numeric(format(as.POSIXct(dati$trans_date_trans_time), format = "%H")) < 18 , "giorno", 'notte')
dati$orario_bin <- as.factor(dati$orario_bin)
chisq.test(dati$is_fraud,dati$orario_bin)

a <- table(dati$orario_bin)
prop.table(a)

dati %>% 
  ggplot(aes(x = orario_bin, fill = is_fraud)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_manual(values = c("#003566","#ffc300")) +  # Specify your desired colors
  theme(axis.text.x = element_text(size = 10), panel.background = element_rect(fill = "white")) +
  labs(y = "Frequenze relative")



# # Densità del tempo di transazione
# ggplot(data_fraud, aes(x = as.numeric(format(trans_date_trans_time, "%H")))) +
#   geom_histogram(binwidth = 1, fill = "darkblue", color = "black", alpha = 0.7) +
#   xlim(0, 24) +  # Set the x-axis limits
#   labs(title = "Distribuzione oraria delle transazioni fraudolente", x = "Ora della giornata", y = "Conteggio") +
#   theme_minimal()

ggplot(data_fraud, aes(x = as.numeric(format(trans_date_trans_time, "%H")))) +
  geom_histogram(binwidth = 1, fill = "darkblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), fill = "red", alpha = 0.5, color = "black") +
  xlim(0, 24) +
  labs(title = "Distribuzione oraria delle transazioni fraudolente", x = "Ora della giornata", y = "Conteggio") +
  theme_minimal()



shopping_offline <- data_fraud %>% filter(category == c("shopping_pos", 'misc_pos', 'grocery_pos'))
shopping_online <-  data_fraud %>% filter(category == c("shopping_net", 'misc_net', 'grocery_net'))


# Densità transazione fraudolente shopping offline
ggplot(shopping_offline, aes(x = as.numeric(format(trans_date_trans_time, "%H")))) +
  geom_histogram(binwidth = 1, color = "black", fill = "#023e8a", alpha = 0.7) +
  geom_density(aes(y = ..count..), fill = "red", alpha = 0.5, color = "black") +
  xlim(0, 24) +
  labs(title = "Distribuzione oraria transazioni CP", x = "Ora della giornata", y = "Conteggio") +
  theme_minimal()

# Densità transazione fraudolente shopping online
ggplot(shopping_online, aes(x = as.numeric(format(trans_date_trans_time, "%H")))) +
  geom_histogram(binwidth = 1, color = "black", fill = "#caf0f8", alpha = 0.7) +
  stat_density(aes(y = ..count..), fill = "red", alpha = 0.5, color = 'black') +
  xlim(0, 24) +
  labs(title = "Distribuzione oraria transazioni CNP", x = "Ora della giornata", y = "Conteggio") +
  theme_minimal()


#### Transazioni fraudolente per categoria negozio
# groupcat <- aggregate(is_fraud ~ category, data = data_fraud, FUN = length)
# 
# ggplot(groupcat, aes(x = category, y = is_fraud)) +
#   geom_bar(stat = "identity", fill = "orange") +
#   labs(title = "Transazioni fraudolente per categoria di negozio",
#        x = "Categoria",
#        y = "Conteggio di is_fraud") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# 

total_transactions <- nrow(dati)

# Calculate the percentage of fraud and non-fraud transactions for each category
category_summary <- dati %>%
  group_by(category, is_fraud) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / total_transactions * 100)

ggplot(category_summary, aes(x = category, y = percentage, fill = factor(is_fraud))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("lightblue", "salmon"), name = "is_fraud",
                    labels = c("Non fraudolenta", "Fraudolenta")) +
  labs(title = "Percentuale transazioni fraudolente e non per categoria di negozio",
       x = "Categoria",
       y = "Percentuale") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10), panel.background = element_rect(fill = "white"))


#### Transazioni fraudolente per Stato
# state_summary <- dati %>%
#   group_by(state, is_fraud) %>%
#   summarise(count = n()) %>%
#   mutate(percentage = count / total_transactions * 100)
# 
# ggplot(state_summary, aes(x = state, y = percentage, fill = factor(is_fraud))) +
#   geom_bar(stat = "identity", position = "stack") +
#   scale_fill_manual(values = c("lightblue", "salmon"), name = "is_fraud",
#                     labels = c("Non fraudolenta", "Fraudolenta")) +
#   labs(title = "Percentuale transazioni fraudolente e non per stato",
#        x = "Stato",
#        y = "Percentuale") +
#   theme(axis.text.x = element_text(size = 10), panel.background = element_rect(fill = "white"))

dati %>% 
  ggplot(aes(x = state, fill = is_fraud)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_manual(values = c("#003566","#ffc300")) +  # Specify your desired colors
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10), panel.background = element_rect(fill = "white")) +
  labs(y = "Frequenze relative")

table(dati$is_fraud, dati$state)



library(caret)
indice <- createDataPartition(dati$is_fraud, p=0.75, list=FALSE)
train <- dati[indice,]   # training set, 75% delle osservazioni
test <- dati[-indice,]   # test set, 25% delle osservazioni


t=table(train$is_fraud)
t
t=table(test$is_fraud)
t

train <- train[,c('categoria_trans', 'jobs_group', 'pop_group', 'amt_category' ,'age_category', 'orario_bin', 'is_fraud')]

save(train, file = "data_training.RData")  # salvo il dataframe credit

test <- test[,c('categoria_trans', 'pop_group', 'jobs_group', 'amt_category','age_category', 'orario_bin', 'is_fraud')]

save(test, file = "data_test.RData")  # salvo il dataframe credit





