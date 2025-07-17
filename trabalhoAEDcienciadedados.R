if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("TCGAbiolinks")
library(tidyverse)
install.packages("tidyverse")
library(TCGAbiolinks)

query <- GDCquery(
  project = "TCGA-LGG", 
  data.category = "Clinical",
  data.type = "Clinical Supplement", 
  data.format = "BCR Biotab"
)
GDCdownload(query)
clinical.lgg <- GDCprepare(query)
names(clinical.lgg)

clinicalraw <- clinical.lgg[["clinical_patient_lgg"]]

clinicalraw <- as.data.frame(clinicalraw)
clinicalraw <- clinicalraw[-c(1,2),]  

# Mudando caracter para fator
clinicalraw <- clinicalraw %>%
  mutate_if(is.character, as.factor) %>%
  mutate(bcr_patient_barcode = as.character(bcr_patient_barcode))

# Mudando as numericas
clinicalraw <- clinicalraw %>%
  mutate(initial_pathologic_dx_year = as.numeric(initial_pathologic_dx_year))
clinicalraw <- clinicalraw %>%
  mutate(last_contact_days_to = as.numeric(last_contact_days_to))
clinicalraw <- clinicalraw %>%
  mutate(initial_pathologic_dx_year = as.numeric(initial_pathologic_dx_year))
clinicalraw <- clinicalraw %>%
  mutate(death_days_to = as.numeric(death_days_to))
clinicalraw <- clinicalraw %>%
  mutate(age_at_initial_pathologic_diagnosis = as.numeric(age_at_initial_pathologic_diagnosis))

save


View(clinicalraw)

names(clinicalraw)

glimpse(clinicalraw)
clinicalraw$tumor_tissue_site
colunasFinal <- c(
  "bcr_patient_barcode", "histologic_diagnosis", "tumor_grade",
  "laterality", "tumor_site", "supratentorial_localization",
  "gender", "race", "ethnicity",
  "history_other_malignancy", "history_neoadjuvant_treatment", "initial_pathologic_dx_year",
  "history_ionizing_rt_to_head", "history_seizures", "history_headaches",
  "symp_changes_mental_status", "symp_changes_visual", "symp_changes_sensory",
  "symp_changes_motor_movement", "related_symptom_first_present", "first_symptom_longest_duration",
  "history_neoadjuvant_steroid_tx", "history_neoadjuvant_medication", "vital_status",
  "last_contact_days_to", "death_days_to", "tumor_status",
  "family_history_cancer_indicator", "family_history_brain_tumor", "performance_status_timing",
  "radiation_treatment_adjuvant", "pharmaceutical_tx_adjuvant", "treatment_outcome_first_course",
  "new_tumor_event_dx_indicator", "age_at_initial_pathologic_diagnosis", "patient_id",
  "tissue_source_site", "tumor_tissue_site"
)




clinicalraw$treatment_outcome_first_course
levels(clinicalraw2$allergy_food_dx_indicator)
sum(clinicalraw2$allergy_food_dx_indicator == 'YES')

vetor <- colnames(clinicalraw)

clinicalraw2 <- clinicalraw2%>%
  select(-c(gender.x,tumor_tissue_site))

#excluir clinicalraw2$history_other_malignancy e tumor statur

clinicalraw2$history_other_malignancy <- as.numeric(clinicalraw2$pharmaceutical_tx_adjuvant)

levels(clinicalraw2$tumor_status)

clinicalraw2 <- clinicalraw2 %>%
  mutate(
    tumor_status = case_when(
      clinicalraw2$tumor_status == "TUMOR FREE" ~ 2,
      clinicalraw2$tumor_status == "[Not Available]" ~ 1,
      clinicalraw2$tumor_status == "[Unknown]" ~ 1,
      clinicalraw2$tumor_status == "[Discrepancy]" ~ 4,
      clinicalraw2$tumor_status == "WITH TUMOR" ~ 3
))



clinicalraw2[,]
names(clinicalraw2)

#  NAs
# allergy_food_dx_age 509
# allergy_animals_insects_dx_age 505
# allergy_food_dx_type 495
# allergy_animals_insects_dx_type 501
# allergy_animals_insects_dx_age 505
# disease_code 515
# allergy_animals_insects_dx_type 501
# inherited_genetic_syndrome_specified 511
# asthma_eczema_allergy_first_dx 480

# booleanas
# retrospective_collection 330YES 176NO 9NA
# prospective_collection 9NA 175YES 331NO
# allergy_food_dx_indicator  
# asthma_eczema_allergy_first_dx
# history_asthma 345NO 21YES 149NA
# history_eczema 105NA 344NO 12YES
# histor_hay_fever 37YES 303NO 175NA
# history_dust_mold_allergy 311NO 17YES 187NA
# idh1_mutation_found 390NA 34NO 91YES

# OUTRAS
#idh1_mutation_test_method 391NA 113IHC 11Sequence Analysis
# birth_days_to
#days_to_initial_pathologic_diagnosis

#icd_10 439C71.0 5 ->.1 6 ->.2 1 -> .3 1 -> .4 63 -> .9
#icd_o_3_site
#icd_o_3_histology '9451/3'>78, '9400/3 > 64, '9382/3' > 131, '9450/3' > 112, '9401/3' 130

# project_code
# informed_consent_verified
# performance_status_days_to
# karnofsky_score
# ecog_score
# bcr_patient_uuid


summary(clinicalraw)   

sum(clinicalraw$asthma_eczema_allergy_first_dx == "[Not Available]")
# checar todos os leveis e quantidades dentro deles para fazer tabela para aula 

# Removendo ids e variaveis com apenas NA
clinical_cleaned <- clinicalraw %>%
  select(-c('bcr_patient_uuid', 'retrospective_collection', 'prospective_collection', 'birth_days_to', 'allergy_food_dx_indicator', 
            'asthma_eczema_allergy_first_dx', 'allergy_food_dx_age', 'allergy_animals_insects_dx_age', 'allergy_food_dx_type', 'allergy_animals_insects_dx_type', 'icd_o_3_site', 'icd_o_3_histology',
            'inherited_genetic_syndrome_specified', 'inherited_genetic_syndrome_indicator', 'idh1_mutation_test_indicator', 'idh1_mutation_test_method', 'idh1_mutation_found', 'project_code'
            ,'informed_consent_verified', 'icd_o_3_site', 'icd_10', 'disease_code', 'days_to_initial_pathologic_diagnosis', 'karnofsky_score', 'ecog_score', 'performance_status_days_to', 'form_completion_date', 'history_asthma','history_eczema', 'histor_hay_fever', 
           'history_dust_mold_allergy', 'allergy_animals_insects_dx_indicator', 'history_dust_mold_allergy'))


clinical_cleaned <- clinicalraw2 %>%
  select(-c(days_to_tumor_progression, days_to_patient_progression_free, extranodal_involvement))

clinicalraw2 <- clinicalraw2 %>%
rownames_to_column("patient_id")

sum(clinical_cleaned$barretts_esophagus == '[Not Available]')
glimpse(clinical_cleaned)


# Usando essa mesma linha para mudar as numericas
clinical_cleaned$birth_days_to <- as.numeric(clinical_cleaned$birth_days_to)

names(clinical_cleaned)
glimpse(clinicalraw)
summary(clinicalraw
        )

#"bcr_patient_barcode"                 "histologic_diagnosis"                "tumor_grade"                        
# "laterality"                          "tumor_site"                          "supratentorial_localization"        
# "gender"                              "race"                                "ethnicity"                          
# "history_other_malignancy"            "history_neoadjuvant_treatment"       "initial_pathologic_dx_year"         
# "history_ionizing_rt_to_head"         "history_seizures"                    "history_headaches"                  
# "symp_changes_mental_status"          "symp_changes_visual"                 "symp_changes_sensory"               
# "symp_changes_motor_movement"         "related_symptom_first_present"       "first_symptom_longest_duration"     
# "history_neoadjuvant_steroid_tx"      "history_neoadjuvant_medication"      "vital_status"                       
# "last_contact_days_to"                "death_days_to"                       "tumor_status"                       
# "family_history_cancer_indicator"     "family_history_brain_tumor"          "performance_status_timing"          
# "radiation_treatment_adjuvant"        "pharmaceutical_tx_adjuvant"          "treatment_outcome_first_course"     
# "new_tumor_event_dx_indicator"        "age_at_initial_pathologic_diagnosis" "patient_id"                         
#"tissue_source_site"                  "tumor_tissue_site"                  # idh1_mutation_found 390NA 34NO 91YES

#muitos levels: tissue_source_site, raca (juntar not avaliable com not evaluated e unknown), 
levels(clinical_cleaned$related_symptom_first_present)

# Plots
# categoricos

# graficos de barra 
ggplot(clinical_cleaned, aes(x = first_symptom_longest_duration)) +
  geom_bar(alpha = 0.8, fill = "grey", color = "black") +
  labs(title = "Tempo de duração do primeiro sintoma",
       x = "Tempo",
       y = "Contagem de Pacientes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 12, face = "bold"))

# comparação entre variaveis categoricas usando grafico de contagem
ggplot(clinical_cleaned, aes(x = vital_status, y = histologic_diagnosis)) +
  geom_count()

# gerando tabela para ver as contagens explicitamenter 
histtable <- table(clinical_cleaned$histologic_diagnosis, clinical_cleaned$vital_status, exclude = NA)
print(histtable)

# histogramas
ggplot(clinical_cleaned, aes(x = last_contact_days_to)) +
  geom_histogram(bins = 20, aes(fill = ..x..), color = "white", alpha = 0.8) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Histograma de último acompanhamento",
       x = "Dias",
       y = "Frequência") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank()  
  )

# boxplot categorico x numerico 
ggplot(clinical_cleaned, aes(x = vital_status, y = age_at_initial_pathologic_diagnosis)) +
  geom_boxplot() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Status vital x idade de diagnóstico",
       x = "Status Vital",
       y = "Idade") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank()) +
    stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black") 
  
# testes estatisticos e correlação

t.test(age_at_initial_pathologic_diagnosis  ~ vital_status, data = clinical_cleaned)

# salvando dataset limpo
write_csv(clinicalraw2, "lgg.clinicCLUSTER.csv")


# Analise de sobrevivencia
# Usaremos kaplan mayer, varaivel count_days será a variavel de tempo
# onde os pacientes vivos terão o valor de dias do ultimo contato
# os pacientes mortos terão valores de dias para a morte 

glimpse(clinicalraw)

library(survminer)
library(survival)
clinicalraw <- clinicalraw%>%
  mutate(gender = as.factor(gender))
clinicalraw <- clinicalraw%>%
  mutate(vital_status = ifelse(vital_status == 'Dead',1,0))

levels(clinicalraw$histologic_diagnosis)

names(clinicalraw)

clinicalraw2 <- clinicalraw %>%
  filter(
    symp_changes_motor_movement != "[Not Available]",
    symp_changes_motor_movement != "[Unknown]"
)

clinicalraw2 <- clinicalraw2 %>% 
  mutate(count_days = case_when(
    vital_status == 0 ~ last_contact_days_to,
    vital_status == 1 ~ death_days_to
)
)

levels(clinicalraw2$symp_changes_visual)

fit <- survfit(Surv(count_days, vital_status) ~ symp_changes_motor_movement, data =clinicalraw2)
ggsurvplot(fit,
           pval = TRUE, conf.int = FALSE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#FF7F00", "#E41A1C", "#1F78B4", "#6A3D9A", "#33A02C"))

?(palette)
citation(package = 'TCGAbiolinks')

write_csv(clinicalraw2, "lgg.clinicCLUSTER.csv")

#CLuSTEZIRAÇÃO


clinicalraw3 <- clinicalraw2
rownames(clinicalraw3) <- clinicalraw2[,1]

clinicalraw3 <- clinicalraw3[,-1]

clinicalraw3$tissue_source_site <- as.numeric(clinicalraw3$tissue_source_site)

clinicalraw3 <- clinicalraw3%>%
  select(-c(patient_id, bcr_patient_barcode))

## fazendo a codificação para as variáveis nessa etapa
clinicalraw3 <- clinicalraw3 %>%
  mutate(
    vital_status = case_when(
      clinicalraw2$vital_status == "Alive" ~ 1,
      clinicalraw2$vital_status == "Dead" ~ 2,
))

## normalizando 

clinicalraw4 <- clinicalraw3 %>%
  scale()# centraliza os dados em torno da média e divide pelo desvio padrao para cada coluna 

## usando k means 

kmeans(clinicalraw3, centers = 4, iter.max = 100, nstart = 100)

## analisando o melhor valor de k com factorextra
library(factoextra)
fviz_nbclust(clinicalraw3, kmeans, method = "wss")
fviz_nbclust(clinicalraw3, kmeans, method = "silhouette")
fviz_nbclust(clinicalraw3, kmeans, method = "gap_stat")

## fazendo um biplot

fviz_cluster(kmeans(clinicalraw3, centers = 6, iter.max = 100, nstart = 100), data = clinicalraw3)

## extraindo resultados dos clusters pra colorir
clusters <- kmeans(clinicalraw4, centers = 5, iter.max = 100, nstart = 100)

## incluindo uma nova coluna cluster em clinicalraw
clinicalraw3 <- clinicalraw3 |> mutate(cluster = clusters$cluster)

library(tidyverse)
clinicalraw3 <- clinicalraw3 %>% 
  mutate(count_days = case_when(
    vital_status == 1 ~ last_contact_days_to,
    vital_status == 2 ~ death_days_to
  )
  )
## plotando usando variaveis originais 
clinicalraw3 |> ggplot(aes(x = history_seizures, y = count_days, colour = as.factor(cluster))) + geom_bar()

write_csv(clinicalraw3, "lgg.clinicCLUSTERIZADO.csv")

# PLOTS
