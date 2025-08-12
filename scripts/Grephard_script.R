#Data_cleaning----
library("tidyverse")
library("readxl")
library(forcats) # Required for fct_infreq

library(ggrepel)
# install.packages("plotly")
library(plotly)

options(scipen = 999) # disable scientific notation



X2025_03_11_atlas_antibiotics <- read_excel("2025_03_11 atlas_antibiotics.xlsx")
View(X2025_03_11_atlas_antibiotics)
main_data <- X2025_03_11_atlas_antibiotics
main_data <- main_data %>%
  mutate(continent = if_else(Country %in% c("France", "Spain", "Belgium", "Italy", "Germany", 
                                            "Ireland", "Portugal", "Greece", "United Kingdom", 
                                            "Poland", "Switzerland", "Hungary", "Austria", 
                                            "Finland", "Denmark", "Sweden", "Croatia", 
                                            "Czech Republic", "Netherlands", "Russia", 
                                            "Romania", "Latvia", "Lithuania", "Serbia", 
                                            "Ukraine", "Slovenia", "Bulgaria", "Norway", 
                                            "Slovak Republic", "Estonia"), "Europe",
                             if_else(Country %in% c("Canada", "United States", "Mexico", 
                                                    "Guatemala", "Dominican Republic", "Costa Rica", 
                                                    "Jamaica", "Honduras", "Puerto Rico", 
                                                    "Nicaragua", "Panama","El Salvador"), "North America",
                                     if_else(Country %in% c("Colombia", "Chile", "Venezuela", "Argentina", 
                                                            "Brazil"), "South America",
                                             if_else(Country %in% c("Australia", "New Zealand"), "Australia",
                                                     if_else(Country %in% c("China", "Hong Kong", "Japan", "Malaysia", 
                                                                            "Thailand", "Philippines", "Korea, South", 
                                                                            "Taiwan", "India", "Singapore", "Vietnam", 
                                                                            "Indonesia"), "Asia",
                                                             if_else(Country %in% c("Nigeria", "Kenya", "South Africa", 
                                                                                    "Ivory Coast", "Morocco", "Cameroon", 
                                                                                    "Malawi", "Uganda", "Ghana", "Namibia", 
                                                                                    "Mauritius", "Tunisia", "Egypt"), "Africa",
                                                                     if_else(Country %in% c("Israel", "Kuwait", "Turkey", "Jordan", 
                                                                                            "Saudi Arabia", "Pakistan", "Lebanon", 
                                                                                            "Qatar", "Oman"), "Middle East", 
                                                                             NA_character_))))))))





# Define the lists of Gram-positive and Gram-negative bacteria
gram_positive <- c(
  "Staphylococcus aureus", "Staphylococcus epidermidis", "Staphylococcus haemolyticus",
  "Staphylococcus saprophyticus", "Staphylococcus hominis", "Staphylococcus lugdunensis",
  "Staphylococcus simulans", "Streptococcus pneumoniae", "Streptococcus pyogenes",
  "Streptococcus agalactiae", "Streptococcus bovis", "Streptococcus salivarius",
  "Enterococcus faecalis", "Enterococcus faecium", "Enterococcus hirae",
  "Enterococcus casseliflavus", "Enterococcus gallinarum", "Enterococcus durans",
  "Enterococcus raffinosus", "Enterococcus avium", "Clostridium perfringens",
  "Clostridioides difficile", "Clostridium tertium", "Clostridium butyricum",
  "Clostridium hathewayi", "Clostridium barati", "Peptostreptococcus anaerobius",
  "Peptostreptococcus magnus", "Peptostreptococcus hydrogenalis", "Peptostreptococcus indolicus",
  "Anaerococcus tetradius", "Anaerococcus vaginalis", "Anaerococcus murdochii",
  "Anaerococcus lactolyticus", "Anaerococcus prevotii", "Peptoniphilus asaccharolyticus",
  "Peptoniphilus olsenii", "Peptoniphilus coxii", "Peptoniphilus lacrimalis",
  "Peptoniphilus indolicus", "Clostridium clostridiiformis", "Clostridium subterminale",
  "Clostridium ramosum", "Clostridium citroniae", "Clostridium innocuum",
  "Clostridium scindens", "Clostridium symbiosum", "Clostridium cadaveris",
  "Clostridium sporogenes", "Clostridium sordellii", "Clostridium septicum",
  "Clostridium aldenense", "Clostridium bifermentans", "Clostridium bolteae",
  "Clostridium disporicum", "Clostridium glycolicum", "Clostridium beijerinckii",
  "Clostridium hastiforme", "Clostridium scatalogenes", "Clostridium putrificum",
  "Clostridium citroniae", "Clostridium novyi", "Clostridium difficile", "Staphylococcus arlettae",
  "Staphylococcus xylosus", "Staphylococcus capitis", "Staphylococcus sciuri",
  "Staphylococcus coagulase negative", "Streptococcus mitis", "Streptococcus gordonii",
  "Streptococcus intermedius", "Streptococcus oralis", "Streptococcus anginosus",
  "Streptococcus dysgalactiae", "Streptococcus canis", "Streptococcus salivarius",
  "Streptococcus viridans", "Streptococcus constellatus", "Streptococcus massiliensis",
  "Streptococcus gallolyticus", "Streptococcus parasanguinis", "Streptococcus sue,",
  "Enterococcus, non-speciated", "Enterococcus mundtii", "Enterococcus canintestini",
  "Enterococcus flavescens", "Staphylococcus warneri", "Staphylococcus caprae",
  "Staphylococcus pseudointermedius", "Staphylococcus intermedius",
  "Staphylococcus cohnii", "Staphylococcus schleiferi", "Staphylococcus auricularis",
  "Staphylococcus pettenkoferi", "Staphylococcus saccharolyticus",
  "Staphylococcus argenteus", "Staphylococcus vitulinus", "Staphylococcus hyicus",
  "Staphylococcus condimenti", "Staphylococcus Coagulase Negative",
  "Staphylococcus spp", "Staphylococcus, Beta Hemolytic", "Staphylococcus pasteuri",
  "Streptococcus sanguinis", "Streptococcus castoreus", "Streptococcus, viridans group",
  "Streptococcus sanguis", "Streptococcus suis", "Streptococcus equi", "Streptococcus spp",
  "Streptococcus pyogenes", "Streptococcus agalactiae", "Clostridium paraputrificum",
  "Clostridium limosum", "Clostridium celerecrescens", "Clostridium histolyticum",
  "Clostridium cochlearium", "Clostridium novyi", "Clostridium rectum",
  "Clostridium bifermentans", "Clostridium sphenoides", "Clostridium citrullae",
  "Clostridium difficile", "Clostridium clostridiiformis", "Clostridium barati",
  "Clostridium butyricum", "Clostridium sporogenes", "Clostridium tertium",
  "Peptostreptococcus lactolyticus", "Peptostreptococcus tetradius",
  "Peptostreptococcus spp", "Peptostreptococcus asaccharolyticus", "Corynebacterium aurimucosum",
  "Acidaminococcus fermentans", "Eggerthella spp", "Phocaeicola vulgatus",
  "Bacteroides ovatus", "Prevotella oralis", "Prevotella intermedia",
  "Bacteroides caccae", "Bacteroides salersyae", "Parabacteroides goldsteinii",
  "Peptoniphilus harei", "Prevotella disiens", "Peptoniphilus gorbachii",
  "Bacteroides pyogenes", "Prevotella baroniae", "Bacteroides intestinalis",
  "Bacteroides stercosis", "Parabacteroides johnsonii", "Anaerococcus hydrogenalis",
  "Anaerococcus spp", "Anaerococcus octavius", "Bacteroides coagulans",
  "Bacteroides cellulosilyticus", "Clostridium colicanis", "Clostridium spp",
  "Clostridium novyia", "Paraclostridium bifermentans", "Kerstersia gyiorum",
  "Enterococcus spp", "Enterococcus Group D", "Streptococcus lutetiensis",
  "Streptococcus, Beta Hemolytic",
  "Parvimonas RISra", "Peptoniphilus spp", "Prevotella oulorum",
  "Staphylococcus petrasii", "Alcaligenes faecalis", "Enterococcus malodoratus",
  "Enterocloster bolteae")



gram_negative <- c(
  "Pseudomonas aeruginosa", "Serratia marcescens", "Acinetobacter pitii", "Acinetobacter baumannii",
  "Enterobacter cloacae", "Escherichia coli", "Haemophilus influenzae", "Citrobacter freundii",
  "Klebsiella pneumoniae", "Klebsiella aerogenes", "Acinetobacter junii", "Klebsiella oxytoca",
  "Enterobacter kobei", "Acinetobacter lwoffii", "Serratia liquefaciens", "Enterobacter asburiae",
  "Citrobacter koseri", "Serratia fonticola", "Serratia rubidaea", "Acinetobacter schindleri",
  "Acinetobacter guillouiae", "Acinetobacter calcoaceticus", "Acinetobacter nosocomialis",
  "Pluralibacter gergoviae", "Acinetobacter radioresistens", "Acinetobacter johnsonii",
  "Acinetobacter ursingii", "Acinetobacter haemolyticus", "Acinetobacter parvus",
  "Acinetobacter tjernbergiae", "Klebsiella variicola", "Klebsiella, non-speciated",
  "Serratia, non-speciated", "Citrobacter murliniae", "Citrobacter sedlakii", "Citrobacter youngae",
  "Pseudomonas putida", "Pseudomonas stutzeri", "Pseudomonas monteilii", "Pseudomonas nitroreducens",
  "Pseudomonas alcaliphila", "Pseudomonas putida/fluorescens Group", "Burkholderia cepacia",
  "Burkholderia cenocepacia", "Moraxella catarrhalis", "Aeromonas hydrophila", "Aeromonas caviae",
  "Aeromonas veronii", "Vibrio cholerae", "Campylobacter jejuni", "Helicobacter pylori",
  "Neisseria gonorrhoeae", "Neisseria meningitidis", "Helicobacter pylori", "Achromobacter xylosoxidans",
  "Achromobacter insolitus", "Bordetella trematum", "Myroides odoratimimus", "Achromobacter xylosoxidans",
  "Pseudomonas citronellolis", "Pseudomonas pseudoalcaligenes", "Bacteroides fragilis",
  "Parabacteroides distasonis", "Prevotella denticola", "Bacteroides thetaiotaoRISron",
  "Bacteroides vulgatus", "Prevotella buccae", "Prevotella oris", "Prevotella bivia",
  "Bacteroides nordii", "Bacteroides uniformis", "Prevotella nanceiensis", "Prevotella melaninogenica",
  "Prevotella loescheii", "Prevotella timonensis", "Prevotella spp", "Prevotella veroralis",
  "Prevotella heparinolytica", "Prevotella tannerae", "Bacteroides eggerthii", "Bacteroides faecis",
  "Bacteroides dorei", "Bacteroides splanchnicus", "Bacteroides bivius", "Bacteroides capsillosis",
  "Bacteroides massiliensis", "Bacteroides merdeae", "Prevotella multiformis", "Fusobacterium nucleatum",
  "Anaerovorax spp", "Veillonella parvula", "Eubacterium lentum", "Eubacterium aerofaciens",
  "Finegoldia magna", "Enterobacter cancerogenus", "Enterobacter intermedium",
  "Enterobacter xiangfangensis", "Citrobacter diversus", "Citrobacter spp", "Enterobacter spp",
  "Pseudomonas graminis", "Pseudomonas spp", "Moraxella osloensis", "Paeniclostridium sordelli",
  "Elizabethkingia anophelis", "Comamonas kerstersii", "Neisseria gonorrhoeae",
  "Neisseria meningitidis", 
  "Acinetobacter, non-speciated", "Acinetobacter baylyi", "Acinetobacter bereziniae",
  "Acinetobacter beijerinckii", "Acinetobacter anitratus", "Acinetobacter seifertii",
  "Acinetobacter dijkshoorniae", "Acinetobacter variabilis", "Acinetobacter gyllenbergii",
  "Acinetobacter colistiniresistens", "Acinetobacter modestus", "Acinetobacter proteolyticus",
  "Acinetobacter spp", "Acinetobacter tandoii", "Acinetobacter vivianii",
  "Acinetobacter lactucae", "Acinetobacter dispersus", "Pseudomonas otitidis",
  "Pseudomonas mendocina", "Pseudomonas fluorescens", "Pseudomonas stewartii",
  "Pseudomonas mosselii", "Pseudomonas alcaligenes", "Pseudomonas guariconensis",
  "Pseudomonas fulva", "Pseudomonas citronellolis", "Enterobacter ludwigii",
  "Enterobacter, non-speciated", "Enterobacter agglomerans", "Enterobacter taylorae",
  "Enterobacter gergoviae", "Enterobacter bugandensis", "Enterobacter hormaechi",
  "Klebsiella ozaenae", "Klebsiella planticola", "Klebsiella spp", "Klebsiella rhinoscleromatis",
  "Klebsiella ornithinolytica", "Serratia odorifera", "Serratia ficaria",
  "Serratia ureilytica", "Serratia grimesii", "Serratia spp", "Citrobacter braakii",
  "Citrobacter farmeri", "Citrobacter gillenii", "Citrobacter amalonaticus", "Citrobacter spp",
  "Stenotrophomonas maltophilia", "Lelliottia amnigena", "Proteus vulgaris",
  "Proteus mirabilis", "Proteus penneri", "Proteus hauseri", "Morganella morganii",
  "Providencia stuartii", "Providencia rettgeri", "Providencia alcalifaciens",
  "Providencia spp", "Raoultella ornithinolytica", "Raoultella planticola",
  "Raoultella terrigena", "Aeromonas spp", "Bordetella spp", "Salmonella spp",
  "Moraxella spp", "Campylobacter ureolyticus", "Haemophilus parainfluenzae",
  "Haemophilus parahaemolyticus", "Haemophilus pittmaniae", "Haemophilus spp",
  "Cronobacter spp", "Kluyvera ascorbata", "Pantoea spp", "Pantoea agglomerans",
  "Pantoea septica", "Escherichia hermanii", "Escherichia vulneris", "Escherichia spp",
  "Phocaeicola vulgatus", "Bacteroides spp", "Prevotella spp", "Parabacteroides spp",
  "Prevotella buccalis", "Prevotella amnii", "Prevotella corporis", "Prevotella nanceinsis",
  "Prevotella nigrescens", "Prevotella jejunii", "Prevotella histicola", "Prevotella pallens",
  "Prevotella bergensis", "Prevotella salivae", "Prevotella maculosa",
  "Eggerthella lenta", "Enterocloster spp", "Enterocloster clostridioformis",
  "Enterocloster citroniae", "Paraclostridium spp",
  "Enterobacter sakazakii", "Serratia plymuthica", "Hafnia alvei",
  "Citrobacter, non-speciated", "Providencia, non-speciated", "Proteus rettgeri",
  "Pantoea dispersa", "Cronobacter sakazakii", "Escherichia fergusonii",
  "Enterobacter roggenkampii", "Acinetobacter towneri", "Acinetobacter soli",
  "Raoultella spp", "Acinetobacter venetianus", "Acinetobacter indicus",
  "Acinetobacter pseudolwoffii", "Acinetobacter alcaligenes", "Escherichia spp",
  "Pseudomonas spp", "Proteus spp", "Acinetobacter courvalinii", "Ochrobactrum anthropi",
  "Providencia rustigianii", "Morganella spp", "Parvimonas spp",
  "Enterobacter liquifaciens")


# Add Gram status column using if_else() and mutate()

bacteria_df <- main_data %>%
  mutate(
    Gram_Status = if_else(
      Species %in% gram_positive, 
      "Gram-positive",
      if_else(
        Species %in% gram_negative,
        "Gram-negative",
        "Unknown"
      )
    )
  ) %>% 
  rename(Continent = continent)

# View the results
print(bacteria_df)



#gram_pos <- bacteria_df %>% filter(Gram_Status == "Gram-positive")

#gram_neg <- bacteria_df %>% filter(Gram_Status == "Gram-negative")


bacteria_df <- bacteria_df %>% select("Isolate Id", "Study", "Species", "Gram_Status", "Family", "Country","Continent", "Gender", "Age Group", "Speciality", "Source",
                                      "In / Out Patient", "Year", "Phenotype", "Amikacin", "Amikacin_I", "Amoxycillin clavulanate", "Amoxycillin clavulanate_I",
                                      "Ampicillin", "Ampicillin_I", "Azithromycin", "Azithromycin_I", "Cefepime", "Cefepime_I", "Cefoxitin", "Cefoxitin_I",
                                      "Ceftazidime", "Ceftazidime_I", "Ceftriaxone", "Ceftriaxone_I", "Clarithromycin", "Clarithromycin_I", "Clindamycin",
                                      "Clindamycin_I", "Erythromycin", "Erythromycin_I", "Imipenem", "Imipenem_I", "Levofloxacin", "Levofloxacin_I", 
                                      "Linezolid", "Linezolid_I", "Meropenem", "Meropenem_I", "Metronidazole", "Metronidazole_I", "Minocycline", 
                                      "Minocycline_I", "Penicillin", "Penicillin_I", "Piperacillin tazobactam", "Piperacillin tazobactam_I", "Tigecycline",
                                      "Tigecycline_I", "Vancomycin", "Vancomycin_I", "Ampicillin sulbactam", "Ampicillin sulbactam_I", "Aztreonam", 
                                      "Aztreonam_I", "Aztreonam avibactam", "Aztreonam avibactam_I", "Cefixime", "Cefixime_I", "Ceftaroline", "Ceftaroline_I",
                                      "Ceftaroline avibactam", "Ceftaroline avibactam_I", "Ceftazidime avibactam", "Ceftazidime avibactam_I", "Ciprofloxacin",
                                      "Ciprofloxacin_I", "Colistin", "Colistin_I", "Daptomycin", "Daptomycin_I", "Doripenem", "Doripenem_I", "Ertapenem",
                                      "Ertapenem_I", "Gatifloxacin", "Gatifloxacin_I", "Gentamicin", "Gentamicin_I", "Moxifloxacin", "Moxifloxacin_I",
                                      "Oxacillin", "Oxacillin_I", "Quinupristin dalfopristin", "Quinupristin dalfopristin_I", "Sulbactam", "Sulbactam_I",
                                      "Teicoplanin", "Teicoplanin_I", "Tetracycline", "Tetracycline_I", "Trimethoprim sulfa", "Trimethoprim sulfa_I",
                                      "Ceftolozane tazobactam", "Ceftolozane tazobactam_I", "Cefoperazone sulbactam", "Cefoperazone sulbactam_I",
                                      "Meropenem vaborbactam", "Meropenem vaborbactam_I", "Cefpodoxime", "Cefpodoxime_I", "Ceftibuten", "Ceftibuten_I",
                                      "Ceftibuten avibactam", "Ceftibuten avibactam_I", "Tebipenem", "Tebipenem_I")

#### partitioning the data into two:

epidem_data <- bacteria_df %>% select("Isolate Id", "Study", "Species", "Gram_Status", "Family", "Country","Continent", "Gender", "Age Group", "Speciality", "Source",
                                      "In / Out Patient", "Phenotype", "Year")



RIS_data <- bacteria_df %>% select("Isolate Id", ends_with("_I"))


### breakpoint_combined data
RIS_combined_data <- epidem_data %>%
  full_join(RIS_data, by = "Isolate Id")


### pivot longer and creating a new columns called antibiotic class:
penicillins <- c("Amoxycillin clavulanate", "Ampicillin", "Ampicillin sulbactam", "Piperacillin tazobactam")
cephalosporins <- c("Cefepime", "Ceftazidime", "Ceftriaxone", "Ceftaroline", "Ceftolozane tazobactam", "Cefoperazone sulbactam")
cephalosporin_bli <- c("Ceftazidime avibactam", "Ceftaroline avibactam")
monobactams <- c("Aztreonam", "Aztreonam avibactam")
carbapenems <- c("Meropenem", "Imipenem", "Doripenem", "Ertapenem", "Meropenem vaborbactam")
tetracyclines <- c("Minocycline", "Tigecycline")
macrolides <- c("Azithromycin")
fluoroquinolones <- c("Levofloxacin", "Ciprofloxacin")
aminoglycosides <- c("Gentamicin", "Amikacin")
polymyxins <- c("Colistin")
folate_inhibitors <- c("Trimethoprim sulfa")
beta_lactamase_inhibitors <- c("Sulbactam")

RIS_pivot_long1 <- RIS_combined_data %>%
  pivot_longer(
    cols = matches("_I$|_R$|_S$"),  # or whatever matches your antibiotics
    names_to = "antibiotics",
    values_to = "RIS"
  ) %>%
  drop_na(RIS) %>%
  mutate(
    antibiotics = str_replace(antibiotics, "_I$", ""),
    Antibiotic_Class = case_when(
      antibiotics %in% penicillins ~ "Penicillins",
      antibiotics %in% cephalosporins ~ "Cephalosporins",
      antibiotics %in% cephalosporin_bli ~ "Cephalosporin/Beta-Lactamase Inhibitor Combinations",
      antibiotics %in% monobactams ~ "Monobactams",
      antibiotics %in% carbapenems ~ "Carbapenems",
      antibiotics %in% tetracyclines ~ "Tetracyclines",
      antibiotics %in% macrolides ~ "Macrolides",
      antibiotics %in% fluoroquinolones ~ "Fluoroquinolones",
      antibiotics %in% aminoglycosides ~ "Aminoglycosides",
      antibiotics %in% polymyxins ~ "Polymyxins",
      antibiotics %in% folate_inhibitors ~ "Folate Pathway Inhibitors",
      antibiotics %in% beta_lactamase_inhibitors ~ "Beta-Lactamase Inhibitors",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(`Isolate Id`, Year, Species, Family, Gram_Status, Country, Continent, 
         Gender, `Age Group`, Speciality, Source, `In / Out Patient`, antibiotics,
         Antibiotic_Class, RIS)

res_data <- RIS_pivot_long1 %>% filter(Continent == "Africa")

#Resistance----
# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)


# Step 1: Prepare resistance data
res_data <- res_data %>%
  filter(!is.na(Antibiotic_Class), !is.na(RIS)) %>%
  mutate(resistant_flag = if_else(str_to_lower(RIS) == "resistant", 1, 0))

# Step 2: One row per isolate-antibiotic_class, taking max resistance (if any)
class_flags <- res_data %>%
  group_by(`Isolate Id`, Year, Species, Country, Antibiotic_Class) %>%
  summarise(class_resistant = max(resistant_flag), .groups = "drop")

# Step 3: Pivot wider so each class is a column, 1 if resistant
wide_flags <- class_flags %>%
  pivot_wider(names_from = Antibiotic_Class, values_from = class_resistant, values_fill = 0)

# Step 4: Count how many classes resistant
wide_flags <- wide_flags %>%
  rowwise() %>%
  mutate(
    n_classes_resistant = sum(c_across(-c(`Isolate Id`, Year, Species, Country))),
    mdr_flag = if_else(n_classes_resistant >= 3, 1, 0)
  ) %>%
  ungroup()

wide_flags <- wide_flags %>%
  rowwise() %>%
  mutate(
    n_classes_resistant = sum(c_across(-c(`Isolate Id`, Year, Species, Country))),
    mdr_flag = if_else(n_classes_resistant >= 3, 1, 0)
  ) %>%
  ungroup()
# Save MDR Flags Table
write.csv(wide_flags, "wide_flags.csv", row.names = FALSE)

library(dplyr)
library(ggplot2)

# Read data
df <- wide_flags

# Filter for isolates with >= 3 resistance classes-----
df_resistant <- df %>% filter(n_classes_resistant >= 3)

# Get top 10 species by count of highly resistant isolates
top_species <- df_resistant %>%
  group_by(Species) %>%
  summarise(
    high_resistant_count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(high_resistant_count)) %>%
  slice_head(n = 10) %>%
  pull(Species)

#ranking ----
df_top_clean <- species_year_country_mdr %>%
  filter(Species %in% top_species) %>%
  mutate(Species = factor(Species, levels = top_species))

ggplot(df_top_clean, aes(x = factor(Year), y = percent_mdr, fill = Species)) +
  geom_col(position = "stack", color = "black", linewidth = 0.2) +
  facet_wrap(~ Country, scales = "free_y") +
  labs(
    title = "Top 10 Species with ≥3 Resistance Classes",
    subtitle = "Percentage MDR per Year, Faceted by Country",
    x = "Year",
    y = "Percentage of isolates (%)",
    fill = "Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# Filter and order by top_species----
df_top_clean <- species_year_country_mdr %>%
  filter(Species %in% top_species) %>%
  mutate(
    Species = factor(Species, levels = top_species),
    highlight = ifelse(Species %in% c("Klebsiella pneumoniae", "Escherichia coli"), "highlight", "normal")
  )

# Assign custom colors: bright for highlighted, muted for others
color_map <- c(
  "Klebsiella pneumoniae" = "#e41a1c",  # bright red
  "Escherichia coli" = "#377eb8",       # bright blue
  setNames(rep("grey70", length(top_species) - 1),
           top_species[!top_species %in% c("Klebsiella pneumoniae", "Escherichia coli")])
)

df_top_clean <- species_year_country_mdr %>%
  filter(Species %in% top_species) %>%
  mutate(
    Species = factor(Species, levels = top_species),
    highlight = ifelse(Species == "Klebsiella pneumoniae", "highlight", "normal")
  )

# Assign custom colors: bright red for Klebsiella, grey for others
others <- setdiff(top_species, "Klebsiella pneumoniae")

color_map <- c(
  "Klebsiella pneumoniae" = "#e41a1c",  # bright red
  setNames(rep("grey70", length(others)), others)
)


# Plot
ggplot(df_top_clean, aes(x = factor(Year), y = percent_mdr, fill = Species)) +
  geom_col(aes(size = highlight), position = "stack", color = "black") +
  scale_size_manual(values = c("normal" = 0.2, "highlight" = 1), guide = "none") +
  scale_fill_manual(values = color_map) +
  facet_wrap(~ Country, scales = "free_y") +
  labs(
    title = "Top 10 Species with ≥3 Resistance Classes",
    subtitle = "Klebsiella pneumoniae Highlighted",
    x = "Year",
    y = "Percentage of isolates (%)",
    fill = "Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(
      face = ifelse(levels(df_top_clean$Species) %in% 
                      c("Klebsiella pneumoniae", "Escherichia coli"), "bold", "plain")
    ),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

#antibiotics%%-----
library(dplyr)
library(tidyr)
library(ggplot2)

# Define antibiotic class columns
abx_cols <- c("Aminoglycosides", "Carbapenems", "Cephalosporins",
              "Fluoroquinolones", "Penicillins", "Tetracyclines",
              "Macrolides")

# Filter Klebsiella pneumoniae MDR isolates
kp_mdr <- df %>%
  filter(Species == "Klebsiella pneumoniae", mdr_flag == 1)

# Calculate percentage resistant per antibiotic class
kp_percent <- kp_mdr %>%
  select(all_of(abx_cols)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Antibiotic_Class",
    values_to = "Resistant"
  ) %>%
  group_by(Antibiotic_Class) %>%
  summarise(
    percent_resistant = mean(Resistant) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(percent_resistant))

# Plot
ggplot(kp_percent, aes(x = reorder(Antibiotic_Class, percent_resistant),
                       y = percent_resistant,
                       fill = Antibiotic_Class)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Klebsiella pneumoniae (MDR) - % Resistance by Antibiotic Class",
    x = "Antibiotic Class",
    y = "Percentage Resistant (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#Antibiotics per year----
# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Define antibiotic class columns
abx_cols <- c("Aminoglycosides", "Carbapenems", "Cephalosporins",
              "Fluoroquinolones", "Penicillins", "Tetracyclines", "Macrolides")

# Filter Klebsiella pneumoniae MDR isolates
kp_mdr <- df %>%
  filter(Species == "Klebsiella pneumoniae", mdr_flag == 1)

# Calculate percentage resistant per antibiotic class *per year*
kp_percent_year <- kp_mdr %>%
  select(Year, all_of(abx_cols)) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Antibiotic_Class",
    values_to = "Resistant"
  ) %>%
  group_by(Year, Antibiotic_Class) %>%
  summarise(
    percent_resistant = mean(Resistant) * 100,
    .groups = "drop"
  ) %>%
  arrange(Year, desc(percent_resistant))
#---

# Faceted plot by Year
ggplot(kp_percent_year, aes(x = reorder(Antibiotic_Class, percent_resistant),
                            y = percent_resistant,
                            fill = Antibiotic_Class)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Klebsiella pneumoniae (MDR) - % Resistance by Antibiotic Class per Year",
    x = "Antibiotic Class",
    y = "Percentage Resistant (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ Year)


# Calculate percentage resistant per antibiotic
resist_summary <- kp_mdr %>%
  summarise(across(all_of(abx_cols), ~ mean(.x, na.rm = TRUE) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Antibiotic", values_to = "Resistance_Percent") %>%
  arrange(desc(Resistance_Percent))

# Plot
ggplot(resist_summary, aes(x = reorder(Antibiotic, Resistance_Percent), y = Resistance_Percent)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Overall % Resistance of Klebsiella Isolates (Africa)",
    x = "Antibiotic",
    y = "Resistance (%)"
  ) +
  theme_minimal()
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# Antibiotic columns
abx_cols <- c("Aminoglycosides", "Carbapenems", "Cephalosporins",
              "Fluoroquinolones", "Penicillins", "Tetracyclines", "Macrolides")

# Filter MDR isolates
mdr_data <- df %>%
  filter(Species == "Klebsiella pneumoniae", mdr_flag == 1) %>%
  select(all_of(abx_cols))

# Compute co-resistance counts
co_matrix <- t(mdr_data) %*% as.matrix(mdr_data)  # matrix multiplication to get pairwise counts

# Convert to dataframe for ggplot
co_df <- as.data.frame(as.table(co_matrix))
names(co_df) <- c("Class1", "Class2", "Count")

# Plot heatmap
ggplot(co_df, aes(x = Class1, y = Class2, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Co-resistance Heatmap (Klebsiella pneumoniae MDR isolates)",
    x = "Antibiotic Class",
    y = "Antibiotic Class",
    fill = "Co-resistant Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))