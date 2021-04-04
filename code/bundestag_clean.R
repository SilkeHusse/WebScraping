# ---------------------------------------------------------------------------------------
# WEB DATA COLLECTION WITH R - Project
# Silke Husse
# Winter Semester 2020/21
#
# legislatoR package & data cleaning (bundestag.de) 
# ---------------------------------------------------------------------------------------

### preparations ###

rm(list=ls(all=TRUE))
setwd("/Web Data Collection with R/") # set working directory
source("R code/packages.R") # install and load packages

load("WebScraping/code/df_bundestag_raw.Rda") # load data

### data cleaning ###

bundestag_raw <- final_data %>%
  # collapse to one string
  paste(collapse="\n") %>% 
  # remove redundant information
  stringr::str_remove_all("\\(ausgeschieden\\)\\\n|\\(Mandat abgelehnt\\)\\\n|\\(verstorben\\)\\\n") %>%
  # replace redundant elements
  gsub("\\\n\\\n","\\\n", x=.) %>%
  # split string
  stringr::str_split("\n")

# store data (name, political party)
num_elements = length(bundestag_raw[[1]])
df <- matrix(ncol = 2, nrow = num_elements/2) %>%
  data.frame() %>%
  set_names(c("name", "political_party"))
row_idx = 1
for (i in seq(1, num_elements, by=2)) {
  df[row_idx, 1] <- bundestag_raw[[1]][i]
  df[row_idx, 2] <- bundestag_raw[[1]][i+1]
  row_idx = row_idx + 1
}

# clean col 'name'
df$name <- df$name %>%
  gsub("\\(.*\\)","", x=.) %>% # remove cities given within brackets
  stringr::str_remove_all("Dr\\.|h\\. c\\.|Prof\\.") %>% # remove academic titles
  stringr::str_squish() # remove redundant whitespaces
df <- df %>%
  separate(name, c("last","first"), ",\\s") %>% # separate name by comma
  unite('name', first, last, sep= " ") # unite in correct order

### legislatoR ###

# get name via 'core' table
df_core <- legislatoR::get_core(legislature = "deu")
# get twitter name via 'social' table
df_social <- left_join(x = df_core, 
                         y = legislatoR::get_social(legislature = "deu"), 
                         by = "wikidataid")
# join data frames
df <- left_join(x = df,
                y = df_social,
                by = "name")
# remove duplicate and redundant cols
df <- df[-c(297), ]
df <- subset(df, select = -c(country, wikititle, religion, birth, death, birthplace, website,
                             deathplace, facebook, youtube, googlep, instagram , linkedin))

#length(which(is.na(df$twitter)))
# = 237

# ad twitter information manually
manual_twitter <- c("NAltenkamp", NA, NA, NA, "DoroBaer", NA, "bela_bach", NA, NA, 
  "nicole_ae_bauer", NA, NA, NA, "Manfredbehrens", "c_bernstiel", NA, NA,
  "MBiadaczMdB", NA, "Lothar_Binding", NA, "MWBirkwald", NA, "JBrandenburgFDP",
  "BraFDP", NA, "BreherSilvia", NA, "BerndBuchholz", NA, NA, "Bubendorferfdp", "Buettner_MdB",
  "joerg_cezanne", "FabioDeMasi", "janoschdahmen", "BernhardDaldrup", NA, "BrittaDassler", 
  NA, NA, NA, "FechnerJohannes", NA, NA, "SusanneFerschl", "thorsten_frei", NA, "DagmarFreitag13",
  NA, NA, NA, NA, NA, NA, "EckhardGnodtke", NA, "kdgroehler", NA, NA, NA, NA, NA,
  "Paul_Hampel_BT", NA, NA, "M_HarderKuehnel", NA, "PeterHeidtFDP", "hubertus_heil",
  NA, "KatrinHelling", NA, "UdoHemmelgarn", NA, NA, "hermiworld", NA, NA, NA, NA, "HoffmannForest",
  NA, NA, NA, NA, "GydeJ", NA, NA, NA, NA, NA, NA, NA, "CarstenKoerber", "PascalKober",
  "KorkmazGT", NA, NA, NA, "AKulitz", "stephankuehn", "AndreasLaemmel", NA, "LangeMdB",
  NA, NA, NA, "vonderleyen", NA, NA, NA, NA, NA, "MaagKarin", "SaskiaLudwigCDU",
  "IsabelMackensen", NA, NA, NA, "juergen_martens", "Doro_Martin", NA, NA, NA, NA, NA, NA, 
  "FalkoMohrs", "ClaudiaMollMdB", "MdBMonstadt", "ElisabethMotsc1", NA, "MuellerChemnitz",
  "alexmuellerfdp", NA, "Norbert_MdB", "smuellermdb", NA, "theliberalfrank", "christ_natterer",
  NA, NA, "AlexanderSNeu", NA, "Matthiasnoelke", NA, "Jan_Nolte_AfD", "oezdemir_spd",
  NA, "MPatzeltCDU", NA, "lisapaus", "TobiasMPeterka", "Paul_Podolay", "Pohl_MdB", NA, NA, 
  "Achim_P", NA, "AloisRainer", NA, "Renner_AfD", NA, NA, NA, NA, NA, NA, "MiRo_SPD", 
  "ruetzelbernd", NA, "RyglewskiS", NA, "ulle_schauws", "Chr_Sauter", "SchaeferCDU", NA, 
  "m_schieder", NA, NA, NA, NA, NA, "schneidercar", "MdB_Schreiber", "charlotte_mdb", 
  NA, NA, NA, NA, "armin_schuster", NA, NA, "PatrickSensburg", NA, NA, "HESommer", "StegemannAlbert",
  "Peter_Stein_CDU", "SteffenSonja", NA, NA, NA, NA, NA, NA, NA, NA, "AntjeTillmann", NA, NA, NA,
  "VolkerUllrich", "MarjaVoellers", NA, "johannesvogel", "JoWadephul", NA, NA, "MarcusWeinberg", 
  "DrJoeWeingarten", "PeterWeissMdB", NA, NA, "BerndWestphal4", NA, NA, NA, "k_willkomm", 
  NA, NA, NA, NA, "GZickenheiner", NA, NA)
df$twitter[which(is.na(df$twitter))] <- manual_twitter

#length(which(is.na(df$twitter)))
# = 140

# save data frame
save(df, file="WebScraping/code/df_bundestag_clean.Rda") 

