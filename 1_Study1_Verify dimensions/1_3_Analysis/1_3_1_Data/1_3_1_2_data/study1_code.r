# This script is used to preprocessing the vocabulary

install.packages("xlsx")
library(tidyverse)
library(rJava)
library(xlsx)
library(matrixStats)

# read the file, using read.xlsx is better than read.csv
df <-  xlsx::read.xlsx(file='./Stage_1_word_collection_raw_en.xlsx'
                       , sheetIndex = 1, encoding="UTF-8")

## convert from wide to long
df <- df %>%
        dplyr::mutate_all(.,as.character) %>%
        tidyr::pivot_longer(cols = Moral_Pos_words:SES_Neg_NumChar,
                            names_to = c("Domain", "Valence","Stats"),
                            names_sep = "_",
                            #names_pattern = "(.*)_(.*)_(.*)",
                            values_to = "val") %>%
        dplyr::mutate(word_indx = rep(1:(nrow(.)/3), each=3)) %>%
        tidyr::pivot_wider(names_from = Stats, values_from = val) %>%
        dplyr::filter(words!='我填过了就试试') # remove this nonsense

df_count <- df %>%
        dplyr::group_by(words) %>%
        dplyr::count() %>%
        dplyr::arrange(., desc(n))

### -------------------------- Rating data of 71 words ----------------------------------
# load raw data
df_rating_71 <- xlsx::read.xlsx(file='./Stage_3_71_words_rating_processing_raw.xlsx'
                                , sheetIndex = 1) %>%
        dplyr::select(Q2_R3:Q9_edu)

# add the expected domain of for each words
domain_w <- rep(c(rep("Morality", 14),
             rep('Competence', 14),
             rep('Sociability', 14),
             rep('Appearance', 15),
             rep('Wealth', 14)), 2)


wordList_71 <- xlsx::read.xlsx(file='./Stage_3_71_words_rating_processing_raw.xlsx'
                            , sheetIndex = 2, encoding="UTF-8") %>%
        dplyr::select(words_id, words) %>%
        dplyr::filter( !words_id %in% c('Q2_R38', 'Q3_R39','Q4_R38', 'Q5_R39')) %>%
        dplyr::mutate(domain_w = domain_w)

# filter out the participants didn't pay attention, with the following rules:
#    Q2_R38: 10, or wrong answer
#    Q3_R39: 4, or wrong answer
#    Q4_R38: 5, or wrong answer
#    Q5_R39: 13, or wrong answer

df_rating_71_v <- df_rating_71 %>%
        dplyr::filter(Q2_R38 == 10 & Q3_R39 == 4 & Q4_R38 == 5 & Q5_R39 == 13) %>%  # passed all attention check
        dplyr::select(-one_of(c('Q2_R38', 'Q3_R39','Q4_R38', 'Q5_R39')))       # remove three attention check items.

# get the frequency for domain belongingness
recode_fun <- function(x, na.rm = FALSE) (x - 7)
recode_cond <- function(x) (ifelse(x > 9, TRUE, FALSE))

# re-code the all the results to the same scale
df_rating_71_domain <- df_rating_71_v %>%
        dplyr::select(contains('Q2_') | contains('Q3_')) %>%
        dplyr::mutate_at(colnames(.)
                ,function(x) ifelse(x > 9 , x-7, x)) 

# transpose the dataframe
col_subj_names <- paste("sub_", 1:nrow(df_rating_71_domain), sep = '')
row_word_id <- colnames(df_rating_71_domain)

df_rating_71_domain <- df_rating_71_domain %>%
        dplyr::mutate(subj = col_subj_names) %>%
        dplyr::select(subj,everything()) %>%
        #dplyr::mutate_if(as.character) %>%
        tidyr::gather(key = var_name, value = value, 2:ncol(.)) %>%
        spread_(colnames(.)[1], "value") %>%
        dplyr::rename(words_id = var_name) %>%
        dplyr::select(words_id, all_of(col_subj_names))

df_rating_71_domain_frq <- df_rating_71_domain %>%
        dplyr::mutate(N_moral = rowSums(.[col_subj_names] == 3),
                      N_compt = rowSums(.[col_subj_names] == 4),
                      N_soccm = rowSums(.[col_subj_names] == 5),
                      N_apprn = rowSums(.[col_subj_names] == 6),
                      N_soccl = rowSums(.[col_subj_names] == 7),
                      N_uncln = rowSums(.[col_subj_names] == 8)) %>%
        select(words_id, N_moral:N_uncln) %>%
        arrange(match(words_id, row_word_id)) %>%
        dplyr::mutate(N_raters = rowSums( .[2:7] )) %>%
        dplyr::mutate(Moral_freq = round(N_moral/N_raters,2),
                      Compt_freq = round(N_compt/N_raters,2),
                      SocCm_freq = round(N_soccm/N_raters,2),
                      Apprn_freq = round(N_apprn/N_raters,2),
                      SocCl_freq = round(N_soccl/N_raters,2),
                      Uncln_freq = round(N_uncln/N_raters,2))

df_rating_71_domain_frq_final <- df_rating_71_domain_frq %>%
        dplyr::left_join(., wordList_71, by = 'words_id')

# get the valence rating
# re-code the all the results to the same scale
row_word_id_v <- df_rating_71_v %>%
        dplyr::select(contains('Q4_') | contains('Q5_')) %>%
        colnames(.)

df_rating_71_valence_1 <- df_rating_71_v %>%
        dplyr::select(contains('Q4_')) %>%
        dplyr::mutate_at(colnames(.)
                         ,function(x, na.rm = FALSE) (x -6)) 

df_rating_71_valence <- df_rating_71_v %>%
        dplyr::select(contains('Q5_')) %>%
        dplyr::mutate_at(colnames(.)
                         ,function(x, na.rm = FALSE) (x -11)) %>%
        dplyr::bind_cols(df_rating_71_valence_1, .)


df_rating_71_valence <- df_rating_71_valence %>%
        dplyr::mutate(subj = col_subj_names) %>%
        dplyr::select(subj,everything()) %>%
        tidyr::gather(key = var_name, value = value, 2:ncol(.)) %>%
        spread_(colnames(.)[1], "value") %>%
        dplyr::rename(words_id = var_name) %>%
        dplyr::select(words_id, all_of(col_subj_names))

df_rating_71_valence_score <- df_rating_71_valence %>%
        dplyr::mutate(mean_Valence = rowMeans(.[col_subj_names] ),
                      sd_Valence = matrixStats::rowSds(as.matrix(.[col_subj_names]))) %>%
        dplyr::select(words_id, mean_Valence,sd_Valence) %>%
        dplyr::arrange(match(words_id, row_word_id_v)) %>%
        dplyr::left_join(., wordList_71, by = 'words_id')

df_rating_71_final <- df_rating_71_domain_frq_final %>%
        dplyr::left_join(., df_rating_71_valence_score, by = c('words')) %>%
        dplyr::select(-c(words_id.y, domain_w.y)) %>%
        dplyr::rename(words_id = words_id.x,
                      domain_w = domain_w.x) %>%
#                      words_id_valence = words_id.y) %>%
        dplyr::select(words_id, words, domain_w, everything())

df_rating_71_final_fn <- file("./Stage_3_71_words_rating_score.csv", encoding="UTF-8")
write.csv(df_rating_71_final, file = df_rating_71_final_fn, row.names = F)

df_rating_71_final_fn <- file("./Stage_3_71_words_rating_score.xlsx", encoding="UTF-8")
xlsx::write.xlsx(df_rating_71_final, "./Stage_3_71_words_rating_score.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)

## plot these
## problem: need to add one more column: domain of words, different from domain of rating.
tmp <- df_rating_71_final %>% 
        dplyr::select(words_id, words, domain_w, mean_Valence, N_moral:N_uncln) %>%
        dplyr::rename(Morality = N_moral, Sociability = N_soccm, Competence = N_compt, 
                      Appearance = N_apprn, Wealth = N_soccl, Unclear = N_uncln) %>%
        tidyr::pivot_longer(-c(words_id, words, domain_w, mean_Valence), 
                            names_to = 'domain_r', values_to = 'freq') %>%
        
        #dplyr::mutate(domain_w = factor(domain_w, levels = c("Morality", "Competence", "Sociability",
        #                                                     "Appearance", "Wealth"))) %>%
        
        dplyr::mutate(domain_r = factor(domain_r, levels = c("Morality", "Competence", "Sociability", 
                                                         "Appearance", "Wealth", "Unclear")),
                      #words_id = factor(words_id, levels = row_word_id),
                      #words = factor(words, levels = .[1:71, "words"]$words),
                      domain_w = factor(domain_w, levels = c("Morality", "Competence", "Sociability",
                                                             "Appearance", "Wealth"))) %>%
        #dplyr::group_by(domain_w) %>% 
        dplyr::arrange(domain_w, domain_r,desc(freq)) # %>%

new_word_ord <- tmp %>%
        dplyr::mutate(domain_w = as.character(domain_w),
                      domain_r = as.character(domain_r)) %>%
        dplyr::filter(domain_w == domain_r)
#write.csv(new_word_ord, file = "b.csv", row.names = F)
new_word_ord<-read.csv("b.csv")
new_word_ord$domain_r <- factor(
  new_word_ord$domain_r,
  levels = c("Appearance","Wealth","Sociability","Competence",
             "Morality", "Unclear"),
  labels = c("App", "SES", "Soc", "Com", "Mor", "Amb")
)

new_word_ord<-new_word_ord %>% dplyr::arrange(
  domain_r, -freq
) %>% dplyr::mutate(
  en_words = paste(words_en, words,seq = "")
)

#tmp <- tmp %>%
       #dplyr::mutate(words = factor(words, levels = new_word_ord$words))
tmp <- tmp %>%
  dplyr::mutate(domain_r = factor(
    domain_r,
    levels = c("Appearance","Wealth","Sociability","Competence",
               "Morality", "Unclear"),
    labels = c("App", "SES", "Soc", "Com", "Mor", "Amb")
  )) %>%
  dplyr::arrange(
    domain_r, -freq
  )

tmp$words <- factor(tmp$words, levels = new_word_ord$words, labels = new_word_ord$en_words)

#pdf('plot_freq_71_word.pdf', width = 15, height = 8)
tmp %>% ggplot2::ggplot(., aes(x=words, y=freq, fill=domain_r)) +
  #geom_histogram(stat="identity", position = 'fill') %>% 
  geom_histogram(stat = "identity",
                 position = "fill") +
  scale_fill_manual(values=rev(c("#ffb3e5","#6666ff", "#ff66ff", "#ffff33","#1ab1ff","#ff4d4d")))+
  labs(x="Words",y="Percentage",fill="Dimension") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 45),
        axis.text.y=element_text(angle = 0, colour = "black", size = 65),
        axis.title.x = element_text(size=80),
        axis.title.y = element_text(size=80),
        legend.title = element_text(size=80),legend.text = element_text(size=65),legend.key.width=unit(2,'cm'))+
  scale_y_continuous(expand=c(0,0))+theme(plot.margin = margin(t = 60, r = 10, b = 10, l = 10, unit = "pt"))
#theme_classic(axis.text.x = element_text(angle = 45, hjust = 1)) #+
#facet_grid(. ~ domain_w )

library(showtext)
showtext_auto()
dev.off()

### -------------------------- Rating data of 42 words ----------------------------------
# load raw data
df_rating_42 <- xlsx::read.xlsx(file='./Data/Stage_3_Ratings_42_words_from_Cui_Wang_words.xlsx'
                                , sheetIndex = 1) %>%
        dplyr::select(Q1_R3:Edu)

wordList_42 <- xlsx::read.xlsx(file='./Data/Stage_3_Ratings_42_words_from_Cui_Wang_words.xlsx'
                               , sheetIndex = 2, encoding="UTF-8") %>%
        dplyr::select(words_id, words)

# load the data about the valence from Wang & Cui (2005), the higher the mean value, the higher the social desireability.
df_val_42 <- xlsx::read.xlsx(file='./Data/Valence_Wang_Cui_2005_42_words.xlsx'
                             , sheetIndex = 1, encoding="UTF-8") %>%
        dplyr::select(Words, Valence) %>%
        dplyr::mutate(words = paste(Words, '???', sep = '')) %>%
        dplyr::select(words, Valence)

# filter out the participants didn't pay attention, with the following rules:
#    Q1_R45: 15, or wrong answer

df_rating_42_v <- df_rating_42 %>%
        dplyr::filter(Q1_R45 == 15 ) %>%  # passed all attention check
        dplyr::select(-one_of(c('Q1_R45', 'Age', 'Edu', 'Sex')))       # remove three attention check items.


# transpose the dataframe
col_subj_names_2 <- paste("sub_", 1:nrow(df_rating_42_v), sep = '')
row_word_id_2 <- colnames(wordList_42)

df_rating_42_v <- df_rating_42_v %>%
        dplyr::mutate(subj = col_subj_names_2) %>%
        dplyr::select(subj,everything()) %>%
        #dplyr::mutate_if(as.character) %>%
        tidyr::gather(key = var_name, value = value, 2:ncol(.)) %>%
        spread_(colnames(.)[1], "value") %>%
        dplyr::rename(words_id = var_name) %>%
        dplyr::select(words_id, all_of(col_subj_names_2))

df_rating_42_v_frq <- df_rating_42_v %>%
        dplyr::mutate(N_moral = rowSums(.[col_subj_names_2] == 10),
                      N_compt = rowSums(.[col_subj_names_2] == 11),
                      N_soccm = rowSums(.[col_subj_names_2] == 12),
                      N_apprn = rowSums(.[col_subj_names_2] == 13),
                      N_soccl = rowSums(.[col_subj_names_2] == 14),
                      N_uncln = rowSums(.[col_subj_names_2] == 15)) %>%
        dplyr::select(words_id, N_moral:N_uncln) %>%
        dplyr::arrange(match(words_id, wordList_42$words_id)) %>%
        dplyr::mutate(N_raters = rowSums( .[2:7] )) %>%
        dplyr::mutate(Moral_freq = round(N_moral/N_raters,2),
                      Compt_freq = round(N_compt/N_raters,2),
                      SocCm_freq = round(N_soccm/N_raters,2),
                      Apprn_freq = round(N_apprn/N_raters,2),
                      SocCl_freq = round(N_soccl/N_raters,2),
                      Uncln_freq = round(N_uncln/N_raters,2)) %>%
        dplyr::arrange(desc(Moral_freq,Compt_freq,SocCm_freq)) %>%
        dplyr::left_join(., wordList_42, by = 'words_id') %>%
        dplyr::left_join(., df_val_42, by = 'words') %>%
        dplyr::select(words_id, words, everything())

df_rating_42_final_fn <-file("./Data/Stage_3_42_words_rating_score.tsv", encoding="UTF-8")
write.csv(df_rating_42_v_frq, file = df_rating_42_final_fn, row.names = F)


