library(tidyverse)
library(fst)
wd = "/home/hi/Documents/projects/Scraping Profil Desa/hasil"
daftar_desa <- read_fst("data/data_daftar_desa.fst")
# bkb <- fread("data/data_bkb.csv")
# write.fst(bkb, "data/data_bkb.fst")
bkb <- read_fst("data/data_bkb.fst")

bkb_scrap = readxl::read_excel(path = "/home/hi/Documents/projects/Scraping Profil Desa/hasil/December-data_bkb1.xlsx")

bkb_scrap <- bkb_scrap %>%
  dplyr::group_by(Provinsi, Kabupaten, Kecamatan, Kelurahan_Desa) %>%
  summarise(JUMLAH = n()) %>%
  full_join(daftar_desa, by = c("Provinsi" = "PROVINSI",
                                "Kabupaten" = "KABUPATEN",
                                "Kecamatan" = "KECAMATAN",
                                "Kelurahan_Desa" = "KELURAHAN")) %>%
  replace_na(list(JUMLAH = 0)) %>%
  ungroup() %>%
  mutate(BULAN = rep("DESEMBER", 648))

colnames(bkb_scrap) <- c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "JUMLAH_BKB", "BULAN")

sum(bkb_scrap$JUMLAH_BKB)

#kolom_v1 <- rep("DESEMBER", 648)

bkb <- rbind(bkb, bkb_scrap)

write_fst(bkb, "data/data_bkb.fst")

###BKR
# bkr <- fread("data/data_bkr.csv")
# write.fst(bkr, "data/data_bkr.fst")
bkr <- read_fst("data/data_bkr.fst")
bkr_scrap = readxl::read_excel(path = "/home/hi/Documents/projects/Scraping Profil Desa/hasil/des-data_bkr_result.xlsx")

bkr_scrap <- bkr_scrap %>%
  dplyr::group_by(Provinsi, Kabupaten, Kecamatan, Kelurahan_Desa) %>%
  summarise(JUMLAH = n()) %>%
  full_join(daftar_desa, by = c("Provinsi" = "PROVINSI",
                                "Kabupaten" = "KABUPATEN",
                                "Kecamatan" = "KECAMATAN",
                                "Kelurahan_Desa" = "KELURAHAN")) %>%
  replace_na(list(JUMLAH = 0)) %>%
  ungroup() %>%
  mutate(BULAN = rep("DESEMBER", 1))

colnames(bkr_scrap) <- c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "JUMLAH_BKR", "BULAN")

sum(bkr_scrap$JUMLAH_BKR)

bkr <- rbind(bkr, bkr_scrap)
write_fst(bkr, "data/data_bkr.fst")

#bkl
# bkl <- fread("data/data_bkl.csv")
# write.fst(bkl, "data/data_bkl.fst")
bkl <- read_fst("data/data_bkl.fst")

bkl_scrap = readxl::read_excel(path = "/home/hi/Documents/projects/Scraping Profil Desa/hasil/des-data_bkl_result.xlsx")

bkl_scrap <- bkl_scrap %>%
  dplyr::group_by(Provinsi, Kabupaten, Kecamatan, Kelurahan_Desa) %>%
  summarise(JUMLAH = n()) %>%
  full_join(daftar_desa, by = c("Provinsi" = "PROVINSI",
                                "Kabupaten" = "KABUPATEN",
                                "Kecamatan" = "KECAMATAN",
                                "Kelurahan_Desa" = "KELURAHAN")) %>%
  replace_na(list(JUMLAH = 0)) %>%
  ungroup() %>%
  mutate(BULAN = rep("DESEMBER", 648))

colnames(bkl_scrap) <- c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "JUMLAH_BKL", "BULAN")

sum(bkl_scrap$JUMLAH_BKL)

bkl <- rbind(bkl, bkl_scrap)
write_fst(bkl, "data/data_bkl.fst")

#upppka
# uppka <- fread("data/data_uppka.csv")
# write.fst(uppka, "data/data_uppka.fst")
uppka <- read_fst("data/data_uppka.fst")

uppka_scrap = readxl::read_excel(path = "/home/hi/Documents/projects/Scraping Profil Desa/hasil/des-data_uppka_result.xlsx")

uppka_scrap <- uppka_scrap %>%
  dplyr::group_by(Provinsi, Kabupaten, Kecamatan, Kelurahan_Desa) %>%
  summarise(JUMLAH = n()) %>%
  full_join(daftar_desa, by = c("Provinsi" = "PROVINSI",
                                "Kabupaten" = "KABUPATEN",
                                "Kecamatan" = "KECAMATAN",
                                "Kelurahan_Desa" = "KELURAHAN")) %>%
  replace_na(list(JUMLAH = 0)) %>%
  ungroup() %>%
  mutate(BULAN = rep("DESEMBER", 648))

colnames(uppka_scrap) <- c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "JUMLAH_UPPKA", "BULAN")

sum(uppka_scrap$JUMLAH_UPPKA)

uppka <- rbind(uppka, uppka_scrap)
write_fst(uppka, "data/data_uppka.fst")

#
# pikr <- fread("data/data_pikr.csv")
# write.fst(pikr, "data/data_pikr.fst")
pikr <- read_fst("data/data_pikr.fst")

pikr_scrap = readxl::read_excel(path = "/home/hi/Documents/projects/Scraping Profil Desa/hasil/des_k0_pikr.xlsx")

pikr_scrap <- pikr_scrap %>%
  dplyr::group_by(Provinsi, Kabupaten, Kecamatan, `Kelurahan/Desa`) %>%
  summarise(JUMLAH = n()) %>%
  full_join(daftar_desa, by = c("Provinsi" = "PROVINSI",
                                "Kabupaten" = "KABUPATEN",
                                "Kecamatan" = "KECAMATAN",
                                "Kelurahan/Desa" = "KELURAHAN")) %>%
  replace_na(list(JUMLAH = 0)) %>%
  ungroup() %>%
  mutate(BULAN = rep("DESEMBER", 648))

colnames(pikr_scrap) <- c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "JUMLAH_PIKR", "BULAN")

sum(pikr_scrap$JUMLAH_PIKR)

pikr <- rbind(pikr, pikr_scrap)
write_fst(pikr, "data/data_pikr.fst")

#
# kkb <- fread("data/data_kkb.csv")
# write.fst(kkb, "data/data_kkb.fst")
kkb <- read_fst("data/data_kkb.fst")

kkb_scrap = readxl::read_excel(path = "/home/hi/Documents/projects/Scraping Profil Desa/hasil/des_mentahan_k0_kkb.xlsx")

kkb_scrap <- kkb_scrap %>%
  dplyr::group_by(Provinsi, Kabupaten, Kecamatan, `Kelurahan/Desa`) %>%
  summarise(JUMLAH = n()) %>%
  full_join(daftar_desa, by = c("Provinsi" = "PROVINSI",
                                "Kabupaten" = "KABUPATEN",
                                "Kecamatan" = "KECAMATAN",
                                "Kelurahan/Desa" = "KELURAHAN")) %>%
  replace_na(list(JUMLAH = 0)) %>%
  ungroup() %>%
  mutate(BULAN = rep("DESEMBER", 648))

colnames(kkb_scrap) <- c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "JUMLAH_KKB", "BULAN")

sum(kkb_scrap$JUMLAH_KKB)

kkb <- rbind(kkb, kkb_scrap)
write_fst(kkb, "data/data_kkb.fst")

#
# rdk <- fread("data/data_rdk.csv")
# write.fst(rdk, "data/data_rdk.fst")
rdk <- read_fst("data/data_rdk.fst")

rdk_scrap = readxl::read_excel(path = "/home/hi/Documents/projects/Scraping Profil Desa/hasil/des-data_k0_rdk.xlsx")

rdk_scrap <- rdk_scrap %>%
  dplyr::group_by(Provinsi, Kabupaten, Kecamatan, `Kelurahan/Desa`) %>%
  summarise(JUMLAH = n()) %>%
  full_join(daftar_desa, by = c("Provinsi" = "PROVINSI",
                                "Kabupaten" = "KABUPATEN",
                                "Kecamatan" = "KECAMATAN",
                                "Kelurahan/Desa" = "KELURAHAN")) %>%
  replace_na(list(JUMLAH = 0)) %>%
  ungroup() %>%
  mutate(BULAN = rep("DESEMBER", 648))

colnames(rdk_scrap) <- c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "JUMLAH_RDK", "BULAN")

sum(rdk_scrap$JUMLAH_RDK)

rdk <- rbind(rdk, rdk_scrap)
write_fst(rdk, "data/data_rdk.fst")


###FASKES
sdm_kb <- read_fst("data/data_faskes_siga.fst") %>%
  filter(BULAN != "DESEMBER")

sdm_kb_scrap = readxl::read_excel(path = "/home/hi/Documents/projects/Scraping Profil Desa/hasil/des-faskes_siga_724.xlsx")
colnames(sdm_kb_scrap)[1] <- "V1"

sdm_kb <- rbind(sdm_kb, sdm_kb_scrap)
write_fst(sdm_kb, "data/data_faskes_siga.fst")

##mix
data_mix_kontra <- read.fst("data/data_mix_kontra.fst") %>%
  filter(Bulan != "DESEMBER") %>%
  filter(Bulan != "")
data_mix_kontra_scrap = readxl::read_excel(path = "/home/hi/Documents/projects/Scraping Profil Desa/hasil/des - kb_kontra1.xlsx")
colnames(data_mix_kontra_scrap)[1] <- "V1"

data_mix_kontra <- rbind(data_mix_kontra, data_mix_kontra_scrap)
write_fst(data_mix_kontra, "data/data_mix_kontra.fst")

##pus
data_pus <- read_fst("data/data_pus.fst")

data_pus_scrap = readxl::read_excel(path = "/home/hi/Documents/projects/Scraping Profil Desa/hasil/des-pus1.xlsx")
colnames(data_pus_scrap)[1] <- "V1"

data_pus_scrap <- rbind(data_pus, data_pus_scrap)
write_fst(data_pus_scrap, "data/data_pus.fst")
