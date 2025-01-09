bkb <- fread("data/data_bkb.csv")
bkb <- bkb %>%
  select(-c(V1, BATAS)) %>%
  filter(BULAN == "AGUSTUS")
bkr <- fread("data/data_bkr.csv")
bkr <- bkr %>%
  select(-c(V1, BATAS)) %>%
  filter(BULAN == "AGUSTUS")

bkl <- fread("data/data_bkl.csv")
bkl <- bkl %>%
  select(-c(V1, BATAS)) %>%
  filter(BULAN == "AGUSTUS")

uppka <- fread("data/data_uppka.csv")
uppka <- uppka %>%
  select(-c(V1, BATAS)) %>%
  filter(BULAN == "AGUSTUS")

pikr <- fread("data/data_pikr.csv")
pikr <- pikr%>%
  select(-c(V1, BATAS)) %>%
  filter(BULAN == "AGUSTUS")

kkb <- fread("data/data_kkb.csv")
kkb <- kkb %>%
  select(-c(V1, BATAS)) %>%
  filter(BULAN == "AGUSTUS")

rdk <- fread("data/data_rdk.csv")
rdk <- rdk %>%
  select(-c(V1, BATAS)) %>%
  filter(BULAN == "AGUSTUS")

pembina_kab <- fread("data/data_pembina_wilayah_kab.csv")
pembina_kec <- fread("data/data_pembina_wilayah_kec.csv")
nama_pkb <- fread("data/nama pkb.csv")

data_poktan_setara <- kkb%>%
  left_join(rdk, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
  left_join(bkb, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
  left_join(bkr, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
  left_join(bkl, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
  left_join(uppka, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
  left_join(pikr, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
  select(BULAN, everything()) %>%
  mutate(across(6:12, ~ ifelse(. == 0, "Tidak Ada", "Ada")))

# Menghitung jumlah "Ada", "Tidak Ada", dan persentase untuk setiap kolom JUMLAH_ yang dikelompokkan berdasarkan KECAMATAN
rekapitulasi <- data_poktan_setara %>%
  group_by(KABUPATEN) %>%
  summarise(
    JUMLAH_KKB_Ada = sum(JUMLAH_KKB == "Ada"),
    JUMLAH_KKB_Tidak_Ada = sum(JUMLAH_KKB == "Tidak Ada"),
    JUMLAH_KKB_Persentase = paste0(round(JUMLAH_KKB_Ada / (JUMLAH_KKB_Ada + JUMLAH_KKB_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_RDK_Ada = sum(JUMLAH_RDK == "Ada"),
    JUMLAH_RDK_Tidak_Ada = sum(JUMLAH_RDK == "Tidak Ada"),
    JUMLAH_RDK_Persentase = paste0(round(JUMLAH_RDK_Ada / (JUMLAH_RDK_Ada + JUMLAH_RDK_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_BKB_Ada = sum(JUMLAH_BKB == "Ada"),
    JUMLAH_BKB_Tidak_Ada = sum(JUMLAH_BKB == "Tidak Ada"),
    JUMLAH_BKB_Persentase = paste0(round(JUMLAH_BKB_Ada / (JUMLAH_BKB_Ada + JUMLAH_BKB_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_BKR_Ada = sum(JUMLAH_BKR == "Ada"),
    JUMLAH_BKR_Tidak_Ada = sum(JUMLAH_BKR == "Tidak Ada"),
    JUMLAH_BKR_Persentase = paste0(round(JUMLAH_BKR_Ada / (JUMLAH_BKR_Ada + JUMLAH_BKR_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_BKL_Ada = sum(JUMLAH_BKL == "Ada"),
    JUMLAH_BKL_Tidak_Ada = sum(JUMLAH_BKL == "Tidak Ada"),
    JUMLAH_BKL_Persentase = paste0(round(JUMLAH_BKL_Ada / (JUMLAH_BKL_Ada + JUMLAH_BKL_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_UPPKA_Ada = sum(JUMLAH_UPPKA == "Ada"),
    JUMLAH_UPPKA_Tidak_Ada = sum(JUMLAH_UPPKA == "Tidak Ada"),
    JUMLAH_UPPKA_Persentase = paste0(round(JUMLAH_UPPKA_Ada / (JUMLAH_UPPKA_Ada + JUMLAH_UPPKA_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_PIKR_Ada = sum(JUMLAH_PIKR == "Ada"),
    JUMLAH_PIKR_Tidak_Ada = sum(JUMLAH_PIKR == "Tidak Ada"),
    JUMLAH_PIKR_Persentase = paste0(round(JUMLAH_PIKR_Ada / (JUMLAH_PIKR_Ada + JUMLAH_PIKR_Tidak_Ada) * 100, 2), "%")
  ) %>%
  left_join(pembina_kab, by = "KABUPATEN")

reactable(
  rekapitulasi,
  columns = list(
    JUMLAH_KKB_Ada = colDef(name = "Ada"),
    JUMLAH_KKB_Tidak_Ada = colDef(name = "Tidak Ada",
                                  style = color_scales(
                                    rekapitulasi[3], 
                                    colors = c("#8fbc8f", "red"), span = TRUE)
    ),
    JUMLAH_KKB_Persentase = colDef(name = "%"),
    
    JUMLAH_RDK_Ada = colDef(name = "Ada"),
    JUMLAH_RDK_Tidak_Ada = colDef(name = "Tidak Ada",
                                  style = color_scales(
                                    rekapitulasi[6], 
                                    colors = c("#8fbc8f", "red"), span = TRUE)
    ),
    JUMLAH_RDK_Persentase = colDef(name = "%"),
    
    JUMLAH_BKB_Ada = colDef(name = "Ada"),
    JUMLAH_BKB_Tidak_Ada = colDef(name = "Tidak Ada",
                                  style = color_scales(
                                    rekapitulasi[9], 
                                    colors = c("#8fbc8f", "red"), span = TRUE)
    ),
    JUMLAH_BKB_Persentase = colDef(name = "%"),
    
    JUMLAH_BKR_Ada = colDef(name = "Ada"),
    JUMLAH_BKR_Tidak_Ada = colDef(name = "Tidak Ada",
                                  style = color_scales(
                                    rekapitulasi[12], 
                                    colors = c("#8fbc8f", "red"), span = TRUE)
    ),
    JUMLAH_BKR_Persentase = colDef(name = "%"),
    
    JUMLAH_BKL_Ada = colDef(name = "Ada"),
    JUMLAH_BKL_Tidak_Ada = colDef(name = "Tidak Ada",
                                  style = color_scales(
                                    rekapitulasi[15], 
                                    colors = c("#8fbc8f", "red"), span = TRUE)
    ),
    JUMLAH_BKL_Persentase = colDef(name = "%"),
    
    JUMLAH_UPPKA_Ada = colDef(name = "Ada"),
    JUMLAH_UPPKA_Tidak_Ada = colDef(name = "Tidak Ada",
                                    style = color_scales(
                                      rekapitulasi[18], 
                                      colors = c("#8fbc8f", "red"), span = TRUE)
    ),
    JUMLAH_UPPKA_Persentase = colDef(name = "%"),
    
    JUMLAH_PIKR_Ada = colDef(name = "Ada"),
    JUMLAH_PIKR_Tidak_Ada = colDef(name = "Tidak Ada",
                                   style = color_scales(
                                     rekapitulasi[21], 
                                     colors = c("#8fbc8f", "red"), span = TRUE)
    ),
    JUMLAH_PIKR_Persentase = colDef(name = "%")
  ),
  columnGroups = list(
    colGroup(name = "Kampung KB", 
             columns = c("JUMLAH_KKB_Ada", "JUMLAH_KKB_Tidak_Ada", "JUMLAH_KKB_Persentase")),
    colGroup(name = "Rumah DataKu", 
             columns = c("JUMLAH_RDK_Ada", "JUMLAH_RDK_Tidak_Ada", "JUMLAH_RDK_Persentase")),
    colGroup(name = "Bina Keluarga Balita", 
             columns = c("JUMLAH_BKB_Ada", "JUMLAH_BKB_Tidak_Ada", "JUMLAH_BKB_Persentase")),
    colGroup(name = "Bina Keluarga Remaja", 
             columns = c("JUMLAH_BKR_Ada", "JUMLAH_BKR_Tidak_Ada", "JUMLAH_BKR_Persentase")),
    colGroup(name = "Bina Keluarga Lansia", 
             columns = c("JUMLAH_BKL_Ada", "JUMLAH_BKL_Tidak_Ada", "JUMLAH_BKL_Persentase")),
    colGroup(name = "UPPKA", 
             columns = c("JUMLAH_UPPKA_Ada", "JUMLAH_UPPKA_Tidak_Ada", "JUMLAH_UPPKA_Persentase")),
    colGroup(name = "PIK-Remaja", 
             columns = c("JUMLAH_PIKR_Ada", "JUMLAH_PIKR_Tidak_Ada", "JUMLAH_PIKR_Persentase"))
  ),
  defaultColDef = colDef(
    align = "center",
    minWidth = 130,
    headerStyle = list(background = "#7393B3", color = "black")
  ),
  filterable = TRUE,
  bordered = TRUE, striped = TRUE, highlight = TRUE,
  resizable = TRUE,
  theme = reactableTheme(
    borderColor = "#808080",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
  )
)

## KECMATAN
# Menghitung jumlah "Ada", "Tidak Ada", dan persentase untuk setiap kolom JUMLAH_ yang dikelompokkan berdasarkan KECAMATAN
rekapitulasi <- data_poktan_setara %>%
  group_by(KABUPATEN, KECAMATAN) %>%
  summarise(
    JUMLAH_KKB_Ada = sum(JUMLAH_KKB == "Ada"),
    JUMLAH_KKB_Tidak_Ada = sum(JUMLAH_KKB == "Tidak Ada"),
    JUMLAH_KKB_Persentase = paste0(round(JUMLAH_KKB_Ada / (JUMLAH_KKB_Ada + JUMLAH_KKB_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_RDK_Ada = sum(JUMLAH_RDK == "Ada"),
    JUMLAH_RDK_Tidak_Ada = sum(JUMLAH_RDK == "Tidak Ada"),
    JUMLAH_RDK_Persentase = paste0(round(JUMLAH_RDK_Ada / (JUMLAH_RDK_Ada + JUMLAH_RDK_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_BKB_Ada = sum(JUMLAH_BKB == "Ada"),
    JUMLAH_BKB_Tidak_Ada = sum(JUMLAH_BKB == "Tidak Ada"),
    JUMLAH_BKB_Persentase = paste0(round(JUMLAH_BKB_Ada / (JUMLAH_BKB_Ada + JUMLAH_BKB_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_BKR_Ada = sum(JUMLAH_BKR == "Ada"),
    JUMLAH_BKR_Tidak_Ada = sum(JUMLAH_BKR == "Tidak Ada"),
    JUMLAH_BKR_Persentase = paste0(round(JUMLAH_BKR_Ada / (JUMLAH_BKR_Ada + JUMLAH_BKR_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_BKL_Ada = sum(JUMLAH_BKL == "Ada"),
    JUMLAH_BKL_Tidak_Ada = sum(JUMLAH_BKL == "Tidak Ada"),
    JUMLAH_BKL_Persentase = paste0(round(JUMLAH_BKL_Ada / (JUMLAH_BKL_Ada + JUMLAH_BKL_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_UPPKA_Ada = sum(JUMLAH_UPPKA == "Ada"),
    JUMLAH_UPPKA_Tidak_Ada = sum(JUMLAH_UPPKA == "Tidak Ada"),
    JUMLAH_UPPKA_Persentase = paste0(round(JUMLAH_UPPKA_Ada / (JUMLAH_UPPKA_Ada + JUMLAH_UPPKA_Tidak_Ada) * 100, 2), "%"),
    
    JUMLAH_PIKR_Ada = sum(JUMLAH_PIKR == "Ada"),
    JUMLAH_PIKR_Tidak_Ada = sum(JUMLAH_PIKR == "Tidak Ada"),
    JUMLAH_PIKR_Persentase = paste0(round(JUMLAH_PIKR_Ada / (JUMLAH_PIKR_Ada + JUMLAH_PIKR_Tidak_Ada) * 100, 2), "%")
  ) %>%
  left_join(pembina_kec, by = c("KABUPATEN", "KECAMATAN"))

reactable(
  rekapitulasi,
  columns = list(
    JUMLAH_KKB_Ada = colDef(name = "Ada"),
    JUMLAH_KKB_Tidak_Ada = colDef(name = "Tidak Ada",
                                  style = color_scales(
                                    rekapitulasi[4], 
                                    colors = c("#8fbc8f", "red"), span = TRUE)
                                  ),
    JUMLAH_KKB_Persentase = colDef(name = "%"),
    
    JUMLAH_RDK_Ada = colDef(name = "Ada"),
    JUMLAH_RDK_Tidak_Ada = colDef(name = "Tidak Ada",
                                  style = color_scales(
                                    rekapitulasi[7], 
                                    colors = c("#8fbc8f", "red"), span = TRUE)
                                  ),
    JUMLAH_RDK_Persentase = colDef(name = "%"),
    
    JUMLAH_BKB_Ada = colDef(name = "Ada"),
    JUMLAH_BKB_Tidak_Ada = colDef(name = "Tidak Ada",
                                  style = color_scales(
                                    rekapitulasi[10], 
                                    colors = c("#8fbc8f", "red"), span = TRUE)
                                  ),
    JUMLAH_BKB_Persentase = colDef(name = "%"),
    
    JUMLAH_BKR_Ada = colDef(name = "Ada"),
    JUMLAH_BKR_Tidak_Ada = colDef(name = "Tidak Ada",
                                  style = color_scales(
                                    rekapitulasi[13], 
                                    colors = c("#8fbc8f", "red"), span = TRUE)
                                  ),
    JUMLAH_BKR_Persentase = colDef(name = "%"),
    
    JUMLAH_BKL_Ada = colDef(name = "Ada"),
    JUMLAH_BKL_Tidak_Ada = colDef(name = "Tidak Ada",
                                  style = color_scales(
                                    rekapitulasi[16], 
                                    colors = c("#8fbc8f", "red"), span = TRUE)
                                  ),
    JUMLAH_BKL_Persentase = colDef(name = "%"),
    
    JUMLAH_UPPKA_Ada = colDef(name = "Ada"),
    JUMLAH_UPPKA_Tidak_Ada = colDef(name = "Tidak Ada",
                                    style = color_scales(
                                      rekapitulasi[19], 
                                      colors = c("#8fbc8f", "red"), span = TRUE)
                                    ),
    JUMLAH_UPPKA_Persentase = colDef(name = "%"),
    
    JUMLAH_PIKR_Ada = colDef(name = "Ada"),
    JUMLAH_PIKR_Tidak_Ada = colDef(name = "Tidak Ada",
                                   style = color_scales(
                                     rekapitulasi[22], 
                                     colors = c("#8fbc8f", "red"), span = TRUE)
                                   ),
    JUMLAH_PIKR_Persentase = colDef(name = "%")
  ),
  columnGroups = list(
    colGroup(name = "Kampung KB", 
             columns = c("JUMLAH_KKB_Ada", "JUMLAH_KKB_Tidak_Ada", "JUMLAH_KKB_Persentase")),
    colGroup(name = "Rumah DataKu", 
             columns = c("JUMLAH_RDK_Ada", "JUMLAH_RDK_Tidak_Ada", "JUMLAH_RDK_Persentase")),
    colGroup(name = "Bina Keluarga Balita", 
             columns = c("JUMLAH_BKB_Ada", "JUMLAH_BKB_Tidak_Ada", "JUMLAH_BKB_Persentase")),
    colGroup(name = "Bina Keluarga Remaja", 
             columns = c("JUMLAH_BKR_Ada", "JUMLAH_BKR_Tidak_Ada", "JUMLAH_BKR_Persentase")),
    colGroup(name = "Bina Keluarga Lansia", 
             columns = c("JUMLAH_BKL_Ada", "JUMLAH_BKL_Tidak_Ada", "JUMLAH_BKL_Persentase")),
    colGroup(name = "UPPKA", 
             columns = c("JUMLAH_UPPKA_Ada", "JUMLAH_UPPKA_Tidak_Ada", "JUMLAH_UPPKA_Persentase")),
    colGroup(name = "PIK-Remaja", 
             columns = c("JUMLAH_PIKR_Ada", "JUMLAH_PIKR_Tidak_Ada", "JUMLAH_PIKR_Persentase"))
  ),
  defaultColDef = colDef(
    align = "center",
    minWidth = 130,
    headerStyle = list(background = "#7393B3", color = "black")
  ),
  filterable = TRUE,
  bordered = TRUE, striped = TRUE, highlight = TRUE,
  resizable = TRUE,
  theme = reactableTheme(
    borderColor = "#808080",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
  )
)

## desa/kel
rekapitulasi <- data_poktan_setara %>%
  select(-c(1:2)) %>%
  left_join(nama_pkb[, -c(1, 6:8)], 
            by = c("KABUPATEN" = "Kabupaten", "KECAMATAN" = "Kecamatan", "KELURAHAN" = "Kelurahan"))

reactable(
  rekapitulasi,
  columns = list(
    JUMLAH_KKB = colDef(name = "Kampung KB",
                        cell = function(value) {
                          color <- if (value == "Ada") {
                            "#8fbc8f"
                          } else if (value == "Tidak Ada") {
                            "red"
                          } else if (value == "Pending") {
                            "orange"
                          }
                          div(style = list(background = color, color = "white", padding = "6px"), value)
                        }
      ),
    JUMLAH_RDK = colDef(name = "Rumah DataKu",
                        cell = function(value) {
                          color <- if (value == "Ada") {
                            "#8fbc8f"
                          } else if (value == "Tidak Ada") {
                            "red"
                          } else if (value == "Pending") {
                            "orange"
                          }
                          div(style = list(background = color, color = "white", padding = "6px"), value)
                        }
    ),
    JUMLAH_BKB = colDef(name = "Bina Keluarga Balita",
                        cell = function(value) {
                          color <- if (value == "Ada") {
                            "#8fbc8f"
                          } else if (value == "Tidak Ada") {
                            "red"
                          } else if (value == "Pending") {
                            "orange"
                          }
                          div(style = list(background = color, color = "white", padding = "6px"), value)
                        }
    ),
    JUMLAH_BKR = colDef(name = "Bina Keluarga Remaja",
                        cell = function(value) {
                          color <- if (value == "Ada") {
                            "#8fbc8f"
                          } else if (value == "Tidak Ada") {
                            "red"
                          } else if (value == "Pending") {
                            "orange"
                          }
                          div(style = list(background = color, color = "white", padding = "6px"), value)
                        }
    ),
    JUMLAH_BKL = colDef(name = "Bina Keluarga Lansia",
                        cell = function(value) {
                          color <- if (value == "Ada") {
                            "#8fbc8f"
                          } else if (value == "Tidak Ada") {
                            "red"
                          } else if (value == "Pending") {
                            "orange"
                          }
                          div(style = list(background = color, color = "white", padding = "6px"), value)
                        }
    ),
    JUMLAH_UPPKA = colDef(name = "UPPKA",
                        cell = function(value) {
                          color <- if (value == "Ada") {
                            "#8fbc8f"
                          } else if (value == "Tidak Ada") {
                            "red"
                          } else if (value == "Pending") {
                            "orange"
                          }
                          div(style = list(background = color, color = "white", padding = "6px"), value)
                        }
    ),
    JUMLAH_PIKR = colDef(name = "PIK-Remaja",
                        cell = function(value) {
                          color <- if (value == "Ada") {
                            "#8fbc8f"
                          } else if (value == "Tidak Ada") {
                            "red"
                          } else if (value == "Pending") {
                            "orange"
                          }
                          div(style = list(background = color, color = "white", padding = "6px"), value)
                        }
    )
  ),
  defaultColDef = colDef(
    align = "center",
    minWidth = 130,
    headerStyle = list(background = "#7393B3", color = "black")
  ),
  filterable = TRUE,
  bordered = TRUE, striped = TRUE, highlight = TRUE,
  resizable = TRUE,
  theme = reactableTheme(
    borderColor = "#808080",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
  )
)



