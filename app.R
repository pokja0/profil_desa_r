#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(waiter)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(leaflegend)
library(htmltools)
library(bs4Dash)
library(plotly)
library(reactablefmtr)
library(tidyr)
library(bsicons)
library(stringr)
#
library(shinythemes)
library(future)
library(promises)
future::plan(multisession)
library(fst)

# filter_kabupaten <- c("PASANGKAYU", "MAMUJU")
# filter_kecamatan <- c("PASANGKAYU", "MAMUJU")
# filter_desa <- c("PASANGKAYU", "BINANGA")

# filter_kabupaten <- unique(data_poktan$KABUPATEN)
# filter_kecamatan <- unique(data_poktan$KECAMATAN)
# filter_desa <- unique(data_poktan$KELURAHAN)

#setwd("/home/hi/Documents/projects/profil_desa_r")
##data
daftar_bulan = c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI", "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER")

data_poktan = read_fst("data/data_profil_poktan.fst")


link_shiny <- tags$a(
  shiny::icon("instagram"), "Instagram BKKBN Sulbar",
  href = "https://www.instagram.com/bkkbnsulbar/",
  target = "_blank"
)
link_posit <- tags$a(
  shiny::icon("globe"), "Website BKKBN Sulbar",
  href = "https://sulbar-new.bkkbn.go.id/",
  target = "_blank"
)

csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

# [1] "cerulean"  "cosmo"ok     "cyborg"    "darkly"    "flatly"   
# [6] "journal"   "litera"    "lumen"     "lux"       "materia"  
# [11] "minty"     "morph"     "pulse"     "quartz"    "sandstone"
# [16] "simplex"   "sketchy"   "slate"     "solar"     "spacelab" 
# [21] "superhero" "united"    "vapor"     "yeti"      "zephyr"

# Define UI for application that draws a histogram
ui <- page_navbar(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  title = "Profil Desa",
  nav_panel(
    "Dataset",
    fluidRow(
      column(4,
             selectInput("pilih_bulan_dataset", "Pilih Bulan", 
                         choices = daftar_bulan[1:9],
                         selected = "SEPTEMBER")),
      column(4,
             selectInput("dataset", "Pilih DataFrame:",
                         choices = c("Pembentukan Poktan/Setara" = "poktan_rampung",
                                     "Unmet Need <= 5" = "unmet_need_0"#,
                                     # "MCPR" = "mcpr",
                                     # "Pendidikan Dasar" = "titik_sd",
                                     # "Pendidikan Menengah" = "titik_smp",
                                     # "Kelompok Umur Laki-laki" = "kelompok_umur_lk",
                                     # "Kelompok Umur Perempuan" = "kelompok_umur_pr"
                         ))
      ),
      column(4,
             conditionalPanel(
               condition = "input.dataset == 'poktan_rampung' | input.dataset == 'mcpr'",
               selectInput("tingkat_wilayah", "Tingkat Wilayah",
                           choices = c("KABUPATEN", "KECAMATAN", "DESA/KELURAHAN"))
             )
      )
    ),
    fluidRow(
      column(
        4,
        layout_column_wrap(
          fixed_width = TRUE,
          input_task_button(label_busy = "Sedang Proses",
                            id = "cari_dataset",
                            label = "Cari",
                            style = "jelly", 
                            color = "primary", size = "sm"
          )
        ), #layout column input 2
      )
    ),
    fluidRow(  
      column(4,
             csvDownloadButton("tabel_data", filename = paste0("data.csv")),
             #downloadButton("downloadData", "Download")
      )
    ),
    fluidRow(
      column(12,
             h5(textOutput("judul_tabel_data"), style="text-align: center;")
      )
    ),
    fluidRow(
      column(12,
             reactableOutput("tabel_data")
      )
    )
  ), #nav panel
  nav_panel(
    title = "Dashboard", 
    page_fluid(
      autoWaiter(),
      tags$div(
        style = "display: flex; align-items: center; justify-content: center;",
        tags$img(src = "https://bkkbnsulbar.id/wp-content/uploads/2022/12/cropped-logobkkbnsulbar.png", height = "100px"),
        tags$h3("Profil Desa", style = "margin-left: 10px;")
      ),
      br(),
      layout_column_wrap(
        selectInput("pilih_kab", "Pilih Kabupaten", 
                    choices = c("SEMUA KABUPATEN", "PASANGKAYU", "MAMUJU TENGAH",
                                "MAMUJU", "MAJENE", "POLEWALI MANDAR", "MAMASA")),
        selectInput("pilih_kec", "Pilih Kecamatan", choices = c()),
        selectInput("pilih_desa_kel", "Pilih Desa/Kel", choices = c()),
        selectInput("pilih_bulan", "Pilih Bulan", 
                    choices = daftar_bulan[1:9],
                    selected = "SEPTEMBER")
        #uiOutput("pilih_bulan"),
      ), #layoyt_column input 1
      layout_column_wrap(
        fixed_width = TRUE,
        input_task_button(
          label_busy = "Sedang Proses",
          id = "cari_rekap",
          label = "Cari"
        )
      ), #layout column input 2
      h5(textOutput("tes_input_rekap"), style="text-align: center;"),
      navset_card_pill(
        nav_panel(
          "Kependudukan",
          layout_column_wrap(
            card(full_screen = T,
                 leafletOutput("peta_titik_rekap")
            ),
            card(
              layout_column_wrap(
                uiOutput("card_profil_poktan_rekap"),
                uiOutput("card_profil_sd_rekap")
              )
            )
          ),
          layout_column_wrap(
            card(
              plotlyOutput("grafik_piramida_rekap"), 
              full_screen = T
            ),
            card(
              reactableOutput("tabel_piramida_rekap")
            )
          ), #layout column
          fluidRow(
            column(
              4,
              value_box(
                h1(span(
                  strong("Total Keluarga: "),
                  style = "font-size:20px;")),
                textOutput("jumlah_keluarga"),
                p("Rekapitulasi KRS Semester 1 - 2023"),
                showcase = bs_icon("person-square"),
                theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E")
              )
            )
          ),
          layout_column_wrap(
            plotlyOutput("bar_kepemilikan_rumah"),
            plotlyOutput("bar_sumber_air"),
            plotlyOutput("bar_bab")
          )
        ), #nav panel Kependudukan
        nav_panel(
          "Keluarga Berencana",
          layout_column_wrap(
            uiOutput("vb_unmet_need_rekap"),
            value_box(
              h1(span(
                strong("Tenaga Kesehatan"),
                style = "font-size:20px;")),
              textOutput("sdm_vb_1_rekap"),
              showcase = bs_icon("person-square"),
              theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E")
            ),            
            value_box(
              h1(span(
                strong("Tempat Pelayanan KB"),
                style = "font-size:20px;")),
              textOutput("vb_tp_kb_rekap"),
              showcase = bs_icon("person-square"),
              theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E")
            ),
            uiOutput("vb_mkjp_rekap")
          ),
          layout_column_wrap(
            value_box(
              h1(span(
                strong("Pasangan Usia Subur"),
                style = "font-size:20px;")),
              textOutput("jumlah_pus"),
              showcase = plotlyOutput("line_pus"),
              theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E")
            ),
            value_box(
              h1(span(
                strong("MCPR"),
                style = "font-size:20px;")),
              textOutput("jumlah_mcpr"),
              showcase = plotlyOutput("line_mcpr"),
              theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E")
            )
          ),
          layout_column_wrap(
            plotlyOutput("bar_mix_kontra"),
            plotlyOutput("line_pa"),
            plotlyOutput("donut_tenaga_terlatih")
          )
        ), #nav panel KB
        nav_panel(
          "Monitoring KRS",
          fluidRow(
            shiny::column(3,
                          uiOutput("profil_stunting_rekap"),
                          navset_card_underline(
                            title = "Faktor Resiko KRS",
                            nav_panel(
                              "Jumlah", 
                              plotlyOutput("bar_faktor_resiko_jumlah")
                            ),
                            nav_panel(
                              "Persentase", 
                              plotlyOutput("bar_faktor_resiko_persen")
                            )
                          ),
                          uiOutput("jumlah_posyandu")
                          
            ),
            shiny::column(9,
                          card(
                            card_header(
                              "VERVAL KRS"
                            ),
                            card_body(
                              uiOutput("jumlah_tpk_rekap"),
                              layout_column_wrap(
                                card(
                                  card_header(
                                    "Keluarga Dengan PUS Hamil"
                                  ),
                                  fluidRow(
                                    column(12,
                                           plotlyOutput("pie_hamil_verval", height = "250px")
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           uiOutput("legend_hamil_verval")
                                    )
                                  )
                                ),
                                card(
                                  card_header(
                                    "Keluarga Dengan Catin"
                                  ),
                                  fluidRow(
                                    column(12,
                                           plotlyOutput("pie_catin_verval", height = "250px")
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           uiOutput("legend_catin_verval")
                                    )
                                  )
                                )
                              ),
                              layout_column_wrap(
                                card(
                                  card_header(
                                    "Keluarga Dengan Baduta"
                                  ),
                                  fluidRow(
                                    column(12,
                                           plotlyOutput("pie_baduta_verval", height = "250px")
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           uiOutput("legend_baduta_verval")
                                    )
                                  )
                                ),
                                card(
                                  card_header(
                                    "Keluarga Dengan Balita"
                                  ),
                                  fluidRow(
                                    column(12,
                                           plotlyOutput("pie_balita_verval", height = "250px")
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           uiOutput("legend_balita_verval")
                                    )
                                  )
                                )
                              ),
                            )
                          )
            )
          )
        ) #nav panel KRS
      ) #nav_card_pil
    ), # page fluid
  ), #Nav panel profil
  # nav_panel(
  #   "Analisis",
  #   uiOutput("analisis")
  # ), #nav panel analisis
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_shiny),
    nav_item(link_posit)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #bs_themer()
  ## data
  # peta

  batas_sulbar <- readRDS("data/batas_sulbar.rds")
  #batas_sulbar <- readRDS("data/batas_sulbar.rds")
  #write_fst(batas_sulbar, "data/data_batas_sulbar.fst")
  
  #titik_puskesmas <- fread("data/titik_puskesmas.csv")
  #write_fst(titik_puskesmas, "data/data_titik_puskesmas.fst")
  titik_puskesmas <- read_fst("data/data_titik_puskesmas.fst")
  
  # titik_sd <- fread("data/dikdasulbar.csv")
  # write_fst(titik_sd, "data/data_dikdasulbar.fst")
  titik_sd <- read_fst("data/data_dikdasulbar.fst")
  titik_sd$Latitude <- as.numeric(titik_sd$Latitude)
  
  # titik_smp <- fread("data/dikmen.csv")
  # write_fst(titik_smp, "data/data_dikmen.fst")
  titik_smp <- read_fst("data/data_dikmen.fst")
  titik_smp$Latitude <- as.numeric(titik_smp$Latitude)
  
  data_desa <- read_fst("data/data_profil_poktan.fst")
  
  #kepemilikan poktan
  filter_poktan <- function(data, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan) {
    hasil <- data %>%
      filter(
        KABUPATEN %in% filter_kabupaten,
        KECAMATAN %in% filter_kecamatan,
        KELURAHAN %in% filter_desa,
        BULAN %in% filter_bulan
      )
    
    return(hasil)
  }
  
  # bkb <- fread("data/data_bkb.csv")
  # write.fst(bkb, "data/data_bkb.fst")
  bkb <- read_fst("data/data_bkb.fst")
  
  # bkr <- fread("data/data_bkr.csv")
  # write.fst(bkr, "data/data_bkr.fst")
  bkr <- read_fst("data/data_bkr.fst")
  
  # bkl <- fread("data/data_bkl.csv")
  # write.fst(bkl, "data/data_bkl.fst")
  bkl <- read_fst("data/data_bkl.fst")
  
  # uppka <- fread("data/data_uppka.csv")
  # write.fst(uppka, "data/data_uppka.fst")
  uppka <- read_fst("data/data_uppka.fst")
  
  # pikr <- fread("data/data_pikr.csv")
  # write.fst(pikr, "data/data_pikr.fst")
  pikr <- read_fst("data/data_pikr.fst")
  
  # kkb <- fread("data/data_kkb.csv")
  # write.fst(kkb, "data/data_kkb.fst")
  kkb <- read_fst("data/data_kkb.fst")
  
  # rdk <- fread("data/data_rdk.csv")
  # write.fst(rdk, "data/data_rdk.fst")
  rdk <- read_fst("data/data_rdk.fst")
  
  # daftar_desa = read.csv("data/data_daftar_desa.csv")
  # write.fst(daftar_desa, "data/data_daftar_desa.fst")
  daftar_desa <- read_fst("data/data_daftar_desa.fst")
  
  # data_sumber_daya <- fread("data/profil_sumber_daya.csv")
  # write.fst(data_sumber_daya, "data/profil_sumber_daya.fst")
  data_sumber_daya <- read_fst("data/profil_sumber_daya.fst")
  
  # kelompok_umur_lk <- fread("data/PIRAMIDA PENDUDUK - Laki-laki.csv")
  # write.fst(kelompok_umur_lk, "data/data_piramida_lk.fst")
  kelompok_umur_lk <- read_fst("data/data_piramida_lk.fst")
  # kelompok_umur_pr <- fread("data/PIRAMIDA PENDUDUK - Perempuan.csv")
  # write.fst(kelompok_umur_pr, "data/data_piramida_perempuan.fst")
  kelompok_umur_pr <- read_fst("data/data_piramida_perempuan.fst")
  
  # data_krs <- fread("data/data_KRS_2023.csv")
  # write.fst(data_krs, "data/data_KRS_2023.fst")
  data_krs <- read_fst("data/data_KRS_2023.fst")
  data_krs <-data_krs %>%
    mutate(across(6:28, as.numeric))
  
  # data_pus <- fread("data/data_pus.csv")
  # write.fst(data_pus, "data/data_pus.fst")
  data_pus <- read_fst("data/data_pus.fst")
  data_pus <- data_pus %>%
    mutate(across(6:7, as.numeric))
  
  # sdm_kb <- fread("data/data_faskes_siga.csv")
  # write.fst(sdm_kb, "data/data_faskes_siga.fst")
  sdm_kb <- read_fst("data/data_faskes_siga.fst")
  
  # data_mix_kontra <- fread("data/data_mix_kontra.csv")
  # write.fst(data_mix_kontra, "data/data_mix_kontra.fst")
  data_mix_kontra <- read_fst("data/data_mix_kontra.fst")
  data_mix_kontra <- data_mix_kontra %>%
    mutate(across(6:16, as.numeric))
  
  # data_faskes <- fread("data/data_faskes_siga.csv")
  # write.fst(data_faskes, "data/data_faskes_siga.fst")
  data_faskes <- read_fst("data/data_faskes_siga.fst")
  
  # faktor_krs <- fread("data/data_faktor_krs.csv")
  # write.fst(faktor_krs, "data/data_faktor_krs.fst")
  faktor_krs <- read_fst("data/data_faktor_krs.fst")
  
  # data_posyandu <- fread("data/data_posyandu_dummy.csv")
  # write.fst(data_posyandu, "data/data_posyandu_dummy.fst")
  data_posyandu <- read_fst("data/data_posyandu_dummy.fst")
  
  # keberadaan_posyandu <- fread("data/keberadaan_posyandu.csv")
  # write.fst(keberadaan_posyandu, "data/data_keberadaan_posyandu.fst")
  keberadaan_posyandu <- read_fst("data/data_keberadaan_posyandu.fst")
  
  # nama_pkb <- fread("data/nama pkb.csv")
  # write.fst(nama_pkb, "data/data_pkb.fst")
  nama_pkb <- read_fst("data/data_pkb.fst")
  
  # nama_tpk <- fread("data/nama_tpk.csv")
  # write.fst(nama_tpk, "data/data_nama_tpk.fst")
  nama_tpk <- read_fst("data/data_nama_tpk.fst")
  
  # pembina_kab <- fread("https://raw.githubusercontent.com/pokja0/profil_desa_r/refs/heads/main/data/data_pembina_wilayah_kab.csv")
  # write.fst(pembina_kab, "data/data_pembina_wilayah_kab.fst")
  pembina_kab <- read_fst("data/data_pembina_wilayah_kab.fst")
  # 
  # pembina_kec <- fread("data/data_pembina_wilayah_kec.csv")
  # write.fst(pembina_kec, "data/data_pembina_wilayah_kec.fst")
  pembina_kec <- read_fst("data/data_pembina_wilayah_kec.fst")
  
  # verval_krs <- fread("data/data_monitoring_krs.csv")
  # write.fst(verval_krs, "data/data_monitoring_krs.fst")
  verval_krs <- read_fst("data/data_monitoring_krs.fst")
  ##batas data
  
  ## fungsi
  pilih_bulan_sebelumnya <- function(bulan) {
    daftar_bulan <- c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI", "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER")
    
    index <- match(toupper(bulan), daftar_bulan) # Mencari indeks bulan yang diberikan
    if (!is.na(index) && index > 1) { # Jika indeks ditemukan dan bukan bulan pertama
      return(daftar_bulan[index - 1]) # Mengembalikan bulan sebelumnya
    } else {
      return("JANUARI") # Jika bulan tidak ditemukan atau merupakan bulan pertama, kembalikan NA
    }
  }
  
  pilih_sampai_bulan <- function(bulan_yang_dipilih) {
    daftar_bulan <- c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI", "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER")
    
    # Temukan indeks bulan yang dipilih
    indeks_bulan <- which(daftar_bulan == toupper(bulan_yang_dipilih))
    
    # Validasi jika bulan tidak ditemukan
    if (length(indeks_bulan) == 0) {
      cat("Bulan tidak ditemukan.")
      return(NULL)
    }
    
    # Buat subset daftar bulan dari Januari hingga bulan yang dipilih
    daftar_bulan_subset <- daftar_bulan[1:indeks_bulan]
    
    return(daftar_bulan_subset)
  }
  
  bandingkan_bulan_rekap <- function(bulan_ini, bulan_lalu) {
    if (bulan_ini > bulan_lalu) {
      return("arrow-up-square-fill")
    } else if (bulan_ini < bulan_lalu) {
      return("arrow-down-square-fill")
    } else {
      return("arrow-left-right")
    }
  }
  
  cek_naik_turun <- function(bulan_ini, bulan_lalu) {
    if (bulan_ini > bulan_lalu) {
      return("Naik")
    } else if (bulan_ini < bulan_lalu) {
      return("Turun")
    } else {
      return("Sama")
    }
  }
  
  ubah_angka <- function(angka) {
    if (angka < 0 | is.infinite(angka) | is.nan(angka) | is.na(angka)) {
      return(0)
    } else {
      return(angka)
    }
  }
  ##batas fungsi 
  
  ## Input
  observeEvent(input$pilih_kab, {
    if (input$pilih_kab == "SEMUA KABUPATEN") {
      updateSelectInput(session, "pilih_kec",
                        choices = c("SEMUA KECAMATAN"))
    } else {
      daftar_kecamatan = data_poktan %>%
        select(KABUPATEN, KECAMATAN) %>%
        filter(KABUPATEN == input$pilih_kab) %>%
        select(KECAMATAN)
      daftar_kecamatan = daftar_kecamatan$KECAMATAN
      updateSelectInput(session, "pilih_kec",
                        choices = c("SEMUA KECAMATAN",
                                    daftar_kecamatan))
    }
  })
  
  observeEvent(input$pilih_kec, {
    if (input$pilih_kec == "SEMUA KECAMATAN") {
      updateSelectInput(session, "pilih_desa_kel",
                        choices = c("SEMUA DESA/KELURAHAN"))
    } else {
      daftar_kel = data_poktan %>%
        select(KECAMATAN, KELURAHAN) %>%
        filter(KECAMATAN == input$pilih_kec) %>%
        select(KELURAHAN)
      daftar_kel = daftar_kel$KELURAHAN
      updateSelectInput(session, "pilih_desa_kel",
                        choices = c("SEMUA DESA/KELURAHAN", 
                                    daftar_kel))
    }
  })
  ## akhir input
  
  ##judul
  values <- reactiveValues(default = 0)
  
  observeEvent(input$cari_rekap,{
    values$default <- input$cari_rekap
  })
  
  teks_judul_rekap <- eventReactive(input$cari_rekap, {
    if(input$pilih_kab == "SEMUA KABUPATEN"){
      nama_daerah = "SULAWESI BARAT"
      tingkat_daerah = "PROVINSI"
    } else if(input$pilih_kec == "SEMUA KECAMATAN"){
      nama_daerah = input$pilih_kab
      tingkat_daerah = "KABUPATEN"
    } else if(input$pilih_desa_kel == "SEMUA DESA/KELURAHAN"){
      nama_daerah = input$pilih_kec
      tingkat_daerah = "KECAMATAN"
    } else{
      nama_daerah = value_filter_desa_kel()
      tingkat_daerah = "DESA/KELURAHAN"
    }
    teks <- paste("PROFIL", tingkat_daerah, nama_daerah, "-", input$pilih_bulan)
    # if(tingkat_daerah == "KELURAHAN"){
    #   teks <- paste0("PROFIL DESA/", tingkat_daerah, " ", nama_daerah, " - ", input$bulan_rekap)
    # } else{
    #   teks <- paste("PROFIL", tingkat_daerah, nama_daerah, "-", input$bulan_rekap)
    # }
    
    
  })
  
  output$tes_input_rekap <- renderText({
    if(values$default == 0){
      teks = "Klik Cari Untuk Menampilkan Halaman"
    }
    else{
      teks = teks_judul_rekap()
    }
  })
  ##akhir judul
  
  ## set filter
  # filter kab
  value_filter_kab <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_rekap, {
    kondisi_input = input$pilih_kab
    if (kondisi_input == "SEMUA KABUPATEN"){
      filter_kabupaten = unique(data_poktan$KABUPATEN)
    } else{
      filter_kabupaten = input$pilih_kab
    }
    value_filter_kab(filter_kabupaten) 
  })
  
  #kecamatan
  value_filter_kec <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_rekap, {
    kondisi_input = input$pilih_kec
    filter_kabupaten = value_filter_kab()
    
    if (kondisi_input == "SEMUA KECAMATAN"){
      daftar_kecamatan = data_poktan %>%
        select(KABUPATEN, KECAMATAN) %>%
        filter(KABUPATEN %in% filter_kabupaten) %>%
        select(KECAMATAN)
      filter_kecamatan = daftar_kecamatan$KECAMATAN
    } else{
      filter_kecamatan = input$pilih_kec
    }
    value_filter_kec(filter_kecamatan) 
  })
  
  # desa
  value_filter_desa_kel <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_rekap, {
    kondisi_input = input$pilih_desa_kel
    filter_kabupaten = value_filter_kab()
    filter_kecamatan = value_filter_kec()
    
    if (kondisi_input == "SEMUA DESA/KELURAHAN"){
      daftar_kel = data_poktan %>%
        select(KABUPATEN, KECAMATAN, KELURAHAN) %>%
        filter(
          KABUPATEN %in% filter_kabupaten,
          KECAMATAN %in% filter_kecamatan) %>%
        select(KELURAHAN)
      filter_desa_kel = daftar_kel$KELURAHAN
    } else{
      filter_desa_kel = input$pilih_desa_kel
    }
    value_filter_desa_kel(filter_desa_kel) 
  })
  
  # bulan
  value_filter_bulan <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_rekap, {
    
    value_filter_bulan(input$pilih_bulan) 
  })
  
  ## set filter
  
  ##profil
  #peta
  peta_titik_rekap_task <- ExtendedTask$new(function(kecamatan, desa_kel){
    future_promise({
      batas_sulbar <- batas_sulbar
      kecamatan <- kecamatan
      
      desa_kel <- desa_kel
      # filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      #   group_by(PROVINSI) %>%
      # 
      # tingkat_daerah <- tingkat_daerah_filter()
      # nama_daerah <- nama_daerah_filter()
      titik_puskesmas <- titik_puskesmas %>%
        filter(KECAMATAN %in% kecamatan)
      if(nrow(titik_puskesmas) <= 0){
        titik_puskesmas <- data.frame(
          PUSKESMAS = "Puskesmas contoh Contoh",
          LATITUDE = 0,
          LONGITUDE = 0,
          Kelurahan_Desa = "Kelurahan Contoh"
        )
      } else{
        titik_puskesmas = titik_puskesmas
      }
      
      if(titik_sd$Latitude[1] == 0){
        cek_puskesmas = "titik_puskesmas"
      } else{
        cek_puskesmas = ""
      }
      
      titik_sd <- titik_sd %>%
        filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
        select(KECAMATAN, Nama_Sekolah, Latitude, Longitude, KELURAHAN)
      
      if(nrow(titik_sd) <= 0){
        titik_sd <- data.frame(
          Nama_Sekolah = "Puskesmas Contoh",
          Latitude = 0,
          Longitude = 0,
          Kelurahan_Desa = "Kelurahan Contoh"
        )
      } else{
        titik_sd = titik_sd
      }
      
      if(titik_sd$Latitude[1] == 0){
        cek_sd = "titik_sd"
      } else{
        cek_sd = ""
      }
      
      titik_smp <- titik_smp %>%
        filter(KECAMATAN %in% kecamatan) %>%
        select(KECAMATAN, Nama_Sekolah, Latitude, Longitude, KELURAHAN)
      
      if(nrow(titik_smp) <= 0){
        titik_smp <- data.frame(
          Nama_Sekolah = "Sekolah Contoh",
          Latitude = 0,
          Longitude = 0,
          Kelurahan_Desa = "Kelurahan Contoh"
        )
      } else{
        titik_smp = titik_smp
      }
      
      if(titik_smp$Latitude[1] == 0){
        cek_smp = "titik_smp"
      } else{
        cek_smp = ""
      }
      
      df_peta <- data_desa %>%
        select(PROVINSI, KABUPATEN, KECAMATAN, KELURAHAN, LATITUDE, LONGITUDE) %>%
        filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel)
      df_peta$LATITUDE <- as.numeric(df_peta$LATITUDE)
      df_peta$LONGITUDE <- as.numeric(df_peta$LONGITUDE)
      
      icon_sekolah <- makeIcon(
        iconUrl = "img/logo_sekolah.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 0, iconAnchorY = 0,
        shadowUrl = "",
        shadowWidth = 50, shadowHeight = 64,
        shadowAnchorX = 4, shadowAnchorY = 62
      )
      
      icon_puskesmas <- makeIcon(
        iconUrl = "img/logo_puskesmas.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 0, iconAnchorY = 0,
        shadowUrl = "",
        shadowWidth = 50, shadowHeight = 64,
        shadowAnchorX = 4, shadowAnchorY = 62
      )
      
      
      leaflet(df_peta) %>% 
        setView(lng = mean(df_peta$LONGITUDE), 
                lat = mean(df_peta$LATITUDE), 
                zoom = 8) %>%
        addPolygons(
          data = batas_sulbar,weight = 2,opacity = 1, fillColor = "white",
          color = "darkgreen", dashArray = "3",fillOpacity = 0.7, label = batas_sulbar$KABUPATEN,
          highlightOptions = highlightOptions(
            weight = 5,color = "#666",  dashArray = "",fillOpacity = 0.7,bringToFront = F),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "10px", direction = "auto")) %>%
        addProviderTiles(providers$CyclOSM) %>%
        setView(lng = mean(df_peta$LONGITUDE), lat = mean(df_peta$LATITUDE), zoom = 7) %>%
        addMarkers(~LONGITUDE, ~LATITUDE, label = ~htmlEscape(KELURAHAN), 
                   clusterOptions = markerClusterOptions()) %>%
        addMarkers(lat = titik_smp$Latitude, lng = titik_smp$Longitude, 
                   clusterOptions = markerClusterOptions(), label = ~htmlEscape(titik_smp$Nama_Sekolah), 
                   icon = icon_sekolah, group = "titik_smp") %>%
        addMarkers(lat = titik_sd$Latitude, lng = titik_sd$Longitude, 
                   clusterOptions = markerClusterOptions(), group = "titik_sd",
                   label = ~htmlEscape(titik_sd$Nama_Sekolah), icon = icon_sekolah) %>%
        addMarkers(lat = titik_puskesmas$LATITUDE, lng = titik_puskesmas$LONGITUDE, group = "titik_puskesmas",
                   clusterOptions = markerClusterOptions(),
                   label = ~htmlEscape(titik_puskesmas$PUSKESMAS), icon = icon_puskesmas) %>%
        addLegendImage(images = c("img/logo_desa.png", "img/logo_sekolah.png",
                                  "img/logo_puskesmas.png"),
                       labels = c('Desa', 'Sekolah', 'Puskesmas'),width = 20, height = 20,
                       orientation = 'vertical',
                       title = htmltools::tags$div('Keterangan',
                                                   style = 'font-size: 20px; text-align: center;'),
                       position = 'topright') %>%
        hideGroup(cek_sd) %>%
        hideGroup(cek_smp)
    })
  }) |> bind_task_button("cari_rekap")
  
  observeEvent(input$cari_rekap,{
    peta_titik_rekap_task$invoke(value_filter_kec(), value_filter_desa_kel())
  })
  
  output$peta_titik_rekap <- renderLeaflet({
    peta_titik_rekap_task$result()
  })
  #batas peta
  
  #card profil_poktan
  output$card_profil_poktan_rekap <- renderUI({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- value_filter_bulan()
    
    bkb = filter_poktan(bkb, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    bkr = filter_poktan(bkr, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    bkl = filter_poktan(bkl, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    uppka = filter_poktan(uppka, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    pikr = filter_poktan(pikr, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    kkb = filter_poktan(kkb, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    rdk = filter_poktan(rdk, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    daftar_desa = daftar_desa %>%
      filter(
        KABUPATEN %in% filter_kabupaten,
        KECAMATAN %in% filter_kecamatan,
        KELURAHAN %in% filter_desa
      )
    
    boxProfile(
      title = "",
      subtitle = "Kepemilikan Poktan",
      bordered = TRUE,
      boxProfileItem(
        title = "Desa/Kelurahan:",    
        description = nrow(daftar_desa)),
      boxProfileItem(
        title = "Kampung KB:",    
        description = sum(as.numeric(kkb$JUMLAH_KKB))),
      boxProfileItem(
        title = "Rumah Dataku:",
        description = sum(as.numeric(rdk$JUMLAH_RDK))),
      boxProfileItem(
        title = "Bina Keluarga Balita:",
        description = sum(as.numeric(bkb$JUMLAH_BKB))),  
      boxProfileItem(
        title = "Bina Keluarga Remaja:",
        description = sum(as.numeric(bkr$JUMLAH_BKR))),  
      boxProfileItem(
        title = "Bina Keluarga Lansia:",
        description = sum(as.numeric(bkl$JUMLAH_BKL))),
      boxProfileItem(
        title = "UPPKA:",
        description = sum(as.numeric(uppka$JUMLAH_UPPKA))),
      boxProfileItem(
        title = "PIK-R:",
        description = sum(as.numeric(pikr$JUMLAH_PIKR)))
    )
    
  })
  
  #card profil sd
  output$card_profil_sd_rekap <- renderUI({
    req(input$cari_rekap)
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    df_sd_rekap <- data_sumber_daya %>%
      filter(KECAMATAN %in% filter_kecamatan, KELURAHAN %in% filter_desa) %>%
      group_by(PROVINSI) %>%
      dplyr::summarise(
        LUAS_WILAYAH = sum(LUAS_WILAYAH),
        JUMLAH_PENDUDUK = sum(JUMLAH_PENDUDUK),
        KEPADATAN_PENDUDUK = round(JUMLAH_PENDUDUK/LUAS_WILAYAH,2),
        KRS = sum(KRS))
    
    if(length(filter_desa) > 1){
      idm = ""
    } else{
      idm = data_sumber_daya %>%
        filter(KECAMATAN %in% filter_kecamatan, KELURAHAN %in% filter_desa) %>%
        select(IDM)
      idm = idm$IDM
      if(idm == ""){
        idm = "NA"
      } else{
        idm = idm
      }
    }
    boxProfile(
      title = "",
      subtitle = "Profil Wilayah",
      bordered = TRUE,
      boxProfileItem(
        title = "Luas Wilayah:",    
        description = paste(scales::comma(df_sd_rekap$LUAS_WILAYAH, 
                                          big.mark = ".",
                                          decimal.mark = ",", 
                                          accuracy = 0.01),
                            "km²")
      ),
      boxProfileItem(
        title = "Jumlah Penduduk:",
        description = paste(scales::comma(df_sd_rekap$JUMLAH_PENDUDUK, 
                                          big.mark = ".",
                                          decimal.mark = ","))
      ),
      boxProfileItem(
        title = "Kepadatan Penduduk:",
        description = paste(scales::comma(df_sd_rekap$KEPADATAN_PENDUDUK, 
                                          big.mark = ".",
                                          decimal.mark = ",", 
                                          accuracy = 0.01))
      ),
      boxProfileItem(
        title = "IDM:",
        description = idm
      )
    )
  })
  
  output$grafik_piramida_rekap <- renderPlotly({
    req(input$cari_rekap)
    kecamatan <- value_filter_kec()
    desa_kel <- value_filter_desa_kel()
    kelompok_umur_lk <- kelompok_umur_lk %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)  %>%
      gather("Kelompok_Umur", "Jumlah", 4:20) %>%
      mutate(Jenis_Kelamin = rep("Laki-laki", 17))
    
    kelompok_umur_pr <- kelompok_umur_pr %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)  %>%
      gather("Kelompok_Umur", "Jumlah", 4:20) %>%
      mutate(Jenis_Kelamin = rep("Peremupuan", 17))
    
    #data_piramida <- rbind(kelompok_umur_lk, kelompok_umur_pr)
    
    ku <- c("0 - 1",	"2 - 4",	"5 - 9",	"10 - 14",	"15 - 19",
            "20 - 24",	"25 - 29",	"30 - 34",	"35 - 39",	"40 - 44",
            "45 - 49",	"50 - 54",	"55 - 59	", "60 - 64",
            "65 - 69",	"70 - 74",	"75+")
    
    # Membuat data untuk grafik piramida
    piramida_data <- data.frame(
      Kelompok_Umur = factor(rep(ku, times = 2), levels = ku),
      Jumlah = c(kelompok_umur_lk$Jumlah, -kelompok_umur_pr$Jumlah),
      Jenis_Kelamin = rep(c("Laki-Laki", "Perempuan"), each = length(kelompok_umur_lk$Kelompok_Umur))
    )
    
    
    #grafik piramida penduduk interaktif menggunakan plotly
    piramida_interaktif <- plot_ly(
      piramida_data,
      x = ~Jumlah,
      y = ~Kelompok_Umur,
      type = "bar",
      orientation = "h",
      color = ~Jenis_Kelamin,
      colors = c("#0d6efd", "#ffc107"),
      hoverinfo = "text",
      text = ~paste(Jenis_Kelamin, abs(Jumlah)),
      textposition = 'none',
      showlegend = FALSE  # Menghapus legend
    ) 
    
    # Fungsi untuk mengambil kelipatan 20 ke atas
    ambil_kelipatan_20_ke_atas <- function(angka) {
      kelipatan_20_terdekat <- ceiling(angka/50) * 50
      return(kelipatan_20_terdekat)
    }
    
    batas_angka <- ambil_kelipatan_20_ke_atas(max(piramida_data$Jumlah))
    
    
    
    # Menambahkan label dan judul
    piramida_interaktif <- piramida_interaktif %>%
      layout(title = "Grafik Piramida Penduduk",
             xaxis = list(title = "Jumlah Penduduk", tickangle=0,
                          tickvals = seq(-batas_angka, batas_angka+1, by = round(batas_angka/3)), 
                          ticktext = abs(seq(-batas_angka, batas_angka+1, by = round(batas_angka/3)))),
             yaxis = list(title = "Kelompok Usia", standoff = 100),
             barmode = "relative", 
             legend = list(orientation = 'h'),
             font = list(family = "Arial"),  # Mengatur jenis font global
             margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
             paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
             plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
             hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
             legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
             hovermode = "closest",  # Mengatur mode hover
             hoverdistance = 30,  # Mengatur jarak hover
             hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
             updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
      )
    
    # Menampilkan grafik interaktif
    piramida_interaktif %>%
      add_annotations(
        text = "Perempuan", x = -batas_angka, y = 15,
        showarrow = FALSE, font = list(color = "#000", size = 14), xref = "x", yref = "y"
      ) %>%
      add_annotations(
        text = "Laki-Laki", x = batas_angka, y = 15,
        showarrow = FALSE, font = list(color = "#000", size = 14), xref = "x", yref = "y"
      )
    
  })
  
  output$tabel_piramida_rekap <- renderReactable({
    req(input$cari_rekap)
    kecamatan <- value_filter_kec()
    desa_kel <- value_filter_desa_kel()
    kelompok_umur_lk <- kelompok_umur_lk %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)  %>%
      gather("Kelompok_Umur", "Jumlah", 4:20) %>%
      mutate(Jenis_Kelamin = rep("Laki-laki", 17))
    
    kelompok_umur_pr <- kelompok_umur_pr %>%
      filter(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)  %>%
      gather("Kelompok_Umur", "Jumlah", 4:20) %>%
      mutate(Jenis_Kelamin = rep("Peremupuan", 17))
    
    tabel_piramida <- left_join(kelompok_umur_lk, kelompok_umur_pr,
                                by = "Kelompok_Umur") %>%
      rename(Laki_Laki = Jumlah.x, Perempuan = Jumlah.y) %>%
      group_by(Kelompok_Umur) %>%
      mutate(Total = sum(Laki_Laki + Perempuan)) 
    
    tabel_piramida <- as_tibble(tabel_piramida)
    
    ku <- c("0 - 1",	"2 - 4",	"5 - 9",	"10 - 14",	"15 - 19",
            "20 - 24",	"25 - 29",	"30 - 34",	"35 - 39",	"40 - 44",
            "45 - 49",	"50 - 54",	"55 - 59	", "60 - 64",
            "65 - 69",	"70 - 74",	"75+")
    
    tabel_piramida$Kelompok_Umur <- factor(ku, levels = ku)
    
    tabel_piramida <- tabel_piramida %>%  
      select(4,5,10,12) %>%
      bind_rows(summarise(., across(where(is.numeric), sum),
                          across(where(is.factor), ~'Total')))
    colnames(tabel_piramida) <- c("Umur", "Laki-laki", "Perempuan", "Total")
    reactable(tabel_piramida,
              columns = list(
                `Laki-laki` = colDef(
                  format = colFormat(separators = TRUE, locales = "en-US")
                ),
                Perempuan = colDef(
                  format = colFormat(separators = TRUE, locales = "en-US")
                ),
                Total = colDef(
                  format = colFormat(separators = TRUE, locales = "en-US")
                )
              ),
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE,
              theme = reactableTheme(
                borderColor = "#dfe2e5",
                stripedColor = "#f6f8fa",
                highlightColor = "#f0f5f9",
                cellPadding = "8px 12px",
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                searchInputStyle = list(width = "100%"))
    )
  })
  
  #
  output$jumlah_keluarga <- renderText({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    jumlah_keluarga = data_krs %>%
      filter(
        KABUPATEN %in% filter_kabupaten,
        KECAMATAN %in% filter_kecamatan,
        KELURAHAN %in% filter_desa
      ) %>%
      select(`JUMLAH KELUARGA`)
    
    scales::comma(sum(jumlah_keluarga$`JUMLAH KELUARGA`), big.mark = ".", 'decimal.mark' = ",")
  })
  
  output$bar_kepemilikan_rumah <- renderPlotly({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    data_kepemilikan_rumah = data_krs %>%
      filter(
        KABUPATEN %in% filter_kabupaten,
        KECAMATAN %in% filter_kecamatan,
        KELURAHAN %in% filter_desa
      ) %>%
      select(24:28) %>%
      summarise_all(sum) %>%
      pivot_longer(cols = 1:5, names_to = "Jenis Kepemilikan", values_to = "Total") %>%
      mutate(Text = scales::comma(Total, big.mark = ".", 'decimal.mark' = ","))
    
    # Membuat grafik bar Plotly
    grafik_kepemilikan_rumah <- plot_ly(
      data_kepemilikan_rumah,
      x = ~Total,
      y = ~`Jenis Kepemilikan`,
      text = ~Text,
      type = "bar",
      textposition = 'outside',
      marker = list(color = "#0d6efd")  # Warna bar
    ) 
    
    # Menambahkan label dan judul
    grafik_kepemilikan_rumah <- grafik_kepemilikan_rumah %>%
      layout(
        title = "Kepemilikan Rumah Keluarga",
        yaxis = list(title = "",
                     categoryorder = "total ascending"),
        xaxis = list(title = "Jumlah Keluarga", 
                     range = c(0,     
                               max(data_kepemilikan_rumah$Total) + 
                                 (round(max(data_kepemilikan_rumah$Total) * 0.4, 0)))),
        font = list(family = "Arial"),  # Mengatur jenis font global
        margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
        paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
        plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
        hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
        legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
        hovermode = "closest",  # Mengatur mode hover
        hoverdistance = 30,  # Mengatur jarak hover
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
        updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
      )
    
    # Menampilkan grafik
    grafik_kepemilikan_rumah
  })
  
  output$bar_sumber_air <- renderPlotly({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- value_filter_bulan()
    
    data_sumber_air = data_krs %>%
      filter(
        KABUPATEN %in% filter_kabupaten,
        KECAMATAN %in% filter_kecamatan,
        KELURAHAN %in% filter_desa
      ) %>%
      select(7:14) %>%
      summarise_all(sum) %>%
      pivot_longer(cols = 1:8, names_to = "Sumber Air", values_to = "Total") %>%
      mutate(Text = scales::comma(Total, big.mark = ".", 'decimal.mark' = ","))
    
    # Membuat grafik bar Plotly
    grafik_sumber_air <- plot_ly(
      data_sumber_air,
      x = ~Total,
      y = ~`Sumber Air`,
      text = ~Text,
      type = "bar",
      textposition = 'outside',
      marker = list(color = "#0d6efd")  # Warna bar
    ) 
    
    # Menambahkan label dan judul
    grafik_sumber_air <- grafik_sumber_air %>%
      layout(
        title = "Sumber Air Keluarga",
        yaxis = list(title = "",
                     categoryorder = "total ascending"),
        xaxis = list(title = "Jumlah Keluarga",
                     range = c(0,     
                               max(data_sumber_air$Total) + 
                                 (round(max(data_sumber_air$Total) * 0.4, 0)))),
        font = list(family = "Arial"),  # Mengatur jenis font global
        margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
        paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
        plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
        hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
        legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
        hovermode = "closest",  # Mengatur mode hover
        hoverdistance = 30,  # Mengatur jarak hover
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
        updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
      )
    
    # Menampilkan grafik
    grafik_sumber_air
  })
  
  output$bar_bab <- renderPlotly({
    withProgress(message = "Membuat Grafik...", value = 0, {
      req(input$cari_rekap)
      filter_kabupaten <- value_filter_kab()
      filter_kecamatan <- value_filter_kec()
      filter_desa <- value_filter_desa_kel()
      filter_bulan <- value_filter_bulan()
      
      incProgress(1/10, detail = "Mengambil Wilayah")
      
      data_bab = data_krs %>%
        filter(
          KABUPATEN %in% filter_kabupaten,
          KECAMATAN %in% filter_kecamatan,
          KELURAHAN %in% filter_desa
        ) %>%
        select(15:17) %>%
        summarise_all(sum) %>%
        pivot_longer(cols = 1:3, names_to = "Fasilitas BAB", values_to = "Total") %>%
        mutate(Text = scales::comma(Total, big.mark = ".", 'decimal.mark' = ","))
      
      incProgress(2/10, detail = "Filter Wilayah")
      
      # Membuat grafik bar Plotly
      grafik_bab<- plot_ly(
        data_bab,
        x = ~Total,
        y = ~`Fasilitas BAB`,
        text = ~Text,
        type = "bar",
        textposition = 'outside',
        marker = list(color = "#0d6efd")  # Warna bar
      ) 
      
      incProgress(3/10, detail = "Grafik Dasar")
      # Menambahkan label dan judul
      grafik_bab <- grafik_bab %>%
        layout(
          title = "Fasilitas Tempat Buang Air Besar",
          yaxis = list(title = "",
                       categoryorder = "total ascending"),
          xaxis = list(title = "Jumlah Keluarga",
                       range = c(0,     
                                 max(data_bab$Total) + 
                                   (round(max(data_bab$Total) * 0.4, 0)))),
          font = list(family = "Arial"),  # Mengatur jenis font global
          margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
          paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
          plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
          hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
          legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
          hovermode = "closest",  # Mengatur mode hover
          hoverdistance = 30,  # Mengatur jarak hover
          hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
          updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
        )
      incProgress(4/10, detail = "Aesthetic Grafik")
      
      for(i in 5:9){
        incProgress(i/10, "Sedikit lagi")
        Sys.sleep(0.25)
      }
      
      # Menampilkan grafik
      incProgress(10/10, detail = "Selesai")
      grafik_bab
    })
  })
  ##batas profil
  
  ## KB
  output$vb_unmet_need_rekap <- renderUI({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- value_filter_bulan()
    
    unmet_need_bulan = data_pus %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        Kelurahan_Desa %in% filter_desa,
        Bulan %in% filter_bulan
      )
    
    unmet_need_bulan <- round(sum(unmet_need_bulan$`Unmet Need`) / sum(unmet_need_bulan$PUS) *100 , 2)
    
    unmet_need_bulan_sebelum = data_pus %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        Kelurahan_Desa %in% filter_desa,
        Bulan %in% pilih_bulan_sebelumnya(filter_bulan)
      )
    
    unmet_need_bulan_sebelum <- round(sum(unmet_need_bulan_sebelum$`Unmet Need`) / sum(unmet_need_bulan_sebelum$PUS) *100 , 2)
    
    perbandingan <- cek_naik_turun(unmet_need_bulan,
                                   unmet_need_bulan_sebelum)
    
    icon <- bandingkan_bulan_rekap(unmet_need_bulan,
                                   unmet_need_bulan_sebelum)
    
    text_un = paste0(perbandingan, " dari capaian ", 
                     str_to_title(pilih_bulan_sebelumnya(filter_bulan)), " (", 
                     scales::comma(unmet_need_bulan_sebelum, big.mark = ".", 
                                   'decimal.mark' = ","), ")")
    
    value_box(
      h1(span(
        strong("Unmet Need"), 
        bsicons::bs_icon(icon),
        style = "font-size:20px;")),
      unmet_need_bulan,
      span(
        text_un,
        style = "font-size:12px;"
      ),
      showcase = bs_icon("clipboard"),
      theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E")
    )
  })
  
  output$sdm_vb_1_rekap <- renderText({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- value_filter_bulan()
    
    sdm_cek <- sdm_kb %>%
      filter(
        Kecamatan %in% filter_kecamatan, 
        `Kelurahan/Desa` %in% filter_desa, 
        BULAN %in% filter_bulan
      )
    
    if(nrow(sdm_cek) <= 0){
      sdm_cek = 0
    } else{
      sdm_cek <- nrow(sdm_cek)
    }
    scales::comma(sdm_cek, big.mark = ".",
                  'decimal.mark' = ",") 
    
    
  })
  
  output$vb_tp_kb_rekap <- renderText({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- value_filter_bulan()
    
    tp_kb_cek <- sdm_kb %>%
      filter(
        Kecamatan %in% filter_kecamatan, 
        `Kelurahan/Desa` %in% filter_desa, 
        BULAN %in% filter_bulan
      )
    
    if(nrow(tp_kb_cek) <= 0){
      tp_kb_cek = 0
    } else{
      tp_kb_cek <- length(unique(tp_kb_cek$`No. Registrasi`))
    }
    
    tp_kb_cek 
  })
  
  output$vb_mkjp_rekap <- renderUI({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- value_filter_bulan()
    
    mkjp_bulan = data_mix_kontra %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa,
        Bulan %in% filter_bulan
      ) %>% 
      summarise(
        mkjp = Implan + IUD + Vasektomi + Tubektomi
      )
    
    mkjp_bulan <- sum(mkjp_bulan$mkjp)
    
    mkjp_bulan_sebelum = data_mix_kontra %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa,
        Bulan %in% pilih_bulan_sebelumnya(filter_bulan)
      )%>% 
      summarise(
        mkjp = Implan + IUD + Vasektomi + Tubektomi
      )
    
    mkjp_bulan_sebelum <- sum(mkjp_bulan_sebelum$mkjp)
    
    perbandingan <- cek_naik_turun(mkjp_bulan,
                                   mkjp_bulan_sebelum)
    
    icon <- bandingkan_bulan_rekap(mkjp_bulan,
                                   mkjp_bulan_sebelum)
    
    text_un = paste0(perbandingan, " dari capaian ", 
                     str_to_title(pilih_bulan_sebelumnya(filter_bulan)), 
                     " (", scales::comma(mkjp_bulan_sebelum, big.mark = ".",
                                         'decimal.mark' = ","), ")")
    
    value_box(
      h1(span(
        strong("MKJP"), 
        bsicons::bs_icon(icon),
        style = "font-size:20px;")),
      scales::comma(mkjp_bulan, big.mark = ".",
                    'decimal.mark' = ","),
      span(
        text_un,
        style = "font-size:12px;"
      ),
      showcase = bs_icon("clipboard"),
      theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E")
    )
  })
  
  output$jumlah_pus <- renderText({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- value_filter_bulan()
    
    jumlah_pus = data_pus %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        Kelurahan_Desa %in% filter_desa,
        Bulan %in% filter_bulan
      )
    
    scales::comma(sum(jumlah_pus$PUS), big.mark = ".",
                  'decimal.mark' = ",")
    
  })
  
  output$line_pus <- renderPlotly({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- pilih_sampai_bulan(value_filter_bulan())
    
    jumlah_pus_plot = data_pus %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        Kelurahan_Desa %in% filter_desa,
        Bulan %in% filter_bulan
      ) %>%
      group_by(Bulan) %>%
      summarise(PUS = sum(PUS))
    
    # Konversi kolom bulan menjadi faktor dengan urutan JANUARI - DESEMBER
    jumlah_pus_plot$Bulan <- factor(jumlah_pus_plot$Bulan, levels = c(
      "JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI",
      "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"
    ))
    
    # Mengurutkan dataframe berdasarkan urutan bulan
    jumlah_pus_plot <- jumlah_pus_plot[order(jumlah_pus_plot$Bulan), ]
    
    # Membuat plot garis dengan 
    
    plot_ly(jumlah_pus_plot, x = ~Bulan, y = ~PUS, 
            type = 'scatter', mode = 'lines+markers') %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
      el.closest('.bslib-value-box')
        .addEventListener('bslib.card', function(ev) {
          Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
        })
    }"
      )
  })
  
  output$jumlah_mcpr <- renderText({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- value_filter_bulan()
    
    jumlah_pus = data_pus %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        Kelurahan_Desa %in% filter_desa,
        Bulan %in% filter_bulan
      )
    
    jumlah_mcpr = data_mix_kontra %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa,
        Bulan %in% filter_bulan
      )
    
    paste0(round(sum(jumlah_mcpr$`KB Modern`)/sum(jumlah_pus$PUS) * 100, 2), "%")
    
  })
  
  output$line_mcpr <- renderPlotly({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- pilih_sampai_bulan(value_filter_bulan())
    
    jumlah_pus_plot = data_pus %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        Kelurahan_Desa %in% filter_desa,
        Bulan %in% filter_bulan
      ) %>%
      group_by(Bulan) %>%
      summarise(PUS = sum(PUS))
    
    jumlah_mcpr_plot = data_mix_kontra %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa,
        Bulan %in% filter_bulan
      ) %>%
      group_by(Bulan) %>%
      summarise(MCPR = sum(`KB Modern`))
    
    jumlah_mcpr_plot <- inner_join(jumlah_mcpr_plot, jumlah_pus_plot, by = "Bulan") 
    jumlah_mcpr_plot <- jumlah_mcpr_plot %>%
      group_by(Bulan) %>%
      summarise(MCPR = round(MCPR/PUS * 100, 2))
    
    # Konversi kolom bulan menjadi faktor dengan urutan JANUARI - DESEMBER
    jumlah_mcpr_plot$Bulan <- factor(jumlah_mcpr_plot$Bulan, levels = c(
      "JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI",
      "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"
    ))
    
    # Mengurutkan dataframe berdasarkan urutan bulan
    jumlah_mcpr_plot <- jumlah_mcpr_plot[order(jumlah_mcpr_plot$Bulan), ]
    
    # Membuat plot garis dengan 
    
    plot_ly(jumlah_mcpr_plot, x = ~Bulan, y = ~MCPR, 
            type = 'scatter', mode = 'lines+markers') %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
      el.closest('.bslib-value-box')
        .addEventListener('bslib.card', function(ev) {
          Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
        })
    }"
      )
  })
  
  output$bar_mix_kontra <- renderPlotly({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- value_filter_bulan()
    
    jumlah_mix_kontra = data_mix_kontra %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa,
        Bulan %in% filter_bulan
      ) %>%
      select(7:14) %>%
      summarise_all(sum) %>%
      pivot_longer(cols = 1:8, names_to = "Metode", values_to = "Total") %>%
      mutate(Text = scales::comma(Total, big.mark = ".",
                                  'decimal.mark' = ","))
    
    # Membuat grafik bar Plotly
    grafik_kontrasepsi <- plot_ly(
      jumlah_mix_kontra,
      x = ~Total,
      y = ~Metode,
      text = ~Text,
      type = "bar",
      textposition = 'auto',
      marker = list(color = "#0d6efd")  # Warna bar
    ) 
    
    # Menambahkan label dan judul
    grafik_kontrasepsi <- grafik_kontrasepsi %>%
      layout(
        title = "Penggunaan Metode Kontrasepsi",
        yaxis = list(title = "Metode Kontrasepsi",
                     categoryorder = "total ascending"),
        xaxis = list(title = "Jumlah Penggunaan"),
        font = list(family = "Arial"),  # Mengatur jenis font global
        margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
        paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
        plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
        hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
        legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
        hovermode = "closest",  # Mengatur mode hover
        hoverdistance = 30,  # Mengatur jarak hover
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
        updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
      )
    
    # Menampilkan grafik
    grafik_kontrasepsi
    
  })
  
  output$line_pa <- renderPlotly({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- pilih_sampai_bulan(value_filter_bulan())
    
    jumlah_pa = data_mix_kontra %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa,
        Bulan %in% filter_bulan
      ) %>%
      group_by(Bulan) %>%
      summarise(PA = sum(PA))
    
    # Konversi kolom bulan menjadi faktor dengan urutan JANUARI - DESEMBER
    jumlah_pa$Bulan <- factor(jumlah_pa$Bulan, levels = c(
      "JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI",
      "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"
    ))
    
    # Mengurutkan dataframe berdasarkan urutan bulan
    jumlah_pa <- jumlah_pa[order(jumlah_pa$Bulan), ]
    
    # Membuat plot garis dengan 
    
    plot_ly(jumlah_pa, x = ~Bulan, y = ~PA, 
            type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = "Tren Peserta KB Aktif",
        yaxis = list(title = "Jumlah",
                     categoryorder = "total ascending"),
        xaxis = list(title = "Bulan"),
        font = list(family = "Arial"),  # Mengatur jenis font global
        margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
        paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
        plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
        hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
        legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
        hovermode = "closest",  # Mengatur mode hover
        hoverdistance = 30,  # Mengatur jarak hover
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
        updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
      )
    
  })
  
  output$donut_tenaga_terlatih <- renderPlotly({
    withProgress(message = "Membuat Grafik", detail = "...", value = 0, {
      #filter(KECAMATAN == "TOBADAK", KELURAHAN == "MAHAHE")
      req(input$cari_rekap)
      filter_kabupaten <- value_filter_kab()
      filter_kecamatan <- value_filter_kec()
      filter_desa <- value_filter_desa_kel()
      filter_bulan <- value_filter_bulan()
      # Data contoh
      incProgress(1/10, detail = "Mengambil Wilayah")
      
      donut_faskes = sdm_kb %>%
        filter(
          Kabupaten %in% filter_kabupaten,
          Kecamatan %in% filter_kecamatan,
          `Kelurahan/Desa` %in% filter_desa,
          BULAN %in% filter_bulan
        ) 
      incProgress(2/10, detail = "Filter Wilayah")
      
      terlatih_tidak <- sdm_kb %>%
        filter(
          Kabupaten %in% filter_kabupaten,
          Kecamatan %in% filter_kecamatan,
          `Kelurahan/Desa` %in% filter_desa,
          BULAN %in% filter_bulan
        ) %>%
        select(Pelatihan) %>%
        mutate(status = if_else(grepl("IUD|Tubektomi|Vasektomi", Pelatihan), "Sudah Terlatih", "Belum Terlatih"))
      incProgress(3/10, detail = "Merekap Data")
      
      terlatih_tidak <- table(terlatih_tidak$status)
      
      terlatih_tidak <- as.data.frame(terlatih_tidak)
      
      # Memberi nama kolom
      names(terlatih_tidak) <- c("Kategori", "Jumlah")
      
      plot_terlatih_tidak <- terlatih_tidak %>% 
        plot_ly(labels = ~Kategori, 
                values = ~Jumlah, 
                textinfo='label+percent+value',
                marker = list(colors = c("#ffc107", "#0d6efd"),
                              line = list(color = '#000', width = 1))
        )%>% 
        add_pie(hole = 0.4)%>% 
        layout(title = "Kompetensi Tenaga KB",  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                            showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                            showticklabels = FALSE),
               font = list(family = "Arial"),  # Mengatur jenis font global
               margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
               paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
               plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
               hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
               legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
               hovermode = "closest",  # Mengatur mode hover
               hoverdistance = 30,  # Mengatur jarak hover
               hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
               updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
        )
      
      incProgress(4/10, detail = "Aesthetic Grafik")
      
      for(i in 5:9){
        incProgress(i/10, "Sedikit lagi")
        Sys.sleep(0.25)
      }
      
      # Menampilkan grafik
      incProgress(10/10, detail = "Selesai")
      
      plot_terlatih_tidak
    })
  })
  
  ## batas KB
  
  ## monitoring krs
  output$profil_stunting_rekap <- renderUI({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    data_profil_stunting <- faktor_krs %>%
      select(c(1:3, 5:7, 13)) %>%
      filter(KECAMATAN %in% filter_kecamatan, KELURAHAN %in% filter_desa) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)
    
    boxProfile(
      title = "Rekapitulasi KRS",
      subtitle = "Semester 2 - 2023",
      bordered = F,
      boxProfileItem(
        title = "Jumlah Keluarga:",    
        description =format(data_profil_stunting$`JUMLAH KELUARGA`, big.mark = ".", decimal.mark = ",")
      ),
      boxProfileItem(
        title = "Jumlah Sasaran:",
        description = format(data_profil_stunting$`JUMLAH KELUARGA SASARAN`, big.mark = ".", decimal.mark = ",")
      ),
      boxProfileItem(
        title = "Jumlah KRS:",
        description = format(data_profil_stunting$TOTAL, big.mark = ".", decimal.mark = ",")
      )#,      
      # boxProfileItem(
      #   title = "Penerima BAAS:",
      #   description = format(data_profil_stunting$PENERIMA_BAAS, big.mark = ".", decimal.mark = ",")
      # ),      
      # boxProfileItem(
      #   title = "Bantuan Pangan:",
      #   description = format(data_profil_stunting$PENERIMA_BANTUAN_PANGAN, big.mark = ".", decimal.mark = ",")
      # ),      
      # boxProfileItem(
      #   title = "Jumlah Stunting:",
      #   description = format(data_profil_stunting$JUMLAH_BALITA_STUNTING, big.mark = ".", decimal.mark = ",")
      # )
    )
  })
  
  output$bar_faktor_resiko_jumlah <- renderPlotly({
    req(input$cari_rekap)
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    data_faktor_resiko <- faktor_krs %>%
      filter(KECAMATAN %in% filter_kecamatan, KELURAHAN %in% filter_desa) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)
    
    
    data_faktor_resiko <- data_faktor_resiko %>%
      mutate(TUA = sum(`TERLALU TUA (UMUR ISTRI 35-40 TAHUN)`),
             MUDA = sum(`TERLALU MUDA (UMUR ISTRI < 20 TAHUN)`),
             BANYAK = sum(`TERLALU BANYAK (≥ 3 ANAK)`),
             DEKAT = sum(`TERLALU DEKAT (< 2 TAHUN)`),
             SANITASI = sum(`KELUARGA TIDAK MEMPUNYAI JAMBAN YANG LAYAK`),
             AIR_MINUM = sum(`KELUARGA TIDAK MEMPUNYAI SUMBER AIR MINUM UTAMA YANG LAYAK`),
             KB_MODERN = sum(`BUKAN PESERTA KB MODERN`)
      ) %>% select(TUA, MUDA, BANYAK, DEKAT, 
                   SANITASI, AIR_MINUM, KB_MODERN)
    
    
    
    colnames(data_faktor_resiko) <- c("TERLALU TUA",
                                      "TERLALU MUDA", "TERLALU BANYAK", "TERLALU DEKAT",
                                      "JAMBAN", "SUMBER AIR", "KB MODERN")
    
    data_faktor_resiko <- 
      data_faktor_resiko %>%
      gather(
        "FAKTOR", "JUMLAH", 1:7
      )
    
    # Mengatur urutan kolom
    data_faktor_resiko$FAKTOR <- factor(data_faktor_resiko$FAKTOR, 
                                        levels = c("JAMBAN", "SUMBER AIR", "KB MODERN", "TERLALU TUA", 
                                                   "TERLALU MUDA", "TERLALU BANYAK", "TERLALU DEKAT"))
    data_faktor_resiko <- data_faktor_resiko %>%
      mutate(TEKS = scales::comma(JUMLAH, 
                                  big.mark = ".",
                                  decimal.mark = ","))
    
    # Membuat grafik bar Plotly
    baru_faktor_resiko <- plot_ly(
      data_faktor_resiko,
      x = ~JUMLAH,
      y = ~FAKTOR,
      text = ~TEKS,
      insidetextfont = list(color = '#FFFFFF'),
      type = "bar",
      orientation = 'h',
      marker = list(color = "#0d6efd")  # Warna bar
    ) 
    
    # Menambahkan label dan judul
    baru_faktor_resiko <- baru_faktor_resiko %>%
      layout(
        title = "",
        xaxis = list(title = "JUMLAH"),
        yaxis = list(title = ""),
        font = list(family = "Arial"),  # Mengatur jenis font global
        margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
        paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
        plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
        hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
        legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
        hovermode = "closest",  # Mengatur mode hover
        hoverdistance = 30,  # Mengatur jarak hover
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
        updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
      )
    baru_faktor_resiko %>%
      layout(
        bargap = 0.4
      ) %>%
      config(displayModeBar = FALSE) %>%
      config(displaylogo = FALSE) %>%
      config(scrollZoom = FALSE) %>%
      config(edits = list(editType = "plotly"))
    
  })
  
  output$bar_faktor_resiko_persen <- renderPlotly({
    req(input$cari_rekap)
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    data_faktor_resiko <- faktor_krs %>%
      filter(KECAMATAN %in% filter_kecamatan, KELURAHAN %in% filter_desa) %>%
      group_by(PROVINSI) %>%
      summarise_if(is.numeric, sum)
    
    
    data_faktor_resiko <- data_faktor_resiko %>%
      mutate(PERSEN_TUA = round(`TERLALU TUA (UMUR ISTRI 35-40 TAHUN)`/TOTAL * 100, 2),
             PERSEN_MUDA = round(`TERLALU MUDA (UMUR ISTRI < 20 TAHUN)`/TOTAL  * 100, 2),
             PERSEN_BANYAK = round(`TERLALU BANYAK (≥ 3 ANAK)`/TOTAL * 100, 2),
             PERSEN_DEKAT = round(`TERLALU DEKAT (< 2 TAHUN)`/TOTAL * 100, 2),
             PERSEN_SANITASI = round(`KELUARGA TIDAK MEMPUNYAI JAMBAN YANG LAYAK`/TOTAL  * 100, 2),
             PERSEN_AIR_MINUM = round(`KELUARGA TIDAK MEMPUNYAI SUMBER AIR MINUM UTAMA YANG LAYAK`/TOTAL * 100, 2),
             PERSEN_KB_MODERN = round(`BUKAN PESERTA KB MODERN`/TOTAL * 100, 2)
      ) %>% select(PERSEN_TUA, PERSEN_MUDA, PERSEN_BANYAK, PERSEN_DEKAT, 
                   PERSEN_SANITASI, PERSEN_AIR_MINUM, PERSEN_KB_MODERN)
    
    
    
    colnames(data_faktor_resiko) <- c("TERLALU TUA",
                                      "TERLALU MUDA", "TERLALU BANYAK", "TERLALU DEKAT",
                                      "JAMBAN", "SUMBER AIR", "KB MODERN")
    
    data_faktor_resiko <- 
      data_faktor_resiko %>%
      gather(
        "FAKTOR", "PERSENTASE", 1:7
      )
    
    # Mengatur urutan kolom
    data_faktor_resiko$FAKTOR <- factor(data_faktor_resiko$FAKTOR, 
                                        levels = c("JAMBAN", "SUMBER AIR", "KB MODERN", "TERLALU TUA", 
                                                   "TERLALU MUDA", "TERLALU BANYAK", "TERLALU DEKAT"))
    
    # Membuat grafik bar Plotly
    baru_faktor_resiko <- plot_ly(
      data_faktor_resiko,
      x = ~PERSENTASE,
      y = ~FAKTOR,
      text = ~paste(PERSENTASE, "%"),
      insidetextfont = list(color = '#FFFFFF'),
      type = "bar",
      orientation = 'h',
      marker = list(color = "#0d6efd")  # Warna bar
    ) 
    
    # Menambahkan label dan judul
    baru_faktor_resiko <- baru_faktor_resiko %>%
      layout(
        title = "",
        xaxis = list(title = "PERSENTASE"),
        yaxis = list(title = ""),
        font = list(family = "Arial"),  # Mengatur jenis font global
        margin = list(l = 50, r = 50, b = 50, t = 50),  # Mengatur margin plot
        paper_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang kanvas
        plot_bgcolor = "#f6f8fa",  # Mengatur warna latar belakang plot
        hoverlabel = list(font = list(family = "Arial")),  # Mengatur jenis font untuk label hover
        legend = list(font = list(family = "Arial")),  # Mengatur jenis font untuk legenda
        hovermode = "closest",  # Mengatur mode hover
        hoverdistance = 30,  # Mengatur jarak hover
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial")),  # Mengatur warna latar belakang dan jenis font untuk label hover
        updatemenus = list(font = list(family = "Arial"))  # Mengatur jenis font untuk menu pembaruan
      )
    baru_faktor_resiko %>%
      layout(
        bargap = 0.4
      ) %>%
      config(displayModeBar = FALSE) %>%
      config(displaylogo = FALSE) %>%
      config(scrollZoom = FALSE) %>%
      config(edits = list(editType = "plotly"))
    
  })
  
  output$jumlah_posyandu <- renderUI({
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    total_posyandu <- keberadaan_posyandu %>%
      filter(KECAMATAN %in% filter_kecamatan, KELURAHAN %in% filter_desa) %>%
      group_by(PROVINSI) %>%
      count()
    
    value_box(
      title = "Total Posyandu ",
      value = scales::comma(total_posyandu$n,
                            big.mark = ".",
                            decimal.mark = ","),
      showcase = bs_icon("file-medical-fill"),
      theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
    )
  })
  
  output$jumlah_tpk_rekap <- renderUI({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    filter_bulan <- value_filter_bulan()
    
    nama_pkb <- nama_pkb %>%
      filter(Kecamatan %in% filter_kecamatan, Kelurahan %in% filter_desa)
    
    nama_tpk <- nama_tpk %>%
      filter(Kecamatan %in% filter_kecamatan, Kelurahan %in% filter_desa)
    
    if(length(filter_desa) > 1){
      nama_pkb <- nama_pkb %>%
        summarise(n=n_distinct(paste(Kecamatan, `Nama PKB`)))
      
      nama_tpk <- nama_tpk %>%
        summarise(n=n_distinct(Register))
      
      vbs <- list(
        value_box(
          title = "Jumlah PKB",
          value = nama_pkb$n,
          showcase = bs_icon("person-x"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("PKB (Penyuluh Keluarga Berencana)")
        ),
        value_box(
          title = "Jumlah TPK",
          value = nama_tpk$n,
          showcase = bs_icon("person"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("TPK (Tim Pendamping Keluarga)")
        )
      )
      
      layout_column_wrap(
        width = "250px",
        !!!vbs
      )
    } else {
      jumlah_tpk <- nama_tpk %>%
        summarise(n=n_distinct(Register))
      
      nama_kader_pkk <- nama_tpk %>%
        filter(status == "PKK")
      nama_kader_pkk <- paste(nama_kader_pkk$`Nama Anggota`[1:nrow(nama_kader_pkk)], collapse = " & ")
      
      nama_bidan <- nama_tpk %>%
        filter(status == "Bidan")
      nama_bidan <- paste(nama_bidan$`Nama Anggota`[1:nrow(nama_bidan)], collapse = " & ") 
      
      nama_kader_kb <- nama_tpk %>%
        filter(status == "Kader KB")
      nama_kader_kb <- paste(nama_kader_kb$`Nama Anggota`[1:nrow(nama_kader_kb)], collapse = " & ") 
      
      vbs <- list(
        value_box(
          title = "Nama PKB",
          value =   h1(span(
            strong(nama_pkb$`Nama PKB`),
            style = "font-size:20px;")),
          showcase = bs_icon("person-x"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("Penyuluh Kelurga Berencana")
        ),
        value_box(
          title = "Jumlah TPK",
          value = h1(span(
            strong(jumlah_tpk$n),
            style = "font-size:20px;")),
          showcase = bs_icon("person"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("Tim Pendamping Keluarga")
        ),
        value_box(
          title = "Nama PKK",
          value = h1(span(
            strong(nama_kader_pkk),
            style = "font-size:20px;")),
          showcase = bs_icon("person"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("Pemberdayaan Kes. Keluarga")
        ),
        value_box(
          title = "Nama Bidan",
          value = h1(span(
            strong(nama_bidan),
            style = "font-size:20px;")),
          showcase = bs_icon("person"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("Tenaga Kesehatan")
        ),
        value_box(
          title = "Nama Kader KB",
          value = h1(span(
            strong(nama_kader_kb),
            style = "font-size:20px;")),
          showcase = bs_icon("person"),
          theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
          p("Kader Keluarga Berencana")
        )
      )
      card(
        layout_columns(fillable = T, 
                       vbs[[1]], vbs[[2]]
        ),
        layout_columns(fillable = T, 
                       vbs[[3]], vbs[[4]], vbs[[5]]
        )
      )
    }
  })
  
  output$pie_hamil_verval <- renderPlotly({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    data_pus_hamil_verval = verval_krs %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa
      ) %>%
      select(10:11)
    
    jumlah_pus_hamil <- sum(data_pus_hamil_verval$`PUS Hamil Ada`) + sum(data_pus_hamil_verval$`PUS Hamil Baru`)
    pus_hamil_ada = sum(data_pus_hamil_verval$`PUS Hamil Ada`)
    pus_hamil_baru = sum(data_pus_hamil_verval$`PUS Hamil Baru`)
    
    data_pie_pus_hamil <-data.frame(
      Kategori = c("Hamil Ada", "Hamil Baru"),
      Nilai = c(pus_hamil_ada, 
                pus_hamil_baru)
    )
    
    # Tentukan teks untuk setiap sektor
    #teks <- paste(data_pie$Kategori, data_pie$Nilai, sep = ": ")
    
    persentase_terdampingi <- paste0(ubah_angka(round(data_pie_pus_hamil$Nilai[2] / sum(data_pie_pus_hamil$Nilai) * 100, 1)), "%")
    
    # Tentukan warna untuk setiap kategori
    warna <- c("purple", "#404040")
    
    # Buat plot donat menggunakan plot_ly dengan argumen text untuk menampilkan teks
    plot_ly(data_pie_pus_hamil, labels = ~Kategori, values = ~Nilai, 
            type = "pie", hole = 0.6, sort = FALSE,
            textinfo = "none",
            texttemplate = "",
            hoverinfo = "none",
            hovertext = paste0(data_pie_pus_hamil$Kategori, ": ", data_pie_pus_hamil$Nilai),
            marker = list(colors = warna)) %>%
      add_annotations(text = persentase_terdampingi,
                      x = 0.5, y = 0.5, 
                      font = list(color = "#404040", size = 30),
                      showarrow = FALSE) %>%
      layout(title = "", 
             xaxis = list(categoryorder = "array", categoryarray = c("Terdampingi", "Tidak Terdampingi")),
             showlegend = F,
             annotations = list(
               text = paste('Jumlah Sasaran:', scales::comma(jumlah_pus_hamil, 
                                                             big.mark = ".",
                                                             decimal.mark = ",")),
               x = 0.5,
               y = -0.2,
               xref = 'paper',
               yref = 'paper',
               showarrow = FALSE,
               font = list(size = 14)
             ))
  })
  
  output$legend_hamil_verval <- renderUI({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    data_pus_hamil_verval = verval_krs %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa
      ) %>%
      select(10:11)
    
    jumlah_pus_hamil <- sum(data_pus_hamil_verval$`PUS Hamil Ada`) + sum(data_pus_hamil_verval$`PUS Hamil Baru`)
    pus_hamil_ada = sum(data_pus_hamil_verval$`PUS Hamil Ada`)
    pus_hamil_baru = sum(data_pus_hamil_verval$`PUS Hamil Baru`)
    
    span(
      h6(
        span(
          bsicons::bs_icon("people"),
          style = "font-size:20px;color:purple"),
        span(strong("Ada:", scales::comma(pus_hamil_ada,
                                          big.mark = ".",
                                          decimal.mark = ",")), 
             style = "font-size:12px;color:purple"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        span(
          bsicons::bs_icon("people"),
          style = "font-size:20px;color:#404040"),
        span(strong("Baru:", scales::comma(pus_hamil_baru,
                                           big.mark = ".",
                                           decimal.mark = ",")), 
             style = "font-size:12px;color:#404040"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        style="text-align: center;"
      )
    )
  })
  
  output$pie_catin_verval <- renderPlotly({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    data_catin_verval = verval_krs %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa
      ) %>%
      select(23:24)
    
    jumlah_kel <- sum(data_catin_verval$`Kel Total`)
    jumlah_kel_catin = sum(data_catin_verval$`Kel Catin`)
    jumlah_kel_non_catin = sum(data_catin_verval$`Kel Total`) - jumlah_kel_catin
    
    data_pie_catin <-data.frame(
      Kategori = c("Bukan Catin", "Catin"),
      Nilai = c(jumlah_kel_non_catin, 
                jumlah_kel_catin)
    )
    
    # Tentukan teks untuk setiap sektor
    #teks <- paste(data_pie$Kategori, data_pie$Nilai, sep = ": ")
    
    persentase_terdampingi <- paste0(ubah_angka(round(data_pie_catin$Nilai[2] / sum(data_pie_catin$Nilai) * 100, 1)), "%")
    
    # Tentukan warna untuk setiap kategori
    warna <- c("#0d6efd", "#404040")
    
    # Buat plot donat menggunakan plot_ly dengan argumen text untuk menampilkan teks
    plot_ly(data_pie_catin, labels = ~Kategori, values = ~Nilai, 
            type = "pie", hole = 0.6, sort = FALSE,
            textinfo = "none",
            texttemplate = "",
            hoverinfo = "none",
            hovertext = paste0(data_pie_catin$Kategori, ": ", data_pie_catin$Nilai),
            marker = list(colors = warna)) %>%
      add_annotations(text = persentase_terdampingi,
                      x = 0.5, y = 0.5, 
                      font = list(color = "#404040", size = 30),
                      showarrow = FALSE) %>%
      layout(title = "", 
             xaxis = list(categoryorder = "array", categoryarray = c("Terdampingi", "Tidak Terdampingi")),
             showlegend = F,
             annotations = list(
               text = paste('Jumlah Keluarga:', scales::comma(jumlah_kel, 
                                                              big.mark = ".",
                                                              decimal.mark = ",")),
               x = 0.5,
               y = -0.2,
               xref = 'paper',
               yref = 'paper',
               showarrow = FALSE,
               font = list(size = 14)
             ))
  })
  
  output$legend_catin_verval <- renderUI({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    warna <- c("#0d6efd", "#404040")
    data_catin_verval = verval_krs %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa
      ) %>%
      select(23:24)
    
    jumlah_kel <- sum(data_catin_verval$`Kel Total`)
    jumlah_kel_catin = sum(data_catin_verval$`Kel Catin`)
    jumlah_kel_non_catin = sum(data_catin_verval$`Kel Total`) - jumlah_kel_catin
    
    
    span(
      h6(
        span(
          bsicons::bs_icon("people"),
          style = "font-size:20px;color:#0d6efd"),
        span(strong("Bukan Catin:", scales::comma(jumlah_kel_non_catin,
                                                  big.mark = ".",
                                                  decimal.mark = ",")), 
             style = "font-size:12px;color:#0d6efd"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        style="text-align: center;",
        span(
          bsicons::bs_icon("people"),
          style = "font-size:20px;color:404040"),
        span(strong("Keluarga Catin:", scales::comma(jumlah_kel_catin,
                                                     big.mark = ".",
                                                     decimal.mark = ",")), 
             style = "font-size:12px;color:#404040"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
      )
    )
  })
  
  output$pie_baduta_verval <- renderPlotly({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    data_baduta_verval = verval_krs %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa
      ) %>%
      select(12:14)
    
    jumlah_baduta <- sum(data_baduta_verval$`Kel Baduta Ada`) + sum(data_baduta_verval$`Kel Baduta Baru`)
    baduta_ada = sum(data_baduta_verval$`Kel Baduta Ada`)
    baduta_baru = sum(data_baduta_verval$`Kel Baduta Baru`)
    
    data_pie_baduta <-data.frame(
      Kategori = c("Hamil Ada", "Hamil Baru"),
      Nilai = c(baduta_ada, 
                baduta_baru)
    )
    
    # Tentukan teks untuk setiap sektor
    #teks <- paste(data_pie$Kategori, data_pie$Nilai, sep = ": ")
    
    persentase_terdampingi <- paste0(ubah_angka(round(data_pie_baduta$Nilai[2] / sum(data_pie_baduta$Nilai) * 100, 1)), "%")
    
    # Tentukan warna untuk setiap kategori
    warna <- c("#2eb857", "#404040")
    
    # Buat plot donat menggunakan plot_ly dengan argumen text untuk menampilkan teks
    plot_ly(data_pie_baduta, labels = ~Kategori, values = ~Nilai, 
            type = "pie", hole = 0.6, sort = FALSE,
            textinfo = "none",
            texttemplate = "",
            hoverinfo = "none",
            hovertext = paste0(data_pie_baduta$Kategori, ": ", data_pie_baduta$Nilai),
            marker = list(colors = warna)) %>%
      add_annotations(text = persentase_terdampingi,
                      x = 0.5, y = 0.5, 
                      font = list(color = "#404040", size = 30),
                      showarrow = FALSE) %>%
      layout(title = "", 
             xaxis = list(categoryorder = "array", categoryarray = c("Terdampingi", "Tidak Terdampingi")),
             showlegend = F,
             annotations = list(
               text = paste('Jumlah Keluarga Baduta:', scales::comma(jumlah_baduta, 
                                                                     big.mark = ".",
                                                                     decimal.mark = ",")),
               x = 0.5,
               y = -0.2,
               xref = 'paper',
               yref = 'paper',
               showarrow = FALSE,
               font = list(size = 14)
             ))
  })
  
  output$legend_baduta_verval <- renderUI({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    data_baduta_verval = verval_krs %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa
      ) %>%
      select(13:15)
    
    jumlah_baduta_hamil <- sum(data_baduta_verval$`Kel Baduta Ada`) + sum(data_baduta_verval$`baduta Hamil Baru`)
    baduta_ada = sum(data_baduta_verval$`Kel Baduta Ada`)
    baduta_baru = sum(data_baduta_verval$`Kel Baduta Baru`)
    
    warna <- c("#2eb857", "#404040")
    span(
      h6(
        span(
          bsicons::bs_icon("people"),
          style = "font-size:20px;color:#2eb857"),
        span(strong("Ada:", scales::comma(baduta_ada,
                                          big.mark = ".",
                                          decimal.mark = ",")), 
             style = "font-size:12px;color:#2eb857"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        span(
          bsicons::bs_icon("people"),
          style = "font-size:20px;color:#404040"),
        span(strong("Baru:", scales::comma(baduta_baru,
                                           big.mark = ".",
                                           decimal.mark = ",")), 
             style = "font-size:12px;color:#404040"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        style="text-align: center;"
      )
    )
  })
  
  output$pie_balita_verval <- renderPlotly({
    withProgress(message = "Membuat Grafik...", value = 0, {
      req(input$cari_rekap)
      filter_kabupaten <- value_filter_kab()
      filter_kecamatan <- value_filter_kec()
      filter_desa <- value_filter_desa_kel()
      incProgress(1/10, detail = "Mengambil Wilayah")
      
      
      data_balita_verval = verval_krs %>%
        filter(
          Kabupaten %in% filter_kabupaten,
          Kecamatan %in% filter_kecamatan,
          `Kelurahan/Desa` %in% filter_desa
        ) %>%
        select(15:17)
      incProgress(2/10, detail = "Filter Wilayah")
      jumlah_balita <- sum(data_balita_verval$`Kel Balita Ada`) + sum(data_balita_verval$`Balita Hamil Baru`)
      balita_ada = sum(data_balita_verval$`Kel Balita Ada`)
      balita_baru = sum(data_balita_verval$`Kel Balita Baru`)
      
      data_pie_balita <-data.frame(
        Kategori = c("Balita Ada", "Balita Baru"),
        Nilai = c(balita_ada, 
                  balita_baru)
      )
      
      # Tentukan teks untuk setiap sektor
      #teks <- paste(data_pie$Kategori, data_pie$Nilai, sep = ": ")
      
      persentase_terdampingi <- paste0(ubah_angka(round(data_pie_balita$Nilai[2] / sum(data_pie_balita$Nilai) * 100, 1)), "%")
      
      # Tentukan warna untuk setiap kategori
      warna <- c("#f05e16", "#404040")
      incProgress(3/10, detail = "Kalkulasi Data")
      # Buat plot donat menggunakan plot_ly dengan argumen text untuk menampilkan teks
      plot_ly_pie_balita <- plot_ly(data_pie_balita, labels = ~Kategori, values = ~Nilai, 
                                    type = "pie", hole = 0.6, sort = FALSE,
                                    textinfo = "none",
                                    texttemplate = "",
                                    hoverinfo = "none",
                                    hovertext = paste0(data_pie_balita$Kategori, ": ", data_pie_balita$Nilai),
                                    marker = list(colors = warna)) %>%
        add_annotations(text = persentase_terdampingi,
                        x = 0.5, y = 0.5, 
                        font = list(color = "#404040", size = 30),
                        showarrow = FALSE) %>%
        layout(title = "", 
               xaxis = list(categoryorder = "array", categoryarray = c("Terdampingi", "Tidak Terdampingi")),
               showlegend = F,
               annotations = list(
                 text = paste('Jumlah Keluarga Balita:', scales::comma(jumlah_balita, 
                                                                       big.mark = ".",
                                                                       decimal.mark = ",")),
                 x = 0.5,
                 y = -0.2,
                 xref = 'paper',
                 yref = 'paper',
                 showarrow = FALSE,
                 font = list(size = 14)
               ))
      incProgress(4/10, detail = "Aesthetic Grafik")
      
      for(i in 5:9){
        incProgress(i/10, "Sedikit lagi")
        Sys.sleep(0.25)
      }
      
      # Menampilkan grafik
      incProgress(10/10, detail = "Selesai")
      plot_ly_pie_balita
    })
  })
  
  output$legend_balita_verval <- renderUI({
    req(input$cari_rekap)
    filter_kabupaten <- value_filter_kab()
    filter_kecamatan <- value_filter_kec()
    filter_desa <- value_filter_desa_kel()
    
    data_balita_verval = verval_krs %>%
      filter(
        Kabupaten %in% filter_kabupaten,
        Kecamatan %in% filter_kecamatan,
        `Kelurahan/Desa` %in% filter_desa
      ) %>%
      select(15:17)
    
    jumlah_balita <- sum(data_balita_verval$`Kel Balita Ada`) + sum(data_balita_verval$`Kel Balita Ada`)
    balita_ada = sum(data_balita_verval$`Kel Balita Ada`)
    balita_baru = sum(data_balita_verval$`Kel Balita Baru`)
    
    warna <- c("#f05e16", "#404040")
    span(
      h6(
        span(
          bsicons::bs_icon("people"),
          style = "font-size:20px;color:#f05e16"),
        span(strong("Ada:", scales::comma(balita_ada,
                                          big.mark = ".",
                                          decimal.mark = ",")), 
             style = "font-size:12px;color:#f05e16"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        span(
          bsicons::bs_icon("people"),
          style = "font-size:20px;color:#404040"),
        span(strong("Baru:", scales::comma(balita_baru,
                                           big.mark = ".",
                                           decimal.mark = ",")), 
             style = "font-size:12px;color:#404040"),
        span(strong("Te"), 
             style = "font-size:12px;color:white"),
        style="text-align: center;"
      )
    )
  })
  ## akhir monitoring krs
  
  ## dataset
  
  nama_dataset <- eventReactive(input$cari_dataset,{
    nama_dataset = input$dataset
  })
  
  tingkat_wil_dataset <- eventReactive(input$cari_dataset,{
    tingkat_wil_Dataset = input$tingkat_wilayah
  })
  
  bulan_dataset <- eventReactive(input$cari_dataset,{
    bulan_dataset = input$pilih_bulan_dataset
  })
  
  output$judul_tabel_data <- renderText({
    if(nama_dataset() == "poktan_rampung"){
      judul = paste(
        "EVALUASI PEMBENTUKAN POKTAN/SETARA TINGKAT WILAYAH",
        tingkat_wil_dataset(), "- BULAN", bulan_dataset()
      )
    } else if(nama_dataset() == "unmet_need_0"){
      judul = "DESA/KELURAHAN DENGAN UNMET NEED (ABSOLUT) SANGAT RENDAH"
    }
  })
  
  output$tabel_data <- renderReactable({
    withProgress(message = 'Making plot', value = 0, {
      incProgress(1/2, detail = paste("Mulai"))
      #req(input$cari_dataset)
      if(nama_dataset() == "poktan_rampung"){
        bkb <- bkb %>%
          #select(-c(V1, BATAS)) %>%
          filter(BULAN == bulan_dataset())
        
        bkr <- bkr %>%
          #select(-c(V1, BATAS)) %>%
          filter(BULAN == bulan_dataset())
        
        bkl <- bkl %>%
          #select(-c(V1, BATAS)) %>%
          filter(BULAN == bulan_dataset())
        
        uppka <- uppka %>%
          #select(-c(V1, BATAS)) %>%
          filter(BULAN == bulan_dataset())
        
        pikr <- pikr%>%
          #select(-c(V1, BATAS)) %>%
          filter(BULAN == bulan_dataset())
        
        kkb <- kkb %>%
          #select(-c(V1, BATAS)) %>%
          filter(BULAN == bulan_dataset())
        
        rdk <- rdk %>%
          #select(-c(V1, BATAS)) %>%
          filter(BULAN == bulan_dataset())
        
        if(tingkat_wil_dataset() == "KABUPATEN"){
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
          
          tabel_react = reactable(
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
              headerStyle = list(background = "#7393B3", color = "black"),
              style = list(color = "black") # Atur teks hitam untuk semua sel
            ),
            filterable = TRUE,
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(10, 25, 50),
            bordered = TRUE, striped = TRUE, highlight = TRUE,
            resizable = TRUE,
            theme = reactableTheme(
              borderColor = "#808080",
              stripedColor = "#f6f8fa",
              highlightColor = "#f0f5f9",
              style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
            )
          )
        } 
        else if(tingkat_wil_dataset() == "KECAMATAN"){
          ## KECMATAN
          # Menghitung jumlah "Ada", "Tidak Ada", dan persentase untuk setiap kolom JUMLAH_ yang dikelompokkan berdasarkan KECAMATAN
          data_poktan_setara <- kkb%>%
            left_join(rdk, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            left_join(bkb, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            left_join(bkr, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            left_join(bkl, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            left_join(uppka, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            left_join(pikr, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            select(BULAN, everything()) %>%
            mutate(across(6:12, ~ ifelse(. == 0, "Tidak Ada", "Ada")))
          
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
          
          tabel_react = reactable(
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
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(10, 25, 50),
            theme = reactableTheme(
              borderColor = "#808080",
              stripedColor = "#f6f8fa",
              highlightColor = "#f0f5f9",
              style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
            )
          )
        } 
        else{
          ## desa/kel
          data_poktan_setara <- kkb%>%
            left_join(rdk, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            left_join(bkb, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            left_join(bkr, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            left_join(bkl, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            left_join(uppka, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            left_join(pikr, by = c("PROVINSI", "KABUPATEN", "KECAMATAN", "KELURAHAN", "BULAN")) %>%
            select(BULAN, everything()) %>%
            mutate(across(6:12, ~ ifelse(. == 0, "Tidak Ada", "Ada")))
          
          rekapitulasi <- data_poktan_setara %>%
            select(-c(1:2)) %>%
            left_join(nama_pkb[, -c(1, 6:8)], 
                      by = c("KABUPATEN" = "Kabupaten", "KECAMATAN" = "Kecamatan", "KELURAHAN" = "Kelurahan"))
          
          tabel_react = reactable(
            rekapitulasi,
            columns = list(
              JUMLAH_KKB = colDef(name = "Kampung KB",
                                  style = function(value) {
                                    color <- if (value == "Ada") {
                                      "#8fbc8f"
                                    } else if (value == "Tidak Ada") {
                                      "red"
                                    } else if (value == "Pending") {
                                      "orange"
                                    }
                                    style = list(background = color, color = "white", padding = "6px")
                                  }
              ),
              JUMLAH_RDK = colDef(name = "Rumah DataKu",
                                  style = function(value) {
                                    color <- if (value == "Ada") {
                                      "#8fbc8f"
                                    } else if (value == "Tidak Ada") {
                                      "red"
                                    } else if (value == "Pending") {
                                      "orange"
                                    }
                                    style = list(background = color, color = "white", padding = "6px")
                                  }
              ),
              JUMLAH_BKB = colDef(name = "Bina Keluarga Balita",
                                  style = function(value) {
                                    color <- if (value == "Ada") {
                                      "#8fbc8f"
                                    } else if (value == "Tidak Ada") {
                                      "red"
                                    } else if (value == "Pending") {
                                      "orange"
                                    }
                                    style = list(background = color, color = "white", padding = "6px")
                                  }
              ),
              JUMLAH_BKR = colDef(name = "Bina Keluarga Remaja",
                                  style = function(value) {
                                    color <- if (value == "Ada") {
                                      "#8fbc8f"
                                    } else if (value == "Tidak Ada") {
                                      "red"
                                    } else if (value == "Pending") {
                                      "orange"
                                    }
                                    style = list(background = color, color = "white", padding = "6px")
                                  }
              ),
              JUMLAH_BKL = colDef(name = "Bina Keluarga Lansia",
                                  style = function(value) {
                                    color <- if (value == "Ada") {
                                      "#8fbc8f"
                                    } else if (value == "Tidak Ada") {
                                      "red"
                                    } else if (value == "Pending") {
                                      "orange"
                                    }
                                    style = list(background = color, color = "white", padding = "6px")
                                  }
              ),
              JUMLAH_UPPKA = colDef(name = "UPPKA",
                                    style = function(value) {
                                      color <- if (value == "Ada") {
                                        "#8fbc8f"
                                      } else if (value == "Tidak Ada") {
                                        "red"
                                      } else if (value == "Pending") {
                                        "orange"
                                      }
                                      style = list(background = color, color = "white", padding = "6px")
                                    }
              ),
              JUMLAH_PIKR = colDef(name = "PIK Remaja",
                                   style = function(value) {
                                     color <- if (value == "Ada") {
                                       "#8fbc8f"
                                     } else if (value == "Tidak Ada") {
                                       "red"
                                     } else if (value == "Pending") {
                                       "orange"
                                     }
                                     style = list(background = color, color = "white", padding = "6px")
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
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(10, 25, 50),
            theme = reactableTheme(
              borderColor = "#808080",
              stripedColor = "#f6f8fa",
              highlightColor = "#f0f5f9",
              style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
            )
          )
        }
      } else if(nama_dataset() == "unmet_need_0"){
        data_pus = data_pus %>%
          filter(`Unmet Need` <= 5,
                 Bulan == bulan_dataset()) %>%
          arrange(`Unmet Need`, desc(PUS)) %>%
          select(Kabupaten, Kecamatan, Kelurahan_Desa, PUS, `Unmet Need`)
        tabel_react = reactable(
          data_pus,
          defaultColDef = colDef(
            align = "center",
            minWidth = 130,
            headerStyle = list(background = "#7393B3", color = "black")
          ),
          filterable = TRUE,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 25, 50),
          bordered = TRUE, striped = TRUE, highlight = TRUE,
          resizable = TRUE,
          theme = reactableTheme(
            borderColor = "#808080",
            stripedColor = "#f6f8fa",
            highlightColor = "#f0f5f9",
            style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
          )
        )
      }
      incProgress(1/1, detail = paste("Hampir Selesai"))
    }) # react incprogress
    
    tabel_react
    
  }) # render react
  
  # Fungsi untuk menghasilkan file yang akan diunduh
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df(), file)
    }
  )
  
  ## akhir dataset
  # ``
  
  ## analisis
  #output$analisis <- renderUI({
  # withProgress(message = "Tunggu", value = 0, {
  #  for(i in 1:9){
  #   incProgress(i/10, detail =  "Sedikit lagi")
  #  Sys.sleep(0.25)
  #}
  #incProgress(10/10, detail= "Selesai")
  #includeHTML("spatial_autokorelasi.html")
  #})
  
  
  #})
  #akhir analisis
}

# Run the application 
shinyApp(ui = ui, server = server)

