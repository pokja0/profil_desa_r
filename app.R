library(shiny)
library(bs4Dash)
library(waiter)
library(collapse)
library(bslib)
#library(dplyr)
library(fst)
library(leaflet)
library(leaflegend)
library(htmltools)
library(reactable)
library(plotly)
library(gt)
library(bsicons)
library(stringr)

daftar_bulan = c("JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI", "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER")
data_poktan = read_fst("data/data_profil_poktan.fst")

rekap_desa_verval_krs <- as.data.table(read_fst("data/rekap_desa_verval_krs.fst"))

#sasaran_genting <- fst::read_fst("DATASET KRS SASARAN GENTING/bnba_keluarga_genting_sulawesi barat.fst")
verval_krs_peta <- as.data.table(fst::read_fst("data/verval_krs_dashboard_peta.fst"))

ui = dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  header = dashboardHeader(title = "Profil Desa"),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "gambaran_umum",
        bs4Card(
          width = 12, title = "Pilih Wilayah",
          fluidRow(
            column(3, 
                   selectInput("pilih_kab_gu", "Daftar Kabupaten",
                               choices = c("SEMUA KABUPATEN", "PASANGKAYU", "MAMUJU TENGAH",
                                           "MAMUJU", "MAJENE", "POLEWALI MANDAR", "MAMASA"))
            ),
            column(3, selectInput("pilih_kec_gu", "Daftar Kecamatan", choices = NULL)),
            column(3, selectInput("pilih_desa_kel_gu", "Pilih Desa/Kel", choices = NULL)),
            column(3, selectInput("pilih_bulan_gu", "Pilih Bulan", choices = daftar_bulan[1:11], selected = "NOVEMBER"))
          ),
          fluidRow(
            input_task_button(
              label_busy = "Sedang Proses",
              id = "cari_gu",
              label = "Cari"
            )
          )
        ),
        card(
          h5(textOutput("tes_input_rekap_gu"), style="text-align: center;"),
          br(),
          layout_column_wrap(
            card(
              full_screen = T,
              leafletOutput("peta_titik_rekap_gu")
            ),
            card(
              full_screen = T,
              plotlyOutput("piramida_gu")
            )
          ),
          layout_column_wrap(
            card(
              gt_output("gt_kepemilikan_poktan")
            ),
            card(
              gt_output("gt_profil_sumber_daya")
            ),
            card(
              "PRO PN (on progress)"
            )
          ),
          layout_column_wrap(
            plotlyOutput("bar_kepemilikan_rumah"),
            plotlyOutput("bar_sumber_air"),
            plotlyOutput("bar_bab")
          )
        )
      ),
      tabItem(
        tabName = "kb",
        bs4Card(
          width = 12, title = "Pilih Wilayah",
          fluidRow(
            column(3, 
                   selectInput("pilih_kab_kb", "Daftar Kabupaten",
                               choices = c("SEMUA KABUPATEN", "PASANGKAYU", "MAMUJU TENGAH",
                                           "MAMUJU", "MAJENE", "POLEWALI MANDAR", "MAMASA"))
            ),
            column(3, selectInput("pilih_kec_kb", "Daftar Kecamatan", choices = NULL)),
            column(3, selectInput("pilih_desa_kel_kb", "Pilih Desa/Kel", choices = NULL)),
            column(3, selectInput("pilih_bulan_kb", "Pilih Bulan", choices = daftar_bulan[1:11], selected = "NOVEMBER"))
          ),
          fluidRow(
            input_task_button(
              label_busy = "Sedang Proses",
              id = "cari_kb",
              label = "Cari"
            )
          )
        ),
        card(
          h5(textOutput("tes_input_rekap_kb"), style="text-align: center;"),
          br(),
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
        )
      ),
      tabItem(
        tabName = "stunting",
        bs4Card(
          width = 12, title = "Pilih Wilayah",
          fluidRow(
            column(3, 
                   selectInput("pilih_kab_krs", "Daftar Kabupaten",
                               choices = c("SEMUA KABUPATEN", "PASANGKAYU", "MAMUJU TENGAH",
                                           "MAMUJU", "MAJENE", "POLEWALI MANDAR", "MAMASA"))
            ),
            column(3, selectInput("pilih_kec_krs", "Daftar Kecamatan", choices = NULL)),
            column(3, selectInput("pilih_desa_kel_krs", "Pilih Desa/Kel", choices = NULL)),
            column(3, selectInput("pilih_bulan_krs", "Pilih Bulan", choices = daftar_bulan[1:11], selected = "NOVEMBER"))
          ),
          fluidRow(
            input_task_button(
              label_busy = "Sedang Proses",
              id = "cari_krs",
              label = "Cari"
            )
          )
        ),
        card(
          h5(textOutput("tes_input_rekap_krs"), style="text-align: center;"),
          br(),
          fluidRow(
            shiny::column(4,
                          uiOutput("profil_stunting_rekap"),
                          tabBox(width = 12,collapsible = F,side = "right", solidHeader = T,type = "pills",
                            title = "Faktor KRS",
                            tabPanel(
                              "Jumlah", 
                              plotlyOutput("bar_faktor_resiko_jumlah")
                            ),
                            tabPanel(
                              "Persentase", 
                              plotlyOutput("bar_faktor_resiko_persen")
                            )
                          ),
                          uiOutput("jumlah_posyandu")
                          
            ),
            shiny::column(8,
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
        )
      ),
      tabItem(
        tabName = "genting",
        bs4Card(
          title = "Pilih Wilayah",
          width = 12,
          fluidRow(
            column(
              3,
              selectInput(
                "jenis_sasaran_genting", label = "Pilih Data", choices = c("Semua", "Jamban", "Sumber Air + 4T")
              )
            ),
            column(
              3,
              selectInput(
                "kabupaten_genting", label = "Pilih Kabupaten", choices = NULL
              )
            ),
            column(
              3,
              selectInput(
                "kecamatan_genting", label = "Pilih Kecamatan", choices = NULL
              )
            ),
            column(
              3,
              input_task_button(
                "cari_genting", "Cari"
              )
            )
          ) #fluid row
        ) #bscard
      ) #tab item
    )
  ),
  sidebar = dashboardSidebar(
    skin = "light",
    inputId = "sidebarState",
    sidebarMenu(
      id = "sidebar",
      menuItem(
        text = "Gambaran Umum",
        tabName = "gambaran_umum",
        icon = icon("house")
      ),
      menuItem(
        text = "Keluarga Berencana",
        tabName = "kb",
        icon = icon("tablets")
      ),
      menuItem(
        text = "Stunting",
        tabName = "stunting",
        icon = icon("baby")
      ),
      menuItem(
        text = "Genting",
        tabName = "genting",
        icon = icon("people-pulling"),
        selected = TRUE
      )
    )
  ) #sidebar
) #dashboarad page

server = function(input, output, session) {
  # GU
  
  # gu input
  observeEvent(input$pilih_kab_gu, {
    if (input$pilih_kab_gu == "SEMUA KABUPATEN") {
      updateSelectInput(session, "pilih_kec_gu",
                        choices = c("SEMUA KECAMATAN"))
    } else {
      daftar_kecamatan = data_poktan |>
        fselect(KABUPATEN, KECAMATAN) |>
        fsubset(KABUPATEN == input$pilih_kab_gu) |>
        fselect(KECAMATAN)
      daftar_kecamatan = daftar_kecamatan$KECAMATAN
      updateSelectInput(session, "pilih_kec_gu",
                        choices = c("SEMUA KECAMATAN",
                                    daftar_kecamatan))
    }
  })
  
  observeEvent(input$pilih_kec_gu, {
    if (input$pilih_kec_gu == "SEMUA KECAMATAN") {
      updateSelectInput(session, "pilih_desa_kel_gu",
                        choices = c("SEMUA DESA/KEL"))
    } else {
      daftar_kel = data_poktan |>
        fselect(KECAMATAN, KELURAHAN) |>
        fsubset(KECAMATAN == input$pilih_kec_gu) |>
        fselect(KELURAHAN)
      daftar_kel = daftar_kel$KELURAHAN
      updateSelectInput(session, "pilih_desa_kel_gu",
                        choices = c("SEMUA DESA/KEL", 
                                    daftar_kel))
    }
  })
  
  ## batas gu input
  
  ## gu filter
  # filter kab
  value_filter_kab_gu <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_gu, {
    kondisi_input = input$pilih_kab_gu
    if (kondisi_input == "SEMUA KABUPATEN"){
      filter_kabupaten = unique(data_poktan$KABUPATEN)
    } else{
      filter_kabupaten = input$pilih_kab_gu
    }
    value_filter_kab_gu(filter_kabupaten) 
  })
  
  #kecamatan
  value_filter_kec_gu <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_gu, {
    kondisi_input = input$pilih_kec_gu
    filter_kabupaten = value_filter_kab_gu()
    
    if (kondisi_input == "SEMUA KECAMATAN"){
      daftar_kecamatan = data_poktan |>
        fselect(KABUPATEN, KECAMATAN) |>
        fsubset(KABUPATEN %in% filter_kabupaten) |>
        fselect(KECAMATAN)
      filter_kecamatan = daftar_kecamatan$KECAMATAN
    } else{
      filter_kecamatan = input$pilih_kec_gu
    }
    value_filter_kec_gu(filter_kecamatan) 
  })
  
  # desa
  value_filter_desa_kel_gu <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_gu, {
    kondisi_input = input$pilih_desa_kel_gu
    filter_kabupaten = value_filter_kab_gu()
    filter_kecamatan = value_filter_kec_gu()
    
    if (kondisi_input == "SEMUA DESA/KEL"){
      daftar_kel = data_poktan |>
        select(KABUPATEN, KECAMATAN, KELURAHAN) |>
        fsubset(
          KABUPATEN %in% filter_kabupaten) |>
        fsubset(KECAMATAN %in% filter_kecamatan) |>
        select(KELURAHAN)
      filter_desa_kel_gu = daftar_kel$KELURAHAN
    } else{
      filter_desa_kel_gu = input$pilih_desa_kel_gu
    }
    value_filter_desa_kel_gu(filter_desa_kel_gu) 
  })
  
  # bulan
  value_filter_bulan_gu <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_gu, {
    value_filter_bulan_gu(input$pilih_bulan_gu) 
  })
  
  ## batas gu filter
  
  ## gu judul
  values <- reactiveValues(default = 0)
  
  observeEvent(input$cari_gu,{
    values$default <- input$cari_gu
  })
  
  teks_judul_rekap_gu <- eventReactive(input$cari_gu, {
    if(input$pilih_kab_gu == "SEMUA KABUPATEN"){
      nama_daerah = "SULAWESI BARAT"
      tingkat_daerah = "PROVINSI"
    } else if(input$pilih_kec_gu == "SEMUA KECAMATAN"){
      nama_daerah = input$pilih_kab_gu
      tingkat_daerah = "KABUPATEN"
    } else if(input$pilih_desa_kel_gu == "SEMUA DESA/KEL"){
      nama_daerah = input$pilih_kec_gu
      tingkat_daerah = "KECAMATAN"
    } else{
      nama_daerah = value_filter_desa_kel_gu()
      tingkat_daerah = "DESA/KELURAHAN"
    }
    teks <- paste(tingkat_daerah, nama_daerah, "-", input$pilih_bulan_gu)
    # if(tingkat_daerah == "KELURAHAN"){
    #   teks <- paste0("PROFIL DESA/", tingkat_daerah, " ", nama_daerah, " - ", input$bulan_rekap)
    # } else{
    #   teks <- paste("PROFIL", tingkat_daerah, nama_daerah, "-", input$bulan_rekap)
    # }
  })

  output$tes_input_rekap_gu <- renderText({
    if(values$default == 0){
      teks = "Klik Cari Untuk Menampilkan Halaman"
    }
    else{
      teks = teks_judul_rekap_gu()
    }
  })
  
  ## batas gu judul
  
  ## peta
  batas_sulbar <- readRDS("data/batas_sulbar.rds")
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
  
  output$peta_titik_rekap_gu <- renderLeaflet({
    req(input$cari_gu)
    batas_sulbar <- batas_sulbar
    #kecamatan <- c("POLEWALI", "PASANGKAYU")
    #desa_kel <- c("POLEWALI", "PASANGKAYU")
    
    kecamatan <- value_filter_kec_gu()
    
    desa_kel <- value_filter_desa_kel_gu()
    # fsubset(KECAMATAN %in% kecamatan, KELURAHAN %in% desa_kel) |>
    #   group_by(PROVINSI) |>
    # 
    # tingkat_daerah <- tingkat_daerah_filter()
    # nama_daerah <- nama_daerah_filter()
    titik_puskesmas <- titik_puskesmas |>
      fsubset(KECAMATAN %in% kecamatan)
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
    
    titik_sd <- titik_sd |>
      fsubset(KECAMATAN %in% kecamatan) |>
      fsubset(KELURAHAN %in% desa_kel) |>
      fselect(KECAMATAN, Nama_Sekolah, Latitude, Longitude, KELURAHAN)
    
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
    
    titik_smp <- titik_smp |>
      fsubset(KECAMATAN %in% kecamatan) |>
      fselect(KECAMATAN, Nama_Sekolah, Latitude, Longitude, KELURAHAN)
    
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
    
   # library(dplyr)
    
    df_peta <- data_desa |>
      fselect(PROVINSI, KABUPATEN, KECAMATAN, KELURAHAN, LATITUDE, LONGITUDE) |>
      fsubset(KECAMATAN %in% kecamatan) |>
      fsubset(KELURAHAN %in% desa_kel)
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
    
    
    leaflet(df_peta) |> 
      setView(lng = mean(df_peta$LONGITUDE), 
              lat = mean(df_peta$LATITUDE), 
              zoom = 8) |>
      addPolygons(
        data = batas_sulbar,weight = 2,opacity = 1, fillColor = "white",
        color = "darkgreen", dashArray = "3",fillOpacity = 0.7, label = batas_sulbar$KABUPATEN,
        highlightOptions = highlightOptions(
          weight = 5,color = "#666",  dashArray = "",fillOpacity = 0.7,bringToFront = F),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "10px", direction = "auto")) |>
      addProviderTiles(providers$CyclOSM) |>
      setView(lng = mean(df_peta$LONGITUDE), lat = mean(df_peta$LATITUDE), zoom = 7) |>
      addMarkers(~LONGITUDE, ~LATITUDE, label = ~htmlEscape(KELURAHAN), 
                 clusterOptions = markerClusterOptions()) |>
      addMarkers(lat = titik_smp$Latitude, lng = titik_smp$Longitude, 
                 clusterOptions = markerClusterOptions(), label = ~htmlEscape(titik_smp$Nama_Sekolah), 
                 icon = icon_sekolah, group = "titik_smp") |>
      addMarkers(lat = titik_sd$Latitude, lng = titik_sd$Longitude, 
                 clusterOptions = markerClusterOptions(), group = "titik_sd",
                 label = ~htmlEscape(titik_sd$Nama_Sekolah), icon = icon_sekolah) |>
      addMarkers(lat = titik_puskesmas$LATITUDE, lng = titik_puskesmas$LONGITUDE, group = "titik_puskesmas",
                 clusterOptions = markerClusterOptions(),
                 label = ~htmlEscape(titik_puskesmas$PUSKESMAS), icon = icon_puskesmas) |>
      addLegendImage(images = c("img/logo_desa.png", "img/logo_sekolah.png",
                                "img/logo_puskesmas.png"),
                     labels = c('Desa', 'Sekolah', 'Puskesmas'),width = 20, height = 20,
                     orientation = 'vertical',
                     title = htmltools::tags$div('Keterangan',
                                                 style = 'font-size: 20px; text-align: center;'),
                     position = 'topright') |>
      hideGroup(cek_sd) |>
      hideGroup(cek_smp)
    
  })
  ## batas peta
  
  ## piramida
  kelompok_umur_lk <- read_fst("data/data_piramida_lk.fst")
  kelompok_umur_pr <- read_fst("data/data_piramida_perempuan.fst")
  
  output$piramida_gu <-renderPlotly({
    req(input$cari_gu)
    kecamatan <- value_filter_kec_gu()
    desa_kel <- value_filter_desa_kel_gu()
    kelompok_umur_lk <- kelompok_umur_lk |>
      fsubset(KECAMATAN %in% kecamatan & KELURAHAN %in% desa_kel) |>
      fgroup_by(PROVINSI) |> 
      fsummarise(across(is.numeric, fsum)) |>
      pivot(values = c(4:20), how = "l", names = list("Kelompok_Umur", "Jumlah"))
    
    kelompok_umur_pr <- kelompok_umur_pr |>
      fsubset(KECAMATAN %in% kecamatan & KELURAHAN %in% desa_kel) |>
      fgroup_by(PROVINSI) |> 
      fsummarise(across(is.numeric, fsum)) |>
      pivot(values = c(4:20), how = "l", names = list("Kelompok_Umur", "Jumlah"))
    
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
    
    piramida_interaktif <- piramida_interaktif |>
      layout(title = "Grafik Piramida Penduduk",
             xaxis = list(title = "Jumlah Penduduk", tickangle=0,
                          tickvals = round(seq(-batas_angka, batas_angka, length.out = 7)), 
                          ticktext = round(abs(seq(-batas_angka, batas_angka, length.out = 7)))
             ),
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
    piramida_interaktif |>
      add_annotations(
        text = "Perempuan", x = -batas_angka, y = 15,
        showarrow = FALSE, font = list(color = "#000", size = 14), xref = "x", yref = "y"
      ) |>
      add_annotations(
        text = "Laki-Laki", x = batas_angka, y = 15,
        showarrow = FALSE, font = list(color = "#000", size = 14), xref = "x", yref = "y"
      )
    
    
  })
  ##batas piramida
  
  ## kepemilikan poktan
  #kepemilikan poktan
  filter_poktan <- function(data, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan) {
    hasil <- data |>
      filter(
        KABUPATEN %in% filter_kabupaten &
        KECAMATAN %in% filter_kecamatan &
        KELURAHAN %in% filter_desa &
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
  
  daftar_desa <- read_fst("data/data_daftar_desa.fst")
  
  output$gt_kepemilikan_poktan <- render_gt({
    req(input$cari_gu)
    filter_kabupaten <- value_filter_kab_gu()
    filter_kecamatan <- value_filter_kec_gu()
    filter_desa <- value_filter_desa_kel_gu()
    filter_bulan <- value_filter_bulan_gu()
    
    bkb = filter_poktan(bkb, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    bkr = filter_poktan(bkr, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    bkl = filter_poktan(bkl, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    uppka = filter_poktan(uppka, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    pikr = filter_poktan(pikr, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    kkb = filter_poktan(kkb, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    rdk = filter_poktan(rdk, filter_kabupaten, filter_kecamatan, filter_desa, filter_bulan)
    daftar_desa = daftar_desa |>
      filter(
        KABUPATEN %in% filter_kabupaten,
        KECAMATAN %in% filter_kecamatan,
        KELURAHAN %in% filter_desa
      )
    
    kepemilikan_poktan <- data.frame(
      "Indikator" = c("Desa/Kelurahan", "Kampung KB", "Rumah DataKu", "Bina Keluarga Balita",
                      "Bina Keluarga Remaja", "Bina Keluarga Lansia", "UPPKA", "PIK-R"),
      "Jumlah" = c(nrow(daftar_desa), sum(as.numeric(kkb$JUMLAH_KKB)), sum(as.numeric(rdk$JUMLAH_RDK)),
                   sum(as.numeric(bkb$JUMLAH_BKB)), sum(as.numeric(bkr$JUMLAH_BKR)),
                   sum(as.numeric(bkl$JUMLAH_BKL)), sum(as.numeric(uppka$JUMLAH_UPPKA)),
                   sum(as.numeric(pikr$JUMLAH_PIKR)))
    )
    
    gt(kepemilikan_poktan) |>
      tab_header(
        title = md("**Kepemilikan Poktan**") # Ganti judul sesuai kebutuhan
      )
  })
  
  # data_sumber_daya <- fread("data/profil_sumber_daya.csv")
  # write.fst(data_sumber_daya, "data/profil_sumber_daya.fst")
  data_sumber_daya <- read_fst("data/profil_sumber_daya.fst")
  
  output$gt_profil_sumber_daya <- render_gt({
    req(input$cari_gu)
    filter_kecamatan <- value_filter_kec_gu()
    filter_desa <- value_filter_desa_kel_gu()
    df_sd_rekap <- data_sumber_daya |>
      fsubset(KECAMATAN %in% filter_kecamatan & KELURAHAN %in% filter_desa) |>
      fgroup_by(PROVINSI) |>
      fsummarise(
        LUAS_WILAYAH = paste(scales::comma(fsum(LUAS_WILAYAH), 
                                           big.mark = ".",
                                           decimal.mark = ",", 
                                           accuracy = 0.01),
                             "km²"),
        JUMLAH_PENDUDUK = paste(scales::comma(fsum(JUMLAH_PENDUDUK), 
                                              big.mark = ".",
                                              decimal.mark = ",")),
        KEPADATAN_PENDUDUK = paste(scales::comma(round(fsum(JUMLAH_PENDUDUK)/fsum(LUAS_WILAYAH),2), 
                                                 big.mark = ".",
                                                 decimal.mark = ",", 
                                                 accuracy = 0.01)),
        KRS = paste(scales::comma(fsum(KRS), 
                                  big.mark = ".",
                                  decimal.mark = ","))
        ) |>
      pivot(values = c(2:5), how = "l", names = list("Indikator", "Jumlah")) |>
      fselect(Indikator, Jumlah)
    df_sd_rekap$Indikator <- as.character(df_sd_rekap$Indikator)
    
    if(length(filter_desa) > 1){
      idm = ""
    } else{
      idm = data_sumber_daya |>
        fsubset(KECAMATAN %in% filter_kecamatan & KELURAHAN %in% filter_desa) |>
        fselect(IDM)
      idm = idm$IDM
      if(idm == ""){
        idm = "NA"
      } else{
        idm = idm
      }
    }
    df_sd_rekap[1,1] = "Luas Wilayah"
    df_sd_rekap[2,1] = "Jumlah Penduduk"
    df_sd_rekap[3,1] = "Kepadatan Penduduk"
    df_sd_rekap[4,1] = "KRS"
    df_sd_rekap[5,1] = "IDM"
    df_sd_rekap[5,2] = idm
    gt(df_sd_rekap) |>
      tab_header(
        title = md("**Profil Wilayah**") # Ganti judul sesuai kebutuhan
      ) |>
      cols_align(
        align = "right",
        columns = Jumlah
      )
  })
  
  ## batas gt
  
  ## bar
  data_krs <- read_fst("data/data_KRS_2023.fst")
  data_krs <-data_krs |>
    fmutate(across(6:28, as.numeric))
  output$bar_kepemilikan_rumah <- renderPlotly({
    req(input$cari_gu)
    filter_kabupaten <- value_filter_kab_gu()
    filter_kecamatan <- value_filter_kec_gu()
    filter_desa <- value_filter_desa_kel_gu()

    data_kepemilikan_rumah = data_krs |>
      fsubset(
        KABUPATEN %in% filter_kabupaten &
        KECAMATAN %in% filter_kecamatan &
        KELURAHAN %in% filter_desa
      ) |>
      fselect(24:28) |>
      fsummarise(across(`MILIK SENDIRI`:`KATEGORI LAINNYA`, fsum)) |>
      pivot(values = c(1:5), how = "l", names = list("Jenis Kepemilikan", "Total")) |>
      fmutate(Text = scales::comma(Total, big.mark = ".", 'decimal.mark' = ","))
    
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
    grafik_kepemilikan_rumah <- grafik_kepemilikan_rumah |>
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
    req(input$cari_gu)
    filter_kabupaten <- value_filter_kab_gu()
    filter_kecamatan <- value_filter_kec_gu()
    filter_desa <- value_filter_desa_kel_gu()
    filter_bulan <- value_filter_bulan_gu()

    data_sumber_air = data_krs |>
      fsubset(
        KABUPATEN %in% filter_kabupaten &
        KECAMATAN %in% filter_kecamatan &
        KELURAHAN %in% filter_desa
      ) |>
      fselect(7:14) |>
      fsummarise(across(`AIR KEMASAN/ISI ULANG`:`SUMBER LAINNYA`, fsum)) |>
      pivot(values = c(1:8), how = "l", names = list("Sumber Air", "Total")) |>
      fmutate(Text = scales::comma(Total, big.mark = ".", 'decimal.mark' = ","))
    
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
    grafik_sumber_air <- grafik_sumber_air |>
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
      req(input$cari_gu)
      filter_kabupaten <- value_filter_kab_gu()
      filter_kecamatan <- value_filter_kec_gu()
      filter_desa <- value_filter_desa_kel_gu()
      filter_bulan <- value_filter_bulan_gu()
      
      incProgress(1/10, detail = "Mengambil Wilayah")
      
      data_bab = data_krs |>
        fsubset(
          KABUPATEN %in% filter_kabupaten &
          KECAMATAN %in% filter_kecamatan &
          KELURAHAN %in% filter_desa
        ) |>
        fselect(15:17) |>
        fsummarise(across(`SEPTIC TANK`:`TIDAK ADA`, fsum)) |>
        pivot(values = c(1:3), how = "l", names = list("Fasilitas BAB", "Total")) |>
        fmutate(Text = scales::comma(Total, big.mark = ".", 'decimal.mark' = ","))
      
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
      grafik_bab <- grafik_bab |>
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
  
  ## batas bar
  
  # batas GU
  
  # kb
  # kb input
  observeEvent(input$pilih_kab_kb, {
    if (input$pilih_kab_kb == "SEMUA KABUPATEN") {
      updateSelectInput(session, "pilih_kec_kb",
                        choices = c("SEMUA KECAMATAN"))
    } else {
      daftar_kecamatan = data_poktan |>
        fselect(KABUPATEN, KECAMATAN) |>
        fsubset(KABUPATEN == input$pilih_kab_kb) |>
        fselect(KECAMATAN)
      daftar_kecamatan = daftar_kecamatan$KECAMATAN
      updateSelectInput(session, "pilih_kec_kb",
                        choices = c("SEMUA KECAMATAN",
                                    daftar_kecamatan))
    }
  })
  
  observeEvent(input$pilih_kec_kb, {
    if (input$pilih_kec_kb == "SEMUA KECAMATAN") {
      updateSelectInput(session, "pilih_desa_kel_kb",
                        choices = c("SEMUA DESA/KEL"))
    } else {
      daftar_kel = data_poktan |>
        fselect(KECAMATAN, KELURAHAN) |>
        fsubset(KECAMATAN == input$pilih_kec_kb) |>
        fselect(KELURAHAN)
      daftar_kel = daftar_kel$KELURAHAN
      updateSelectInput(session, "pilih_desa_kel_kb",
                        choices = c("SEMUA DESA/KEL", 
                                    daftar_kel))
    }
  })
  
  ## batas kb input
  
  ## kb filter
  # filter kab
  value_filter_kab_kb <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_kb, {
    kondisi_input = input$pilih_kab_kb
    if (kondisi_input == "SEMUA KABUPATEN"){
      filter_kabupaten = unique(data_poktan$KABUPATEN)
    } else{
      filter_kabupaten = input$pilih_kab_kb
    }
    value_filter_kab_kb(filter_kabupaten) 
  })
  
  #kecamatan
  value_filter_kec_kb <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_kb, {
    kondisi_input = input$pilih_kec_kb
    filter_kabupaten = value_filter_kab_kb()
    
    if (kondisi_input == "SEMUA KECAMATAN"){
      daftar_kecamatan = data_poktan |>
        fselect(KABUPATEN, KECAMATAN) |>
        fsubset(KABUPATEN %in% filter_kabupaten) |>
        fselect(KECAMATAN)
      filter_kecamatan = daftar_kecamatan$KECAMATAN
    } else{
      filter_kecamatan = input$pilih_kec_kb
    }
    value_filter_kec_kb(filter_kecamatan) 
  })
  
  # desa
  value_filter_desa_kel_kb <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_kb, {
    kondisi_input = input$pilih_desa_kel_kb
    filter_kabupaten = value_filter_kab_kb()
    filter_kecamatan = value_filter_kec_kb()
    
    if (kondisi_input == "SEMUA DESA/KEL"){
      daftar_kel = data_poktan |>
        select(KABUPATEN, KECAMATAN, KELURAHAN) |>
        fsubset(
          KABUPATEN %in% filter_kabupaten) |>
        fsubset(KECAMATAN %in% filter_kecamatan) |>
        select(KELURAHAN)
      filter_desa_kel_kb = daftar_kel$KELURAHAN
    } else{
      filter_desa_kel_kb = input$pilih_desa_kel_kb
    }
    value_filter_desa_kel_kb(filter_desa_kel_kb) 
  })
  
  # bulan
  value_filter_bulan_kb <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_kb, {
    value_filter_bulan_kb(input$pilih_bulan_kb) 
  })
  ## batas kb filter
  
  ## kb judul
  values_judul_kb <- reactiveValues(default = 0)
  
  observeEvent(input$cari_kb,{
    values_judul_kb$default <- input$cari_kb
  })
  
  teks_judul_rekap_kb <- eventReactive(input$cari_kb, {
    if(input$pilih_kab_kb == "SEMUA KABUPATEN"){
      nama_daerah = "SULAWESI BARAT"
      tingkat_daerah = "PROVINSI"
    } else if(input$pilih_kec_kb == "SEMUA KECAMATAN"){
      nama_daerah = input$pilih_kab_kb
      tingkat_daerah = "KABUPATEN"
    } else if(input$pilih_desa_kel_kb == "SEMUA DESA/KEL"){
      nama_daerah = input$pilih_kec_kb
      tingkat_daerah = "KECAMATAN"
    } else{
      nama_daerah = value_filter_desa_kel_kb()
      tingkat_daerah = "DESA/KELURAHAN"
    }
    teks <- paste(tingkat_daerah, nama_daerah, "-", input$pilih_bulan_kb)
    # if(tingkat_daerah == "KELURAHAN"){
    #   teks <- paste0("PROFIL DESA/", tingkat_daerah, " ", nama_daerah, " - ", input$bulan_rekap)
    # } else{
    #   teks <- paste("PROFIL", tingkat_daerah, nama_daerah, "-", input$bulan_rekap)
    # }
  })
  
  output$tes_input_rekap_kb <- renderText({
    if(values_judul_kb$default == 0){
      teks = "Klik Cari Untuk Menampilkan Halaman"
    }
    else{
      teks = teks_judul_rekap_kb()
    }
  })
  
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
  
  ## vb kb
  data_pus <- read_fst("data/data_pus.fst")
  data_pus <- data_pus |>
    fmutate(across(6:7, as.numeric))
  
  output$vb_unmet_need_rekap <- renderUI({
    req(input$cari_kb)
    filter_kabupaten <- value_filter_kab_kb()
    filter_kecamatan <- value_filter_kec_kb()
    filter_desa <- value_filter_desa_kel_kb()
    filter_bulan <- value_filter_bulan_kb()
    
    unmet_need_bulan = data_pus |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        Kelurahan_Desa %in% filter_desa &
        Bulan %in% filter_bulan
      )
    
    unmet_need_bulan <- round(sum(unmet_need_bulan$`Unmet Need`) / sum(unmet_need_bulan$PUS) *100 , 2)
    
    unmet_need_bulan_sebelum = data_pus |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        Kelurahan_Desa %in% filter_desa &
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
  
  sdm_kb <- read_fst("data/data_faskes_siga.fst")
  output$sdm_vb_1_rekap <- renderText({
    req(input$cari_kb)
    filter_kabupaten <- value_filter_kab_kb()
    filter_kecamatan <- value_filter_kec_kb()
    filter_desa <- value_filter_desa_kel_kb()
    filter_bulan <- value_filter_bulan_kb()
    
    sdm_cek <- sdm_kb |>
      fsubset(
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa &
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
    req(input$cari_kb)
    filter_kabupaten <- value_filter_kab_kb()
    filter_kecamatan <- value_filter_kec_kb()
    filter_desa <- value_filter_desa_kel_kb()
    filter_bulan <- value_filter_bulan_kb()
    
    tp_kb_cek <- sdm_kb |>
      fsubset(
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa & 
        BULAN %in% filter_bulan
      )
    
    if(nrow(tp_kb_cek) <= 0){
      tp_kb_cek = 0
    } else{
      tp_kb_cek <- length(unique(tp_kb_cek$`No. Registrasi`))
    }
    
    tp_kb_cek 
  })
  
  data_mix_kontra <- read_fst("data/data_mix_kontra.fst")
  data_mix_kontra <- data_mix_kontra |>
    fmutate(across(6:16, as.numeric))
  output$vb_mkjp_rekap <- renderUI({
    req(input$cari_kb)
    filter_kabupaten <- value_filter_kab_kb()
    filter_kecamatan <- value_filter_kec_kb()
    filter_desa <- value_filter_desa_kel_kb()
    filter_bulan <- value_filter_bulan_kb()
    
    mkjp_bulan = data_mix_kontra |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa &
        Bulan %in% filter_bulan
      ) |> 
      fsummarise(
        mkjp = Implan + IUD + Vasektomi + Tubektomi
      )
    
    mkjp_bulan <- sum(mkjp_bulan$mkjp)
    
    mkjp_bulan_sebelum = data_mix_kontra |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa &
        Bulan %in% pilih_bulan_sebelumnya(filter_bulan)
      )|> 
      fsummarise(
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
    req(input$cari_kb)
    filter_kabupaten <- value_filter_kab_kb()
    filter_kecamatan <- value_filter_kec_kb()
    filter_desa <- value_filter_desa_kel_kb()
    filter_bulan <- value_filter_bulan_kb()
    
    jumlah_pus = data_pus |>
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
    req(input$cari_kb)
    filter_kabupaten <- value_filter_kab_kb()
    filter_kecamatan <- value_filter_kec_kb()
    filter_desa <- value_filter_desa_kel_kb()
    filter_bulan <- pilih_sampai_bulan(value_filter_bulan_kb())
    
    jumlah_pus_plot = data_pus |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        Kelurahan_Desa %in% filter_desa &
        Bulan %in% filter_bulan
      ) |>
      fgroup_by(Bulan) |>
      fsummarise(PUS = sum(PUS))
    
    # Konversi kolom bulan menjadi faktor dengan urutan JANUARI - DESEMBER
    jumlah_pus_plot$Bulan <- factor(jumlah_pus_plot$Bulan, levels = c(
      "JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI",
      "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"
    ))
    
    # Mengurutkan dataframe berdasarkan urutan bulan
    jumlah_pus_plot <- jumlah_pus_plot[order(jumlah_pus_plot$Bulan), ]
    
    # Membuat plot garis dengan 
    
    plot_ly(jumlah_pus_plot, x = ~Bulan, y = ~PUS, 
            type = 'scatter', mode = 'lines+markers') |>
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) |>
      config(displayModeBar = F) |>
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
    req(input$cari_kb)
    filter_kabupaten <- value_filter_kab_kb()
    filter_kecamatan <- value_filter_kec_kb()
    filter_desa <- value_filter_desa_kel_kb()
    filter_bulan <- value_filter_bulan_kb()
    
    jumlah_pus = data_pus |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        Kelurahan_Desa %in% filter_desa &
        Bulan %in% filter_bulan
      )
    
    jumlah_mcpr = data_mix_kontra |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa &
        Bulan %in% filter_bulan
      )
    
    paste0(round(sum(jumlah_mcpr$`KB Modern`)/sum(jumlah_pus$PUS) * 100, 2), "%")
    
  })
  
  output$line_mcpr <- renderPlotly({
    req(input$cari_kb)
    filter_kabupaten <- value_filter_kab_kb()
    filter_kecamatan <- value_filter_kec_kb()
    filter_desa <- value_filter_desa_kel_kb()
    filter_bulan <- pilih_sampai_bulan(value_filter_bulan_kb())
    
    jumlah_pus_plot = data_pus |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        Kelurahan_Desa %in% filter_desa &
        Bulan %in% filter_bulan
      ) |>
      fgroup_by(Bulan) |>
      fsummarise(PUS = sum(PUS))
    
    jumlah_mcpr_plot = data_mix_kontra |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa &
        Bulan %in% filter_bulan
      ) |>
      fgroup_by(Bulan) |>
      fsummarise(MCPR = sum(`KB Modern`))
    
   # jumlah_mcpr_plot <- inner_join(jumlah_mcpr_plot, jumlah_pus_plot, by = "Bulan") 
    jumlah_mcpr_plot <- join(jumlah_mcpr_plot, jumlah_pus_plot, 
                             on = "Bulan", how = "inner", multiple = TRUE)
    jumlah_mcpr_plot <- jumlah_mcpr_plot |>
      fgroup_by(Bulan) |>
      fsummarise(MCPR = round(MCPR/PUS * 100, 2))
    
    # Konversi kolom bulan menjadi faktor dengan urutan JANUARI - DESEMBER
    jumlah_mcpr_plot$Bulan <- factor(jumlah_mcpr_plot$Bulan, levels = c(
      "JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI",
      "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"
    ))
    
    # Mengurutkan dataframe berdasarkan urutan bulan
    jumlah_mcpr_plot <- jumlah_mcpr_plot[order(jumlah_mcpr_plot$Bulan), ]
    
    # Membuat plot garis dengan 
    
    plot_ly(jumlah_mcpr_plot, x = ~Bulan, y = ~MCPR, 
            type = 'scatter', mode = 'lines+markers') |>
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) |>
      config(displayModeBar = F) |>
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
    req(input$cari_kb)
    filter_kabupaten <- value_filter_kab_kb()
    filter_kecamatan <- value_filter_kec_kb()
    filter_desa <- value_filter_desa_kel_kb()
    filter_bulan <- value_filter_bulan_kb()
    
    jumlah_mix_kontra = data_mix_kontra |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa &
        Bulan %in% filter_bulan
      ) |>
      fselect(7:14) |>
      fsummarise(across(is.numeric, fsum)) |>
      pivot(values = c(1:8), how = "l", names = list("Metode", "Total")) |>
      fmutate(Text = scales::comma(Total, big.mark = ".",
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
    grafik_kontrasepsi <- grafik_kontrasepsi |>
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
    req(input$cari_kb)
    filter_kabupaten <- value_filter_kab_kb()
    filter_kecamatan <- value_filter_kec_kb()
    filter_desa <- value_filter_desa_kel_kb()
    filter_bulan <- pilih_sampai_bulan(value_filter_bulan_kb())
    
    jumlah_pa = data_mix_kontra |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa &
        Bulan %in% filter_bulan
      ) |>
      fgroup_by(Bulan) |>
      fsummarise(PA = sum(PA))
    
    # Konversi kolom bulan menjadi faktor dengan urutan JANUARI - DESEMBER
    jumlah_pa$Bulan <- factor(jumlah_pa$Bulan, levels = c(
      "JANUARI", "FEBRUARI", "MARET", "APRIL", "MEI", "JUNI",
      "JULI", "AGUSTUS", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DESEMBER"
    ))
    
    # Mengurutkan dataframe berdasarkan urutan bulan
    jumlah_pa <- jumlah_pa[order(jumlah_pa$Bulan), ]
    
    # Membuat plot garis dengan 
    
    plot_ly(jumlah_pa, x = ~Bulan, y = ~PA, 
            type = 'scatter', mode = 'lines+markers') |>
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
      req(input$cari_kb)
      filter_kabupaten <- value_filter_kab_kb()
      filter_kecamatan <- value_filter_kec_kb()
      filter_desa <- value_filter_desa_kel_kb()
      filter_bulan <- value_filter_bulan_kb()
      # Data contoh
      incProgress(1/10, detail = "Mengambil Wilayah")
      
      donut_faskes = sdm_kb |>
        fsubset(
          Kabupaten %in% filter_kabupaten &
          Kecamatan %in% filter_kecamatan &
          `Kelurahan/Desa` %in% filter_desa &
          BULAN %in% filter_bulan
        ) 
      incProgress(2/10, detail = "Filter Wilayah")
      
      terlatih_tidak <- sdm_kb |>
        fsubset(
          Kabupaten %in% filter_kabupaten &
          Kecamatan %in% filter_kecamatan &
          `Kelurahan/Desa` %in% filter_desa &
          BULAN %in% filter_bulan
        ) |>
        fselect(Pelatihan) |>
        fmutate(status = ifelse(grepl("IUD|Tubektomi|Vasektomi", Pelatihan), "Sudah Terlatih", "Belum Terlatih"))
      incProgress(3/10, detail = "Merekap Data")
      
      terlatih_tidak <- table(terlatih_tidak$status)
      
      terlatih_tidak <- as.data.frame(terlatih_tidak)
      
      # Memberi nama kolom
      names(terlatih_tidak) <- c("Kategori", "Jumlah")
      
      plot_terlatih_tidak <- terlatih_tidak |> 
        plot_ly(labels = ~Kategori, 
                values = ~Jumlah, 
                textinfo='label+percent+value',
                marker = list(colors = c("#ffc107", "#0d6efd"),
                              line = list(color = '#000', width = 1))
        )|> 
        add_pie(hole = 0.4)|> 
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
  
  ##batas vb kb
  
  ## batas kb judul
  # batas kb
  
  # krs
  # krs input
  observeEvent(input$pilih_kab_krs, {
    if (input$pilih_kab_krs == "SEMUA KABUPATEN") {
      updateSelectInput(session, "pilih_kec_krs",
                        choices = c("SEMUA KECAMATAN"))
    } else {
      daftar_kecamatan = data_poktan |>
        fselect(KABUPATEN, KECAMATAN) |>
        fsubset(KABUPATEN == input$pilih_kab_krs) |>
        fselect(KECAMATAN)
      daftar_kecamatan = daftar_kecamatan$KECAMATAN
      updateSelectInput(session, "pilih_kec_krs",
                        choices = c("SEMUA KECAMATAN",
                                    daftar_kecamatan))
    }
  })
  
  observeEvent(input$pilih_kec_krs, {
    if (input$pilih_kec_krs == "SEMUA KECAMATAN") {
      updateSelectInput(session, "pilih_desa_kel_krs",
                        choices = c("SEMUA DESA/KEL"))
    } else {
      daftar_kel = data_poktan |>
        fselect(KECAMATAN, KELURAHAN) |>
        fsubset(KECAMATAN == input$pilih_kec_krs) |>
        fselect(KELURAHAN)
      daftar_kel = daftar_kel$KELURAHAN
      updateSelectInput(session, "pilih_desa_kel_krs",
                        choices = c("SEMUA DESA/KEL", 
                                    daftar_kel))
    }
  })
  
  ## batas krs input
  
  ## krs filter
  # filter kab
  value_filter_kab_krs <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_krs, {
    kondisi_input = input$pilih_kab_krs
    if (kondisi_input == "SEMUA KABUPATEN"){
      filter_kabupaten = unique(data_poktan$KABUPATEN)
    } else{
      filter_kabupaten = input$pilih_kab_krs
    }
    value_filter_kab_krs(filter_kabupaten) 
  })
  
  #kecamatan
  value_filter_kec_krs <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_krs, {
    kondisi_input = input$pilih_kec_krs
    filter_kabupaten = value_filter_kab_krs()
    
    if (kondisi_input == "SEMUA KECAMATAN"){
      daftar_kecamatan = data_poktan |>
        fselect(KABUPATEN, KECAMATAN) |>
        fsubset(KABUPATEN %in% filter_kabupaten) |>
        fselect(KECAMATAN)
      filter_kecamatan = daftar_kecamatan$KECAMATAN
    } else{
      filter_kecamatan = input$pilih_kec_krs
    }
    value_filter_kec_krs(filter_kecamatan) 
  })
  
  # desa
  value_filter_desa_kel_krs <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_krs, {
    kondisi_input = input$pilih_desa_kel_krs
    filter_kabupaten = value_filter_kab_krs()
    filter_kecamatan = value_filter_kec_krs()
    
    if (kondisi_input == "SEMUA DESA/KEL"){
      daftar_kel = data_poktan |>
        select(KABUPATEN, KECAMATAN, KELURAHAN) |>
        fsubset(
          KABUPATEN %in% filter_kabupaten) |>
        fsubset(KECAMATAN %in% filter_kecamatan) |>
        select(KELURAHAN)
      filter_desa_kel_krs = daftar_kel$KELURAHAN
    } else{
      filter_desa_kel_krs = input$pilih_desa_kel_krs
    }
    value_filter_desa_kel_krs(filter_desa_kel_krs) 
  })
  
  # bulan
  value_filter_bulan_krs <- reactiveVal(0)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$cari_krs, {
    value_filter_bulan_krs(input$pilih_bulan_krs) 
  })
  ## batas krs filter
  
  ## krs judul
  values_judul_krs <- reactiveValues(default = 0)
  
  observeEvent(input$cari_krs,{
    values_judul_krs$default <- input$cari_krs
  })
  
  teks_judul_rekap_krs <- eventReactive(input$cari_krs, {
    if(input$pilih_kab_krs == "SEMUA KABUPATEN"){
      nama_daerah = "SULAWESI BARAT"
      tingkat_daerah = "PROVINSI"
    } else if(input$pilih_kec_krs == "SEMUA KECAMATAN"){
      nama_daerah = input$pilih_kab_krs
      tingkat_daerah = "KABUPATEN"
    } else if(input$pilih_desa_kel_krs == "SEMUA DESA/KEL"){
      nama_daerah = input$pilih_kec_krs
      tingkat_daerah = "KECAMATAN"
    } else{
      nama_daerah = value_filter_desa_kel_krs()
      tingkat_daerah = "DESA/KELURAHAN"
    }
    teks <- paste(tingkat_daerah, nama_daerah, "-", input$pilih_bulan_krs)
    # if(tingkat_daerah == "KELURAHAN"){
    #   teks <- paste0("PROFIL DESA/", tingkat_daerah, " ", nama_daerah, " - ", input$bulan_rekap)
    # } else{
    #   teks <- paste("PROFIL", tingkat_daerah, nama_daerah, "-", input$bulan_rekap)
    # }
  })

  
  output$tes_input_rekap_krs <- renderText({
    if(values_judul_krs$default == 0){
      teks = "Klik Cari Untuk Menampilkan Halaman"
    }
    else{
      teks = teks_judul_rekap_krs()
    }
  })
  
  # nama_pkb <- fread("data/nama pkb.csv")
  # write.fst(nama_pkb, "data/data_pkb.fst")
  nama_pkb <- read_fst("data/data_pkb.fst")
  
  # nama_tpk <- fread("data/nama_tpk.csv")
  # write.fst(nama_tpk, "data/data_nama_tpk.fst")
  nama_tpk <- read_fst("data/data_nama_tpk.fst")
  output$jumlah_tpk_rekap <- renderUI({
    req(input$cari_krs)
    filter_kabupaten <- value_filter_kab_krs()
    filter_kecamatan <- value_filter_kec_krs()
    filter_desa <- value_filter_desa_kel_krs()
    filter_bulan <- value_filter_bulan_krs()
    
    nama_pkb <- nama_pkb |>
      fsubset(Kecamatan %in% filter_kecamatan & Kelurahan %in% filter_desa)
    
    nama_tpk <- nama_tpk |>
      fsubset(Kecamatan %in% filter_kecamatan & Kelurahan %in% filter_desa)
    
    if(length(filter_desa) > 1){
      nama_pkb <- nama_pkb |>
        fsummarise(n=length(unique(paste(Kecamatan, `Nama PKB`))))
      
      nama_tpk <- nama_tpk |>
        fsummarise(n=length(unique(Register)))
      
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
      jumlah_tpk <- nama_tpk |>
        fsummarise(n=length(unique(Register)))
      
      nama_kader_pkk <- nama_tpk |>
        fsubset(status == "PKK")
      nama_kader_pkk <- paste(nama_kader_pkk$`Nama Anggota`[1:nrow(nama_kader_pkk)], collapse = " & ")
      
      nama_bidan <- nama_tpk |>
        fsubset(status == "Bidan")
      nama_bidan <- paste(nama_bidan$`Nama Anggota`[1:nrow(nama_bidan)], collapse = " & ") 
      
      nama_kader_kb <- nama_tpk |>
        fsubset(status == "Kader KB")
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
  
  verval_krs <- read_fst("data/data_monitoring_krs.fst")
  output$pie_hamil_verval <- renderPlotly({
    req(input$cari_krs)
    filter_kabupaten <- value_filter_kab_krs()
    filter_kecamatan <- value_filter_kec_krs()
    filter_desa <- value_filter_desa_kel_krs()
    
    data_pus_hamil_verval = verval_krs |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa
      ) |>
      fselect(10:11)
    
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
            marker = list(colors = warna)) |>
      add_annotations(text = persentase_terdampingi,
                      x = 0.5, y = 0.5, 
                      font = list(color = "#404040", size = 30),
                      showarrow = FALSE) |>
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
    req(input$cari_krs)
    filter_kabupaten <- value_filter_kab_krs()
    filter_kecamatan <- value_filter_kec_krs()
    filter_desa <- value_filter_desa_kel_krs()
    
    data_pus_hamil_verval = verval_krs |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa
      ) |>
      fselect(10:11)
    
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
    req(input$cari_krs)
    filter_kabupaten <- value_filter_kab_krs()
    filter_kecamatan <- value_filter_kec_krs()
    filter_desa <- value_filter_desa_kel_krs()
    
    data_catin_verval = verval_krs |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa
      ) |>
      fselect(23:24)
    
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
            marker = list(colors = warna)) |>
      add_annotations(text = persentase_terdampingi,
                      x = 0.5, y = 0.5, 
                      font = list(color = "#404040", size = 30),
                      showarrow = FALSE) |>
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
    req(input$cari_krs)
    filter_kabupaten <- value_filter_kab_krs()
    filter_kecamatan <- value_filter_kec_krs()
    filter_desa <- value_filter_desa_kel_krs()
    warna <- c("#0d6efd", "#404040")
    data_catin_verval = verval_krs |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa
      ) |>
      fselect(23:24)
    
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
    req(input$cari_krs)
    filter_kabupaten <- value_filter_kab_krs()
    filter_kecamatan <- value_filter_kec_krs()
    filter_desa <- value_filter_desa_kel_krs()
    
    data_baduta_verval = verval_krs |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa
      ) |>
      fselect(12:14)
    
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
            marker = list(colors = warna)) |>
      add_annotations(text = persentase_terdampingi,
                      x = 0.5, y = 0.5, 
                      font = list(color = "#404040", size = 30),
                      showarrow = FALSE) |>
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
    req(input$cari_krs)
    filter_kabupaten <- value_filter_kab_krs()
    filter_kecamatan <- value_filter_kec_krs()
    filter_desa <- value_filter_desa_kel_krs()
    
    data_baduta_verval = verval_krs |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa
      ) |>
      fselect(13:15)
    
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
      req(input$cari_krs)
      filter_kabupaten <- value_filter_kab_krs()
      filter_kecamatan <- value_filter_kec_krs()
      filter_desa <- value_filter_desa_kel_krs()
      incProgress(1/10, detail = "Mengambil Wilayah")
      
      
      data_balita_verval = verval_krs |>
        fsubset(
          Kabupaten %in% filter_kabupaten &
          Kecamatan %in% filter_kecamatan &
          `Kelurahan/Desa` %in% filter_desa
        ) |>
        fselect(15:17)
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
                                    marker = list(colors = warna)) |>
        add_annotations(text = persentase_terdampingi,
                        x = 0.5, y = 0.5, 
                        font = list(color = "#404040", size = 30),
                        showarrow = FALSE) |>
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
    
    data_balita_verval = verval_krs |>
      fsubset(
        Kabupaten %in% filter_kabupaten &
        Kecamatan %in% filter_kecamatan &
        `Kelurahan/Desa` %in% filter_desa
      ) |>
      fselect(15:17)
    
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
  
  faktor_krs <- read_fst("data/data_faktor_krs.fst")
  output$bar_faktor_resiko_jumlah <- renderPlotly({
    req(input$cari_krs)
    filter_kecamatan <- value_filter_kec_krs()
    filter_desa <- value_filter_desa_kel_krs()
    
    data_faktor_resiko <- faktor_krs |>
      fsubset(KECAMATAN %in% filter_kecamatan & KELURAHAN %in% filter_desa) |>
      fgroup_by(PROVINSI) |>
      fsummarise(across(is.numeric, fsum))
    
    
    data_faktor_resiko <- data_faktor_resiko |>
      fmutate(TUA = fsum(`TERLALU TUA (UMUR ISTRI 35-40 TAHUN)`),
             MUDA = fsum(`TERLALU MUDA (UMUR ISTRI < 20 TAHUN)`),
             BANYAK = fsum(`TERLALU BANYAK (≥ 3 ANAK)`),
             DEKAT = fsum(`TERLALU DEKAT (< 2 TAHUN)`),
             SANITASI = fsum(`KELUARGA TIDAK MEMPUNYAI JAMBAN YANG LAYAK`),
             AIR_MINUM = fsum(`KELUARGA TIDAK MEMPUNYAI SUMBER AIR MINUM UTAMA YANG LAYAK`),
             KB_MODERN = fsum(`BUKAN PESERTA KB MODERN`)
      ) |> fselect(TUA, MUDA, BANYAK, DEKAT, 
                   SANITASI, AIR_MINUM, KB_MODERN)
    
    
    
    colnames(data_faktor_resiko) <- c("TERLALU TUA",
                                      "TERLALU MUDA", "TERLALU BANYAK", "TERLALU DEKAT",
                                      "JAMBAN", "SUMBER AIR", "KB MODERN")
    
    data_faktor_resiko <- 
      data_faktor_resiko |>
      pivot(values = c(1:7), how = "l", names = list("FAKTOR", "JUMLAH"))
    
    # Mengatur urutan kolom
    data_faktor_resiko$FAKTOR <- factor(data_faktor_resiko$FAKTOR, 
                                        levels = c("JAMBAN", "SUMBER AIR", "KB MODERN", "TERLALU TUA", 
                                                   "TERLALU MUDA", "TERLALU BANYAK", "TERLALU DEKAT"))
    data_faktor_resiko <- data_faktor_resiko |>
      fmutate(TEKS = scales::comma(JUMLAH, 
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
    baru_faktor_resiko <- baru_faktor_resiko |>
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
    baru_faktor_resiko |>
      layout(
        bargap = 0.4
      ) |>
      config(displayModeBar = FALSE) |>
      config(displaylogo = FALSE) |>
      config(scrollZoom = FALSE) |>
      config(edits = list(editType = "plotly"))
    
  })
  
  output$bar_faktor_resiko_persen <- renderPlotly({
    req(input$cari_krs)
    filter_kecamatan <- value_filter_kec_krs()
    filter_desa <- value_filter_desa_kel_krs()
    
    data_faktor_resiko <- faktor_krs |>
      fsubset(KECAMATAN %in% filter_kecamatan & KELURAHAN %in% filter_desa) |>
      fgroup_by(PROVINSI) |>
      fsummarise(across(is.numeric, fsum))
    
    
    data_faktor_resiko <- data_faktor_resiko |>
      fmutate(PERSEN_TUA = round(`TERLALU TUA (UMUR ISTRI 35-40 TAHUN)`/TOTAL * 100, 2),
             PERSEN_MUDA = round(`TERLALU MUDA (UMUR ISTRI < 20 TAHUN)`/TOTAL  * 100, 2),
             PERSEN_BANYAK = round(`TERLALU BANYAK (≥ 3 ANAK)`/TOTAL * 100, 2),
             PERSEN_DEKAT = round(`TERLALU DEKAT (< 2 TAHUN)`/TOTAL * 100, 2),
             PERSEN_SANITASI = round(`KELUARGA TIDAK MEMPUNYAI JAMBAN YANG LAYAK`/TOTAL  * 100, 2),
             PERSEN_AIR_MINUM = round(`KELUARGA TIDAK MEMPUNYAI SUMBER AIR MINUM UTAMA YANG LAYAK`/TOTAL * 100, 2),
             PERSEN_KB_MODERN = round(`BUKAN PESERTA KB MODERN`/TOTAL * 100, 2)
      ) |> fselect(PERSEN_TUA, PERSEN_MUDA, PERSEN_BANYAK, PERSEN_DEKAT, 
                   PERSEN_SANITASI, PERSEN_AIR_MINUM, PERSEN_KB_MODERN)
    
    
    
    colnames(data_faktor_resiko) <- c("TERLALU TUA",
                                      "TERLALU MUDA", "TERLALU BANYAK", "TERLALU DEKAT",
                                      "JAMBAN", "SUMBER AIR", "KB MODERN")
    
    data_faktor_resiko <- 
      data_faktor_resiko |>
      pivot(values = c(1:7), how = "l", names = list("FAKTOR", "PERSENTASE"))
    
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
    baru_faktor_resiko <- baru_faktor_resiko |>
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
    baru_faktor_resiko |>
      layout(
        bargap = 0.4
      ) |>
      config(displayModeBar = FALSE) |>
      config(displaylogo = FALSE) |>
      config(scrollZoom = FALSE) |>
      config(edits = list(editType = "plotly"))
    
  })
  
  keberadaan_posyandu <- read_fst("data/data_keberadaan_posyandu.fst")
  output$jumlah_posyandu <- renderUI({
    req(input$cari_krs)
    filter_kecamatan <- value_filter_kec_krs()
    filter_desa <- value_filter_desa_kel_krs()
    
    total_posyandu <- keberadaan_posyandu |>
      fsubset(KECAMATAN %in% filter_kecamatan & KELURAHAN %in% filter_desa) |>
      group_by(PROVINSI) |>
      nrow()
    
    value_box(
      title = "Total Posyandu ",
      value = scales::comma(total_posyandu,
                            big.mark = ".",
                            decimal.mark = ","),
      showcase = bs_icon("file-medical-fill"),
      theme = value_box_theme(bg = "#f6f8fa", fg = "#0B538E"),
    )
  })
  ## batas krs judul
  # batas krs
}

shinyApp(ui, server)