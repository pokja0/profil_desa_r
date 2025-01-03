kelompok_umur_lk <- read_fst("data/data_piramida_lk.fst")
kelompok_umur_pr <- read_fst("data/data_piramida_perempuan.fst")

  kecamatan <- value_filter_kec()
  desa_kel <- value_filter_desa_kel()
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
  
  piramida_interaktif <- piramida_interaktif %>%
    layout(title = "Grafik Piramida Penduduk",
           xaxis = list(title = "Jumlah Penduduk", tickangle=0,
                        tickvals = round(seq(-batas_angka, batas_angka, length.out = jumlah_angka)), 
                        ticktext = round(abs(seq(-batas_angka, batas_angka, length.out = jumlah_angka)))
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
  piramida_interaktif %>%
    add_annotations(
      text = "Perempuan", x = -batas_angka, y = 15,
      showarrow = FALSE, font = list(color = "#000", size = 14), xref = "x", yref = "y"
    ) %>%
    add_annotations(
      text = "Laki-Laki", x = batas_angka, y = 15,
      showarrow = FALSE, font = list(color = "#000", size = 14), xref = "x", yref = "y"
    )
  
