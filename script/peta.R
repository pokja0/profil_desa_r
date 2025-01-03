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

batas_sulbar <- batas_sulbar
kecamatan <- c("POLEWALI", "PASANGKAYU")
desa_kel <- c("POLEWALI", "PASANGKAYU")

kecamatan <- value_filter_kec()

desa_kel <- value_filter_desa_kel()
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

library(dplyr)

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
