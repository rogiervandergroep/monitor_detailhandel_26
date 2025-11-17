productgroepen <- c(
  "dagelijks",
  "modeartikelen",
  "elektronica",
  "huishoudelijk",
  "woninginrichting",
  "doehetzelf",
  "planten",
  "media",
  "sportspel"
)

recreatief <- c(
  'modeartikelen',
  'media',
  'sportspel',
  'huishoudelijk'
)

doelgericht <- c(
  'elektronica',
  'doehetzelf',
  'planten',
  'woninginrichting'
)
# V18_nw_modeartikelen_GV1		0,188
# V18_nw_elektronica_GV1		  0,084
# V18_nw_huishoudelijk_GV1		0,171
# V18_nw_woninginrichting_GV1	0,258
# V18_nw_doehetzelf_GV1	      0,043
# V18_nw_planten_GV1		      0,050
# V18_nw_media_GV1		        0,080
# V18_nw_sportspel_GV1		    0,032

# omzetcijfers <- c(0.188, 0.084, 0.171, 0.258, 0.043, 0.050, 0.080, 0.032)

### volgorde eindtabellenm

monitor_levels = c(
  "monitor 2020",
  "monitor 2022",
  "monitor 2024",
  "monitor 2026"
)

woongebied_code_levels <- c(
  "Amsterdam",
  "A",
  "B",
  "E",
  "F",
  "K",
  "M",
  "N",
  "S",
  "T"
)

woongebied_naam_levels <- c(
  "Amsterdam",
  "Centrum",
  "Westpoort",
  "West",
  "Nieuw-West",
  "Zuid",
  "Oost",
  "Noord",
  "Weesp",
  "Zuidoost"
)

wink_stadsdeel_code_levels <- c(
  "A",
  "B",
  "E",
  "F",
  "K",
  "M",
  "N",
  "S",
  "T",
  "MRA",
  "Buiten MRA",
  "online",
  "onbekend"
)


woon_ggw_code_levels <- c(
  "GA01",
  "GA02",
  "GE03",
  "GE04",
  "GE05",
  "GF06",
  "GF07",
  "GF08",
  "GF09",
  "GF10",
  "GK11",
  "GK12",
  "GK13",
  "GM14",
  "GM15",
  "GM16",
  "GM17",
  "GN18",
  "GN19",
  "GN20",
  "GT21",
  "GT22",
  "GT23",
  "GT24",
  "GS25"
)


wink_stadsdeel_naam_levels <- c(
  "Centrum",
  "Westpoort",
  "West",
  "Nieuw-West",
  "Zuid",
  "Oost",
  "Noord",
  "Weesp",
  "Zuidoost",
  "MRA",
  "Buiten MRA",
  "online",
  "onbekend"
)


wink_ggw_code_levels <- c(
  "GA01",
  "GA02",
  "B",
  "GE03",
  "GE04",
  "GE05",
  "GF06",
  "GF07",
  "GF08",
  "GF09",
  "GF10",
  "GK11",
  "GK12",
  "GK13",
  "GM14",
  "GM15",
  "GM16",
  "GM17",
  "GN18",
  "GN19",
  "GN20",
  "GT21",
  "GT22",
  "GT23",
  "GT24",
  "GS25",
  "MRA",
  "Buiten MRA",
  "online",
  "onbekend"
)

hoofdgroep_levels <- c("dagelijks", "niet-dagelijks")

subgroep_levels <- c("dagelijks", "doelgericht", "recreatief")

productgroep_levels <- productgroepen

### rapportcijfers voor beoordeing winkelgebeiden

rap <- list()

rap[["20_22_v7"]] <- c(
  "v6",
  "v7_het_uiterlijk_va_gv1",
  "v7_de_aankleding_en_gv1",
  "v7_de_sfeer_en_de_g_gv1",
  "v7_service_en_bedie_gv1",
  "v7_de_keuze_mogelij_gv1",
  "v7_de_keuze_mogeli6_gv1",
  "v7_het_algemeen_pri_gv1",
  "v7_veiligheid_gv1",
  "v7_parkerenfiets_gv1",
  "v7_parkerenauto_gv1",
  "v7_aanbod_daghoreca_gv1",
  "v7_bereikbaar_gv1"
)

# deze rapportcijfers verwijzen naar v20
# (winkelgebieden voor niet-dagelijkse boodschappen)

rap[["20_22_v22"]] <- c(
  "v21",
  "v22_het_uiterlijk_va_gv1",
  "v22_de_aankleding_en_gv1",
  "v22_de_sfeer_en_de_g_gv1",
  "v22_service_en_bedie_gv1",
  "v22_de_keuze_mogelij_gv1",
  "v22_de_keuze_mogeli6_gv1",
  "v22_het_algemeen_pri_gv1",
  "v22_veiligheid_gv1",
  "v22_parkerenfiets_gv1",
  "v22_parkerenauto_gv1",
  "v22_aanbod_daghoreca_gv1",
  "v22_bereikbaar_gv1"
)

# rapportcijfers 2024 en 2026
rap[["24_26_v7"]] <- c(
  "v6",
  "v7_nw_het_uiterlijk_va_gv1",
  "v7_nw_de_aankleding_en_gv1",
  "v7_nw_de_sfeer_en_de_g_gv1",
  "v7_nw_de_keuze_mogelij_gv1",
  "v7_nw_bio_aanbod_gv1",
  "v7_nw_de_keuze_mogeli6_gv1",
  "v7_nw_duurzaam_aanbod_gv1",
  "v7_nw_het_algemeen_pri_gv1",
  "v7_nw_veiligheid_gv1",
  "v7_nw_veiligheid_avond_gv1",
  "v7_nw_schoonhouden_gv1",
  "v7_nw_overlast_gv1",
  "v7_nw_overlast_vuil_gv1",
  "v7_nw_overlast_hor_gv1",
  "v7_nw_parkerenfiets_gv1",
  "v7_nw_parkerenauto_gv1",
  "v7_nw_aanbod_daghoreca_gv1",
  "v7_nw_bereikbaar_gv1"
)

rap[["24_26_v22"]] <- c(
  "v21",
  "v22_nw_het_uiterlijk_va_gv1",
  "v22_nw_de_aankleding_en_gv1",
  "v22_nw_de_sfeer_en_de_g_gv1",
  "v22_nw_de_keuze_mogelij_gv1",
  "v22_nw_bio_gv1",
  "v22_nw_de_keuze_mogeli6_gv1",
  "v22_nw_duurzame_gv1",
  "v22_nw_het_algemeen_pri_gv1",
  "v22_nw_veiligheid_gv1",
  "v22_nw_veiligheid_avond_gv1",
  "v22_nw_schoonhouden_gv1",
  "v22_nw_overlast_gv1",
  "v22_nw_overlast_vuil_gv1",
  "v22_nw_overlast_horeca_gv1",
  "v22_nw_parkerenfiets_gv1",
  "v22_nw_parkerenauto_gv1",
  "v22_nw_aanbod_daghoreca_gv1",
  "v22_nw_bereikbaar_gv1"
)

readr::write_rds(rap, "01 references/vragen_rapportcijfers.rds")
