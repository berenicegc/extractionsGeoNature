
#' @title Import des fichiers nécessaires
#'
#' @description La fonction import permet d’importer les fichiers nécessaires à l’extraction des données contenues dans les champs additionnels du fichier GéoNature. Deux fichiers sont nécessaires : la synthèse téléchargée sur la plateforme GéoNature et le fichier TaxRef.
#'
#' @details
#' Marche à suivre :
#' - Télécharger la synthèse en .csv sur GeoNature
#' - Mettre le csv dans le même dossier que le script R utilisé ou spécifier le dossier dans lequel les fichiers sont présents dans l’argument 'path', le chemin devant être sous la forme "~/Downloads» (non recommandé car peut être source d’erreurs). Dans tous les cas, le fichier synthese_observations de GeoNature et TaxRef doivent être dans le même dossier.
#'
#' Par défaut, si l’argument 'path' n’est pas précisé, les fichiers seront cherchés dans le même dossier que le script R.
#'
#' Si plusieurs synthèses GeoNature sont présentes dans le même dossier, c’est le fichier le plus récent (téléchargé en dernier) qui sera importé, pour éviter l’ouverture de tous les fichiers en simultané et donc potentiellement la présence de doublons.
#'
#' - Préciser le nom des fichiers s’ils ont été changés avec les arguments 'geonature' et 'taxref'. Si les noms des fichiers n’ont pas été modifiés il n’est pas nécessaire de préciser ces arguments et les noms par défaut des fichiers téléchargés seront pris en compte. Attention, les noms de fichiers ne doivent pas contenir d'accents ou de caractères spéciaux. Le nom des fichiers peut être écrit avec ou sans l’extension (.csv), toutefois les fichiers doivent obligatoirement être des .csv (et non des .xls ou .xlsx). Si l’import ne marche pas, relancer la fonction peut fonctionner.
#'
#' @param path Chemin d'accès aux fichiers
#' @param geonature Nom du fichier de la synthèse téléchargée depuis GéoNature
#' @param TaxRef Nom du fichier TaxRef
#'
#' @importFrom cli cli_alert_danger
#' @return La fonction importe les jeux de données
#' @export
#'
#' @example Import par défaut (fichiers non renommés et présents dans le même dossier que le script R)
#' import()
#'
#' @example Import avec modification du chemin d'accès
#' import(path = "~/Documents")
#' import(path = "~/Downloads")
#'
#' @example Import avec modification des noms de fichiers (les fichiers doivent être des .csv)
#' import(geonature = "BDD_ABAURA.csv", TaxRef = "TaxRefv16.csv")

import <- function(path = ".", geonature = "synthese_observations", TaxRef = "taxref"){
  directory <- setwd(dir = path)
  synthese_observations <<- read_delim(list.files(directory, pattern = geonature, full.names = TRUE)
                                       [which.max(file.mtime(list.files(directory, pattern = geonature, full.names = TRUE)))], delim = ";")
  taxref <<- read_delim(list.files(directory, pattern = TaxRef), delim = ",")

  #Contrôle import données GéoNature
  if(length(synthese_observations)==0){
    cli_alert_danger(col_red("Erreur : données GéoNature non importées. Veuillez relancer la fonction import().
                                    Si le problème persiste, vérifiez le nom du fichier (argument geonature) ainsi que le chemin (argument path)."), wrap = TRUE)}

  #Contrôle import référentiel taxonomique
  if(length(taxref)==0){
    cli_alert_danger(col_red("Erreur : référentiel taxonomique TaxRef non importé. Veuillez relancer la fonction import().
                                    Si le problème persiste, vérifiez le nom du fichier (argument TaxRef) ainsi que le chemin (argument path)."), wrap = TRUE)}
}

#' @title Extraction des colonnes d'intérêt
#'
#' @description La fonction 'extract' permet d’extraire les colonnes d’intérêt, contenues dans les champs additionnels de la synthèse GéoNature. Les colonnes peuvent être : plante, caste, station, année de détermination et méthode de capture. Un tri des données de plantes pourra également être réalisé dans la fonction.
#'
#' @details
#' Marche à suivre :
#' - Sélectionner les colonnes à extraire (plante, caste, station, annee_determination, methode) à l’aide de l’argument 'col.' Il est possible de ne sélectionner qu’une colonne ou plusieurs, l’ordre n’ayant pas d’importance.
#'
#' Si une seule colonne est sélectionnée, la fonction s’écrit sous la forme extract(col = "plante").
#'
#' Dans le cas où plusieurs colonnes sont sélectionnées, la fonction prend la forme extract(col = c("plante", "caste", "annee_determination")) par exemple, c() permettant de combiner des valeurs.
#'
#' Par défaut, si l’argument "col" n’est pas précisé, toutes les colonnes sont sélectionnées.
#'
#' - Sélectionner le degré de précision taxonomique choisie ("famille", "genre" ou "sp") à l’aide de l’argument "precision_taxo". Si un rang taxonomique plus grossier est sélectionné (famille ou genre), les rangs plus précis seront conservés. Par exemple, si on autorise de conserver une précision allant jusqu’à la famille, les observations allant jusqu’au genre ou à l’espèce seront conservées.
#'
#' Par défaut, toutes les observations sont conservées et aucun tri n’est effectué.
#'
#' La fonction extract peut simplement s’écrire extract(), dans ce cas toutes les colonnes utiles des champs additionnels seront extraites et toutes les données de plantes seront gardées (même les observations NA ou les données ne renvoyant pas à un cd_nom dans TaxRef).
#'
#' Il faut compter environ 2 minutes d’attente pour l’export de toutes les colonnes et pour une base de données d’environ 100 000 observations
#'
#' @param col Colonnes à extraire des champs additionnels. Les colonnes peuvent être : "plante", "caste", "station", "annee_determination" et "methode".
#' @param precision_taxo Précision taxonomique souhaitée pour la colonne plante ("famille", "genre" ou "sp").
#'
#' @return La synthèse GéoNature complétée des colonnes sélectionnées
#' @export
#'
#' @example Extraction par défaut (toutes les colonnes des champs additionnels et sans mise en place d'un filtre sur la précision taxonomique)
#' extract()
#'
#' @example Extraction avec sélection de colonnes
#' extract(col = "plante")
#' extract(col = c("plante", "caste", "station", "annee_determination", "methode"))
#'
#' @example Extraction avec filtre par précision taxonomique (la colonne 'plante' doit obligatoirement être sélectionnée pour préciser cet argument)
#' extract(col = "plante", precision_taxo = "sp")


extract <- function(col = c("plante", "caste", "station", "annee_determination", "methode"), precision_taxo = NULL){

  export_final_GeoNature <<- synthese_observations

  # Export caste
  if(length(which(str_detect("caste", col) == TRUE))==1){
    caste <- synthese_observations %>%
      mutate(caste = str_match(champs_additionnels, "'caste': '(.*?)', '")[,2]) %>%
      select(caste)

    export_final_GeoNature <<- export_final_GeoNature %>%
      cbind(caste)

  }

  # Export station
  if(length(which(str_detect("station", col) == TRUE))==1){
    station <- synthese_observations %>%
      mutate(station = str_match(champs_additionnels, "'station': '(.*?)', '")[,2]) %>%
      select(station)

    export_final_GeoNature <<- export_final_GeoNature %>%
      cbind(station)

  }

  # Export année de détermination
  if(length(which(str_detect("annee_determination", col) == TRUE))==1){
    annee <- synthese_observations %>%
      mutate(annee_determination = str_match(champs_additionnels, "'annee_determination': (.*?), '")[,2],
             annee_determination = na_if(annee_determination, "None")) %>%
      select(annee_determination)

    export_final_GeoNature <<- export_final_GeoNature %>%
      cbind(annee)
  }

  # Export méthode de capture
  if(length(which(str_detect("methode", col) == TRUE))==1){
    methode <- synthese_observations %>%
      group_by(id_synthese) %>%
      mutate(meth_collecte = str_match(champs_additionnels, "'meth_collecte': '(.*?)', '")[,2],
             type_piegeage = str_match(champs_additionnels, "'type_piegeage': '(.*?)', '")[,2],
             trapping_type = str_match(champs_additionnels, "'trapping_type': '(.*?)', '")[,2],
             methode_capture = if_else(!str_detect(meth_collecte, "Piégeage"), meth_collecte,
                                       if_else({str_detect(meth_collecte, "Piégeage") & is.na(trapping_type)} |
                                                 {str_detect(type_piegeage, "Coupelle") & type_piegeage==trapping_type}, type_piegeage, NA))) %>%
      ungroup() %>%
      select(methode_capture)

    export_final_GeoNature <<- export_final_GeoNature %>%
      cbind(methode)
  }

  # Export plantes
  if(length(which(str_detect("plante", col) == TRUE))==1){
    #Extraction variable plante
    synthese_observations %>%
      mutate(plante_sp = str_match(champs_additionnels, "'plante': '(.*?)', '")[,2],
             lb_nom_plante = str_match(champs_additionnels, "'lb_nom': '(.*?)', '")[,2],
             plante_sp = as.character(plante_sp),
             lb_nom_plante = as.character(lb_nom_plante),
             plante_sp = paste0(plante_sp, lb_nom_plante),
             plante_sp = Hmisc::capitalize(plante_sp),
             plante_sp = str_remove(plante_sp, "NA|N. arvernus|Problematicus ssp. Arvernus|Ssp. Arvenus|\\s\\(S\\)|\\s\\(PF\\)|\\sL."),
             plante_sp = trimws(plante_sp, which = c("both")),
             plante_sp = case_when(plante_sp == "Reine des" ~ "Filipendula ulmaria",
                                   plante_sp == "Pavot orange" ~ "Eschscholzia californica",
                                   plante_sp == "Stachiys palustris" ~ "Stachys palustris",
                                   plante_sp == "Cirsium arvensis" ~ "Cirsium arvense",
                                   plante_sp == "Erigeron anuus" ~ "Erigeron annuus",
                                   plante_sp == "Rubus fructicosus" ~ "Rubus fruticosus",
                                   plante_sp == "Prunus 'accolade'" ~ "Prunus serrulata",
                                   plante_sp == "Salvia memorosa" ~ "Salvia nemorosa",
                                   plante_sp == "Perovskia atripicifolia" ~ "Perovskia atriplicifolia",
                                   plante_sp == "Chelidonia majus" ~ "Chelidonium majus",
                                   plante_sp == "Helianthemum apeninnum" ~ "Helianthemum apenninum",
                                   plante_sp == "Reseda luthea" ~ "Reseda lutea",
                                   plante_sp == "Melilotus alba" ~ "Melilotus albus",
                                   plante_sp == "Bellis perenis" ~ "Bellis perennis",
                                   plante_sp == "Buddleia Davidii" ~ "Buddleja davidii",
                                   plante_sp == "Epilobium angustifiolia" ~ "Epilobium angustifolium",
                                   plante_sp == "Trifolium pratens" ~ "Trifolium pratense",
                                   plante_sp == "Aster novi" ~ "Symphyotrichum novi-belgii",
                                   plante_sp == "Ribes nignum" ~ "Ribes nigrum",
                                   plante_sp == "Bryonia dioca" ~ "Bryonia dioica",
                                   plante_sp == "Ajugan repens" ~ "Ajuga reptans",
                                   plante_sp == "Gallega officinalis" ~ "Galega officinalis",
                                   plante_sp == "Buddleia davidii" ~ "Buddleja davidii",
                                   plante_sp == "Jasione mantana" ~ "Jasione montana",
                                   plante_sp == "Rubus ideaus" ~ "Rubus idaeus",
                                   plante_sp == "Melampodium paludosum" ~ "Melanpodium paludosum",
                                   plante_sp == "Pelargonium zonal" ~ "Pelargonium zonale",
                                   plante_sp == "Escholtzia californica" ~ "Eschscholzia californica",
                                   plante_sp == "Sinapsis arvensis" ~ "Sinapis arvensis",
                                   plante_sp == "Alyssum maritima" ~ "Lobularia maritima",
                                   plante_sp == "Moutarde des" ~ "Sinapis arvensis",
                                   plante_sp == "Choysia ternata" ~ "Choisya ternata",
                                   plante_sp == "Origarum vulgare" ~ "Origanum vulgare",
                                   plante_sp == "Melilotus officinalisCirsium" ~ "Melilotus officinalis",
                                   plante_sp == "Chamomillia vulgaris" ~ "Chamomilla vulgaris",
                                   plante_sp == "Lecanthemum vulgare" ~ "Leucanthemum vulgare",
                                   plante_sp == "Centaurea scabiosae" ~ "Centaurea scabiosa",
                                   plante_sp == "Erigeron annus" ~ "Erigeron annuus",
                                   plante_sp == "Picris hieracioides " ~ "Picris hieracioides",
                                   plante_sp == "Perovskia fructicans" ~ "Perovskia fruticans",
                                   plante_sp == "Senecio inaquidens" ~ "Senecio inaequidens",
                                   plante_sp == "Glechoma hederaceae" ~ "Glechoma hederacea",
                                   plante_sp == "Bonago officinalis" ~ "Borago officinalis",
                                   plante_sp == "Circium arvense" ~ "Cirsium arvense",
                                   plante_sp == "Salvia officinale" ~ "Salvia officinalis",
                                   plante_sp == "Nepata faassenii" ~ "Nepeta x faassenii",
                                   plante_sp == "Allaiaria petiolata" ~ "Alliaria petiolata",
                                   plante_sp == "Ceratostigma plumbaginoïdes" ~ "Ceratostigma plumbaginoides",
                                   plante_sp == "Achilea millefolium" ~ "Achillea millefolium",
                                   plante_sp == "Senecio maritima" ~ "Jacobaea maritima",
                                   plante_sp == "Ocinum basilicum" ~ "Ocimum basilicum",
                                   plante_sp == "Lathyrum pratensis" ~ "Lathyrus pratensis",
                                   plante_sp == "Cephalaria lecantha" ~ "Cephalaria leucantha",
                                   plante_sp == "Phyteuma orbiculareTrifolium" ~ "Phyteuma orbiculare",
                                   plante_sp == "Solidago canavensis" ~ "Solidago canadensis",
                                   plante_sp == "Bellis perrenis" ~ "Bellis perennis",
                                   plante_sp == "Stachys officinialis" ~ "Stachys officinalis",
                                   plante_sp == "Lysimachia memorum" ~ "Lysimachia nemorum",
                                   plante_sp == "Trifolium arvensis" ~ "Trifolium arvense",
                                   plante_sp == "Sedum ochroleucuml" ~ "Sedum ochroleucum",
                                   plante_sp == "Circium vulgare" ~ "Cirsium vulgare",
                                   plante_sp == "Plante2(lotier)" ~ "Lotus sp.",
                                   plante_sp == "Plante4(Verbena sp)" ~ "Verbena sp.",
                                   plante_sp == "Asteraceae jaune" ~ "Asteraceae",
                                   plante_sp == "Plante1(lamiaceae)" ~ "Lamiaceae",
                                   plante_sp == "Lotier rose" ~ "Lotus sp.",
                                   plante_sp == "Pissenlit" ~ "Asteraceae liguliflore",
                                   plante_sp == "moutarde" ~ "Brassicaceae",
                                   plante_sp == "coquelicot" ~ "Papaver rhoeas",
                                   plante_sp == "Coquelicot" ~ "Papaver rhoeas",
                                   plante_sp == "Picris\\xa0?" ~ "Picris sp.",
                                   plante_sp == "Clinopodium nepeta (culture)" ~ "Clinopodium nepeta",
                                   plante_sp == "Rudbéckia" ~ "Rudbeckia sp.",
                                   plante_sp == "Lonicera caerulea (UB14-1)" ~ "Lonicera caerulea",
                                   plante_sp == "Liguliflore capitule velu" ~ "Asteraceae liguliflore",
                                   plante_sp == "R. japonica" ~ "Reynoutria japonica",
                                   plante_sp == "Plante6(seneçon jacob)" ~ "Jacobaea vulgaris",
                                   plante_sp == "Asteracee jaune Seneçon ?" ~ "Asteraceae",
                                   plante_sp == "Roncier" ~ "Rubus sp.",
                                   plante_sp == "Pyracantha\\xa0coccinea" ~ "Pyracantha coccinea",
                                   plante_sp == "Rudbeckia hirta becky jaune" ~ "Rudbeckia hirta",
                                   plante_sp == "Aster novi belgii" ~ "Symphyotrichum novi-belgii",
                                   plante_sp == "Peuplier" ~ "Populus sp.",
                                   plante_sp == "Centauree photos 1333-35" ~ "Centaurea sp.",
                                   plante_sp == "Picris hieracioides\\xa0" ~ "Picris hieracioides",
                                   plante_sp == "Taraxacum gr. Ruderalia" ~ "Taraxacum sect. Ruderalia",
                                   plante_sp == "Pavot orange photo" ~ "Eschscholzia californica",
                                   plante_sp == "Trifolium(rose)" ~ "Trifolium sp.",
                                   plante_sp == "Trifolium alba" ~ "Trifolium sp.",
                                   plante_sp == "Asteracee ligulifore" ~ "Asteraceae liguliflore",
                                   plante_sp == "Fleurs ligulées" ~ "Asteraceae liguliflore",
                                   plante_sp == "liguliflorée" ~ "Asteraceae liguliflore",
                                   plante_sp == "Asteracee jaune photos 1309-1313" ~ "Asteraceae",
                                   plante_sp == "Liseron bleu cultivé" ~ "Convolvulaceae",
                                   plante_sp == "Plante3(Origan)" ~ "Origanum sp.",
                                   plante_sp == "Chamomille" ~ "Asteraceae radiee",
                                   plante_sp == "Asteracee ligulifore jaune" ~ "Asteraceae liguliflore",
                                   plante_sp == "Fleurs ligulées jaune" ~ "Asteraceae liguliflore",
                                   plante_sp == "Camomille" ~ "Asteraceae radiee",
                                   plante_sp == "Plante2(Rubus sp)" ~ "Rubus sp.",
                                   plante_sp == "Asterecée jaune" ~ "Asteraceae",
                                   plante_sp == "Cardons" ~ "Cynara cardunculus",
                                   plante_sp == "Reine des près" ~ "Filipendula ulmaria",
                                   plante_sp == "Chamomillia vulgaris" ~ "Chamomilla vulgaris",
                                   plante_sp == "Coronille jaune" ~ "Coronilla sp.",
                                   plante_sp == "Apiacée plte 1" ~ "Apiaceae",
                                   plante_sp == "Asteracee liguliflorée" ~ "Asteraceae liguliflore",
                                   plante_sp == "Bleuet" ~ "Cyanus sp.",
                                   plante_sp == "Asteracee ligulifore photos" ~ "Asteraceae liguliflore",
                                   plante_sp == "Bambou" ~ "Bambusoideae",
                                   plante_sp == "Saponaire" ~ "Saponaria sp.",
                                   plante_sp == "Basilic (spirale)" ~ "Ocimum sp.",
                                   plante_sp == "Verbena\\xa0officinalis" ~ "Verbena officinalis",
                                   plante_sp == "Coronille" ~ "Coronilla sp.",
                                   plante_sp == "Trefle rose" ~ "Trifolium sp.",
                                   plante_sp == "Plante1(chardon)" ~ "Asteraceae",
                                   plante_sp == "Fleur jaune ligulée" ~ "Asteraceae liguliflore",
                                   plante_sp == "Asterecée jaune fleur 1" ~ "Asteraceae",
                                   plante_sp == "Bourrache (spirale)" ~ "Borago officinalis",
                                   plante_sp == "Menthe (spirale)" ~ "Mentha sp.",
                                   plante_sp == "Coronille rose" ~ "Coronilla sp.",
                                   plante_sp == "Sauge" ~ "Salvia sp.",
                                   plante_sp == "Aster bleue" ~ "Asteraceae",
                                   plante_sp == "Chou" ~ "Brassica sp.",
                                   plante_sp == "Lilas violet" ~ "Syringa vulgaris",
                                   plante_sp == "Marguerite" ~ "Leucanthemum vulgare",
                                   plante_sp == "Moutarde des champs" ~ "Sinapis arvensis",
                                   plante_sp == "SCABIOSAE" ~ "Scabiosa sp.",
                                   plante_sp == "L. salicariae" ~ "Lythrum salicaria",
                                   plante_sp == "Chardons" ~ "Asteraceae",
                                   plante_sp == "Abella x grandiflora" ~ "Abelia x grandiflora",
                                   plante_sp == "Chardon" ~ "Asteraceae",
                                   plante_sp == " Cirsium sp." ~ "Cirsium sp.",
                                   plante_sp == "Vesce ou gesse" ~ "Fabaceae",
                                   plante_sp == "Lila" ~ "Syringa vulgaris",
                                   plante_sp == "Bleuet à double pétales" ~ "Cyanus sp.",
                                   plante_sp == "Fenouil" ~ "Foeniculum vulgare",
                                   plante_sp == "Lavande" ~ "Lavandula angustifolia",
                                   plante_sp == "Fabacée sp." ~ "Fabaceae",
                                   plante_sp == "B. dioica" ~ "Bryonia dioica",
                                   plante_sp == "Cichorium\\xa0intybus" ~ "Cichorium intybus",
                                   plante_sp == "Lavande " ~ "Lavandula angustifolia",
                                   plante_sp == "Liguliflore jaune" ~ "Asteraceae liguliflore",
                                   plante_sp == "Asteracee jaune ligulee" ~ "Asteraceae liguliflore",
                                   plante_sp == "Hysope" ~ "Hyssopus officinalis",
                                   plante_sp == "Genêt" ~ "Fabaceae",
                                   plante_sp == "Centauree photos 1333-35" ~ "Centaurea sp.",
                                   plante_sp == "Plante3(Erigeron)" ~ "Erigeron sp.",
                                   plante_sp == " Lythrum salicaria" ~ "Lythrum salicaria",
                                   plante_sp == "Aster sp. (jaune)" ~ "Asteraceae",
                                   plante_sp == "CIRSIUM" ~ "Cirsium sp.",
                                   plante_sp == "Asteracee jaune" ~ "Asteraceae",
                                   plante_sp == "Asteracée jaune" ~ "Asteraceae",
                                   plante_sp == "Moutarde" ~ "Sinapis sp.",
                                   plante_sp == "Campanule" ~ "Campanula sp.",
                                   plante_sp == "R. cania" ~ "Rosa canina",
                                   plante_sp == "Scabiosae" ~ "Scabiosa sp.",
                                   plante_sp == "Scabieuse" ~ "Scabiosa sp.",
                                   plante_sp == "Trifolum (rose)" ~ "Trifolium sp.",
                                   plante_sp == "Trifolum (blanc)" ~ "Trifolium sp.",
                                   plante_sp == "Gallium sp. jaune" ~ "Galium sp.",
                                   plante_sp == "Erigeon sp." ~ "Erigeron sp.",
                                   plante_sp == "Thymnus sp." ~ "Thymus sp.",
                                   plante_sp == "Hippocastanum" ~ "Aesculus hippocastanum",
                                   plante_sp == "Geraium pyrenaicum" ~ "Geranium pyrenaicum",
                                   plante_sp == "Chrysantemum" ~ "Chrysanthemum sp.",
                                   plante_sp == "Ciscium sp." ~ "Cirsium sp.",
                                   plante_sp == "Cirgium sp." ~ "Cirsium sp.",
                                   plante_sp == "Centauraceae sp." ~ "Centaurea sp.",
                                   plante_sp == "Salvias sp." ~ "Salvia sp.",
                                   plante_sp == "Ranunculis sp." ~ "Ranunculus sp.",
                                   plante_sp == "Menthe" ~ "Mentha sp.",
                                   plante_sp == "Disgitalis sp." ~ "Digitalis sp.",
                                   plante_sp == "Echinacea purpurea magnus" ~ "Echinacea purpurea",
                                   plante_sp == "Epilobe" ~ "Epilobium sp.",
                                   plante_sp == "Trifolium " ~ "Trifolium sp.",
                                   plante_sp == "Acer  " ~ "Acer sp.",
                                   plante_sp == "DIsgitalis sp." ~ "Digitalis sp.",
                                   plante_sp == "Solanum " ~ "Solanum sp.",
                                   plante_sp == "Rubus sp (UB11-1)" ~ "Rubus sp.",
                                   plante_sp == "Gallium sp." ~ "Galium sp.",
                                   plante_sp == "Cirsium " ~ "Cirsium sp.",
                                   plante_sp == "2021.03256Thymus serpyllum" ~ "Thymus serpyllum",
                                   plante_sp == "Taraxacum " ~ "Taraxacum sp.",
                                   plante_sp == "Salvia pratensis var.\\xa0haematodes " ~ "Salvia pratensis",
                                   plante_sp == "Cirsium " ~ "Cirsium sp.",
                                   plante_sp == "Veronica spicataTrifolium" ~ "Veronica spicata",
                                   plante_sp == "Solidago gr. canadensis" ~ "Solidago canadensis",
                                   plante_sp == "Rosa x gallica" ~ "Rosa gallica",
                                   plante_sp == "Gaillardia aristata arizona sun" ~ "Gaillardia aristata",
                                   plante_sp == "Hedera Helix" ~ "Hedera helix",
                                   plante_sp == "Melilotus officinalisCirsium arvense" ~ "Melilotus officinalis",
                                   plante_sp == "Euphorbia diamond frost" ~ "Euphorbia hypericifolia",
                                   plante_sp == "Prunus domestica subsp. Syriaca" ~ "Prunus domestica subsp. syriaca",
                                   plante_sp == "Lavandula hybrida" ~ "Lavandula x hybrida",
                                   plante_sp == "Campanula persicifolia blue" ~ "Campanula persicifolia",
                                   plante_sp == "Rosa rugosa rubra" ~ "Rosa rugosa",
                                   plante_sp == "Solidago gr. Canadensis" ~ "Solidago canadensis",
                                   plante_sp == "Salix x fragilis" ~ "Salix fragilis",
                                   plante_sp == "Abelia X grandiflora" ~ "Abelia x grandiflora",
                                   plante_sp == "Aesculus x carnea" ~ "Aesculus carnea",
                                   plante_sp == "Lotus corniculatus hirsutus" ~ "Lotus corniculatus subsp. hirsutus",
                                   plante_sp == "Abelia grandiflora" ~ "Abelia x grandiflora",
                                   plante_sp == "Lotus corniculatus alpinus" ~ "Lotus corniculatus subsp. alpinus",
                                   plante_sp == "Rosa foetida persiana" ~ "Rosa foetida",
                                   plante_sp == "Campanula persicifolia alba" ~ "Campanula persicifolia",
                                   plante_sp == "Lamium purpurea" ~ "Lamium purpureum",
                                   plante_sp == "Centaurea jacae" ~ "Centaurea jacea",
                                   TRUE ~ as.character(plante_sp)),
             plante_genre = if_else(
               !str_detect(plante_sp, paste0(na.omit(c("\\wSol", "[0-9]",unique(taxref$famille
                                                                                [which(taxref$regne == "Plantae")]))), collapse = "|")),
               paste0(word(plante_sp, start = 1, end = 1)), "NA", missing = NULL),
             plante_sp = str_remove(plante_sp, "NA"),
             plante_sp = na_if(plante_sp, "NA"),
             plante_genre = na_if(plante_genre, "NA")) %>%
      select(-lb_nom_plante) -> synthese_observations_plantes


    # Espèces
    synthese_observations_plantes %>%
      filter(plante_sp %in% taxref$lb_nom[which(taxref$id_rang %in% c("ES", "SMES", "MES", "SSES","NAT", "VAR",
                                                                      "SVAR", "FO", "SSFO", "FOES", "LIN",
                                                                      "CLO", "RACE", "CAR", "MO"))]) %>%
      left_join(taxref %>%
                  select(lb_nom, famille, cd_ref), by=c("plante_sp"="lb_nom"), relationship = "many-to-many") %>%
      rename(plante_famille = famille.y,
             plante_cd_ref = cd_ref.y) -> temp_file_extraction_sp


    suppressMessages(full_join(synthese_observations_plantes, temp_file_extraction_sp)) %>%
      select(-famille.x, -cd_ref.x) %>%
      filter(!duplicated(id_synthese, fromLast = TRUE)) %>%
      arrange(id_synthese) -> export_temp_plante

    # Genres
    synthese_observations_plantes %>%
      filter(!plante_sp %in% c("Penstemon hybride", "Hypericum hookerianum", "Anthemis vulgaris", "Agastache foeniculum", "Agastache aurantiaca",
                               "Nepeta faassenii", "Nepeta x faassenii", "Penstemon garnet galane", "Penstemon blackbird", "Lavandula hybrida",
                               "Zinnia grandiflora", "Fuchsia fulgens", "Erigeron aureus") & ### Pas automatisé
               !plante_sp %in% taxref$lb_nom[which(taxref$id_rang %in% c("ES", "SMES", "MES", "SSES","NAT", "VAR",
                                                                         "SVAR", "FO", "SSFO", "FOES", "LIN",
                                                                         "CLO", "RACE", "CAR", "MO"))] &
               !str_detect(plante_sp,
                           paste0(na.omit(unique(taxref$famille[which(taxref$regne == "Plantae")])), collapse = "|")) &
               plante_genre %in% taxref$lb_nom[which(taxref$id_rang %in% c("GN", "SSGN", "SC", "SBSC", "SER", "SSER", "AGES"))]) %>%

      left_join(taxref %>%
                  select(lb_nom, famille, cd_ref), by=c("plante_genre"="lb_nom"), relationship = "many-to-many") %>%
      rename(plante_famille = famille.y,
             plante_cd_ref = cd_ref.y) -> temp_file_extraction_genre


    suppressMessages(full_join(export_temp_plante, temp_file_extraction_genre)) %>%
      select(-famille.x, -cd_ref.x) %>%
      filter(!duplicated(id_synthese, fromLast = TRUE)) %>%
      arrange(id_synthese) -> export_temp_plante


    # Familles

    synthese_observations_plantes %>%
      filter(str_detect(plante_sp,
                        paste0(na.omit(unique(taxref$famille[which(taxref$regne == "Plantae")])), collapse = "|"))) %>%
      mutate(plante_famille = str_match(
        plante_sp, paste0(na.omit(unique(taxref$famille[which(taxref$regne == "Plantae")])), collapse = "|"))[,1]) %>%
      left_join(taxref %>%
                  select(lb_nom, cd_ref), by=c("plante_famille"="lb_nom"), relationship = "many-to-many") %>%
      rename(plante_cd_ref = cd_ref.y) -> temp_file_extraction_famille


    suppressMessages(full_join(export_temp_plante, temp_file_extraction_famille)) %>%
      select(-cd_ref.x) %>%
      filter(!duplicated(id_synthese, fromLast = TRUE)) %>%
      arrange(id_synthese) -> export_temp_plante


    #Join jeu de données initiales
    plante <- export_temp_plante %>%
      select(plante_sp, plante_genre, plante_famille, plante_cd_ref)

    export_final_GeoNature <<- export_final_GeoNature %>%
      cbind(plante)

}


  # Précision taxonomique
  if(!is.null(precision_taxo)){
    if(length(which(str_detect("plante", col) == TRUE))==1 & str_detect("famille|genre|sp", precision_taxo)){
      if(precision_taxo == "famille"){
        export_final_GeoNature %>%
          filter_at(vars(plante_cd_ref,plante_genre),any_vars(!is.na(.))) ->> export_final_GeoNature
      }

      if(precision_taxo == "genre"){
        export_final_GeoNature %>%
          filter(plante_cd_ref %in% taxref$cd_ref[which(taxref$id_rang %in% c("GN", "SSGN", "SC", "SBSC", "SER", "SSER", "AGES", "ES",
                                                                              "SMES", "MES", "SSES","NAT", "VAR","SVAR", "FO", "SSFO",
                                                                              "FOES", "LIN","CLO", "RACE", "CAR", "MO"))]) ->> export_final_GeoNature
      }

      if(precision_taxo == "sp"){
        export_final_GeoNature %>%
          filter(plante_sp %in% taxref$lb_nom[which(taxref$id_rang %in% c("ES", "SMES", "MES", "SSES","NAT", "VAR",
                                                                          "SVAR", "FO", "SSFO", "FOES", "LIN",
                                                                          "CLO", "RACE", "CAR", "MO"))]) ->> export_final_GeoNature
      }
    }


  else{
    cli_alert_danger(col_red(
      paste0("Colonne 'plante' manquante, impossible d'indiquer la précision taxonomique. Veuillez ajouter la valeur 'plante' dans l'argument 'col'.")),
      wrap = TRUE)
  }
  }
}


#' @title Export du fichier final
#'
#' @description La fonction 'export' permet d’exporter la base de données finale, contenant les variables sélectionnées.
#'
#' @details Par défaut, le fichier est appelé "export_final_GeoNature.xlsx" et est stocké au même endroit que le script R.
#' Il est possible de changer le nom du fichier à l’aide de l’argument 'fichier', en spécifiant l’extension .xlsx (par exemple : fichier.xlsx).
#' Il est également possible de changer le chemin d’accès à l’aide de l’argument 'path', qui doit être sous la forme «~/Documents» ou «~/Downloads», comme pour la fonction 'import'.
#'
#' @param path Le chemin permettant d'accéder aux fichiers
#' @param fichier Nom du fichier enregistré
#'
#' @return La synthèse GéoNature finale contenant les colonnes sélectionnées par l'utilisateur
#' @export
#'
#' @example Export par défaut (fichiers non renommés et présents dans le même dossier que le script R)
#' export()
#'
#' @example Export avec modification du chemin d'accès
#' export(path = "~/Documents")
#' export(path = "~/Downloads")
#'
#' @example Export avec modification des noms de fichiers (les fichiers doivent être des .csv)
#' export(fichier = "BDD_ABAURA.xlsx")



export <- function(path = ".", fichier = "export_final_GeoNature.xlsx"){
  write.csv(x = export_final_GeoNature, file = paste0(path, "/", fichier))
}



