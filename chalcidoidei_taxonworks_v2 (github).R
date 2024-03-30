#dati di partenza: la lista degli esemplari identificati
#scopo: costruire un grafo delle associazioni parassitoidi-ospiti
#elaborati a partire da taxonworks UCD Universal Chalcidoidea Database

library(httr2)
library(jsonlite)
library(tidyverse)
library(tibble)
library(dplyr)
library(tidyr)
library(readr)

identificati <- read_tsv("Monitoraggio Chalcidoidei 2022.tsv") #elenco delle specie (o eventualmente generi) identificati. I taxa devono essere valildi per taxonworks UCD, Quindi prima fare un controllo!
identificati = as.data.frame(unique(identificati23$taxon)) #se le specie sono elencate più volte

lista_identificati = split(identificati$scientificName, seq(nrow(identificati))) #trasformare in una lista in cui ogni oggetto è un nome. serve per lapply in seguito

#non uso il nuovo pacchetto rtaxonworks perché manca di alcuni endpoints e query
#inoltre usanto httr2 in rstudio mi compare nell'elaborazione il tempo di lavoro

#Define access token: varrà in tutto il codice
access_token_taxonworks_sfg = "xxxxxxxx" #ovviamente non posso pubblicare il token del mio acount
project_token_taxonworks_sfg = "yyyyyyyy" #questo è il token del progetto UCD. Anche questo non lo pubblico

#Define base urls: varrà per tutto il codice
base_url_taxonworks_sfg = "https://sfg.taxonworks.org/api/v1" #per fare delle prove usare l'url sandfly

# find biological association data for the taxon_name previously elaborated
#Define endpoints
endpoint_bioass = "/biological_associations" #endpoints to get the biological association of a taxon_id of a taxon name

#Define request
req_bioass_sfg = request(base_url_taxonworks_sfg) %>% 
  req_url_path_append(endpoint_bioass) %>%
  req_url_query(project_token=project_token_taxonworks_sfg) #the project_token is necessary to authorize the request

#Define base query, ovvero la base della ricerca
req_bioass_sfg_query = req_bioass_sfg %>%
  req_url_query(
    "taxon_name_query[synonymify]"="true",
    #        "biological_relationship_id[]"=7, 
    #        "biological_relationship_id[]"=8, #compared with / reference for, in generale non riguarda associazioni biologiche, ma tassonomiche
    #        "biological_relationship_id[]"=9, #Plant associate, nn riguarda relazioni con altri artropodi
    "biological_relationship_id[]"=10, #Associate (spesso legato alle galle direi)
    "biological_relationship_id[]"=11, #parasitoid
    "biological_relationship_id[]"=12, #parasitoid host (quindi iperparassitoide)
    #        "biological_relationship_id[]"=13, #Plant host - per i fitofagi
    "biological_relationship_id[]"=14, #"primary host", la relazione più importante
    geographic_area_mode="false",#geo descendants
    "geographic_area_id[]"=33539, #Europe
    "geographic_area_id[]"=33567, #Westerna Asia
    "geographic_area_id[]"=33553, #Northern Africa
    "extend[]"="object", #per avere il nome dell'oggetto. altrimenti verrebbe solo l'id dell'otu
    "extend[]"="subject", #per avere il nome del soggetto. altrimenti verrebbe solo l'id dell'otu
    "extend[]"="taxonomy") %>%  #questo è importante per avere la tassonomia del soggetto e dell'oggetto
  req_url_query(
    taxon_name_id_mode="false") #non ricordo a che serve, ma funziona

#Provide access token via header
req_bioass_sfg_query_auth = req_bioass_sfg_query %>%
  req_headers(
    'Authorization'=paste0('Token ',access_token_taxonworks_sfg) #aggiungere l'autorizzazioni
  )

#take taxon_names from identificati
reqs_bioass = lapply(lista_identificati, 
                     function(x) req_url_query(req_bioass_sfg_query_auth,"taxon_name_query[name]"=x)) #creare una lista di request con la base precedente, ognuna per un taxon valido che interessa il nostro lavoro


#Perform response
resps = reqs_bioass %>%
  req_perform_sequential() #da qui parte un indicatore del tempo, molto utile

resp_perform_bioass_sfg_string = resps |> resps_successes() |> resps_data(\(resp) resp_body_string(resp)) #trasforma le risposte in lista di stringhe

#Transform to dataframe
library(jsonlite)
resp_perform_bioass_sfg_dataframe = lapply(resp_perform_bioass_sfg_string, function(x) fromJSON(x))
library(dplyr)
dataframe_bioass = bind_rows(resp_perform_bioass_sfg_dataframe)

dataframe_bioass = dataframe_bioass %>% filter(object$taxonomy$phylum=="Arthropoda") #togliamo subito ciò che non è artropoda. vi sono possibili associazioni con funghi o batteri, che non sono l'oggetto del mio studio

#per un motivo strano genus e specie sono in vettori nella forma c(NA,...). Trasformiamoli
dataframe_bioass$subject$taxonomy$genus = sapply(dataframe_bioass$subject$taxonomy$genus, "[", 2)
dataframe_bioass$subject$taxonomy$species = sapply(dataframe_bioass$subject$taxonomy$species, "[", 2)
dataframe_bioass$object$taxonomy$genus = sapply(dataframe_bioass$object$taxonomy$genus, "[", 2)
dataframe_bioass$object$taxonomy$genus = sapply(dataframe_bioass$object$taxonomy$genus, paste0, collapse="")#non so perché rimane lista, quindi va fatto anche questo, per una seconda volta
dataframe_bioass$object$taxonomy$species = sapply(dataframe_bioass$object$taxonomy$species, "[", 2)
dataframe_bioass$object$taxonomy$species = sapply(dataframe_bioass$object$taxonomy$species, paste0, collapse="")#non so perché rimane lista, quindi va fatto anche questo, per una seconda volta
dataframe_bioass$subject$taxonomy$species[dataframe_bioass$subject$taxonomy$species=="NULL"]="" #serve per la colonna scientificName
#creo le colonne dei nomi "genere+specie"
dataframe_bioass$subject$taxonomy$scientificName = ifelse(dataframe_bioass$subject$taxonomy$species=="","",paste0(dataframe_bioass$subject$taxonomy$genus,' ',dataframe_bioass$subject$taxonomy$species))
dataframe_bioass$object$taxonomy$scientificName = ifelse(dataframe_bioass$object$taxonomy$species=="","",paste0(dataframe_bioass$object$taxonomy$genus,' ',dataframe_bioass$object$taxonomy$species))

#semplifico il dataframe mantenendo solo id relationship e tassonomia
dataframe_bioass_simple = data.frame(relationship_id=dataframe_bioass$biological_relationship_id,
                                     subj_scientificName=dataframe_bioass$subject$taxonomy$scientificName,
                                     subj_genus=dataframe_bioass$subject$taxonomy$genus,
                                     subj_family=dataframe_bioass$subject$taxonomy$family,
                                     subj_order=dataframe_bioass$subject$taxonomy$order,
                                     subj_class=dataframe_bioass$subject$taxonomy$class,
                                     subj_phylum=dataframe_bioass$subject$taxonomy$phylum,
#                                     obj_scientificName=dataframe_bioass$object$taxonomy$scientificName,#per il soggetto essendo la lista dei scientificName molto lunga si può mantenere solo il genere
                                     obj_genus=dataframe_bioass$object$taxonomy$genus,
                                     obj_family=dataframe_bioass$object$taxonomy$family,
                                     obj_order=dataframe_bioass$object$taxonomy$order,
                                     obj_class=dataframe_bioass$object$taxonomy$class,
                                     obj_phylum=dataframe_bioass$object$taxonomy$phylum)
dataframe_bioass_simple[(dataframe_bioass_simple)==""] <- NA #è importante trasformare tutti gli NA in blank, per via che funzioni la rilevazione dell'oggetto

#scegliere l'oggetto tra genere, famiglia e ordine a seconda del primo dove c'è il dato per associazione biologica, in quanto nelle associazioni a volte si arriva alla specie, a volte al genus, a volte alla famiglia o persino solo all'ordine
dataframe_bioass_simple = dataframe_bioass_simple %>% mutate(object=ifelse(is.na(obj_genus),
                                                                                  ifelse(is.na(obj_family),obj_order,obj_family)
                                                                                  ,obj_genus))
dataframe_bioass_simple = subset(dataframe_bioass_simple,!is.na(object))#togliere gli NA perché igraph non riesce poi a toglierli
#scegliere il soggetto tra genere e scientifcName (alcune volte è solo il genere), sipende dalla prima lista di interesse di taxa validi da cui si è partiti
dataframe_bioass_simple = dataframe_bioass_simple %>% mutate(subject=ifelse(is.na(subj_scientificName),
                                                                            subj_genus,
                                                                           subj_scientificName))
#per uno strano motivo taxonworks mi tira su anche nomi che non ci sono, quindi faccio un join tra gli identificati e il from.
#A mio avviso con queste linee mi sto togliendo delle associazioni. Ed ho perso dei generi tra gli identificati. Ma altrimenti il dataframe sarebbe piuttosto "sporco"
dataframe_bioass_simple = inner_join(dataframe_bioass_simple,identificati,by=join_by(subject==scientificName))
dataframe_bioass_simple = distinct(dataframe_bioass_simple)
dataframe_bioass_simple_basegraph = dataframe_bioass_simple %>% select(subject,
                                                                       object,
                                                                       relationship_id)

dataframe_bioass_simple_basegraph = distinct(dataframe_bioass_simple_basegraph) #un'ultima pulizia da eventuali duplicati

colnames(dataframe_bioass_simple_basegraph)=c("from","to","attr") #do dei nomi di colonne codificati per l'utilizzo in seguito di tidygraph, igraph e ggraph
dataframe_bioass_simple_basegraph$relazione = NA #attribuisco dei nomi facili da capire per le interazioni
dataframe_bioass_simple_basegraph$relazione[dataframe_bioass_simple_basegraph$attr==10]="D associazione non parassitoide"
dataframe_bioass_simple_basegraph$relazione[dataframe_bioass_simple_basegraph$attr==11]="B parassitoide-ospite"
dataframe_bioass_simple_basegraph$relazione[dataframe_bioass_simple_basegraph$attr==12]="C parassitoide-iperparassitoide"
dataframe_bioass_simple_basegraph$relazione[dataframe_bioass_simple_basegraph$attr==14]="A parassitoide-ospite primario"
#bioass_graph = tbl_graph(dataframe_bioass_simple_basegraph,directed = FALSE) #questo se è necessario calcolare delle metriche del grafo
#questa parte è da fare solo una volta
dataframe_bioass_simple_basegraph$from=sapply(dataframe_bioass_simple_basegraph$from, function(x) paste0("-",x))#devo aggiungere i suffissi al from delle associazioni in maniera da avere distinto la source, ovvero gli identificati, dai to, in quanto alcuni Chalcidoidei hanno come relazione altri Chalcidoidei, quindi il grafico avrebbe delle linee che tornano indietro, poco comprensibili. In questa maniera creo una tassonomia fittizia di facile lettura

