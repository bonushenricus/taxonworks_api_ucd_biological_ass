# taxonworks_api_ucd_biological_ass
r httr2 code to get biological association from a list of Chalcidoidea taxa of Universal Chalcidoidea Database based on taxonworks

non uso per ora il nuovo pacchetto rtaxonworks perché manca di alcuni endpoints e query

inoltre usanto httr2 in rstudio mi compare nell'elaborazione il tempo di lavoro, che è molto comodo

Il risultato è la base delle associazioni biologiche + la tassonomia degli object (from) e dei subject (to) di queste associazioni, tale da poter essere utilizzato per la visualizzazione in un grafico "hierarchical edge bundle"
