

create_profile_qmd <- function(form.table, row){
  
    
    pers.table <- as.data.frame(form.table[row,])
   
    name   <- pers.table[,"Nombre y apellidos"]
    email  <- pers.table[,"Email"]
    inves  <- pers.table[,"Resumen investigación (max. 3 frases)"]
    inter  <- pers.table[,"Intereses dentro del grupo (max. 4 intereses)"]
    web    <- pers.table[,"Pagina web"]
    github <- pers.table[,"Cuenta de GitHub"]
    twitter<- pers.table[,"Cuenta de Bluesky o Twitter"]
    # bsky   <- pers.table[,"Cuenta de Bluesky"]
     
    gh.icon <- fontawesome::fa(name = "github",  fill = "#bfe6f5", height = "1em")
    tw.icon <- fontawesome::fa(name = "twitter", fill = "#bfe6f5", height = "1em")
    
    icons <- " "

    if(!is.na(twitter)){icons <- paste0(icons, tw.icon,"\t", twitter, "\t\t")}
    if(!is.na(github)) {icons <- paste0(icons, gh.icon,"\t", github , "\t\t")}    

        if(is.na(github)){
      if(!file.exists("blank.png")){
      png("blank.png", 100,100,res = 100, units = "mm")
      plot.new()
      dev.off()
      }
      image <- "blank.png"
    }else{
      ghuser <- gsub("https://", "", github)
      ghuser <- gsub("github.com/", "", ghuser)
      image <- paste0("https://github.com/", ghuser, ".png")
      }
    
    ord    <- which(sort(formdata$`Nombre y apellidos`) == name)[1]
    qmdname <- paste0(gsub(" ", "_", name), ".qmd")
    
    # if(!(file.exists(paste0("miembros/",qmdname)))){
    # mejor re-renderizar para asegurar el orden ?
      
  txt <- paste0(
    "---", "\n",
    "title: ", name,  "\n",
    "sortby: ", ord,  "\n",
    "subtitle: ", email,  "\n",
    "image: ", image, "\n",
    "toc: false",  "\n",
    "about: ", "\n",
    "  id: person-profile",  "\n",
    "  template: jolla",  "\n",
    "",  "\n",
    "---",  "\n",
    "",  "\n",
    ":::{#person-profile}",  "\n",
    "\n",
    "# Resumen de investigación:\n",
    inves, "\n\n",
    "#  Interés profesional dentro del grupo:\n",    
    inter, "\n",
    "\n","\n",
    icons,"\n",
    ":::"
  )
  
  writeLines(txt, paste0("miembros/", qmdname))
      
  # }
}    


# for(i in 1:nrow(formdata)){
# create_profile_qmd(formdata, i)
# }
