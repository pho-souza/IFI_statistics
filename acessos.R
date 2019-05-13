library(readxl)
library(tidyverse)
library(rvest)
library(data.table)
library(lubridate)

# Importação dos dados da Lista do Excel com todos os documentos da IFI
itens <- read_excel(path = "Lista.xlsx")



# Definição da função para extrair dados do site da IFI
count.views.now <- function(pub){
  my.url <- paste(c("https://www2.senado.leg.br/bdsf/handle/id/",as.character(pub),"/statistics"),collapse="")
  file <- read_html(my.url)
  sections <- html_text(html_nodes(x=file,css="h2"))
  n.sections <- length(sections)
  tables <- html_nodes(file,"table")
  tables <- html_table(tables)
  tables <- tables[1:length(sections)]
  names(tables) <- sections
  value <- tables
}

count.ifi <- function(){
  attach(what="U:/Pastas pessoais/Alessandro/atualiza/acessos/RAF",name="RAF")
  store <- vector(mode="list",length=nrow(RAFs))
  for (i in 1:nrow(RAFs)){    store[[i]] <- count.views.now(RAFs[i,"Código"])    }
  attr(x=store,which="time") <- date()
  load(file="U:/Pastas pessoais/Alessandro/atualiza/acessos/number")
  name.update <- paste(c("U:/Pastas pessoais/Alessandro/atualiza/acessos/","RAFs_",number),collapse="")
  number <- number + 1
  save(number,file="U:/Pastas pessoais/Alessandro/atualiza/acessos/number")
  save(store,file=name.update)
  detach(name="RAF")
}

# Abrir a base01, que contém os dados da IFI brutos. 
load(file = "base01")
# Abrir base inicialmente
teste01.total$Numero
#rm(teste01.total)
# Com base   na quantidade de elementos na planilha do Excel

for(i in 1:NROW(itens)){
  teste01 <- count.views.now(itens$ID[i])
  #teste01.total$Numero <- ifelse(teste01.total$ID==itens$ID[i],itens$Numero[i],teste01.total$Numero)
  #
  print(i)
  
  if(#i == 1 & 
    exists(x = "teste01.total") != T){
    teste01.total <- teste01$`Arquivos visitados`
    teste01.total$ID <- itens$ID[i]
    teste01.total$Nome <- itens$Nome[i]
    teste01.total$Categoria <- itens$Categoria[i]
    teste01.total$Numero <- ifelse(teste01.total$ID == itens$ID[i],itens$Numero[i],teste01.total$Numero)
    teste01.total$Data <- as.POSIXct(as.character("2019-01-15 14:34:30"),format="%Y-%m-%d %H")
    teste01.total$Data_Original <- itens$Data[i]
    names(teste01.total) <- c("Nome_Arquivo","visualizações","ID","Nome","Categoria","Numero","Data", "Data_Original")
    
    
  }
  else
  {
    tmp.total <- teste01$`Arquivos visitados`
    tmp.total$ID <- itens$ID[i]
    tmp.total$Nome <- itens$Nome[i]
    tmp.total$Categoria <- itens$Categoria[i]
    tmp.total$Numero <- ifelse(tmp.total$ID == itens$ID[i], itens$Numero[i],tmp.total$Numero)
    tmp.total$Data <- as.POSIXct(Sys.time())
    tmp.total$Data_Original <- itens$Data[i]
    names(tmp.total) <- c("Nome_Arquivo","visualizações","ID","Nome","Categoria","Numero","Data", "Data_Original")
    
    teste01.total <- rbind(teste01.total,tmp.total)
  }
  
}

for(i in 1:NROW(itens)){
  teste01.total$Numero = ifelse(teste01.total$ID == itens$ID[i],itens$Numero[i],teste01.total$Numero)
}

# Salva a base após atualizar
save(teste01.total,file = "base01")
save(teste01.total,file = "base001-bkp")

troca <- as.character(c("%20","%C3%A9"))
trocado <- as.character(c(" ", "é"))
subs <- as.data.frame(cbind(troca,trocado))
subs <- subs %>% mutate(troca=as.character(troca),trocado=as.character(trocado))
rm(troca, trocado)


# Aqui substitui os elementos do texto referentes à acentos e outros elementos
for(i in 1:NROW(subs)){
  itens$`Nome arquivo` <- gsub(pattern = subs$troca[i],replacement = subs$trocado[i], x = itens$`Nome arquivo`)
}



#filtro dos arquivos principais 
teste02.total <- teste01.total %>%
  #mutate(Nome_Arquivo = gsub("%20"," ",Nome_Arquivo)) %>%
  filter(Nome_Arquivo %in% itens$`Nome arquivo`) %>%
  mutate(ID= factor(ID), Categoria = factor(Categoria), Nome = factor(Nome),Nome_Arquivo = factor(Nome_Arquivo),Numero = as.integer(Numero))

teste02.total <- teste02.total %>% 
  group_by(Data,Categoria,ID,Numero,Nome,Data_Original,Nome_Arquivo) %>%
  summarise(visualizações=sum(visualizações))


teste02.total <- teste02.total %>%
  group_by(Data) %>%
  mutate(Hora = sub(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2} ",replacement = "", x = Data)) %>%
  mutate(Hora = as.POSIXct(paste("2019-01-01 ", Hora))) %>%
  ungroup(Data) %>%
  mutate(Data = as.Date(Data))

# Seleciona o último valor de cada data
teste03.total <- teste02.total %>%
  group_by(Data,Nome) %>%
  filter(Hora==max(Hora)) #%>%


teste02.total$Data[which.max(teste02.total$Data)]

save(teste02.total, file="base02")
save(teste03.total, file="base03")
write_excel_csv(teste01.total, "relatorio01.xlsx")


