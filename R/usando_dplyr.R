#? Comentários com "?" são comentários normais
#! Comentários com "!" são códigos errados
#* Comentários com "*" são para correções
# Apenas "#" são códigos comentados (ignorados)
#TODO é algo para fazermos juntos

#? Vamos adicionar a biblioteca dplyr 
library(dplyr)

#? E outras bibliotecas que serão úteis
library(lubridate)
library(stringr)

#? Vamos começar com os dados de pokemon
#? https://www.kaggle.com/datasets/igorcoelho24/pokemon-all-generations/versions/1?resource=download
dados <- read.csv("R/Dados/Pokemon_full.csv")
head(dados) #? vê as primeiras linhas de dados
#dados <- read.csv("D:/Aulas/ferramentasdemodelagem/R/Dados/Pokemon_full.csv")
#? A biblioteca dplyr possui o operador "pipe"
#? dado por  %>%
#? Ele "pega" tudo que está à esquerda dele e coloca como primeiro elemento
#? da função à direita.
#? Também é possível usar o operador "."
#? para especificar onde ele deve substituir.

#? Exemplo: contar o número de linhas de dados

nrow(dados)
dados %>% nrow()
dados %>% nrow(.)


## grepl() # verifica se um padrão de string esta em um elemento
grepl("ap", "apple")
grepl("apple", "ap")

x <- "apple"

x %>% grepl("ap", .)
x %>% grepl("ap")

#? Algumas funções da biblioteca dplyr

#? A função filter seleciona linhas com base em um teste
df_grass <- filter(dados, type == "grass")
df_grass

#? podemos usar o seguinte comando também
 dados %>% filter(type == "grass")

#TODO Vamos filtrar todos os pokemons do tipo fogo ou água

df_fogo_e_agua <- dados %>% filter(type == "fire" | type == "water")

#TODO Vamos filtrar todos os pokemons que tem  "fly"
dados %>% filter(grepl("fly", name))

#TODO Vamos filtrar todos os pokemons que tem  "bee" ou "saur"
dados %>% filter(grepl("bee", name) | grepl("saur", name))
dados %>% filter(grepl("bee|saur", name))
dados %>% head

#? REGEX

vetor = c("banana", "Banana", "maça")
grepl("[Bb]anana", vetor)
grep("[Bb]anana", vetor)
vetor = c("2023/04/01", "Banana", "maça")
grepl("[A-Z]*", vetor)



#! errado
dados %>% filter((name == "bee" | name == "saur"))
filter(dados, name == "bee" | name =="saur")

#* FUNCIONA
dados[str_detect(dados$name, "bee|saur"),]

names(dados)

#? A função pull devolve um vetor
dados %>%
    filter(type == "fire") %>%
    pull(secundary.type) %>%
    unique

dados2 <- dados[dados$type == "fire",]
unique(dados2$secundary.type)

unique(dados[dados$type == "fire",]$secundary.type)


dados %>% select(type) %>% unique
dados %>% select(type, secundary.type)
dados %>% select(type, secundary.type) %>% unique

#? A função select seleciona colunas
dados %>% select(c(1, 2, 3)) #? pelo número
df <- dados %>% select(name, type, height) #? pelo nome

df <- dados %>%select(height, weight, hp) %>% as.matrix()
df
#TODO achar todas as combinações existentes de type e secondary.type

#? Outras possibilidades
dados %>% names
dados %>%
    select(starts_with("h")) %>% head #? starts_with, ends_with, contains

dados %>% select(-name) %>% head #? negativo exclui as colunas

#? A função mutate modifica ou cria uma coluna com base em outras
df <- mutate(dados, height2 = 2*height) %>% head
nrow(df)

dados <- dados %>% 
    mutate(
        height2 = 2*height,
        speed2 = 2*speed,
        bee = grepl("bee", name)
    )

dados %>% head
#? A função arrange organiza o data frame com base em colunas

dados %>%
    arrange(name) %>%
        head()

dados %>%
    arrange(name) %>%
        tail()

dados %>%
    arrange(desc(name)) %>%
        tail()

dados %>%
    arrange(desc(name)) %>%
        head()

dados %>%
    arrange(desc(name), height) %>%
        head()

df <- data.frame(
        nome = c("maria", "zé", "joão", "maria"),
        altura = c(2, 3, 4, 1)
    )

df %>%
    arrange(nome, altura)

#? Vamos fazer algumas contas!!

dados %>%
    summarise(
        media_altura = mean(height),
        media_peso = mean(weight)
    )

#? Podemos fazer isso por grupos
dados %>%
    group_by(type) %>%
        summarise(
            media_altura = mean(height),
            media_peso = mean(weight),
            N = n()
        ) %>%
            arrange(media_altura)

#TODO Filtrar os pokemons que tem o peso acima da média da altura do seu type
dados %>%
    group_by(type) %>%
    mutate(
        media_altura = mean(height)
    )  -> df 

    write.csv(df, "df.csv")
    xlsx::write.xlsx(df, "df.csv")

dados %>%
    group_by(type) %>%
    mutate(
        media_altura = mean(height),
        media_peso = mean(weight)
    ) %>%
    filter(height > media_altura, weight > media_peso) %>%
    select(-media_altura) %>%
    ungroup() %>%
    mutate(
        imc = weight/height^2,
        mm = sum(weight) #! Não funciona
    )-> df 

dados %>%
    group_by(type) %>%
    mutate(
        media_altura = mean(height),
        media_peso = mean(weight)
    ) %>%
    filter(height > media_altura, weight > media_peso) %>%
    select(-media_altura) %>%
    ungroup() %>%
    rowise() %>%
    mutate(
        imc = weight/height^2,
        mm = sum(weight) #* Funciona
    )-> df 

    write.csv(df, "df.csv")

#TODO Lição
#TODO criar uma coluna com a transformação Z-score para altura POR type utilizando TODAS
    
df <-   dados %>%
    group_by(type) %>%
    mutate(
      z_height = (height-mean(height))/sd(height),
      z_weight = (weight-mean(weight))/sd(weight)
    )

library(ggplot2)

dados %>% pull(type) %>% unique

ggplot(df)+
  geom_density(aes(x = z_height, color = type))+
  theme_bw()


#? Renomear colunas
dados %>%
    group_by(type) %>%
        summarise(
            media_altura = mean(height),
            media_peso = mean(weight),
            N = n()
        ) %>%
            arrange(media_altura) %>%
                rename("Número de pokemons" = N)

#? Movê-las
dados %>%
    group_by(type) %>%
        summarise(
            media_altura = mean(height),
            media_peso = mean(weight),
            N = n()
        ) %>%
            arrange(media_altura) %>%
                rename("Número de pokemons" = N) %>%
                relocate("Número de pokemons")
dados %>%
    group_by(type) %>%
        summarise(
            media_altura = mean(height),
            media_peso = mean(weight),
            N = n()
        ) %>%
            arrange(media_altura) %>%
                rename("Número de pokemons" = N) %>%
                relocate("Número de pokemons", .after = type)


#? rowwise

#? A função mutate e outras do pacote dplyr trabalham diretamente com as colunas
#? como se fossem operações de vetor

dados %>%
    mutate(
        name2 = paste(name, " - NOVO")
    ) %>% head

#? Na prática, o dplyr faz isso:
paste(dados$name, "- NOVO")

#? o que significa que a função PRECISA aceitar um vetor

#? Imagine que você queira criar uma função que testa se o valor de uma coluna
#? na observação i é maior ou menor que um dado valor e executa uma certa ação.

f <- function(x){
    if(x <= 15){ #? no caso, o valor é 300
        return("Executei essa ação")
    }else{

        return("Executei Aquela ação")
    }
}

x1 <- c(30, 16, 20, 3)
f(x1)

#! O código abaixo não funciona
dados %>%
    mutate(
        nova_var = f(height)
    ) %>%
        select(height, nova_var) %>% head(30)

#* O código abaixo funciona
#TODO
dados %>%
rowwise() %>%
    mutate(
        nova_var = f(height)
    ) %>%
        select(height, nova_var) %>% head(30)


#? ifelse e case_when

dados %>%
    mutate(
        tamanho = ifelse(
            height < 15,
            "baixinho",
            "altão"
        )
    ) %>% head

ff <- function(y){
  resposta <- c()
  
  for(i in 1:length(y)){
    if(y[i] <= 15){ #? no caso, o valor é 300
      resposta[i] <- "baixinho"
    }else{
      
      resposta[i] <- "altão"
    }
  }
  
  return(resposta)
}

ff(x1)
x1

dados %>%
  mutate(
    tamanho = ff(height)
  ) %>% head

# 
# ifelse(
#   height < 5,
#   "baixinho",
#   ifelse(
#     height < 10, 
#     ...
#   )
# )

dados %>%
    mutate(
        tamanho = case_when(
            height < 5 ~ "baixinho",
            height < 10 ~ "pequeno",
            height < 15 ~ "médio",
            TRUE ~ "altão"
        )
    ) %>% head


dados %>%
  mutate(
    tamanho = case_when(
      height < 5 ~ "baixinho",
      height < 10 ~ "pequeno",
      height < 15 ~ "médio",
      TRUE ~ "altão"
    )
  ) %>% head


# ERRADO!!!
dados %>%
  mutate(
    tamanho = case_when(
      height < 5 ~ "baixinho",
      height < 10 ~ "pequeno",
      height < 15 ~ "médio"
    )
  ) %>% head(15)

#? Alguns perrengues da vida

#! O código abaixo não funciona
dados %>%
    mutate(
        tamanho = case_when(
            height < 5 ~ "baixinho",
            height < 10 ~ "pequeno",
            height < 15 ~ NA,
            TRUE ~ "altão"
        )
    ) %>% head

#* O código abaixo conserta isso
#TODO

dados %>%
  mutate(
    tamanho = case_when(
      height < 5 ~ "baixinho",
      height < 10 ~ "pequeno",
      height < 15 ~ NA_character_,
      TRUE ~ "altão"
    )
  ) %>% head

# Juntar dados

#bind

#rbind
#cbind

df_A = data.frame(A = c(1,2,3,4), B = c(5, 6, 3, 2))
df_B = data.frame(A = c(12,22,32,42), B = c(7, 5, 3, 2))

rbind(df_A, df_B)


df_A = data.frame(A = c(1,2,3,4), B = c(5, 6, 3, 2))
df_B = data.frame(A = c(12,22,32,42), C = c(7, 5, 3, 2))

bind_rows(df_A, df_B)


df_A = data.frame(A = c(1,2,3,4))
df_B = data.frame(B = c(12,22,32,42))

cbind(df_A, df_B)


df_A = data.frame(A = c(1,2,3,4))
df_B = data.frame(B = c(12,22,32))

cbind(df_A, df_B)
bind_cols(df_A, df_B) # checar o bind_cols()

# outras funçoes

#? Vamos falar de JOIN

df_means <- dados %>%
    group_by(type) %>%
    summarise(
        media_h = mean(height),
        media_w = mean(weight)
    )
df_means

#? vamos excluir os grupos que começam com "g"
#TODO


df_means %>% 
  filter(grepl("^g", type)) #REGEX

df_means %>% 
  filter(grepl("g$", type)) #REGEX

df_means %>% 
  filter(grepl(".+g.+", type)) #REGEX

df_means %>% 
  filter(grepl(".+g.*", type)) #REGEX

df_means <- df_means %>% 
  filter(!grepl("^g", type))

df_means
#? vamos adicionar um grupo que não existe

novo_grupo <- data.frame(
    type = "Vozes da minha cabeça",
    media_h = 1000,
    media_w = 400.82
)

#TODO adicionar o grupo
df_means <- rbind(df_means, novo_grupo)

### BOA Prática

dados <- dados %>% 
  mutate_if(is.character, function(x) trimws(x,"both"))

#? full_join
#TODO

# full -> manter tudo de todos
df <- full_join(dados, df_means, by = "type")
View(df)
#? inner_join
#TODO
df <- inner_join(dados, df_means, by = "type")
View(df)
#? left_join
#TODO
df <- left_join(dados, df_means, by = "type")
View(df)
#? right_join
#TODO
df <- right_join(dados, df_means, by = "type")
View(df)

dados %>% 
  left_join(df_means, by = "type") %>% 
  left_join(df_means, by = "type") %>% View


# SINTAXE
names(dados)

df_means <- dados %>%
  group_by(type, secundary.type) %>%
  summarise(
    media_h = mean(height),
    media_w = mean(weight)
  )

df_means


df <- right_join(dados, df_means, by = c("type", "secundary.type"))
View(df)

df_means <- df_means %>% rename(height = media_h)

## checar se existe ou
df <- right_join(dados, df_means, by = c("type" = "type2", "secundary.type" = "secundary.type"))
View(df)

#? vamos adicionar um grupo que JÁ existe

novo_grupo <- data.frame(
    type = "bug",
    media_h = 10,
    media_w = 800
)


#TODO adicionar o grupo
df_means <- rbind(df_means, novo_grupo)

#? left_join
#TODO
df <- left_join(dados, df_means, by = "type")
View(df)
df %>% 
  filter(type == "bug") %>%  head(15)
df %>% tail()

#? right_join
#TODO


#?#####################################################################
#? TIDYR
#?#####################################################################

library(tidyr)

#? baixado de https://livro.curso-r.com/

dados <- readr::read_rds("R/Dados/imdb.rds")
dados <- readr::read_rds("D:/Aulas/ferramentasdemodelagem/R/Dados/imdb.rds")
View(dados)
head(dados)
names(dados)


df <- dados %>% 
  select(titulo, orcamento, receita, receita_eua)
df

# um gráfico com 10 primeiros filmes
# barras
# cada barra vem de uma coluna e aparece com uma cor diferente

#TODO checar se cada filme tem apenas um genero associado

#? Pivoteamento

#? pivot_longer

df_long <- df %>%
  slice(1:10) %>% 
  tidyr::pivot_longer(2:4, values_to = Valor, names_to = "Tipo de Valor")

df %>%
  slice(1:10) %>% 
  tidyr::pivot_longer(2:4, values_to = "Valor", names_to = "Tipo de Valor")




View(df %>% slice(1:10))
View(df_long)

# carregar
library(ggplot2)

ggplot()+
  geom_col(data = df_long, aes(x = titulo, y = Valor, fill = `Tipo de Valor`),
            position = position_dodge2()
           )+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1.0)
  )



## Pivot wider
df_long %>% 
  tidyr::pivot_wider(names_from = `Tipo de Valor`, values_from = Valor)

# correlação cor()

# Calcular correlação do conjunto inteiro entre as variáveis

df %>%
  select_if(is.numeric) %>%
  filter(complete.cases(.)) %>% 
  cor()

#? Emissões de ar
#? https://www.kaggle.com/datasets/ashishraut64/global-methane-emissions


#? Netflix
#? https://www.kaggle.com/datasets/victorsoeiro/netflix-tv-shows-and-movies
