# Carregando os dados
library(readxl)
DF <- read_excel("Planejamento/Trabalho2/dados_completos.xlsx")
genero=DF$V1 # bloco
esporte=DF$V2 # fator fixo
gordura_corporal=DF$Y # variavel resposta
data.frame(genero,esporte,gordura_corporal)

# Sorteando os dados
# Fixaremos apenas os niveis Barco, Nataçao e Tenis dos Esportes
set.seed(3001) # rodar antes de todos os sorteios!
row_fem=sample(14:35,4); row_fem
swim_fem=sample(59:67,4); swim_fem
ten_fem=sample(90:96,4); ten_fem
swim_mal=sample(101:113,4); swim_mal
row_mal=sample(114:128,4); row_mal
ten_mal=199:202; ten_mal # sÃ³ temos 4 amostras

dado=c(row_fem,swim_fem,ten_fem,swim_mal,row_mal,ten_mal)
y=as.numeric(gordura_corporal[dado])
genero=factor(c(rep("feminino",12),rep("masculino",12)))
esporte=factor(c(rep("barco",4),rep("natacao",4),rep("tenis",4),rep("natacao",4),rep("barco",4),rep("tenis",4)))
datas=data.frame(genero,esporte,y); datas

# AnÃ¡lise descritiva
interaction.plot(genero,esporte,y, fun = mean)
boxplot(y~genero)
boxplot(y~esporte)

# Teste de Aditividade de Tukey
require(asbio)
tukey.add.test(y,genero,esporte) # sem efeito de interaÃ§Ã£o polinomial

# Ajuste do modelo com interaÃ§Ã£o 
m<-aov(y~genero*esporte)
summary(m) # rejeita interaÃ§Ã£o

# Ajuste do modelo sem interaÃ§Ã£o
m1<-aov(y~genero+esporte)
summary(m1)

# Boxplot dos nÃ­veis do bloco
library(ggplot2)
ggplot(datas, aes(y = y, x = genero, fill = genero)) +
    geom_boxplot(show.legend = F, alpha = .5) +
    theme_classic(base_size = 18) +
    labs(y = "Gordura Corporal", x = "GÃªnero",
         title = "Boxplot dos Niveis do Bloco Genero")+
    theme( plot.title = element_text(hjust = 0.5),
           axis.title = element_text(size = 11),
           axis.text = element_text(size = 13), 
           title =  element_text(size = 13) ) 

# Dotplot dos niveis do bloco
ggplot(datas, aes(x=genero, y=y, fill=genero))+
    geom_dotplot(show.legend=F, binaxis = "y", stackdir = "center", 
                 binwidth = 1.5, alpha = 0.8)+
    labs(title = "Dotplot dos Niveis do Bloco Genero", x = "Genero", 
         y = "Gordura Corporal") +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5), axis.title = 
              element_text(size = 13), 
          axis.text = element_text(size = 13), title =  
              element_text(size = 15))

# Boxplot dos nÃ­veis do fator
ggplot(datas, aes(y = y, x = esporte, fill = esporte)) +
    geom_boxplot(show.legend = F, alpha = .5) +
    theme_classic(base_size = 18) +
    labs(y = "Gordura Corporal", x = "Esporte", 
         title = "Boxplot dos Niveis do Fator Esporte")+
    theme( plot.title = element_text(hjust = 0.5),
           axis.title = element_text(size = 11),
           axis.text = element_text(size = 13), 
           title =  element_text(size = 13) )

# Dotplot dos nÃ­veis do fator
ggplot(datas, aes(x=esporte, y=y, fill=esporte))+
    geom_dotplot(show.legend=F, binaxis = "y", stackdir = "center", 
                 binwidth = 1.5, alpha = 0.8)+
    labs(title = "Dotplot dos Niveis do Fator Esporte", x = "Esporte", 
         y = "Gordura Corporal") +
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5), axis.title = 
              element_text(size = 13), 
          axis.text = element_text(size = 13), title =  
              element_text(size = 15))

# GrÃ¡fico de interaÃ§Ã£o
ggplot(datas) +
    aes(x = genero, color = esporte, group = esporte, y = y) +
    stat_summary(fun = mean, geom = "point", size = 2) +
    stat_summary(fun = mean, geom = "line", size = 1)+
    labs(title = "Grafico de interação entre fator Esporte e bloco Genero", 
         x = "Genero", y = "Gordura Corporal") +
    theme_classic()+
    scale_colour_discrete("Esporte:")+
    theme(plot.title = element_text(hjust = 0.5), axis.title = 
              element_text(size = 13), 
          axis.text = element_text(size = 13), title =  
              element_text(size = 13), legend.position="bottom")

# Analise de diagnostico
res<-residuals(m1)
pred<-fitted.values(m1)
df_res <- data.frame(res, pred)

# Normalidade
(qq <- ggplot(data = df_res, aes(sample = res)) +
        stat_qq(alpha = 0.7, size = 3.5, col = "black")  + 
        stat_qq_line(size = 1, col = "red", lty = 1) +
        labs(title = "Grafico Quantil-Quantil da Normal", x = "Quantis Teoricos",
             y = "Quantis Amostrais") +
        theme_classic()+
        theme(plot.title = element_text(hjust = 0.5), axis.title = 
                  element_text(size = 13), 
              axis.text = element_text(size = 13), title =  
                  element_text(size = 15)))

# Testes de normalidade - não rejeita em todos
library(nortest)
lillie.test(res) # Lilliefors
ks.test(res, "pnorm", mean(res), sd(res),alternative='two.sided')
shapiro.test(res) # Shapiro-Wilk
sf.test(res) # Shapiro-Francia
ad.test(res) # Anderson-Darling

# Homocedasticidade
(h <- ggplot(data = df_res) +
        geom_point(aes(x = pred, y = res), alpha = 0.7, size = 3.5, col = "black") +
        geom_line(aes(x = pred, y = 0), col = "Red", size = 1) +
        labs(title = "Grafico Residuos x Preditos", x = "Preditos", y = "Residuos") +
        theme_classic()+
        theme(plot.title = element_text(hjust = 0.5), axis.title = 
                  element_text(size = 13), 
              axis.text = element_text(size = 13), title =  
                  element_text(size = 15)))

# Teste de homocedasticidade - não rejeita em todos
bartlett.test(split(datas$y,list(genero,esporte)))

library(PMCMRplus)
hartleyTest(split(datas$y,list(genero,esporte)))

library(car)
leveneTest(datas$y~esporte*genero, center=mean)
leveneTest(datas$y~esporte*genero, center=median)

# Independência
(ind <- ggplot(data=df_res) +
        geom_point(aes(y = res, x = 1:24), alpha = 0.7, size = 3.5, col = "black") + 
        geom_hline(yintercept=0, colour="red", lwd = 1)+
        labs(title = "Grafico Residuos x Ordem de Coleta", x = "Ordem de Coleta",
             y = "Residuos") +
        theme_classic()+
        theme( plot.title = element_text(hjust = 0.5), axis.title = 
                   element_text(size = 13), 
               axis.text = element_text(size = 13), title =  
                   element_text(size = 15)))

# Comparações múltiplas para o fator Esporte - Método de Tukey
(compar <- TukeyHSD(x=m1, conf.level=0.95))
