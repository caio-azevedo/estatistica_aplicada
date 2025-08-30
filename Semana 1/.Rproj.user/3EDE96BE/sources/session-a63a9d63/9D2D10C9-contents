rm(list = ls())

# Pacotes utilizados
library(tidyverse) #carregar outros pacotes do R
library(knitr) #formatação de tabelas
library(kableExtra) #formatação de tabelas
library(sjPlot) #tabelas de contingência
library(DescTools) #diferentes medidas de associação
library(vcd) #diferentes medidas de associação
library(lsr) #coeficiente V de Cramer
library(rcompanion) #coeficiente V de Cramer
library(correlation) #gráfico da correlação de Pearson
library(PerformanceAnalytics) #gráfico da correlação de Pearson com histogramas
library(plotly) #plataforma gráfica

# Carregamento da base de dados 'PlanoSaude'
load(file = "Inadimplência.RData")

attach(Inadimplência)

# Tabela de contingência
sjt.xtab(var.row = faixa_etaria,
         var.col = inadimplencia)

# Tabela de contingência - frequências relativas em relação ao total geral
sjt.xtab(var.row = faixa_etaria,
         var.col = inadimplencia,
         show.obs = FALSE,
         show.cell.prc = TRUE)

# Tabela de contingência - frequências relativas em relação ao total da linha
sjt.xtab(var.row = faixa_etaria,
         var.col = inadimplencia,
         show.obs = FALSE,
         show.row.prc = TRUE)

# Tabela de contingência - frequências relativas em relação ao total da coluna
sjt.xtab(var.row = faixa_etaria,
         var.col = inadimplencia,
         show.obs = FALSE,
         show.col.prc = TRUE)

# Tabela de contingência - frequências esperadas
sjt.xtab(var.row = faixa_etaria,
         var.col = inadimplencia,
         show.obs = FALSE,
         show.exp = TRUE)

# Medida de associação - estatística qui-quadrado e teste
qui2 <- chisq.test(faixa_etaria, inadimplencia)

# Valores esperados (mais aproximados)
tab_esp <- qui2$expected

addmargins(tab_esp)

# Outra forma - Tabela de contingência
tab_abs <- table(faixa_etaria, inadimplencia)

tab_abs

# Adicionar margens
tab_abs_margem <- addmargins(tab_abs)

# Mudando de "Sum" para "Total"

rownames(tab_abs_margem)[rownames(tab_abs_margem)=="Sum"] <- "Total"
colnames(tab_abs_margem)[colnames(tab_abs_margem)=="Sum"] <- "Total"

# Visualizar a tabela
tab_abs_margem

# Tabela de frequência relativa pelo total geral
tab_prop_all <- round(prop.table(tab_abs),3)

# Adicionado margens

tab_prop_all <- addmargins(tab_prop_all)

# Mudando de "Sum" para "Total"

rownames(tab_prop_all)[rownames(tab_prop_all)=="Sum"] <- "Total"
colnames(tab_prop_all)[colnames(tab_prop_all)=="Sum"] <- "Total"

# Tabela de frequência relativa pelo total da linha - margin = 1
tab_prop_row <- round(prop.table(tab_abs, margin = 1),3)
tab_prop_row

# Soma de cada coluna (sempre = 1.00 no caso de proporção)
col_totais <- colSums(tab_prop_all)

# Adicionar linha "Total"
tab_prop_row <- rbind(tab_prop_row, Total = col_totais)

# Transformando em Tabela
tab_prop_row <- as.table(tab_prop_row)

# Tabela de frequência relativa pelo total da linha - margin = 2
tab_prop_col <- round(prop.table(tab_abs, margin = 2),3)
tab_prop_col

# Soma de cada coluna (sempre = 1.00 no caso de proporção)
row_totais <- rowSums(tab_prop_all)

# Adicionar linha "Total"
tab_prop_col <- cbind(tab_prop_col, Total = row_totais)

# Transformando em Tabela
tab_prop_col <- as.table(tab_prop_col)

# Resolvendo letra b)

tab_prop_all["31 a 40", "Total"]

# letra c)

tab_prop_all["Total", "muito endividado"]

# letra d)
tab_prop_all["ate 20", "nao tem dividas"]

# letra e)
tab_prop_row["acima de 60", "pouco endividado"]

# letra f)
tab_prop_col["41 a 50", "mais ou menos endividado"]

# letra g)
tab_prop_row

# letra h)
qui2$statistic

# letra i)
# Coeficiente Phi
sqrt(qui2$statistic/nrow(Inadimplência))

# Coeficiente de contingência
ContCoef(faixa_etaria, inadimplencia)

# Coeficiente V de Cramer
cramerV(faixa_etaria, inadimplencia)

