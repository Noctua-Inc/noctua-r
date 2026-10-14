# install.packages("tidyverse")

library(tidyverse)
library(ggplot2)

df_componentes <- rbind(MF.ALPHA.002)

# Transformando o campo Timestamp de texto para horário
df_componentes$Timestamp <- as.POSIXct(df_componentes$Timestamp, format="%Y-%m-%d %H:%M:%S")
# Inserindo uma nova coluna no dataframe, sendo apenas as horas de cada registro
df_componentes$Hora <- format(df_componentes$Timestamp, "%H")
# Inserindo uma nova coluna no dataframe, sendo apenas os dias da semana de cada registro
df_componentes$DiaSemana <- weekdays(df_componentes$Timestamp)

media_cpu_hora <- aggregate(Porcentagem_CPU ~ Hora, data = df_componentes, FUN = mean)
media_cpu_dia <- aggregate(Porcentagem_CPU ~ DiaSemana, data = df_componentes, FUN = mean)

media_freq_cpu_hora <- aggregate(Freq_CPU ~ Hora, data = df_componentes, FUN = mean)
media_freq_cpu_dia <- aggregate(Freq_CPU ~ DiaSemana, data = df_componentes, FUN = mean)

media_ram_hora <- aggregate(Porcentagem_RAM ~ Hora, data = df_componentes, FUN = mean)
media_ram_dia <- aggregate(Porcentagem_RAM ~ DiaSemana, data = df_componentes, FUN = mean)

media_swap_hora <- aggregate(Swap ~ Hora, data = df_componentes, FUN = mean)
media_swap_dia <- aggregate(Swap ~ DiaSemana, data = df_componentes, FUN = mean)

media_disco_hora <- aggregate(Porcentagem_Disco ~ Hora, data = df_componentes, FUN = mean)
media_disco_dia <- aggregate(Porcentagem_Disco ~ DiaSemana, data = df_componentes, FUN = mean)

# Gráficos da porcentagem de CPU
plot(media_cpu_hora$Hora, media_cpu_hora$Porcentagem_CPU, 
     type = "b", 
     col = "blue", 
     xlab = "Hora do Dia", 
     ylab = "Média % CPU", 
     main = "Consumo Médio de CPU por Hora")

barplot(media_cpu_dia$Porcentagem_CPU, 
        names.arg = media_cpu_dia$DiaSemana, 
        col = "orange", 
        ylab = "Média % CPU", 
        main = "Consumo Médio de CPU por Dia da Semana")

# Gráficos da freqeência de CPU
plot(media_freq_cpu_hora$Hora, media_cpu_hora$Freq_CPU, 
     type = "b", 
     col = "blue", 
     xlab = "Hora do Dia", 
     ylab = "Média Freq CPU", 
     main = "Consumo Médio da Frequência de CPU por Hora")

barplot(media_freq_cpu_dia$Freq_CPU, 
        names.arg = media_cpu_dia$DiaSemana, 
        col = "orange", 
        ylab = "Média Freq CPU", 
        main = "Consumo Médio da Frequência de CPU por Dia da Semana")

# Gráficos da porcentagem de RAM
plot(media_ram_hora$Hora, media_ram_hora$Porcentagem_RAM, 
     type = "b", 
     col = "blue", 
     xlab = "Hora do Dia", 
     ylab = "Média % RAM", 
     main = "Consumo Médio de RAM por Hora")

barplot(media_ram_dia$Porcentagem_RAM, 
        names.arg = media_ram_dia$DiaSemana, 
        col = "orange", 
        ylab = "Média % RAM", 
        main = "Consumo Médio de RAM por Dia da Semana")

# Gráficos do consumo de SWAP
plot(media_swap_hora$Hora, media_swap_hora$Swap, 
     type = "b", 
     col = "blue", 
     xlab = "Hora do Dia", 
     ylab = "Média % SWAP", 
     main = "Consumo Médio de SWAP por Hora")

barplot(media_swap_dia$Swap, 
        names.arg = media_swap_dia$DiaSemana, 
        col = "orange", 
        ylab = "Média % SWAP", 
        main = "Consumo Médio de SWAP por Dia da Semana")

# Gráficos da porcentagem de disco
plot(media_swap_hora$Hora, media_swap_hora$Swap, 
     type = "b", 
     col = "blue", 
     xlab = "Hora do Dia", 
     ylab = "Média % Disco", 
     main = "Consumo Médio de Disco por Hora")

barplot(media_swap_dia$Swap, 
        names.arg = media_swap_dia$DiaSemana, 
        col = "orange", 
        ylab = "Média % Disco", 
        main = "Consumo Médio de Disco por Dia da Semana")

# Verificando correlação e regressão entre CPU e RAM
cor(df_componentes$Porcentagem_RAM, df_componentes$Porcentagem_CPU)

lm(df_componentes$Porcentagem_RAM ~ df_componentes$Porcentagem_CPU)

summary(lm(df_componentes$Porcentagem_RAM ~ df_componentes$Porcentagem_CPU))

ggplot(df_componentes, aes(Porcentagem_RAM, Porcentagem_CPU)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


# Verificando correlação e regressão entre Swap e RAM
cor(df_componentes$Swap, df_componentes$Porcentagem_RAM)

lm(df_componentes$Swap ~ df_componentes$Porcentagem_RAM)

summary(lm(df_componentes$Swap ~ df_componentes$Porcentagem_RAM))

ggplot(df_componentes, aes(Swap, Porcentagem_RAM)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)