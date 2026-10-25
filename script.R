# Scanntech: contexto sobre frequência de compras no varejo. Ajudou a pensar
# na demanda por transações, mas não fornece uma curva de CPU de mainframe.
# https://www.scanntech.com.br/consumidor-passou-a-fazer-compras-menores-e-ir-mais-vezes-aos-mercados-e-atacarejos/
# IBM: processamento em lote, conciliação e backups fora dos horários de pico.
# Serviu de base para a rotina de conciliação durante a madrugada.
# https://www.redbooks.ibm.com/abstracts/redp5719.html
# psutil: significado das métricas do coletor, como CPU por processo, memória
# residente e contadores de leitura/escrita. A CPU de um processo pode passar
# de 100% quando ele usa mais de um processador lógico.
# https://psutil.readthedocs.io/stable/
# Os horários exatos, percentuais e variações dos CSVs são escolhas da simulação,
# não medições nem valores fornecidos por essas fontes.

if (.Platform$OS.type == "windows") {
  invisible(Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8"))
}

pasta <- if (file.exists("MF-SIM-001.csv")) "." else "noctua-r"

componentes <- read.csv(file.path(pasta, "MF-SIM-001.csv"))
servicos <- read.csv(file.path(pasta, "MF-SIM-001_processos.csv"))

componentes$Timestamp <- as.POSIXct(componentes$Timestamp, tz = "America/Sao_Paulo")
servicos$Timestamp <- as.POSIXct(servicos$Timestamp, tz = "America/Sao_Paulo")

componentes <- componentes[order(componentes$Timestamp), ]
componentes$Hora <- as.integer(format(componentes$Timestamp, "%H"))

componentes$Periodo <- cut(
  componentes$Hora,
  breaks = c(0, 6, 12, 18, 24),
  right = FALSE,
  labels = c("00h-06h", "06h-12h", "12h-18h", "18h-24h")
)

quantidade_cpus <- 32
servicos$CPU_Servidor <- servicos$Porcentagem_CPU_Processo / quantidade_cpus

cpu_media_por_servico <- aggregate(CPU_Servidor ~ Nome_Processo, servicos, mean)
ram_media_por_servico <- aggregate(Memoria_MB ~ Nome_Processo, servicos, mean)

io_servicos <- data.frame(Nome = character(), Taxa_MiB_s = numeric())

for (id in 1:4) {
  servico <- servicos[servicos$PID == id, ]
  servico <- servico[order(servico$Timestamp), ]
  
  segundos <- as.numeric(diff(servico$Timestamp), units = "secs")
  bytes <- diff(servico$Bytes_Lidos_Acumulados + servico$Bytes_Escritos_Acumulados)
  taxa_media <- mean(bytes / segundos / 1024^2)
  
  io_servicos <- rbind(
    io_servicos,
    data.frame(Nome = servico$Nome_Processo[1], Taxa_MiB_s = taxa_media)
  )
}

colunas_componentes <- c(
  "Porcentagem_CPU", "Porcentagem_RAM", "Porcentagem_Disco", "Swap"
)

matriz_correlacao <- cor(componentes[colunas_componentes])
colnames(matriz_correlacao) <- rownames(matriz_correlacao) <- c(
  "CPU", "RAM", "Disco", "Swap"
)

correlacao_cpu_ram <- cor(componentes$Porcentagem_CPU, componentes$Porcentagem_RAM)
modelo_servidor <- lm(Porcentagem_RAM ~ Porcentagem_CPU, data = componentes)

roteamento <- servicos[servicos$Nome_Processo == "roteamento_transacoes", ]
correlacao_roteamento <- cor(roteamento$CPU_Servidor, roteamento$Memoria_MB)
modelo_roteamento <- lm(Memoria_MB ~ CPU_Servidor, data = roteamento)

par(mfrow = c(2, 1))

plot(
  componentes$Timestamp, componentes$Porcentagem_CPU,
  type = "l",
  main = "CPU ao longo do dia",
  xlab = "Horário",
  ylab = "CPU (%)"
)

plot(
  componentes$Timestamp, componentes$Porcentagem_RAM,
  type = "l",
  main = "RAM ao longo do dia",
  xlab = "Horário",
  ylab = "RAM (%)"
)

par(mfrow = c(1, 1))

boxplot(
  Porcentagem_CPU ~ Periodo,
  data = componentes,
  main = "CPU por período",
  xlab = "Período",
  ylab = "CPU (%)"
)

hist(
  componentes$Porcentagem_CPU,
  main = "Distribuição da CPU",
  xlab = "CPU (%)",
  ylab = "Quantidade de amostras"
)

par(mar = c(5, 13, 4, 2))

barplot(
  cpu_media_por_servico$CPU_Servidor,
  names.arg = cpu_media_por_servico$Nome_Processo,
  horiz = TRUE,
  las = 1,
  main = "CPU média por serviço",
  xlab = "Contribuição à CPU (p.p.)"
)

barplot(
  ram_media_por_servico$Memoria_MB,
  names.arg = ram_media_por_servico$Nome_Processo,
  horiz = TRUE,
  las = 1,
  main = "Memória média por serviço",
  xlab = "Memória (MiB)"
)

barplot(
  io_servicos$Taxa_MiB_s,
  names.arg = io_servicos$Nome,
  horiz = TRUE,
  las = 1,
  main = "Leitura + escrita por serviço",
  xlab = "Taxa média (MiB/s)"
)

nomes_servicos <- sort(unique(servicos$Nome_Processo))
cores_servicos <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")

par(mar = c(5, 5, 4, 2))

plot(
  servicos$Timestamp, servicos$CPU_Servidor,
  type = "n",
  ylim = c(0, max(servicos$CPU_Servidor) * 1.4),
  main = "CPU por serviço ao longo do dia",
  xlab = "Horário",
  ylab = "Contribuição à CPU (p.p.)"
)

for (i in seq_along(nomes_servicos)) {
  servico <- servicos[servicos$Nome_Processo == nomes_servicos[i], ]
  servico <- servico[order(servico$Timestamp), ]
  
  lines(
    servico$Timestamp, servico$CPU_Servidor,
    col = cores_servicos[i],
    lwd = 1.5
  )
}

legend(
  "topright",
  legend = nomes_servicos,
  col = cores_servicos,
  lty = 1,
  lwd = 1.5,
  bty = "n",
  cex = 0.85
)

plot(
  servicos$Timestamp, servicos$Memoria_MB,
  type = "n",
  ylim = c(0, max(servicos$Memoria_MB) * 1.4),
  main = "RAM por serviço ao longo do dia",
  xlab = "Horário",
  ylab = "Memória (MiB)"
)

for (i in seq_along(nomes_servicos)) {
  servico <- servicos[servicos$Nome_Processo == nomes_servicos[i], ]
  servico <- servico[order(servico$Timestamp), ]
  
  lines(
    servico$Timestamp, servico$Memoria_MB,
    col = cores_servicos[i],
    lwd = 1.5
  )
}

legend(
  "topright",
  legend = nomes_servicos,
  col = cores_servicos,
  lty = 1,
  lwd = 1.5,
  bty = "n",
  cex = 0.85
)

par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

for (i in seq_along(nomes_servicos)) {
  servico <- servicos[servicos$Nome_Processo == nomes_servicos[i], ]
  
  plot(
    servico$CPU_Servidor, servico$Memoria_MB,
    main = nomes_servicos[i],
    xlab = "Contribuição à CPU (p.p.)",
    ylab = "Memória (MiB)",
    col = cores_servicos[i],
    pch = 16,
    cex = 0.5
  )
}

par(mfrow = c(1, 1), mar = c(6, 9, 4, 2))

image(
  1:4, 1:4, matriz_correlacao,
  zlim = c(-1, 1),
  axes = FALSE,
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(100),
  xlab = "",
  ylab = "",
  main = "Correlação entre os componentes (-1 a 1)"
)
axis(1, at = 1:4, labels = colnames(matriz_correlacao), cex.axis = 0.8)
axis(2, at = 1:4, labels = rownames(matriz_correlacao), las = 1, cex.axis = 0.8)

for (i in 1:4) {
  for (j in 1:4) {
    text(
      i, j, round(matriz_correlacao[i, j], 2),
      col = ifelse(abs(matriz_correlacao[i, j]) >= 0.6, "white", "black")
    )
  }
}

par(mar = c(5, 5, 4, 2))

plot(
  componentes$Porcentagem_CPU, componentes$Porcentagem_RAM,
  main = "CPU e RAM do servidor",
  xlab = "CPU (%)",
  ylab = "RAM (%)"
)
abline(modelo_servidor, col = "red")
legend(
  "topleft",
  c(
    paste("r =", round(correlacao_cpu_ram, 3)),
    paste("R² =", round(summary(modelo_servidor)$r.squared, 3))
  )
)

plot(
  roteamento$CPU_Servidor, roteamento$Memoria_MB,
  main = "CPU e memória do roteamento",
  xlab = "CPU do serviço (p.p.)",
  ylab = "Memória (MiB)"
)
abline(modelo_roteamento, col = "red")
legend(
  "topleft",
  c(
    paste("r =", round(correlacao_roteamento, 3)),
    paste("R² =", round(summary(modelo_roteamento)$r.squared, 3))
  )
)
