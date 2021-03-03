library(cronR)

script <- "/Users/lucas.eosilva/Documentos/Trabalhos com Dalson/Maranhao/dashboards/new/processar-dados.R"
cmd <- cron_rscript(script)
cron_add(command = cmd, 
         at = "11:20PM",
         frequency = 'daily', 
         id = 'gauss-sesma-covid', 
         description = 'Dados SESMA')

