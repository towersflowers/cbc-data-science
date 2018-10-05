# Script R para refrescar BBDD Maestras

#Instalo Paquetes y Ayudas
#install.packages("openssl")
#install.packages("httr")
#install.packages("xml2")
#install.packages("googlesheets")
#install.packages("dplyr")
help("read.csv2")

# Variable Globl
timestamp <- paste(format(Sys.time(),"%d-%m-%Y"),"Semana",format(Sys.time(), "%W"))

# Cargo librerias
library("dplyr")
library("googlesheets")

gs_auth(new_user = TRUE) #4/bgBWtNMwzDiGMb0YnQpAZ0QQXgHVzkwmn70ICDjI18oEan6kd8zpJ1c

# Leo las Bases de Datos y las guardo en las matrices correspondientes (Los csv deben estar separados por ";")
base_Clientes_dist <- read.csv2("BBDD/Maestra Clientes Dist.csv", header = TRUE, sep = ";")
base_productos <- read.csv2("BBDD/BBDD sku.csv",header = TRUE, sep = ";")
base_Ventas_BV <-read.csv2("BBDD/2015-2018m9.csv", header = TRUE, sep = ";")

# Extraigo los clientes de Dist.
base_Ventas_BV_dist <- base_Ventas_BV %>% filter(Tipo.Documento == "factura electrónica")

# Dejo un rut por cliente & Dejo Sku unico por producto
base_Clientes_BV_dist <- base_Ventas_BV_dist %>% group_by(Rut.Cliente) %>% summarise()
base_Clientes_dist <- base_Clientes_dist %>% group_by(Rut.Cliente) %>% summarise()

base_SKU_BV <- base_Ventas_BV %>% group_by(sku) %>% summarise()
base_SKU <- base_productos %>% group_by(sku.numero) %>% summarise()

# Calculo SKU que tengan costo Cero
base_Costo_zero <- base_Ventas_BV %>% filter(Costo.Neto.Unitario == "0")
#ase_Costo_zero <- base_Costo_zero[!duplicated(base_Costo_zero$sku),]
base_Costo_zero <- base_Costo_zero %>% filter(Tipo.de.Producto.o.Servicio != "Sin Tipo")

# Saco los puntos a los Rut de CLientes en el paño de Ventas (importante poner \\)
base_Clientes_BV_dist$Rut.Cliente <- gsub('\\.', '', base_Clientes_BV_dist$Rut.Cliente)

# Paso todo a Data.Frame
base_Clientes_BV_dist <- as.data.frame(base_Clientes_BV_dist)
base_Clientes_dist <- as.data.frame(base_Clientes_dist)
base_SKU_BV <- as.data.frame(base_SKU_BV)
base_SKU <- as.data.frame(base_SKU)

# Paso Columan a Factor para igualar formato de la otra base
base_Clientes_BV_dist$Rut.Cliente <- factor(base_Clientes_BV_dist$Rut.Cliente)
base_SKU_BV$SKU <- factor(base_SKU_BV$sku)

# Reviso y comparo Rut.Cliente 
clientes_por_revisar <- data.frame(c(0))
colnames(clientes_por_revisar) <- "Rut Clientes No Encontrados"
a <- 0
for(i in 1:length(base_Clientes_BV_dist$Rut.Cliente)){
  if(base_Clientes_BV_dist[i,1] %in% base_Clientes_dist$Rut.Cliente)
    {a <- a + 1}else {
      clientes_por_revisar <- rbind(clientes_por_revisar, c(toString(base_Clientes_BV_dist[i,1])))
    }
}

#clientes_por_revisar <- clientes_por_revisar[-c(1:1),] # ELimino la primera fila que puse al crear el Data Frame

# Reviso y comparo Sku productos
sku_por_revisar <- data.frame(c(0))
colnames(sku_por_revisar) <- "Sku No Encontrados"
b <- 0
for(i in 1:length(base_SKU_BV$SKU)){
  if(base_SKU_BV[i,1] %in% base_SKU$sku)
  {
    b <- b + 1
  }else {
    sku_por_revisar <- rbind(sku_por_revisar, c(toString(base_SKU_BV[i,1])))
  }
}
#sku_por_revisar <- sku_por_revisar[-c(1:1),] # ELimino la primera fila que puse al crear el Data Frame

# Escribo CSV con los Clientes no Encontrados & los Sku no encontrados
#write.csv2(clientes_por_revisar, file = paste("Clientes No Encontrados Dist",timestamp,".csv"), row.names=FALSE)
#write.csv2(sku_por_revisar, file = paste("Sku No encontrados",timestamp,".csv"), row.names=FALSE) 

# Rescato del Paño de Venta los clientes no encontrados
clientes_por_revisar2 <- data.frame(base_Ventas_BV[1,])
base_Ventas_BV$Rut.Cliente <- gsub('\\.', '', base_Ventas_BV$Rut.Cliente)
clientes_por_revisar2 <- base_Ventas_BV %>% filter(base_Ventas_BV$Rut.Cliente %in% clientes_por_revisar$`Rut Clientes No Encontrados`)
clientes_por_revisar2 <- clientes_por_revisar2[-c(1:1),] # ELimino la primera fila que puse al crear el Data Frame
clientes_por_revisar2 <- clientes_por_revisar2[!duplicated(clientes_por_revisar2$Rut.Cliente),]

# Rescato del paño de Venta los sku no encontrados
sku_por_revisar2 <- data.frame(base_Ventas_BV[1,])
sku_por_revisar2 <- base_Ventas_BV %>% filter(base_Ventas_BV$sku %in% sku_por_revisar$`Sku No Encontrados`)
sku_por_revisar2 <- sku_por_revisar2[-c(1:1),] # ELimino la primera fila que puse al crear el Data Frame
sku_por_revisar2 <- sku_por_revisar2[!duplicated(sku_por_revisar2$sku),] # Elimino Duplicados

# Escribo CSV con los Clientes & los Sku recuperados
#write.csv2(clientes_por_revisar2, file = paste("clientes no Encontrados",timestamp,".csv"), row.names=FALSE)
#write.csv2(sku_por_revisar2, file = paste("sku no Encontrados",timestamp,".csv"), row.names=FALSE) 
#write.csv2(base_Costo_zero, file = paste("Sku Costeados en Zero",timestamp,".csv"), row.names = FALSE)

# Imprimir Indicadores

print(paste("Estan registrados el",a/length(base_Clientes_BV_dist$Rut.Cliente)*100,"% de los Clientes, Y Estan Documentados el ",b/length(base_SKU_BV$SKU)*100,"% de los Sku."))

# Datos A Google Drive
clientes_por_revisar2 <- clientes_por_revisar2 %>% select(one_of(c("Sucursal","Vendedor","Cliente","Rut.Cliente","date"))) %>% filter(clientes_por_revisar2$Sucursal != "Los Leones")
sku_por_revisar2 <- sku_por_revisar2 %>% filter(sku_por_revisar2$Tipo.de.Producto.o.Servicio != "Sin Tipo") %>% select(one_of(c("date","Producto.Servicio","sku","Variante")))
base_Costo_zero$date <- as.Date(base_Costo_zero$date, "%Y-%m-%d")
base_Costo_zero <- base_Costo_zero %>% select(one_of(c("date","Sucursal","sku"))) %>% filter(base_Costo_zero$date >= "2018-05-1")

# Interaccon con Google Drive
gs_auth(new_user = TRUE) #4/bwADCt93GXw6zDdtNsdQpFvRva2_79HwMbPgj54gi_hKNSE3xoRgSNw
#drive <- gs_ls()
for_gs <- gs_key("1skTjmyKBoWSOl_MUtMFPknnG6pIDUF_tVzFrncQYWR0")

gs_edit_cells(for_gs, ws = "Lista de clientes no Encontrados", anchor = "A1", input = clientes_por_revisar2, byrow = TRUE)
gs_edit_cells(for_gs, ws = "Lista de Sku no Encontrados", anchor = "A1", input = sku_por_revisar2, byrow = TRUE)
gs_edit_cells(for_gs, ws = "Lista de Sku no Costeados", anchor = "A1", input = base_Costo_zero, byrow = TRUE) 
