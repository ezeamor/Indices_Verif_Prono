# Indices_Verif_Prono

####################################################

# INDICES PARA VERIFICACION DE PRONOSTICOS:
# METODOS PARA PRONOSTICOS PROBABILISTICOS.

####################################################

graphics.off() # Elimina confifuracion de graficos previos.
setwd("/home/ezequiel.amor")

# Liberia a usar en este programa.

library(verification)

# En "attach" cambiar el nombre del dataframe segun cual se necesite usar. Esto 
# permite que no haya que indicar a que dataframe pertencen los datos y que luego 
# solo se ponga el nombre de las columnas con la informacion necesaria para trabajar.
# (Ejemplo: en vez de usar "Probabilidad$Prob_1mm" se usa solo "Prob_1mm").

attach(Ocurrencias_estaciones)
attach(Probabilidad)

####################################################

# DIAGRAMA DE CONFIABILIDAD: RELIABILITY PLOT.

####################################################

# Primero con la funcion "verify" obtengo los datos de y.i, obar.i y de prob.y
# que voy a usar en el diagrama de confiabilidad.

A <- verify(Ocurrencia_PP_1mm,Prob_1mm,baseline=Prob_1mm_clima,frcst.type="prob",obs.type="binary")
B <- verify(Ocurrencia_PP_20mm,Prob_20mm,baseline=Prob_20mm_clima,frcst.type="prob",obs.type="binary")
C <- verify(Ocurrencia_PP_50mm,Prob_50mm,baseline=Prob_50mm_clima,frcst.type="prob",obs.type="binary")
D <- verify(Ocurrencia_PP_100mm,Prob_100mm,baseline=Prob_100mm_clima,frcst.type="prob",obs.type="binary")

# Veo los graficos como prueba.

reliability.plot(A$y.i,A$obar.i,A$prob.y,titl="Diagrama de confiabilidad")
reliability.plot(B$y.i,B$obar.i,B$prob.y,titl="Diagrama de confiabilidad")
reliability.plot(C$y.i,C$obar.i,C$prob.y,titl="Diagrama de confiabilidad")
reliability.plot(D$y.i,D$obar.i,D$prob.y,titl="Diagrama de confiabilidad")

# Luego en el programa "graficos" hago todos los plots juntos y con mas detalles.

####################################################

# BRIER SCORE Y OTROS PUNTAJES.

####################################################

# Primero hago el calculo de todos los scores y otras caracteristicas para cada 
# categoria de acumulado.

BS_1mm   <- brier(Ocu_1mm_Lote16,Prob_1mm,Prob_1mm_clima)
BS_20mm  <- brier(Ocu_20mm_Lote16,Prob_20mm,Prob_20mm_clima)
BS_50mm  <- brier(Ocu_50mm_Lote16,Prob_50mm,Prob_50mm_clima)
BS_100mm <- brier(Ocu_100mm_Lote16,Prob_100mm,Prob_100mm_clima)

# Ahora leo los scores que necesito:

# Brier Score y Baseline

print(BS_1mm$bs)
print(BS_1mm$bs.baseline)

print(BS_20mm$bs)
print(BS_20mm$bs.baseline)

print(BS_50mm$bs)
print(BS_50mm$bs.baseline)

print(BS_100mm$bs)
print(BS_100mm$bs.baseline)

# Brier Skill Score.

print(BS_1mm$ss)
print(BS_20mm$ss)
print(BS_50mm$ss)
print(BS_100mm$ss)

# Ahora calculo las diferentes componenetes de los scores:

# Componente Confiabilidad.

print(BS_1mm$bs.reliability)
print(BS_20mm$bs.reliability)
print(BS_50mm$bs.reliability)
print(BS_100mm$bs.reliability)

# Componenete Resolucion.

print(BS_1mm$bs.resol)
print(BS_20mm$bs.resol)
print(BS_50mm$bs.resol)
print(BS_100mm$bs.resol)

# Componenete Incertidumbre.

print(BS_1mm$bs - BS_1mm$bs.reliability + BS_1mm$bs.resol)
print(BS_20mm$bs - BS_20mm$bs.reliability + BS_20mm$bs.resol)
print(BS_50mm$bs - BS_50mm$bs.reliability + BS_50mm$bs.resol)
print(BS_100mm$bs - BS_100mm$bs.reliability + BS_100mm$bs.resol)

####################################################

# CARACTERISTICA OPERATIVA RELATIVA: CURVA ROC. 

####################################################

# Guardo la informacion de las curvas en variables, luego voy a plotearlas todas
# juntas y con mas detalles en el programa "graficos".

Z1 <- roc.plot(Ocurrencia_1mm,Prob_1mm,main="Curva ROC 1 mm")
Z2 <- roc.plot(Ocurrencia_20mm,Prob_20mm,main="Curva ROC 20 mm")
Z3 <- roc.plot(Ocurrencia_50mm,Prob_50mm,main="Curva ROC 50 mm")
Z4 <- roc.plot(Ocurrencia_100mm,Prob_100mm,main="Curva ROC 100 mm")

# Calculo tambien el area bajo la curva.

roc.area(Ocurrencia_PP_1mm,Prob_1mm)
roc.area(Ocurrencia_PP_20mm,Prob_20mm)
roc.area(Ocurrencia_PP_50mm,Prob_50mm)
roc.area(Ocurrencia_PP_100mm,Prob_100mm)
