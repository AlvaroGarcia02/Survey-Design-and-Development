#==============================================================================
#===    ESTRATIFICACIÓN MEDIANTE LA REGLA GEOMÉTRICA DE GUNNING-HORGAN      ===
#==============================================================================
#===       Diseño y desarrollo de encuestas - Álvaro A. García G.           ===
#===                              2025-2                                    ===
#==============================================================================

rm(list = ls())

library(stratification)

# (1) Simulación de número de empleados:
set.seed(2020)
N = 1000
x <- round(rlnorm(N, meanlog = 4, sdlog = 1.2), 0)
hist(x)
summary(x)

# (2) Simulación de valor de ventas mensuales:
set.seed(2021)
y <- 30*x+20000+rnorm(1000, mean = 0, sd = 5000)
summary(y)
cor(x,y)
modelo <- lm(y ~ x)
plot(x, y)
abline(modelo, col = "blue", lwd = 2)

# (3) Estratificación en 3 estratos con un estrato take-all para las empresas
# más grandes y CVM deseado del 5%:
M <- strata.geo(x = x,
                Ls = 3, #número total de estratos
                n = 90, #tamaño total de muestra
                alloc = list(q1 = 0.5, q2 = 0, q3 = 0)) #afijación proporcional

# (4) Resultados:
bounds <- M$bh; bounds #límites de los estratos
Ne <- M$Nh; Ne #tamaños poblacionales de cada estrato
sizes <- M$nh; sizes #tamaños de muestra en cada estrato

# (5) Gráfico:
x.ord <- order(x)
x_sorted <- x[x.ord]
col <- ifelse(x_sorted <= bounds[1], "blue",
              ifelse(x_sorted <= bounds[2], "orange", "red"))
plot(x_sorted,
     col = col,
     pch = 16, cex = 0.5,
     xlab = "Empresas ordenadas",
     ylab = "Número de empleados",
     main = "")
abline(h = bounds[1], lty = 2, lwd = 2)  # Límite estrato 1
abline(h = bounds[2], lty = 3, lwd = 2)  # Límite estrato 2
legend("topleft",
       legend = c(bquote(b[1] * " = " * .(bounds[1])),
                  bquote(b[2] * " = " * .(bounds[2]))),
                  lty = c(2,3), lwd = 2, bty = "n")

# (6) Muestreo aleatorio simple dentro de los estratos:
U1 <- x[x<=bounds[1]]; U1
U2 <- x[x>bounds[1] & x<=bounds[2]]; U2
U3 <- x[x>bounds[2]]; U3
set.seed(2022)
idx1 <- sample(seq_along(U1), size = sizes[1], replace = FALSE); idx2 <- sample(seq_along(U2), size = sizes[2], replace = FALSE)

# (7) Simulación de la aplicación de la encuesta:
yU1 <- y[idx1]
yU2 <- y[idx2+Ne[1]]
yU3 <- y[(Ne[1]+1+Ne[2]):1000]

# (8) Estimación de la media de ventas mensuales:
pi.estimator = Ne[1]*mean(yU1)+Ne[2]*mean(yU2)+Ne[3]*mean(yU3)
mean.est = pi.estimator/N; mean.est
mean(y) #Media conocida en virtud de la simulación

# (9) CVM alcanzado:
var.pi.estimator = (Ne[1])^2*((1-sizes[1]/Ne[1])/sizes[1])*var(yU1)+(Ne[2])^2*((1-sizes[2]/Ne[2])/sizes[2])*var(yU2)
sqrt(var.pi.estimator)/pi.estimator

# (10) Variantes:

#     (a) Cálculo con afijación de Neyman (default):  
M <- strata.geo(x = x,
                Ls = 3, #número total de estratos
                n = 90) #tamaño total de muestra
bounds <- M$bh; bounds #límites de los estratos
Ne <- M$Nh; Ne #tamaños poblacionales de cada estrato
sizes <- M$nh; sizes #tamaños de muestra en cada estrato
U1 <- x[x<=bounds[1]]; U1
U2 <- x[x>bounds[1] & x<=bounds[2]]; U2
U3 <- x[x>bounds[2]]; U3
set.seed(2022)
idx1 <- sample(seq_along(U1), size = sizes[1], replace = FALSE); idx2 <- sample(seq_along(U2), size = sizes[2], replace = FALSE)
yU1 <- y[idx1]
yU2 <- y[idx2+Ne[1]]
yU3 <- y[(Ne[1]+1+Ne[2]):1000]
pi.estimator = Ne[1]*mean(yU1)+Ne[2]*mean(yU2)+Ne[3]*mean(yU3)
mean.est = pi.estimator/N; mean.est
var.pi.estimator = (Ne[1])^2*((1-sizes[1]/Ne[1])/sizes[1])*var(yU1)+(Ne[2])^2*((1-sizes[2]/Ne[2])/sizes[2])*var(yU2)
sqrt(var.pi.estimator)/pi.estimator

#     (b) Cálculo con afijación de potencia (p=1):  
M <- strata.geo(x = x,
                Ls = 3, #número total de estratos
                n = 90, #tamaño total de muestra
                alloc = list(q1 = 0.5, q2 = 0.5, q3 = 0)) #afijación de potencia) 
bounds <- M$bh; bounds #límites de los estratos
Ne <- M$Nh; Ne #tamaños poblacionales de cada estrato
sizes <- M$nh; sizes #tamaños de muestra en cada estrato
U1 <- x[x<=bounds[1]]; U1
U2 <- x[x>bounds[1] & x<=bounds[2]]; U2
U3 <- x[x>bounds[2]]; U3
set.seed(2022)
idx1 <- sample(seq_along(U1), size = sizes[1], replace = FALSE); idx2 <- sample(seq_along(U2), size = sizes[2], replace = FALSE)
yU1 <- y[idx1]
yU2 <- y[idx2+Ne[1]]
yU3 <- y[(Ne[1]+1+Ne[2]):1000]
pi.estimator = Ne[1]*mean(yU1)+Ne[2]*mean(yU2)+Ne[3]*mean(yU3)
mean.est = pi.estimator/N; mean.est
var.pi.estimator = (Ne[1])^2*((1-sizes[1]/Ne[1])/sizes[1])*var(yU1)+(Ne[2])^2*((1-sizes[2]/Ne[2])/sizes[2])*var(yU2)
sqrt(var.pi.estimator)/pi.estimator

