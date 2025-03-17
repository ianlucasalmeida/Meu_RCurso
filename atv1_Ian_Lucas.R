X <- 133
Y <- 36

Z <- X + Y   # Z = 133 + 36 = 169

raiz_Z <- sqrt(Z)   # sqrt(169) = 13

log_raiz <- log(raiz_Z)   # ln(13) ≈ 2.564949

resultado <- round(log_raiz, 0)   # 2.564949 → 3

# atv_2-questão fibonacci

# Sequência de Fibonacci (5 primeiros termos começando em 0)
fibonacci <- c(0, 1, 1, 2, 3)
soma <- sum(fibonacci)           # Soma = 7
log_soma <- log(soma)            # ln(7) ≈ 1.94591
resultado <- round(log_soma, 3)  # Arredonda para 1.946

print(resultado)  # Saída: 1.946


#q3_Calculo da derivada

derivada <- cos(2) + 1        # Calcula f'(2) ≈ 0.5838532
resultado <- round(derivada, 3)  # Arredonda para 0.584

print(resultado)  # Saída: 0.584

#q4_factorial

fatorial <- factorial(10)      # Calcula 10! = 3628800
log_fatorial <- log10(fatorial)  # log10(3628800) ≈ 6.560305
resultado <- round(log_fatorial, 2)  # Arredonda para 6.56

print(resultado)  # Saída: 6.56

#q5_velocidade da luz

m <- 10
c <- 3e8  # Velocidade da luz em m/s
E <- m * c^2
log_E <- log(E)
resultado <- round(log_E, 2)

print(resultado)  # Saída: 41.34

#q6_vetor

v <- c(2, 5, 7, 8, 9)
resultados <- v^3 + v^2 + v
media <- mean(resultados)
print(media)  # Saída: 394.2
