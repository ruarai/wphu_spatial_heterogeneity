



dt <- 0.005
beta <- 0.6
pop_size <- 100000
gamma <- 0.2
alpha <- 0.1
max_t <- 150

states <- tibble(
  S = 0, E = 0, I = 0, R = 0,
  
  .rows = max_t / dt + 1
)

states$S[1] <- pop_size - 10
states$E[1] <- 0
states$I[1] <- 10
states$R[1] <- 0

for(i in 1:(max_t / dt)) {
  S <- states$S[i]
  E <- states$E[i]
  I <- states$I[i]
  R <- states$R[i]
  
  
  StoE <- S * I / pop_size * beta * dt
  EtoI <- alpha * E * dt
  ItoR <- gamma * I * dt
  
  S_new <- S - StoE
  E_new <- E + StoE - EtoI
  I_new <- I + EtoI - ItoR
  R_new <- R + ItoR
  
  states$S[i + 1] <- S_new
  states$E[i + 1] <- E_new
  states$I[i + 1] <- I_new
  states$R[i + 1] <- R_new
  
}

plot_data <- states %>%
  mutate(t = row_number() * dt) %>%
  filter(round(t) == t) %>%
  mutate(
    incidence = lag(S) - S,
    growth_rate = log(incidence) - log(lag(incidence)),
    approx =  lag(S) / pop_size * 0.2 - 0.1,
  ) 

ggplot(plot_data) +
  geom_line(aes(x = t, y = growth_rate)) +
  geom_line(aes(x = t, y = approx), colour = "red")


ggplot(plot_data) +
  geom_line(aes(x = t, y = (I - E) / pop_size))
