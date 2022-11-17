



dt <- 0.005
beta <- 0.9
pop_size <- 100000
gamma <- 0.2
max_t <- 100

states <- tibble(
  S = 0, I = 0, R = 0,
  
  .rows = max_t / dt + 1
)

states$S[1] <- pop_size - 10
states$I[1] <- 10
states$R[1] <- 0

for(i in 1:(max_t / dt)) {
  S <- states$S[i]
  I <- states$I[i]
  R <- states$R[i]
  
  StoI <- S * I / pop_size * beta * dt
  ItoR <- gamma * I * dt
  
  S_new <- S - StoI
  I_new <- I + StoI - ItoR
  
  R_new <- R + ItoR
  
  states$S[i + 1] <- S_new
  states$I[i + 1] <- I_new
  states$R[i + 1] <- R_new
  
}

plot_data <- states %>%
  mutate(t = row_number() * dt) %>%
  filter(round(t) == t) %>%
  mutate(
    incidence = lag(S) - S,
    growth_rate = log(incidence) - log(lag(incidence)),
    approx =  S / pop_size * beta - gamma,
    approx_2 =  lag(S) / pop_size * beta - beta * lag(I) / pop_size - gamma,
    approx_3 =  lag(S - I) / pop_size * beta - gamma,
  ) 

ggplot(plot_data) +
  geom_line(aes(x = t, y = incidence))

ggplot(plot_data) +
  geom_line(aes(x = t, y = growth_rate)) +
  geom_line(aes(x = t, y = approx), colour = "red") +
  geom_line(aes(x = t, y = approx_2), colour = "blue") +
  geom_line(aes(x = t, y = approx_3), colour = "green", linetype = "dashed")

            