hist(runif(1000))

mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)

library(ggplot2)
library(kableExtra)
library(ggthemes)

seed(123456)
count_sims <- 1000
lambda <-  0.2 
n <- 40  

simulation <- matrix(rexp(n = count_sims * n, rate = lambda), count_sims, n)
mean_sim_rows <- rowMeans(simulation)

df_tbl <- data.frame("Mean"=c(mean(mean_sim_rows), 1/ lambda))
rownames(df_tbl) = c("Simulation","Theoretical")
tbl <- df_tbl %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"), full_width = F)

df_plot <- as.data.frame(mean_sim_rows)

g <- ggplot(df_plot, aes(mean_sim_rows)) +
    theme_minimal() +
    geom_histogram(alpha = 0.75, bins = 30, aes(y = stat(density)),
        position="identity",
        fill="orange", col="darkgrey") +
    geom_line(aes(y = ..density..), colour = 'red', stat = 'density', size = 1.5, alpha = 0.6) +  
    geom_vline(
        aes(xintercept = 1/ lambda, colour="Theoretical")) +
    geom_vline(
        aes(xintercept = mean(mean_sim_rows), colour="Sample")) +
    ggtitle ("Sample Means ") + 
    xlab("Sample mean") +
    ylab("Density") + 
    scale_color_manual(name = "Means", values = c(Theoretical = "blue", Sample = "green")) +
    theme(plot.margin = margin(0, 0, 0.5, 0.5, "cm"))

actual_variance <- var(mean_sim_rows) 

theoretical_variance <- (1/ lambda)^2 / n 

result2 <-data.frame("Variance"=c(actual_variance, theoretical_variance), 
                     row.names = c("Variance from the sample ","Theoretical variance"))


ggplot(df_plot, aes(mean_sim_rows)) +
    theme_minimal() +
    stat_function(fun = dnorm, aes(colour = "Theoretical"), size = 1.5, 
        args = list(mean = (1/ lambda), sd = sqrt((1/ lambda)^2 / n )))+
    geom_density(aes(colour="Sample"), size = 1.5)+
    ggtitle ("Sample Means Distribution vs Normal") + xlab("Sample mean") + ylab("Density") + 
    scale_color_manual(name = "Means", values = c(Theoretical = "blue", Sample = "green")) +
    theme(plot.margin = margin(0, 0, 0.5, 0.5, "cm"))

median(mean_sim_rows)
skewness(mean_sim_rows)

ggplot(df_plot, aes(sample = mean_sim_rows)) +
    theme_minimal() +
    ggtitle ("Normal Q-Q Plot") + xlab("Theoretical Quantiles") + ylab("Sample Quantiles") + 
    stat_qq(colour = "green", alpha = 0.3, size = 2.5) +
    stat_qq_line(colour = "blue")
