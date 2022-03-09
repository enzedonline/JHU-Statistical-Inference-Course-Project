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

#===================================================

library(dplyr)
library(ggplot2)
library(kableExtra)
library(moments)

data("ToothGrowth")
str(ToothGrowth)

ToothGrowth <- ToothGrowth %>% mutate(dose = as.factor(dose))

p <- ToothGrowth %>% ggplot(aes(y=len, x=as.factor(dose), fill=supp)) + 
    geom_boxplot() + ggtitle("Tooth Length by Treatment Type and Dosage") +
    xlab("Dosage (mg/day)") + ylab("Tooth Length (mm)") +
    guides(fill=guide_legend(title="Supplement")) + theme_minimal() 

tg_summary <- ToothGrowth %>% group_by(supp, dose) %>%
    summarise(count=n(),min=min(len),q25=quantile(len, 0.25),median=median(len), 
        q75=quantile(len, 0.75),max=max(len),mean=round(mean(len),2),
        sd=round(sd(len),2),skew=round(skewness(len),2)) %>%
    kbl() %>% kable_styling(bootstrap_options = c("condensed"))

gsupp <- ToothGrowth %>% ggplot(aes(x=len)) +
    geom_histogram(alpha = 0.75, bins = 30, aes(y = stat(density)),
                   position="identity", fill="orange", col="darkgrey") +
    geom_line(aes(y = ..density..), colour = 'red', stat = 'density', 
              size = 1.5, alpha = 0.6) + facet_wrap(~supp) +
    ggtitle ("Distribution of Odontoblast Length by Supplement Type") + 
    xlab("Odontoblast Length") + ylab("Density") + theme_minimal() 

gdose <- ToothGrowth %>% ggplot(aes(x=len)) +
    geom_histogram(alpha = 0.75, bins = 30, aes(y = stat(density)),
                   position="identity", fill="orange", col="darkgrey") +
    geom_line(aes(y = ..density..), colour = 'red', stat = 'density', 
              size = 1.5, alpha = 0.6) + facet_wrap(~dose) +
    ggtitle ("Distribution of Odontoblast Length by Dosage Level (mg/day)") + 
    xlab("Odontoblast Length") + ylab("Density") + theme_minimal() 

ToothGrowth %>% group_by(dose) %>% summarise(var(len))
ToothGrowth %>% group_by(supp) %>% summarise(var(len))

td1 <- t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data = subset(ToothGrowth, dose %in% c(0.5, 1)))
td2 <- t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data = subset(ToothGrowth, dose %in% c(1, 2)))
df_dose <- data.frame(td1$p.value, td1$conf.int[1], td1$conf.int[2])
df_dose <- rbind(df_dose, c(td2$p.value, td2$conf.int[1], td2$conf.int[2]))
rownames(df_dose) = c("0.5:1","1:2")
df_dose %>% kbl(col.names = c("P-value", "CI Lower", "CI Upper")) %>% kable_styling(bootstrap_options = c("condensed"), full_width = F, position = "float_right")

tsupp <- t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = ToothGrowth)
tsuppd1 <- t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = subset(ToothGrowth, dose == 0.5))
tsuppd2 <- t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = subset(ToothGrowth, dose == 1))
tsuppd3 <- t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = subset(ToothGrowth, dose == 2))
df_supp <- data.frame(tsupp$p.value, tsupp$conf.int[1], tsupp$conf.int[2])
df_supp <- rbind(df_supp, c(tsuppd1$p.value, tsuppd1$conf.int[1], tsuppd1$conf.int[2]))
df_supp <- rbind(df_supp, c(tsuppd2$p.value, tsuppd2$conf.int[1], tsuppd2$conf.int[2]))
df_supp <- rbind(df_supp, c(tsuppd3$p.value, tsuppd3$conf.int[1], tsuppd3$conf.int[2]))
rownames(df_supp) = c("All dosages", "Dosage 0.5", "Dosage 1", "Dosage 2")
df_supp %>% kbl(col.names = c("P-value", "CI Lower", "CI Upper")) %>% kable_styling(bootstrap_options = c("condensed"), full_width = F, position = "float_right")

