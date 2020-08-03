## figure 1:
## panel of plots
## 1: ternary plot foraging, repair, repro (last below due to clashes with ggplot2)
## 2: age distribution
## 3: damage distribution
## 4: freq deleterious alleles by damage
## 5: damage vs. age
## 6: mortality (damage-caused) by age

library(tidyverse)
library(cowplot)
library(mgcv)

# read data (last generation pop)
d <- read.table("pop_sym.txt")
names(d) <- c("age","rep0","rep1","rpr0","rpr1","har0","har1",
              "tra0","tra1","dam0","dam1","res0","res1",
              paste0("v0_",0:15),paste0("v1_",0:15),"dead")

d2 <- read.table("evol_sym.txt",header = F)
names(d2) <- c("generation","popsize","age","rep0","rep1","rpr0",
               "rpr1","har0","har1","tra0","tra1","dam0","dam1","res0","res1",
               paste0("v0_",0:15),paste0("v1_",0:15))
d2 <- mutate(d2, for0 = 1-rep0-rpr0,
             for1 = 1-rep1-rpr1)

d3 <- with(d2, data.frame(y = c(for0,for1,rpr0,rpr1,rep0,rep1)))
d3$cycle <- rep(d2$generation,6)
d3$trait <- factor(rep(c("forage","repair","repro"), each = 2*nrow(d2)))
d3$cell <- factor(rep(c(1,2),each=nrow(d2),times=3))

##

my_font_size <- 16

## fig1A: no tern because can't combine with cowplot :-(

d_time <- d3 %>% filter(cycle <= 1000)

fig1A <- ggplot(d_time) + theme_cowplot(my_font_size) +
  geom_line(aes(x=cycle, y=y, lty=cell, color=trait),
            size = 1.5) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,1000),
                     breaks = seq(0,1000,250)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = seq(0,1,0.5)) +
  scale_color_manual(values = c("darkgreen","brown","orange")) + 
  scale_linetype_manual(values = c(1,3)) +
  labs(x = "\nTime", y = "Allocation") +
  background_grid(major = "xy", minor = "y") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_blank())

fig1A

## fig 1B: age distribution

age_fill <- "#a887ab"

fig1B <- ggplot(d,aes(age)) + theme_cowplot(my_font_size) +
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 fill = age_fill,
                 color = "black") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,15),
                     breaks = seq(1,13,2)) +
  labs(x = "Age", y = "Distribution\n") +
  background_grid(major = "xy", minor = "y") +
  theme(axis.text.y = element_blank())
fig1B

## fig1C: damage distribution

damage_fill <- "#d96125"

fig1C <- ggplot(d,aes(dam0)) + theme_cowplot(my_font_size) +
  geom_histogram(aes(y=..density..),
                 binwidth = 0.2,
                 fill = damage_fill,
                 color = "black") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,6),
                     breaks = 0:6) +
  labs(x = "Damage", y = "Distribution\n") +
  background_grid(major = "xy", minor = "y") +
  theme(axis.text.y = element_blank())

fig1C

## fig1D: freq deleterious alleles vs. damage

V <- d[,14:29]
V_means <- apply(V,2,mean)
damage_vals <- seq(0.5*6/15,6+0.5*6/15,length.out = 16)
d_1D <- data.frame(damage_int=1:16,freq=V_means)

my_del_col <- "#d92567"

fig1D <- ggplot(d_1D,aes(damage_int,freq)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = my_del_col) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0.5,16.5),
                     breaks = seq(1,15,2)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = seq(0,1,0.5)) +
  labs(x = "Damage interval", y = "Mutant frequency") +
  background_grid(major = "xy", minor = "y")
fig1D


## fig1E

d_dam <- d %>% group_by(age) %>% summarise(y = median(dam0),
                                           ymin = quantile(dam0,0.2),
                                           ymax = quantile(dam0,0.8))
d_dam <- d_dam %>% filter(age <= 15)

fig1E <- ggplot(d_dam,aes(age,y)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = damage_fill) +
  geom_errorbar(aes(ymin=ymin,ymax=ymax),
                width=0,
                color = damage_fill) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0.5,15.5),
                     breaks = seq(1,15,2)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,4),
                     breaks = seq(0,4,1)) +
  labs(x = "Age", y = "Damage") +
  background_grid(major = "xy", minor = "y")

fig1E

## fig1F: mortality vs. age

d_mort <- d %>% group_by(age) %>% summarise(y = mean(dead))

d_mort <- d_mort %>% filter(age <= 15)    

d_gam <- d %>% filter(age <= 15)
  
m1 <- gam(dead ~ s(age), family = binomial, data = d_gam) 
gam.check(m1)
d.pred <- data.frame(age=seq(1,15,0.1))
fitted <- predict(m1,newdata = d.pred, type = "response", se.fit = T)
d.pred <- d.pred %>% mutate(y = fitted$fit,
                            ymin = y-1.96*fitted$se.fit,
                            ymax = y+1.96*fitted$se.fit)


fig1F <- ggplot(d_mort,aes(age,y)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = my_del_col) +
  geom_line(data = d.pred, color = my_del_col, size = 1.3) +
  geom_ribbon(data = d.pred, aes(ymin=ymin,ymax=ymax),
              alpha = 0.3) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0.5,15.5),
                     breaks = seq(1,15,2)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.06),
                     breaks = seq(0,0.06,0.02)) +
  labs(x = "Age", y = "Mortality") +
  background_grid(major = "xy", minor = "y")

fig1F

## Grid:

plot_grid(fig1A, fig1B, fig1C, fig1D, fig1E, fig1F,
          labels = c('A','B','C','D','E','F'), 
          label_size = 18,
          ncol = 3)


##########################################################

## fig1A: ternary

library(ggtern)

d2 <- read.table("evol_sym.txt",header = F)

names(d2) <- c("generation","popsize","age","rep0","rep1","rpr0",
               "rpr1","har0","har1","tra0","tra1","dam0","dam1","res0","res1",
               paste0("v0_",0:15),paste0("v1_",0:15))

d3 <- with(d2, data.frame(generation=rep(generation,2),
                          reproduction=c(rep0,rep1),
                          repair=c(rpr0,rpr1),
                          foraging=c(1-rep0-rpr0,1-rep1-rpr1),
                          cell=as.factor(rep(c(1,2),each=nrow(d2)))))

## smooth curve

d4 <- d3 %>% filter(generation < 2000)

dT <- data.frame(foraging=c(0.333,0.0),repair=c(0.333,0.333),reproduction=c(0.333,0.666),
                 cell=as.factor(c(1,1)))
dR <- data.frame(foraging=c(0.333,0.666),repair=c(0.333,0.0),reproduction=c(0.333,0.333),
                 cell=as.factor(c(1,1)))
dL <- data.frame(foraging=c(0.333,0.333),repair=c(0.333,0.666),reproduction=c(0.333,0.0),
                 cell=as.factor(c(1,1)))

fig1A <- ggtern(d4,aes(x=foraging,y=repair,z=reproduction,color=cell)) +
  geom_line(data=dT,color="black",lty=2) + 
  geom_line(data=dR,color="black",lty=2) + 
  geom_line(data=dL,color="black",lty=2) + 
  geom_point(size=0.7,alpha=0.4) +
  #geom_line(alpha=0.4,lwd=0.7) +
  labs(x="",xarrow="Foraging %",
       y="",yarrow="Repair %",
       z="",zarrow="Reproduction %") +
  theme_bw(base_size = 20) + 
  theme_showsecondary() +
  theme_showarrows() +
  theme(legend.position=c(0.0,0.7), legend.justification=c(-0.1,1))
fig1A





######################################

