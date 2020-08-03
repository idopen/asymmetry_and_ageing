## figure 2:
## panel of plots
## A: dynamics r=1
## B: dynamics r=0
## C: damage r=0,1

library(tidyverse)
library(cowplot)
library(mgcv)



## fig2A: dynamics for m0=1

## read data (dynamics)
dA <- read.table("evol_m0_1.txt",header = F)
names(dA) <- c("generation","popsize","age","rep0","rep1","rpr0",
               "rpr1","har0","har1","tra0","tra1","dam0","dam1","res0","res1",
               paste0("v0_",0:15),paste0("v1_",0:15))

dA <- mutate(dA, for0 = 1-rep0-rpr0,
             for1 = 1-rep1-rpr1)

dA2 <- with(dA, data.frame(y = c(for0,for1,rpr0,rpr1,rep0,rep1)))
dA2$cycle <- rep(dA$generation,6)
dA2$trait <- factor(rep(c("forage","repair","repro"), each = 2*nrow(dA)))
dA2$cell <- factor(rep(c(2,1),each=nrow(dA),times=3))

my_font_size <- 16

fig2A <- ggplot(dA2) + theme_cowplot(my_font_size) +
  geom_line(aes(x=cycle, y=y, lty=cell, color=trait),
            size = 1.5) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,10000),
                     breaks = seq(0,10000,2500)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = seq(0,1,0.5)) +
  scale_color_manual(values = c("darkgreen","brown","orange")) + 
  scale_linetype_manual(values = c(1,2)) +
  labs(x = "\nTime", y = "Allocation",
       lty = "Cell type:", color = "Trait:") +
  background_grid(major = "xy", minor = "y") +
  #theme(legend.position = "top") +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  ggtitle("     r = 1")

fig2A

## fig2B: m0 = 0

## read data (dynamics)
dB <- read.table("evol_m0_0.txt",header = F)
names(dB) <- c("generation","popsize","age","rep0","rep1","rpr0",
               "rpr1","har0","har1","tra0","tra1","dam0","dam1","res0","res1",
               paste0("v0_",0:15),paste0("v1_",0:15))

dB <- mutate(dB, for0 = 1-rep0-rpr0,
             for1 = 1-rep1-rpr1)

dB2 <- with(dB, data.frame(y = c(for0,for1,rpr0,rpr1,rep0,rep1)))
dB2$cycle <- rep(dB$generation,6)
dB2$trait <- factor(rep(c("forage","repair","repro"), each = 2*nrow(dB)))
dB2$cell <- factor(rep(c(1,2),each=nrow(dB),times=3))

fig2B <- ggplot(dB2) + theme_cowplot(my_font_size) +
  geom_line(aes(x=cycle, y=y, lty=cell, color=trait),
            size = 1.5) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,10000),
                     breaks = seq(0,10000,2500)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1),
                     breaks = seq(0,1,0.5)) +
  scale_color_manual(values = c("darkgreen","brown","orange")) + 
  scale_linetype_manual(values = c(1,2)) +
  labs(x = "\nTime", y = "Allocation",
       lty = "Cell type:", color = "Trait:") +
  background_grid(major = "xy", minor = "y") +
  # theme(legend.position = "top") +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  ggtitle("     r = 0")

fig2B

## fig2C: resources + damage

## damage multiplier:
dm <- 10

damage_color <- "#d96125"

dC <- with(dA, data.frame(y = c(res0,res1,dm*dam0,dm*dam1)))
dC$cycle <- rep(dA$generation,4)
dC$trait <- factor(rep(c("resources","damage"), each = 2*nrow(dA)))
dC$cell <- factor(rep(c(2,1),each=nrow(dA),times=2))

fig2C <- ggplot(dC) + theme_cowplot(my_font_size) +
  geom_line(aes(x=cycle, y=y, lty=cell, color=trait),
            size = 1.5) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,10000),
                     breaks = seq(0,10000,2500)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,8),
                     breaks = seq(0,8,2)) +
  scale_color_manual(values = c(damage_color,"blue")) + 
  scale_linetype_manual(values = c(1,2), ) +
  labs(x = "Time", y = "Amount",
       lty = "Cell type:", color = "Trait:") +
  background_grid(major = "xy", minor = "y") +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(t=0, r=15, b=0, l=0))) +
  guides(linetype = FALSE)

fig2C

## fig2C: resources + damage

## damage multiplier:

dD <- with(dB, data.frame(y = c(res0,res1,dm*dam0,dm*dam1)))
dD$cycle <- rep(dB$generation,4)
dD$trait <- factor(rep(c("resources","damage"), each = 2*nrow(dB)))
dD$cell <- factor(rep(c(1,2),each=nrow(dB),times=2))

fig2D <- ggplot(dD) + theme_cowplot(my_font_size) +
  geom_line(aes(x=cycle, y=y, lty=cell, color=trait),
            size = 1.5) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,10000),
                     breaks = seq(0,10000,2500)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,8),
                     breaks = seq(0,8,2)) +
  scale_color_manual(values = c(damage_color,"blue")) + 
  scale_linetype_manual(values = c(1,2), ) +
  labs(x = "Time",
       lty = "Cell type:", color = "Trait:") +
  background_grid(major = "xy", minor = "y") +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_blank()) +
  guides(linetype = FALSE)

fig2D



## Grid:

prow1 <- plot_grid(
  fig2A + theme(legend.position="none"),
  fig2B + theme(legend.position="none"),
  align = 'vh',
  labels = c("A", "B"),
  label_size = 18,
  hjust = -1,
  nrow = 1
)

legend1 <- get_legend(
  # create some space to the left of the legend
  fig2A + theme(legend.box.margin = margin(0, 0, 0, 12)) +
    theme(legend.key.width=unit(1.5,"cm"))
)

prow2 <- plot_grid(
  fig2C + theme(legend.position="none"),
  fig2D + theme(legend.position="none"),
  align = 'vh',
  labels = c("C", "D"),
  label_size = 18,
  hjust = -1,
  nrow = 1
)

legend2 <- get_legend(
  # create some space to the left of the legend
  fig2C + theme(legend.box.margin = margin(0, 0, 0, 12)) +
    theme(legend.key.width=unit(1.5,"cm"))
)


plot_grid(prow1, legend1, 
          prow2, legend2, 
          nrow = 2,
          axis = "l",
          align = "v",
          rel_widths = c(3, .6, 3, .6))

##################





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

