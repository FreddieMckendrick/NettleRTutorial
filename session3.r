# R script to accompany session 3 - see PDF handout for details

# Chunk 1
fig1 = ggplot(d, aes(x=Brood.type, y=Telo.change)) + 
  geom_boxplot() + 
  geom_point(colour="red") + 
  theme_bw() + 
  xlab("Type of brood") + 
  ylab("Early life telomere change") +
  ggtitle("Telomere change by brood type")

fig1

# Chunk 2
fig1 = ggplot(d, aes(x=Brood.type, y=Telo.change)) + 
  geom_boxplot() + 
  geom_point(aes(colour=Sex)) +
  theme_bw() + 
  xlab("Type of brood") + 
  ylab("Early life telomere change") +
  ggtitle("Telomere change by brood type")

fig1

# Chunk 3
fig1 = ggplot(d, aes(x=Brood.type, y=Telo.change, colour=Sex)) + 
  geom_boxplot() + 
  geom_point() + 
  theme_bw() + 
  xlab("Type of brood") + 
  ylab("Early life telomere change") +
  ggtitle("Telomere change by brood type")

fig1

# Chunk 4
fig1 = ggplot(d, aes(x=Brood.type, y=Telo.change, colour=Sex)) + 
  geom_boxplot() + 
  geom_point(position=position_dodge(width=0.75)) + 
  theme_bw() + 
  xlab("Type of brood") + 
  ylab("Early life telomere change") +
  ggtitle("Telomere change by brood type")

fig1


# Chunk 5
pdf("figure1.pdf", width = 5, height =5)
fig1
dev.off()

# Chunk 6
png("figure1.png", res=300, width = 12, height =12, units="cm")
fig1
dev.off()

# Chunk 7
fig2=ggplot(d, aes(x=Weight, y=Telo.change)) + 
      theme_bw() + 
      geom_point() + 
      geom_smooth(method="lm")

fig2

# Chunk 8
d$Weight.centred=d$Weight-mean(d$Weight)
m=lm(Telo.change ~ Brood.type + Weight.centred, data=d)
summary(m)

# Chunk 9
mi=lm(Telo.change ~ Brood.type + Weight.centred + Brood.type:Weight.centred, data=d)
summary(mi)
confint(mi)

