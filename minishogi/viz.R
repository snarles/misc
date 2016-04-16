####
##  Minishogi visualization
####

#plot(1:2);text(1.5, 1.5, "\\#J3023", vfont = c("serif", "plain"), cex = 10, srt = 0)

library(showtext)
font.add("heiti", "华文黑体.ttf")
font.add("constan", "Chalkduster.ttf")
dev.new(noRStudioGD = TRUE)
showtext.auto()
plot(1:2)
text(1.5, 1.5, "\u4F60\u597D\uFF0C\u4E16\u754C", family = "heiti")


dev.off()

text(1.5, 1.5, "\u4F60\u597D\uFF0C\u4E16\u754C", family = "heiti")


library(ggplot2)
p = ggplot(NULL, aes(x = 1, y = 1)) + ylim(0.8, 1.2) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank()) +
  annotate("text", 1, 1.1, family = "heiti", size = 15,
           label = "\u4F60\u597D\uFF0C\u4E16\u754C") +
  annotate("text", 1, 0.9, label = 'Chinese for "Hello, world!"',
           family = "constan", size = 12)

showtext.auto()  ## automatically use showtext for new devices

print(p)  ## on-screen device

showtext.auto(FALSE)  ## turn off if no longer needed