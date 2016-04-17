####
##  Minishogi visualization
####

#plot(1:2);text(1.5, 1.5, "\\#J3023", vfont = c("serif", "plain"), cex = 10, srt = 0)

library(showtext)
font.files()
font.add("heiti", "华文黑体.ttf")
font.add("fangti", "华文仿宋.ttf")
font.add("pro1","儷宋 Pro.ttf")
font.add("xihei", "华文细黑.ttf")


chars <- c("K"="\u7389",
           "R"="\u98DB",
           "Rp"="\u7ADC",
           "B"="\u89D2",
           "Bp"="\u99AC",
           "G"="\u91D1",
           "S"="\u9280",
           "Sp"="\u91D1",
           "P"="\u6B69",
           "Pp"="\u3068")

dev.new(noRStudioGD = TRUE)
showtext.auto()
plot(1:2)
text(1.5, 1.5, paste(chars, collapse = ""), family = "wqy-microhei", cex = 3, col = "red")
text(1.5, 1.3, paste(chars, collapse = ""), family = "pro1", cex = 3, srt = 180)


dev.off()
