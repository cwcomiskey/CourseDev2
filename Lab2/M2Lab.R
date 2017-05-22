# Bonferroni Correction ===================================

experiment <- function(){
  
  data1 <- rnorm(40, 0, 1)
  data2 <- rnorm(40, 0, 1)
  data3 <- rnorm(40, 0, 1)
  
  p1 <- t.test(data1, data2)$p.val
  p2 <- t.test(data1, data3)$p.val
  p3 <- t.test(data2, data3)$p.val
  
  max(c(p1 < 0.05, p2 < 0.05, p3 < 0.05))
}
experiment()

mean(replicate(1000, experiment()))


# TukeyHSD ====================================

Heights <- read.csv("Sport_Heights.csv")

?TukeyHSD
alt <- aov(Height ~ Sport, data = Heights) # Analysis of Variance
summary(alt); names(alt) # May look familiar!
TukeyHSD(alt) # Notice wider intervals, way bigger p-vals
names(TukeyHSD(alt))

soccer <- subset(Heights, Sport == "baseball", "Height")$Height
baseball <- subset(Heights, Sport == "soccer", "Height")$Height
t <- t.test(baseball, soccer, var.equal = TRUE)
names(t)
t$conf.int; t$p.value

# Pg. 161 in Sleuth for "Multiplier" talk, regarding CI coverage
