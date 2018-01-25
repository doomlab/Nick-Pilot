##setup
library(reshape)
library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

##data screening
dat = read.csv("Melted Data.csv")
colnames(dat)[2] = "Judgment.task"
colnames(dat)[4] = "Judgment"

dat$Judgment[ dat$Judgment > 100 ] = NA

mahal = mahalanobis(dat[ , c(4,5)],
                    colMeans(dat[ , c(4,5)], na.rm = TRUE),
                    cov(dat[ , c(4,5)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(dat[ , c(4,5)]))
cutoff;ncol(dat[ , c(4,5)])
summary(mahal < cutoff)
dat.noout = subset(dat, mahal < cutoff)

dat.noout$Judgment = dat.noout$Judgment/100

##capitalize things
dat.noout$Judgment.task = as.numeric(dat.noout$Judgment.task)

dat.noout$Judgment.task = factor(dat.noout$Judgment.task,
                           levels = c(1, 2, 3),
                           labels = c("Associative", "Semantic", "Thematic"))

##melt the data
long.dat = melt(dat.noout, id = c("Partno", "Judgment.task", "Word.Pair", "COS", "LSA", "FSG"))

##rename columns
colnames(long.dat)[2] = "Judgment.Type" 
colnames(long.dat)[7] = "Task"
colnames(long.dat)[8] = "Score"

##make the graph
bar.plot = ggplot(long.dat, aes(Task, Score, fill = Judgment.Type)) +
  cleanup +
  theme(legend.position="bottom") +
  stat_summary(fun.y = mean,
              geom = "bar",
              position = "dodge",
              color = "black") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = .2,
               color = "darkgrey") +
  scale_fill_manual("Judgment Type", 
                   values = c("Associative" = "black", 
                              "Semantic" = "dimgrey",
                              "Thematic" = "indianred4"))
bar.plot

##updated graph
bar.plot2 = ggplot(long.dat, aes(Judgment.Type, Score, fill = Task)) +
  cleanup +
  theme(legend.position="bottom") +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = "dodge",
               color = "black") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = .2,
               color = "black") +
  scale_fill_manual("Task", 
                    values = c("Judgment" = "indianred4", 
                               "Recall" = "dimgray")) +
  ylim(0, 1) + 
  xlab("Judgment Type")
bar.plot2


tiff(filename = "scoregraph.tiff", res = 300, width = 6, 
     height = 6, units = 'in', compression = "lzw")
plot(bar.plot2)
dev.off()
