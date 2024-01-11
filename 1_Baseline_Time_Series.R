#!/usr/bin/Rscript

##############################################################
#                                                            #
# LIBRARIES                                                  #
#                                                            #
##############################################################
library(ggplot2)

##############################################################
#                                                            #
# DATA                                                       #
#                                                            #
##############################################################

ingresos <- read.table("ingresos.csv", header=TRUE, sep=",")
ingresos$Var1 <- as.Date(ingresos$Var1, "%Y-%m-%d")

##############################################################
#                                                            #
# BASELINE PLOTS                                              #
#                                                            #
##############################################################

# Basic line plot con GGPLOT
# Set axis limits c(min, max)
min <- as.Date("2020-02-27")
max <- as.Date("2021-12-31")

grafico_nuevos_ingresos <- ggplot(data = ingresos, aes(x = as.Date(Var1), y = Freq, group=1))+
  geom_bar(stat="identity", color = "#00AFBB") +
  ylab('Admissions')+xlab('Date') +
  scale_x_date(limits = c(min, max), date_labels = "%B-%Y", date_breaks = '2 months') +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black"),
        axis.text=element_text(size=8),  
        axis.title.x = element_text(vjust=-0.2),
        axis.title.y = element_text(vjust=+0.6),
        axis.title=element_text(size=10,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Daily hospitalizations in Spain (2020-2021)") +
  theme(plot.title = element_text(hjust = 0.5))
grafico_nuevos_ingresos

############################
# SCATTER PLOT
############################
grafico_nuevos_ingresos_2 <- ggplot(data = ingresos, aes(x = as.Date(Var1), y = Freq, group=1))+
  geom_point(color = "#00AFBB", size = 1) +
  ylab('Admissions')+xlab('Date') +
  scale_x_date(limits = c(min, max), date_labels = "%B-%Y", date_breaks = '2 months') +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black"),
        axis.text=element_text(size=8), 
        axis.title.x = element_text(vjust=-0.2),
        axis.title.y = element_text(vjust=+0.6),
        axis.title=element_text(size=10,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Daily hospitalizations in Spain (2020-2021)",
       subtitle="Waves 1 to 6") + 
  theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
        plot.caption = element_text(size = 7.5, color = "grey40"))
grafico_nuevos_ingresos_2

############################
# NICE, COLOURED PLOT
############################

# Set axis limits c(min, max)
min <- as.Date("2020-02-12")
max <- as.Date("2021-12-31")

grafico_split <- ggplot(data = ingresos) +
  geom_point(aes(x = Var1, y = Freq, group=1), color = "#00AFBB", size = 1) +
  geom_line(aes(x = Var1, y = moving_average), color = "red", size = .75)+
  ylab('Admissions')+
  xlab('') +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype="dashed", color = "grey", size=.5) + #New year
  geom_vline(xintercept = as.Date("2022-01-01"), linetype="dashed", color = "grey", size=.5) + #Anther new year

  annotate("rect", xmin=as.Date("2020-02-12"),xmax=as.Date("2020-06-14"), ymin = 0, ymax = Inf, alpha = .25, fill="#999999") +
  annotate("rect", xmin=as.Date("2020-06-15"),xmax=as.Date("2020-11-27"), ymin = 0, ymax = Inf, alpha = .25, fill="#E69F00") +
  annotate("rect", xmin=as.Date("2020-11-28"),xmax=as.Date("2021-03-26"), ymin = 0, ymax = Inf, alpha = .25, fill="#56B4E9") +
  annotate("rect", xmin=as.Date("2021-03-27"),xmax=as.Date("2021-06-22"), ymin = 0, ymax = Inf, alpha = .25, fill="#009E73") +
  annotate("rect", xmin=as.Date("2021-06-23"),xmax=as.Date("2021-10-27"), ymin = 0, ymax = Inf, alpha = .25, fill="#F0E442") +
  annotate("rect", xmin=as.Date("2021-10-28"),xmax=as.Date("2021-12-31"), ymin = 0, ymax = Inf, alpha = .25, fill="#0072B2") +
  annotate("text", x = as.Date("2020-04-20"), y = 4500, label = "1 ^ st", parse = TRUE, size=5, color="darkgrey") +
  annotate("text", x = as.Date("2020-09-02"), y = 4500, label = "2 ^ nd", parse = TRUE, size=5, color="darkgrey") +
  annotate("text", x = as.Date("2021-02-02"), y = 4500, label = "3 ^ rd", parse = TRUE, size=5, color="darkgrey") +
  annotate("text", x = as.Date("2021-05-10"), y = 4500, label = "4 ^ th", parse = TRUE, size=5, color="darkgrey") +
  annotate("text", x = as.Date("2021-08-20"), y = 4500, label = "5 ^ th", parse = TRUE, size=5, color="darkgrey") +
  annotate("text", x = as.Date("2021-12-30"), y = 4500, label = "6 ^ th", parse = TRUE, size=5, color="darkgrey") +
  scale_x_date(limits = c(min, max), date_labels = "%B-%Y", date_breaks = '2 months') +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black"),
        axis.text=element_text(size=8),  #tamaño de las fechas
        axis.title.x = element_text(vjust=-0.2),
        axis.title.y = element_text(vjust=+0.6),
        axis.title=element_text(size=10,face="bold"), #tamaño de los títulos de los ejes
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
        plot.caption = element_text(size = 7.5, color = "grey40")) +
  labs(title="Daily hospitalizations due to COVID-19 by wave",
  subtitle="Spain - Years 2020-2021")+  
  theme(plot.title = element_text(hjust = 0.5)) 
grafico_split
ggsave("trend_waves.png", height = 6, width = 8)