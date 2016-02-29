library(ggplot2)
library(gridExtra)
library(dplyr)
library(datasets)

## Load "ToothGrowth" dataset into dataframe and summarize
df <- ToothGrowth
summary(df)
## Change variable names within dataframe for better understanding and summarize
names(df) <- c("Tooth.Length", "Delivery.Method", "Dosage")
summary(df)

## Simple boxplot to see if tooth length trends are similar across delivery methods within dosage
box1 <- ggplot(data=df) + geom_boxplot(aes(x=Delivery.Method, y=Tooth.Length, fill=Delivery.Method)) +
    scale_fill_manual(name="Delivery\nMethod",
                        breaks=c("OJ", "VC"),
                        labels=c("OJ", "VC2"),
                        values=c("orangered3", "green4")) +
    labs(title="Fig.1- Boxplot of Tooth Length by\nDelivery Method Within Dosage", x="Delivery Method", y="Tooth Length") +
    theme(axis.text.x=element_text(angle=30, vjust=0.5, size=8, face="bold"),
          axis.text.y=element_text(size=10, face="bold"),
          axis.title=element_text(size=10, face="bold"),
          plot.title=element_text(size=12, face="bold"),
          legend.text=element_text(size=6)) +
    facet_grid(. ~ Dosage)

## Simple boxplot to see if tooth length trends are similar across dosage within delivery methods
box2 <-ggplot(data=df) + geom_boxplot(aes(x=as.factor(Dosage), y=Tooth.Length, fill=Delivery.Method))  +
    scale_fill_manual(name="Delivery\nMethod",
                      breaks=c("OJ", "VC"),
                      labels=c("OJ", "VC2"),
                      values=c("orangered3", "green4")) +labs(title="Fig.1- Boxplot of Tooth Length by\nDosage Within Delivery Method", x="Dosage", y="Tooth Length") +
    theme(axis.text.x=element_text(angle=30, vjust=0.5, size=8, face="bold"),
          axis.text.y=element_text(size=10, face="bold"),
          axis.title=element_text(size=10, face="bold"),
          plot.title=element_text(size=12, face="bold"),
          legend.text=element_text(size=6)) +
    facet_grid(. ~ Delivery.Method)

grid.arrange(box1, box2, ncol=2)

## Evalaute differences in delivery method for teeth length across dosage
delivery.t <- t.test(Tooth.Length ~ Delivery.Method, paired=FALSE, alternative="two.sided", data=df)


## Set up variables for dosage and delivery method.
dosages <- c(0.5, 1, 2)
deliverys <- c("OJ", "VC")

## Loop to evaluate effect of delivery method within same dosage
for(i in 1:length(dosages)){
    df.temp <- df[df$Dosage == dosages[i], ]
    t.test.temp <- t.test(Tooth.Length ~ Delivery.Method, data=df.temp)
    print(paste0("Comparison of delivery mechanism t-test of ", dosages[i], "mg dosage"))
    ifelse(t.test.temp$p.value <= 0.05, 
           print(paste0("Since p-value of t-test is less than 0.05 we reject the Null ", 
                        "hypothesis that delivery mechanisms are the same for ", 
                        dosages[i], "mg dosage")),
           print(paste0("Since p-value of t-test is not less than 0.05 we fail to reject ", 
                        "the Null hypothesis that delivery mechansisms are the same for ", 
                        dosages[i], "mg dosage")))
    print(t.test.temp)
}
