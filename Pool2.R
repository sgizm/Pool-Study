raporlevels(data[data$jobfunction.other=="",]$jobfunction.other<-NA)
y <- droplevels(data[!is.na(data$jobfunction.other),]$jobfunction.other)
plot(y, col=brewer.pal(9, "GnBu"))


# ggplot(data, aes(x=useractivities.other.open)) +
#  +     geom_bar(fill="#FF9999", colour="#FF9999") +
#  +     labs(x="useractivities.other.open", y="Frequency")

levels(data[data$useractivities.other.open=="",]$useractivities.other.open<-NA)
zz <- droplevels(data[!is.na(data$useractivities.other.open),]$useractivities.other.open)

# x axis labelleri sigmiyor. 45 derece yapamadik
plot(zz, ylab="Freqency",xlab="Other", col=brewer.pal(9, "GnBu")) 
text(tck, par("usr")[3], labels=labels, srt=315,
     xpd=TRUE, adj=c(-0.2,1.2), cex=0.9)

#bu 45 derece oldu ama bunda da NA yi nasil cikaririm bilemedim
ggplot(data, aes(x=useractivities.other.open)) + 
  +        geom_bar(fill="#FF9999", colour="#FF9999") +
  +        labs(x="useractivities.other.open", y="Frequency") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# 2.2  How much do you agree with the following statements?
userinv <- data.frame(Statement=userinv.statements,
                      Rating=c(
                        userinv.S1,
                        userinv.S2,
                        userinv.S3,
                        userinv.S4,
                        userinv.S5,
                        userinv.S6))
ggplot(data=userinv, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

# 2.3 icin:
# 2.3 In your experience, how easy is it for the following to get information from users?
userinf <- data.frame(Statement=factor(rep(userinf.statements, each=length(userinf.mgr))),
                      Rating=c(
                        userinf.mgr,
                        userinf.uxd,
                        userinf.dev,
                        userinf.tst,
                        userinf.arc,
                        userinf.ops,
                        userinf.slf))
ggplot(data=userinf, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

# 2.4 How often do you use the following ways to get information about users?
infofreq <- data.frame(Statement=factor(rep(infofreq.statements, each=length(infofreq.O1))),
                       Rating=c(
                         infofreq.O1,
                         infofreq.O2,
                         infofreq.O3,
                         infofreq.O4))
ggplot(data=infofreq, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

#3.1 Does your company conduct experiments involving the users? How often?
#bu asagidaki olmadi
#condexp <- data.frame(data$condexp)
#plot(data$condexp, ylab="Freqency",xlab="Frequency of experiments", col=brewer.pal(9, "GnBu")) 

ggplot(data, aes(x=condexp)) +
  geom_bar(fill="#FF9999", colour="#FF9999") +
  labs(x="Conducting experiments", y="Frequency")

#3.3 Below are three pairs of 
understanding <- data.frame(Statement=factor(rep(understanding.statements, each=length(understanding.S1))),
                      Rating=c(
                        understanding.S1,
                        understanding.S2,
                        understanding.S3,
                        understanding.S4,
                        understanding.S5,
                        understanding.S6))
ggplot(data=understanding, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

#4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif <- data.frame(Statement=factor(rep(usernotif.statements, each=length(usernotif.S1))),
                            Rating=c(
                              usernotif.S1,
                              usernotif.S2,
                              usernotif.S3,
                              usernotif.S4,
                              usernotif.S5,
                              usernotif.S6,
                              usernotif.S7))
ggplot(data=undernotif, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

#4.2 How much do you agree with the following statements about involving users in experiments?
expinv <- data.frame(Statement=factor(rep(expinv.statements, each=length(expinv.S1))),
                         Rating=c(
                           expinv.S1,
                           expinv.S2,
                           expinv.S3,
                           expinv.S4,
                           expinv.S5,
                           expinv.S6,
                           expinv.S7))
ggplot(data=expinv, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip()