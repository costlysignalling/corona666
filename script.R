#I have used an English word database from https://github.com/dwyl/english-words
#I have restricted the analysis to words that contain only letters (no numbers or symbols)
#Load the data as a vector
d<-read.table("words_alpha.txt")
v<-as.character(d[,1])

#Total word count
(n<-length(v))

#I calculate the character lengths
vlength<-nchar(v)
(lengths<-summary(as.factor(vlength)))

#Most frequent are words of length 9
lengths[which.max(lengths)]
lengths[which.max(lengths)]/n #14%

barplot(lengths,col="dodgerblue")

#I want only words of length 6
v6<-v[vlength==6]

length(v6)/n #still pretty frequent 8% of al words

#The longest word is
v[vlength==31]

#I will create the function that returns the sum of numeric letter values
getNum<-function(string){
  lc<-tolower(string)
  lv<-strsplit(lc,"")[[1]]
  numval<-sum(match(lv,letters))
  return(numval)
}

getNum("corona") #It works
match("corona",v6) #wooow :D

#lowest and highest relized sum
v6[sumv==10]
v6[sumv==128]

#I will calculate this for all 6-letter words
sumv<-sapply(v6,getNum)
s6<-summary(as.factor(sumv),maxsum = 1000)

#Fourth most frequent option
sort(s6,decreasing=T)

s6[names(s6)=="66"]/length(v6) #2.3 percent of all 6 letter words
s6[names(s6)=="66"]/n          #almor 2 promile of all words (0.0018 total probability)

beasts1<-v6[sumv==66]
write.table(beasts1,"beasts.txt",row.names = F,col.names=F,sep="\t")

favourites<-c(
  "anubis",
  "bootie",
  "empire",
  "monads",
  "nomads",
  "pigeon",
  "pyjama",
  "trader",
  "trifid",
  "weewee")

#See here why 616 might be the original "number of the beast" https://en.wikipedia.org/wiki/616_(number)
beasts2<-v6[sumv==16]

mybar<-function(s,w=0.5,col="dodgerblue",border=NA){
  vals<-as.numeric(names(s))
  for(i in 1:length(s)){
    rect(vals[i]-w,0,vals[i]+w,s[i],col=col,border=border)
  }
}

png("Figure1.png",width=20,height=16,units="cm",res=600)
plot(NULL,xlim=range(as.numeric(names(s6))),ylim=c(0,max(s6)),main="Six-letter words",xlab="Letter sum",ylab="Frequency",bty="n")
mybar(s6)

abline(v=66,lwd="2",col="red")
text(66,760,"666",col="red",xpd=T,pos=3)
text(10,seq(650,300,l=6),strsplit("CORONA","")[[1]])
text(15,seq(650,300,l=6),"=")
text(20,seq(650,300,l=6),match(strsplit("corona","")[[1]],letters))
segments(c(8,18),250,c(12,22),250)
text(10,210,"6",col=2)
text(20,210,"66",col=2)
text(110,seq(700,100,l=10),favourites,pos=4,col=2)

dev.off()












