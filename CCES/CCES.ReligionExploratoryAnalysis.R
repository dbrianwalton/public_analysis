cces = read.csv('Downloads/CCES20_Common_OUTPUT.csv', header=TRUE)

groupList = list( list(groupName="Catholic", fileName="catholic_voting",
                       constraints=list(religpew=2, religpew_catholic=1)),
                  list(groupName="Southern Baptist", fileName="southernbaptist_voting",
                       constraints=list(religpew=1, religpew_protestant=1, religpew_baptist=1)),
                  list(groupName="Other Baptists", fileName="otherbaptist_voting",
                       constraints=list(religpew=1, religpew_protestant=1, religpew_baptist=c(2,3,4,5,6,7,8,9,10,90))),
                  list(groupName="Methodist", fileName="methodist_voting",
                       constraints=list(religpew=1, religpew_protestant=2)),
                  list(groupName="Lutheran", fileName="lutheran_voting",
                       constraints=list(religpew=1, religpew_protestant=4)),
                  list(groupName="Presbyterian", fileName="presbyterian_voting",
                       constraints=list(religpew=1, religpew_protestant=5)),
                  list(groupName="Pentacostal", fileName="pentacostal_voting",
                       constraints=list(religpew=1, religpew_protestant=6)),
                  list(groupName="Episcopalian", fileName="episcopalian_voting",
                       constraints=list(religpew=1, religpew_protestant=7)),
                  list(groupName="Latter Day Saint", fileName="mormon_voting",
                       constraints=list(religpew=3, religpew_mormon=1)),
                  list(groupName="Eastern Orthodox", fileName="e_orthodox_voting",
                       constraints=list(religpew=4)),
                  list(groupName="Jewish", fileName="jewish_voting",
                       constraints=list(religpew=5)),
                  list(groupName="Muslim", fileName="muslim_voting",
                       constraints=list(religpew=6)),
                  list(groupName="Buddhist", fileName="buddhist_voting",
                       constraints=list(religpew=7)),
                  list(groupName="Hindu", fileName="hindu_voting",
                       constraints=list(religpew=8)),
                  list(groupName="Athiest", fileName="atheist_voting",
                       constraints=list(religpew=9)),
                  list(groupName="Agnostic", fileName="agnostic_voting",
                       constraints=list(religpew=10)),
                  list(groupName="Other", fileName="other_voting",
                       constraints=list(religpew=c(11,12)))
)

ageCuts = c(17, 30, 45, 60, 80, 105)
numCuts = length(ageCuts)-1

pdf(file="religious_voting_patterns.pdf", 
    width=10, height=8, onefile=TRUE)
par(mfcol=c(2,2))
for (i in 1:length(groupList)) {
  myGroup = groupList[[i]]$groupName
  index = TRUE
  constraints = names(groupList[[i]]$constraints)
  for (j in 1:length(constraints)) {
    constraint = constraints[j]
    constraintValue = groupList[[i]]$constraints[[j]]
    if (length(constraintValue) == 1) {
      index = index & (cces[[constraint]] == constraintValue)
    } else {
      cat(myGroup,'\n')
      subindex = FALSE
      for (k in length(constraintValue)) {
        subindex = subindex | (cces[[constraint]] == constraintValue[k])
      }
      index = index & subindex
    }
  }
  
  groupData = cces[index,]
  age = 17:100
  voters=rep(0,length(age))
  trumpVoters=voters
  bidenVoters=voters
  for (i in 1:length(age)) {
    year = 2020-age[i]
    voters[i] = sum(groupData$birthyr >= year & groupData$CC20_410<5, na.rm=TRUE); 
    trumpVoters[i] = sum(groupData$birthyr >= year & groupData$CC20_410==2, na.rm=TRUE); 
    bidenVoters[i] = sum(groupData$birthyr >= year & groupData$CC20_410==1, na.rm=TRUE); 
  }

  plot(age,trumpVoters, type='l', lwd=2, col='red', ylim=c(0,max(trumpVoters, bidenVoters)),
       main="Cumulative Votes by Age",ylab=paste(myGroup, 'Voters'))
  lines(age,bidenVoters, lwd=2, col='blue')
  legend('topleft',c('Trump','Biden'), lwd=2, col=c('red','blue'))

  plot(age, bidenVoters / trumpVoters, pch=23, cex=0.25,
       main="Ratio of Voters by Age", ylab="(# Biden)/(# Trump)")

  bidenVoters = groupData$CC20_410 == 1
  trumpVoters = groupData$CC20_410 == 2
  otherVoters = groupData$CC20_410 == 4
  maleVoters = groupData$gender == 1
  femaleVoters = groupData$gender == 2
  
  regNoParty = groupData$CC20_360 == 1
  regDemocrats = groupData$CC20_360 == 2
  regRepublicans = groupData$CC20_360 == 3
  regOther = groupData$CC20_360 == 4

  age.data = data.frame(low=ageCuts[1:numCuts], high=ageCuts[2:(numCuts+1)])
  age.party = data.frame(low=ageCuts[1:numCuts], high=ageCuts[2:(numCuts+1)])
  for (i in 1:numCuts) {
    ageGroup = (groupData$birthyr <= 2020-age.data$low[i] &
                  groupData$birthyr > 2020-age.data$high[i])
    
    age.data$biden[i] = sum(ageGroup & bidenVoters, na.rm=T)
    age.data$trump[i] = sum(ageGroup & trumpVoters, na.rm=T)
    age.data$other[i] = sum(ageGroup & otherVoters, na.rm=T)
    age.data$voters[i] = age.data$biden[i] + age.data$trump[i] + age.data$other[i]
    
    age.data$biden.m[i] = sum(ageGroup & bidenVoters & maleVoters, na.rm=T)
    age.data$trump.m[i] = sum(ageGroup & trumpVoters & maleVoters, na.rm=T)
    age.data$other.m[i] = sum(ageGroup & otherVoters & maleVoters, na.rm=T)
    age.data$voters.m[i] = age.data$biden.m[i] + age.data$trump.m[i] + age.data$other.m[i]
    
    age.data$biden.f[i] = sum(ageGroup & bidenVoters & femaleVoters, na.rm=T)
    age.data$trump.f[i] = sum(ageGroup & trumpVoters & femaleVoters, na.rm=T)
    age.data$other.f[i] = sum(ageGroup & otherVoters & femaleVoters, na.rm=T)
    age.data$voters.f[i] = age.data$biden.f[i] + age.data$trump.f[i] + age.data$other.f[i]
    
    age.party$indep.f[i] = sum(ageGroup & femaleVoters & regNoParty, na.rm=T)
    age.party$democ.f[i] = sum(ageGroup & femaleVoters & regDemocrats, na.rm=T)
    age.party$repub.f[i] = sum(ageGroup & femaleVoters & regRepublicans, na.rm=T)
    age.party$other.f[i] = sum(ageGroup & femaleVoters & regOther, na.rm=T)
    
    age.party$indep.m[i] = sum(ageGroup & maleVoters & regNoParty, na.rm=T)
    age.party$democ.m[i] = sum(ageGroup & maleVoters & regDemocrats, na.rm=T)
    age.party$repub.m[i] = sum(ageGroup & maleVoters & regRepublicans, na.rm=T)
    age.party$other.m[i] = sum(ageGroup & maleVoters & regOther, na.rm=T)
  }
  age.party$total.m = with(age.party, indep.m+repub.m+democ.m+other.m)
  age.party$total.f = with(age.party, indep.f+repub.f+democ.f+other.f)
  
  myMatrix.m = with(age.party, 
                    matrix(c(democ.m/total.m, other.m/total.m, indep.m/total.m, repub.m/total.m),
                    nrow=numCuts, ncol=4))
  myMatrix.f = with(age.party, 
                    matrix(c(democ.f/total.f, other.f/total.f, indep.f/total.f, repub.f/total.f),
                           nrow=numCuts, ncol=4))
  myMatrix.s = rbind(myMatrix.m,myMatrix.f)[c(1,6,2,7,3,8,4,9,5,10),]
  barplot(t(myMatrix.s), col=c('blue', 'green','gray', 'red'),
          main=paste0("Party Registration (", myGroup," by Age Group)"),
          las=2,
          names.arg=c('M18-30','F18-30','M30-45','F30-45','M45-60','F45-60','M60-80','F60-80','M80+','F80+'))
  
  myMatrix.m = matrix(c(age.data$biden.m/age.data$voters.m,
                        age.data$other.m/age.data$voters.m,
                        age.data$trump.m/age.data$voters.m), 
                      nrow=numCuts, ncol=3)
  myMatrix.f = matrix(c(age.data$biden.f/age.data$voters.f,
                        age.data$other.f/age.data$voters.f,
                        age.data$trump.f/age.data$voters.f), 
                      nrow=numCuts, ncol=3)
  myMatrix.s = rbind(myMatrix.m,myMatrix.f)[c(1,6,2,7,3,8,4,9,5,10),]
  barplot(t(myMatrix.s), col=c('blue','green','red'),
          main=paste0("2020 Pres. Votes (", myGroup," by Gender/Age Group)"),
          las=2,
          names.arg=c('M18-30','F18-30','M30-45','F30-45','M45-60','F45-60','M60-80','F60-80','M80+','F80+'))
  
}
dev.off()
