names<-c("verna","karvou","nick","koula","kontos","araphs","dilos",
         "souba","pavlos","johnie")
players<-data.frame(names)
players$stamina<-0
players$handling<-0
players$sprint<-0
players$dribbling<-0
players$passing<-0
#players$tactics<-0
players$defence<-0
players$finishing<-0
players$shooting<-0
players$mentalite<-0


#filling the matrix
players$stamina<-c(3,6,5,5,3,9,4,7,9,4)
players$handling<-c(7,4,5,4,8,7,4,4,5,7)
players$sprint<-c(5,9,5,9,3,9,4,7,8,4)
players$dribbling<-c(4,9,5,7,3,3,6,8,6,5)
players$passing<-c(6,7,7,8,4,3,6,8,7,4)
#players$tactics<-c()
players$defence<-c(7,3,5,6,5,8,4,7,6,5)
players$finishing<-c(6,8,6,6,4,3,7,7,8,4)
players$shooting<-c(5,7,6,8,4,3,6,7,6,3)
players$mentalite<-c(8,8,8,7,8,8,6,7,7,7)





#computing first overall scores
players$ovr_keeper<-players$handling
players$ovr_field<-apply(players[,c(2,(4:10))],1,mean)




##
##plotting
library(ggplot2)
x<-ggplot(players,aes(x=names,y=ovr_field))
x<-x+ geom_bar(stat='identity',width=0.5,fill="#E69F00")
x<-x+coord_flip()
x<-x+theme_bw()
x<-x+
x

##### first estimate of the best team pick

##calculating overall performance of the squad
squad_overall<-sum(players$ovr_field[c(1:4,6:9)])
##team pick and performances
team<-rep(0,8)
einai_deneinai<-c(0,1)
team_performances<-rep(0,70)
t<-1
for( i in einai_deneinai){#prwtos paixths
  team[1]<-i
  for(j in einai_deneinai){
    team[2]<-j
    for (k in einai_deneinai){
      team[3]<-k
      for(l in einai_deneinai){
        team[4]<-l
        for(m in einai_deneinai){
          team[5]<-m
          for(n in einai_deneinai){
            team[6]<-n
            for(o in einai_deneinai){
              team[7]<-o
              for(p in einai_deneinai){
                team[8]<-p
                if (sum(team)==4){#check eligibility
                  
                # performance calculations
                ovr_field<-players$ovr_field[c(1:4,6:9) ]
                team_performances[t]<-sum(ovr_field[team==1])
                print(t);print(team)
                t<-t+1
                
                
                
                
                }
                
              }
            }
          }
        }
      }
    }
  }
}
team_performances<-team_performances - (squad_overall/2)

##distribution of team_performances /symmetric 
team_performances<-as.data.frame(team_performances)
team_performances$iter<-seq(1,70)
my_plot<-ggplot(team_performances,aes(iter,team_performances))
my_plot<-my_plot+ geom_point()
my_plot