set.seed(100)
#X|theta~Binomial(n,theta)
#theta~Beta(alpha,beta)
#N=10000
ci=function(alpha,beta,n){
  theta=rbeta(10000,alpha,beta)
  x=rep(0,length(theta))
  for (i in 1:length(theta)) {
    
    x[i]=rbinom(1,16,theta[i])
  }
  
  #fix x=10
  th1=c()
  for(i in 1:length(x)){
    if(x[i]==6){
      th1=c(th1,theta[i])
    }
  }
  length(th1)
  l_x_star=qbeta(0.025,(alpha+x^*),(n-10+beta))
  u_x_star=qbeta((1-0.025),(alpha+n),(n-x+beta))
  n_theta=sum(l_x_star<=th1 & th1<=u_x_star)
  a=n_theta/length(th1)
  c(l_x_star,u_x_star,a)
}
