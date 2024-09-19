#simple monthly model of population growth in a vertebrate pest
#population grows logistically defined by rmax and K.
#at specificed times a control operation kills a fixed proportion of individuals
#relative effectiveness is just differences in mean abundance between control operations

#need to add simulation of hunting and trapping.

#hunting has an amount of effort (km or time), an encounter rate and a kill rate.
#follow Choquenot et al. 1999
#params are representative only from that study but will do for now.
kill_rate<-function(density, a=50, b=1, d=2) {
	#a= asymptotic kill rate at high density
	#b= density at which no pigs are shot
	#d= efficiency parameter. High d means low efficiency
	kr<-a*(1-exp(-(density-b)*d))
	kr
}

#Roughly based on Choquenot et al. paper. on Helicopter culling
# plot(y=kill_rate(0:10, a=50, b=1, d=2), x=0:10, type="o", las=1,
# 		 xlab="Density (pigs per km^2)",
# 		 ylab="Pigs killed per hour in chopper",
# 		 ylim=c(0, 50),
# 		 xlim=c(0, 10))

#heli control with kill rate function
heli_control<-function(N, A=100, hours){
 times<-round(hours) #hour by hour
 if(times==0) {killed=0} else {
	NN<-numeric(times)
	Nkill<-numeric(times)
	Nkill[1]<-0
	NN[1]<-N   #starting abundance
	for(h in 1:times){
		Nkill[h+1]<-round(min(kill_rate(NN[h]/A), NN[h]))
		NN[h+1]<-NN[h]-Nkill[h+1]
	}
	killed<-sum(Nkill)}
	killed
}

#heli_control(500, A=100, hours=0)
#modded to include fixed control
logistic_mod_monthly<-function(rmax=r_max_schedule,
															 K=500, N0=400, Area=100, sigma=0,
															 fixed_cull_sched=once_annual_control*0,
															 prop_cull_sched=once_annual_control*0,
															 heli_cull_sched=once_annual_control*0, #hours of helicopter shooting control
															 years=10){
	N <- numeric(years*12)                 #monthly increments
	month<-rep(1:12, length.out=length(N)) #indicators for month
	times<-length(N)
	N[1] <- N0
	# Logistic growth model
	for (t in 2:times) {
		#density dependent growth rate
		r=rnorm(1, rmax[month[t]]*(1-(N[t-1]/K)), abs(rmax[month[t]]*sigma))
		#population growth with demographic stochasticity (Poisson) and envir stoch (normal, sigma)
		N[t] <-  rpois(1, N[t-1] *exp(r))
		#proportional culling of the population
		N[t] <-rbinom(1, N[t], (1-prop_cull_sched[month[t]]))
		#fixed number removal
		N[t]<-max(N[t]-fixed_cull_sched[month[t]], 0)
		#helicopter cull (could be applied to ground shooting too)
		N[t]<-N[t]-
			    min(heli_control(N[t], A=Area, hours=heli_cull_sched[month[t]]), N[t])
	}
	return(N)
}

#wrapper function for the model.
simmer<-function(rmax=r_max_schedule,
								 K=500, N0=400, Area=100, sigma=0,
								 fixed_cull_sched=once_annual_control*0,
								 prop_cull_sched=once_annual_control*0,
								 heli_cull_sched=once_annual_control*0,
								 years=10, reps=250){
	sims<-replicate(reps,
									logistic_mod_monthly(rmax,
																			 K, N0, Area, sigma,
																			 fixed_cull_sched,
																			 prop_cull_sched,
																			 heli_cull_sched, #hours of helicopter shooting control
																			 years=years), simplify="matrix") %>%
		data.frame() %>%
	  dplyr::mutate(time=dplyr::row_number()) %>%
	  tidyr::pivot_longer(cols=-time, names_to="rep", values_to="N") %>%
		mutate(rep=readr::parse_number(rep)) %>%
		dplyr::arrange(rep, time, N)
	return(sims)

}

