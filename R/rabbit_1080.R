# Rabbit control script
source(here::here("_posts", "2024-09-18-1080", "utility_functions.R"))
#Hone (1999) annual r = 0.14, therefore monthly r = 0.14/12
r_max_schedule<-rep(2.06/12, 12)
prod(exp(r_max_schedule))

#set up an annual control schedule to use in the models.
#this has control in September, but it could be altered to whatever we want including multiple annual control
once_annual_control<-c(rep(0, 8), 1, rep(0, 3))   #once annual control in September

logistic_mod_monthly(rmax=r_max_schedule,
                     K=5000, N0=4000, Area=10)

# Make up some numbers
# 25 rabbits per km transect with 50 strip width = 25/0.05
#K=         5000 rabbits (assuming pop was at carrying capacity, perhaps it wasn't?)
#Area=      10 km2
#Density =  500   rabbits per km2


test_run_0<-simmer(prop_cull_sched=once_annual_control*0,
                   reps=250, K=5000, N0=4000, Area=10, sigma=0.1, years=25) %>% mutate(control=0)
test_run_20<-simmer(prop_cull_sched=once_annual_control*0.2,
                    reps=250, K=5000, N0=4000, Area=10, sigma=0.1, years=25) %>% mutate(control=20)
test_run_40<-simmer(prop_cull_sched=once_annual_control*0.4,
                    reps=250, K=5000, N0=4000, Area=10,sigma=0.1,  years=25) %>% mutate(control=40)
test_run_60<-simmer(prop_cull_sched=once_annual_control*0.6,
                    reps=250, K=5000, N0=4000, Area=10,sigma=0.1,  years=25) %>% mutate(control=60)
test_run_80<-simmer(prop_cull_sched=once_annual_control*0.8,
                    reps=250, K=5000, N0=4000, Area=10, sigma=0.1, years=25) %>% mutate(control=80)
test_run_90<-simmer(prop_cull_sched=once_annual_control*0.9,
                    reps=250, K=5000, N0=4000, Area=10, sigma=0.1, years=25) %>% mutate(control=90)

test_run<-bind_rows(test_run_0, test_run_20, test_run_40, test_run_60, test_run_80, test_run_90)

#plot the results
test_run %>% mutate(years=time/12) %>% ggplot(aes(x=years, y=N))+
  geom_line(aes(group=rep), col="steelblue", alpha=0.1)+
  stat_summary(geom="line", fun = "median", col="firebrick", lwd=0.3)+
  stat_summary(geom="line", fun = function(x){quantile(x, 0.025)}, col="firebrick", lty=2, lwd=0.2)+
  stat_summary(geom="line", fun = function(x){quantile(x, 0.975)}, col="firebrick", lty=2, lwd=0.2)+
  scale_x_continuous(breaks=seq(0, 25, by=5))+
  ylim(0, NA)+
  facet_wrap(~control, labeller = labeller(control = ~ paste(.x, "% cull once per annum")))+
  xlab("Time (years)")+
  theme_bw()


##########################################################################################
#simulate some management scenarios for Made up landscape
#  N=4000, K=5000, monthly r = 0.7/12, sigma=0.1
#  1. unmanaged, no culling
#  2. unmanged, but double the environmental stochasiticity
#  3. 1080 used by itself (efficacy = 85%)
#  4. 1080 used by itself (efficacy = 70%)
#  5. Pindone used by itself (efficacy = 60%)
#  6. Warren ripping used by itself (80 %)
#  7. Ripping + Fumigation x 3 (77 %)
#  8. Poisoninig + Ripping x 3 (85 %)
#  9. Poisoning + Fumigation x 3 (90 %)
#  10. Poisoning + Ripping + Fumigation x 3 (98 %)
##########################################################################################

simulated_scenarios <- list()

simulated_scenarios$unmanaged<-simmer(prop_cull_sched=once_annual_control*0,
                  reps=250, K=5000, N0=4000, Area=10, sigma=0.1, years=25) %>% mutate(method="Unmanaged")

simulated_scenarios$unmanaged_hisigma<-simmer(prop_cull_sched=once_annual_control*0,
                          reps=250, K=5000, N0=4000, Area=10, sigma=0.2, years=25) %>% mutate(method="Unmanaged, high sigma")

simulated_scenarios$poison_1080_85 <-simmer(prop_cull_sched=once_annual_control*0.85,
                          reps=250, K=5000, N0=4000, Area=10, sigma=0.2, years=25) %>% mutate(method="1080, 85% efficacy")

simulated_scenarios$poison_1080_70 <-simmer(prop_cull_sched=once_annual_control*0.70,
                        reps=250, K=5000, N0=4000, Area=10, sigma=0.2, years=25) %>% mutate(method="1080, 70% efficacy")

simulated_scenarios$poison_pindone_60 <-simmer(prop_cull_sched=once_annual_control*0.60,
                        reps=250, K=5000, N0=4000, Area=10, sigma=0.2, years=25) %>% mutate(method="Pindone, 60% efficacy")

simulated_scenarios$warren_ripping <-simmer(prop_cull_sched=once_annual_control*0.80,
                           reps=250, K=5000, N0=4000, Area=10, sigma=0.2, years=25) %>% mutate(method="Warren ripping, 80% efficacy")

simulated_scenarios$warren_ripping_fumigation <-simmer(prop_cull_sched=once_annual_control*0.77,
                        reps=250, K=5000, N0=4000, Area=10, sigma=0.2, years=25) %>% mutate(method="Warren ripping + fumigation, 77% efficacy")

simulated_scenarios$poisoning_ripping <-simmer(prop_cull_sched=once_annual_control*0.85,
                                   reps=250, K=5000, N0=4000, Area=10, sigma=0.2, years=25) %>% mutate(method="Warren ripping + poisoning, 85% efficacy")

simulated_scenarios$poisoning_fumigation <-simmer(prop_cull_sched=once_annual_control*0.90,
                           reps=250, K=5000, N0=4000, Area=10, sigma=0.2, years=25) %>% mutate(method="Poisoning + fumigation, 90% efficacy")

simulated_scenarios$poisoning_ripping_fumigation <-simmer(prop_cull_sched=once_annual_control*0.98,
                           reps=250, K=5000, N0=4000, Area=10, sigma=0.2, years=25) %>% mutate(method="Warren ripping + poisoning + fumigation, 98 % efficacy")


bind_rows(simulated_scenarios) %>%
  mutate(years=time/12) %>%
  mutate(method=factor(method, levels = unique(bind_rows(simulated_scenarios)$method)))  %>%
  ggplot(aes(x=years, y=N))+
  geom_line(aes(group=rep), col="steelblue", alpha=0.05)+
  stat_summary(geom="line", fun = "median", col="firebrick", lwd=0.3)+
  stat_summary(geom="line", fun = function(x){quantile(x, 0.025)}, col="firebrick", lty=2, lwd=0.2)+
  stat_summary(geom="line", fun = function(x){quantile(x, 0.975)}, col="firebrick", lty=2, lwd=0.2)+
  geom_hline(yintercept=5000, lty=2)+
  scale_x_continuous(breaks=seq(0, 25, by=5))+
  ylim(0, NA)+
  facet_wrap(~method, labeller = labeller(method = ~ paste(.x)), nrow=5)+
  xlab("Time (years)")+
  title("Rabbit control scenarios")+
  theme_bw()
