
# library -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(ggplot2)
library(lemon)
library(patchwork)


# input data --------------------------------------------------------------

df.all <- read_csv('data/scenario_data.csv')
df.var <- read_csv('define/variable.csv')

df <- df.all %>% 
  select(-Model,-Region,-Unit) %>% 
  left_join(df.var) %>% 
  mutate(Variable=Variable2) %>% select(-Variable2) %>% 
  pivot_longer(cols=-c(Scenario,Variable), names_to='Y5', values_to='value') %>% 
  mutate(Y5=as.numeric(Y5),Scenario=factor(Scenario,levels=c('Baseline','1.5C w/ Synfuel','1.5C w/o Synfuel','1.5C OptBio')))

year_all <- seq(2020,2050,5)

dir.create(path='output',showWarnings=F)



# Theme & Function --------------------------------------------------------

MyTheme <- theme_bw() +
  theme(
    panel.border=element_blank(),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="bold"),
    strip.text.y = element_text(size=10, colour = "black", angle = 270,face="bold"),
    axis.text.x=element_text(size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 10,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    axis.title.x = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.ticks.length=unit(0.15,"cm"),
    plot.tag=element_text(face='bold')
  )

set_plot <- function(var){
  plt <- list()
  plt$Color <- as.character(var$Color); names(plt$Color) <- as.character(var$Variable)
  plt$Legend <- as.character(var$Legend); names(plt$Legend) <- as.character(var$Variable)
  return(plt)
}

source('prog/dataframe.R')



# Figure 1 ----------------------------------------------------------------

df_finene <- df %>% 
  filter(Variable %in% finene$Variable, Y5%in%year_all) %>% 
  mutate(Sb=case_when(
    str_detect(Variable,'Bio') ~ 'Biomass',
    TRUE ~ Variable
  )) %>% 
  group_by(Scenario,Sb,Y5) %>% 
  summarise(value=sum(value)) %>% 
  ungroup() %>% 
  rename(Variable=Sb) %>% 
  mutate(Variable=factor(Variable,levels=rev(c('Fin_Ene_Liq_Oil',
                                   'Fin_Ene_SolidsCoa',
                                   'Fin_Ene_Gas',
                                   'Biomass',
                                   'Fin_Ene_Solar',
                                   'Fin_Ene_Ele',
                                   'Fin_Ene_Heat',
                                   'Fin_Ene_Hyd',
                                   'Fin_Ene_Liq_Hyd_syn'))))

df_sector <- list(name1=c('Ind','Res_and_Com','Tra'),name2=list(finind,finbui,fintra))

df_finsec <- map2(df_sector$name1, df_sector$name2,~{
  df %>% filter(Variable%in%..2$Variable) %>% 
    mutate(Sb=case_when(
      str_detect(Variable,'Oil') ~ 'Fin_Ene_Liq_Oil',
      str_detect(Variable,'Coa') ~ 'Fin_Ene_SolidsCoa',
      str_detect(Variable,'Gas') ~ 'Fin_Ene_Gas',
      str_detect(Variable,'Bio') ~ 'Biomass',
      str_detect(Variable,'Solar') ~ 'Fin_Ene_Solar',
      str_detect(Variable,'Ele') ~ 'Fin_Ene_Ele',
      str_detect(Variable,'Heat') ~ 'Fin_Ene_Heat',
      str_detect(Variable,'Hyd')&!str_detect(Variable,'syn') ~ 'Fin_Ene_Hyd',
      str_detect(Variable,'syn') ~ 'Fin_Ene_Liq_Hyd_syn'
    )) %>% 
    group_by(Scenario,Sb,Y5) %>% 
    summarise(value=sum(value)) %>% 
    rename(Variable=Sb) %>%
    mutate(Variable=factor(Variable,levels=rev(c('Fin_Ene_Liq_Oil',
                                     'Fin_Ene_SolidsCoa',
                                     'Fin_Ene_Gas',
                                     'Biomass',
                                     'Fin_Ene_Solar',
                                     'Fin_Ene_Ele',
                                     'Fin_Ene_Heat',
                                     'Fin_Ene_Hyd',
                                     'Fin_Ene_Liq_Hyd_syn')))) %>% 
    mutate(Se=..1)
}) %>% bind_rows() %>% 
  mutate(Se=case_when(
    Se=='Ind'~'Industry',
    Se=='Tra'~'Transport',
    TRUE~'Buildings'
  )) %>%
  mutate(Se=factor(Se,levels=c('Industry','Buildings','Transport')))

plt <- set_plot(finene %>% 
                  filter(!str_detect(Variable,'Bio')) %>%
                  bind_rows(data.frame(Variable='Biomass',Legend='Biomass',Color='darkolivegreen2')) %>%
                  mutate(Variable=as.character(Variable)) %>% 
                  mutate(Legend=factor(Legend,levels=c('Oil',
                                                       'Coal',
                                                       'Gas',
                                                       'Biomass',
                                                       'Solar',
                                                       'Electricity',
                                                       'Heat',
                                                       'Hydrogen',
                                                       'Synfuel'))) %>% 
                  arrange(Legend))

g_finarea <- df_finene %>%
  filter(Scenario!='Baseline') %>% 
  mutate(Scenario=factor(Scenario,levels=c('1.5C w/ Synfuel','1.5C w/o Synfuel','1.5C OptBio'))) %>% 
  ggplot() +
  geom_area(aes(x=Y5,y=value,fill=Variable)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='left axis') +
  facet_wrap(vars(Scenario),nrow=1) +
  guides(fill=guide_legend(reverse=TRUE)) +
  labs(y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(legend.title = element_text(),
        legend.position = 'none')

g_finbar <- df_finsec %>%
  filter(Y5==2050,Scenario!='Baseline') %>%
  mutate(Scenario=str_remove_all(Scenario,'1.5C ')) %>% 
  mutate(Scenario=factor(Scenario,levels=c('w/ Synfuel','w/o Synfuel','OptBio'))) %>% 
  ggplot() +
  geom_bar(aes(x=Scenario,y=value,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  guides(fill=guide_legend(reverse=FALSE,ncol=1)) +
  labs(y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  facet_wrap(vars(Se)) +
  MyTheme +
  theme(legend.title = element_text(),
        legend.position = 'right',
        legend.text=element_text(size=10))
plot(g_finbar)

l_finene <- g_legend(g_finbar+theme(legend.position='bottom'))

g_fin <- g_finarea + g_finbar + plot_layout(nrow=1, width=c(4.5,3)) + plot_annotation(tag_levels = 'a', tag_suffix = ')')

ggsave(paste0('output/Fig1.png'),width=9,height=3.5)



# Figure 2 --------------------------------------------------------------------

df_emi <- df %>% 
  filter(Variable %in% emisec$Variable)

plt <- set_plot(emisec %>% filter(Variable!='Emi_CO2_Ene_Dem_AFO'))

g_total <- df %>%
  filter(Variable=='Emi_CO2_Ene_and_Ind_Pro_inc_Dir_Air',Scenario%in%c('Baseline','1.5C w/ Synfuel')) %>%
  mutate(Scenario=recode(Scenario,`1.5C w/ Synfuel`='1.5C')) %>%
  group_by(Scenario,Y5) %>%
  summarise(value=sum(value)) %>%
  ggplot() +
  geom_path(aes(x=Y5,y=value/1000,color=Scenario)) +
  labs(y=expression(paste({CO[2]},' emissions (Gt',{CO[2]},' ',{yr^-1},')'))) +
  scale_y_continuous(limits=c(-15,60)) +
  geom_hline(yintercept=0,linetype='dashed',color='grey60',size=0.5) +
  scale_color_manual(values=c('indianred2','deepskyblue3')) +
  MyTheme +
  theme(legend.position = c(0.32,0.4),
        legend.background = element_blank())

g_sec <- df_emi %>%
  filter(Y5==2050, Scenario!='Baseline') %>%
  mutate(Variable=recode(Variable,Emi_CO2_Ene_Dem_AFO='Emi_CO2_Ene_Dem_Oth_Sec')) %>% 
  group_by(Scenario,Variable,Y5) %>% 
  summarise(value=sum(value)) %>% 
  mutate(Variable=factor(Variable,levels=emisec$Variable)) %>% 
  mutate(Scenario=str_remove_all(Scenario,'1.5C ')) %>% 
  mutate(Scenario=factor(Scenario,levels=c('w/ Synfuel','w/o Synfuel','OptBio'))) %>% 
  ggplot() +
  geom_bar(aes(x=Scenario,y=value/1000,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='Variable') +
  guides(fill=guide_legend(reverse=TRUE)) +
  labs(y=expression(paste({CO[2]},' emissions (Gt',{CO[2]},' ',{yr^-1},')'))) +
  geom_hline(yintercept=0,linetype='dashed',color='grey60',size=0.5) +
  MyTheme

g_rsd <- df_emi %>%
  filter(Y5==2050, Scenario!='Baseline',Variable%in%c('Emi_CO2_Ene_Dem_Ind','Emi_CO2_Ene_Dem_Tra','Emi_CO2_Ene_Dem_Res_and_Com')) %>%
  mutate(Variable=recode(Variable,'Emi_CO2_Ene_Dem_Ind'='Industry','Emi_CO2_Ene_Dem_Tra'='Transport','Emi_CO2_Ene_Dem_Res_and_Com'='Buildings')) %>%
  mutate(Variable=factor(Variable,levels=c('Industry','Buildings','Transport'))) %>% 
  mutate(Scenario=factor(Scenario,levels=c('1.5C w/ Synfuel','1.5C w/o Synfuel','1.5C OptBio'))) %>% 
  ggplot() +
  geom_point(aes(x=Variable,y=value/1000,color=Variable,shape=Scenario),size=2,stroke=1) +
  scale_color_manual(values=c('indianred2','deepskyblue3','mediumseagreen')) +
  scale_shape_manual(values=c(0,1,2)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(y=expression(paste('Residual ',{CO[2]},' emissions (Gt',{CO[2]},' ',{yr^-1},')'))) +
  MyTheme

g_emi <- g_total + plot_spacer() + g_sec + plot_spacer() +g_rsd + 
  plot_layout(widths=c(5,0,2.5,0,2.5)) + plot_annotation(tag_levels = 'a',tag_suffix = ')')

ggsave(paste0('output/Fig2.png'),width = 8, height = 3.7)



# Figure 3 --------------------------------------------------------------------

g_prccar <- df %>%
  filter(Variable=='Prc_Car',Scenario!='Baseline',Y5%in%c(2020:2050)) %>% 
  mutate(Scenario=factor(Scenario,levels=c('1.5C w/ Synfuel','1.5C w/o Synfuel','1.5C OptBio'))) %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=value,color=Scenario),size=0.4) +
  geom_point(aes(x=Y5,y=value,color=Scenario,shape=Scenario),stroke=0.7) +
  scale_color_manual(values=c('indianred2','deepskyblue3','darkgoldenrod2')) +
  scale_shape_manual(values=c(0,1,2)) +
  labs(y=expression(paste('Carbon price (US$ t-',{CO[2]^-1},')'))) +
  MyTheme +
  theme(legend.position=c(0.3,0.8),
        legend.text=element_text(size=10))

g_totcos <- df %>% 
  filter(Variable=='Pol_Cos_Add_Tot_Ene_Sys_Cos',Scenario!='Baseline') %>%
  filter(Y5 %in% c(2025:2050)) %>% 
  mutate(diScenarioount=0.95^(Y5-2023)) %>%
  mutate(value=value*diScenarioount*5/1000) %>% 
  group_by(Scenario) %>% 
  summarise(value=sum(value)) %>% 
  mutate(Scenario=str_remove_all(Scenario,'1.5C ')) %>% 
  mutate(Scenario=factor(Scenario,levels=c('w/ Synfuel','w/o Synfuel','OptBio'))) %>% 
  ggplot() +
  geom_bar(aes(x=Scenario,y=value),stat='identity') +
  labs(y='Cumulative energy system cost\n(trillion US$)') +
  MyTheme 

g_mitcost <- g_prccar + g_totcos + plot_layout(width=c(2,1)) + plot_annotation(tag_levels='a',tag_suffix=')')

ggsave(paste0('output/Fig3.png'),width=7,height=4)



# Figure 4 --------------------------------------------------------------------

df_enshr <- map2(df_sector$name1,df_sector$name2,~{
  v1 <- ..1
  v2 <- ..2
  df %>% filter(Variable%in%v2$Variable|Variable==paste0('Fin_Ene_',v1)) %>%
    pivot_wider(names_from=Variable,values_from=value) %>%
    mutate(across(starts_with('Fin_Ene'),~.*100/eval(parse(text=paste0('Fin_Ene_',v1))))) %>%
    pivot_longer(cols=starts_with('Fin_Ene'),names_to='Variable',values_to='value')
}) %>% bind_rows()

df_eleshr <- df_enshr %>% 
  filter(str_detect(Variable,'Ele'),Scenario%in%c('1.5C OptBio','1.5C w/ Synfuel','1.5C w/o Synfuel'),Y5 %in% year_all) %>%
  mutate(Variable=case_when(
    str_detect(Variable,'Ind')~'Industry',
    str_detect(Variable,'Res_and_Com')~'Buildings',
    str_detect(Variable,'Tra')~'Transport'
  )) %>%
  mutate(Variable=factor(Variable,levels=c('Industry','Buildings','Transport'))) %>%
  mutate(Sn=rep('Electricity'))

df_hydshr <- df_enshr %>% 
  filter(str_detect(Variable,'Hyd'),!str_detect(Variable,'Liq'),Scenario%in%c('1.5C OptBio','1.5C w/ Synfuel','1.5C w/o Synfuel'),Y5 %in% year_all) %>%
  mutate(Variable=case_when(
    str_detect(Variable,'Ind')~'Industry',
    str_detect(Variable,'Res_and_Com')~'Buildings',
    str_detect(Variable,'Tra')~'Transport'
  )) %>%
  mutate(Variable=factor(Variable,levels=c('Industry','Buildings','Transport'))) %>%
  mutate(Sn=rep('Hydrogen'))

df_hycshr <- df_enshr %>% 
  filter(str_detect(Variable,'Solid|Liq|Gas'),Scenario%in%c('1.5C OptBio','1.5C w/ Synfuel','1.5C w/o Synfuel'),Y5 %in% year_all) %>%
  mutate(Se=case_when(
    str_detect(Variable,'Ind')~'Industry',
    str_detect(Variable,'Res_and_Com')~'Buildings',
    str_detect(Variable,'Tra')~'Transport'
  )) %>%
  mutate(Se=factor(Se,levels=c('Industry','Buildings','Transport'))) %>%
  group_by(Scenario,Se,Y5) %>% 
  summarise(value=sum(value)) %>%
  rename(Variable=Se) %>% 
  mutate(Sn=rep('Hydrocarbon'))

g_enshr <- bind_rows(df_eleshr,df_hydshr,df_hycshr) %>%
  filter(Y5>=2030) %>% 
  pivot_wider(names_from=Variable,values_from=value,values_fill=0) %>%
  pivot_longer(cols=c(Industry,Buildings,Transport),names_to='Variable',values_to='value') %>% 
  mutate(Sn=factor(Sn,levels=c('Electricity','Hydrogen','Hydrocarbon'))) %>%
  mutate(Variable=factor(Variable,levels=c('Industry','Buildings','Transport'))) %>%
  mutate(Scenario=factor(Scenario,levels=c('1.5C w/ Synfuel','1.5C w/o Synfuel','1.5C OptBio'))) %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=value,color=Scenario),size=0.3) +
  geom_point(aes(x=Y5,y=value,color=Scenario,shape=Scenario),stroke=0.7,size=1) +
  geom_hline(yintercept=0,linetype='dashed',color='grey60',size=0.3) +
  scale_color_manual(values=c('indianred2','deepskyblue3','darkgoldenrod2')) +
  scale_shape_manual(values=c(0,1,2)) +
  labs(y='Share of energy carriers in final energy (%)') +
  facet_grid(cols=vars(Variable),rows=vars(Sn)) +
  MyTheme +
  theme(legend.position='bottom',
        legend.text=element_text(size=10))

ggsave(paste0('output/Fig4.png'),width=5,height=7)



# Figure 5 --------------------------------------------------------------------

df_str <- df %>% 
  filter(Variable %in% c('Str_Inv_Ene_Dem_Ind','Str_Inv_Ene_Dem_Bui','Str_Inv_Ene_Dem_Tra','Str_Inv_Ene_Sup')) %>%
  mutate(Variable=recode(Variable,Str_Inv_Ene_Dem_Ind='Industry',Str_Inv_Ene_Dem_Bui='Buildings',Str_Inv_Ene_Dem_Tra='Transport',Str_Inv_Ene_Sup='Energy supply')) %>%
  pivot_wider(names_from=Scenario,values_from=value) %>% 
  mutate(across(starts_with('1.5C'),~.-Baseline)) %>% select(-Baseline) %>% 
  pivot_longer(cols=starts_with('1.5C'),names_to='Scenario',values_to='value') %>% 
  mutate(Variable=factor(Variable,levels=c('Industry','Buildings','Transport','Energy supply')),
         Scenario=factor(Scenario,levels=c('1.5C OptBio','1.5C w/ Synfuel','1.5C w/o Synfuel')))

g_str <- df_str %>%
  filter(Y5>=2020) %>% 
  mutate(Scenario=factor(Scenario,levels=c('1.5C w/ Synfuel','1.5C w/o Synfuel','1.5C OptBio'))) %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=value,color=Scenario),size=0.4) +
  geom_point(aes(x=Y5,y=value,color=Scenario,shape=Scenario),stroke=0.7) +
  geom_hline(yintercept=0,linetype='dashed',color='grey60',size=0.4) +
  scale_color_manual(values=c('indianred2','deepskyblue3','darkgoldenrod2')) +
  scale_shape_manual(values=c(0,1,2)) +
  labs(y=expression(paste('Stranded investment (billion US$ ',{yr^-1},')'))) +
  facet_wrap(vars(Variable),nrow=1) +
  MyTheme +
  theme(legend.position='bottom',
        legend.text=element_text(size=10))

ggsave(paste0('output/Fig5.png'),width=8,height=4)



# Figure 6 --------------------------------------------------------------------

df_path <- df %>%
  filter(str_detect(Variable, 'Trd_'),!str_detect(Variable,'Vol')) %>% 
  mutate(Variable=recode(Variable,Trd_Coa='Coal',
                         Trd_Cru='Crude oil',
                         Trd_Oil='Oil products',
                         Trd_Gas='LNG',
                         Trd_Crn='Solid biomass',
                         Trd_Bio='Liquid biomass',
                         Trd_Amm='Ammonia',
                         Trd_Syn='Synthetic fuels')) %>% 
  mutate(Variable=factor(Variable,levels=rev(c('Coal','Crude oil','Oil products','LNG','Solid biomass','Liquid biomass','Ammonia','Synthetic fuels')))) %>% 
  filter(Y5%in%c(2030,2040,2050),Scenario!='Baseline') %>% 
  mutate(Variable2=case_when(
    str_detect(Variable,'biomass')~'Biomass',
    str_detect(Variable,'Ammonia')~'Ammonia',
    str_detect(Variable,'Synthetic fuels')~'Synthetic fuels',
    TRUE~'Fossil'
  )) %>% 
  group_by(Variable2,Scenario,Y5) %>% 
  summarise(value=sum(value)) %>% 
  mutate(Variable2=factor(Variable2,levels=c('Synthetic fuels','Ammonia','Biomass','Fossil')))

g_fbar <- df %>%
  filter(str_detect(Variable, 'Trd_'),!str_detect(Variable,'Vol')) %>% 
  mutate(Variable=recode(Variable,Trd_Coa='Coal',
                         Trd_Cru='Crude oil',
                         Trd_Oil='Oil products',
                         Trd_Gas='LNG',
                         Trd_Crn='Solid biomass',
                         Trd_Bio='Liquid biomass',
                         Trd_Amm='Ammonia',
                         Trd_Syn='Synthetic fuels')) %>% 
  mutate(Variable=factor(Variable,levels=rev(c('Coal','Crude oil','Oil products','LNG','Solid biomass','Liquid biomass','Ammonia','Synthetic fuels')))) %>% 
  filter(Y5%in%c(2030,2040,2050),Scenario!='Baseline') %>%
  group_by(Scenario,Y5) %>% 
  mutate(SUM=sum(value)) %>% 
  mutate(value=value*100/SUM) %>% 
  select(-SUM) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=Variable),stat='identity') +
  geom_path(data=df_path,aes(x=Y5,y=value/200*100,color=Variable2),size=0.5) +
  geom_point(data=df_path,aes(x=Y5,y=value/200*100,color=Variable2,shape=Variable2),size=1.5,stroke=0.7,fill='white') +
  scale_y_continuous(name='Share in energy trade (%)',breaks=seq(0,100,25),
                     sec.axis = sec_axis(~.*200/100,breaks=seq(0,200,50),name=expression(paste('Energy trade by ship (EJ ',{yr^-1},')')))) +
  scale_fill_manual(values=c('mediumorchid1',
                             'thistle2',
                             'darkolivegreen2',
                             'darkolivegreen4',
                             'moccasin',
                             'sandybrown',
                             'tan3',
                             'grey50'),name='left axis') +
  scale_color_manual(values=c('indianred2','darkgoldenrod2','palegreen3','deepskyblue3'),name='right axis') +
  scale_shape_manual(values=c(21,22,23,24),name='right axis') +
  facet_wrap(vars(Scenario),nrow=1) +
  guides(fill=guide_legend(order=1)) +
  labs(tag='a)') +
  MyTheme +
  theme(legend.title=element_text(),
        legend.text=element_text(size=10),
        legend.position = 'none')

g_stock <- df %>%
  filter(str_detect(Variable,'Shi_')) %>% 
  mutate(Variable=recode(Variable,Shi_Bul='Bulk carrier',
                         Shi_Cru='Crude oil tanker',
                         Shi_Pro='Products tanker',
                         Shi_LNG='LNG tanker',
                         Shi_NH3='Ammonia tanker')) %>% 
  filter(Y5>=2030,Scenario!='Baseline') %>%
  pivot_wider(names_from=Variable,values_from=value) %>%
  mutate(Total=`Bulk carrier`+`Crude oil tanker`+`Products tanker`+`LNG tanker`+`Ammonia tanker`) %>%
  pivot_longer(col=-c(Scenario,Y5),names_to='Variable',values_to='value') %>% 
  mutate(Variable=factor(Variable,levels=c('Bulk carrier','Crude oil tanker','Products tanker','LNG tanker','Ammonia tanker','Total'))) %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=value,color=Scenario),size=0.5) +
  geom_point(aes(x=Y5,y=value,color=Scenario,shape=Scenario),size=1.5,stroke=0.7) +
  scale_color_manual(values=c('indianred2','deepskyblue3','darkgoldenrod2')) +
  scale_shape_manual(values=c(0,1,2)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(y='Energy trade vessel (million dwt)',tag='b)') +
  facet_wrap(vars(Variable),scales='free') +
  MyTheme +
  theme(
    legend.position='bottom',
    legend.text=element_text(size=10)
  )

l_fbar <- g_legend(g_fbar+theme(legend.position = 'right'))
g_trade <- (g_fbar + l_fbar + plot_layout(width=c(3,1)))/g_stock

ggsave(paste0('output/Fig6.png'),width=6.5,height=9)
