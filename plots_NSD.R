## Setup

library(dplyr)
library(tidyverse)
library(tidyr)
library(waterfalls)
library(stringr)
library(ggrepel)
library(lubridate)
library(ggjoy)
library(scales)
library(ranger)
library(caret)
library(pROC)
library(ModelMetrics)
library(PRROC)
library(table1)

## Import Data

data <- read.csv('../opd.csv', stringsAsFactors = FALSE)
deaths <- read.csv('../calc_deaths.csv', stringsAsFactors = FALSE)

data %>% group_by(OPO) %>% summarise(n=n())

## Preprocess

date_convert <- function(x){
  as.POSIXct(x, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
}
data <- data %>% mutate_at(c('Age'), as.numeric)
data <- data %>% mutate_at(c('Procured_Year'), as.integer)
data <- data %>% mutate_at(c('brain_death', 'approached', 'authorized', 'procured', 'transplanted', 
                             'Tissue_Referral', 'Eye_Referral'), as.logical)
data <- data %>% mutate_at(names(data)[grepl("time_*", names(data))], date_convert)

## Compute overall Performance Metrics for each OPO

performance <- data %>% select(OPO, Referral_Year, PatientID, approached, authorized, procured, transplanted) %>% 
                group_by(OPO, Referral_Year) %>%
                  summarise(referrals = n(), 
                            approaches = sum(approached), 
                            authorizations = sum(authorized), 
                            procurements = sum(procured), 
                            transplants = sum(transplanted)) %>% rename(Year=Referral_Year)

### Join deaths data to summarized OPO performance
performance <- performance %>% left_join(deaths %>% select(OPO, Year, calc_deaths), c("OPO","Year"))
performance <- performance %>% mutate(`Approach Rate` = approaches / referrals, 
                                      `Authorization Rate` = authorizations / approaches, 
                                      `Authorized Procurement Rate` = procurements / authorizations,
                                      `Procured Transplant Rate` = transplants / procurements, 
                                      `Donation Rate` = transplants / calc_deaths) %>% 
                pivot_longer(cols = ends_with("Rate")) %>% select(OPO, Year, name, value)

## Compute Performance Metrics by Race

race_performance <- data %>% select(OPO, Referral_Year, Race, PatientID, approached, authorized, procured, transplanted) %>% 
  group_by(OPO, Race, Referral_Year) %>%
  summarise(referrals = n(), 
            approaches = sum(approached), 
            authorizations = sum(authorized), 
            procurements = sum(procured), 
            transplants = sum(transplanted)) %>% rename(Year=Referral_Year) %>% 
  mutate(`Approach Rate` = approaches / referrals, 
                                      `Authorization Rate` = authorizations / approaches, 
                                      `Authorized Procurement Rate` = procurements / authorizations,
                                      `Procured Transplant Rate` = transplants / procurements) %>% 
  pivot_longer(cols = ends_with("Rate")) %>% select(OPO, Year, name, value)

# Figure 2
## Figure 2a: waterfall

wf <- data %>% group_by(Referral_Year) %>%
  summarise(referrals = n(), 
            approaches = sum(approached), 
            authorizations = sum(authorized), 
            procurements = sum(procured), 
            transplants = sum(transplanted)) %>% rename(Year=Referral_Year) %>% 
  filter(Year==2020) %>% ungroup() %>%
  mutate(Referred=referrals, 
         `Evaluated, not Approached` = approaches - referrals, 
         `Approached, not Authorized`= authorizations - approaches, 
         `Authorized, not Procured` = procurements - authorizations, 
         `Procured, not Transplanted` = transplants -  procurements) %>% 
  select(-referrals, -approaches, -authorizations, -procurements, -transplants,-Year) %>% 
  pivot_longer(cols=everything()) %>% mutate(name = str_wrap(name, width = 10))
waterfall(wf, calc_total = TRUE, rect_width = 0.6, draw_lines = TRUE, linetype = 2,
          fill_by_sign = FALSE, 
          fill_colours = c("#D5E6F2", "#FF9999", "#FF9999", "#FF9999", "#FF9999"),
          rect_border = c("#D5E6F2", "#FF9999", "#FF9999", "#FF9999", "#FF9999"),
          total_rect_color = "darkgreen",
          total_axis_text = "Transplanted \nDonors",
          rect_text_labels = scales::label_comma(accuracy = 1)(abs(wf$value)),
          total_rect_text = scales::label_comma(accuracy = 1)(sum(wf$value)), 
          draw_axis.x = "front", rect_text_size=1.6) +
  theme_classic() + xlab("") + 
  ylab("Count") +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,30000, 5000), limits = c(0,26000), 
                     labels = scales::comma) + 
  theme(text= element_text(size=16))
ggsave('../figures/fig2a.pdf', height = 4, width=8)

## Figure 2b: Metrics

performance_all <- data %>% select(Referral_Year, PatientID, approached, authorized, procured, transplanted) %>% 
                      group_by(Referral_Year) %>%
                      summarise(referrals = n(), 
                                approaches = sum(approached), 
                                authorizations = sum(authorized), 
                                procurements = sum(procured), 
                                transplants = sum(transplanted)) %>% rename(Year=Referral_Year) %>% 
                      left_join(deaths %>% group_by(Year) %>% 
                                    summarise(calc_deaths = sum(calc_deaths)), c("Year")) %>% 
                        mutate(`Approach Rate` = approaches / referrals, 
                               `Authorization Rate` = authorizations / approaches, 
                               `Authorized Procurement Rate` = procurements / authorizations,
                               `Procured Transplant Rate` = transplants / procurements, 
                               `Donation Rate (% of CALC Deaths)` = transplants / calc_deaths, 
                               `Referred Donation Rate (% of Referrals)` = transplants / referrals,
                               `Referrals` = referrals, 
                               `CALC Deaths` = calc_deaths) %>% filter(Year < 2021) %>% 
                          pivot_longer(cols = 8:15) %>% select(Year, name, value)


cbPalette <- c("#56B4E9", "#009E73", "#E69F00", "#CCC442","#0072B2", "#CC79A7", "#D55E00", "#999999", "#000000")

performance_all$name <- factor(performance_all$name, 
                           levels = c("Approach Rate", "Authorization Rate", "Authorized Procurement Rate",
                                      "Procured Transplant Rate", "Donation Rate (% of CALC Deaths)", 
                                      "Referred Donation Rate (% of Referrals)", "Referrals", "CALC Deaths"))
ggplot(performance_all %>% filter(name %in% c("Approach Rate", "Authorization Rate", "Authorized Procurement Rate",
                                              "Procured Transplant Rate")), aes(x=Year, y=value, col=name, shape=name)) + 
  geom_line(lty=3, lwd=0.5) + geom_point(size=3) + 
  theme_classic() + xlim(2015, 2020) + theme(legend.position = "bottom") + 
  scale_colour_manual(values = cbPalette[1:5]) + 
  scale_y_continuous(breaks=seq(0,1,0.25), limits = c(-0.0, 1.05), expand = c(0, 0)) + 
  ylab("") + 
  guides(color=guide_legend(ncol=3), shape=guide_legend(ncol=3)) +
  theme(text = element_text(size=16), legend.title = element_blank()) + 
  geom_text_repel(aes(label = round(value, 2), size=1), nudge_x = 0, 
            show.legend = FALSE) 
ggsave('../figures/fig2b.pdf', height = 3, width=6)

ggplot(performance_all %>% filter(!name %in% c("Approach Rate", "Authorization Rate", "Authorized Procurement Rate",
                                               "Procured Transplant Rate", "Referrals", "CALC Deaths")), 
       aes(x=Year, y=value, col=name, shape=name)) + 
  geom_line(lty=3, lwd=0.5) + geom_point(size=3) + 
  theme_classic() + xlim(2015, 2020) + theme(legend.position = "bottom") + 
  scale_colour_manual(values = cbPalette[6:7]) + 
  scale_y_continuous(breaks=seq(0,1,0.05), limits = c(-0.0, 0.2), expand = c(0, 0)) + 
  ylab("") + 
  guides(color=guide_legend(ncol=1), shape=guide_legend(ncol=1)) +
  theme(text = element_text(size=16), legend.title = element_blank()) + 
  geom_text_repel(aes(label = round(value, 2), size=1), nudge_x = 0, 
                  show.legend = FALSE)
ggsave('../figures/fig2d.pdf', height = 3, width=4.5)

ggplot(performance_all %>% 
         filter(name %in% c("Referrals",  "CALC Deaths")), 
       aes(x=Year, y=value, col=name, shape=name)) + 
  geom_line(lty=3, lwd=0.5) + geom_point(size=3) + 
  theme_classic() + xlim(2015, 2020) + theme(legend.position = "bottom") + 
  scale_colour_manual(values = c("#009E73", "#006600")) + 
  ylab("") + scale_y_continuous(breaks=seq(0, 30000, 5000), labels=unit_format(unit = "k", scale = 1/1000)) + 
  guides(color=guide_legend(ncol=1), shape=guide_legend(ncol=1)) +
  theme(text = element_text(size=18), legend.title = element_blank()) + 
  geom_text(aes(label = paste0(round(value/1000, 1), "k"), size=1), nudge_x = 0, nudge_y = 1000,
                  show.legend = FALSE)
ggsave('../figures/fig2c.pdf', height = 3, width=4.5)

# Fig 3: Racial disparities
## Fig 3a: Auth rates
ggplot(race_performance %>% filter(name %in% c('Authorization Rate'), Year > 2014, Year < 2021, 
                                   OPO %in% c('OPO1', 'OPO4', 'OPO5'),
                                   Race %in% c('White / Caucasian') | 
                                     (Race=='Black / African American' & OPO %in% c('OPO1', 'OPO2', 'OPO5')) |
                                     (Race=='Hispanic' & OPO %in% c('OPO1', 'OPO4'))), 
       aes(col=Race, shape=Race, y=value, x=Year)) + facet_wrap(.~OPO,ncol=3) + 
  geom_point(size=3) + geom_line(lty=3) +
  theme_classic() + 
  scale_color_manual(values = c("#D55E00", "#009E73", "#0072B2")) + 
  theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size=14)) + 
  ylab("Authorization Rate") + scale_y_continuous(breaks=seq(0,1,0.1), limits = c(0, 1)) 
ggsave('../figures/fig3a.pdf', height = 4, width=8)

## Fig 3b: Approach times

data$bd_to_approach <- (data$time_approached - data$time_brain_death)/(60*60)
data$ref_to_approach <- (data$time_referred - data$time_referred)/(60*60)
ggplot(data %>% filter(OPO=="OPO1", 
                       brain_death, 
                       approached,
                       !is.na(bd_to_approach), 
                       Race !="Other / Unknown"), aes(x=bd_to_approach, y=Race, fill=Race)) + 
  scale_fill_manual(values = c("#D55E00", "#009E73", "#0072B2")) + 
  geom_boxplot() + 
  xlim(-5, 8) + 
  theme_classic() + ylab("") + 
  theme(legend.title = element_blank(), text = element_text(size=14), 
        legend.position = "none") + 
  xlab("Time between Brain Death and Approach (hours)")
ggsave('../figures/fig3b.pdf', height = 4, width=8)

# Table 1: demographics

render.categorical <- function(x, ...) {
  c("", sapply(stats.apply.rounding(stats.default(x)), function(y) with(y,
                                                                        sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT))))
}
data_table <- data %>% select(OPO, Age, Gender, Race, brain_death, 
                              Cause_of_Death_UNOS, Mechanism_of_Death, Circumstances_of_Death) %>% 
  mutate(Gender=factor(Gender, levels = c("F", "M", ""), labels=c("Female", "Male", "Unknown")),
         Age = cut(Age, breaks=c(0,24,44,64,75, 101), include.lowest = FALSE, 
                   labels=c("Under 25", "25-44", "45-64", "65-75", "Over 75")),
         Race=factor(Race, levels=c("White / Caucasian", "Black / African American","Hispanic","Other / Unknown")), 
         OPO = factor(OPO),
         brain_death = factor(brain_death, levels=c(FALSE, TRUE), label=c("Brain death", "Cardiac death")), 
         Cause_of_Death_UNOS = factor(ifelse(Cause_of_Death_UNOS %in% c("Anoxia", "", "CVA/Stroke", "Head Trauma"), 
                                             Cause_of_Death_UNOS, "Other"), 
                                      levels = c("Anoxia", "CVA/Stroke", "Head Trauma", "Other", ""), 
                                      labels = c("Anoxia", "Cerebrovascular accident/Stroke", "Head trauma", "Other", "Unknown"))) %>% 
  rename(`Death Type` = brain_death, 
         `Cause of Death` = Cause_of_Death_UNOS)

## HTML table
table1(~ Age + Gender + Race + `Death Type` + `Cause of Death` | OPO, data=data_table,render.categorical=render.categorical)
## Latex Table
t1kable(table1(~ Age + Gender + Race + `Death Type` + `Cause of Death` | OPO, data=data_table,render.categorical=render.categorical),
              format = 'latex')
    




