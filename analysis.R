## Bodo Winter
## Created January 20, 2016
## Adapted to new files and incorporating E3, Dec 28, 2016

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Set options:

options(stringsAsFactors = F)

## Load in packages:

library(dplyr)
library(lme4)
library(stringr)

## Load in data:

setwd('/Users/winterb/Research/politeness/korean_perception_study_new_full_package/analysis/')
f0 <- read.csv('E1_f0_konkuk.csv')
int <- read.csv('E2_int_konkuk.csv')
both <- read.csv('E3_both_konkuk.csv')
f0.UO <- read.csv('E1B_f0_UO.csv')
both.UO <- read.csv('E3B_both_UO.csv')

## Define a vector with all objects:

objs <- c('f0', 'int', 'both', 'f0.UO', 'both.UO')

## Define columns to exclude:

bad_columns <- c('ExperimentName', 'Group', 'Session', 'Clock.Information', 'DataFile.Basename',
	'Display.RefreshRate', 'ExperimentVersion', 'RandomSeed', 'RuntimeVersion',
	'RuntimeVersionExpected', 'SessionDate', 'SessionStartDateTimeUtc',
	'SessionTime', 'StudioVersion', 'Block', 'List1', 'List1.Cycle',
	'List1.Sample', 'Procedure.Block.', 'Running.Block.', 'Trial',
	'Condition', 'Con.Resp.Trial.', 'PracStim', 'PracStim.Cycle',
	'PracStim.Sample', 'Procedure.Trial.', 'RespK.ACC.Trial.',
	'RespK.CRESP.Trial.', 'RespK.DurationError.Trial.', 'RespK.OnsetDelay.Trial.',
	'RespK.OnsetTime.Trial.', 'RespK.OnsetToOnsetTime.Trial.', 'RespK.RESP.Trial.',
	'RespK.RT.Trial.', 'RespK.RTTime.Trial.', 'Running.Trial.', 'Scenario.Trial.',
	'SoundFile.Trial.', 'Speaker.Trial.', 'SpeakerBlocks',
	'SpeakerBlocks.Cycle', 'SpeakerBlocks.Sample', 'SubTrial', 'ConResp.SubTrial.',
	'Procedure.SubTrial.', 'RespK.CRESP.SubTrial.', 'RespK.DurationError.SubTrial.',
	'RespK.OnsetDelay.SubTrial.', 'RespK.OnsetTime.SubTrial.', 'RespK.OnsetToOnsetTime.SubTrial.',
	'RespK.RESP.SubTrial.', 'RespK.RTTime.SubTrial', 'Running.SubTrial.',
	'Speaker.SubTrial.', 'StimList', 'StimList.Cycle', 'StimNum', 'ConResp.Trial.',
	'RuntimeCapabilities', 'SessionTimeUtc', 'RespK.RTTime.SubTrial.')

## Loop through objects and exclude columns:

for (i in 1:length(objs)) {
	this_obj <- get(objs[i])
	col_pos <- colnames(this_obj) %in% bad_columns
	this_obj <- this_obj[, !col_pos]
	assign(objs[i], this_obj)
	}

## Loop through objects and rename columns:

for (i in 1:length(objs)) {
	this_obj <- get(objs[i])
	this_obj <- rename(this_obj,
		Listener = Subject,
		ListenerSex = Sex,
		Resp = RespK.ACC.SubTrial.,
		RT = RespK.RT.SubTrial.,
		Trial = StimList.Sample,
		Speaker = SpeakerNum,
		Item = Scenario.SubTrial.,
		File = SoundFile.SubTrial.)
		
	if (str_count(objs[i], 'UO|both')) {		# only applies to 'both' ('gender' -> 'sex')
		this_obj <- rename(this_obj,
			SpeakerSex = SpeakerGender)
		}
	assign(objs[i], this_obj)
	}

## Match speaker sex (information is in 'f0.UO' dataframe):

f0$SpeakerSex <- f0.UO[match(f0$Speaker, f0.UO$Speaker), ]$SpeakerSex
int$SpeakerSex <- f0.UO[match(int$Speaker, f0.UO$Speaker), ]$SpeakerSex
f0.UO$SpeakerSex <- f0.UO[match(f0.UO$Speaker, f0.UO$Speaker), ]$SpeakerSex

## Transform variables:

for (i in 1:length(objs)) {
	this_obj <- get(objs[i])
	this_obj <- mutate(this_obj,	# log-transform and center RTs, center trials
		LogRT = log(RT + 1),
		LogRT_c = LogRT - mean(LogRT),
		Trial_c = Trial - mean(Trial))
	this_obj <- mutate(this_obj,
		ListenerSex_c = ifelse(ListenerSex == 'female', -0.5, 0.5),
		SpeakerSex_c = ifelse(SpeakerSex == 'F', -0.5, 0.5))
	if (str_count(objs[i], 'f0|both')) {		# center f0
		this_obj <- mutate(this_obj,
			F0_c = F0LevelNum - mean(F0LevelNum),
			F0_c2 = F0_c ^ 2)
		}
	if (str_count(objs[i], 'int|both')) {	# center intensity
		this_obj <- mutate(this_obj,
			int_c = IntLevelNum - mean(IntLevelNum),
			int_c2 = int_c ^ 2)
		}
	assign(objs[i], this_obj)
	}



##------------------------------------------------------------------
## Merge production data into it:
##------------------------------------------------------------------

## Load in raw acoustics:

prod <- read.csv('stimuli_acoustics.csv')

## Define vector with variables to extract and rename:

relevant_variables <- c('experiment', 'filename', 'dur', 'f0mnhz', 'f0sdhz', 'inmn', 'inrange',
	'jitloc', 'shimloc', 'h1mh2mn', 'mnHNR')
prod <- prod[, colnames(prod) %in% relevant_variables]
prod <- rename(prod,
	f0mn = f0mnhz,
	f0sd = f0sdhz,
	jitter = jitloc,
	shimmer = shimloc,
	H1H2 = h1mh2mn,
	HNR = mnHNR)

## Separate datasets:

prod.f0 <- filter(prod, experiment == 'F0')
prod.int <- filter(prod, experiment == 'Intensity')
prod.both <- filter(prod, experiment == 'F0-Intensity')

## Match into 'f0' datasets:

prod.f0 <- mutate(prod.f0,
	filename = str_replace(filename, 'eq_', ''),
	filename = paste0(filename, '.wav'))
f0 <- cbind(f0,
	select(prod.f0[match(f0$File, prod.f0$filename), ], dur:HNR))
f0.UO <- cbind(f0.UO,
	select(prod.f0[match(f0.UO$File, prod.f0$filename), ], dur:HNR))

## Match into 'intensity' datasets:

prod.int <- mutate(prod.int,
	filename = str_replace(filename, 'F0eq_', ''),
	filename = paste0(filename, '.wav'))
int <- mutate(int,
	File = str_replace(File, 'F0eq_', ''))
int <- cbind(int,
	select(prod.int[match(int$File, prod.int$filename), ], dur:HNR))

## Match into 'both' datasets:

prod.both <- mutate(prod.both,
	filename = paste0(filename, '.wav'))
both <- cbind(both,
	select(prod.both[match(both$File, prod.both$filename), ], dur:HNR))
both.UO <- cbind(both,
	select(prod.both[match(both$File, prod.both$filename), ], dur:HNR))

## Z-score acoustic variables within speaker:

zscore <- function(x) (x - mean(x)) / sd(x)
vars <- c('f0mn', 'inmn', 'f0sd', 'inrange',
	'jitter', 'shimmer', 'HNR', 'H1H2', 'dur')
for (i in 1:length(objs)) {
	this_df <- get(objs[i])
	for (j in 1:length(vars)) {
		this_var <- vars[j]
		this_var_z <- paste0(this_var, '_z')
		df <- data.frame(x = rep(NA, nrow(this_df)))
		colnames(df) <- this_var_z
		this_df <- cbind(this_df, df)
		speakers <- unique(this_df$Speaker)
		for (k in 1:length(unique(this_df$Speaker))) {
			speaker_ids <- this_df$Speaker == speakers[k]
			this_df[speaker_ids, this_var_z] <- zscore(this_df[speaker_ids, this_var])
			}
		}
	assign(objs[i], this_df)
	}

## Write objects with z-scores to data frames:

write.table(f0, 'f0_z.csv', sep = ',', row.names = F)
write.table(int, 'int_z.csv', sep = ',', row.names = F)
write.table(both, 'both_z.csv', sep = ',', row.names = F)



##------------------------------------------------------------------
## Experiment 1, f0 analysis:
##------------------------------------------------------------------

## Full model with quadratic predictors:

summary(f0.quad <- glmer(Resp ~ F0_c + F0_c2 +
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0 + F0_c|Listener) + (0 + F0_c|Speaker) +
	(0 + F0_c2|Listener) + (0 + F0_c2|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Model with only linear pitch predictor:

summary(f0.lin <- glmer(Resp ~ F0_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0 + F0_c|Listener) + (0 + F0_c|Speaker) +
	(0 + F0_c2|Listener) + (0 + F0_c2|Speaker),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test for quadratic effect:

anova(f0.lin, f0.quad, test = 'Chisq')	# n.s. (p = 0.06), drop quadratic effects

## New pitch model without quadratic random effects:
## Also, add interaction effects for a potential gender interaction of the manipulation:

summary(f0.x3 <- glmer(Resp ~ F0_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:F0_c + SpeakerSex_c:F0_c +
	ListenerSex_c:SpeakerSex_c:F0_c + 
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Model without the three-way interaction:

summary(f0.no3 <- glmer(Resp ~ F0_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:F0_c + SpeakerSex_c:F0_c +
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test for three-way interaction:

anova(f0.no3, f0.x3, test = 'Chisq')	# n.s. (p = 0.07), drop three-way interaction

## Model without the two two-way condition interactions:

summary(f0.no2A <- glmer(Resp ~ F0_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:F0_c + 		# omit SpeakerSex_c:F0_c
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(f0.no2B <- glmer(Resp ~ F0_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	SpeakerSex_c:F0_c +			# omit ListenerSex_c:F0_c
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Tests for two-way interactions:

anova(f0.no2A, f0.no3, test = 'Chisq')
anova(f0.no2B, f0.no3, test = 'Chisq')

## Main model with trial (causes convergence issues):

summary(f0.withtrial <- glmer(Resp ~ F0_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Main model without trial:

summary(f0.mdl <- glmer(Resp ~ F0_c + 
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test trial effect:

anova(f0.mdl, f0.withtrial, test = 'Chisq')

## Reduced models:

summary(f0.noint <- glmer(Resp ~ F0_c + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + 
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(f0.nolist <- glmer(Resp ~ F0_c + 
	LogRT_c +
	SpeakerSex_c + 
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(f0.nospeak <- glmer(Resp ~ F0_c + 
	LogRT_c +
	ListenerSex_c + 
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test sex-variables:

anova(f0.noint, f0.mdl, test = 'Chisq')			# sig. interaction
anova(f0.nospeak, f0.noint, test = 'Chisq')		# p = 0.05
anova(f0.nolist, f0.noint, test = 'Chisq')

## Reduced model without F0:

summary(f0.null <- glmer(Resp ~ 1 +
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test of f0 effect:

anova(f0.null, f0.mdl, test = 'Chisq')

## Test of RT effect:

summary(f0.nort <- glmer(Resp ~ F0_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(f0.nort, f0.mdl, test = 'Chisq')

## Test RT interaction (motivated by Brown et al., 2014 results):

summary(f0.rtinteract <- glmer(Resp ~ F0_c + 
	LogRT_c +
	F0_c:LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(f0.mdl, f0.rtinteract, test = 'Chisq')

## SANITY CHECK: Main model, categorical:

summary(f0.cat <- glmer(Resp ~ F0Level + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + F0Level|Listener) + (1 + F0Level|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(f0.cat.null <- glmer(Resp ~ 1 + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + F0Level|Listener) + (1 + F0Level|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(f0.cat.null, f0.cat, test = 'Chisq')

## Look at model without slopes:

summary(f0.nolistenerslope <- glmer(Resp ~ F0_c + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(f0.nospeakerslope <- glmer(Resp ~ F0_c + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + F0_c|Listener) + (1|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Compare:

anova(f0.nolistenerslope, f0.mdl, test = 'Chisq')
anova(f0.nospeakerslope, f0.mdl, test = 'Chisq')

## Look at listener coefficients:

coef(f0.mdl)$Listener[ , 'F0_c']
sum(coef(f0.mdl)$Listener[ , 'F0_c'] > 0)
sum(coef(f0.mdl)$Listener[ , 'F0_c'] < 0)

## Save models:

save.image('models_so_far.RData')



##------------------------------------------------------------------
## Experiment 2, intensity analysis:
##------------------------------------------------------------------

## Full model with quadratic predictors:

summary(int.quad <- glmer(Resp ~ int_c + int_c2 + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0 + int_c|Listener) + (0 + int_c|Speaker) +
	(0 + int_c2|Listener) + (0 + int_c2|Speaker),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Model with only linear pitch predictor:

summary(int.lin <- glmer(Resp ~ int_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1|Listener) + (1|Speaker) + (1|Item) +
	(0 + int_c|Listener) + (0 + int_c|Speaker) +
	(0 + int_c2|Listener) + (0 + int_c2|Speaker),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test for quadratic effect:

anova(int.lin, int.quad, test = 'Chisq')	# n.s., drop quadratic effects

## New pitch model without quadratic random effects:
## Also, add interaction effects for a potential gender interaction of the manipulation:

summary(int.x3 <- glmer(Resp ~ int_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + SpeakerSex_c:int_c +
	ListenerSex_c:SpeakerSex_c:int_c + 
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Model without the three-way interaction:

summary(int.no3 <- glmer(Resp ~ int_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + SpeakerSex_c:int_c +
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test for three-way interaction:

anova(int.no3, int.x3, test = 'Chisq')	# n.s, drop three-way interaction

## Model without the two two-way condition interactions:

summary(int.no2A <- glmer(Resp ~ int_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 		# omit SpeakerSex_c:int_c
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(int.no2B <- glmer(Resp ~ int_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	SpeakerSex_c:int_c +			# omit ListenerSex_c:int_c
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Tests for two-way interactions:

anova(int.no2A, int.no3, test = 'Chisq')
anova(int.no2B, int.no3, test = 'Chisq')

## Main model with trial:

summary(int.withtrial <- glmer(Resp ~ int_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Main model without trial:

summary(int.mdl <- glmer(Resp ~ int_c + 
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test trial effect:

anova(int.mdl, int.withtrial, test = 'Chisq')

## Reduced models:

summary(int.noint <- glmer(Resp ~ int_c + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + 
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(int.nolist <- glmer(Resp ~ int_c + 
	LogRT_c +
	SpeakerSex_c + 
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(int.nospeak <- glmer(Resp ~ int_c + 
	LogRT_c +
	ListenerSex_c + 
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test sex-variables:

anova(int.noint, int.mdl, test = 'Chisq')			# sig. interaction
anova(int.nospeak, int.noint, test = 'Chisq')		# p = 0.01
anova(int.nolist, int.noint, test = 'Chisq')

## Reduced model without int:

summary(int.null <- glmer(Resp ~ 1 +
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test of intensity effect:

anova(int.null, int.mdl, test = 'Chisq')

## Test of RT effect:

summary(int.nort <- glmer(Resp ~ int_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(int.nort, int.mdl, test = 'Chisq')

## Test RT interaction (motivated by Brown et al. 2014 results):

summary(int.rtinteract <- glmer(Resp ~ int_c + 
	LogRT_c +
	int_c:LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + int_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(int.mdl, int.rtinteract, test = 'Chisq')

## SANITY CHECK: Main model, categorical:

summary(int.cat <- glmer(Resp ~ IntLevel + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + IntLevel|Listener) + (1 + IntLevel|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(int.cat.null <- glmer(Resp ~ 1 + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + IntLevel|Listener) + (1 + IntLevel|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(int.cat.null, int.cat, test = 'Chisq')

## Look at model without listener slope (individual differences):

summary(int.nolistenerslope <- glmer(Resp ~ int_c + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(int.nospeakerslope <- glmer(Resp ~ int_c + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + int_c|Listener) + (1|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Compare:

anova(int.nolistenerslope, int.mdl, test = 'Chisq')
anova(int.nospeakerslope, int.mdl, test = 'Chisq')

## Look at listener coefficients:

coef(int.mdl)$Listener[ , 'int_c']
sum(coef(int.mdl)$Listener[ , 'int_c'] > 0)
sum(coef(int.mdl)$Listener[ , 'int_c'] < 0)

## Save models:

save.image('models_so_far.RData')



##------------------------------------------------------------------
## Experiment 3, intensity-f0 analysis:
##------------------------------------------------------------------

## Full model with all interactions:

summary(both.massive <- glmer(Resp ~ int_c + F0_c + int_c:F0_c +
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:F0_c + SpeakerSex_c:F0_c + ListenerSex_c:SpeakerSex_c:F0_c + 
	ListenerSex_c:int_c + SpeakerSex_c:int_c + ListenerSex_c:SpeakerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Full model with no three-way interactions:

summary(both.no3A <- glmer(Resp ~ int_c + F0_c + int_c:F0_c +
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:F0_c + SpeakerSex_c:F0_c + 
	ListenerSex_c:int_c + SpeakerSex_c:int_c + ListenerSex_c:SpeakerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.no3B <- glmer(Resp ~ int_c + F0_c + int_c:F0_c +
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:F0_c + SpeakerSex_c:F0_c + ListenerSex_c:SpeakerSex_c:F0_c + 
	ListenerSex_c:int_c + SpeakerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Tests of three-way interactions:

anova(both.no3A, both.massive, test = 'Chisq')		# p = 0.0567 (n.s.)
anova(both.no3B, both.massive, test = 'Chisq')

## Model without any three-way interactions:

summary(both.no3 <- glmer(Resp ~ int_c + F0_c + int_c:F0_c +
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:F0_c + SpeakerSex_c:F0_c +
	ListenerSex_c:int_c + SpeakerSex_c:int_c +
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Models without two-way condition interactions:

summary(both.nospeakf0 <- glmer(Resp ~ int_c + F0_c + int_c:F0_c +
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:F0_c + 
	ListenerSex_c:int_c + SpeakerSex_c:int_c +
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.nolistf0 <- glmer(Resp ~ int_c + F0_c + int_c:F0_c +
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	SpeakerSex_c:F0_c +
	ListenerSex_c:int_c + SpeakerSex_c:int_c +
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.nospeakint <- glmer(Resp ~ int_c + F0_c + int_c:F0_c +
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:F0_c + SpeakerSex_c:F0_c +
	ListenerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.nolistint <- glmer(Resp ~ int_c + F0_c + int_c:F0_c +
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:F0_c + SpeakerSex_c:F0_c +
	SpeakerSex_c:int_c +
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Tests of two-way condition interactions:

anova(both.nospeakf0, both.no3, test = 'Chisq')
anova(both.nolistf0, both.no3, test = 'Chisq')
anova(both.nospeakint, both.no3, test = 'Chisq')
anova(both.nolistint, both.no3, test = 'Chisq')		# sig.

## Interaction model:

summary(both.mdl.with.int <- glmer(Resp ~ int_c + F0_c + int_c:F0_c +
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Main model with trial variable (causes convergence issues):

summary(both.withtrial <- glmer(Resp ~ int_c + F0_c + 
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test intensity / f0 interaction:

anova(both.withtrial, both.mdl.with.int, test = 'Chisq')	# n.s.

## Main model without trial variable:

summary(both.mdl <- glmer(Resp ~ int_c + F0_c + 
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test trial effect:

anova(both.mdl, both.withtrial, test = 'Chisq')

## Model without speaker / listener sex interaction:

summary(both.no2sex <- glmer(Resp ~ int_c + F0_c +
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test spaker / listener sex interaction:

anova(both.no2sex, both.mdl, test = 'Chisq')

## Model without listener sex - intensity interaction:

summary(both.nosexint <- glmer(Resp ~ int_c + F0_c + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Test of listener sex / intensity interaction:

anova(both.nosexint, both.no2sex, test = 'Chisq')

## Models without listener and speaker effects:

summary(both.nolist <- glmer(Resp ~ int_c + F0_c + 
	LogRT_c +
	SpeakerSex_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.nospeak <- glmer(Resp ~ int_c + F0_c + 
	LogRT_c +
	ListenerSex_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Tests of listener and speaker effects:

anova(both.nolist, both.nosexint, test = 'Chisq')
anova(both.nospeak, both.nosexint, test = 'Chisq')

## Models without intensity / f0 effects:

summary(both.noint <- glmer(Resp ~ F0_c +
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.nof0 <- glmer(Resp ~ int_c +
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Tests of intensity / f0 effects:

anova(both.nof0, both.mdl, test = 'Chisq')		# p = 0.09
anova(both.noint, both.mdl, test = 'Chisq')	# p < 0.001

## Models without RT:

summary(both.nort <- glmer(Resp ~ int_c + F0_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(both.nort, both.mdl, test = 'Chisq')

## SANITY CHECK, main model categorical:

both <- mutate(both,
	F0Level = as.factor(F0Level),
	IntLevel = as.factor(IntLevel))
contrasts(both$F0Level) <- contr.sum(3)
contrasts(both$IntLevel) <- contr.sum(3)
summary(both.mdl.cat <- glmer(Resp ~ F0Level + IntLevel + F0Level:IntLevel +
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + IntLevel + F0Level|Listener) + (1 + IntLevel + F0Level|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.mdl.cat.no2 <- glmer(Resp ~ F0Level + IntLevel + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + IntLevel + F0Level|Listener) + (1 + IntLevel + F0Level|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.mdl.cat.noint <- glmer(Resp ~ F0Level + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + IntLevel + F0Level|Listener) + (1 + IntLevel + F0Level|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.mdl.cat.nof0 <- glmer(Resp ~ IntLevel + 
	LogRT_c +
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + IntLevel + F0Level|Listener) + (1 + IntLevel + F0Level|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Tests of categorical effects:

anova(both.mdl.cat.nof0, both.mdl.cat.no2, test = 'Chisq')	# test of f0
anova(both.mdl.cat.noint, both.mdl.cat.no2, test = 'Chisq')	# test of int
anova(both.mdl.cat.no2, both.mdl.cat, test = 'Chisq')	# test of interaction

## Prediction of intensity / listener gender interaction:

xpred <- data.frame(ListenerSex_c = rep(c(-0.5, 0.5), each = 3),
	int_c = rep(c(-1, 0, 1), 2), F0_c = 0, LogRT_c = 0, Trial_c = 0,
	SpeakerSex_c = 0)
xpred$Resp <- predict(both.mdl, newdata = xpred, re.form = NA)
xpred$Resp <- plogis(xpred$Resp)

## Look at model without random slopes for f0 (individual differences) :

summary(both.nof0speakerslope <- glmer(Resp ~ int_c + F0_c +
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.nof0listenerslope <- glmer(Resp ~ int_c + F0_c +
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + int_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Look at model without random slopes for int (individual differences) :

summary(both.nointspeakerslope <- glmer(Resp ~ int_c + F0_c +
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.nointlistenerslope <- glmer(Resp ~ int_c + F0_c +
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

summary(both.nointslope <- glmer(Resp ~ int_c + F0_c +
	LogRT_c + Trial_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + int_c + F0_c|Listener) + (1 + int_c + F0_c|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Look at listener coefficients:

coef(both.mdl)$Listener[ , c('F0_c', 'int_c')]
sum(coef(both.mdl)$Listener[ , 'F0_c'] > 0)
sum(coef(both.mdl)$Listener[ , 'F0_c'] < 0)
sum(coef(both.mdl)$Listener[ , 'int_c'] > 0)
sum(coef(both.mdl)$Listener[ , 'int_c'] < 0)

## Save models:

save.image('models_so_far.RData')



##------------------------------------------------------------------
## Descriptive tables:
##------------------------------------------------------------------

## Tables for descriptive values:

f0 %>% group_by(F0LevelNum) %>%
	summarise(M = round(mean(Resp), 2), SD = round(sd(Resp), 2))
int %>% group_by(IntLevelNum) %>%
	summarise(M = round(mean(Resp), 2), SD = round(sd(Resp), 2))
both %>% group_by(F0Level) %>%
	summarise(M = round(mean(Resp), 2), SD = round(sd(Resp), 2))
both %>% group_by(IntLevel) %>%
	summarise(M = round(mean(Resp), 2), SD = round(sd(Resp), 2))




##------------------------------------------------------------------
## Plot predictions of models:
##------------------------------------------------------------------

## Create dataframe for getting predictions:

f0.pred <- data.frame(F0_c = c(-2, -1, 0, 1, 2),
	ListenerSex_c = 0, SpeakerSex_c = 0, LogRT_c = 0, Resp = 0)
int.pred <- data.frame(int_c = c(-2, -1, 0, 1, 2),
	ListenerSex_c = 0, SpeakerSex_c = 0, LogRT_c = 0, Resp = 0)
both.pred.int <- data.frame(int_c = c(-2, -1, 0, 1, 2),
	F0_c = rep(0, 5), ListenerSex_c = 0, SpeakerSex_c = 0, LogRT_c = 0, Resp = 0)
both.pred.f0 <- data.frame(F0_c = c(-2, -1, 0, 1, 2),
	int_c = rep(0, 5), ListenerSex_c = 0, SpeakerSex_c = 0, LogRT_c = 0, Resp = 0)

## Extract model matrices for standard errors:

f0.mm <- model.matrix(terms(f0.mdl), f0.pred)
int.mm <- model.matrix(terms(int.mdl), int.pred)
both.mm.f0 <- model.matrix(terms(both.mdl), both.pred.f0)
both.mm.int <- model.matrix(terms(both.mdl), both.pred.int)

## Extract fitted values:

f0.pred$Resp <- predict(f0.mdl,
	newdata = f0.pred, re.form = NA)
int.pred$Resp <- predict(int.mdl,
	newdata = int.pred, re.form = NA)
both.pred.int$Resp <- predict(both.mdl,
	newdata = both.pred.int, re.form = NA)
both.pred.f0$Resp <- predict(both.mdl,
	newdata = both.pred.f0, re.form = NA)

## Extract variance component:

f0.var <- diag(f0.mm %*% tcrossprod(vcov(f0.mdl), f0.mm))
int.var <- diag(int.mm %*% tcrossprod(vcov(int.mdl), int.mm))
both.f0.var <- diag(both.mm.f0 %*% tcrossprod(vcov(both.mdl), both.mm.f0))
both.int.var <- diag(both.mm.int %*% tcrossprod(vcov(both.mdl), both.mm.int))

## Add lower and upper bound confidence intervals to pred.data:

f0.pred$UB <- f0.pred$Resp + 1.96 * sqrt(f0.var)
f0.pred$LB <- f0.pred$Resp - 1.96 * sqrt(f0.var)
int.pred$UB <- int.pred$Resp + 1.96 * sqrt(int.var)
int.pred$LB <- int.pred$Resp - 1.96 * sqrt(int.var)
both.pred.int$UB <- both.pred.int$Resp + 1.96 * sqrt(both.int.var)
both.pred.int$LB <- both.pred.int$Resp - 1.96 * sqrt(both.int.var)
both.pred.f0$UB <- both.pred.f0$Resp + 1.96 * sqrt(both.f0.var)
both.pred.f0$LB <- both.pred.f0$Resp - 1.96 * sqrt(both.f0.var)

## Back-transform everything to probabilities for plotting:

f0.pred <- mutate(f0.pred,
	Resp = plogis(Resp), UB = plogis(UB), LB = plogis(LB))
int.pred <- mutate(int.pred,
	Resp = plogis(Resp), UB = plogis(UB), LB = plogis(LB))
both.pred.int <- mutate(both.pred.int,
	Resp = plogis(Resp), UB = plogis(UB), LB = plogis(LB))		
both.pred.f0 <- mutate(both.pred.f0,
	Resp = plogis(Resp), UB = plogis(UB), LB = plogis(LB))
	
## Create dataframe for getting predictions of gender interaction effect:

gender.pred <- data.frame(int_c = 0,
	F0_c = 0,
	ListenerSex_c = c(-0.5, -0.5, 0.5, 0.5),
	SpeakerSex_c = c(-0.5, 0.5, -0.5, 0.5),
	LogRT_c = 0, Resp = 0)

## Loop through to create predictions:

objs <- c('f0', 'int', 'both')
for (i in 1:3) {
	gender.pred$Resp <- 0
	this_mdl <- get(paste0(objs[i], '.mdl'))
	mm <- model.matrix(terms(this_mdl), gender.pred)
	gender.pred$Resp <- predict(this_mdl,
		newdata = gender.pred, re.form = NA)
	myvar <- diag(mm %*% tcrossprod(vcov(this_mdl), mm))
	gender.pred$UB <- gender.pred$Resp + 1.96 * sqrt(myvar)
	gender.pred$LB <- gender.pred$Resp - 1.96 * sqrt(myvar)
	gender.pred <- mutate(gender.pred,
		Resp = plogis(Resp), UB = plogis(UB), LB = plogis(LB))
	assign(paste('gender.pred', objs[i], sep = '.'), gender.pred)
	}

## X-axis factor for all plots:

xfactor <- 0.1	# for second plot

## Plot predictions for Experiment 1 (f0):

quartz('', 11.5, 5.5)
par(mfrow = c(1, 2), mai = c(0, 0, 0, 0.25), omi = c(1.25, 1.75, 1, 0.5))
# PLot 1:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0.5, 5.5), ylim = c(0.2, 0.8))
axis(side = 2, at = seq(0.2, 0.8, 0.2),
	labels = paste0(seq(20, 80, 20), '%'),
	las = 2, font = 2,
	lwd.ticks = 2, cex.axis = 1.25)
axis(side = 1, at = 1:5,
	labels = c('-16%', '-8%', '0%', '+8%', '+16%'),
	font = 2,
	lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Pitch level', side = 1, line = 3, cex = 1.5, font = 2)
mtext(text = '% Polite', side = 2, font = 2, line = 4.65, cex = 1.75)
mtext(text = 'Pitch effect', side = 3, font = 2, line = 1.25, cex = 2)
text(x = 0.6125, y = 0.77, labels = '(a)', font = 2, cex = 1.5)
# Actual data:
points(1:5, f0.pred$Resp, type = 'b', pch = 19, lwd = 2)
arrows(x0 = 1:5, x1 = 1:5,
	y0 = f0.pred$LB, y1 = f0.pred$UB, lwd = 2,
	code = 3, angle = 90, length = 0.1)
box(lwd = 2)
# Plot 2:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0, 4), ylim = c(0.2, 0.8))
mtext(text = 'Gender interaction', side = 3, font = 2, line = 1.25, cex = 1.85)
axis(side = 1, at = c(1, 3),
	font = 2, lwd.ticks = 2, labels = c('', ''))
axis(side = 1, at = c(1, 3),
	labels = c('female\nlisteners', 'male\nlisteners'),
	font = 2, line = 1.5, tick = F, cex.axis = 1.5)
legend('topright', pch = 16:17, lty = 1:2, lwd = 2,
	legend = c('female speakers', 'male speakers'))
text(x = 0.1, y = 0.77, labels = '(b)', font = 2, cex = 1.5)
# Actual data:
points(x = c(1, 3) - xfactor, y = gender.pred.f0[c(1, 3),]$Resp, type = 'b',
	pch = 16, cex = 1.5,
	lwd = 2)
points(x = c(1, 3) + xfactor, y = gender.pred.f0[c(2, 4),]$Resp, type = 'b',
	pch = 17, cex = 1.5,
	lwd = 2, lty = 2)
arrows(x0 = c(1, 3) - xfactor,
	x1 = c(1,3 ) - xfactor,
	y0 = gender.pred.f0[c(1, 3),]$LB,
	y1 = gender.pred.f0[c(1, 3),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
arrows(x0 = c(1, 3) + xfactor,
	x1 = c(1,3 ) + xfactor,
	y0 = gender.pred.f0[c(2, 4),]$LB,
	y1 = gender.pred.f0[c(2, 4),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
box(lwd = 2)

## Plot predictions for Experiment 2 (intensity):

quartz('', 11.5, 5.5)
par(mfrow = c(1, 2), mai = c(0, 0, 0, 0.25), omi = c(1.25, 1.75, 1, 0.5))
# PLot 1:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0.5, 5.5), ylim = c(0.2, 0.8))
axis(side = 2, at = seq(0.2, 0.8, 0.2),
	labels = paste0(seq(20, 80, 20), '%'),
	las = 2, font = 2,
	lwd.ticks = 2, cex.axis = 1.25)
axis(side = 1, at = 1:5,
	labels = c('-10%', '-7.5%', '-5%', '-2.5%', '0%'),
	font = 2,
	lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Intensity level', side = 1, line = 3, cex = 1.5, font = 2)
mtext(text = '% Polite', side = 2, font = 2, line = 4.65, cex = 1.75)
mtext(text = 'Intensity effect', side = 3, font = 2, line = 1.25, cex = 2)
text(x = 0.6125, y = 0.77, labels = '(a)', font = 2, cex = 1.5)
# Actual data:
points(1:5, int.pred$Resp, type = 'b', pch = 19, lwd = 2)
arrows(x0 = 1:5, x1 = 1:5,
	y0 = int.pred$LB, y1 = int.pred$UB, lwd = 2,
	code = 3, angle = 90, length = 0.1)
box(lwd = 2)
# Plot 2:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0, 4), ylim = c(0.2, 0.8))
mtext(text = 'Gender interaction', side = 3, font = 2, line = 1.25, cex = 1.85)
axis(side = 1, at = c(1, 3),
	font = 2, lwd.ticks = 2, labels = c('', ''))
axis(side = 1, at = c(1, 3),
	labels = c('female\nlisteners', 'male\nlisteners'),
	font = 2, line = 1.5, tick = F, cex.axis = 1.5)
legend('topright', pch = 16:17, lty = 1:2, lwd = 2,
	legend = c('female speakers', 'male speakers'))
text(x = 0.1, y = 0.77, labels = '(b)', font = 2, cex = 1.5)
# Actual data:
points(x = c(1, 3) - xfactor, y = gender.pred.int[c(1, 3),]$Resp, type = 'b',
	pch = 16, cex = 1.5,
	lwd = 2)
points(x = c(1, 3) + xfactor, y = gender.pred.int[c(2, 4),]$Resp, type = 'b',
	pch = 17, cex = 1.5,
	lwd = 2, lty = 2)
arrows(x0 = c(1, 3) - xfactor,
	x1 = c(1,3 ) - xfactor,
	y0 = gender.pred.int[c(1, 3),]$LB,
	y1 = gender.pred.int[c(1, 3),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
arrows(x0 = c(1, 3) + xfactor,
	x1 = c(1,3 ) + xfactor,
	y0 = gender.pred.int[c(2, 4),]$LB,
	y1 = gender.pred.int[c(2, 4),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
box(lwd = 2)

## Plot predictions for Experiment 3 (intensity & f0), pitch and intensity effect:

quartz('', 11.5, 5.5)
par(mfrow = c(1, 2), mai = c(0, 0, 0, 0.25), omi = c(1.25, 1.75, 1, 0.5))
# PLot 1:
# PLot 1:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0.5, 5.5), ylim = c(0.1, 0.8))
axis(side = 2, at = seq(0.2, 0.8, 0.2),
	labels = paste0(seq(20, 80, 20), '%'),
	las = 2, font = 2,
	lwd.ticks = 2, cex.axis = 1.25)
axis(side = 1, at = 1:5,
	labels = c('-16%', '-8%', '0%', '+8%', '+16%'),
	font = 2,
	lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Pitch level', side = 1, line = 3, cex = 1.5, font = 2)
mtext(text = '% Polite', side = 2, font = 2, line = 4.65, cex = 1.75)
mtext(text = 'Pitch effect', side = 3, font = 2, line = 1.25, cex = 2)
text(x = 0.6125, y = 0.77, labels = '(a)', font = 2, cex = 1.5)
# Actual data:
points(1:5, both.pred.f0$Resp, type = 'b', pch = 19, lwd = 2)
arrows(x0 = 1:5, x1 = 1:5,
	y0 = both.pred.f0$LB, y1 = both.pred.f0$UB, lwd = 2,
	code = 3, angle = 90, length = 0.1)
box(lwd = 2)
# Plot 2:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0.5, 5.5), ylim = c(0.1, 0.8))
axis(side = 1, at = 1:5,
	labels = c('-10%', '-7.5%', '-5%', '-2.5%', '0%'),
	font = 2,
	lwd.ticks = 2, cex.axis = 1.25)
mtext(text = 'Intensity level', side = 1, line = 3, cex = 1.5, font = 2)
mtext(text = 'Intensity effect', side = 3, font = 2, line = 1.25, cex = 2)
text(x = 0.6125, y = 0.77, labels = '(b)', font = 2, cex = 1.5)
# Actual data:
points(1:5, both.pred.int$Resp, type = 'b', pch = 19, lwd = 2)
arrows(x0 = 1:5, x1 = 1:5,
	y0 = both.pred.int$LB, y1 = both.pred.int$UB, lwd = 2,
	code = 3, angle = 90, length = 0.1)
box(lwd = 2)

## Plot predictions for gender interactions across all experiments:

quartz('', 13.5, 5)
par(mfrow = c(1, 3), mai = c(0, 0, 0, 0.25), omi = c(0.9, 1.35, 0.9, 0.5))
# PLot 1:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0, 4), ylim = c(0.2, 0.8))
axis(side = 2, at = seq(0.2, 0.8, 0.2),
	labels = paste0(seq(20, 80, 20), '%'),
	las = 2, font = 2,
	lwd.ticks = 2, cex.axis = 1.85)
mtext(text = '% Polite', side = 2, font = 2, line = 5.5, cex = 2)
mtext(text = 'Experiment 1', side = 3, font = 2, line = 3.4, cex = 1.85)
mtext(text = '(F0 manipulation)', side = 3, font = 2, line = 0.95, cex = 1.35)
axis(side = 1, at = c(1, 3),
	font = 2, lwd.ticks = 2, labels = c('', ''))
axis(side = 1, at = c(1, 3),
	labels = c('female\nlisteners', 'male\nlisteners'),
	font = 2.5, line = 2.5, tick = F, cex.axis = 2)
legend('topright', pch = 16:17, lty = 1:2, lwd = 2,
	legend = c('female speakers', 'male speakers'), cex = 1.5)
text(x = 0.1, y = 0.79, labels = '(a)', font = 2, cex = 1.75)
# Actual data:
points(x = c(1, 3) - xfactor, y = gender.pred.f0[c(1, 3),]$Resp, type = 'b',
	pch = 16, cex = 1.5,
	lwd = 2)
points(x = c(1, 3) + xfactor, y = gender.pred.f0[c(2, 4),]$Resp, type = 'b',
	pch = 17, cex = 1.5,
	lwd = 2, lty = 2)
arrows(x0 = c(1, 3) - xfactor,
	x1 = c(1,3 ) - xfactor,
	y0 = gender.pred.f0[c(1, 3),]$LB,
	y1 = gender.pred.f0[c(1, 3),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
arrows(x0 = c(1, 3) + xfactor,
	x1 = c(1,3 ) + xfactor,
	y0 = gender.pred.f0[c(2, 4),]$LB,
	y1 = gender.pred.f0[c(2, 4),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
box(lwd = 2)
# Plot 2:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0, 4), ylim = c(0.2, 0.8))
mtext(text = 'Experiment 2', side = 3, font = 2, line = 3.4, cex = 1.85)
mtext(text = '(intensity manipulation)', side = 3, font = 2, line = 0.95, cex = 1.35)
axis(side = 1, at = c(1, 3),
	font = 2, lwd.ticks = 2, labels = c('', ''))
axis(side = 1, at = c(1, 3),
	labels = c('female\nlisteners', 'male\nlisteners'),
	font = 2, line = 2.5, tick = F, cex.axis = 2)
text(x = 0.1, y = 0.79, labels = '(b)', font = 2, cex = 1.75)
# Actual data:
points(x = c(1, 3) - xfactor, y = gender.pred.int[c(1, 3),]$Resp, type = 'b',
	pch = 16, cex = 1.5,
	lwd = 2)
points(x = c(1, 3) + xfactor, y = gender.pred.int[c(2, 4),]$Resp, type = 'b',
	pch = 17, cex = 1.5,
	lwd = 2, lty = 2)
arrows(x0 = c(1, 3) - xfactor,
	x1 = c(1,3 ) - xfactor,
	y0 = gender.pred.int[c(1, 3),]$LB,
	y1 = gender.pred.int[c(1, 3),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
arrows(x0 = c(1, 3) + xfactor,
	x1 = c(1,3 ) + xfactor,
	y0 = gender.pred.int[c(2, 4),]$LB,
	y1 = gender.pred.int[c(2, 4),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
box(lwd = 2)
# Plot 3:
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(0, 4), ylim = c(0.2, 0.8))
mtext(text = 'Experiment 3', side = 3, font = 2, line = 3.4, cex = 1.85)
mtext(text = '(F0 & intensity manipulation)', side = 3, font = 2, line = 0.95, cex = 1.35)
axis(side = 1, at = c(1, 3),
	font = 2, lwd.ticks = 2, labels = c('', ''))
axis(side = 1, at = c(1, 3),
	labels = c('female\nlisteners', 'male\nlisteners'),
	font = 2, line = 2.5, tick = F, cex.axis = 2)
text(x = 0.1, y = 0.79, labels = '(c)', font = 2, cex = 1.75)
# Actual data:
points(x = c(1, 3) - xfactor, y = gender.pred.both[c(1, 3),]$Resp, type = 'b',
	pch = 16, cex = 1.5,
	lwd = 2)
points(x = c(1, 3) + xfactor, y = gender.pred.both[c(2, 4),]$Resp, type = 'b',
	pch = 17, cex = 1.5,
	lwd = 2, lty = 2)
arrows(x0 = c(1, 3) - xfactor,
	x1 = c(1,3 ) - xfactor,
	y0 = gender.pred.both[c(1, 3),]$LB,
	y1 = gender.pred.both[c(1, 3),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
arrows(x0 = c(1, 3) + xfactor,
	x1 = c(1,3 ) + xfactor,
	y0 = gender.pred.both[c(2, 4),]$LB,
	y1 = gender.pred.both[c(2, 4),]$UB,
	code = 3, angle = 90, length = 0.1, lwd = 2)
box(lwd = 2)



##------------------------------------------------------------------
## Raw f0 and intensity analysis:
##------------------------------------------------------------------

## Calculate z-scores to compare effects:

f0 <- mutate(f0,
	f0mn_z2 = (f0mn_z - mean(f0mn_z)) / sd (f0mn_z))
int <- mutate(int,
	f0mn_z2 = (f0mn_z - mean(f0mn_z)) / sd (f0mn_z),
	inmn_z2 = (inmn_z - mean(inmn_z)) / sd (inmn_z))
both <- mutate(both,
	f0mn_z2 = (f0mn_z - mean(f0mn_z)) / sd (f0mn_z),
	inmn_z2 = (inmn_z - mean(inmn_z)) / sd (inmn_z))

## Experiment 1, raw F0 effect:

summary(f0.raw <- glmer(Resp ~ f0mn_z2 + LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + f0mn_z2|Listener) + (1 + f0mn_z2|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(f0.raw.null <- glmer(Resp ~ LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + f0mn_z2|Listener) + (1 + f0mn_z2|Speaker) + (1|Item),
	data = f0, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(f0.raw.null, f0.raw, test = 'Chisq')

## Experiment 2, raw intensity effect:

summary(int.raw <- glmer(Resp ~ inmn_z2 + LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + inmn_z2|Listener) + (1 + inmn_z2|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(int.raw.null <- glmer(Resp ~ LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + inmn_z2|Listener) + (1 + inmn_z2|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(int.raw.null, int.raw, test = 'Chisq')

## Experiment 2, raw f0 effect:

summary(int.raw.f0 <- glmer(Resp ~ f0mn_z2 + LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + f0mn_z2|Listener) + (1 + f0mn_z2|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(int.raw.f0.null <- glmer(Resp ~ LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + f0mn_z2|Listener) + (1 + f0mn_z2|Speaker) + (1|Item),
	data = int, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(int.raw.f0.null, int.raw.f0, test = 'Chisq')

## Experiment 3, raw intensity effect:

summary(both.int.raw <- glmer(Resp ~ inmn_z2 + LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + inmn_z2|Listener) + (1 + inmn_z2|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.int.raw.null <- glmer(Resp ~ LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + inmn_z2|Listener) + (1 + inmn_z2|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(both.int.raw.null, both.int.raw, test = 'Chisq')

## Experiment 3, raw f0 effect:

summary(both.f0.raw <- glmer(Resp ~ f0mn_z2 + LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + f0mn_z2|Listener) + (1 + f0mn_z2|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(both.f0.raw.null <- glmer(Resp ~ LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1 + f0mn_z2|Listener) + (1 + f0mn_z2|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(both.f0.raw.null, both.f0.raw, test = 'Chisq')

## Save models:

save.image('models_so_far.RData')




##------------------------------------------------------------------
## Meta-analysis of gender differences and f0 effect across experiments:
##------------------------------------------------------------------

## Create unique listener identifiers:

f0 <- mutate(f0,
	Listener2 = paste0('f0.', Listener))
int <- mutate(int,
	Listener2 = paste0('int.', Listener))
both <- mutate(both,
	Listener2 = paste0('both.', Listener))

## Select all:

all <- select(f0, Listener2, LogRT_c, Resp, Item, Speaker,
	ListenerSex_c, SpeakerSex_c, f0mn_z2)
all <- rbind(all,
	select(int, Listener2, LogRT_c, Resp, Item, Speaker,
		ListenerSex_c, SpeakerSex_c, f0mn_z2))
all <- rbind(all,
	select(both, Listener2, LogRT_c, Resp, Item, Speaker,
		ListenerSex_c, SpeakerSex_c, f0mn_z2))

## Model for cross-experiment gender interactions:

summary(cross.gender <- glmer(Resp ~ 
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1|Listener) + (1|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(cross.gender.noint <- glmer(Resp ~ 
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + 
	(1|Listener) + (1|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(cross.gender.noint.nolist <- glmer(Resp ~ 
	LogRT_c + 
	SpeakerSex_c + 
	(1|Listener) + (1|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(cross.gender.noint.nospeak <- glmer(Resp ~ 
	LogRT_c + 
	ListenerSex_c + 
	(1|Listener) + (1|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Likelihood ratio tests:

anova(cross.gender.noint, cross.gender, test = 'Chisq')	# p = .036
anova(cross.gender.noint.nolist, cross.gender.noint, test = 'Chisq')		# listener gender
anova(cross.gender.noint.nospeak, cross.gender.noint, test = 'Chisq')	# speaker gender

## Get predictions for the interactions to understand:

cross.gender.pred <- data.frame(ListenerSex_c = c(-0.5, -0.5, 0.5, 0.5),
	SpeakerSex_c = c(-0.5, 0.5, -0.5, 0.5),
	LogRT_c = 0, Resp = 0)
cross.gender.pred$Resp <- predict(cross.gender,
	newdata = cross.gender.pred, re.form = NA)
cross.gender.pred$Resp <- plogis(cross.gender.pred$Resp)

## Model for cross-experiment f0 effect:

summary(cross.f0 <- glmer(Resp ~ f0mn_z2 + 
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1|Listener) + (1|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(cross.f0.null <- glmer(Resp ~ 1 + 
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	(1|Listener) + (1|Speaker) + (1|Item),
	data = both, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
anova(cross.f0.null, cross.f0)



##------------------------------------------------------------------
## UO analyses:
##------------------------------------------------------------------

## Main model for UO (Experiment 3B):

summary(both.UO.mdl <- glmer(Resp ~ int_c + F0_c + int_c:F0_c +
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c + 
	ListenerSex_c:int_c + 
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = both.UO, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Main model for UO (Experiment 1B):

summary(f0.UO.mdl <- glmer(Resp ~ F0_c + 
	LogRT_c + 
	ListenerSex_c + SpeakerSex_c + ListenerSex_c:SpeakerSex_c +
	(1 + F0_c|Listener) + (1 + F0_c|Speaker) + (1|Item),
	data = f0.UO, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
	# what's the problem with listener sex?

## Save models:

save.image('models_so_far.RData')


