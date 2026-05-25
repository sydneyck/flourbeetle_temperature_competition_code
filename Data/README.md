
Below is the metadata for the experimental data files associated with …

#### **General**

\- `hist_temp`: historical temperature (25, 30, or 35°C) under which
beetles evolved for 3.5 years

\- `competition`: interspecific competition treatment (wo: without
interspecific competition [*T. confusum* absent] or w: with
interspecific competition [*T. confusum* present])

\- `rep`: replicate population or community (1-10 for each `hist_temp`
and `competition` combination)

\- `sex`: f (female) or m (male)

\- `weight`: dry weight of beetle (g)

\- `egg_num`: replicate egg (1-3 for each beetle)

#### **beetlecounts.csv**

\- `week`: weeks elapsed since the start of the interspecific
competition experiment at the time of data collection (6, 12, 18, 24, or
30)

\- `cast`: number of live adult *T. castaneum* beetles counted

\- `cast_dead`: number of dead adult *T. castaneum* beetles counted

\- `conf`: number of live adult *T. confusum* beetles counted (*NA* in
competition treatments without interspecific competition [wo])

\- `conf_dead`: number of dead adult *T. confusum* beetles counted (*NA*
in competition treatments without interspecific competition [wo])

#### **bodysize_cg_before.csv**

\- `indiv_num`: replicate beetle (1-10 for each of females and males for
each `hist_temp`)

#### **bodysize_cg_end.csv**

\- `indiv_num`: replicate beetle (1-5 for each of females and males for
each replicate population/community)

#### **bodysize_founder.csv**

\- `year`: year body size was measured 

\- `temp`: temperature under which beetles evolved (25, 30, or 35°C; *founder* indicates body size measured in the single original source population)

\- `rep`: replicate population (1-6 for each `temp` in 2022 and 2023; 1-10 for each `temp` in 2025; *NA* for the single original source population)

\- `indiv_num`: replicate beetle (1-5 for each of females and males for each replicate population each year; 1-10 for each of females and males in the original source population)

\- `sex`: female or male

\- `weightinmg`: dry weight of beetle (mg)

#### **fecundity.csv**

\- `indiv_num`: replicate beetle (1-10 for each `hist_temp`)

\- `egg_count`: number of eggs laid by each female beetle in 48 hours

#### **development.csv**

\- `indiv_num`: replicate beetle (1-10 for each `hist_temp`)

\- `start_date`: date when individual eggs were added to their habitats
to start the development assay (YYYY-MM-DD)

\- `pupation_date`: date when individual pupated (YYYY-MM-DD)

\- `days_to_pupation`: number of days it took for the egg to develop
into pupa calculated as the difference between the `start_date`  
and `pupation_date`

#### **survival.csv**

\- `indiv_num`: replicate beetle (1-10 for each `hist_temp`)

\- `survival`: survival recorded after 35 days since the start of the
development assay (y: yes or n: no)

#### **eggsize.csv**

\- `indiv_num`: replicate beetle (1-10 for each `hist_temp`)

\- `length`: length of the egg (µm)

\- `width`: width of the egg (µm)
