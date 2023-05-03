# Assessing changes in an ant-aphid mutualism across urban grasslands

This repository contains the data and R code to analyse changes in aphid and ant populations and behaviour along a gradient of urbanisation in Berlin, Germany. 
This is a work in progress associated to a publication in preparation.

## Abstract
Urbanisation challenges species to cope with novel environmental conditions, which may result in the disruption of mutualistic interactions. While some mutualisms, like pollination, have been shown to be at risk in cities (Ropars et al., 2019), little is known about how other types of mutualisms are affected by urbanisation. We studied the protective mutualism between the pink tansy aphid (Metopeurum fuscoviride) and the black garden ant (Lasius niger) along an urbanisation gradient in Berlin, Germany. Repeated observations in nine locations along this gradient allowed us to quantify the investment of ants in tending aphids, the survival and growth of tended aphid colonies, and the behavioural response of ants to a simulated attack of the aphids. Aphid colonies flourished and were tended equally across the urbanisation gradient, and there was a consistent positive relationship between aphid and ant numbers. However, ants defended aphids more aggressively in more urban environments. Our study suggests that not only this protective mutualism is maintained in the city, but that ants might even rely more on it and thus defend it more aggressively. Such behavioural changes may explain in part the success of these mutualistic ants as food resources become scarce and unpredictable with urbanisation, and highlights the importance of protective mutualisms for urban biodiversity. 

## Authors
Hannah Gaber, Maud Bernard-Verdier, Florian Ruland and Jonathan Jeschke

## Licence
All R code is under the MIT license.
All data files are available for reuse only with permission from the authors.

## Data files
Raw data of this study is stored in ten *.csv*-files  that are located in the folder [data](data):
- [GivenMeta_plots.csv](data/GivenMeta_plots.csv): metadata of 56 dry grassland sites located in Berlin, Germany, which form a research platform to investigate urban ecosystem functioning (see [CityScapeLab Berlin](https://www.mdpi.com/2071-1050/12/6/2565) for more details). We selected nine of these sites to conduct our study which were located along a gradient of urbanisation. The percentage of sealed surfaces in a 500m buffer around the study site ("Seal_500") was used as explanatory variable to describe the gradient of urbanisation.
- [General_plot.csv](data/General_plot.csv): metadata of the nine study sites selected for the field survey, i.e. the plot ID ("plot"), the geographical coordinates ("Long", "Lat"), the number of visits of the site ("visits"), the number of sampling sessions that were undertaken ("sample size") and the number of studied ant-aphid-host plant systems ("N_plant")
- [Met_plot_date.csv](data/Met_plot_date.csv): site metadata collected during each sampling session (i.e. study of a particular ant-aphid-host plant system at a particular date). We measured the temperature before, during and after each sampling session ("temp1","temp2","temp3") to control for temperature effects on ant behaviour. We additionally collected supporting information on weather, presence of other ant species on the host plant and presence of aphid predators to describe the ecological context of our data.
- [Met_plant.csv](data/Met_plant.csv): host plant metadata collected during each sampling session. It includes supporting information on host plant phenology ("stade1": budding, "stade2": full flowering, "stade3": end of flowering, "stade4": desiccated plant), location of the aphid colony (i.e. stalk/leaves/flower) and organisation of the aphid colony (i.e. aggregated/dispersed). We further counted the number of parasitized aphids (i.e. mummies) in the aphid colony and calculated the proportion of parasitized aphids ("prop_paras") to test how parasitism influenced the survival/extinction of aphid colonies. 
- [aphid_extinctions.csv](data/aphid_extinctions.csv): We recorded at each visit of the study site whether the previously studied ant-aphid-host plant systems survived or went extinct. The file lists the systems which went extinct during the field survey and the date of record. This information was used to test how parasitism influenced the survival/extinction of aphid colonies.

- [Exp1.csv](data/Exp1.csv): We recorded during each sampling session the behaviour of 2-5 focal ants during 2-5 minutes each using a voice recorder. The file contains supporting information about the focal ants, essentially in which plant area the ants were situated during the focus observation. This information was used to discriminate between "caretakers" (ant stays in the area of the aphid colony), "shepperds" (ant moves away from the aphids to explore other plant parts) and "transporters" (ant leaves the host plant). Furthermore, the behavioural record of each ant (stored in [activity sequence.csv](data/activity sequence.csv)) is linked to the focal ant by its unique identifier ("rec"). 

- [activity sequence.csv](data/activity sequence.csv):  This file contains sequences of ant behaviour collected during focus observations of 2-5 focal ants per sampling session. The focal ant was observed during 2-5 minutes and in situ behaviour was recorded based on predefined behavioural categories using a voice recorder and voice records were subsequently re-written into text files. These text files include the sequence of behaviours each focal ant and how long .....
The classified ant behaviours include palpating aphids with antennae and collecting their honeydew ("cl"), walking ("w" and "wf"), standing ("st"), interacting with ants by antennae contact ("a"), trophallaxis ("k"), stretching the abdomen ("str"), grooming itself ("li") and possible other, undefined behaviours ("o").

- [Exp2.csv](data/Exp2.csv)

- [Exp3a.csv](data/Exp3a.csv)

- [Exp3b.csv](data/Exp3b.csv)

- [geodata](data/geodata) and [bezirksgrenzen.geojson](data/bezirksgrenzen.geojson)


## How to run the code
