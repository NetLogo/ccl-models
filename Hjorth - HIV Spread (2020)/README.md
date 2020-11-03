NetLogo HIV spread model exploring the impact of PrEP prescription guidelines

Description
---
This agent-based model was built as part of a replication effort of Jeness et al.?s work (linked below). The model simulates an MSM sexual activity network for the purpose of modeling the effects of respectively PrEP and ART on HIV prevention. The purpose of the model is to explore the differences between different interpretations of the NIH Indication Guidelines for PrEP.

All data-driven assumptions in the model are taken directly from Jenness et. al. However, as R and NetLogo function differently as languages, models are structured differently. We therefore include here just an overview of what happens in each time increment in the model, and in which order this happens.

The Go Procedure

Network updates:
- All couples increment the remaining time they are together. If that reaches 0, they break up.
- New relationships are then created, based on criteria from Jenness et al. Individual people who decide they want to find a new partner use a utility function to find a partner close to what they are looking for, and who is also looking for someone like that.
- When criteria for number of relationships in the population are met, no new relationships are created.

Sexual transmission:
- Couples decide how many intercourses they have this week, and for each intercourse, they decide whether to use a condom. These intercourses are then executed.
- As NIH PrEP indications all relate to specific conditions for an intercourse, we now save the tick/time if any of the intercourses fulfill the indication criteria.
- Finally for each intercourse in a serodiscordant couple, we calculate whether there was HIV transmission

PrEP:
- Those who have been on prep for one year, go off PrEP.
- Those who have not been indicated for PrEP for a year are indicated if they meet criteria, and are put on PrEP for a year

HIV Testing
- People (with the exception of those who never test) have a chance of going in for testing.
- If they test HIV positive, they are now "known HIV+"
- If they test negative, then they are indicated for PrEP and put on PrEP if they meet criteria.

Anti Retroviral Treatment (ART)
- People who are in treatment have a risk of falling out of treatment
- People who have previously fallen out of treatment have a chance of going into treatment
- People who are known positive but who have never been treated have a chance to go into treatment

Aging, death, and time
- People age, and as they get older they have an increasing risk of dying from aging.
- People with HIV have their viral load updated
- People with HIV may progress into AIDS stage
- People in AIDS stage may die


Citation information
---

For the model itself:

> [Arthur Hjorth, Wouter Vermeer, Uri Wilensky (2020, June 05). "The NetLogo HIV Spread Model Exploring Impact of PrEP Indication Guidelines" (Version 1.0.0). CoMSES Computational Model Library. Retrieved from: https://www.comses.net/codebases/d3d45a7e-24a4-42e9-a44b-e6e8e293e578/releases/1.0.0/](https://www.comses.net/codebases/d3d45a7e-24a4-42e9-a44b-e6e8e293e578/releases/1.0.0/)

For the associated paper:

> [Vermeer, Wouter, Hjorth, Arthur, Jenness, Samuel M., Brown, C Hendrick and Wilensky, Uri (2020) 'Leveraging Modularity During Replication of High-Fidelity Models: Lessons from Replicating an Agent-Based Model for HIV Prevention' Journal of Artificial Societies and Social Simulation 23 (4) 7 <http://jasss.soc.surrey.ac.uk/23/4/7.html>. doi: 10.18564/jasss.4352](http://jasss.soc.surrey.ac.uk/23/4/7.html)
		
BibTex:

```
@article{vermeer2020,
   title = {Leveraging Modularity During Replication of High-Fidelity Models: Lessons from Replicating an Agent-Based Model for HIV Prevention},
   author = {Vermeer, Wouter and Hjorth, Arthur and Jenness, Samuel M. and Brown, C Hendrick and Wilensky, Uri},
   journal = {Journal of Artificial Societies and Social Simulation},
   ISSN = {1460-7425},
   volume = {23},
   number = {4},
   pages = {7},
   year = {2020},
   URL = {http://jasss.soc.surrey.ac.uk/23/4/7.html},
   DOI = {10.18564/jasss.4352},
   keywords = {Replication, Agent-Based Models, Modular, High-Fidelity, HIV},
   abstract = {High-fidelity models are increasingly used to predict, and guide decision making. Prior work has emphasized the importance of replication in ensuring reliable modeling, and has yielded important replication strategies. However, this work is based on relatively simple theory generating models, and its lessons might not translate to high-fidelity models used for decision support. Using NetLogo we replicate a recently published high-fidelity model examining the effects of a HIV biomedical intervention. We use a modular approach to build our model from the ground up, and provide examples of the replication process investigating the replication of two sub-modules as well as the overall simulation experiment. For the first module, we achieved numerical identity during replication, whereas we obtained distributional equivalence in replicating the second module. We achieved relational equivalence among the overall model behaviors, with a 0.98 correlation across the two implementations for our outcome measure even without strictly following the original model in the formation of the sexual network. Our results show that replication of high-fidelity models is feasible when following a set of systematic strategies that leverage the modularity, and highlight the role of replication standards, modular testing, and functional code in facilitating such strategies.},
}		

```


