# self-handicapping
Code and data for: Xiang, Y., Gershman, S.J., & Gerstenberg, T. (2024). A signaling theory of self-handicapping.

People use various strategies to bolster the perception of their competence. One strategy
is self-handicapping, by which people place obstacles that undermine performance in order
to protect perceived competence in case of failure, or enhance it in case of success. Despite
much prior research, it is unclear why, when, and how self-handicapping occurs. We develop
a formal theory that chooses the optimal degree of self-handicapping based on its anticipated
performance and signaling effects. We test the theory’s predictions in two experiments (𝑁 =
400), showing that self-handicapping occurs more often when it is unlikely to affect the
outcome and when it increases the perceived competence in the eyes of a naive observer.
With sophisticated observers (who reason recursively and thereby recognize the tactic), self-
handicapping is less effective when followed by failure. We show that the theory also explains
the findings of several past studies. By offering a systematic explanation of self-handicapping,
the theory lays the groundwork for developing effective interventions.

<img src="Figures/theory_schematic.png" alt="Theory Schematic" width="800" />

Experiment 1: https://gershmanlab.com/experiments/yang/hidden_genius/exp1final.html

Experiment 2: https://gershmanlab.com/experiments/yang/hidden_genius/exp2final.html

Preregistration: https://aspredicted.org/f4h3-f4xv.pdf

In the `Code` folder, `model_fitting.R` outputs the fitted parameter values (`fitted_params.csv`). `simulation.R` then uses these values to generate model predictions (`model_prediction.csv`). `plot_figures.R` uses data under the Data folder and model predictions to plot figures. `helper.R` contains helper functions for plotting. `regression.R` contains Bayesian mixed-effects models. 

In the `Data` folder, `data.csv` contains data from both experiments. The `accuracy` and `probability_10` (probability of choosing to be evaluated on 10 answers) columns are from the actor block. `observer`, `outcome`, and `evaluation` columns are from the naive and sophisticated observer blocks.
