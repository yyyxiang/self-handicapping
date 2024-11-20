# self-handicapping
Code and data for: Xiang, Y., Gershman, S.J., & Gerstenberg, T. (2024). A signaling theory of self-handicapping.

<img src="Figures/theory_schematic.png" alt="Theory Schematic" width="400" />

Experiment 1: https://gershmanlab.com/experiments/yang/hidden_genius/exp1final.html

Experiment 2: https://gershmanlab.com/experiments/yang/hidden_genius/exp2final.html

Preregistration: https://aspredicted.org/f4h3-f4xv.pdf

In the `Code` folder, `model_fitting.R` outputs the fitted parameter values (`fitted_params.csv`). `simulation.R` then uses these values to generate model predictions (`model_prediction.csv`). `plot_figures.R` uses data under the Data folder and model predictions to plot figures. `helper.R` contains helper functions for plotting. `regression.R` contains Bayesian mixed-effects models. 

In the `Data` folder, `data.csv` contains data from both experiments. The `accuracy` and `probability_10` (probability of choosing to be evaluated on 10 answers) columns are from the actor block. `observer`, `outcome`, and `evaluation` columns are from the naive and sophisticated observer blocks.
