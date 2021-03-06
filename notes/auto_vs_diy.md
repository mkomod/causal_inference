---
title: "Summary: Automate vs. Do-it-Yourself methods for Causal Inference"
output:
  html_document:
    keep_md: yes
    css: style.css
    fig_captions: true
---

---

# Summary

 - Researchers are incentivised to control for a large number of pretreatment covariates
   - Requires strong assumptions
   - or a flexible approach to fitting the model
 - Methods that flexibly model the response surface dominate over other methods
 - ACICC tried to address the issue of few and unfair comparisons by controlling
   - magnitude and size of treatment effects to unexplained variability
   - prevalence of non-linearities / higher order interactions
   - number / kinds of covariates
   - average size of treatment effect 
   - biases in misspecified models

----
    
# Notations and Assumptions

 - $Z$: binary treatment with $Z=0$ indicating assignment to treatment
 - $Y_i(0)$: outcome that would manifest if $Z_i=0$
 - $Y_i(1)$: outcome that would manifest if $Z_i=1$
 - *Individual level causal effects*: difference between the potential outcomes (see Rubin 78)
 - $Y_i=(1-Z_i)Y_i(0) + Z_i Y_i(1)$ is the observed outcome

## Estimands

 - *Sample average treatment effect (SATE)*: $E[Y(1) - Y(0)]$, where the expectation is taken over the analysis sample 
 - *Sample average effect of the treatment on the treated (SATT)*, average taken over those only in the treatment group

## Structural Assumptions

 - We can only every observe $Y(0)$ or $Y(1)$, therefore the treatment effects are not identified without further assumptions.
 - Assumptions:
   - *Ignorability*: i.e. all co-founders measured or there is conditional independence in assignment, for example in a randomised experiment $Y(0), Y(1) \perp Z \implies j[Y(a) | Z = a] = E[Y(a)]$.
   - In typical studies a more general form of ignroaibility is needed, $Y(0), Y(1) \perp Z | X$, under which, $E[Y(a) | Z = a, X] = E[Y(a)]$. Giving the ATE
$$
 E(Y(1) - Y(0) | X] =  E[Y(1) | Z=1, X] - E[Y(0) | Z=0, X]
$$
   - *Strong Ignorability* is a further assumption where $P(Z=1 | X) \in (0, 1)$ i.e. given a set of covariates a sample cannot belong to treatment with probability 1 or probability 0. Therefore there is always the possibility a sample receives (or does not receive) treatment.
 - Under the above assumptions (i.e. by construction) the causal effect is identifiable and can be estimated from the observed covariates.

----

# Data generation 

 - Attempts to mimic a hypothetical study on the effect of birth weight on IQ
 - Estimand is the effect of the treatment on the treated (SATT)
 - Assumes ignorability
 - Common support for the inferential groups (i.e. overlap in X for Z=1 and Z=0)
 - 4802 observations with 58 covariates:
   - 3 categorical
   - 5 binary
   - 27 count
   - 23 cont.

## Knobs

$$
P(Y(1), Y(0), Z | X) = \underset{\text{response surface}}{\underbrace{P(Y(1), Y(0) | X)}} \quad \times 
    \underset{\text{assignment mechanism}}{\underbrace{P(Z | X)}}
$$

The response surface and assignment mechanism where generated using a number of tunable knobs, these correspond to applying functions $f_j(x_k)$ and combining them or transforming them in different ways

The knobs are summarised as

 1. Degree of non-linearity
 2. Percentage of treated 
 3. Overlap
 4. Alignment
 5. Treatment effect heterogeneity
 6. Magnitude of treatment effect

----

# Methods

Broken up into Do-it-Yourself and Black box (BB) methods

## Comparison of methods

 - Root MSE
 - Confidence interval coverage i.e. $P(\beta_0 \in I_{0.95})$ where $I_{0.95}$ is the $95\%$ confidence set and $\beta_0$ is the true parameter value.
