The dataset this time contained exacerbations while in the trial. The associated paper looked at something different: exacerbation rate.

In the associated paper, (modelled) exacerbation rate was essentially the total number of exacerbations in a ground by the total number or study days (prorata'd to a year)

This allowed the two groups to be compared for the number of exacerbations per year.

However - while I agree with displaying modelled exacerbations, this is a bit misleading as we are only looking at the group. This does not (easily?) inform as to the individual variability

Here two graphs were produced.
1)  Adjusted exacerbations per year
this graph uses a simple regression to adjust the empiracle exacerbations to give each patient an adjusted (predicted) number of exacerbations based on thier demographics
it plots the exacerbation rate that would be expected over a year for each treatment.
median group difference is shown (small but significant)
highlighting that there is a difference but _a lot_ of variability

However, acknowledged that a simple regression may not be appropriate given the Y variable of exacerbations here (and the neg binomial dist)

2) Probability of Exacerbation
similar to the above chart, but instead uses a binomial logistic regression to predict the probability of having 1 or more exacerbations while on trial
each patient is assigned a probability and this is plotted. again, median difference (probably based on sample size) but a lot of patient variability


bonus video
https://www.youtube.com/watch?v=HhGuXCuDb1U