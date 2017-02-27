Bus Ridership Prediction for Predictive Analytics Capstone
----------------------------------------------------------

The data for this repo was originally used in a Capstone project for my
MS in Predictive Analytics at Northwestern. Unlike most Capstones
groups, though, ours had a real client and we were using real-world data
to try to solve a problem. Our client was a transportation agency for a
city in the Northwest region of the US. One of the team members had
worked for this agency and had coordinated our project as a more
realistic real-world analytics project and we jumped at the chance.

Part of the brief was to help the agency develop a model to accuracy
predict bus ridership that they could then use internal for a variety of
functions. The model they used now was housed in Excel and nobody had
any institutional memory about how it worked. What's more, manual edits
were commonplace so that no one could really be sure if the predictions
in use were accuracy anymore or not. So even if we were unable to "beat"
the accuracy of the model, just having a replicable model that is well
documented would be a vast improvement over current practice.

My part of the group project was to develop this predictive model. The
team pulled together a wide range of data variables for the predictive
tasks as well as the other tasks handled by the other team members. In
additional to using standard training hold-out datasets, we used a
collection of predictions from their current model and compared their
current predictions to our models predictions.

I assessed 48 different models to see if I could beat their current
prediction methodology. I found a model that predict bus ridership
within ~200 rides, which was far more accurate than their current
methodology that predicted withing ~800 rides. Unfortunately, executing
this model was going to be a problem because it used over 170 pieces of
data to do it. I then explored more parsimonious models that might be a
bit less accurate than the best found but would be far easier for the
agency to execute in the future.

### R syntax files I used to generate the material necessary for the repo:

------------------------------------------------------------------------

-   [01 - Data Integration & Missing Imputation
    Codeup.R](https://github.com/msheffer2/Bus-Ridership-Prediction/blob/master/01%20-%20Data%20Integration%20%26%20Missing%20Imputation.R)
    -- Original data codeup spanned several data files and syntax files
    that were used to coordinate the codeup across different individuals
    and different data collection efforts. This file begins with the
    completed "coded data" and uses it to integrate into a single
    dataset usable for prediction..
-   [02 - EDA & Model
    Prep.R](https://github.com/msheffer2/Bus-Ridership-Prediction/blob/master/02%20-%20EDA%20%26%20Model%20Prep.R)
    -- This syntax cycles through each of the predictors and creates
    graphs of its distribution with ridership. The purpose of this was
    not only to perform EDA but to also examine if data transformations
    might not be helpful. The data is then prepped for analysis by
    checking for Zero Variance variables and highly correlated
    predictors, as well as by creating training and testing
    partitions needed.
-   [03 - Model
    25.R](https://github.com/msheffer2/Bus-Ridership-Prediction/blob/master/03%20-%20Model%2025.R)
    -- Generates the original winning model out of the 48 competing
    models: bagged trees based on untransformed data.
-   [04 - Plotting
    Data.R](https://github.com/msheffer2/Bus-Ridership-Prediction/blob/master/04%20-%20Plotting%20Data.R)
    -- This is only an excerpt from the original file that plots
    precision estimates for the competing models.
-   [05 - Improving Model
    25.R](https://github.com/msheffer2/Bus-Ridership-Prediction/blob/master/05%20-%20Improving%20Model%2025.R)
    -- This syntax file revises Model \#25 to see if its possible to
    improve on current predictions but with fewer predictors. Revised
    \#25 uses just 12 predictors to beat out the current model but with
    far more manageable predictors for use later.
-   [06 - Post Revision
    Plots.R](https://github.com/msheffer2/Bus-Ridership-Prediction/blob/master/06%20-%20Post%20Revision%20Plots.R)
    -- Comparing the case RMSE for Model \#25 to the original method and
    revised \#25. Also plots the variable importances for the
    revised model.
-   [fit\_assess.R](https://github.com/msheffer2/Bus-Ridership-Prediction/blob/master/fit_assess.R)
    -- a wrapper function to calculate the performance metrics required
    to compare the models. It calculates prediction accuracies via RMSE
    for training, testing, and test case data sets.

#### Technical Notes:

-   The work shown here is less "replicable" than some of the other
    repos because of the need to alter all the original data and to
    avoid showing lots of rather uninteresting syntax showing multiple
    modelling efforts. Instead, I provide pre-cleaned or pre-outputted
    data to streamline this process. The models show here, though, ,
    should be completely reproducible.

### Analytical Highlights

------------------------------------------------------------------------

Part of my research strategy is to try as many different ways of
modelling as I can, particularly because you never know what will work
the best but also you want to be able to ensure an end client that lots
of different things have been tried. In this case, I felt it would help
to instill confidence in the results if the client could see that a wide
range of predictive algorithms were tried in pursuit of their goal.

I ran 48 different models across 4 different datasets (reflecting
different transforms of the predictors) and 8 different predictive
algorithms, including a Lasso regression, a regression tree using the
CART algorithm, a regression tree using the Cubist algorithm, a random
forest, bagged tree model, gradient boosting machine, support vector
machine, and a neural network.

##### Figure 1: Comparing the predictive accuracy of the 48 models tested

![](README_files/figure-markdown_strict/unnamed-chunk-2-1.png)

There were a few different ways of looking at the accuracy of the model
including comparing train and test root mean square error (RMSE) rates
in general but I thought comparing the test case RMSE rates would be the
most illustrative of the new models ability to compete against the
current methodology (although comparison of the train/test RMSE would
have resulted in the same conclusion). Figure 1 summaries each of the 48
in models in how precise each is when predicting to the test-cases that
compare real-world ridership levels to outputs of the models. The
current method's RMSE is shown in the yellow line across the graph to
make it easier to identify a winner. The Bagged trees, Cubist models,
and SVM all perform well against the current prediction method but the
best model, model \#25, improved on the current method by about 74%.
Model \#25 was a bagged tree ensemble method that did not use any data
transformations and didn't exclude any correlated values from
consideration. Although Model \#25 outperforms the current model, its
only downside is that it requires 178 predictors to do it.

#### Revising the model

------------------------------------------------------------------------

In the syntax file, [05 - Improving Model
25.R](https://github.com/msheffer2/Bus-Ridership-Prediction/blob/master/05%20-%20Improving%20Model%2025.R)
I highlight part of the process I used to try to improve on Model \#25.
Essentially, I tried multiple versions of \#25 that relied on a smaller
sub-set of important variables determined by the "Out of Bag" error rate
or using prima facie judgement to pull out important predictors from the
EDA or from what was known about the client's requirements. Suffice it
to say, I was able to find a more parsimonious model that while not as
precise in it's predictions as was Model \# 25, was slightly more
accurate than the existing one without relying on a large number of
predictors.

##### Figure 2: Model \#25, Revised Model \#25, & Historical Prediction Precision

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)

The original model (in yellow) predicted the test cases within about 818
rides while Model \#25 (in blue) could predict the test cases within 208
rides (but using 178 predictors). By limiting Model \#25 to just 12
important variables (both quantitatively and qualitatively), it was
possible to still beat out the current method but to do it more
succinctly than the original Model \#25.

##### Figure 3: Model \#25 Revised Model Importance

![](README_files/figure-markdown_strict/unnamed-chunk-4-1.png)

The twelve variables chosen to remain the revised Model \#25 were chosen
for both quantitative and qualitative reasons. Several of the important
features in the model, shown in Figure 3, are already being used by the
agency in their current prediction (albeit not systematically), such as
day of service and the number of retail and non-retail jobs. There are
other influential variables added to \#25 Revised, including several
crime statistics and neighborhood walk scores.

### Deployment Example

------------------------------------------------------------------------

One thing we wanted to describe to the client was how this new model
could be implemented in their current organization. While we understood
the ease with which Excel offered users to manipulate predictions, we
recommended keeping the predictive algorithm within R so that not only
could someone be responsible for owning the prediction process but also
keeping the model inside R would provide opportunities to document and
update future changes as needed.

Although it isn't much to look at, I created a Skeleton [Shiny
App](https://msheffer.shinyapps.io/shinyapp/) based on Model \#25
Revised as a proof-of-concept in how the model could be "opened up" to
others to make predictions but still keeping the the predictive model
details safe from "hand tuning" as much as possible.
