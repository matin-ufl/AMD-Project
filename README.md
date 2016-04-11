## Analyzing the effect of temporal accelerometer variables in detecting Cardio Vascular Disease, Cognitive Dysfuncton, and Mobility Impairment.

There have been numberous works done on evaluating mobility quality, estimating cognitive function and predicting cardio vascular diseases (CVD). However, to best of our knowledge, there is little done to see the effect of temporal variables for CVD prediction and very few works which sought variables beyond activity count and steps to evaluate mobility and cogntion in free-living condition.

We hypothesize that by including temporal information can highlight an underlying structure for the mentioned target variables (*mobility*, *cognition*, and/or *CVD*). To evaluate our hypothesis, we consider the following procedure:

* First we create a dataset containing standard features: daily activity count (average and standard deviation - count/min), daily number of steps (average and standard deviation - step/min), and axis 1 proportion of vector magnitude (average and standard deviation) which can be interpreted as the time the participant is standing or sitting straight.
* The second dataset is an extended version of the abovementioned daily dataset - it contains the same variables but instead of averaging over days, it provides four periods within a day: *Morning* (8am-11am), *Noon* (11am-2pm), *Afternoon* (2pm-5pm), and *Evening* (5pm-8pm).
* The third dataset is constructed by borrowing the ideas from *Circadian Rythm* analysis. However, since we do not have 24hr data, we cannot replicate the same analysis and we follow the procedure as much depth as we are allowed: constructing features like *Peak* (activity count count/min, and its time-of-day), *Nadir* (activity count/min and its time-of-day), the difference between *Peak* and *Nadir* (in terms of time and activity count) and *Mesor* (median of daily activity count/min, and time-of-day).

The structure of this repository is as follows:

* **Dataset Creation** which contains the scripts for cleaning the activity count per second data and constructing the abovementioned feature sets.
* 
