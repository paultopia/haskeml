Strange behavior: `trainOLS testFeatures testLabels 0.01 0.0001 66` works, but `trainOLS testFeatures testLabels 0.01 0.0001 67` blows up.  For anything with a learning rate of 0.01, anything 67 or higher for numiters just yields a list of [Infinity...]

Decreasing the learning rate seems to increase the number of iterations I can run before it blows up to infinity.  I get to `trainOLS testFeatures testLabels 0.001 0.0001 85` but then that learning rate blows up to the [Infinity, Infinity...] at 86. 

I get to `trainOLS testFeatures testLabels 0.0001 0.0001 118` before blowup.

I can get into the 4 digits: `trainOLS testFeatures testLabels 0.0000001 0.0001 2295`

I think I might be getting bitten by some kind of floating point problem?  

Also, noticing the numbers are wrong, I realize the numbers I was relying on were column-wise not row-wise, so let's fix that, eh?  In next commit.

--- 

After fixing the dimensions and radically cranking down the learning rate and the threshold, I can make some progress: 

Olsgradient> trainOLS testFeatures testLabels 0.000000001 0.000001 100000
[3.451887498398911e150,3.2124696683713294e151,1.4698249124294339e153]

which is still way wrong, but at least in the ballpark?

But still fluctuating widely: 

Olsgradient> trainOLS testFeatures testLabels 0.000000001 0.000001 170000
[5.901057974271367e257,5.49176928924837e258,2.512689658714969e260]

Olsgradient> trainOLS testFeatures testLabels 0.00000000001 1 500000
[104617.7409380684,973607.775151017,4.4546149295402564e7]

w.t.f.  

Since it fluctuates so widely, even with tiny learning rates, my best guesses are that: 

1.  the Infinity results are where the fluctuations are so big that they overflow the limit for a Double, and 
2.  and this is because somehow my calculation of the gradient step is wrong
3.  Or maybe I should be using ratios?
4.  I need to learn how people actually do serious math without integers and floats blowing everything the hell up.

Fix plan: review gradient descent algorithm, preferably not alone, make sure I'm not just being dumb on the math.  Then dig through from inside out with hspec and get some known correct results (by hand calculation) + tests.

(currently writing a reference implementation in python; if that blows up similarly, I'll know that the math is probably wrong, since the code ought to be easier to manage with the power to drop down to imperative land prn.)

ALSO: part of the problem might be that I need to scale the features before actually doing gradient descent.  See http://sebastianraschka.com/Articles/2014_about_feature_scaling.html 

----

Ok, now I have a python reference implementation, which works.  And I've got values that are known to work in python: 

- learnRate = 0.001

- threshold = 0.01

- maxiterations = 100000

I've also scaled the features, and verified that my haskell scaling implementation matches what scikit-learn does to scale.

Unfortunately:

Olsgradient> trainOLS testFeatures testLabels 0.001 0.01 100000
[NaN,NaN,NaN]

so something else is still amiss... 

