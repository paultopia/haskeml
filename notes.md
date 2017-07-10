Strange behavior: `trainOLS testFeatures testLabels 0.01 0.0001 66` works, but `trainOLS testFeatures testLabels 0.01 0.0001 67` blows up.  For anything with a learning rate of 0.01, anything 67 or higher for numiters just yields a list of [Infinity...]

Decreasing the learning rate seems to increase the number of iterations I can run before it blows up to infinity.  I get to `trainOLS testFeatures testLabels 0.001 0.0001 85` but then that learning rate blows up to the [Infinity, Infinity...] at 86. 

I get to `trainOLS testFeatures testLabels 0.0001 0.0001 118` before blowup.

I can get into the 4 digits: `trainOLS testFeatures testLabels 0.0000001 0.0001 2295`

I think I might be getting bitten by some kind of floating point problem?  

Also, noticing the numbers are wrong, I realize the numbers I was relying on were column-wise not row-wise, so let's fix that, eh?  In next commit.


