# ----------------------------------------------------------------
# Load Modules
# ----------------------------------------------------------------

import os
import json
import math
import random
import numpy  as np
import pandas as pd
from sklearn.multiclass import OneVsRestClassifier
from sklearn.ensemble   import RandomForestClassifier
from sklearn.metrics    import f1_score

# ----------------------------------------------------------------
# Functions
# ----------------------------------------------------------------

# Rep function from R - Provides a list contraining items to repeat
def rep(item, n ):
    new = []
    for i in range(n):
        new.append(item)
    return new  

# Dumps Json files
def save(name,file):
	with open(name, 'w') as songs:
	    json.dump( file, songs, indent = 3 )
	print "Dumped File"

# Read Json Files
def read(name):
	with open(name) as data_file:
		data = json.load(data_file)
	return data

# ----------------------------------------------------------------
# Load Data
# ----------------------------------------------------------------

# Set working directory to data folder
os.chdir('CHANGE ME --- CHANGE ME --- CHANGE ME')

# Run this for full model - mod_matrix comes from get_data.m
#X = pd.read_csv('mod_matrix.csv',header=None)
#Y = pd.read_csv('labels.csv',header=None)

# Run this for for sub model - mod_matrix_new comes from get_data.m
comm = pd.read_csv("new_communities.csv", header=None)
mod  = pd.read_csv('mod_matrix_new.csv',header=None)
X = pd.concat([mod, comm], axis=1)
Y = pd.read_csv('new_labels.csv',header=None)

# ----------------------------------------------------------------
# Run models
# ----------------------------------------------------------------

# Init storage
rolling_micro  = []
rolling_macro  = []
size_training  = []
size_test      = []

# Define test set sizes to use 
step_wise_size = range(500,1500,50)

# Run rolling model
for j,i in enumerate(rep(10,9)):

	print '# ------------------------'
	print 'Started iteration ' + str(j)
	print '# ------------------------'

	N    = X.shape[0]
	ind  = random.sample(range(N), N)
	size = i/float(100)
	
	training_ind = ind[:int(math.ceil(size*N))]
	test_ind     = ind[int(math.ceil(size*N)):int(math.ceil(size*N))+step_wise_size[j]]

	size_training.append(len(training_ind))
	size_test.append(len(test_ind))

	X_training   = X.ix[training_ind,:]
	Y_training   = Y.ix[training_ind,:]

	X_test = X.ix[test_ind,:]
	Y_test = Y.ix[test_ind,:]

	# Train multi-label classifier
	classifier = OneVsRestClassifier(RandomForestClassifier(n_estimators = 100))
	classifier.fit(X_training, Y_training)

	# Predict multi labels
	predictions = classifier.predict(X_test)

	# Compute micro and macro F1-Score
	micro = f1_score(Y_test,predictions,average='micro')
	macro = f1_score(Y_test,predictions,average='macro')

	print '# ------------------------'
	print 'Finished iteration ' + str(j)
	print '# ------------------------'
	print 'Results:' 
	print 'Micro: ' + str(micro)
	print 'Macro: ' + str(macro)

	rolling_micro.append(micro)
	rolling_macro.append(macro)

save('micro.txt',rolling_micro)
save('macro.txt',rolling_macro)
save('size_training.txt',size_training)
save('size_test.txt',size_test)

# ----------------------------------------------------------------
# END CODE
# ----------------------------------------------------------------