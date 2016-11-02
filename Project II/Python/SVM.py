# ----------------------------------------------------------------
# Load Modules
# ----------------------------------------------------------------

import os
import json
import math
import numpy  as np
import pandas as pd
import random
from sklearn.svm import SVC
from sklearn.ensemble   import RandomForestClassifier
from sklearn.metrics    import f1_score
from sklearn.multiclass import OneVsRestClassifier
from sklearn.ensemble   import AdaBoostClassifier

# ----------------------------------------------------------------
# Load Data
# ----------------------------------------------------------------

os.chdir('CHANGE ME --- CHANGE ME --- CHANGE ME')
X = pd.read_csv('mod_matrix.csv', header=None)
Y = pd.read_csv('labels.csv', header=None)

# ----------------------------------------------------------------
# Run models
# ----------------------------------------------------------------

np.random.seed(123)

N    = X.shape[0]
ind  = random.sample(range(N), N)
size = 0.10

training_ind = ind[:int(math.ceil(size*N))]
test_ind     = ind[int(math.ceil(size*N)):]

X_training   = X.ix[training_ind,:]
Y_training   = Y.ix[training_ind,:]

X_test = X.ix[test_ind,:]
Y_test = Y.ix[test_ind,:]

# Train multi-label classifier
classifier = OneVsRestClassifier(SVC(C = 1.0, kernel='linear'))
classifier.fit(X_training, Y_training)

# Predict multi labels
predictions = classifier.predict(X_test)

# Compute micro and macro F1-Score
micro = f1_score(Y_test,predictions,average='micro')
macro = f1_score(Y_test,predictions,average='macro')

print 'Results:' 
print 'Micro: ' + str(micro)
print 'Macro: ' + str(macro)

# ----------------------------------------------------------------
# END CODE
# ----------------------------------------------------------------