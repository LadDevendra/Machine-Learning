
# coding: utf-8

# In[31]:

import pandas as pd # data analysis library
import numpy as np # numerical & matrix computation library
import seaborn as sns # visualization library
from biokit.viz import corrplot # visualization library
import matplotlib.pyplot as plt # visualization library
from random import choice

# non-classifier sklearn modules to import
from sklearn.preprocessing import Imputer, LabelEncoder
from sklearn.model_selection import GridSearchCV, train_test_split, cross_val_predict
from sklearn.metrics import accuracy_score

# sklearn classifiers to import
from sklearn.ensemble import RandomForestClassifier, BaggingClassifier, AdaBoostClassifier
from sklearn.linear_model import LogisticRegression, Perceptron
from sklearn.tree import DecisionTreeClassifier
from sklearn.svm import SVC
from sklearn.neural_network import MLPClassifier
from sklearn.naive_bayes import GaussianNB, BernoulliNB, MultinomialNB 
from sklearn.neighbors import KNeighborsClassifier

from itertools import chain, combinations
from IPython.display import display


# In[2]:

# load data (kaggle testing sets don't include lables in downloadable dataset)
data = pd.read_csv('../Data/train.csv')

# split into training and testing sets
train, test = train_test_split(data, test_size=0.2)


# In[3]:

train.head()


# In[4]:

train.shape


# In[5]:

test.shape


# In[6]:

# see which columns have NaN values
print(train.isnull().sum())
print()
print(test.isnull().sum())


# In[7]:

sns.pairplot(train.drop(['Age', 'Cabin', 'Embarked'], axis=1))
sns.plt.show()


# In[8]:

# Visualize Correlations 
corrs = train.drop(['Cabin'], axis=1).corr()
c = corrplot.Corrplot(corrs)
c.plot(method='pie', shrink=.9, grid=False)
plt.show()


# In[9]:

# look at age values to get an idea for value imputation
train.Age.value_counts()


# In[10]:

ages_train = train.Age
ages_test = test.Age


# In[11]:

# initialize imputer to replace missing age values using the most frequent strategy
imputer = Imputer(missing_values='NaN', strategy='most_frequent', axis=0)


# In[12]:

ages_train = imputer.fit_transform(ages_train.values.reshape(-1, 1))
ages_test = imputer.fit_transform(ages_test.values.reshape(-1, 1))


# In[13]:

train.Age = ages_train
test.Age = ages_test


# In[14]:

# see which columns now have NaN values
print(train.isnull().sum())
print()
print(test.isnull().sum())


# In[15]:

# find indexes of null values for embarked
nan_indexes = np.where(pd.isnull(train.Embarked))
nan_indexes


# In[16]:

# get index of Embarked
embarked_idx = train.columns.get_loc('Embarked')
embarked_idx


# In[17]:

# make sure we have the correct location
for idx in nan_indexes:
    print(train.iloc[idx, embarked_idx])


# In[18]:

# replace NaN with random choice from Embarked options
# first we need to see what those options are
embarked_choices = train.Embarked.value_counts().keys()
for idx in nan_indexes:
    train.iloc[idx, embarked_idx] = choice(embarked_choices)


# In[19]:

# check to see that the NaN embarked values were replaced
print(train.isnull().sum())


# In[20]:

# Need to encode categorical string features to numerical values in training and testing set
categorical = ['Sex', 'Embarked']
enc = LabelEncoder()
for category in categorical:
    train[category] = enc.fit_transform(train[category])
    test[category] = enc.fit_transform(test[category])


# In[21]:

# check that encoding worked
train.head()


# In[22]:

test.head()


# In[23]:

# remove unwanted columns from features
features = train.columns.values
idx = np.argwhere(features == 'Cabin')
jdx = np.argwhere(features == 'Name')
kdx = np.argwhere(features == 'Ticket')
ldx = np.argwhere(features == 'Survived')
features = np.delete(features, [idx, jdx, kdx, ldx])
target = 'Survived'


# In[24]:

# set up parameter grids to use with exhaustive grid search
# in order to find the best combination of parameters for the classifiers
random_forest_params = {
    'n_estimators': [10, 12, 15], 'criterion': ('gini', 'entropy'),
    'bootstrap': (True, False), 'class_weight': ('balanced', None)
}

decision_tree_params = {
    'criterion': ('gini', 'entropy'), 'splitter': ('best', 'random'),
    'class_weight': ('balanced', None), 'presort': (False, True)
}

perceptron_params = {
    'penalty': [None, 'l2', 'l1', 'elasticnet'], 'shuffle': [False, True],
    'class_weight': ['balanced', None]
}

'''
svm_params = {
    'kernel': ['linear', 'rbf'],
    'shrinking': [False, True], 'class_weight': ['balanced', None]
}
'''

neural_net_params = {
    'activation': ['identity', 'logistic', 'tanh', 'relu'],
    'solver': ['adam'],
    'learning_rate': ['constant', 'invscaling', 'adaptive']
}

log_reg_params = {
    'class_weight': ['balanced', None],
    'solver': ['newton-cg', 'lbfgs', 'liblinear']
}

knn_params = {
    'n_neighbors': [5, 10, 12], 'weights': ('uniform', 'distance'),
    'algorithm': ['auto', 'ball_tree', 'kd_tree', 'brute']
}

bagging_params = {
    'n_estimators': [10, 12, 15], 'bootstrap': [False, True]
}

ada_boost_params = {
    'n_estimators': [50, 75, 100], 'algorithm': ['SAMME', 'SAMME.R']
}

params = [
    random_forest_params, decision_tree_params, perceptron_params, 
    neural_net_params, log_reg_params, knn_params,
    bagging_params, ada_boost_params
]


# In[25]:

# classifiers to test
classifiers = [
    RandomForestClassifier(), DecisionTreeClassifier(), Perceptron(), 
    MLPClassifier(), LogisticRegression(), 
    KNeighborsClassifier(), BaggingClassifier(), AdaBoostClassifier()
]

names = [
    'RandomForest', 'DecisionTree', 'Perceptron', 
    'NeuralNetwork', 'LogisticRegression',
    'KNearestNeighbors', 'Bagging', 'AdaBoost'
]

models = dict(zip(names, zip(classifiers, params)))
models


# In[36]:

# Helper functions 

def find_best_model_with_param_tuning(models, train, test):
    '''
    Uses grid search to find the best accuracy
    for a model by finding the best set of values
    for the parameters the model accepts
    '''
    accuracies = []
    # dataframe to store intermediate results
    dataframes = []
    for name, clf_and_params in models.items():
        print('Performing GridSearch on {}-classfier'.format(name))
        clf, clf_params = clf_and_params
        grid_clf = GridSearchCV(estimator=clf, param_grid=clf_params)
        grid_clf = grid_clf.fit(train[features], train[target])
        dataframes.append((name, grid_clf.cv_results_))
        predictions = grid_clf.predict(test[features])
        accuracy = accuracy_score(test[target], predictions)
        accuracies.append((name, accuracy))
    return accuracies, dataframes


def generate_powerset(items):
    '''
    Generates the powerset of a group of items, 
    in this case the features available to a model
    '''
    p = list(items)
    return chain.from_iterable(combinations(p, r) for r in range(len(p)+1))


def get_best_feature_group_and_acc(classifier, features, train, test):
    '''
    Using the powerset of a group of features, finds
    the best combination of features for a model
    that produces the best accuracy 
    '''
    accs = []
    name, clf = classifier
    print('Performing feature find on {}-classifier'.format(name))
    for feature_group in generate_powerset(features):
        if len(feature_group) > 0:
            f = list(feature_group)
            #clf = classifier()
            clf = clf.fit(train[f], train[target])
            preds = clf.predict(test[f])
            acc = accuracy_score(test[target], preds)
            accs.append((name, acc, f))
    return max(accs)


# In[37]:

results, dataframes = find_best_model_with_param_tuning(models, train, test)
print()
for classifier, acc in results:
    print('Classifier = {}: Accuracy = {}'.format(classifier, acc))


# In[42]:

# total results (final & intermediate) from runnining
# exhuastive grid search on each classifier
for name, df in dataframes:
    print('============================================================')
    print('{}-classifier GridSearch Total Results'.format(name))
    print('============================================================')
    display(df)
    print()


# In[28]:

feature_accs = []
for classifier in zip(names, classifiers):
    acc = get_best_feature_group_and_acc(classifier, features, train, test)
    feature_accs.append(acc)
print()
for name, accs, ftrs in feature_accs:
    print('Classifier = {}, Accuracy = {}, Features = {}'.format(name, accs, features))


# In[40]:

# Perform cross validation with feature groups discovered above

scores = []
feature_classifier_map = dict(zip(names, classifiers))
for name, _, ftrs in feature_accs:
    print('Performing Cross-Validation on {}-classifier'.format(name))
    clf = feature_classifier_map[name]
    predictions = cross_val_predict(clf, test[ftrs], test[target], cv=10)
    score = accuracy_score(test[target], predictions)
    scores.append((name, score))
print()

for name, scr in scores:
    print('Classifier-{}: Accuracy = {}'.format(name, scr))


# In[46]:

# try Naive Bayes

nb_clf = GaussianNB()
nb_clf = nb_clf.fit(train[features], train[target])
preds = nb_clf.predict(test[features])
acc = accuracy_score(test[target], preds)
print('Naive-Bayes-classifier: Accuracy = {}'.format(acc))

