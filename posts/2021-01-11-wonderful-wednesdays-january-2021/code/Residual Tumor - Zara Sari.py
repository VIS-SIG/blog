#!/usr/bin/env python
# coding: utf-8

# In[1]:


###### #Following instructions in DataCamp course:
 
#https://campus.datacamp.com/courses/generalized-linear-models-in-python/modeling-binary-data?ex=15
#https://towardsdatascience.com/a-quick-guide-on-descriptive-statistics-using-pandas-and-seaborn-2aadc7395f32
#https://github.com/VIS-SIG/Wonderful-Wednesdays/tree/master/data/2020/2020-12-09


#Import libraries
 
import statsmodels.api as sm #Array based model
 
from statsmodels.formula.api import glm
 
import numpy as np
import matplotlib.pyplot as plt

#from lifelines import KaplanMeierFitter

from sklearn.ensemble import RandomForestClassifier


# In[2]:


import seaborn as sns


# In[3]:


import pandas as pd


# In[4]:


import os
print(os.getcwd()) #/Users/zahraSari


# In[23]:



cwd=os.chdir('/Users/zahraSari/Desktop/') 

#Change directory

Files = os.listdir(cwd) 


# In[24]:


#Data from Github : 
#https://github.com/VIS-SIG/Wonderful-Wednesdays/blob/master/data/2020/2020-12-09/Reexcision.csv    

data= pd.read_csv('Book.csv')

print(data)
 
#age
#tumorsize
#histology (hist; 0: others, 1: Invasive-duct./ductal-lob.)
#Multifocality (mult.foc; 0: no, 1: yes)
#Accomp. in situ (acc.in.situ; 0: others, 1: DCIS and LCIS)
#Lymphovascular invasion (lymph.inv; 0: no, 1: yes)
#Estrogen-receptor (estr.rec; 0: no, 1: yes)
#Progesterone-receptor (prog.rec; 0: no, 1: yes)


# In[25]:



print(data.columns.tolist()) #View the column names
 


# In[26]:


data.info() # Tumor size has 9 unknown variables
 


# In[27]:


data.mean()


# In[28]:


total_rows=len(data.axes[0])
total_cols=len(data.axes[1])
print("Number of Rows: "+str(total_rows))
print("Number of Columns: "+str(total_cols))


# In[29]:



data['tumorsize']= pd.to_numeric(data['tumorsize'])


# In[30]:


data=data.dropna() #Removing the 9 unknown variables from Data Frame
 


# In[31]:


data.mean()


# In[32]:



# Plot the age variable
sns.distplot(data['age'])

plt.axvline(np.median(data['age']),color='b', linestyle='--') 

plt.axvline(np.mean(data['age']),color='b', linestyle='-') 

#Display the plot
plt.show()
 
data.age.mean()


# In[33]:



# Plot first variable

sns.distplot(data['tumorsize'])
 
plt.axvline(np.median(data['tumorsize']),color='b', linestyle='--') 

plt.axvline(np.mean(data['tumorsize']),color='b', linestyle='-') 
    
    
# Display the

plt.show()

data.tumorsize.mean()
 


# In[34]:


#Interpretation: People with re-exision 

g = sns.FacetGrid(data, col='RE')
g.map(plt.hist, 'tumorsize', bins=20)


# In[35]:



g = sns.FacetGrid(data, col='RE')
g.map(plt.hist, 'age', bins=20)


# # Pivoting Features

# In[57]:


data['hist'].value_counts().sort_values()


# In[58]:


sns.countplot(x='hist',data=data,palette='hls')
plt.show()


# In[56]:


sns.countplot(x='RE',data=data,palette='hls')
plt.show()


# In[48]:



# Summary Chart Re-excision

ax = ((100 *data["RE"].value_counts() / len(data))).plot.bar(rot=0)
ax.set( ylabel="%", title="Re-excision")

plt.show()

# Summary Chart hist

ax = ((100 *data["hist"].value_counts() / len(data))).plot.bar(rot=0)
ax.set( ylabel="%", title="hist")

plt.show()

# Summary Chart multfoc

ax = ((100 *data["multfoc"].value_counts() / len(data))).plot.bar(rot=0)
ax.set( ylabel="%", title="multfoc")

plt.show()


# In[38]:



data.groupby(['RE']).mean() #Mean of variables for RE of 0 or 1


# In[39]:



#Interpretation: Age distribution for patinets who had Re-excision is lower than those with 

get_ipython().run_line_magic('matplotlib', 'inline')

sns.set(style="whitegrid")
plt.figure(figsize=(10,8))
ax = sns.boxplot( x='RE',y='age', data=data, orient="v")


# In[40]:



get_ipython().run_line_magic('matplotlib', 'inline')

sns.set(style="whitegrid")

plt.figure(figsize=(10,8))
ax = sns.boxplot(y='tumorsize' , x='RE', data=data, orient="v")

#The box plot shows you how a feature's values spread out for each class. 
#It's a compact representation of the distribution, showing the extreme high value, 
#upper quartile, median, lower quartile and extreme low value.


# In[41]:


df = df.convert_objects(convert_numeric=True)    
sub_df = df.groupby(['RECL_LCC','RECL_PI'])['COUNT'].sum().unstack()
sub_df.plot(kind='bar',stacked=True)


# # Correlating categorical features

# In[42]:



grid = sns.FacetGrid(data, row='accinsitu', size=2.2, aspect=1.6)
grid.map(sns.pointplot, 'hist', 'RE' , 'lymphinv', palette='deep')
grid.add_legend()


# In[43]:


print(data.corr)


# # Correlation

# In[44]:


####Correlation Plot#######

corr=data.corr()

# Generate a mask for the upper triangle
mask = np.zeros_like(corr, dtype=np.bool)
mask[np.triu_indices_from(mask)] = True

# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(15, 13))

# Generate a custom diverging colormap
cmap = sns.diverging_palette(220, 10, as_cmap=True)

# Draw the heatmap with the mask and correct aspect ratio
sns.heatmap(corr, mask=mask, cmap=cmap, vmax=.3, center=0,
            square=True, linewidths=.5, cbar_kws={"shrink": .5})


# # Statistical Model - Logistic Model

# In[ ]:



#########Statistical Model############

#Fit logistic regression model
#Logistic regression is an improved version of linear regression.


# In[17]:



model = sm.GLM.from_formula("RE ~ hist + age + tumorsize + hist + multfoc + accinsitu + lymphinv + estrrec + progrec ", family = sm.families.Binomial(), data=data)
result = model.fit()
result.summary()

#Based on p-value being less than 0.05, 
#Significant variables are: hist, tumorsize, accinsitu, lymphinv
#Age is very close to 0.05 so speculuative whether it is significant, 
#Similarly for Intercept, p-value is very close to 0.05 but doesn't pass


#Coef for hist: (Thinking of linear regression formula Y = AX + B) 
#where A= -1.2014 , B is 0 since it is non-significant. 
#If a person’s hist is 1 unit more s/he will have a 0.052 (coefficient with age in the table above) unit more 
#chance of having heart disease based on the p-value in the table.

#Generally, positive coefficients indicate that the event becomes more likely as the predictor increases. 
#Negative coefficients indicate that the event becomes less likely as the predictor increases.


# In[67]:


#Removing non-significant variables and re-fitting the model
#Age seems to be significant now

model = sm.GLM.from_formula("RE ~ hist + age + tumorsize + accinsitu + lymphinv  -1 ", family = sm.families.Binomial(), data=data)
result = model.fit()
result.summary()

#coeffiecnt -1.2849 for hist shows increase of odds
#for ones with hist=1 than ones with hist=0

#According to this fitted model, older people are more
#likely to have Reexicision than younger people. The 
#log odds for heart disease increases by 0.0545 units for each year.
#If a person is 10 years older his or her chance of having RE
#increases by 0.0545 * 10 = 0.545 units.


# In[68]:



data[['RE','hist', 'age' , 'tumorsize' , 'accinsitu' , 'lymphinv']].corr()


# # Visualization of the Fitted Model

# In[91]:


#https://towardsdatascience.com/logistic-regression-model-fitting-and-finding-the-correlation-p-value-z-score-confidence-8330fb86db19
#With help from this site


# In[92]:


from statsmodels.sandbox.predict_functional import predict_functional


# In[104]:


values = {"hist": 0, "tumorsize": 50, "accinsitu":0 , "lymphinv" :0 }


# In[105]:


pr, cb, fv = predict_functional(result, "age", values=values, ci_method="simultaneous")


# In[106]:


ax = sns.lineplot(fv, pr, lw=4)
ax.fill_between(fv, cb[:, 0], cb[:, 1], color='grey', alpha=0.4)
ax.set_xlabel("age")
ax.set_ylabel("Re-excision")

ax.set_title('Fitted Model: Log-odd probability of Age by Re-excision')

#This plot of fitted log-odds  visualizes the effect of age on reexcision for 
#hist=0, tumorsize=23, accinsitu=0 and lumphinv=0 by the glm fitted model
#Slight negative correlation of age and RE are visible in this plot
#For the specific described variables


# In[ ]:





# In[100]:


from statsmodels.sandbox.predict_functional import predict_functional
values = {"hist": 0, "age": 45, "accinsitu":0 , "lymphinv" :0 }
pr, cb, fv = predict_functional(result, "tumorsize", values=values, ci_method="simultaneous")

ax = sns.lineplot(fv, pr, lw=4)
ax.fill_between(fv, cb[:, 0], cb[:, 1], color='grey', alpha=0.4)
ax.set_xlabel("Tumor Size")
ax.set_ylabel("Re-excision")

ax.set_title('Fitted Model: Log-odd probability of Tumorsize by Re-excision')

#This plot of fitted log-odds  visualizes the effect of tumorsize on reexcision for 
#hist=0, age=45, accinsitu=0 and lumphinv=0 by the glm fitted model
#Clear Positive correlation of tumorsize and RE are visible in this plot


# In[ ]:





# In[ ]:





# In[103]:



import seaborn as sns
 
#Plot the relationship between two variables in a DataFrame and 
#add overlay with the logistic fit


sns.regplot(x = 'tumorsize', y = 'RE',
            y_jitter = 0.03,
            data = data,
            logistic = True,
            ci = 95)

plt.title('Fitted Plot for Re-excision vs. Tumorsize')

# Display the plot
plt.show()

#Interpretation: the lower tumor sizes are associated with value 0 for Reexcision, 
#higher values of tumorsize.
#Tumor sizes of over 55 are associated with value of 1 for Reexicision.
#The confidence interval gets wider as the value of the predictor increases. The 
#wide interval is partly due to the small amount of data for larger tumor size.


# In[101]:


#Plot the relationship between two variables in a DataFrame and add overlay with the logistic fit

sns.regplot(x = 'age', y = 'RE',
            y_jitter = 0.03,
            data = data,
            logistic = True,
            ci = 95)
 
# Display the plot
plt.show()

#Interpretation: the lower values age is associated with value 0 for Reexcision, higher values of tumorsize
#eg. tumor size of over 55 are associated with value of 1 for Reexicision.
#The confidence interval gets wider as the value of the predictor increases. The 
#wide interval is partly due to the small amount of data for larger lower and higher ages.


# In[ ]:


# Compute predictions for the test sample df and save as prediction
prediction = model_fit.predict(exog = data)


# In[ ]:


# Add prediction to the existing data frame df and assign column name prediction
data['prediction'] = prediction


# In[ ]:


# Examine the first 5 computed predictions
print(data[['RE',  'hist', 'multfoc', 'accinsitu', 'lymphinv', 'estrrec', 'progrec']].head())


# In[ ]:



# Define the cutoff
cutoff = 0.5

# Compute class predictions: y_prediction
y_prediction = np.where(prediction > cutoff, 1, 0)


# # Machine Learning

# In[ ]:


#Random Forest


# In[ ]:



from sklearn.model_selection import train_test_split


# In[ ]:



y=data['RE']


# In[ ]:



train_df = data.drop(['RE'], axis=1)


# In[ ]:


import numpy as np

from sklearn.model_selection import train_test_split
X, y = np.arange(10).reshape((5, 2)), range(5)
X
list(y)


# In[ ]:


X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.33, random_state=42)
X_train


# In[ ]:


y_train


# In[ ]:


X_test


# In[ ]:


y_test


# In[ ]:



#print("Before", train_df.shape, test_df.shape, combine[0].shape, combine[1].shape)

#train_df = train_df.drop(['Ticket', 'Cabin'], axis=1)
#test_df = test_df.drop(['Ticket', 'Cabin'], axis=1)
#combine = [train_df, test_df]

#"After", train_df.shape, test_df.shape, combine[0].shape, combine[1].shape


# In[ ]:



train_df = data.drop(['RE'], axis=1)


# In[ ]:


df = pd.DataFrame(np.random.randn(100, 2))

msk = np.random.rand(len(df)) < 0.8

train = df[msk]

test = df[~msk]

len(test)
len(train)


# In[ ]:


#####################DATACAMP#################

# Import train_test_split function
#from sklearn.model_selection import train_test_split

#X=data[['sepal length', 'sepal width', 'petal length', 'petal width']]  # Features
#y=data['species']  # Labels

# Split dataset into training set and test set
#X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3) # 70% training and 30% test




# Import train_test_split function
from sklearn.model_selection import train_test_split

X=data[['age' , 'tumorsize' , 'hist',  'multfoc' , 'accinsitu',  'lymphinv',  'estrrec' , 'progrec']]  # Features
y=data['RE']  # Labels

# Split dataset into training set and test set
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3) # 70% training and 30% test



# In[ ]:





# In[ ]:




#Import Random Forest Model
from sklearn.ensemble import RandomForestClassifier

#Create a Gaussian Classifier
clf=RandomForestClassifier(n_estimators=100)

#Train the model using the training sets y_pred=clf.predict(X_test)
clf.fit(X_train,y_train)

y_pred=clf.predict(X_test)


# In[ ]:



#Import scikit-learn metrics module for accuracy calculation
from sklearn import metrics
# Model Accuracy, how often is the classifier correct?
print("Accuracy:",metrics.accuracy_score(y_test, y_pred))


# In[ ]:



RandomForestClassifier(bootstrap=True, class_weight=None, criterion='gini',
            max_depth=None, max_features='auto', max_leaf_nodes=None,
            min_impurity_decrease=0.0, min_impurity_split=None,
            min_samples_leaf=1, min_samples_split=2,
            min_weight_fraction_leaf=0.0, n_estimators=100, n_jobs=1,
            oob_score=False, random_state=None, verbose=0,
            warm_start=False)


# In[ ]:




