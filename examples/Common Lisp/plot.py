import seaborn as sns
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("~/quicklisp/local-projects/HEMS/examples/Common Lisp/model-scores.csv")

df['POPULATION_ID'] = df['POPULATION_ID'].astype(str)
df1 = df.drop('OBSERVATION_ID', axis=1).reset_index(drop=True)
sns.barplot(df1, x="POPULATION_ID", y="SCORE").set(title="Model 1 Average Scores per Population")
plt.show()

g = sns.catplot(kind='bar', data=df, x='OBSERVATION_ID', y="SCORE", row='POPULATION_ID', color="g")
g.set(xticks=[0, 99, 199, 299])
plt.subplots_adjust(hspace=0.5, top=.9)
g.fig.suptitle("Model 1 Observation Scores by Population")
plt.show()
