import seaborn as sns
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("~/quicklisp/local-projects/HEMS/examples/Common Lisp/model-scores.csv")

df['DATASET_ID'] = df['DATASET_ID'].astype(str)
df1 = df.drop('OBSERVATION_ID', axis=1).reset_index(drop=True)
sns.barplot(df1, x="DATASET_ID", y="SCORE").set(title="Model 1 Average Scores per Dataset")
plt.show()

g = sns.catplot(kind='bar', data=df, x='OBSERVATION_ID', y="SCORE", row='DATASET_ID', color="g")
g.set(xticks=[0, 99, 199, 299])
plt.subplots_adjust(hspace=0.5, top=.9)
g.fig.suptitle("Model 1 Observation Scores by Population")
plt.show()
