import pandas as pd
import numpy as np
import pingouin as pg
import seaborn as sns

# Let's assume that we have a balanced design with 30 students in each group
n = 30
months = ['August', 'January', 'June']

# Generate random data
np.random.seed(1234)
control = np.random.normal(5.5, size=len(months) * n)
meditation = np.r_[np.random.normal(5.4, size=n),
                   np.random.normal(5.8, size=n),
                   np.random.normal(6.4, size=n)]

# Create a dataframe
df = pd.DataFrame({'Scores': np.r_[control, meditation],
                   'Time': np.r_[np.repeat(months, n), np.repeat(months, n)],
                   'Group': np.repeat(['Control', 'Meditation'], len(months) * n),
                   'Subject': np.r_[np.tile(np.arange(n), 3),
                                    np.tile(np.arange(n, n + n), 3)]})
# DESCRIPTIVE STATS
pg.print_table(df.head())

# import seaborn as sns
sns.set()
sns.pointplot(data=df, x='Time', y='Scores', hue='Group', dodge=True,
              markers=['o', 's'], capsize=.1, errwidth=1, palette='colorblind')

print(df.groupby(['Time', 'Group']).agg(['mean', 'std']))

# ANOVA
aov = pg.mixed_anova(dv='Scores', within='Time', between='Group',
                     subject='Subject', data=df)
pg.print_table(aov)

# POST-HOC TESTS
posthocs = pg.pairwise_ttests(dv='Scores', within='Time', between='Group',
                              subject='Subject', data=df)
pg.print_table(posthocs)
