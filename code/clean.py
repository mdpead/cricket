import pandas as pd
import numpy as np

df = pd.read_pickle('..\\data\\batting_history_raw.pkl')


clean = df \
  .drop(columns=df.columns[8]) \
  .assign(mt_start_date=df['Start Date'].astype('datetime64[ns]')) \
  .assign(mt_ground=df['Ground']) \
  .assign(mt_format=df['Opposition'].str.extract(r'^(.*?)\sv')) \
  .assign(mt_team_code=df['Player'].str.extract(r'(?<=\()(.+?)(?=\))')) \
  .assign(mt_opposition=df['Opposition'].str.extract(r'v\s(.*)')) \
  .assign(in_no=df['Inns'].astype('int')) \
  .assign(pl_name=df['Player'].str.extract(r'^(.*?)\s\(')) \
  .assign(pf_runs=np.where(df['Runs'] == 'DNB', 0, df['Runs'].str.replace(r'\*', '')).astype('int')) \
  .assign(pf_balls=np.where(df['Runs'] == 'DNB', 0, np.where(df['BF'] == '-', np.nan, df['BF'])).astype('float')) \
  .assign(pf_mins=np.where(df['Runs'] == 'DNB', 0, np.where(df['Mins'] == '-', np.nan, df['Mins'])).astype('float')) \
  .assign(pf_fours=np.where(df['Runs'] == 'DNB', 0, np.where(df['4s'] == '-', np.nan, df['4s'])).astype('float')) \
  .assign(pf_sixes=np.where(df['Runs'] == 'DNB', 0, np.where(df['6s'] == '-', np.nan, df['6s'])).astype('float')) \
  .assign(pf_no=np.where(df['Runs'].str.contains('*', regex=False), True, False).astype('bool')) \
  .assign(pf_dnb=np.where(df['Runs'] == 'DNB', True, False).astype('bool')) \
  .drop(['Player', 'Runs', 'Mins', 'BF', '4s', '6s', 'SR', 'Inns', 'Opposition', 'Ground', 'Start Date', 'Page'],
        axis=1)

clean.to_pickle('..\\data\\batting_history_clean.pkl')
