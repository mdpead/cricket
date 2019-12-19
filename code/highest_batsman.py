import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup


def download(first_page, last_page):

    dfs = []
    for i in range(first_page, last_page):
        url = f'http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;orderby=start;page={i};template=results' \
              f';type=batting;view=innings;wrappertype=print'
        page = requests.get(url)
        html_table = BeautifulSoup(page.text, 'html.parser').find_all('table', class_='engineTable')[2]
        df = pd.read_html(str(html_table), header=0)[0]
        df = df.assign(Page = i)
        dfs.append(df)
    ds = pd.concat(dfs, axis = 0)
    
    return(ds)


def clean(df):
    
    clean = df \
      .drop(columns = df.columns[8]) \
      .assign(mt_start_date = df['Start Date'].astype('datetime64[ns]')) \
      .assign(mt_ground = df['Ground']) \
      .assign(mt_format = df['Opposition'].str.extract(r'^(.*?)\sv')) \
      .assign(mt_team_code = df['Player'].str.extract(r'(?<=\()(.+?)(?=\))')) \
      .assign(mt_opposition = df['Opposition'].str.extract(r'v\s(.*)')) \
      .assign(in_no = df['Inns'].astype('int')) \
      .assign(pl_name = df['Player'].str.extract(r'^(.*?)\s\(')) \
      .assign(pf_runs = np.where(df['Runs'] == 'DNB', 0, df['Runs'].str.replace(r'\*', '')).astype('int')) \
      .assign(pf_balls = np.where(df['Runs'] == 'DNB', 0, np.where(df['BF'] == '-', np.nan, df['BF'])).astype('float')) \
      .assign(pf_mins = np.where(df['Runs'] == 'DNB', 0, np.where(df['Mins'] == '-', np.nan, df['Mins'])).astype('float')) \
      .assign(pf_fours = np.where(df['Runs'] == 'DNB', 0, np.where(df['4s'] == '-', np.nan, df['4s'])).astype('float')) \
      .assign(pf_sixes = np.where(df['Runs'] == 'DNB', 0, np.where(df['6s'] == '-', np.nan, df['6s'])).astype('float')) \
      .assign(pf_no = np.where(df['Runs'].str.contains('*', regex=False), True, False).astype('bool')) \
      .assign(pf_dnb = np.where(df['Runs'] == 'DNB', True, False).astype('bool')) \
      .drop(['Player', 'Runs', 'Mins', 'BF', '4s', '6s', 'SR', 'Inns', 'Opposition', 'Ground', 'Start Date', 'Page'],
            axis = 1)
    
    return(clean)

