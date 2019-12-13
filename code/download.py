import requests
import pandas as pd
from bs4 import BeautifulSoup

dfs = []
for i in range(1, 5):
    url = f'http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;orderby=start;page={i};template=results' \
          f';type=batting;view=innings;wrappertype=print'
    page = requests.get(url)
    html_table = BeautifulSoup(page.text, 'html.parser').find_all('table', class_='engineTable')[2]
    df = pd.read_html(str(html_table), header=0)[0]
    df = df.assign(Page=i)
    dfs.append(df)

ds = pd.concat(dfs, axis=0)

ds.to_pickle('..\\data\\batting_history_raw.pkl')
