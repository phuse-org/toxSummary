from http.client import _DataType
import sqlite3
import pandas as pd
import datetime
import numpy as np

con = sqlite3.connect('C:/Users/Md.Ali/OneDrive - FDA/yousuf/10_DATA/SEND_unclean.db')

# cur = con.cursor()
# df = cur.execute('SELECT * FROM PP WHERE STUDYID ="8416388"')
# print(df)
# for row in cur.execute('SELECT * FROM PP WHERE STUDYID ="8416388"'):

# 	print(row)

st = datetime.datetime.now()
df = pd.read_sql('SELECT * FROM PP WHERE STUDYID ="2884-002"', con)
et = datetime.datetime.now()
elapsed = et-st
print("Execution timeL", elapsed, " second")

df.head()
df.columns


st = datetime.datetime.now()
df = pd.read_sql('SELECT STUDYID,USUBJID,POOLID,PPTESTCD FROM PP', con)
et = datetime.datetime.now()
elapsed = et-st
print("Execution timeL", elapsed, " second")

newst = df[df["STUDYID"]=="2293-004" ]

df_full = pd.read_sql('SELECT * FROM PP', con)
st = datetime.datetime.now()
dff = pd.read_sql('SELECT STUDYID, DOMAIN,USUBJID, POOLID,PPSEQ,PPGRPID,PPTESTCD,PPTEST,PPCAT,PPSCAT,PPORRESU,PPSTRESN,PPSTRESU,PPSTAT,PPSPEC,VISITDY,PPNOMDY,PPNOMLBL,PPRFTDTC,PPSTINT,PPENINT FROM PP', con)
et = datetime.datetime.now()
elapsed = et-st
print("Execution timeL", elapsed, " second")
dff.shape


dff = pd.read_sql_query('SELECT STUDYID, USUBJID, POOLID,PPSEQ,PPGRPID,PPTESTCD,PPTEST,PPCAT,PPSCAT, PPORRESU,PPSTRESN,PPSTRESU FROM PP',
 con,  dtype= {'STUDYID': np.str_ , 'USUBJID': np.str_})

################
query = 'SELECT STUDYID,DOMAIN,USUBJID,POOLID,PPSEQ,PPGRPID,PPTESTCD,PPTEST,PPCAT,PPSCAT,PPORRESU,PPSTRESN,PPSTRESU,PPSTAT,PPSPEC,VISITDY,PPNOMDY,PPNOMLBL,PPRFTDTC,PPSTINT,PPENINT FROM PP'

st = datetime.datetime.now()
dd = pd.read_sql_query(query, con)
et = datetime.datetime.now()
elapsed = et-st
print("Execution timeL", elapsed, " second")