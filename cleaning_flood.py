#### Cleaning the data
import pandas as pd

# Import data file
file = pd.ExcelFile('GlobalFloodsRecord.xls')
df = file.parse('MasterTable')

# Drop the replicated columns and useless columns 
df.drop(df.columns[30:], axis=1, inplace=True)
df.drop(df.columns[[1, 2, 4, 5, 6, 7, 14, 26, 29]], axis=1, inplace=True)

## rename the some of columns 
df.rename(columns={'Register #': 'register_number', 'Validation (post event #3503)': 'validation',  'Duration in Days': 'duration',  }, inplace=True)

# write to csv file 
df.to_csv('../cleaned_flood.csv',encoding='utf-8')


