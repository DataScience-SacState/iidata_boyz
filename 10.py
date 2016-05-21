import pandas as pd

data['weekday'] = data['tpep_dropoff_datetime'].apply(lambda x: x.weekday())


