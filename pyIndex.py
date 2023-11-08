print('lets start')

import os
import pandas as pd
from datetime import datetime



# Set working directory
os.chdir("/home/uarik/Documents/RProjects/DataAir")
# Display working directory
print(os.getcwd())

# suppress_messages is somewhat equivalent to Python's 'warnings.filterwarnings'
# Read CSV file without column names
jobdata = pd.read_csv("ECOCITY_Archive_377_40_2019-02-20_2023-09-23.csv", header=None)

# Get column names
print(jobdata.columns)
