from HinetPy import Client
import pandas as pd
import numpy as np
import sys
from pathlib import Path
from datetime import date

"""
検測値を取得する
"""

def main(username:str, password:str, year:int, month:int, day:int):
    client = Client(username, password)
    startdate = date(int(year), int(month), int(day))
    client.get_arrivaltime(startdate, 1)
    return 

if __name__ == '__main__':
    args = sys.argv
    if len(args) > 5 :
        main(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5])
    else:
        print("なにかがおかしいよ\npython3 kensoku_divide.py [検測値データ]")
