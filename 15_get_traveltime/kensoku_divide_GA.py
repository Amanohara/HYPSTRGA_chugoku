import math
import sys
from typing import Optional
from pathlib import Path
import pandas as pd
import numpy as np
import datetime
from cut_kensoku import cut

"""
# 検測値データから震源レコードと検測値レコードを分割する
# 1.データの1行目だけを抜き出して震源レコードの分割をする。欲しいのは地震発震時、緯度、経度、深さ、マグニチュード
# 2.2行目以降を抜き出して検測値レコードを分割する。欲しいのは各観測点名、P波到達時間、S波到達時間
# 3.各波の到達時間から発震時をマイナスする。これで発震時から各観測点までの走時時間が分かる
"""

def main(kensoku_data) :
    # 震源レコードの分割
    df_hypo = cut(kensoku_data)
    # 検測値レコードの分割
    df_kensoku = cut_kensoku(kensoku_data)
    # print(df_hypo)
    # print(df_kensoku)
    # 発震時の引き算
    df_travel = hikzian_kensoku(df_hypo, df_kensoku)
    # print(df_travel)
    df_travel['wave1_traveltime'] = df_travel['wave1_traveltime'].dt.total_seconds()
    df_travel['wave2_traveltime'] = df_travel['wave2_traveltime'].dt.total_seconds()
    # df_travel.to_csv("hoge2.csv")
    # 欲しいのは各観測点名、P波到達時間、S波到達時間なので、
    df_travel2 = df_travel.loc[:, ["obs_name", "wave1_traveltime", "wave2_traveltime"]]
    df_travel2.insert(2, "score_P", "B")
    df_travel2.insert(4,"score_S", "C")
    df_travel2 = df_travel2.fillna({'wave2_traveltime': 99.99})
    df_travel2 = df_travel2[df_travel2["obs_name"].str.contains("N\.")]
    df_travel2.loc[df_travel2['wave2_traveltime'] >= 99.99, 'score_S'] = "*"
    df_travel2.insert(3, "wavename_P", "P")
    df_travel2.insert(6, "wavename_S", "S")
    df_travel2.to_csv(kensoku_data + "_travelGA.csv", index=None, float_format='%.2f')
    df_hypo.to_csv(kensoku_data + "_hypoGA.csv", index=None, na_rep='99.99', float_format='%.2f')

def hikzian_kensoku(dfhypo, dfkensoku) :
    # 引き算
    dfkensoku['wave1_traveltime'] = dfkensoku['wave1_datetime'] - dfhypo.iat[0,0]
    dfkensoku['wave2_traveltime'] = dfkensoku['wave2_datetime'] - dfhypo.iat[0,0]
    return dfkensoku

def cut_kensoku(data) :
    # 検測値レコードの分割
    colspecs = [(0, 7), (7, 11), (11, 12), (12, 13), (13, 15), (15, 19), (19, 27), (27, 31), (31, 37), (37, 42),
                (42, 45),
                (45, 48), (48, 53), (53, 56), (56, 59), (59, 64), (64, 67), (67, 70), (70, 71), (71, 72), (72, 75),
                (75, 76), (76, 79), (79, 80), (80, 83), (83, 84), (84, 87), (87, 89), (89, 91), (91, 92), (92, 93),
                (93, 94), (94, 95), (95, 96)]
    names = ["obs_name", "obs_number", "blank", "types", "hypo_day", "wave1_type", "wave1_date", "wave2_type",
             "wave2_date",
             "max_amplitudeNS", "amplitude_periodNS", "amplitude_timeNS", "max_amplitudeEW", "amplitude_periodEW",
             "amplitude_timeEW", "max_amplitudeUD", "amplitude_periodUD", "amplitude_timeUD", "amplitude_unit",
             "init_responseNS", "init_amplitudeNS", "init_responseEW", "init_amplitudeEW", "init_responseUD",
             "init_amplitudeUD", "init_unit", "duration", "year", "month", "flag1", "flag2", "flag3", "flag4",
             "weight"]
    converters = { 'wave1_date': str, 'wave2_date': str, 'obs_name': str, 'obs_number': int, 'hypo_day': int,
                   'year': int,
                   'month': int }

    df = pd.read_fwf(data, colspecs=colspecs, names=names, converters=converters, skiprows=1)

    df["obs_name"] = df["obs_name"].str[1:]

    df = df[df["obs_name"] != ""]
    df['wave1_datetime'] = df.apply(
        lambda f: create_full_datetime(f['year'], f['month'], f['hypo_day'], f["wave1_date"]), axis=1)

    df['wave2_datetime'] = df.apply(
        lambda f: create_wave2_datetime(f['wave2_date'], f['wave1_datetime']), axis=1)
    return df

def create_wave2_datetime(wave2_date, wave1_datetime: datetime.datetime) -> Optional[datetime.datetime]:
    """
    wave2_date からwave2のdatetimeを求める関数.
    :param wave2_date: str 分+秒 %M%S%f形式
    :param wave1_datetime: datetime.datetime wave1のdatetime
    :return: wave2_datetime: Optional[datetime] ない場合はNone
    """
    if not wave2_date or pd.isnull(wave2_date):
        return None

    wave2_date_str = str(wave2_date)

    wave2_hour = wave1_datetime.hour
    if int(wave1_datetime.minute) > int(wave2_date_str[0:2]):
        # 24時を超えたら戻す
        wave2_hour += 1
        if wave2_hour >= 24:
            wave2_hour -= 24

    wave2_time_str = str(wave2_hour).zfill(2) + wave2_date

    # wave1の年月日とwave2の時刻でdatetimeを生成
    wave2_datetime = create_full_datetime(int(str(wave1_datetime.year)[-2:]), wave1_datetime.month,
                                          wave1_datetime.day,
                                          wave2_time_str)

    if wave2_datetime < wave1_datetime:
        # wave2_datetime の方が古い場合、１日前にする
        wave2_datetime = wave2_datetime + datetime.timedelta(days=1)

    return wave2_datetime

def create_full_datetime(year, month, day, time_str):
    """
    年月日と時刻文字列からdatetimeを求める関数
    :param year: int
    :param month: int
    :param day: int
    :param time_str: str  %H%M%S%f形式
    :return: Optional[datetime]
    """
    full_year = "20" + str(year) if year < 51 else "19" + str(year)
    full_month = str(month).zfill(2)
    full_day = str(day).zfill(2)

    if not time_str or pd.isnull(time_str):
        return None

    datetime_str = full_year + full_month + full_day + str(time_str)

    return datetime.datetime.strptime(datetime_str, "%Y%m%d%H%M%S%f")

if __name__ == '__main__':
    args = sys.argv
    if Path(sys.argv[1]).exists():
        main(sys.argv[1])
    else:
        print("なにかがおかしいよ\npython3 kensoku_divide.py [検測値データ]")
