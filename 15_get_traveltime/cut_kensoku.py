import pandas as pd
import sys
from pathlib import Path

from datetime import datetime as dt


def cut(input_file):
    # input = args[1]
    # 固定長データに分割
    input = input_file
    df = kotei_hypo(input)
    # FAR FIELDは削除する
    df = df[df.hypo_locale != "FAR FIELD"]
    # 型を変更する
    df = convert(df)
    # 度分を直す
    df = degree(df)
    # マグニチュードを直す
    df = magnitude(df)
    # 秒を直す
    df = minute(df)
    # 深さを直す
    df = depth(df)

    # 出力
    df2 = df.round(4)
    df2 = df2.drop(columns=['latitude_min', 'longitude_min', 'mag1_2'])
    df3 = df2.loc[:, ["datetime", "longitude_deg", "latitude_deg", "depth",
                      "mag1_1"]]
    # df2.to_csv(str(input) + ".csv", index=None, header=None)
    # df2.to_csv(str(input) + ".csv", index=None)
    format_string = '%Y%m%d%H%M%S.%f'
    df3['datetime'] = pd.to_datetime(df3['datetime'], format=format_string)
    df3 = df3.astype(object).where(pd.notnull(df3), None)
    # df3.to_csv(str(input) + "_DB.csv", index=None)
    return df3


def kotei_hypo(hypo):
    # 固定長に分割する
    colspecs = [(0, 1), (1, 13), (13, 17), (17, 21), (21, 24), (24, 28), (28, 32), (32, 36), (36, 40), (40, 44),
                (44, 49), (49, 52), (52, 53), (53, 54),
                (54, 55), (55, 57), (57, 58), (58, 59), (59, 60), (60, 61), (61, 62), (62, 63), (63, 64), (64, 65),
                (65, 68), (68, 92), (92, 95), (95, 96)]
    names = ['recode', 'date', 'second', 'time_SE', 'latitude_deg', 'latitude_min', 'lat_SD', 'longitude_deg',
             'longitude_min', 'lon_SD', 'depth', 'dep_SD', 'mag1_1', 'mag1_2', 'mag1_type', 'mag2',
             'mag2_type', 'travel_time', 'hypo_judge', 'hypo_type', 'max_coefficient', 'scale_victim', 'tsunami_victim',
             'region_main', 'region_sub', 'hypo_locale', 'obs_number', 'determination_way']
    df = pd.read_fwf(hypo, colspecs=colspecs, names=names, nrows=1)
    return df


def convert(input):
    if input.latitude_deg.dtypes == 'O':
        input['latitude_deg'] = input['latitude_deg'].str.replace(' ', '')
    if input.longitude_deg.dtypes == 'O':
        input['longitude_deg'] = input['longitude_deg'].str.replace(' ', '')
    if input.depth.dtypes == 'O':
        input['depth'] = input['depth'].str.replace(' ', '')
    if input.hypo_judge.dtypes == 'O':
        input.replace({'hypo_judge': {'M': 10}}, inplace=True)
    input.hypo_judge = input.hypo_judge.astype("float64")
    input.latitude_deg = input.latitude_deg.astype("float64")
    input.latitude_min = input.latitude_min.astype("float64")
    input.longitude_deg = input.longitude_deg.astype("float64")
    input.longitude_min = input.longitude_min.astype("float64")
    input.depth = input.depth.astype("float64")
    return input


def degree(input):
    # 度分秒を度のみに直す
    # 震源固定の場合は小数点以下空白
    # 震源固定は震源評価9番。
    # mask はFalseはそのまま。
    # where はTrueはそのまま。
    input.latitude_deg.mask(
        input.hypo_judge == 9, input.latitude_deg + (input.latitude_min / 60.0), inplace=True)
    input.longitude_deg.mask(
        input.hypo_judge == 9, input.longitude_deg + (input.longitude_min / 60.0), inplace=True)
    # それ以外はF4.2なので、
    input.latitude_deg.where(input.hypo_judge == 9, input.latitude_deg +
                             (input.latitude_min / 60.0 * 0.01), inplace=True)
    input.longitude_deg.where(input.hypo_judge == 9, input.longitude_deg +
                              (input.longitude_min / 60.0 * 0.01), inplace=True)
    return input


def magnitude(input):
    # マグニチュードを直す
    # 0未満の場合は以下のように表記する
    # -0.1: -1   -0.9: -9   -1.0: A0
    # -1.9: A9   -2.0: B0   -3.0: C0
    if input.mag1_1.dtypes == 'O':
        input.replace({'mag1_1': {'-': -0}}, inplace=True)
        input.replace({'mag1_1': {'A': -1}}, inplace=True)
        input.replace({'mag1_1': {'B': -2}}, inplace=True)
        input.replace({'mag1_1': {'C': -3}}, inplace=True)
    input.mag1_1 = input.mag1_1.astype("float64")
    input.mag1_2 = input.mag1_2.astype("float64")
    input.mag1_1 = input["mag1_1"].where(
        input["mag1_1"] >= 0, input["mag1_1"] - (input["mag1_2"] / 10.0))
    input.mag1_1 = input["mag1_1"].where(
        input["mag1_1"] < 0, input["mag1_1"] + (input["mag1_2"] / 10.0))
    return input


def minute(input):
    # 秒を直す。
    # 震源固定の場合は小数点以下空白
    # 震源固定は震源評価9番。
    input.second.where(input.hypo_judge == 9,
                       input.second * 0.01, inplace=True)
    input = input.round({'second': 4})
    input.second = input.second.astype("str")
    input.date = input.date.astype("str")
    input["datetime"] = input['date'].str.cat(input['second'])
    return input


def depth(input):
    # 深さを直す
    # 深さフリーならF5.2
    # 深さ固定ならI3
    # 深さフリーは震源評価1番
    input.depth.mask(input.hypo_judge == 1, input.depth * 0.01, inplace=True)
    input.depth.mask(input.hypo_judge == 10, input.depth * 0.01, inplace=True)
    return input


if __name__ == '__main__':
    args = sys.argv
    if Path(sys.argv[1]).exists():
        cut()
    else:
        print("なにかがおかしいよ\npython3 cut_revised.py [気象庁一元化震源データ]")
