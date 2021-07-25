import numpy as np
import pandas as pd
import random
import shutil
import subprocess
from subprocess import PIPE

# プログラムの根幹部分
def main():
    # お掃除
    subprocess.run(['rm velocity* final.csv'], shell=True)
    # 速度構造設定ファイル
    input = "hogehoge.csv"
    # 子孫数
    gene = 200
    # 世代数
    generation = 10
    # 選択・交叉・突然変異の確率[%]
    rate_sentaku = 5
    rate_kousa = 65
    rate_henni = 30
    # 拡大係数
    rate_min = 0.8
    rate_max = 1.5
    # 選択する際の抽出数[1/]
    proportion = 10
    # 層の数を求める
    layer_number = layer_count(input)
    # 層の範囲の読み込み
    thick_min = first_gene00(input,layer_number)
    thick_max = first_gene01(input,layer_number)
    vp_min = first_gene02(input,layer_number)
    vp_max = first_gene03(input,layer_number)
    vs_min = first_gene04(input,layer_number)
    vs_max = first_gene05(input,layer_number)
    # 初期値作成
    create_gene(layer_number, thick_min, thick_max, vp_min, vp_max, vs_min, vs_max, gene)
    # 1回目の適応度計算(震源決定)
    subprocess.run(["bash GA.sh"], shell=True)
    # 結果を読み込む
    infile = "out_HYPST_2020m2.dat"
    inversion_result = np.loadtxt(infile)
    gene2 = inversion_result.shape[0]
    # NaNは削除
    inversion_result = inversion_result[~np.isnan(inversion_result).any(axis=1), :]
    # 結果を書き出す
    minrow = np.argmin(inversion_result[:,layer_number*3])
    temp = np.delete(inversion_result, minrow, 0)
    minrow2 = np.argmin(temp[:,layer_number*3]) + 1
    final_result = inversion_result[minrow]
    final_result.reshape(1, layer_number*3 + 1)
    np.savetxt("final.csv", final_result, fmt='%5.5f', delimiter=",")
    # ここから世代分回す
    for m in range (0, generation) :
        print(str(m + 2) + "世代目開始…。")
        j = 0
        while True :
            # 選択・交叉・突然変異の判定
            next = judge(rate_sentaku, rate_kousa, rate_henni)
            if next == "sentaku" :
                append = selection(gene2, inversion_result, proportion, layer_number)
            elif next == "kousa" :
                append = kousa(gene2, inversion_result, layer_number, minrow, minrow2)
            elif next == "henni" :
                append = henni(gene2, inversion_result, layer_number, rate_min, rate_max)
            if j == 0 :
                next_gene = inversion_result[minrow]
                # for check2 in range(0, layer_number) :
                #     vp = layer_number+check2
                #     vs = layer_number+layer_number+check2
                #     vp_s = append[vp]
                #     vs_s = append[vs]
                #     vpvs = vp_s / vs_s
                #     if vpvs < 1.5 and vpvs > 1.9 :
                #         guard = "ng"
                #         break
                #     else :
                #         guard = "ok"
                #         pass
                # if guard == "ng" :
                #     continue
                # else :
                #     pass
                #     next_gene = append     
            elif j == 1 :
                next_gene = np.vstack([next_gene, inversion_result[minrow2]])
            else :
                vp_s = []
                vs_s = []
                for check2 in range(0, layer_number) :
                    vp = layer_number + check2
                    vs = layer_number + layer_number + check2
                    vp_s = append[vp]
                    vs_s = append[vs]
                    vpvs = vp_s / vs_s
                    if vpvs < 1.5 and vpvs > 1.9 :
                        check = "ng"
                        break
                        # append = inversion_result[minrow]
                        # next_gene = np.vstack([next_gene, append])
                        # next_gene = np.unique(next_gene, axis=0)
                    else :
                        # next_gene = np.vstack([next_gene, append])
                        check = "ok"
                if check == "ok" :
                    next_gene = np.vstack([next_gene, append])
                    # next_gene = np.unique(next_gene, axis=0)
                else :
                    pass
            # ループ判定
            if next_gene.shape[0] > gene + 2 :
                break
            else :
                pass
            j += 1
        # 結果列はもういらないので削除
        next_gene = np.delete(next_gene, layer_number*3, 1)    
        np.savetxt("test2.dat", next_gene, fmt="%5.3f")
        # 列を並び替える
        for k in range(0, len(next_gene)) :
            for l in range(0, layer_number) :
                layer_number0 = l
                layer_number1 = l + layer_number
                layer_number2 = layer_number1 + layer_number
                if l == 0 :
                    temp_gene = next_gene[k]
                    new_gene = temp_gene[[layer_number0, layer_number1, layer_number2]]
                else :
                    temp_gene = next_gene[k]
                    temp_gene = temp_gene[[layer_number0, layer_number1, layer_number2]]
                    new_gene = np.vstack([new_gene, temp_gene])
            np.savetxt("velocity" + str(k), new_gene, fmt='%5.3f')
            with open("velocity" + str(k), mode='a') as f:
                f.write(" ")
        # 適応度計算
        subprocess.run(["bash GA.sh"], shell=True)
        inversion_result = np.loadtxt("out_HYPST_2020m2.dat")
        # NaNは削除
        inversion_result = inversion_result[~np.isnan(inversion_result).any(axis=1), :]
        # 結果を書き出す
        minrow = np.argmin(inversion_result[:,layer_number*3])
        temp = np.delete(inversion_result, minrow, 0)
        minrow2 = np.argmin(temp[:,layer_number*3]) + 1
        final_result = np.vstack([final_result,inversion_result[minrow]])
        np.savetxt("final.csv", final_result, fmt='%5.3f', delimiter=",")
        print(str(m + 2 ) + "世代目終了…。\n 最小は" + str(minrow) + "番目。")
    np.savetxt("final.csv", final_result, fmt='%5.5f', delimiter=",")
    return

# 層の数を求める
def layer_count(input) :
    df = pd.read_csv(input, header=None)
    return len(df)

# ----------------- ここから ------------------------
# ファイルから層厚・P波速度・S波速度の範囲を読み込む。
# ファイルはカンマ区切り。(hogehoge.csvを参照のこと)
def first_gene00(input, layer_number) :
    layer_thickmin = []
    df = np.loadtxt(input, delimiter=",")
    for i in range(0, layer_number):
        layer_thickmin.append(df[i, 1])
    return layer_thickmin

def first_gene01(input, layer_number) :
    layer_thickmax = []
    df = np.loadtxt(input, delimiter=",")
    for i in range(0, layer_number):
        layer_thickmax.append(df[i, 2])
    return layer_thickmax

def first_gene02(input, layer_number) :
    layer_velpmin = []
    df = np.loadtxt(input, delimiter=",")
    for i in range(0, layer_number):
        layer_velpmin.append(df[i, 3])
    return layer_velpmin

def first_gene03(input, layer_number) :
    layer_velpmax = []
    df = np.loadtxt(input, delimiter=",")
    for i in range(0, layer_number):
        layer_velpmax.append(df[i, 4])
    return layer_velpmax

def first_gene04(input, layer_number) :
    layer_velsmin = []
    df = np.loadtxt(input, delimiter=",")
    for i in range(0, layer_number):
        layer_velsmin.append(df[i, 5])
    return layer_velsmin

def first_gene05(input, layer_number) :
    layer_velsmax = []
    df = np.loadtxt(input, delimiter=",")
    for i in range(0, layer_number):
        layer_velsmax.append(df[i, 6])
    return layer_velsmax
# ----------------- ここまで ------------------------

# 初期遺伝子作成
def create_gene(layer_number, thick_min, thick_max, vp_min, vp_max, vs_min, vs_max, gene_number):
    i = 0
    while True :
        depth = []
        vp = []
        vs = []
        for j in range(0, layer_number):
            dep = round(random.uniform(thick_min[j], thick_max[j]), 2)
            while True :
                vpvp = round(random.uniform(vp_min[j], vp_max[j]), 2)
                vsvs = round(random.uniform(vs_min[j], vs_max[j]), 2)
                vpvs = vpvp /vsvs
                if vpvs < 1.83 and vpvs > 1.5 :
                    break
                else :
                    pass
            depth.append(dep)
            vp.append(vpvp)
            vs.append(vsvs)
        depth = np.array(depth)
        vp = np.array(vp)
        vs = np.array(vs)
        # print(depth)
        origin = np.c_[depth, vp, vs]
        np.savetxt("velocity" + str(i), origin, fmt='%5.3f')
        with open("velocity" + str(i), mode='a') as f:
            f.write(" ")
        origin2 = np.hstack([depth,vp,vs])
        # print(origin2)
        if i == 0 :
            log = origin2
        else :
            loglog = origin2
            new_log = np.vstack([log,loglog])
            # 重複行は削除
            log = np.unique(new_log, axis=0)
        # ループ判定
        if log.shape[0] > gene_number :
            break
        else :
            pass
        i +=1
    np.savetxt("log.dat", log, fmt="%5.3f")
    return

# 速度構造ログと誤差を合体させて結果ファイルを作成する
def result(infile, infile2, gene) :
    sig = np.loadtxt(infile)
    sig2 = np.loadtxt(infile2)
    sig = np.hstack([sig,sig2.reshape(gene,1)])
    sig = sig[~np.isnan(sig).any(axis=1), :]
    return sig

# 選択パート
# 世代数の1/Nを抽出して最も誤差が小さい物を探す。(トーナメント選択)
def selection(gene, sig, proportion, layer_count) :
    for i in range(0, int(round(gene/proportion,0))) :
        if i == 0 :
            a = random.randint(0, len(sig) - 1)
            select_matrix = sig[a]
            sig = np.delete(sig, a, 0)
        else :
            a = random.randint(0, len(sig) - 1)
            select_matrix = np.vstack([select_matrix,sig[a]])
            sig = np.delete(sig, a, 0)
    minrow = np.argmin(select_matrix[:,layer_count*3])
    generated = select_matrix[minrow]
    np.savetxt("next_select.csv", generated, fmt="%5.3f")
    return generated

# 交叉パート
def kousa(gene, sig, layer_count, minrow, minrow2):
    # a = random.randint(0,1)
    a = 0
    if a == 0 :
        # 新たに生成する
        temp1 = sig[minrow]
        temp2 = sig[minrow2]
        new_ab = temp1
        for i in range(0,len(temp1)) :
            new_ab[i] = round(random.uniform(temp1[i], temp2[i]), 2)
    else :
        # ランダムに2つの列を選択する
        a = 1
        b = 1
        while a == b :
            a = random.randint(0, len(sig) - 1)
            b = random.randint(0, len(sig) - 1)
        change_start = 1
        change_goal =  1 
        # 変更する始点・終点を決める
        while change_start > change_goal:
            change_start = random.randint(0,layer_count*3)
            change_goal = random.randint(0,layer_count*3)
        # 1点交叉
        c = sig[a].reshape((1,layer_count*3 + 1))
        d = sig[b].reshape((1,layer_count*3 + 1))
        new_a = c
        new_b = d
        new_a[change_start:change_goal] = d[change_start:change_goal]
        new_b[change_start:change_goal] = c[change_start:change_goal]
        judge = random.randint(0,1)
        if judge == 0 :
            new_ab = new_a
        else :
            new_ab = new_b
        np.savetxt("next_kousa.csv", new_ab, fmt="%5.3f")
    return new_ab

# 突然変異パート
def henni(gene, sig, layer_count, rate_min, rate_max):
    # 拡大係数を積算したものに交換する。
    # ランダムに1つの列を選択する。
    a = random.randint(0, len(sig) - 1)
    # 任意の地震波速度構造に変異させる。
    new_henni = sig[a]
    change_henni = 0
    # 1層目をいじってはいけない
    # while change_henni == 0 or change_henni == 6 or change_henni == 12 or change_henni == 1 or change_henni == 7 or change_henni == 13 or change_henni == 5 or change_henni == 11 or change_henni == 17 :
    while change_henni == 0 or change_henni == 6 or change_henni == 12 or change_henni == 5 or change_henni == 11 or change_henni == 17 :
        change_henni = random.randint(0,layer_count*3)
    rate = random.uniform(rate_min, rate_max)
    new_henni[change_henni] = new_henni[change_henni] * round(rate, 2)
    np.savetxt("next_henni.csv", new_henni, fmt="%5.3f")
    return np.round(new_henni, decimals=2)

# 選択・交叉・突然変異を決定する
# 確率は「突然変異」 < 「交叉」「選択」 の順にする。
def judge(rate_sentaku, rate_kousa, rate_henni):
    a = random.randint(0,100)
    if a < rate_henni :
        choice = "henni"
    elif a < rate_henni + rate_kousa :
        choice = "kousa"
    else :
        choice = "sentaku"
    return choice

if __name__ == "__main__":
    main()