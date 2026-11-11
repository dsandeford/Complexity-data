#!/usr/bin/env python3
"""
Replication code for

    Sandeford, D.S., Turchin, P., Hamilton, M.J. and Lobo, J.
    "Social complexity promotes cooperation and functional integration in
    large-scale societies." PNAS Nexus.

One script, one data file. Running

    python replicate.py

reads Complexity_data.xlsx, rebuilds the analysis data set from the raw Seshat
records, checks the rebuild against the archived scalingData sheet, runs every
analysis reported in the paper, and writes the results to ./results:

    results/report.txt      everything printed to the console
    results/results.xlsx    Table 1 and every other table, one sheet each
    results/Figure2.png/pdf energy vs. road network length
    results/Figure3.png/pdf territory and complexity vs. population
    results/Figure4.png/pdf the six complexity components vs. population

Options
    --data PATH     workbook to read (default: Complexity_data.xlsx)
    --out DIR       output folder (default: results)
    --from-sheet    skip the rebuild and analyse the archived scalingData sheet
    --boot R        bootstrap replicates (default 5000)
    --seed S        random seed (default 20261027)

Requirements: Python 3.9+, numpy, pandas, scipy, matplotlib, openpyxl
(pip install -r requirements.txt). The full run takes about a minute.

This script replaces the four R scripts that were previously in the repository:
    Part 1  (0UpdatePolsVars.R, 1ProcDat.R)  raw Seshat records -> polity-century table
    Part 2  (2AggrAvg.R)                     polity-century table -> scalingData
    Part 3  (3Scaling.R)                     regressions, bootstrap CIs, robustness checks
    Part 4  (3Scaling.R)                     figures

Notes on fidelity to the R code
  * Data processing follows 1ProcDat.R and 2AggrAvg.R, with two differences:
    Idea includes HeredStatus (see IDEA_INCLUDES_HEREDSTATUS below), and the
    hand corrections are matched to the intended records rather than by row
    position (see build_canon()).
  * OLS estimates, R^2, standard errors, the measurement-error correction, the
    saturation model and the Huber and Theil-Sen fits are deterministic and
    match R.
  * Bootstrap confidence intervals use the same method as R's boot.ci
    (BCa with regression-estimated influence values, and the same order-statistic
    interpolation), but numpy's random number generator differs from R's, so
    interval endpoints can differ from the published ones in the third decimal.
  * The MM-estimator follows robustbase::lmrob defaults (bisquare S-estimate by
    fast-S with 500 subsamples, then a 95%-efficient bisquare M-step). Because the
    subsamples are random, its estimates can differ from R in the fourth decimal.
"""

import argparse
import io
import math
import os
import sys
import time
from contextlib import redirect_stdout

import numpy as np
import pandas as pd
from scipy import optimize, stats

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt


# =============================================================================
# Definition switches
# =============================================================================
# Social complexity is the sum of Gov, Infra, Idea, Info, Hier and MilTech, and
# Idea includes "elite status is hereditary" (HeredStatus). Set either switch to
# False to see how the results change without that variable.
IDEA_INCLUDES_HEREDSTATUS = True   # HeredStatus counted in Idea
COMPLEXITY_INCLUDES_HIER = True    # Hier summed into C


# =============================================================================
# Small R-compatibility helpers
# =============================================================================
def r_num(s):
    """R's as.numeric() for a single string; nan where R would give NA."""
    if s is None:
        return math.nan
    if isinstance(s, (int, float)):
        return float(s)
    t = str(s).strip()
    if t == '' or '_' in t:
        return math.nan
    try:
        return float(t)
    except ValueError:
        return math.nan


def r_chr(x):
    """R's as.character() for a double: 15 significant digits, fixed unless
    scientific notation is shorter (so 100000 -> "1e+05", as in R). This matters
    because the R code stores numbers as text and compares rows as text."""
    if x is None or (isinstance(x, float) and math.isnan(x)):
        return None
    if x == 0:
        return '0'
    mant, exp = f'{x:.14e}'.split('e')
    exp = int(exp)
    neg = '-' if mant.startswith('-') else ''
    digits = mant.lstrip('-').replace('.', '').rstrip('0') or '0'
    sci = (f"{neg}{digits[0]}{'.' + digits[1:] if len(digits) > 1 else ''}"
           f"e{'-' if exp < 0 else '+'}{abs(exp):02d}")
    if exp >= 0:
        ip, fp = digits[:exp + 1].ljust(exp + 1, '0'), digits[exp + 1:]
    else:
        ip, fp = '0', '0' * (-exp - 1) + digits
    fixed = f"{neg}{ip}{'.' + fp if fp else ''}"
    return fixed if len(fixed) <= len(sci) else sci


def r_seq(a, b):
    """R's a:b, which counts down when a > b."""
    return list(range(a, b + 1)) if a <= b else list(range(a, b - 1, -1))


# =============================================================================
# Data input
# =============================================================================
def load_workbook(path):
    print(f'Reading {path} ...')
    xl = pd.read_excel(path, sheet_name=None, dtype=str, keep_default_na=False)
    raw = xl['Seshat_raw']
    raw.columns = [c.replace(' ', '.') for c in raw.columns]   # R's read.table names
    def numeric(df, keep_text=()):
        out = df.copy()
        for c in out.columns:
            if c in keep_text:
                continue
            v = pd.to_numeric(out[c].replace('', np.nan), errors='coerce')
            if v.notna().sum() == (out[c] != '').sum():
                out[c] = v
        return out

    polities = numeric(xl['polities'], keep_text=('NGA', 'PolName', 'PolID', 'Dupl'))
    variables = numeric(xl['variables'], keep_text=('Variable', 'ShortName'))
    hy = xl['HistYield'].set_index('NGA')
    hy.columns = [float(c) for c in hy.columns]
    hy = hy.apply(pd.to_numeric)
    roads = numeric(xl['Roadways_Energy'], keep_text=('Country',))
    scaling = numeric(xl['scalingData'], keep_text=('NGA', 'PolID', 'PolName'))
    return dict(raw=raw, polities=polities, variables=variables, histyield=hy,
                roads=roads, scaling=scaling)


# =============================================================================
# PART 1. Raw Seshat records -> polity-century table   (0UpdatePolsVars.R, 1ProcDat.R)
# =============================================================================
CODE_MAP = {'present': '1', 'inferred present': '1', 'inferred absent': '0', 'absent': '0',
            'suspected unknown': '0', 'unknown': '', 'uncoded': '', 'not applicable': ''}
# Columns that must agree for two consecutive records to be one uncertain/disputed value
CMP_COLS = ['NGA', 'Polity', 'Section', 'Subsection', 'Variable',
            'Date.From', 'Date.To', 'Fact.Type', 'Value.Note', 'Date.Note']


def build_canon(raw, polities, Vars):
    """Step 1.1: keep the records for the variables and polities analysed."""
    dat = raw[raw['Polity'] != 'Code book'].drop(columns=['Error.Note']).copy()
    pol_nga = {}
    for pid, nga in zip(polities['PolID'], polities['NGA']):
        pol_nga.setdefault(pid, []).append(nga)
    # attach the NGA; polities not in the polity list keep "NGA" and are dropped
    dat['NGA'] = [pol_nga[p][0] if len(pol_nga.get(p, [])) == 1 else n
                  for p, n in zip(dat['Polity'], dat['NGA'])]
    dat = dat[dat['NGA'] != 'NGA']
    # records ordered by the variable list, then by file order (as R's rbind loop did)
    by_var = {v: g for v, g in dat.groupby('Variable', sort=False)}
    canon = pd.concat([by_var[v] for v in Vars['Variable'] if v in by_var], ignore_index=True)
    canon = canon[canon['Polity'].isin(set(polities['PolID']))]
    # drop variables that are duplicated in the economy sections
    canon = canon[~canon['Section'].isin(['Economy variables',
                                          'Economy and Technology variables (NGA-Level)'])]
    canon = canon.reset_index(drop=True)

    # Hand corrections to the Seshat records.
    # 1ProcDat.R applied these by row position (CanonDat[12,], [196,], [626,] ...).
    # In this data release positions 626 and 1006 fall one row below the intended
    # IrSusa1 records, so the R script changed Iceland's territory (IsCommw,
    # 103,000 km2) to 1,500-2,100 km2 and IrSusa2's largest settlement instead.
    # The paper's results were computed with Iceland at 103,000 km2. Here each
    # correction is matched on polity, variable and original value, so it always
    # lands on the intended record.
    fixes = [  # polity, variable, original Value.From, new from, new to, note
        ('CnEHan*', 'Polity Population', '480000-53869588', 480000, 53869588,
         'range entered in a single field'),
        ('CnEHan*', 'Polity Population', '49150220-49730550', 49150220, 49730550,
         'range entered in a single field'),
        ('IrSusa1', 'Polity Population', '8000', 8500, 25000, 'Hole 1987:91'),
        ('IrSusa1', 'Polity Population', '800', 8500 / 2, 25000 / 2,
         'occupied sites fell from 82 to 53 in Susiana, so ~50% decline, not 90% (Hole 1987)'),
        ('IrSusa1', 'Polity territory', '1', 1500, 2100, 'Hole 1987:73, 90'),
        ('IrSusa1', 'Population of the largest settlement', '350', 750, 3600, ''),
    ]
    for pol, var, v0, lo, hi, note in fixes:
        hit = canon.index[(canon['Polity'] == pol) & (canon['Variable'] == var)
                          & (canon['Value.From'] == v0)]
        if len(hit) != 1:
            raise RuntimeError(f'hand correction target not found: {pol} / {var} = {v0}')
        r = hit[0]
        old = canon.at[r, 'Value.From'] + ('-' + canon.at[r, 'Value.To'] if canon.at[r, 'Value.To'] else '')
        print(f'  hand correction  {pol:8s} {var:38s} {old:>18s} -> {r_chr(float(lo))}-{r_chr(float(hi))}')
        canon.at[r, 'Value.From'] = r_chr(float(lo))
        canon.at[r, 'Value.To'] = r_chr(float(hi))
    return canon


def _date(s):
    """'3000BCE' -> '-3000', '1200CE' -> '1200'; None if the number cannot be read."""
    for suffix, sign in (('BCE', -1), ('CE', 1)):
        if s.endswith(suffix):
            v = r_num(s[:-len(suffix)])
            return None if math.isnan(v) else r_chr(sign * v)
    return s


def process_values(canon):
    """Step 1.2: code presence/absence, convert dates, merge uncertain and disputed
    values into ranges, and reduce every range to its midpoint."""
    rows = canon.to_dict('records')
    for r in rows:
        for c in ('Value.From', 'Value.To'):
            r[c] = CODE_MAP.get(r[c], r[c])
        r['Date.From'] = _date(r['Date.From'])
        r['Date.To'] = _date(r['Date.To'])
    rows = [r for r in rows if r['Date.From'] is not None]

    def same(a, b):
        return all(a[c] == b[c] for c in CMP_COLS)

    # Uncertain: a pair of consecutive matching records becomes one range.
    # (As in R, the test looks at the table before this pass modifies it.)
    orig = [dict(r) for r in rows]
    for i in range(len(orig) - 1):
        if orig[i]['Value.Note'] == 'uncertain' and same(orig[i], orig[i + 1]):
            rows[i]['Value.To'] = rows[i + 1]['Value.From']
            rows[i + 1]['Value.Note'] = 'eliminate'
    rows = [r for r in rows if r['Value.Note'] != 'eliminate']
    # Disputed: the same, but (as in R) the test looks at the table as it is modified.
    for i in range(len(rows) - 1):
        if rows[i]['Value.Note'] == 'disputed' and same(rows[i], rows[i + 1]):
            rows[i]['Value.To'] = rows[i + 1]['Value.From']
            rows[i + 1]['Value.Note'] = 'eliminate'
    rows = [r for r in rows if r['Value.Note'] != 'eliminate']
    # Ranges, uncertain and disputed values -> midpoint
    for r in rows:
        if r['Value.To'] != '':
            r['Value.From'] = r_chr((r_num(r['Value.From']) + r_num(r['Value.To'])) / 2)
    return rows


def reshape(rows, polities, Vars):
    """Step 1.3: one row per polity per century, one column per variable.
    Undated values apply to the whole polity; dated values (a single date covers
    +/-49 years) override them for the centuries they cover."""
    var_names, short = list(Vars['Variable']), list(Vars['ShortName'])
    undated, dated = {}, {}
    for r in rows:
        if r['Date.From'] == '':
            undated.setdefault((r['Polity'], r['Variable']), []).append(r['Value.From'])
        else:
            if r['Date.To'] == '':
                t = r_num(r['Date.From'])
                r['Date.From'], r['Date.To'] = r_chr(t - 49), r_chr(t + 49)
            dated.setdefault((r['NGA'], r['Polity'], r['Variable']), []).append(
                (r_num(r['Date.From']), r_num(r['Date.To']), r['Value.From']))
    out = []
    for nga, pid, start, end, dupl in zip(polities['NGA'], polities['PolID'], polities['Start'],
                                          polities['End'], polities['Dupl']):
        for T in [100 * t for t in r_seq(math.ceil(0.01 * start), math.floor(0.01 * end))]:
            rec = {'NGA': nga, 'PolID': pid, 'Time': T}
            for v, s in zip(var_names, short):
                val = undated.get((pid, v), [None])[0]       # first undated value
                for lo, hi, x in dated.get((nga, pid, v), []):
                    if lo <= T <= hi:                           # first dated value covering T
                        val = x
                        break
                rec[s] = '' if val is None else val
            rec['Dupl'] = dupl
            out.append(rec)
    ts = pd.DataFrame(out)
    # Flag rows identical to the previous row in everything but the date
    cmp = ts.drop(columns=['Time', 'Dupl']).astype(str).values
    uniq = np.ones(len(ts), bool)
    uniq[1:] = ~(cmp[1:] == cmp[:-1]).all(axis=1)
    ts['uniq'] = np.where(uniq, 'y', 'n')
    # Missing ('', 'U', 'U*') -> -999, then numeric
    for s in short:
        ts[s] = [-999.0 if v in ('', 'U', 'U*') else r_num(v) for v in ts[s]]
    # Hand fix carried over from 1ProcDat.R (also to be fixed in Seshat)
    ts.loc[ts['PolID'] == 'FrBurbL', 'LargestArmy'] = (130000 + 150000) / 2
    return ts


# =============================================================================
# PART 2. Polity-century table -> scalingData   (2AggrAvg.R)
# =============================================================================
GOV = ['ProfOfficer', 'ProfSoldier', 'ProfPriest', 'FullTBur', 'ExamSyst', 'MeritProm', 'GovtBldg',
       'Court', 'LegCode', 'Judge', 'Lawyer', 'ConstrGov', 'ConstrNonGov', 'Impeach']
INFRA = ['Irrigation', 'WaterSuppl', 'Market', 'FoodStor', 'Road', 'Bridge', 'Canal', 'Port', 'Mine',
         'Courier', 'PostStation', 'PostService']
INFO = ['Mnemonic', 'NonWRecord', 'Script', 'NonPhWrit', 'PhAlph', 'WRecord', 'Lists', 'Calendar',
        'SacrTxt', 'ReligLit', 'PractLit', 'History', 'Philosophy', 'SciLit', 'Fiction',
        'Article', 'Token', 'PrecMetal', 'ForCoin', 'IndigCoin', 'PaperCurr']
IDEA = ['PrimaryMSP', 'CertainMSP', 'BroadMSP', 'TargetedMSP', 'RulerMSP', 'ElitesMSP', 'CommonersMSP',
        'RuleLegGods', 'SelectStatus', 'IdProsocial', 'IdEqual', 'IdElitCom', 'IdRuleCom', 'PublGoods']
if IDEA_INCLUDES_HEREDSTATUS:
    IDEA = IDEA + ['HeredStatus']


def _known_sum(ts, cols):
    """Sum over coded values; NA when nothing is coded (Gov, Infra)."""
    v = ts[cols].to_numpy(float)
    k = v != -999
    return np.where(k.any(1), np.where(k, v, 0).sum(1), np.nan)


def _z(ts, cols):
    """Values with missing coded as absent (Info, Idea, military technology)."""
    v = ts[cols].to_numpy(float)
    return np.where(v == -999, 0, v)


def aggregate(ts, polities, hy):
    ts = ts.copy()
    ts['Llama'] = (ts['NGA'] == 'Cuzco').astype(float)   # the "Other" pack animal in Cuzco
    vals = ts.drop(columns=['NGA', 'PolID', 'Time', 'Dupl', 'uniq']).to_numpy(float)
    if np.isnan(vals).any():
        raise RuntimeError('a coded value could not be read as a number')

    def lg(x):
        x = np.asarray(x, float)
        return np.where((x != 0) & (x != -999), np.log10(np.where(x > 0, x, 1)), np.nan)

    A = pd.DataFrame({'NGA': ts['NGA'], 'PolID': ts['PolID'], 'Time': ts['Time']})
    A['Pop'] = lg(ts['PolPop'])
    A['Terr'] = lg(ts['PolTerr'])
    h = ts[['SettlHier', 'AdmLev']].to_numpy(float)
    hk = h != -999
    A['Hier'] = np.where(hk.any(1), np.where(hk, h, 0).sum(1) / np.maximum(hk.sum(1), 1), np.nan)
    A['AL'] = np.where(ts['AdmLev'] != -999, ts['AdmLev'], np.nan)
    A['Gov'] = _known_sum(ts, GOV)
    A['Infra'] = _known_sum(ts, INFRA)
    A['Info'] = _z(ts, INFO).sum(1)
    A['Idea'] = _z(ts, IDEA).sum(1)

    # Military technology: six war components (Turchin et al. 2021, PLOS One).
    # Ordered technologies score the most advanced one present.
    metal = (_z(ts, ['Copper', 'Bronze', 'Iron', 'Steel']) * np.arange(1, 5)).max(1)
    proj = (_z(ts, ['Javelin', 'Atlatl', 'Sling', 'SelfBow', 'CompBow', 'Crossbow', 'TensSiege',
                    'SlingSiege', 'Artillery', 'HandGun']) * np.arange(1, 11)).max(1)
    weapon = _z(ts, ['WarClub', 'BattleAxe', 'Dagger', 'Sword', 'Spear', 'Polearm']).sum(1)
    animal = (_z(ts, ['Donkey', 'Llama', 'Camel', 'Elephant', 'Horse']) * [1, 1, 2, 2, 3]).max(1)
    arm = _z(ts, ['WoodArmor', 'LeathArmor', 'Chainmail', 'ScaleArmor', 'LaminArmor', 'PlateArmor',
                  'Shield', 'Helmet', 'Breastplate', 'LimbProt'])
    armor = (arm[:, :6] * [1, 2, 3, 3, 4, 4]).max(1) + arm[:, 6:].sum(1)
    d = _z(ts, ['DefPosition', 'Rampart', 'NonMStone', 'Palisade', 'StoneWall', 'Ditch', 'Moat',
                'FortCamp', 'ComplxFort', 'ModernFort', 'LongWall'])
    defense = ((d[:, :5] * np.arange(1, 6)).max(1) + (d[:, 5:7] * [1, 2]).max(1)
               + d[:, 7:10].sum(1) + (d[:, 10] > 0))
    A['MilTech'] = metal + proj + weapon + animal + armor + defense

    parts = ['Gov', 'Infra', 'Idea', 'Info'] + (['Hier'] if COMPLEXITY_INCLUDES_HIER else [])
    with np.errstate(divide='ignore'):
        A['Complexity'] = np.log10(A[parts].sum(axis=1, skipna=True) + A['MilTech'])
    A.loc[np.isinf(A['Complexity']), 'Complexity'] = np.nan

    # Agricultural productivity for the NGA and century (tons/ha/yr)
    A['Agri'] = [float(hy.at[n, float(t)]) if (t >= -10000 and n in hy.index and float(t) in hy.columns)
                 else np.nan for n, t in zip(A['NGA'], A['Time'])]
    A['Cap'] = lg(ts['CapPop'])
    first_name = dict(zip(polities['PolID'][::-1], polities['PolName'][::-1]))
    A['PolName'] = A['PolID'].map(first_name)
    A['uniq'], A['Dupl'] = ts['uniq'].values, ts['Dupl'].values

    S = A[(A['Dupl'] == 'n') & (A['uniq'] == 'y')].copy()
    # Territory entries that record village area, or have no source (see seshat-db.com)
    S.loc[S['PolID'].isin(['IrSusa1', 'MlJeJe1', 'EcJivaE', 'USMisSp']), 'Terr'] = np.nan
    cols = ['NGA', 'PolID', 'PolName', 'Time', 'Pop', 'Terr', 'Hier', 'AL', 'Gov', 'Infra', 'Info',
            'Idea', 'MilTech', 'Complexity', 'Agri', 'Cap']
    return S[cols].reset_index(drop=True)


def rebuild(data):
    print('\nPART 1-2. Rebuilding scalingData from the raw Seshat records')
    pol, var = data['polities'], data['variables']
    polities = pol[pol['Dupl'] == 'n']
    Vars = var[var['Variable'] != 'Other']            # "Other" is duplicated in Seshat
    canon = build_canon(data['raw'], polities, Vars)
    rows = process_values(canon)
    ts = reshape(rows, pol, Vars)
    S = aggregate(ts, pol, data['histyield'])
    print(f'  {len(canon)} records -> {len(rows)} values -> {len(ts)} polity-centuries '
          f'-> {len(S)} rows in scalingData')
    return S


def compare_to_sheet(S, ref):
    ok = len(S) == len(ref) and (S['PolID'].values == ref['PolID'].values).all()
    worst = 0.0
    for c in ref.columns:
        if ref[c].dtype.kind == 'f' or S[c].dtype.kind == 'f':
            a, b = S[c].to_numpy(float), ref[c].to_numpy(float)
            if (np.isnan(a) != np.isnan(b)).any():
                ok = False
            m = ~np.isnan(a) & ~np.isnan(b)
            worst = max(worst, float(np.max(np.abs(a[m] - b[m]), initial=0)))
        else:
            ok &= bool((S[c].astype(str).values == ref[c].astype(str).values).all())
    ok &= worst < 1e-9
    print(f'  check against the scalingData sheet: {"identical" if ok else "DIFFERENT"}'
          f' (largest numeric difference {worst:.1e})')
    return ok


# =============================================================================
# PART 3. Statistics
# =============================================================================
def design(df, y, terms):
    """Build y and X for an R-style formula. terms: column names; 'f:COL' for a factor
    (treatment coding, first level dropped). Rows with any missing value are dropped."""
    cols = [y] + [t[2:] if t.startswith('f:') else t for t in terms]
    d = df.dropna(subset=cols)
    d = d[np.isfinite(d[y].astype(float))]
    X, names = [np.ones(len(d))], ['(Intercept)']
    for t in terms:
        if t.startswith('f:'):
            levels = sorted(d[t[2:]].unique())
            for lv in levels[1:]:
                X.append((d[t[2:]] == lv).to_numpy(float))
                names.append(f'{t[2:]}{lv}')
        else:
            X.append(d[t].to_numpy(float))
            names.append(t)
    return d[y].to_numpy(float), np.column_stack(X), names, d


def _drop_aliased(X, tol=1e-7):
    """Indices of columns to keep, dropping columns that are linear combinations of
    earlier ones (as R's lm reports NA for aliased coefficients)."""
    keep, Q = [], np.zeros((X.shape[0], 0))
    for j in range(X.shape[1]):
        v = X[:, j] - Q @ (Q.T @ X[:, j])
        nv, n0 = np.linalg.norm(v), np.linalg.norm(X[:, j])
        if n0 > 0 and nv > tol * n0:
            keep.append(j)
            Q = np.column_stack([Q, v / nv])
    return keep


def ols(df, y, terms):
    yv, X, names, d = design(df, y, terms)
    keep = _drop_aliased(X)
    Xk = X[:, keep]
    beta, *_ = np.linalg.lstsq(Xk, yv, rcond=None)
    res = yv - Xk @ beta
    n, p = Xk.shape
    dfr = n - p
    s2 = res @ res / dfr
    se = np.sqrt(np.diag(s2 * np.linalg.inv(Xk.T @ Xk)))
    tt = beta / se
    pv = 2 * stats.t.sf(np.abs(tt), dfr)
    tss = ((yv - yv.mean()) ** 2).sum()
    r2 = 1 - (res @ res) / tss
    adj = 1 - (1 - r2) * (n - 1) / dfr
    F = ((tss - res @ res) / (p - 1)) / s2 if p > 1 else np.nan
    coef = pd.DataFrame({'estimate': beta, 'se': se, 't': tt, 'p': pv},
                        index=[names[j] for j in keep])
    return dict(coef=coef, r2=r2, adj_r2=adj, n=n, df=dfr, sigma=math.sqrt(s2), F=F,
                F_p=stats.f.sf(F, p - 1, dfr) if p > 1 else np.nan, rss=res @ res,
                resid=res, X=Xk, y=yv, data=d)


def fmt_model(label, m, show=('(Intercept)', 'Pop', 'AL')):
    lines = [f'{label}   n = {m["n"]}, R2 = {m["r2"]:.3f}, adj. R2 = {m["adj_r2"]:.3f}, '
             f'resid. SE = {m["sigma"]:.4f} on {m["df"]} df']
    for k in show:
        if k in m['coef'].index:
            r = m['coef'].loc[k]
            lines.append(f'    {k:12s} {r.estimate:9.4f}  (SE {r.se:.4f}, t = {r.t:7.2f}, p = {r.p:.2g})')
    fe = [k for k in m['coef'].index if k not in show]
    if fe:
        lines.append(f'    + {len(fe)} fixed-effect coefficients (not shown)')
    return '\n'.join(lines)


def slope(x, y):
    xc = x - x.mean()
    return float(xc @ (y - y.mean()) / (xc @ xc))


# ---------------------------------------------------------------- bootstrap
def bootstrap(n, statistic, R, rng):
    """Ordinary nonparametric bootstrap (as boot::boot). statistic(idx) -> array."""
    t0 = np.atleast_1d(statistic(np.arange(n)))
    idx = rng.integers(0, n, size=(R, n))
    t = np.array([np.atleast_1d(statistic(i)) for i in idx])
    return dict(t0=t0, t=t, idx=idx, n=n)


def bootstrap_slope(x, y, R, rng):
    """Fast vectorised bootstrap of a simple-regression slope."""
    n = len(x)
    idx = rng.integers(0, n, size=(R, n))
    X, Y = x[idx], y[idx]
    Xc = X - X.mean(1, keepdims=True)
    t = ((Xc * (Y - Y.mean(1, keepdims=True))).sum(1) / (Xc ** 2).sum(1))[:, None]
    return dict(t0=np.array([slope(x, y)]), t=t, idx=idx, n=n)


def _influence_reg(b, k):
    """Empirical influence values by regression on the bootstrap frequency array
    (boot::empinf type = 'reg', which boot.ci uses for BCa)."""
    t = b['t'][:, k]
    fin = np.isfinite(t)
    R, n = b['idx'].shape
    f = np.zeros((R, n))
    np.add.at(f, (np.repeat(np.arange(R), n), b['idx'].ravel()), 1)
    X = np.column_stack([np.ones(fin.sum()), f[fin][:, 1:] / n])
    coef, *_ = np.linalg.lstsq(X, t[fin], rcond=None)
    L = np.concatenate([[0.0], coef[1:]])
    return L - L.mean()


def _norm_inter(t, alpha):
    """boot:::norm.inter: interpolate order statistics on the normal scale."""
    t = np.sort(t[np.isfinite(t)])
    R = len(t)
    out = []
    for a in np.atleast_1d(alpha):
        rk = (R + 1) * a
        k = int(math.trunc(rk))
        if k == rk:
            out.append(t[k - 1])
        elif k == 0:
            out.append(t[0])
        elif k >= R:
            out.append(t[-1])
        else:
            z, z1, z2 = stats.norm.ppf(a), stats.norm.ppf(k / (R + 1)), stats.norm.ppf((k + 1) / (R + 1))
            out.append(t[k - 1] + (z - z1) / (z2 - z1) * (t[k] - t[k - 1]))
    return np.array(out)


def ci_bca(b, k=0, conf=0.95):
    t, t0 = b['t'][:, k], b['t0'][k]
    t = t[np.isfinite(t)]
    w = stats.norm.ppf(np.mean(t < t0))
    L = _influence_reg(b, k)
    a = (L ** 3).sum() / (6 * (L ** 2).sum() ** 1.5)
    za = stats.norm.ppf([(1 - conf) / 2, (1 + conf) / 2])
    adj = stats.norm.cdf(w + (w + za) / (1 - a * (w + za)))
    return _norm_inter(t, adj)


def ci_perc(b, k=0, conf=0.95):
    return _norm_inter(b['t'][:, k], [(1 - conf) / 2, (1 + conf) / 2])


def boot_summary(label, b, k=0):
    t = b['t'][:, k]
    bc, pc = ci_bca(b, k), ci_perc(b, k)
    print(f'  {label:34s} estimate {b["t0"][k]:.4f}  bias {t.mean() - b["t0"][k]:+.5f}  '
          f'SE {t.std(ddof=1):.4f}  95% BCa [{bc[0]:.3f}, {bc[1]:.3f}]  percentile [{pc[0]:.3f}, {pc[1]:.3f}]')
    return dict(statistic=label, estimate=b['t0'][k], bias=t.mean() - b['t0'][k], se=t.std(ddof=1),
                bca_low=bc[0], bca_high=bc[1], perc_low=pc[0], perc_high=pc[1], n=b['n'], R=len(t))


# ---------------------------------------------------------------- robust regression
def huber(x, y, k=1.345, maxit=100, acc=1e-4):
    """MASS::rlm default: Huber psi, MAD scale re-estimated each step, LS start."""
    X = np.column_stack([np.ones_like(x), x])
    beta = np.linalg.lstsq(X, y, rcond=None)[0]
    res = y - X @ beta
    w = np.ones_like(y)
    for _ in range(maxit):
        scale = np.median(np.abs(res[w != 0])) / 0.6745
        if scale == 0:
            break
        u = np.abs(res / scale)
        w = np.where(u <= k, 1.0, k / np.maximum(u, 1e-300))
        sw = np.sqrt(w)
        beta = np.linalg.lstsq(X * sw[:, None], y * sw, rcond=None)[0]
        new = y - X @ beta
        conv = math.sqrt(((res - new) ** 2).sum() / max(1e-20, (res ** 2).sum()))
        res = new
        if conv <= acc:
            break
    return beta[1]


def _rho_bisq(u, c):
    v = np.minimum(np.abs(u) / c, 1.0)
    return 1 - (1 - v ** 2) ** 3


def _w_bisq(u, c):
    v = u / c
    return np.where(np.abs(v) < 1, (1 - v ** 2) ** 2, 0.0)


def _m_scale(r, c=1.54764, b=0.5, s=None, tol=1e-10, maxit=200):
    s = s if s else np.median(np.abs(r)) / 0.6745
    if s <= 0:
        return 0.0
    for _ in range(maxit):
        s_new = s * math.sqrt(np.mean(_rho_bisq(r / s, c)) / b)
        if abs(s_new / s - 1) < tol:
            return s_new
        s = s_new
    return s


def _wls(X, y, w):
    sw = np.sqrt(w)
    return np.linalg.lstsq(X * sw[:, None], y * sw, rcond=None)[0]


def mm_estimate(x, y, rng, nres=500, best_r=2, k_fast=1, k_max=200, c_s=1.54764,
                c_m=4.685061, tol=1e-7, max_it=50):
    """MM-regression with robustbase::lmrob defaults."""
    X = np.column_stack([np.ones_like(x), x])
    n, p = X.shape

    def refine(beta, steps):
        s = None
        for _ in range(steps):
            r = y - X @ beta
            s = _m_scale(r, c_s, s=s)
            if s == 0:
                break
            new = _wls(X, y, _w_bisq(r / s, c_s))
            done = np.linalg.norm(new - beta) <= tol * max(tol, np.linalg.norm(beta))
            beta = new
            if done:
                break
        r = y - X @ beta
        return beta, _m_scale(r, c_s, s=s)

    cands = []
    for _ in range(nres):
        for _try in range(50):
            sub = rng.choice(n, p, replace=False)
            if abs(np.linalg.det(X[sub])) > 1e-10:
                break
        beta = np.linalg.solve(X[sub], y[sub])
        beta, s = refine(beta, k_fast)
        cands.append((s, beta))
    cands.sort(key=lambda z: z[0])
    best = min((refine(b0, k_max) for _, b0 in cands[:best_r]), key=lambda z: z[1])
    beta, s = best
    for _ in range(max_it):          # M-step with fixed S-scale
        new = _wls(X, y, _w_bisq((y - X @ beta) / s, c_m))
        done = np.linalg.norm(new - beta) <= tol * max(tol, np.linalg.norm(beta))
        beta = new
        if done:
            break
    return beta[1]


def theil_sen(x, y):
    """mblm(repeated = FALSE): median of all pairwise slopes (pairs with equal x skipped)."""
    return float(stats.theilslopes(y, x, method='separate')[0])


# =============================================================================
# PART 3. Analyses   (3Scaling.R)
# =============================================================================
def run_analyses(S, roads, raw, R, seed):
    T = {}                                    # tables for results.xlsx
    S = S.copy()
    for v in ('Agri', 'Gov', 'Idea', 'Infra', 'Info', 'MilTech', 'Hier'):
        S['log' + v] = np.log10(S[v].where(S[v] > 0))
    road = roads[(roads['Country'] != 'Singapore') & (roads['TFC_PJ_IEA'] > 0) & (roads['Total_km'] > 0)].copy()
    road['logE'], road['logL'] = np.log10(road['TFC_PJ_IEA']), np.log10(road['Total_km'])

    # ------------------------------------------------------------ 3.1 OLS models
    print('\n' + '=' * 78 + '\nPART 3.1  Regression models (OLS on log10 values)\n' + '=' * 78)
    print('The paper reports R2 for simple regressions and adjusted R2 for fixed-effects models.')
    specs = [
        ('Energy vs. road length (Fig. 2)', road, 'logE', ['logL']),
        ('Intensity, I ~ N', S, 'logAgri', ['Pop']),
        ('Intensity, I ~ N + AL + NGA', S, 'logAgri', ['Pop', 'AL', 'f:NGA']),
        ('Territory, A ~ N', S, 'Terr', ['Pop']),
        ('Territory, NGA fixed effects', S, 'Terr', ['f:NGA', 'Pop']),
        ('Territory, century fixed effects', S, 'Terr', ['f:Time', 'Pop']),
        ('Territory, NGA + century fixed effects', S, 'Terr', ['f:NGA', 'f:Time', 'Pop']),
        ('Complexity, C ~ N', S, 'Complexity', ['Pop']),
        ('Complexity, NGA fixed effects', S, 'Complexity', ['f:NGA', 'Pop']),
        ('Complexity, century fixed effects', S, 'Complexity', ['f:Time', 'Pop']),
        ('Complexity, NGA + century fixed effects', S, 'Complexity', ['f:NGA', 'f:Time', 'Pop']),
        ('Government (Gov)', S, 'logGov', ['Pop']),
        ('Ideology (Idea)', S, 'logIdea', ['Pop']),
        ('Infrastructure (Infra)', S, 'logInfra', ['Pop']),
        ('Information (Info)', S, 'logInfo', ['Pop']),
        ('Military technology (MilTech)', S, 'logMilTech', ['Pop']),
        ('Hierarchy (Hier)', S, 'logHier', ['Pop']),
    ]
    models, rows = {}, []
    for label, d, y, terms in specs:
        m = ols(d, y, terms)
        models[label] = m
        print('\n' + fmt_model(label, m, show=('(Intercept)', 'Pop', 'AL', 'logL')))
        x = 'logL' if 'logL' in terms else 'Pop'
        r = m['coef'].loc[x]
        rows.append(dict(model=label, response=y, predictors=' + '.join(t.replace('f:', 'factor ') for t in terms),
                         exponent=r.estimate, se=r.se, p=r.p,
                         intercept=m['coef'].loc['(Intercept)', 'estimate'] if len(terms) == 1 else np.nan,
                         R2=m['r2'], adj_R2=m['adj_r2'], n=m['n']))
    T['Regressions'] = pd.DataFrame(rows)

    # ------------------------------------------------------------ 3.2 bootstrap
    print('\n' + '=' * 78 + f'\nPART 3.2  Bootstrap 95% confidence intervals ({R} replicates, seed {seed})\n' + '=' * 78)
    brows = []

    rng = np.random.default_rng(seed)
    bR = bootstrap_slope(road['logL'].to_numpy(), road['logE'].to_numpy(), R, rng)
    brows.append(boot_summary('Energy ~ road length', bR))

    # Intensity exponent beta with NGA fixed effects and AL; predicted ranges 1-2b, 2b
    rng = np.random.default_rng(seed)
    yI, XI, namesI, dI = design(S, 'logAgri', ['Pop', 'AL', 'f:NGA'])
    jp = namesI.index('Pop')

    def beta_fun(i):
        b = np.linalg.lstsq(XI[i], yI[i], rcond=None)[0][jp]
        return np.array([b, 1 - 2 * b, 2 * b])
    bI = bootstrap(len(yI), beta_fun, R, rng)
    brows.append(boot_summary('Intensity exponent, beta', bI, 0))
    brows.append(boot_summary('Predicted territory exponent, 1-2b', bI, 1))
    brows.append(boot_summary('Predicted complexity exponent, 2b', bI, 2))

    rng = np.random.default_rng(seed)
    dA = S.dropna(subset=['Terr', 'Pop'])
    dC = S.dropna(subset=['Complexity', 'Pop'])
    bA = bootstrap_slope(dA['Pop'].to_numpy(), dA['Terr'].to_numpy(), R, rng)
    bC = bootstrap_slope(dC['Pop'].to_numpy(), dC['Complexity'].to_numpy(), R, rng)
    brows.append(boot_summary('Territory exponent', bA))
    brows.append(boot_summary('Complexity exponent', bC))

    rng = np.random.default_rng(seed)
    comp = {}
    for v in ('Gov', 'Idea', 'Infra', 'Info', 'MilTech', 'Hier'):
        d = S.dropna(subset=['log' + v, 'Pop'])
        comp[v] = (bootstrap_slope(d['Pop'].to_numpy(), d['log' + v].to_numpy(), R, rng), d)
        brows.append(boot_summary(f'{v} exponent', comp[v][0]))
    T['Bootstrap'] = pd.DataFrame(brows)

    # ------------------------------------------------------------ Table 1
    rangeA, rangeC = ci_bca(bI, 1), ci_bca(bI, 2)
    mI = models['Intensity, I ~ N + AL + NGA']
    t1 = [dict(Variable='Agricultural intensity, I', Predicted='-', Pred_low=np.nan, Pred_high=np.nan,
               Estimated=bI['t0'][0], CI_low=ci_bca(bI, 0)[0], CI_high=ci_bca(bI, 0)[1], R2=mI['adj_r2'], n=mI['n']),
          dict(Variable='Territory, A', Predicted='5/6', Pred_low=rangeA[0], Pred_high=rangeA[1],
               Estimated=bA['t0'][0], CI_low=ci_bca(bA)[0], CI_high=ci_bca(bA)[1],
               R2=models['Territory, A ~ N']['r2'], n=bA['n']),
          dict(Variable='Social complexity, C', Predicted='1/6', Pred_low=rangeC[0], Pred_high=rangeC[1],
               Estimated=bC['t0'][0], CI_low=ci_bca(bC)[0], CI_high=ci_bca(bC)[1],
               R2=models['Complexity, C ~ N']['r2'], n=bC['n'])]
    names = dict(Gov='Government', Idea='Ideology', Infra='Infrastructure', Info='Information',
                 MilTech='Military technology', Hier='Hierarchy')
    for v, (b, d) in comp.items():
        ci = ci_bca(b)
        r2 = np.corrcoef(d['Pop'], d['log' + v])[0, 1] ** 2
        t1.append(dict(Variable='   ' + names[v], Predicted='1/6', Pred_low=rangeC[0], Pred_high=rangeC[1],
                       Estimated=b['t0'][0], CI_low=ci[0], CI_high=ci[1], R2=r2, n=b['n']))
    t1 = pd.DataFrame(t1)
    T['Table1'] = t1
    print('\nTable 1. Predicted and estimated scaling exponents (95% BCa intervals)')
    print('(R2 for agricultural intensity is the adjusted R2 of the fixed-effects model, as in the paper)\n')
    print(f'{"Variable":28s} {"Pred.":>5s} {"Predicted range":>16s} {"Estimate":>9s} {"95% CI":>14s} {"R2":>5s} {"n":>4s}')
    for _, r in t1.iterrows():
        pr = '' if np.isnan(r.Pred_low) else f'{r.Pred_low:.3f}-{r.Pred_high:.3f}'
        print(f'{r.Variable:28s} {r.Predicted:>5s} {pr:>16s} {r.Estimated:9.3f} '
              f'{r.CI_low:7.3f}-{r.CI_high:.3f} {r.R2:5.2f} {r.n:4d}')

    # ------------------------------------------------------------ 3.3 zeros
    print('\n' + '=' * 78 + '\nPART 3.3  Effect of dropping zero-valued components\n' + '=' * 78)
    zrows = []
    for v in ('Gov', 'Idea', 'Infra', 'Info', 'MilTech'):
        s = S.dropna(subset=[v, 'Pop'])
        pos = s[s[v] > 0]
        zrows.append(dict(component=v, n=len(s), zeros=int((s[v] == 0).sum()),
                          slope_zeros_dropped=slope(pos['Pop'].to_numpy(), np.log10(pos[v].to_numpy())),
                          slope_log_plus_1=slope(s['Pop'].to_numpy(), np.log10(s[v].to_numpy() + 1))))
    T['Zeros'] = pd.DataFrame(zrows)
    print(T['Zeros'].round(4).to_string(index=False))

    # ------------------------------------------------------------ 3.4 measurement error
    print('\n' + '=' * 78 + '\nPART 3.4  Measurement error in log10 population\n' + '=' * 78)
    pop = raw[raw['Variable'] == 'Polity Population']
    lo = pop['Value.From'].map(r_num).to_numpy()
    hi = pop['Value.To'].map(r_num).to_numpy()
    ranged = ~np.isnan(lo) & ~np.isnan(hi) & (lo > 0) & (hi > 0) & (hi != lo)
    lw = np.log10(hi[ranged] / lo[ranged])
    su2 = np.mean(lw ** 2) / 12           # error variance if the true value is uniform over the range
    print(f'  population entries {int((~np.isnan(lo)).sum())}, with a range {int(ranged.sum())}, '
          f'median range factor {10 ** np.median(lw):.3f}')
    print(f'  error variance {su2:.5f}, error SD {math.sqrt(su2):.4f} (log10 units)')

    def mm(d, y):
        d = d.dropna(subset=['Pop', y])
        b = slope(d['Pop'].to_numpy(), d[y].to_numpy())
        rel = 1 - su2 / d['Pop'].var(ddof=1)
        return dict(variable=y, n=len(d), OLS=b, reliability=rel, corrected=b / rel, change=b / rel - b)
    me = pd.DataFrame([mm(S, 'Terr'), mm(S, 'Complexity')] +
                      [mm(S, 'log' + v) for v in ('Gov', 'Idea', 'Infra', 'Info', 'MilTech', 'Hier')])
    print('\n  Reliability-corrected (method-of-moments) exponents')
    print(me.round(4).to_string(index=False))

    upper_A = rangeA[1]
    bAo = slope(dA['Pop'].to_numpy(), dA['Terr'].to_numpy())
    sd_needed = math.sqrt((1 - bAo / upper_A) * dA['Pop'].var(ddof=1))
    bCo = slope(dC['Pop'].to_numpy(), dC['Complexity'].to_numpy())
    relC = 1 - sd_needed ** 2 / dC['Pop'].var(ddof=1)
    print(f'\n  Error needed to push the territory exponent above its predicted range '
          f'(upper bound {upper_A:.3f}):')
    print(f'    error SD {sd_needed:.4f} log10 units = a factor of {10 ** sd_needed:.2f}')
    print(f'    the same error moves the complexity exponent from {bCo:.4f} to {bCo / relC:.4f}')
    T['MeasurementError'] = pd.concat([me, pd.DataFrame([
        dict(variable='error variance (log10 pop)', OLS=su2),
        dict(variable='breakdown error SD (log10)', OLS=sd_needed),
        dict(variable='breakdown error factor', OLS=10 ** sd_needed),
        dict(variable='complexity exponent at breakdown error', OLS=bCo, corrected=bCo / relC)])],
        ignore_index=True)

    # ------------------------------------------------------------ 3.5 leverage
    print('\n' + '=' * 78 + '\nPART 3.5  High-leverage and influential observations; robust fits\n' + '=' * 78)
    rng = np.random.default_rng(seed)
    lrows, flagged = [], {}
    for y in ['Terr', 'Complexity'] + ['log' + v for v in ('Gov', 'Idea', 'Infra', 'Info', 'MilTech', 'Hier')]:
        d = S.dropna(subset=['Pop', y])
        x, yy = d['Pop'].to_numpy(), d[y].to_numpy()
        n, p = len(d), 2
        b = slope(x, yy)
        a = yy.mean() - b * x.mean()
        e = yy - a - b * x
        h = 1 / n + (x - x.mean()) ** 2 / ((x - x.mean()) ** 2).sum()
        s2 = (e @ e) / (n - p)
        cd = e ** 2 / (p * s2) * h / (1 - h) ** 2
        flag = cd > 4 / n
        flagged[y] = d['PolName'][flag].tolist()
        lrows.append(dict(variable=y, n=n, high_leverage=int((h > 2 * p / n).sum()), influential=int(flag.sum()),
                          OLS=b, drop_infl=slope(x[~flag], yy[~flag]), Huber=huber(x, yy),
                          MM=mm_estimate(x, yy, rng), TheilSen=theil_sen(x, yy),
                          infl_mean_Pop=x[flag].mean(), infl_pct_neg=np.mean(e[flag] < 0)))
    T['Leverage'] = pd.DataFrame(lrows)
    print(T['Leverage'].round(3).to_string(index=False))
    print('\n  Influential polities in the complexity regression:\n    ' +
          '\n    '.join(sorted(set(flagged['Complexity']))))

    # ------------------------------------------------------------ 3.6 saturation
    print('\n' + '=' * 78 + '\nPART 3.6  Counterfactual: linear growth in institutions, then saturation\n' + '=' * 78)
    d = S.dropna(subset=['Pop', 'Complexity'])
    win = d[(d['Pop'] >= 3) & (d['Pop'] <= 5)]
    above = d[d['Pop'] > 5]
    rng_c = 10 ** win['Complexity']
    print(f'  complexity counts for polities of 10^3-10^5 people range from {rng_c.min():.1f} to {rng_c.max():.1f}')
    srows = []
    for lab, dd in (('10^3 <= N <= 10^5 (counterfactual slope 1)', win), ('N > 10^5 (counterfactual slope 0)', above)):
        m = ols(dd, 'Complexity', ['Pop'])
        r = m['coef'].loc['Pop']
        q = stats.t.ppf(0.975, m['df'])
        print(f'  {lab:44s} slope {r.estimate:.4f}  95% CI [{r.estimate - q * r.se:.3f}, {r.estimate + q * r.se:.3f}]  n = {m["n"]}')
        srows.append(dict(model=lab, slope=r.estimate, ci_low=r.estimate - q * r.se, ci_high=r.estimate + q * r.se, n=m['n']))

    x, yy = d['Pop'].to_numpy(), d['Complexity'].to_numpy()
    sat = lambda P, lImax, lK: lImax + P - np.log10(10 ** lK + 10 ** P)
    par, _ = optimize.curve_fit(sat, x, yy, p0=[2, 4], maxfev=10000)
    n = len(x)
    aic = lambda rss, k: n * math.log(2 * math.pi) + n * math.log(rss / n) + n + 2 * (k + 1)
    rss_pow = ((yy - np.polyval(np.polyfit(x, yy, 1), x)) ** 2).sum()
    rss_sat = ((yy - sat(x, *par)) ** 2).sum()
    aic_pow, aic_sat = aic(rss_pow, 2), aic(rss_sat, 2)
    print(f'  power law AIC {aic_pow:.2f};  linear-then-saturating AIC {aic_sat:.2f};  '
          f'Delta AIC = {aic_sat - aic_pow:.1f}')
    print(f'  saturating fit: ceiling {10 ** par[0]:.1f} institutions, half-saturation population {10 ** par[1]:,.0f}')
    srows += [dict(model='power law AIC', slope=aic_pow, n=n), dict(model='saturating AIC', slope=aic_sat, n=n),
              dict(model='Delta AIC (saturating - power law)', slope=aic_sat - aic_pow, n=n),
              dict(model='saturating ceiling, 10^lImax', slope=10 ** par[0]),
              dict(model='half-saturation population, 10^lK', slope=10 ** par[1])]
    T['Saturation'] = pd.DataFrame(srows)
    return T, models, road


# =============================================================================
# PART 4. Figures   (3Scaling.R)
# =============================================================================
GREY, NAVY = '#9e9e9e', '#000080'


def _panel(ax, x, y, label, xlab, ylab, r2, digits=3, xlim=None, ylim=None):
    b, a = np.polyfit(x, y, 1)
    ax.scatter(x, y, s=10, color=GREY, alpha=0.5, linewidths=0)
    xs = np.array([x.min(), x.max()])
    ax.plot(xs, a + b * xs, color=NAVY, alpha=0.5, lw=1.6)
    ax.text(0.5, 0.95, rf'$\log\,{label} = {a:.{digits}f} + {b:.{digits}f}\,\log\,N$   $R^2 = {r2:.{digits}f}$',
            transform=ax.transAxes, ha='center', va='top', fontsize=8.5)
    ax.set_xlabel(xlab); ax.set_ylabel(ylab)
    if xlim: ax.set_xlim(*xlim)
    if ylim: ax.set_ylim(*ylim)
    ax.grid(color='#ebebeb', lw=0.6); ax.set_axisbelow(True)
    for s in ax.spines.values():
        s.set_visible(False)
    ax.tick_params(length=0, labelsize=8)


def figures(S, road, models, out):
    # Figure 2: energy vs. road network length
    fig, ax = plt.subplots(figsize=(6, 4))
    m = models['Energy vs. road length (Fig. 2)']
    x, y = road['logL'].to_numpy(), road['logE'].to_numpy()
    b, a = np.polyfit(x, y, 1)
    ax.scatter(x, y, s=12, color=GREY, alpha=0.5, linewidths=0)
    ax.plot([4, 7], [a + 4 * b, a + 7 * b], color=NAVY, alpha=0.5, lw=1.6)
    ax.text(5.5, 5, rf'$\log\,Energy = {a:.2f} + {b:.2f}\,\log\,L_n$   $R^2 = {m["adj_r2"]:.2f}$',
            ha='center', fontsize=9)
    ax.set_xlim(4, 7)
    ax.set_xlabel(r'log Length of Road Network (km), $L_n$'); ax.set_ylabel(r'log $E$ (PJ/y)')
    ax.grid(color='#ebebeb', lw=0.6); ax.set_axisbelow(True)
    [s.set_visible(False) for s in ax.spines.values()]
    fig.tight_layout(); _save(fig, out, 'Figure2')

    # Figure 3: territory and complexity
    fig, axs = plt.subplots(2, 1, figsize=(6, 7))
    dA = S.dropna(subset=['Pop', 'Terr']); dC = S.dropna(subset=['Pop', 'Complexity'])
    _panel(axs[0], dA['Pop'], dA['Terr'], 'A', '', 'log Area', models['Territory, A ~ N']['r2'], ylim=(2, 8))
    _panel(axs[1], dC['Pop'], dC['Complexity'], 'C', 'log Polity population', 'log Complexity',
           models['Complexity, C ~ N']['r2'], ylim=(0.5, 2.5))
    fig.tight_layout(); _save(fig, out, 'Figure3')

    # Figure 4: complexity components
    fig, axs = plt.subplots(3, 2, figsize=(8, 9))
    labels = [('Gov', 'CC_1', 'log Gov'), ('Idea', 'CC_2', 'log Idea'), ('Infra', 'CC_3', 'log Infra'),
              ('Info', 'CC_4', 'log Info'), ('MilTech', 'CC_5', 'log MilTech'), ('Hier', 'CC_6', 'log Hier')]
    for k, (ax, (v, cc, yl)) in enumerate(zip(axs.ravel(), labels)):
        d = S.dropna(subset=['Pop', 'log' + v])
        r2 = np.corrcoef(d['Pop'], d['log' + v])[0, 1] ** 2
        _panel(ax, d['Pop'], d['log' + v], cc, 'log Polity population' if k >= 4 else '', yl, r2)
    fig.tight_layout(); _save(fig, out, 'Figure4')


def _save(fig, out, name):
    for ext in ('png', 'pdf'):
        fig.savefig(os.path.join(out, f'{name}.{ext}'), dpi=300)
    plt.close(fig)
    print(f'  wrote {name}.png and {name}.pdf')


# =============================================================================
# Output
# =============================================================================
def write_results(T, S, path):
    order = ['Table1', 'Regressions', 'Bootstrap', 'Zeros', 'MeasurementError', 'Leverage', 'Saturation']
    with pd.ExcelWriter(path, engine='openpyxl') as xw:
        for k in order:
            T[k].to_excel(xw, sheet_name=k, index=False)
        S.to_excel(xw, sheet_name='scalingData_used', index=False)
        for ws in xw.book.worksheets:
            ws.freeze_panes = 'A2'
            for col in ws.columns:
                w = max(len(str(c.value)) if c.value is not None else 0 for c in col[:60])
                ws.column_dimensions[col[0].column_letter].width = min(max(10, w + 2), 48)
                for c in col[1:]:
                    if isinstance(c.value, float):
                        c.number_format = '0.0000'


class Tee(io.TextIOBase):
    def __init__(self, *streams):
        self.streams = streams

    def write(self, s):
        for st in self.streams:
            st.write(s)
        return len(s)

    def flush(self):
        for st in self.streams:
            st.flush()


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    ap = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    ap.add_argument('--data', default=os.path.join(here, 'Complexity_data.xlsx'))
    ap.add_argument('--out', default=os.path.join(here, 'results'))
    ap.add_argument('--from-sheet', action='store_true')
    ap.add_argument('--boot', type=int, default=5000)
    ap.add_argument('--seed', type=int, default=20261027)
    args = ap.parse_args()
    os.makedirs(args.out, exist_ok=True)
    log = open(os.path.join(args.out, 'report.txt'), 'w', encoding='utf-8')
    t0 = time.time()
    with redirect_stdout(Tee(sys.stdout, log)):
        print('Social complexity promotes cooperation and functional integration in large-scale societies')
        print('Replication report generated', time.strftime('%Y-%m-%d %H:%M'))
        print(f'Definitions: Idea includes HeredStatus = {IDEA_INCLUDES_HEREDSTATUS}; '
              f'Complexity sum includes Hier = {COMPLEXITY_INCLUDES_HIER}')
        data = load_workbook(args.data)
        if args.from_sheet:
            S = data['scaling']
            print('\nUsing the archived scalingData sheet (no rebuild).')
        else:
            S = rebuild(data)
            compare_to_sheet(S, data['scaling'])
        T, models, road = run_analyses(S, data['roads'], data['raw'], args.boot, args.seed)
        print('\n' + '=' * 78 + '\nPART 4  Figures\n' + '=' * 78)
        figures(S.assign(**{'log' + v: np.log10(S[v].where(S[v] > 0))
                            for v in ('Gov', 'Idea', 'Infra', 'Info', 'MilTech', 'Hier')}), road, models, args.out)
        write_results(T, S, os.path.join(args.out, 'results.xlsx'))
        print(f'\nWrote results.xlsx and report.txt to {args.out}  ({time.time() - t0:.0f} s)')
    log.close()


if __name__ == '__main__':
    main()
