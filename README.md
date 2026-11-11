# Complexity-data

Data and code for Sandeford, Turchin, Hamilton and Lobo, "Social complexity promotes cooperation and functional integration in large-scale societies" (*PNAS Nexus*, 2026).

## Contents

| File | Description |
|---|---|
| `Complexity_data.xlsx` | All data in one workbook (see the README sheet inside for sources and a codebook). |
| `replicate.py` | Rebuilds the analysis data from the raw Seshat records and reproduces every analysis and figure in the paper. |
| `requirements.txt` | Python packages needed. |

The workbook has six data sheets:

- `scalingData`: the analysis data, 530 polity-century observations.
- `Seshat_raw`: the Seshat: Global History Databank Equinox 2020 release, as scraped in October 2020.
- `polities`: the polity list with start and end dates.
- `variables`: the Seshat variables processed.
- `HistYield`: agricultural productivity by NGA and century (Turchin et al. 2021).
- `Roadways_Energy`: road network length and energy consumption for contemporary countries, 2018–2023.

## Replicating the analyses

```
pip install -r requirements.txt
python replicate.py
```

The script takes about a minute. It writes the following to `results/`:

- `report.txt`: the full printed output.
- `results.xlsx`: Table 1, all regression models, bootstrap intervals, and the robustness analyses reported in Materials and Methods and the Supplementary Information.
- `Figure2`, `Figure3` and `Figure4`, as PNG and PDF.

To analyse the archived `scalingData` sheet directly without rebuilding it, run `python replicate.py --from-sheet`. The `--boot` and `--seed` options set the number of bootstrap replicates (default 5000) and the random seed.

## Notes

The script replaces the earlier R scripts (`0UpdatePolsVars.R`, `1ProcDat.R`, `2AggrAvg.R`, `3Scaling.R`), which remain in the repository history. The rebuilt analysis data match the data used in the paper to floating-point precision.

Bootstrap intervals are computed with the same BCa method as R's `boot.ci`. Python's random number generator differs from R's, so interval endpoints may differ from the published values in the third decimal place.
