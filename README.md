# Capital Bikeshare EDA (R)

**Stack:** R · dplyr · ggplot2 · lattice  
Exploratory analysis and linear models on the public **UCI Bike Sharing Dataset** (Capital Bikeshare, Washington D.C., 2011–2012).

## What’s here

| File | Role |
|------|------|
| `analysis.R` | Weekday/season summaries, density & level plots, LM for casual ridership + simple CV |
| `day.csv` / `hour.csv` | Daily and hourly aggregates |
| `Readme.txt` | Original dataset documentation (Fanaee-T & Gama) |
| `Rplot.jpeg` | Example plot from the coursework session |
| `qsar_fish_toxicity.csv` | Companion public QSAR dataset present in the original folder (not required for `analysis.R`) |

Session binary files (`.RData`, `.Rhistory`) are gitignored; `analysis.R` was recovered from the coursework history for a runnable script.

## Run

```r
# From this directory in R or RStudio
install.packages(c("dplyr", "ggplot2", "lattice"))
source("analysis.R")
```

## Data citation

Fanaee-T, Hadi, and Gama, Joao. *Event labeling combining ensemble detectors and background knowledge.* Progress in Artificial Intelligence (2013).  
UCI: [Bike Sharing Dataset](https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset)

## License / academic use

Coursework packaged for portfolio demonstration. Dataset remains under its original terms.
