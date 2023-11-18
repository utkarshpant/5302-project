[33mcommit 97b33c63cb743378c0dc8d969338c3f9e369f696[m[33m ([m[1;36mHEAD -> [m[1;32mrelative_paths[m[33m)[m
Author: Utkarsh Pant <utkarsh.pant@outlook.com>
Date:   Sat Nov 18 14:30:58 2023 -0600

    Add contents to .gitignore with DataSpell/RStudio artifacts ignored

[1mdiff --git a/.gitignore b/.gitignore[m
[1mnew file mode 100644[m
[1mindex 0000000..8a6f001[m
[1m--- /dev/null[m
[1m+++ b/.gitignore[m
[36m@@ -0,0 +1,47 @@[m
[32m+[m[32m# R specific[m
[32m+[m[32m.Rhistory[m
[32m+[m[32m.RData[m
[32m+[m[32m.Rproj.user/[m
[32m+[m
[32m+[m[32m# Generated files[m
[32m+[m[32m*.html[m
[32m+[m[32m*.pdf[m
[32m+[m
[32m+[m[32m# Logs[m
[32m+[m[32m*.log[m
[32m+[m
[32m+[m[32m# Packages[m
[32m+[m[32m*.Rcheck/[m
[32m+[m[32m.Renviron[m
[32m+[m[32m.Rproj/[m
[32m+[m[32m.Rproj.site/[m
[32m+[m
[32m+[m[32m# Data files[m
[32m+[m[32mdata/*.csv[m
[32m+[m[32mdata/*.tsv[m
[32m+[m[32mdata/*.xlsx[m
[32m+[m
[32m+[m[32m# Exclude specific large datasets[m
[32m+[m[32mlarge_data.csv[m
[32m+[m
[32m+[m[32m# Temporary files[m
[32m+[m[32m.Rproj.knit/[m
[32m+[m[32m.Rproj.markdown/[m
[32m+[m
[32m+[m[32m# Shiny related[m
[32m+[m[32m*.Rapp.history[m
[32m+[m[32m.Rdata[m
[32m+[m[32mshiny_cache/[m
[32m+[m
[32m+[m[32m# DataSpell specific[m
[32m+[m[32m.idea/[m
[32m+[m[32m__pycache__/[m
[32m+[m[32mdata_spell.log[m
[32m+[m[32mspell.cfg[m
[32m+[m
[32m+[m[32m# macOS system files[m
[32m+[m[32m.DS_Store[m
[32m+[m
[32m+[m[32m# Windows-specific files[m
[32m+[m[32mThumbs.db[m
[32m+[m[32mdesktop.ini[m
