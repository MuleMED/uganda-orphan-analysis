------------------------------------------------------------------------

---
title: "Uganda Orphan Analysis"
author: "Mule Samuel"
output:
  pdf_document: default
  html_notebook: default
source: https://open.africa/
---

# Where Are Uganda's Most Vulnerable Children?

## 1. Project Overview

### 1.1 Title

*Where Are Uganda's Most Vulnerable Children? A District Level Analysis of Orphanhood and Parental Survival Status*

### 1.2 Objective

Quantify and rank Uganda's districts by the **number** and **proportion** of children under 18 who have lost one or both parents. Explore geographic patterns and concentrations of orphanhood to inform child-protection and social-protection planning.

### 1.3 Data

District level counts of children below 18 by parental survival status:

| Column | Meaning |
|:-----------------------------------|:-----------------------------------|
| `both_alive` | Both parents alive |
| `mother_alive_father_dead` | Paternal orphans (father deceased, mother alive) |
| `father_alive_mother_dead` | Maternal orphans (mother deceased, father alive) |
| `both_dead` | Double orphans (both parents deceased) |

### 1.4 Scope and limitations

This analysis describes the **distribution** of orphanhood in the available data. It does **not** establish the causes of orphanhood, nor does it measure the full vulnerability of individual children. Rates are derived from reconstructed totals (the sum of the four survival categories) and should be read as descriptive, not causal.

------------------------------------------------------------------------

## 2. Setup

```{r setup}
# ---- Libraries ----
# Run once in a fresh session and uncomment the install for the first run
# install.packages(c("tidyverse", "janitor", "scales", "forcats", "gt", "here"))

library(tidyverse)
library(janitor)
library(scales)
library(forcats)
library(gt)
library(here)
```

## 3. Load and clean the data

```{r load-data}
# ---- Load ----
raw <- read_csv(
  here("data", "children-below-18-by-parents-survival-status.csv"),
  show_col_types = FALSE
)

# ---- Inspect raw columns ----
names(raw)
names(clean_names(raw))
```

```{r}
# ---- Standardize names and select analysis columns ----
df <- raw %>%
  clean_names() %>%
  select(
    code,
    level,
    district = district_3,
    both_alive,
    mother_alive_father_dead,
    father_alive_mother_dead,
    both_dead
  )

names(df)

```

## 4. Validate the data

```{r validate-data}
# ---- Structure ----
glimpse(df)

# ---- Missing values per indicator ----
df %>%
  summarise(
    across(
      c(both_alive, mother_alive_father_dead,
        father_alive_mother_dead, both_dead),
      ~ sum(is.na(.x))
    )
  )

# ---- Duplicate district records ----
df %>%
  count(code, district) %>%
  filter(n > 1)

# ---- Coerce counts to numeric (robust to character-typed CSV columns) ----
df <- df %>%
  mutate(
    across(
      c(both_alive, mother_alive_father_dead,
        father_alive_mother_dead, both_dead),
      ~ readr::parse_number(as.character(.x))
    )
  )

```

**Interpretation:** Missing counts, negative values, or duplicated codes would each invalidate the ranking that follows.

## 5. Main indicators

For each district we reconstruct:

- **Total children under 18** — sum of the four survival categories.

- **Single orphans** — paternal + maternal.

- **Total orphans** — single + double.

- **Orphanhood rate** — total orphans ÷ total children (share of *all* children who have lost at least one parent).

```{r indicators}
df <- df %>%
  mutate(
    total_children = rowSums(
      across(c(both_alive, mother_alive_father_dead,
               father_alive_mother_dead, both_dead)),
      na.rm = FALSE
    ),
    
    paternal_orphans = mother_alive_father_dead,
    maternal_orphans = father_alive_mother_dead,
    
    single_orphans = paternal_orphans + maternal_orphans,
    double_orphans = both_dead,
    total_orphans  = single_orphans + double_orphans,
    
    orphanhood_rate = if_else(
      total_children > 0, total_orphans / total_children, NA_real_
    ),
    paternal_orphan_rate = if_else(
      total_children > 0, paternal_orphans / total_children, NA_real_
    ),
    maternal_orphan_rate = if_else(
      total_children > 0, maternal_orphans / total_children, NA_real_
    ),
    double_orphan_rate = if_else(
      total_children > 0, double_orphans / total_children, NA_real_
    )
  )

df %>% select(district, total_children, total_orphans, orphanhood_rate) %>% head()
```

**Interpretation:** Reporting the rate alongside the count separates districts with large child populations from those where orphanhood represents a larger share of children

## 6. Data quality and geographic coverage

```{r data_quality}
# ---- Missing values after mutation ----
df %>%
  summarise(
    across(
      c(both_alive, mother_alive_father_dead,
        father_alive_mother_dead, both_dead),
      ~ sum(is.na(.x))
    )
  )

# ---- Negative counts ----
df %>%
  filter(
    if_any(
      c(both_alive, mother_alive_father_dead,
        father_alive_mother_dead, both_dead),
      ~ !is.na(.x) & .x < 0
    )
  )


# ---- duplicate codes by level 
df %>%
  count(code, level, name = "record_count") %>%
  filter(record_count > 1)

# ----  arithmetic consistency ----
stopifnot(
  all(
    df$total_children ==
      rowSums(df[c("both_alive", "mother_alive_father_dead",
                   "father_alive_mother_dead", "both_dead")],
              na.rm = FALSE),
    na.rm = TRUE
  )
)

# ---- Districts with zero or missing children ----
df %>%
  filter(level == "district") %>%
  filter(total_children == 0 | is.na(total_children)) %>%
  count()
```

**Interpretation.** The arithmetic check confirms that `total_children` is a faithful reconstruction of the four categories. Any district with zero or missing children would be excluded from rate rankings and must be documented

## 7. Region mapping

Uganda's districts are grouped into four regions: **Central, Eastern, Northern, Western**. The mapping is stored as a lookup table so it can be tested and reused.

```{r missing_district}
# ---- single source of truth 
region_lookup <- tribble(
  ~district, ~region,
  # Central
  "Buikwe", "Central", "Bukomansimbi", "Central", "Butambala", "Central",
  "Buvuma", "Central", "Gomba", "Central", "Kalangala", "Central",
  "Kalungu", "Central", "Kampala", "Central", "Kayunga", "Central",
  "Kiboga", "Central", "Kyankwanzi", "Central", "Kyotera", "Central",
  "Luwero", "Central", "Lwengo", "Central", "Lyantonde", "Central",
  "Masaka", "Central", "Mityana", "Central", "Mpigi", "Central",
  "Mubende", "Central", "Mukono", "Central", "Nakaseke", "Central",
  "Nakasongola", "Central", "Rakai", "Central", "Ssembabule", "Central",
  "Wakiso", "Central", "Kassanda", "Central",
  # Eastern
  "Amuria", "Eastern", "Budaka", "Eastern", "Bududa", "Eastern",
  "Bugiri", "Eastern", "Bugweri", "Eastern", "Bukedea", "Eastern",
  "Bukwo", "Eastern", "Bulambuli", "Eastern", "Busia", "Eastern",
  "Butaleja", "Eastern", "Butebo", "Eastern", "Buyende", "Eastern",
  "Iganga", "Eastern", "Jinja", "Eastern", "Kaberamaido", "Eastern",
  "Kalaki", "Eastern", "Kaliro", "Eastern", "Kamuli", "Eastern",
  "Kapchorwa", "Eastern", "Kapelebyong", "Eastern", "Katakwi", "Eastern",
  "Kibuku", "Eastern", "Kumi", "Eastern", "Kween", "Eastern",
  "Luuka", "Eastern", "Manafwa", "Eastern", "Mbale", "Eastern",
  "Mayuge", "Eastern", "Namisindwa", "Eastern", "Namayingo", "Eastern",
  "Namutumba", "Eastern", "Ngora", "Eastern", "Pallisa", "Eastern",
  "Serere", "Eastern", "Sironko", "Eastern", "Soroti", "Eastern",
  "Tororo", "Eastern",
  # Northern
  "Abim", "Northern", "Adjumani", "Northern", "Agago", "Northern",
  "Alebtong", "Northern", "Amolatar", "Northern", "Amudat", "Northern",
  "Amuru", "Northern", "Apac", "Northern", "Arua", "Northern",
  "Dokolo", "Northern", "Gulu", "Northern", "Kaabong", "Northern",
  "Karenga", "Northern", "Kitgum", "Northern", "Koboko", "Northern",
  "Kole", "Northern", "Kotido", "Northern", "Kwania", "Northern",
  "Lamwo", "Northern", "Lira", "Northern", "Madi-Okollo", "Northern",
  "Maracha", "Northern", "Moroto", "Northern", "Moyo", "Northern",
  "Nakapiripirit", "Northern", "Napak", "Northern", "Nebbi", "Northern",
  "Nabilatuk", "Northern", "Nwoya", "Northern", "Obongi", "Northern",
  "Omoro", "Northern", "Otuke", "Northern", "Oyam", "Northern",
  "Pader", "Northern", "Pakwach", "Northern", "Terego", "Northern",
  "Yumbe", "Northern", "Zombo", "Northern",
  # Western
  "Buhweju", "Western", "Buliisa", "Western", "Bundibugyo", "Western",
  "Bunyangabu", "Western", "Bushenyi", "Western", "Hoima", "Western",
  "Ibanda", "Western", "Isingiro", "Western", "Kabale", "Western",
  "Kabarole", "Western", "Kagadi", "Western", "Kakumiro", "Western",
  "Kamwenge", "Western", "Kanungu", "Western", "Kasese", "Western",
  "Kibaale", "Western", "Kikuube", "Western", "Kiruhura", "Western",
  "Kiryandongo", "Western", "Kisoro", "Western", "Kitagwenda", "Western",
  "Kyegegwa", "Western", "Kyenjojo", "Western", "Mbarara", "Western",
  "Mitooma", "Western", "Ntoroko", "Western", "Ntungamo", "Western",
  "Rubanda", "Western", "Rubirizi", "Western", "Rukiga", "Western",
  "Rukungiri", "Western", "Rwampara", "Western", "Sheema", "Western",
  "Masindi", "Western", "Kazo", "Western"
) %>%
  mutate(district_key = str_to_lower(str_squish(district))) %>%
  select(district_key, region)

# ---- region onto the full dataframe
df <- df %>%
  mutate(district_key = str_to_lower(str_squish(district))) %>%
  left_join(region_lookup, by = "district_key") %>%
  select(-district_key)

```

## 8. Check the region mapping

```{r mappings}
# ---- full data
df %>%
  filter(is.na(region)) %>%
  count(level, district, sort = TRUE)

# ---- restrict to district level records
districts <- df %>% filter(level == "district")
stopifnot(nrow(districts) > 0)

# ---- assert full region coverage at district level
unmapped <- districts %>%
  filter(is.na(region)) %>%
  distinct(district)

if (nrow(unmapped) > 0) {
  warning(
    "Unmapped districts (add to region_lookup): ",
    paste(unmapped$district, collapse = ", ")
  )
}

unmapped

```

**Interpretation.** Every district record must map to exactly one region and any unmapped district would silently drop out of the regional and national aggregations below

## 9. National summary

```{r national_sum}
national <- districts %>%
  summarise(
    total_children   = sum(total_children,   na.rm = TRUE),
    paternal_orphans = sum(paternal_orphans, na.rm = TRUE),
    maternal_orphans = sum(maternal_orphans, na.rm = TRUE),
    double_orphans   = sum(double_orphans,   na.rm = TRUE),
    total_orphans    = sum(total_orphans,    na.rm = TRUE)
  ) %>%
  mutate(
    orphanhood_rate = if_else(total_children > 0,
                              total_orphans / total_children, NA_real_)
  )

national %>%
  gt() %>%
  tab_header(title = "Uganda — National Orphanhood Summary") %>%
  fmt_number(
    columns = c(total_children, paternal_orphans, maternal_orphans,
                double_orphans, total_orphans),
    decimals = 0
  ) %>%
  fmt_percent(columns = orphanhood_rate, decimals = 1) %>%
  cols_label(
    total_children   = "Children <18",
    paternal_orphans = "Paternal orphans",
    maternal_orphans = "Maternal orphans",
    double_orphans   = "Double orphans",
    total_orphans    = "Total orphans",
    orphanhood_rate  = "Orphanhood rate"
  )
```

**Interpretation.** The national rate is the population weighted orphanhood share for Uganda. It anchors the district and regional comparisons that follow.

## 10. Rank districts by number and by rate

```{r rank}
# ---- Top 15 by absolute count ----
top_n_count <- districts %>%
  arrange(desc(total_orphans)) %>%
  slice_head(n = 15) %>%
  mutate(share_of_national_orphans = total_orphans / national$total_orphans) %>%
  select(district, region, total_children, total_orphans,
         orphanhood_rate, share_of_national_orphans)

top_n_count %>%
  gt() %>%
  tab_header(title = "Top 15 Districts by Number of Orphans") %>%
  fmt_number(columns = c(total_children, total_orphans), decimals = 0) %>%
  fmt_percent(
    columns = c(orphanhood_rate, share_of_national_orphans),
    decimals = 1
  ) %>%
  cols_label(
    district                 = "District",
    region                   = "Region",
    total_children           = "Children <18",
    total_orphans            = "Orphans",
    orphanhood_rate          = "Orphanhood rate",
    share_of_national_orphans = "Share of national orphans"
  )

```

```{r }
# ---- Top 15 by rate ----
top_n_rate <- districts %>%
  filter(!is.na(orphanhood_rate)) %>%
  arrange(desc(orphanhood_rate)) %>%
  slice_head(n = 15) %>%
  select(district, region, total_children, total_orphans, orphanhood_rate)

top_n_rate %>%
  gt() %>%
  tab_header(title = "Top 15 Districts by Orphanhood Rate") %>%
  fmt_number(columns = c(total_children, total_orphans), decimals = 0) %>%
  fmt_percent(columns = orphanhood_rate, decimals = 1) %>%
  cols_label(
    district        = "District",
    region          = "Region",
    total_children  = "Children <18",
    total_orphans   = "Orphans",
    orphanhood_rate = "Orphanhood rate"
  )

```

## 11. Visualise the rankings

```{r visual }
districts %>%
  filter(!is.na(orphanhood_rate)) %>%
  arrange(desc(orphanhood_rate)) %>%
  slice_head(n = 15) %>%
  mutate(district = fct_reorder(district, orphanhood_rate)) %>%
  ggplot(aes(x = orphanhood_rate, y = district, fill = region)) +
  geom_col() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Districts with the Highest Orphanhood Rates",
    subtitle = "Share of children under 18 who have lost at least one parent",
    x = "Orphanhood rate", y = NULL, fill = "Region",
    caption = "Source: open.africa"
  ) +
  theme_minimal()

```

## 12. Compare regions

```{r compare}

regional <- districts %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(
    districts        = n(),
    total_children   = sum(total_children,   na.rm = TRUE),
    paternal_orphans = sum(paternal_orphans, na.rm = TRUE),
    maternal_orphans = sum(maternal_orphans, na.rm = TRUE),
    double_orphans   = sum(double_orphans,   na.rm = TRUE),
    total_orphans    = sum(total_orphans,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    orphanhood_rate      = if_else(total_children > 0,
                                   total_orphans / total_children, NA_real_),
    share_of_all_orphans = total_orphans / national$total_orphans
  )

regional %>%
  gt() %>%
  tab_header(title = "Regional Orphanhood Summary") %>%
  fmt_number(where(is.numeric), decimals = 0) %>%
  fmt_percent(
    columns = c(orphanhood_rate, share_of_all_orphans),
    decimals = 1
  )

```

## 13. Composition of orphanhood by region

```{r compo}
regional %>%
  select(region, paternal_orphans, maternal_orphans, double_orphans) %>%
  pivot_longer(-region, names_to = "type", values_to = "n") %>%
  mutate(type = factor(type,
    levels = c("paternal_orphans", "maternal_orphans", "double_orphans"),
    labels = c("Paternal", "Maternal", "Double")
  )) %>%
  ggplot(aes(x = region, y = n, fill = type)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Composition of Orphanhood by Region",
    subtitle = "Share of orphans by survival type within each region",
    x = NULL, y = "Share of orphans", fill = "Orphan type"
  ) +
  theme_minimal()
```

## 14. District size vs orphanhood rate

```{r}
districts %>%
  filter(!is.na(orphanhood_rate), total_children > 0) %>%
  ggplot(aes(x = total_children, y = orphanhood_rate,
             colour = region, size = total_orphans)) +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = comma) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "District Size vs Orphanhood Rate",
    subtitle = "Point size = absolute number of orphans",
    x = "Children under 18 (log scale)", y = "Orphanhood rate",
    colour = "Region", size = "Orphans"
  ) +
  theme_minimal()

```

## 15. Priority districts — burden and rate

```{r priority}
districts <- districts %>%
  mutate(
    high_count = !is.na(total_orphans) &
                 total_orphans >= quantile(total_orphans, 0.75, na.rm = TRUE),
    high_rate  = !is.na(orphanhood_rate) &
                 orphanhood_rate >= quantile(orphanhood_rate, 0.75, na.rm = TRUE),
    priority = case_when(
      is.na(total_orphans) | is.na(orphanhood_rate) ~ "Missing data",
      high_count & high_rate ~ "High burden and high rate",
      high_count             ~ "High burden only",
      high_rate              ~ "High rate only",
      TRUE                   ~ "Other"
    )
  )

# ---- distribution of priority classes 
districts %>%
  count(priority) %>%
  gt() %>%
  tab_header(title = "Priority Classes (relative thresholds at the 75th percentile)")

# ---- districts in the highest-priority class 
districts %>%
  filter(priority == "High burden and high rate") %>%
  arrange(desc(total_orphans)) %>%
  select(district, region, total_orphans, orphanhood_rate) %>%
  gt() %>%
  tab_header(title = "Districts Flagged as High Burden AND High Rate") %>%
  fmt_number(columns = total_orphans, decimals = 0) %>%
  fmt_percent(columns = orphanhood_rate, decimals = 1)

```
