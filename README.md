# AU Basketball Shooting Dashboard
This repository contains the code and project materials for a capstone project that turns manually tracked AU men's basketball shot data into a decision-support dashboard. The project uses R and Shiny to estimate shot make probability, calculate expected points, and compare player performance across different shooting contexts.

## Overview
The project was built as an affordable and interpretable basketball analytics tool for American University. Shot data was manually collected with Hudl Sportscode, exported to CSV files, cleaned in R, modeled using logistic regression, and displayed in a Shiny dashboard.

## The tool is designed to answer questions such as:

- Which players score above or below expectation once shot difficulty is considered?

- Which shot zones are strongest or weakest for each player?

- How does performance change by contest level or shot type?

- How can two players be compared side by side in a way that is easy for coaches to understand?


## Data inputs

### The project currently expects two cleaned CSV files:

- practice data for model training

- game data for model testing and dashboard outputs

### Important columns include:

- Shooter

- MadeShot

- ShotZone

- Shot.Type

- Shot.Contest

- Shot.Clock

- ShotValue

## Current model
The current dashboard uses a logistic regression model trained on practice data and evaluated on game data. The predictors in the model include shot zone, shot clock group, contest level, and shot type.

### The predicted make probability for each shot is used to calculate:

- Expected points

- Actual points

- Points Above Expected (PAE)

- PAE per 100 shots

These metrics are then summarized by player, shot zone, shot type, and contest level.


## Dashboard sections
### The Shiny dashboard contains three main views:

#### Team Overview

-Team shooting summary

-Player leaderboard by PAE per 100 shots

-Team-level plots by shot zone and contest level


#### Individual Player

-Player stat boxes

-Tables and charts by shot zone

-Tables and charts by shot type

-Tables and charts by contest level


#### Compare Players

-Side-by-side player comparisons

-Multiple metric choices including FG%, total PAE, and PAE per 100 shots


## How to run the app
-Download or clone this repository.

-Replace the current CSV paths.

-Open the file in RStudio.

-Install required packages if needed: install.packages(c("shiny", "tidyverse", "scales"))
