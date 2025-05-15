# Machine-learning-methods-for-forest-type-classification
This article explores machine learning methods for classifying land plots using Random Forest, Support Vector Machine, and neural networks. It also includes clustering analysis with algorithms like Iterative Distance-Based, EM Mixture Model, and DBSCAN to identify homogeneous data groups.

# Overview

During this project, analyses were carried out on data referring to four land areas located in the **Roosevelt National Forest** in northern Colorado (USA).  
The main objective was to classify each observation based on the type of vegetation present—specifically, to determine which of the **seven forest types** each instance belongs to.

The analysis was conducted by training **machine learning algorithms**, which, using the various explanatory variables available in the dataset, are capable of assigning each observation to the corresponding forest type.

A second objective was to apply **unsupervised learning algorithms** capable of identifying **internally homogeneous and mutually heterogeneous groups**.  
The goal was to detect clusters of land plots that are **geographically close** to one another, corresponding to the **four wilderness areas** into which the Roosevelt National Forest is divided.

---

# Dataset Description

The dataset was provided by the **United States Geological Survey (USGS)** and the **United States Forest Service (USFS)**.  
It contains data from four wilderness areas located in the Roosevelt National Forest in northern Colorado. These areas are defined as _wilderness_ because they are **unaffected by human activities**, and the identified forest types result from **natural ecological processes** rather than forest management.

- The **original dataset** consists of **581,012 observations**.
- The version used in this project is a **reduced dataset** selected from a Kaggle competition.
- It contains **15,120 observations** and **54 variables**, including:
  - **10 quantitative variables**
  - **4 binary variables** indicating wilderness area types
  - **40 binary variables** indicating soil types

Each observation represents a **30m x 30m** plot of forested land.
