# EIPIP
EIPIP is an R package designed for the implementation of the IPIP (Imbalanced Problem Identification and Prediction) approach and its variant. This package provides a robust solution for handling imbalanced data problems, allowing you to create ensembles of machine learning models for improved predictive accuracy.

## Methodology

The IPIP approach is a powerful technique for addressing imbalanced data problems. It involves forming ensembles by combining multiple base learners, including Support Vector Machines (SVM), Regularized Logistic Regression (RLOG), Random Forest (RANGER), and Gradient Boosting Machines (GBM). Each base learner contributes its unique strengths and characteristics to the ensemble, enabling a comprehensive analysis of imbalanced data. This approach has been proven to be effective in a wide range of applications, including fraud detection, medical diagnosis, and anomaly detection.

### Two Versions: Exhaustive and Non-Exhaustive

The EIPIP package offers two versions of the IPIP approach, each tailored to different use cases:

1. **Exhaustive IPIP**: This version involves an exhaustive exploration of various base learners and their combinations to find the optimal ensemble for the given imbalanced dataset. It systematically evaluates a wide range of configurations to deliver the best possible results. The exhaustive approach is suitable when you have the computational resources and time to perform an in-depth search for the ideal ensemble.

2. **Non-Exhaustive IPIP**: In contrast, the non-exhaustive version provides a faster and more practical solution. It focuses on a subset of base learners and configurations, which allows you to build an ensemble more efficiently. This version is useful when computational resources or time constraints are a concern, while still delivering competitive results.

### Implementation through Classes and Functional Approaches

The EIPIP package is implemented in R using a combination of class-based and functional programming approaches. This design provides flexibility and ease of use to accommodate a wide range of user preferences and needs.

#### Class-Based Implementation
You can utilize the EIPIP package through predefined classes that encapsulate the functionality of the IPIP approach. These classes offer an object-oriented way of working with imbalanced data, allowing you to easily customize and experiment with different configurations.

#### Functional Programming Implementation
For users who prefer a functional approach, EIPIP also provides functions to create and manage imbalanced ensembles. This approach is more modular and may be preferred by those who want to have more direct control over the process.

Both class-based and functional programming implementations offer the same underlying IPIP methodology, giving you the flexibility to choose the one that best suits your coding style and requirements.

More info regarding IPIP and EIPIP study in the https://github.com/42Azul/Imbalanced-data-IPIP repository.
