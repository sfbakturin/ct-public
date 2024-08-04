# MATPLOTLIB Pyplot sizes.
MATPLOTLIB_PYPLOT_FIGSIZE = (30, 22)
MATPLOTLIB_PYPLOT_FIGSIZE_SMALL = (15, 11)

# Target for binary classification.
TARGET_BIN_CLASS = "Agency Type"

# Text translations.
FEATURES_TEXT = ["Agency", "Agency Type", "Destination", "Gender"]
FEATURES_NUMERIC = ["Age", "Claim"]
FEATURES_ALL = FEATURES_TEXT + FEATURES_NUMERIC

# Data hardcoded constants.
DATA_DELIMITER = ","
DATA_FILENAME ="claims.csv"

# Colors.
COLOR_SET_TRAIN = "red"
COLOR_SET_TEST = "blue"
COLOR_SET_TRAIN_LIBRARY = "green"
COLOR_SET_TEST_LIBRARY = "orange"

# OPTUNA study number of trials.
OPTUNA_STUDY_N_TRIALS = 200

# Training constants.
TEST_SIZE = 0.4
RANDOM_STATE = 42
