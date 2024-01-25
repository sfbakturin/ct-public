import torch
import torch.optim
import torch.nn

# matplotlib pyplot sizes
PLT_FIGSIZE = (30, 22)
PLT_FIGSIZE_SMALL = (15, 11)

# image resized sizes
IMG_RESIZED_SIZE = (32, 32)

# colors for plots
CLR_TRAIN = "green"
CLR_TEST = "red"

# dirnames
DIR_DATASET = "model"
DIR_TESTING = "test"

# model setup
MDL_OPTIMIZER = torch.optim.Adam
MDL_CRITERIA = torch.nn.MSELoss
MDL_BEST_MODEL = "lamps.model"
MDL_BATCH_SIZE = 32
MDL_LEARNING_RATE = 0.0005
MDL_EPOCHS = 10
MDL_MAX_EPOCHS = MDL_EPOCHS * 10
MDL_TRAIN_FRACTION = 0.8

# devices
DEV_CPU = torch.device("cpu")
DEV_IS_GPU = torch.cuda.is_available()
DEV_GPU = torch.device("cuda:0" if DEV_IS_GPU else "cpu")
