import torch
import torch.optim
import torch.nn

# matplotlib pyplot sizes
PLT_FIGSIZE = (30, 22)
PLT_FIGSIZE_SMALL = (15, 11)

# colors for plots
CLR_TRAIN = "green"
CLR_TEST = "red"

# dirnames
DIR_DATASET = "model"
DIR_TESTING = "test"

# dataset setup
DAT_TOPICS = ["Biology", "Computing", "Culture", "Games", "Mathematics", "Philosophy", "Sports"]
CNT_TOPICS = len(DAT_TOPICS)

# `youtokentome` setup
PTH_TOKEN_MODEL = "articles.pth"
PTH_TOKEN_TRAIN_DATA = "articles.txt"

# model setup
MDL_VOCAB_SIZE = 500000
MDL_WORD_EMBEDDING_SIZE = 500
MDL_HIDDEN_SIZE = MDL_WORD_EMBEDDING_SIZE * 2
MDL_TEST_SIZE = 0.4
MDL_OPTIMIZER = torch.optim.Adam
MDL_CRITERIA = torch.nn.CrossEntropyLoss
MDL_BATCH_SIZE = 32
MDL_LEARNING_RATE = 0.0005
MDL_EPOCHS = 200
MDL_BEST_MODEL = "articles.model"

# devices
DEV_CPU = torch.device("cpu")
DEV_IS_GPU = torch.cuda.is_available()
DEV_GPU = torch.device("cuda:0" if DEV_IS_GPU else "cpu")
