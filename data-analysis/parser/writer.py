import config
import multiprocessing
import os

class csv_writer:
    def __init__(self, filename, init_str_array):
        self.stream = open(file = filename, mode = "w")
        self.mutex = multiprocessing.Lock()
        self.write(init_str_array)

    def write(self, string_array):
        str = string_array[0]

        for i in range(1, len(string_array)):
            str += config.MSVETA_STAT_SEPARATOR
            str += string_array[i]

        with self.mutex:
            self.stream.write(str + os.linesep)
            self.stream.flush()

    def __del__(self):
        self.stream.close()
