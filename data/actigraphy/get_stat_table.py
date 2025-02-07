import os
import glob

fnames = glob.glob("data/actigraphy/raw/*.csv")
fname=fnames[2]

with open(fname, "r") as f:
  text=f.read()

