module BQueue where


import Queue
import Debug(debug)


data BQueue a = BQueue Int (Queue a)
