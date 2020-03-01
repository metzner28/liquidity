# scrapes HTML minutes, 1990-2019, from FOMC and tokenizes + removes stopwords, then scores by FSS algorithm (londono et al.)
import pandas as pd
import string
import nltk.tokenize
from nltk.corpus import stopwords
import numpy as np
import matplotlib.pyplot as plt
import os

FSS_dict = pd.read_csv("FSS_dict.csv")
positive_words = FSS_dict.Word[FSS_dict.Positive == 1].tolist()
negative_words = FSS_dict.Word[FSS_dict.Negative == 1].tolist()

def score(minutes, positive_words, negative_words, negators):

    total = len(minutes)
    positive, negative = 0, 0
    negated = False

    for word in minutes:
        if word not in positive_words and word not in negative_words:
            continue
        elif word in negative_words:
            negative += 1
        elif word in positive_words:
            idx = minutes.index(word)
            window = minutes[idx - 3:idx + 4]
            for n in negators:
                if n in window:
                    negated = True
                    break
            if negated:
                negative += 1
            else:
                positive += 1

    FSS = (negative - positive) / total
    FSSminus = negative / total
    FSSstar = (negative + positive) / total

    return (FSS, FSSminus, FSSstar)

negators = ["not", "no", "nobody", "none", "never", "neither", "cannot"]
stop = [w for w in stopwords.words("english") if w not in negators] + list(string.punctuation)

dates = pd.read_csv("fed0.csv")
timeline = dates.date.tolist()

filenames = [filename for filename in os.listdir() if filename.endswith(".txt")]
filenames_ordered = sorted(filenames, key = lambda f: timeline.index(f[:-4]))
filenames_ordered

scores = []
for file in filenames_ordered:
    with open(file,'r', encoding = 'utf-8') as current_file:
        minutes = current_file.read()
        minutes_tokenized = nltk.tokenize.wordpunct_tokenize(minutes)
        minutes_ready = [word for word in minutes_tokenized if word not in stop]
        FSS = score(minutes_ready, positive_words, negative_words, negators)
        scores.append(FSS)

dfFSS = pd.DataFrame({'date': timeline, 'FSS': [s[0] for s in scores],
                      'FSSminus': [s[1] for s in scores], 'FSSstar': [s[2] for s in scores]})


dfFSS.to_csv('FSS_scores_appendix.csv', index = False)
