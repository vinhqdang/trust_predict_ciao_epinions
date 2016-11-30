# idea
# read CSV file which includes 3 columns
# trustor trustee sign
# to adjacency matrix to feed to deepwalk
# https://github.com/phanein/deepwalk

import sys
import os
import pandas as pd

in_filename = sys.argv[1]

out_filename = sys.argv[2]

try:
    os.remove(out_filename)
except OSError:
    pass

df = pd.read_csv (in_filename)

nrows = len (df.index)

trustors = df.Trustor
trustees = df.Trustee
signs    = df.Sign

for i in range(nrows):
    print ("Processing line " + str(i + 1) + "/" + str(nrows))
    trustor = df.iat [i,1]
    trustee = df.iat [i,2]
    sign    = df.iat [i,3]

    # print (trustor)
    # print (trustee)

    # store all adjacency edges (not vertices)
    adjacency_list = list ()

    adjacency_list.extend(trustors[trustors==trustor].index.tolist())
    # print (adjacency_list)
    adjacency_list.extend(trustors[trustors==trustee].index.tolist())
    # print (adjacency_list)
    adjacency_list.extend(trustees[trustees==trustor].index.tolist())
    # print (adjacency_list)
    adjacency_list.extend(trustees[trustees==trustee].index.tolist())
    # print (adjacency_list)

    # remove duplicate
    adjacency_list = list (set (adjacency_list))

    # remove the current edge itself
    adjacency_list = filter(lambda a: a != i, adjacency_list)

    adjacency_list = [x + 1 for x in adjacency_list]

    adjacency_list = [i+1] + adjacency_list

    # print (adjacency_list)

    out_string = ' '.join(str(x) for x in adjacency_list)

    with open (out_filename, "a") as f:
        f.write (out_string)
        if i < nrows - 1:
            f.write ("\n")