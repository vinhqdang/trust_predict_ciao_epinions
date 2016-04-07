import sys

file_name = sys.argv[1]

src = list ()
tgt = list ()
vot = list ()
res = list ()
yea = list ()
dat = list ()
txt = list ()

with open (file_name) as f:
    lines = f.readlines ()
    for i in range (len(lines)):
        line = lines [i]
        line = line.strip ()
        if (line == ""):
            continue
        array = line.split (":")
        if (array[0] == "SRC"):
            src.append (array[1])
        if (array[0] == "TGT"):
            tgt.append (array[1])
        if (array[0] == "VOT"):
            vot.append (array[1])
        if (array[0] == "RES"):
            res.append (array[1])
        if (array[0] == "YEA"):
            yea.append (array[1])
        if (array[0] == "DAT"):
            dat.append (line[4:])
        if (array[0] == "TXT"):
            txt.append (array[1])

# print (src[1])
# print len(src)

# out_file = "wiki_rfa.txt"
# f = open (out_file, "a+")
# f.write ("SRC;TGT;VOT;RES;YEA;DAT;TXT\n")
# for i in range (len(src)):
#     f.write (src[i] + ";" + 
#         tgt[i] + ";" +
#         vot[i] + ";" +
#         res[i] + ";" +
#         yea[i] + ";" +
#         dat[i] + ";" +
#         txt[i] + "\n")

# f.close ()

out_file = "wiki_rfa_simple.txt"
f = open (out_file, "a+")
f.write ("SRC;TGT;VOT;RES;YEA\n")
for i in range (len(src)):
    f.write (src[i] + ";" + 
        tgt[i] + ";" +
        vot[i] + ";" +
        res[i] + ";" +
        yea[i] + "\n")

f.close ()